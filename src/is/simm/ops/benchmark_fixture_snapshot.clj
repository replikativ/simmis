(ns is.simm.ops.benchmark-fixture-snapshot
  "HOST-only C0 capture from the already owned private fixture. The returned
   DB is a native read handle, kept inside the coordinator's physical lifetime;
   only :c0 is portable evidence. No author Room, fallback repository or pin is
   selected here, and this extent must end before any case world is forked."
  (:require [dvergr.runtime.preparation :as preparation]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.system.db :as control]
            [dvergr.system.rooms :as native]
            [geschichte.repo :as repo]
            [is.simm.model.work-request-checkpoints :as checkpoints]
            [is.simm.ops.benchmark-case-manifest :as manifests]
            [is.simm.ops.benchmark-fixture :as fixture]
            [is.simm.ops.knowledge-changes :as changes]
            [org.replikativ.spindel.engine.core :as ec]))

(defn capture!
  "Require the exact creation receipt and canonical default repository. Hold a
   short fixture HOST lease, select its exact registered native system, verify
   a clean published snapshot and inventory the complete candidate filesystem.
   No author Room lease spans this work; authority for journal publication must
   still be checked by the original coordinator after this call returns."
  [host-ctx request-id {:keys [room persistent-id] :as receipt}]
  (fixture/validate-receipt! host-ctx request-id receipt)
  (let [registry-db (binding [ec/*execution-context* host-ctx] @(control/get-conn))]
    (binding [ec/*execution-context* (:ctx room)]
      (preparation/with-host-operation!
        room
        (fn [lease]
          (let [registered (native/default-workspace registry-db persistent-id)
                descriptor (workspace/workspace-for-system! (:system registered))
                conn (:conn descriptor)]
            (locking (workspace/workspace-lock conn)
              (workspace/assert-workspace-current! descriptor)
             ;; Case builders inherit this fixture-local marker. Never select
             ;; a workspace in the author's or installation's context.
              (workspace/select-workspace! (:runtime-system registered))
              (let [db @conn]
                (when-not (:clean? (repo/status conn))
                  (throw (ex-info "Benchmark fixture C0 must be clean" {:type ::dirty-fixture})))
                (let [basis (changes/basis db)
                      target (merge (select-keys basis [:repository :store :backend :branch :ref])
                                    (select-keys registered [:registry-system :runtime-system]))
                      selection (checkpoints/validate-selection!
                                 {:version 1 :policy :request-evidence-v1
                                  :origin {:version 1 :kind :datahike :commit (:commit basis)
                                           :source (select-keys basis [:store :repository])}
                                  :basis basis :target target})
                      c0 {:selection selection :manifest (manifests/inventory db)}]
                  (workspace/assert-workspace-current! descriptor)
                  (fixture/validate-receipt! host-ctx request-id receipt)
                  (let [current (native/default-workspace
                                 (binding [ec/*execution-context* host-ctx] @(control/get-conn)) persistent-id)]
                    (when-not (and (= (select-keys registered [:registry-system :runtime-system])
                                      (select-keys current [:registry-system :runtime-system]))
                                   (identical? (:system registered) (:system current))
                                   (identical? conn (:conn current)))
                      (throw (ex-info "Benchmark fixture default repository changed" {:type ::fixture-changed}))))
                 ;; Datahike deref may wrap an unchanged DB with metadata.
                 ;; Verify published revision identity, not wrapper identity;
                 ;; basis also rejects same-CID unpublished d/with changes.
                  (when-not (= basis (changes/basis @conn))
                    (throw (ex-info "Benchmark fixture changed during C0 capture" {:type ::fixture-changed})))
                  (when-not (= (:runtime-system registered) (workspace/selected-system-id))
                    (throw (ex-info "Benchmark fixture workspace selection changed" {:type ::fixture-changed})))
                  (preparation/with-host-current! lease (fn [] nil))
                  {:db db :c0 c0})))))))))
