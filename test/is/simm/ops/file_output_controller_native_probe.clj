(ns is.simm.ops.file-output-controller-native-probe
  "OPT-IN isolated native adoption diagnostic, not a default test or inference
   benchmark. The producer is synthetic trusted pre-certified input: retention,
   joint filing, authority, native publication and adoption are real. No Attempt,
   agent execution, provider, paid wallet or provisioned-home claim is made."
  (:require [clojure.test :refer [is]]
            [datahike.api :as d]
            [dvergr.rooms.files :as native-files]
            [dvergr.rooms.publication :as native-publication]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.file-output-controller :as controller]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.file-output-publication :as publication]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.owned-workspace-test :as fixture]
            [is.simm.ops.proposals :as proposals]))

(def ^:private owned-source "(ns owned.workflow)\n(def task :owned-only)\n")
(def ^:private attached-source "(ns attached.workflow)\n(def task :never-owned)\n")
(def ^:private after-text "# Adopted native output\n\nRetained and merged through the exact owned workspace.\n")

(defn- tree [db]
  (into (sorted-map)
        (map (fn [datom] [(:v datom) (files/entry db (:v datom))]))
        (d/datoms db :aevt :geschichte.work/path)))

(defn- fact-tx [db eid attr]
  (:tx (first (d/datoms db :eavt eid attr))))

(defn- seed-source! [conn source]
  (repo/write! conn "workflows/fixture.clj" (markdown/source-bytes source))
  (repo/stage-all! conn)
  (repo/commit! conn {:message "Distinct diagnostic workflow source"}))

(defn- synthetic-file!
  "Trusted TEST producer seam, not a new host authority or certification lane.
   Exact frozen request provenance and retained body refs are joined with the
   Proposal in the actual serialized control writer under real GC guards."
  [{:keys [conn w room owned]} output]
  (let [binding (owned/resolve! @conn (:owner w) :merge (:room w) room)
        captured (inputs/capture (:db binding) inputs/default-policy)
        basis (inputs/publication-basis (:db binding)
                                        (select-keys (:target binding) [:registry-system :runtime-system]))
        input {:workflow {:input-selection {:execution-id (:execution-id captured)}}
               :knowledge/publication-basis basis :fixture :synthetic-pre-certified}
        row (:request (requests/claim! conn {:party (:owner w) :room (:room w)
                                             :client-id (random-uuid) :kind :workflow :intent input}))
        request (:work.request/id row) job (random-uuid) candidate (random-uuid)
        _ (requests/record-job! conn request (:work.request/claim row) job)
        _ (requests/mark-returned! conn request)
        _ (is (= basis (requests/publication-basis conn request)))
        frozen (dispositions/request-binding (requests/get-request conn request))
        capture (outputs/capture-db @output)
        plan (plans/plan {:request request :room (:room w) :attempt candidate
                          :environment {:fixture :synthetic-pre-certified} :workflow-bundle (random-uuid)
                          :publication-basis basis :output capture :retained-inputs (:inputs captured)})
        manifest (:manifest plan)
        value (dispositions/make {:request request :intent (:work.request/intent-id frozen)
                                  :job job :candidate candidate :attempt-content (random-uuid)
                                  :basis (:id basis) :output (:id capture) :kind :proposed
                                  :manifest (:id manifest) :proposal (:proposal plan)})]
    (storage/publish!
     conn plan
     (fn [retained]
       (let [entity (proposals/prepare-filing
                     {:id (:proposal plan) :title "Native adoption diagnostic"
                      :summary "Synthetic pre-certified fixture; actual retained native adoption."
                      :author (:owner w) :room (:room w) :run candidate
                      :forks [{:scope (:id manifest) :authority-scope (:room w)
                               :branch (str "files-" (:id manifest)) :system-type :file-output
                               :file-output retained}]})]
         (d/transact conn [[:db.fn/call
                            (fn [db]
                              (into (proposals/filing-tx-data db entity)
                                    (dispositions/tx-data db frozen value)))]]))))
    (is (= value (dispositions/disposition (requests/get-request conn request))))
    (is (= manifest (storage/manifest conn (:id manifest))))
    {:plan plan :disposition value}))

(defn native-adoption-probe
  "Run once with true and once with false for BOTH registration orders.
   Calls the real native writer: an unavailable explicit writer throws rather
   than silently skipping. Uses real canonical control mappings/default grants,
   two registered native systems, current EACL, retention, joint filing, journal,
   native receipt and terminal acceptance. Only upstream file-backed default
   provisioning/UUID-to-context discovery is scoped to the existing memory
   fixture descriptor. This does NOT prove agent certification or source routing."
  [owned-first?]
  (when-not (and (boolean? owned-first?)
                 (contains? native-files/write-capabilities :explicit-workspace)
                 (ns-resolve 'dvergr.rooms.files 'write-to!))
    (throw (ex-info "Native adoption diagnostic requires the actual explicit native writer"
                    {:type ::unavailable})))
  (let [configs (atom []) room-store-conn (atom nil)]
    (try
      (@#'fixture/with-fixture
       owned-first?
       (fn [{:keys [conn w room owned attached attached-descriptor] :as state}]
         (let [store-conn (:conn (:store room))]
           (when-not store-conn
             (throw (ex-info "Diagnostic requires the actual native room store" {})))
           (reset! room-store-conn store-conn)
           (swap! configs into (map #(get @% :config) [conn owned attached store-conn])))
         (let [cfg (assoc workspace/repository-options :store {:backend :memory :id (random-uuid)})]
           (d/create-database cfg)
           (swap! configs conj cfg)
           (let [output (d/connect cfg)]
             (try
               (repo/init! output {:name "Isolated synthetic producer"})
               (seed-source! owned owned-source)
               (seed-source! attached attached-source)
               (repo/write! output "out/wiki/file.md" (markdown/source-bytes after-text))
               (d/transact conn adoptions/schema)
               (let [owned-before @owned attached-before @attached
                     selected (workspace/selected-system-id)
                     {:keys [plan disposition]} (synthetic-file! state output)
                     proposal (:proposal plan)
                     _ (is (= (:runtime-system attached-descriptor) selected))
                     _ (is (not= (tree owned-before) (tree attached-before)))
                     _ (is (= owned-source (:text (files/read-file-in owned-before "workflows/fixture.clj"))))
                     _ (is (= attached-source (:text (files/read-file-in attached-before "workflows/fixture.clj"))))
                     _ (is (= (tree owned-before) (tree @owned)))
                     _ (is (= (tree attached-before) (tree @attached)))
                     result (controller/adopt! conn (:owner w) (:room w) room proposal)
                     db @conn operation (:operation result)
                     p (d/pull db '[:proposal/status :proposal/resolved-by :proposal/resolved-at
                                    {:proposal/forks [:db/id :proposal.fork/status
                                                      :proposal/resolved-by :proposal/resolved-at
                                                      {:proposal.fork/file-adoption [:file.adoption/id]}]}]
                               [:proposal/id proposal])
                     fork (first (:proposal/forks p))
                     winning (native-publication/recover @owned (publication/identity operation (:manifest plan)))]
                 (is (= :accepted (:status result) (:state operation)
                        (:proposal/status p) (:proposal.fork/status fork)))
                 (is (uuid? (:id operation)))
                 (is (= operation (adoptions/operation db (:id operation))))
                 (is (= (:id operation) (get-in fork [:proposal.fork/file-adoption :file.adoption/id])))
                 (is (= (:owner w) (:actor operation) (:proposal/resolved-by p) (:proposal/resolved-by fork)))
                 (is (= (:updated-at operation) (:proposal/resolved-at p) (:proposal/resolved-at fork)))
                 (let [journal-eid (:db/id (d/entity db [:file.adoption/id (:id operation)]))
                       proposal-eid (:db/id (d/entity db [:proposal/id proposal]))
                       txs [(fact-tx db journal-eid :file.adoption/state)
                            (fact-tx db proposal-eid :proposal/status)
                            (fact-tx db (:db/id fork) :proposal.fork/status)]]
                   (is (every? integer? txs))
                   (is (apply = txs)))
                 (is (map? winning))
                 (is (= winning (publication/validate-receipt! operation (:manifest plan) winning)))
                 (is (= (:head operation) (:publication/head winning)))
                 (is (false? (:publication/noop? winning)))
                 (let [target-db @owned
                       ref-eid (:db/id (d/entity target-db [:geschichte.ref/name (:publication/ref winning)]))
                       commit-eid (:db/id (d/entity target-db [:geschichte.commit/id (:publication/commit winning)]))
                       txs [(fact-tx target-db (:db/id winning) :publication/id)
                            (fact-tx target-db ref-eid :geschichte.ref/target)
                            (fact-tx target-db commit-eid :geschichte.commit/id)]]
                   (is (every? integer? txs))
                   (is (apply = txs)))
                 (is (= after-text (:text (files/read-file-in @owned "wiki/file.md"))))
                 (is (= owned-source (:text (files/read-file-in @owned "workflows/fixture.clj"))))
                 (is (= (tree attached-before) (tree @attached)))
                 (is (= (query/current-ref attached-before) (query/current-ref @attached)))
                 (is (= (query/refs attached-before) (query/refs @attached)))
                 (is (= selected (workspace/selected-system-id)))
                 (is (= disposition (dispositions/disposition
                                     (requests/get-request conn (:request disposition)))))
                 (is (= 1 (count (d/datoms db :aevt :file.adoption/id))))
                 (is (= 1 (count (d/datoms @owned :aevt :publication/id))))
                 ;; Accepted replay is intentionally NOT a second publisher or
                 ;; an accepted read API: the ordinary open-only guard refuses.
                 (let [control-before @conn target-before @owned
                       forbidden (fn [& _] (throw (AssertionError. "Accepted repeat attempted body IO/native dispatch")))]
                   (is (every? integer? [(:max-tx control-before) (:max-tx target-before)]))
                   (with-redefs-fn {#'controller/native-writer forbidden
                                    #'storage/read-file-in forbidden
                                    #'storage/read-file forbidden}
                     #(is (thrown? clojure.lang.ExceptionInfo
                                   (controller/adopt! conn (:owner w) (:room w) room proposal))))
                   (is (= (:max-tx control-before) (:max-tx @conn)))
                   (is (= (:max-tx target-before) (:max-tx @owned)))
                   (is (= (tree target-before) (tree @owned)))
                   (is (= (query/refs target-before) (query/refs @owned)))
                   (is (= operation (adoptions/operation @conn (:id operation))))
                   (is (= 1 (count (d/datoms @conn :aevt :file.adoption/id))))
                   (is (= 1 (count (d/datoms @owned :aevt :publication/id)))))
                 {:owned-first? owned-first? :status (:status result)
                  :operation (:id operation) :proposal proposal :head (:head operation)
                  :synthetic-producer? true :agent-certification-tested? false})
               (finally (d/release output)))))))
      (finally
        ;; The existing fixture has synchronously unregistered and closed its
        ;; Room before returning. Only these captured UUID memory stores are
        ;; deleted; never delete live app homes or re-create stores to inspect.
        (when-let [conn @room-store-conn] (d/release conn))
        (doseq [cfg (distinct @configs)]
          (when-not (and (= :memory (get-in cfg [:store :backend]))
                         (uuid? (get-in cfg [:store :id])))
            (throw (ex-info "Refuse non-fixture cleanup" {:type ::invalid-cleanup})))
          (d/delete-database cfg))))))
