(ns is.simm.ops.owned-knowledge-discovery
  "Internal fixed HOST discovery. No public registration, model dispatch or
   processing receipt. Native IO stays within the original App/Room operation."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.room.registry :as registry]
            [dvergr.runtime.preparation :as preparation]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.system.rooms :as native-rooms]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.knowledge-batch-inputs :as batch-inputs]
            [is.simm.ops.knowledge-checkpoint-roots :as checkpoints]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as owned-workflows]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]))

(def refresh-schema
  ;; First live slice uses the ordinary workflow's default parameters and no
  ;; benchmark case/explicit frozen inputs. Extend retained invocation data
  ;; before admitting overrides, rather than guessing them during recovery.
  [:map {:closed true} [:name workflows/name-schema] [:expected-source :uuid]
   [:expected-bundle :uuid] [:epoch :uuid]])

(defn- refuse! [reason]
  (throw (ex-info "Knowledge discovery is held" {:type ::held :reason reason})))

(defn- with-room! [host principal handle f]
  (let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (when-not (fn? with-operation) (refuse! :missing-lifetime))
    (with-operation
      (fn []
        (lifetime/check-current!)
        (let [resolved (reads/resolve-host-room! host principal handle) room (:room resolved)]
          (binding [ec/*execution-context* (:execution-ctx host)]
            (preparation/with-host-operation!
              ;; One short metadata/native-retention operation per human Room
              ;; owner. Recovery and refresh share the SAME physical gate even
              ;; when the retained program/default is no longer current.
              room {:exclusive-key [::discovery (:party resolved)]}
              (fn [lease]
                (binding [ec/*execution-context* (:ctx room)]
                  (let [check! #(do (lifetime/check-current!)
                                    (preparation/with-host-current! lease (constantly true))
                                    (reads/validate-host-room! host principal handle resolved))]
                    (check!)
                    (f resolved lease lifetime/*ticket* check!)))))))))))

(defn- writer-current!
  "Short observations on a writer thread, never thread-entry admission or
   source IO. The original caller still owns and physically joins its lease."
  [host principal handle resolved lease ticket db]
  (reads/current-host-control-db! host)
  (when-not (and (lifetime/retained? ticket) (lifetime/ready? (:owner ticket)))
    (refuse! :application-retired))
  (let [room (:room resolved)]
    (locking (:meta room)
      (when-not (and (= :pending @(:state lease))
                     (identical? lease (get-in @(:meta room) [preparation/leases-key (:id lease)]))
                     (binding [ec/*execution-context* (:ctx room)]
                       (and (identical? room (registry/lookup (:id room)))
                            (registry/admitted-incarnation? room)))
                     (nil? (:dvergr/fork-transfer-state @(:meta room))))
        (refuse! :room-retired))))
  (when-not (= (select-keys resolved [:party :room-id :slug])
               (reads/authorize-read! db principal handle))
    (refuse! :room-changed))
  (work/authorize-in! db principal (:room-id resolved))
  true)

(defn- source-revision [binding name]
  (owned-workflows/source-id binding name (workflows/manifest-in @(:conn binding) name)))

(defn- capture! [binding {:keys [name expected-source expected-bundle epoch]}]
  (let [db (:db binding) manifest (workflows/manifest-in db name)]
    (when-not (= expected-source (owned-workflows/source-id binding name manifest))
      (refuse! :program-changed))
    (let [snapshot (workflows/snapshot-in db name)
          planned (runs/plan-source snapshot
                                    {:name name :expected-bundle expected-bundle})
          policy (some-> (:selection-policy planned) inputs/normalize-policy)]
      (when-not policy (refuse! :missing-knowledge-policy))
      (when (contains? (:inputs planned) "/in/knowledge/manifest.edn")
        (refuse! :benchmark-inputs-are-not-a-live-stream))
      {:policy policy :planned planned :snapshot snapshot
       :origin {:target (:target binding) :target-stamp (:target-stamp binding)
                :workflow name :source-revision expected-source}
       :value (streams/descriptor
                {:by (:party binding) :room (:room-id binding)
                 :target (pins/target-id (:target binding)) :policy (hasch/uuid policy)
                 :program (hasch/uuid [:simmis/knowledge-program-v1
                                      (select-keys (:workflow planned) [:name :bundle :case :params])
                                      (:task planned) (:inputs planned)]) :epoch epoch})})))

(defn- plan! [control source-db value expected policy new]
  (if-let [root (:knowledge.stream/cursor-root expected)]
    (let [previous (checkpoints/verify! control source-db root)]
      (when-not (and (= value (get-in previous [:body :stream]))
                     (= (:knowledge.stream/cursor-commit expected) (get-in previous [:basis :commit]))
                     (= (:knowledge.stream/cursor-tx expected) (get-in previous [:basis :max-tx])))
        (refuse! :cursor-pin-mismatch))
      (select-keys (changes/between (:db previous) (:db new) policy) [:before :after :changes]))
    (let [inventory (changes/inventory (:db new) policy)]
      {:before nil :after (:basis inventory)
       :changes (mapv (fn [[path entry]] {:path path :kind :added :before nil :after entry})
                      (:files inventory))})))

(defn- finish! [control source-db intent check! transact!]
  ;; Validate the joint journal/cursor BEFORE releasing any old native root.
  ;; An uncertain release is inspected on recovery; a replacement is never made.
  (if (#{:retained :released} (:knowledge.pin/state intent))
    (do (pins/finish-tx @control intent)
        {:status (:knowledge.pin/state intent) :operation (:knowledge.pin/id intent)})
    (do
      (pins/finish-tx @control intent)
      (checkpoints/verify! control source-db (:knowledge.pin/id intent))
      (check!)
      (checkpoints/release-previous! control source-db intent)
      (transact! #(pins/finish-tx % intent))
      (let [finished (pins/intent @control (:knowledge.pin/id intent))]
        (when-not (= :retained (:knowledge.pin/state finished)) (refuse! :cleanup-unconfirmed))
        {:status :retained :operation (:knowledge.pin/id intent)}))))

(defn- stream-context! [host principal handle request resolved lease ticket room-check!]
  (let [control (get-in host [::reads/owner :control-conn])
        binding (reads/resolve-host-read! host principal handle)
        _ (when-not (identical? (:room resolved) (:room binding)) (refuse! :room-changed))
        captured (capture! binding request)
        mapping (owned/control-revision @control (:room-id binding) (get-in binding [:target :registry-system]))
        check! (fn []
                 (room-check!)
                 (owned/validate! (reads/current-host-control-db! host) principal :merge
                                  (:room-id binding) (:room binding) binding)
                 (when-not (= (:expected-source request) (source-revision binding (:name request)))
                   (refuse! :program-changed)))
        transact! (fn [fragment]
                    (d/transact control
                      [[:db.fn/call (fn [db]
                                     (writer-current! host principal handle resolved lease ticket db)
                                     (owned/validate-control-in! db principal (:room-id binding) binding mapping)
                                     (fragment db))]]))]
    (check!)
    (assoc captured :control control :binding binding :mapping mapping :check! check! :transact! transact!)))

(defn refresh!
  "Metadata-only refresh of the current authorized default workflow stream.
   Closed internal input; no override parameters/cases, provider or execution."
  [host principal handle request]
  (when-not (m/validate refresh-schema request) (refuse! :invalid-request))
  (with-room! host principal handle
    (fn [resolved lease ticket room-check!]
      (let [{:keys [control binding value policy origin check! transact!]}
            (stream-context! host principal handle request resolved lease ticket room-check!)]
        (check!)
        (let [expected (streams/stream @control (:id value))
              active (pins/active @control (:id value))
              basis (changes/basis (:db binding))]
          (if (and (nil? active) (= (:commit basis) (:knowledge.stream/cursor-commit expected)))
            (do (checkpoints/verify! control (:db binding) (:knowledge.stream/cursor-root expected))
                (check!)
                {:status :unchanged :stream (:id value)
                 :pending (count (streams/pending-items @control (:id value)))})
            (let [prepared
                  (if active
                    {:intent active :new? false}
                    (let [{:keys [payload digest root claim] :as prepared}
                          (pins/preparation value expected basis origin)]
                      (artifact/publish-value! (artifact/datahike-store control) payload
                        (fn [ref]
                          (check!)
                          (transact! #(pins/prepare-tx % value expected digest ref root claim))))
                      (pins/preparation-result @control prepared)))
                  _ (check!)
                  verified (if (:new? prepared)
                             (checkpoints/create! control (:db binding) prepared)
                             (checkpoints/verify! control (:db binding) (get-in prepared [:intent :knowledge.pin/id])))
                  body (:body verified)]
              (when-not (and (= value (:stream body)) (= origin (:origin body)))
                (refuse! :original-program-changed))
              (check!)
              (when (= :creating (get-in verified [:intent :knowledge.pin/state]))
                (transact! #(pins/mark-pinned-tx % (:intent verified))))
              (let [intent (pins/intent @control (get-in prepared [:intent :knowledge.pin/id]))]
                (when (= :pinned (:knowledge.pin/state intent))
                  (let [plan (plan! control (:db binding) value (:expected body) policy verified)]
                    (check!)
                    (artifact/publish-value! (artifact/datahike-store control) plan
                      (fn [ref]
                        (check!)
                        (transact! #(pins/discovery-tx % intent value (:expected body) plan (:origin body) ref))))))
                ;; Cleanup belongs to this captured physical source. It does
                ;; not authorize more ingestion if the default/program changes.
                (let [current (pins/intent @control (:knowledge.pin/id intent))
                      result (finish! control (:db binding) current
                                      #(lifetime/check-current!)
                                      (fn [fragment]
                                        (d/transact control [[:db.fn/call fragment]])))]
                  (assoc result :stream (:id value)
                         :pending (count (streams/pending-items @control (:id value)))))))))))))

(defn- with-stream! [host principal handle request f]
  (with-room! host principal handle
    (fn [resolved lease ticket room-check!]
      (f (stream-context! host principal handle request resolved lease ticket room-check!)))))

(defn- batch-declaration! [request context]
  (let [declaration (batch-inputs/declaration (get-in context [:snapshot :files]))]
    (when-not (and declaration (= (:epoch request) (:epoch declaration)))
      (refuse! :batching-not-enabled-for-this-epoch))
    declaration))

(defn- active-batch-reply [control value expected]
  (when-let [id (:knowledge.stream/active-batch expected)]
    (let [row (batches/batch @control id)]
      (when-not (and row (= (:by value) (:knowledge.batch/by row))
                     (= (:room value) (:knowledge.batch/room row))
                     (= (:target value) (:knowledge.batch/target row))
                     (= (:id value) (:knowledge.batch/stream row)))
        (refuse! :batch-binding-mismatch))
      {:status :held :reason :active-batch :stream (:id value) :batch id
       :batch-state (:knowledge.batch/state row)})))

(defn prepare-batch!
  "Internal opt-in claim and durable freeze, with NO request or dispatch.
   Returns only resource-independent source/byte data after releasing its source
   lease. The later canonical admission must reauthorize and verify this exact
   origin in its hashed intent. Active retries revalidate bounded workflow
   source, but never recapture evidence/context or manufacture dispatch ownership."
  [host principal handle request]
  (when-not (m/validate refresh-schema request) (refuse! :invalid-request))
  ;; Validate opt-in before any discovery effect; do not acquire the discovery
  ;; gate recursively. Every subsequent physical extent repeats current gates.
  (if-let [held (with-stream! host principal handle request
                 (fn [{:keys [control value] :as context}]
                   (batch-declaration! request context)
                   (active-batch-reply control value (streams/stream @control (:id value)))))]
    held
    (do
     (refresh! host principal handle request)
  (with-stream! host principal handle request
    (fn [{:keys [control binding value policy planned snapshot origin mapping check! transact!] :as context}]
      (let [declaration (batch-declaration! request context)
            expected (streams/stream @control (:id value))]
        (if-let [held (active-batch-reply control value expected)]
          held
          (let [pinned (checkpoints/verify! control (:db binding) (:knowledge.stream/cursor-root expected))
                _ (when-not (= value (get-in pinned [:body :stream])) (refuse! :cursor-pin-mismatch))
                reserved {:files (count (:inputs planned))
                          :bytes (reduce + 0 (map #(alength (.getBytes ^String % "UTF-8")) (vals (:inputs planned))))}
                selection (batch-inputs/selection (:db pinned) value expected
                            (streams/pending-items @control (:id value)) policy (:policy declaration) reserved)]
            (if (= :empty (:status selection))
              (do (check!) {:status :empty :stream (:id value)})
              (let [selected (:batch selection) claim (random-uuid)
                    ;; Reject deterministic occupancy bounds before claiming.
                    ;; Capture repeats this bounded metadata read on the SAME
                    ;; immutable snapshot when forming its retained envelope.
                    _ (inputs/publication-basis (:db pinned)
                                               (select-keys (:target binding) [:registry-system :runtime-system]))]
                (check!)
                (artifact/publish-value! (artifact/datahike-store control) selected
                  (fn [ref]
                    (check!)
                    (transact! #(batches/claim-tx % selected ref claim))))
                (let [{:keys [batch new?]} (batches/claim-result @control (:id selected) claim)]
                  (when-not new? (refuse! :capture-not-owned))
                  (check!)
                  (let [frozen (batch-inputs/capture (:db pinned) selection
                                                   (select-keys (:target binding) [:registry-system :runtime-system]))
                        digest (hasch/uuid frozen)]
                    (check!)
                    (artifact/publish-value! (artifact/datahike-store control) frozen
                      (fn [ref]
                        (check!)
                        (transact! #(batches/freeze-tx % batch ref digest))))
                    (let [current (batches/batch @control (:id selected))
                          retained (:value (artifact/get-bounded-value (artifact/datahike-store control)
                                              (:knowledge.batch/frozen current) batch-inputs/max-frozen-bytes))]
                      (when-not (and (= :ready (:knowledge.batch/state current))
                                     (= digest (:knowledge.batch/frozen-id current) (hasch/uuid retained))
                                     (= selected (:batch retained)))
                        (refuse! :frozen-inputs-unconfirmed))
                      (batch-inputs/verify retained)
                      (check!)
                      {:status :ready :stream (:id value) :batch (:id selected)
                       :batch-origin {:batch (:id selected) :selection (:id selected) :frozen digest}
                       :control-revision mapping
                       :captured (:captured retained) :publication-basis (:publication-basis retained)
                       :remaining (:remaining selected)
                       :snapshot snapshot :source-origin origin})))))))))))))

(defn recover!
  "Finish only a committed cleanup on its exact original registered source.
   Creating/pinned discovery resumes through refresh! with inspected program
   preconditions. Never create a missing root or resolve a new default here."
  [host principal handle {:keys [operation] :as request}]
  (when-not (and (= #{:operation} (set (keys request))) (uuid? operation))
    (refuse! :invalid-request))
  (with-room! host principal handle
    (fn [resolved _ _ check!]
      (let [control (get-in host [::reads/owner :control-conn])
            intent (pins/intent @control operation)]
        (when-not (and intent (= (:party resolved) (:knowledge.pin/by intent))
                       (= (:room-id resolved) (:knowledge.pin/room intent)))
          (refuse! :unknown-operation))
        (let [{:keys [origin]} (pins/payload control intent)
              current (pins/intent @control operation)]
            (when-not (#{:committed :retained :released} (:knowledge.pin/state current))
              (refuse! :discovery-needs-refresh))
            (if (#{:retained :released} (:knowledge.pin/state current))
              (do (pins/finish-tx @control current)
                  {:status (:knowledge.pin/state current) :operation operation})
              (let [target (:target origin)
                    registered (native-rooms/registry-system-for (reads/current-host-control-db! host) (:runtime-system target))
                    _ (when-not (and (= :repo (:system/type registered))
                                     (= (:registry-system target) (:system/id registered)))
                        (refuse! :original-system-unavailable))
                    system (ygg/system (:runtime-system target))
                    _ (when-not system (refuse! :original-system-unavailable))
                    source (workspace/workspace-for-system! system)]
                (finish! control @(:conn source) current check!
                         (fn [fragment]
                           (check!)
                           (d/transact control [[:db.fn/call fragment]]))))))))))
