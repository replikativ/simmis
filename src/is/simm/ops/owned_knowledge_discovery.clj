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
            [is.simm.model.knowledge-batch-recovery :as recovery]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.knowledge-batch-proof :as batch-proof]
            [is.simm.ops.owned-room-operation :as room-operation]
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

(defn- with-room!
  ([host principal handle f] (with-room! host principal handle nil f))
  ([host principal handle original-author f]
   (when-not (or (nil? original-author) (uuid? original-author)) (refuse! :invalid-discovery-owner))
   (let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
     (when-not (fn? with-operation) (refuse! :missing-lifetime))
     (with-operation
       (fn []
         (lifetime/check-current!)
         (let [resolved (reads/resolve-host-room! host principal handle) room (:room resolved)]
           (binding [ec/*execution-context* (:execution-ctx host)]
             (preparation/with-host-operation!
              ;; Recovery uses the original publisher's physical gate, derived
              ;; from canonical batch metadata. Authorization stays with ACTOR.
              room {:exclusive-key [::discovery (or original-author (:party resolved))]}
              (fn [lease]
                (binding [ec/*execution-context* (:ctx room)]
                  (let [check! #(do (lifetime/check-current!)
                                    (preparation/with-host-current! lease (constantly true))
                                    (reads/validate-host-room! host principal handle resolved))]
                    (check!)
                    (f resolved lease lifetime/*ticket* check!))))))))))))

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

(defn- active-batch-reply [db value expected]
  (when-let [id (:knowledge.stream/active-batch expected)]
    (let [row (batches/batch db id)]
      (when-not (and row (= (:by value) (:knowledge.batch/by row))
                     (= (:room value) (:knowledge.batch/room row))
                     (= (:target value) (:knowledge.batch/target row))
                     (= (:id value) (:knowledge.batch/stream row))
                     (#{:capturing :ready :submitted :review} (:knowledge.batch/state row))
                     (= id (:knowledge.batch/selection-id row))
                     (every? uuid? [(:knowledge.batch/claim row) (:knowledge.batch/selection row)])
                     (let [state (:knowledge.batch/state row)
                           frozen [(:knowledge.batch/frozen row) (:knowledge.batch/frozen-id row)]
                           request [(:knowledge.batch/request row) (:knowledge.batch/request-intent row)]]
                       (case state
                         :capturing (every? nil? (concat frozen request))
                         :ready (and (every? uuid? frozen) (every? nil? request))
                         (:submitted :review) (every? uuid? (concat frozen request))
                         false)))
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
                   (let [db @control]
                     (active-batch-reply db value (streams/stream db (:id value))))))]
    held
    (do
     (refresh! host principal handle request)
  (with-stream! host principal handle request
    (fn [{:keys [control binding value policy planned snapshot origin mapping check! transact!] :as context}]
      (let [declaration (batch-declaration! request context)
            db @control
            expected (streams/stream db (:id value))]
        (if-let [held (active-batch-reply db value expected)]
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


(def ^:private abandon-ready-schema
  [:map {:closed true} [:batch :uuid] [:expected-claim :uuid] [:expected-frozen :uuid]
   [:request-id :uuid] [:reason [:string {:min 1 :max 1000}]]])

(defn- rethrow-interrupted! [error]
  (when (or (.isInterrupted (Thread/currentThread))
            (loop [cause error seen #{}]
              (cond (nil? cause) false
                    (instance? InterruptedException cause) true
                    (contains? seen cause) false
                    :else (recur (.getCause ^Throwable cause) (conj seen cause)))))
    (.interrupt (Thread/currentThread)))
  (throw error))

(defn abandon-ready!
  "Explicit human skip of an inspected unlinked ready batch. Current Room
   merge authority precedes artifacts and repeats in the writer. No current
   source/default/model lookup, inference, refund or automatic schedule resume.
   Lost ACK may throw; repeat the SAME request-id to inspect the joint result."
  [host principal handle request]
  (when-not (m/validate abandon-ready-schema request) (refuse! :invalid-recovery))
  (try
    (room-operation/with-app!
     host
     (fn []
       (let [{:keys [control-db room-id]} (reads/preflight-host-read! host principal handle)
             _ (work/authorize-in! control-db principal room-id)
             control (get-in host [::reads/owner :control-conn])
             initial (batches/batch control-db (:batch request))
             author (:knowledge.batch/by initial)]
         (when-not (and (uuid? author) (= room-id (:knowledge.batch/room initial)))
           (refuse! :batch-unavailable))
         (with-room!
          host principal handle author
          (fn [resolved lease ticket check!]
            (let [db (reads/current-host-control-db! host)
                  _ (work/authorize-in! db principal room-id)
                  b (batches/batch db (:batch request))]
              (when-not (and b (= [author room-id (:expected-claim request) (:expected-frozen request)]
                               (mapv b [:knowledge.batch/by :knowledge.batch/room :knowledge.batch/claim :knowledge.batch/frozen-id]))
                             (or (= :ready (:knowledge.batch/state b))
                                 (and (= :closed (:knowledge.batch/state b))
                                      (= :abandoned-skip (:knowledge.batch/outcome b))
                                      (= :ready (:knowledge.batch/recovery-from b)))))
                (refuse! :batch-not-ready-for-recovery))
              (let [origin {:batch (:knowledge.batch/id b) :selection (:knowledge.batch/selection-id b)
                            :frozen (:knowledge.batch/frozen-id b)}
                    proof (batch-proof/retained! control author room-id origin)
                    o (scheduled/bound-occurrence @control (:batch request))
                    anchor (when o {:value (scheduled/retained control o) :reference (:schedule.occurrence/batch-ref o)})
                    expected (recovery/expected proof anchor)
                    value (recovery/decision {:batch (:batch request) :room room-id :by (:party resolved)
                                              :client (:request-id request) :at (java.util.Date.)
                                              :mode :skip :reason (:reason request)})]
                (check!)
                (when (seq (recovery/abandon-tx @control expected value))
                  (d/transact control
                    [[:db.fn/call
                      (fn [current]
                        (writer-current! host principal handle resolved lease ticket current)
                        (recovery/abandon-tx current expected value))]]))
                (check!)
                (work/authorize-in! (reads/current-host-control-db! host) principal room-id)
                ;; This is observation of the complete terminal result, never a
                ;; new native action or a reconstructed dispatch acknowledgement.
                (when-not (empty? (recovery/abandon-tx @control expected value)) (refuse! :recovery-unconfirmed))
                {:status :abandoned :batch (:batch request) :operation (:id value) :mode :skip})))))))
    (catch Throwable error (rethrow-interrupted! error))))
