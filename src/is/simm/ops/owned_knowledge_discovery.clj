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
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.knowledge-changes :as changes]
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
    (let [planned (runs/plan-source (workflows/snapshot-in db name)
                                    {:name name :expected-bundle expected-bundle})
          policy (some-> (:selection-policy planned) inputs/normalize-policy)]
      (when-not policy (refuse! :missing-knowledge-policy))
      (when (contains? (:inputs planned) "/in/knowledge/manifest.edn")
        (refuse! :benchmark-inputs-are-not-a-live-stream))
      {:policy policy
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

(defn refresh!
  "Metadata-only refresh of the current authorized default workflow stream.
   Closed internal input; no override parameters/cases, provider or execution."
  [host principal handle request]
  (when-not (m/validate refresh-schema request) (refuse! :invalid-request))
  (with-room! host principal handle
    (fn [resolved lease ticket room-check!]
      (let [control (get-in host [::reads/owner :control-conn])
            binding (reads/resolve-host-read! host principal handle)
            _ (when-not (identical? (:room resolved) (:room binding)) (refuse! :room-changed))
            {:keys [value policy origin]} (capture! binding request)
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
