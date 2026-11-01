(ns is.simm.ops.knowledge-failed-settlement
  "Read-only HOST verification of an exact failed knowledge Request. Native
   lifecycle/ledger remain the authorities; no compensation or execution here."
  (:require [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [is.simm.model.access :as access]
            [is.simm.model.knowledge-submitted-recovery :as recovery]
            [is.simm.ops.knowledge-filing :as filing]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [reason]
  (throw (ex-info "Failed update native settlement is not established" {:type ::held :reason reason})))

(defn- returned-resources! [room id]
  ;; Missing resource support is uncertainty, not an unfunded wallet. A missing
  ;; individual account is safe only when no allocation receipt exists either.
  (when-not (satisfies? store/PResourceStore (:store room)) (refuse! :resource-store-unavailable))
  (let [balance (resource/run-wallet-balance room id)
        allocation (store/-resource-receipt (:store room) (resource/allocation-id id))]
    (when-not (and (or balance (nil? allocation))
                   (or (nil? balance) (every? (fn [[_ amount]] (and (number? amount) (zero? amount))) balance)))
      (refuse! :resources-not-returned))))

(defn- world-returned? [r]
  (or (nil? (:run/world r))
      (and (store/family-return-acknowledged? r)
           (= :discarded (:run/settlement-status r))
           (#{:policy :failed :cancelled :settled-elsewhere} (:run/settlement-reason r)))))

(defn verify!
  "Caller authorizes before passing retained Request provenance. Certify either
   the exact failed direct candidate, or native Job failure with no fabricated
   Attempt. A positive family/core/adapter return and empty conserved wallets
   are required before making evidence eligible for any new update."
  [room {:keys [row] :as context}]
  (binding [ec/*execution-context* (:ctx room)]
    (let [id (:work.request/job row) root (run/run room id)
          observation (jobs/observe-settlement room id)
          rows (vec (take (inc filing/max-runs) (run/runs room {:root-run-id id :limit (inc filing/max-runs)})))
          ids (set (map :run/id rows))]
      (when-not (and (uuid? id) (= id (:run/id root)) (= :workflow (:run/kind root))
                     (= (store/conversation-id room) (:run/room root))
                     (= (:work.request/by row) (access/actor->party-uuid (:run/actor root)))
                     (nil? (:run/parent root)) (inst? (:run/ended-at root))
                     (false? (:work.request/return-pending? row))
                     (#{:completed :failed :cancelled} (:run/status root))
                     (= (:run/status root) (:outcome observation))
                     (true? (:settled? observation))
                     (= (:run/family-return-ack root) (get-in observation [:family-return :ack]))
                     (= (:run/job-core-cleanup-ack root) (get-in observation [:core-cleanup :ack]))
                     (= (:run/job-adapter-cleanup-ack root) (get-in observation [:adapter-cleanup :ack]))
                     (<= 1 (count rows) filing/max-runs)
                     (contains? ids id)
                     (not-any? #(contains? ids (:run/id %)) (run/active-runs (:id room))))
        (refuse! :native-family-not-returned))
      (doseq [r rows]
        (when-not (and (= (store/conversation-id room) (:run/room r))
                       (#{:completed :failed :cancelled} (:run/status r)) (inst? (:run/ended-at r))
                       (world-returned? r))
          (refuse! :world-not-discarded))
        (returned-resources! room (:run/id r)))
      (let [proof {:job id :status (:run/status root)
                   :core (:run/job-core-cleanup-ack root) :adapter (:run/job-adapter-cleanup-ack root)
                   :family (:run/family-return-ack root)}]
        (recovery/native-proof!
         (if (= :completed (:run/status root))
           (merge proof {:kind :candidate-failed} (filing/failed-candidate-for-recovery! room context))
           (assoc proof :kind :job-failed)))))))
