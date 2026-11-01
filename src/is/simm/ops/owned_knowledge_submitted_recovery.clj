(ns is.simm.ops.owned-knowledge-submitted-recovery
  "Fixed HOST recovery of failed submitted wiki updates. Requeue only; never
   dispatch, fund, resume, adopt, release roots or impersonate the requester."
  (:require [datahike.api :as d]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-submitted-recovery :as recovery]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-failed-settlement :as native]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.work :as work]
            [malli.core :as m]))

(def schema
  [:map {:closed true} [:request :uuid] [:batch :uuid] [:expected-claim :uuid]
   [:expected-frozen :uuid] [:request-id :uuid] [:reason [:string {:min 1 :max 1000}]]])

(defn- refuse! [reason]
  (throw (ex-info "Failed knowledge update recovery is held" {:type ::held :reason reason})))

(defn recover!
  "Current actor must have room read/merge authority. Retain the original
   author's physical discovery gate through frozen proof, native observation
   and serialized mutation. An exact replay acknowledges history only."
  [host principal handle request]
  (when-not (m/validate schema request)
    (throw (ex-info "Invalid failed-update recovery arguments" {:type ::invalid})))
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
        (discovery/with-discovery-owner!
          host principal handle author
          (fn [{:keys [room party] :as resolved} lease ticket check!]
            (let [context (filing/recovery-context! control principal room-id (:request request))
                  proof (:retained-proof context)
                  row (:row context) batch (:row proof)
                  _ (when-not (and proof
                                   (= author (:knowledge.batch/by batch) (:work.request/by row))
                                   (= (:batch request) (:knowledge.batch/id batch) (:work.request/knowledge-batch row))
                                   (= (:expected-claim request) (:knowledge.batch/claim batch))
                                   (= (:expected-frozen request) (:knowledge.batch/frozen-id batch))
                                   (#{:submitted :closed} (:knowledge.batch/state batch)))
                      (refuse! :inspected-batch-changed))
                  native (if (= :closed (:knowledge.batch/state batch))
                           (recovery/recorded-native! @control (:batch request) (:request request))
                           (native/verify! room context))
                  occurrence (scheduled/bound-occurrence @control (:batch request))
                ;; retained Request proof already verifies the archived schedule
                ;; input and its bidirectional canonical linkage.
                  expected (recovery/expected proof row native occurrence)
                  value (recovery/decision {:batch (:batch request) :request (:request request) :room room-id
                                            :by party :client (:request-id request) :at (java.util.Date.)
                                            :reason (:reason request)})]
              (check!)
              (when (seq (recovery/requeue-tx @control expected value))
                (d/transact control
                            [[:db.fn/call
                              (fn [current]
                                (discovery/writer-current! host principal handle resolved lease ticket current)
                                (recovery/requeue-tx current expected value))]]))
              (check!)
              (work/authorize-in! (reads/current-host-control-db! host) principal room-id)
              (when-not (empty? (recovery/requeue-tx @control expected value))
                (refuse! :recovery-unconfirmed))
              {:status :recovered :mode :fresh-update :batch (:batch request) :request (:request request)
               :operation (:id value) :dispatched? false :schedule-resumed? false})))))))
