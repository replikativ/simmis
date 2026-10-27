(ns is.simm.ops.work-request-checkpoints
  "Internal Request-owned retention. The caller retains original HOST source
   ownership and rechecks current authority; labels/UUIDs are never capabilities."
  (:require [datahike.api :as d]
            [is.simm.model.work-request-checkpoints :as checkpoints]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.checkpoint-roots :as physical]
            [is.simm.ops.knowledge-batch-proof :as batch-proof]))

(defn- refuse! [reason]
  (throw (ex-info "Work request retention is unavailable" {:type ::held :reason reason})))

(defn owner [row body]
  {:simmis/work-request (:work.checkpoint/request row)
   :simmis/root-intent (:work.checkpoint/id row) :claim (:work.checkpoint/claim row)
   :request-intent (:request-intent body) :request-claim (:request-claim body)
   :job (:job body) :policy (get-in body [:selection :policy])})

(defn- batch-binding! [conn request body]
  (when (:work.request/knowledge-batch request)
    (let [proof (batch-proof/request! conn request)]
      (when-not (and (= (get-in body [:selection :basis]) (get-in proof [:selection :basis]))
                     (= (get-in body [:selection :target]) (get-in proof [:source-origin :target])))
        (refuse! :batch-checkpoint-mismatch)))))

(defn verify!
  "Inspect only. Missing/partial roots hold; retry never recreates them.
   Evidence retention survives execution settlement and stream advancement."
  [conn source-db request-id]
  (let [row (or (checkpoints/intent @conn request-id) (refuse! :missing-intent))
        _ (when-not (#{:creating :retained} (:work.checkpoint/state row))
            (refuse! :invalid-state))
        body (checkpoints/payload conn row)
        request (requests/get-request conn request-id)
        _ (batch-binding! conn request body)
        verified (physical/verify! source-db (get-in body [:selection :basis])
                                   (:work.checkpoint/id row) (owner row body))]
    (assoc verified :intent row :body body)))

(defn create!
  "Original serialized relation winner only, within its one live HOST extent.
   Any uncertain native result must be reconciled with verify!, never replayed."
  [conn source-db {:keys [intent new?]}]
  (when-not (true? new?) (refuse! :not-creation-winner))
  (when-not (and (= intent (checkpoints/intent @conn (:work.checkpoint/request intent)))
                 (= :creating (:work.checkpoint/state intent)))
    (refuse! :intent-changed))
  (let [body (checkpoints/payload conn intent)
        request (requests/get-request conn (:request body))]
    (when-not (and (= :submitted (:work.request/state request))
                   (= (:job body) (:work.request/job request)))
      (refuse! :request-changed))
    (batch-binding! conn request body)
    (physical/create! source-db (get-in body [:selection :basis])
                      (:work.checkpoint/id intent) (owner intent body))
    (verify! conn source-db (:request body))))

(defn acknowledge!
  "Physical readback before durable retained state. Safe after lost pin ACK;
   this does not schedule/resume a Job or grant access to historical evidence."
  [conn source-db request-id]
  (let [{:keys [intent]} (verify! conn source-db request-id)]
    (d/transact conn [[:db.fn/call checkpoints/mark-retained-tx intent]])
    (verify! conn source-db request-id)))
