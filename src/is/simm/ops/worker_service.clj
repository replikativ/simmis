(ns is.simm.ops.worker-service
  "Bounded, sanitized worker transport over existing durable ownership.
   No worker verdict, unrestricted room API or direct trunk write is accepted."
  (:require [datahike.api :as d]
            [is.simm.model.access :as access]
            [is.simm.model.workers :as workers]
            [is.simm.ops.worker-credentials :as credentials]
            [is.simm.ops.worker-files :as files]
            [malli.core :as m])
  (:import [java.util Date]))

(def ttl-ms 30000)
(defn- clock [] (Date.))
(def ^:private uuid-string [:string {:min 36 :max 36}])
(def pending-schema [:map {:closed true}])
(def claim-schema [:map {:closed true} [:run uuid-string] [:nonce uuid-string]])
(def run-schema [:map {:closed true} [:run uuid-string]])
(def input-schema
  [:map {:closed true} [:run uuid-string]
   [:offset {:optional true} [:int {:min 0 :max 4194304}]]
   [:length {:optional true} [:int {:min 1 :max 16384}]]])
(def complete-schema [:map {:closed true} [:run uuid-string] [:output files/transport-output-schema]])

(defn- refuse! [type message] (throw (ex-info message {:type type})))
(defn- checked! [schema args]
  (when-not (m/validate schema args) (refuse! ::invalid "Invalid worker operation arguments")))

(defn- human! [conn principal room]
  (let [by (access/actor->party-uuid (:principal/id principal))]
    (when-not (and (= :human (:principal/kind principal)) by
                   (credentials/permitted? @conn {:simmis.worker/by by :simmis.worker/room room}))
      (refuse! ::denied "Current human room read and merge authority is required"))
    by))

(defn enroll!
  "Stable worker UUID/configuration make lost enrollment responses retryable.
   Return the credential ONLY to the authenticated enrolling local client."
  [conn principal room host-secret worker config]
  (when-not (and (uuid? worker) (uuid? room) (m/validate files/config-schema config))
    (refuse! ::invalid "Invalid closed worker configuration"))
  (let [by (human! conn principal room)
        secret (credentials/derive-secret host-secret by room worker)
        row (workers/enroll! conn {:id worker :by by :room room :secret secret :config config})]
    (when (:simmis.worker/revoked-at row) (refuse! ::denied "Worker was revoked; enroll a new identity explicitly"))
    {:worker worker :room room :generation (:simmis.worker/generation row)
     :config-id (:simmis.worker/config-id row) :config config
     :credential (credentials/enrollment-token worker secret)}))

(defn revoke! [conn principal room worker]
  (human! conn principal room)
  (let [row (workers/enrollment conn worker)]
    (when-not (= room (:simmis.worker/room row)) (refuse! ::missing "No such worker in this room"))
    (workers/revoke! conn worker)
    {:worker worker :revoked true}))

(defn- worker! [conn principal op args now]
  (when-not (:allow? (credentials/authorize conn principal {:op op :args args} now))
    (refuse! ::denied "Worker credential does not authorize this operation")))

(defn- receipt [row]
  (cond-> {:run (:simmis.worker.assignment/id row) :state (:simmis.worker.assignment/state row)
           :cancel-requested? (boolean (:simmis.worker.assignment/cancel-at row))
           :stop-ack? (boolean (:simmis.worker.assignment/stop-ack-at row))}
    (:simmis.worker.assignment/lease row) (assoc :lease (:simmis.worker.assignment/lease row))
    (:simmis.worker.assignment/expires-at row)
    (assoc :expires-at-ms (.getTime ^Date (:simmis.worker.assignment/expires-at row)))
    (:simmis.worker.assignment/output-id row) (assoc :output-id (:simmis.worker.assignment/output-id row))))

(defn pending [conn principal args]
  (checked! pending-schema args)
  (worker! conn principal :simmis/worker-pending args (Date.))
  {:runs (->> (d/q '[:find [?id ...] :in $ ?worker
                     :where [?a :simmis.worker.assignment/worker ?worker]
                     [?a :simmis.worker.assignment/state :queued]
                     [?a :simmis.worker.assignment/id ?id]] @conn (:principal/worker principal))
              (sort-by str) (take 10) vec)})

(defn claim! [conn principal {:keys [run nonce] :as args}]
  (checked! claim-schema args)
  (let [now (Date.) id (parse-uuid run) nonce (parse-uuid nonce)]
    (when-not (and id nonce) (refuse! ::invalid "Invalid claim identity"))
    (worker! conn principal :simmis/worker-claim args now)
    (receipt (workers/claim-hashed! conn id (:principal/worker principal) (:principal/generation principal)
                                    nonce (:principal/proof-hash principal) clock ttl-ms credentials/permitted?))))

(defn input [conn principal args]
  (checked! input-schema args)
  (worker! conn principal :simmis/worker-input args (Date.))
  (let [row (workers/assignment conn (:principal/assignment principal))
        frozen (workers/payload conn row :simmis.worker.assignment/input-ref)]
    (files/input-chunk frozen (get args :offset 0) (get args :length (:chunk-bytes files/policy)))))

(defn heartbeat! [conn principal args]
  (checked! run-schema args)
  (let [now (Date.)]
    (worker! conn principal :simmis/worker-heartbeat args now)
    (receipt (workers/heartbeat-hashed! conn (:principal/assignment principal) (:principal/lease principal)
                                        (:principal/proof-hash principal) clock ttl-ms credentials/permitted?))))

(defn complete! [conn principal args]
  (checked! complete-schema args)
  (let [now (Date.)]
    (worker! conn principal :simmis/worker-complete args now)
    (let [row (workers/assignment conn (:principal/assignment principal))
          frozen (workers/payload conn row :simmis.worker.assignment/input-ref)
          output (files/validate-output! frozen (files/transport-output (:output args)))]
      (receipt (workers/complete-hashed! conn (:principal/assignment principal) (:principal/lease principal)
                                         (:principal/proof-hash principal) output clock credentials/permitted?)))))

(defn stop! [conn principal args]
  (checked! run-schema args)
  (let [now (Date.)]
    (worker! conn principal :simmis/worker-stop args now)
    (workers/acknowledge-stop-hashed! conn (:principal/assignment principal) (:principal/lease principal)
                                      (:principal/proof-hash principal) now)
    ;; No configuration, frozen input, output, room details, hashes or refs.
    {:run (:principal/assignment principal) :stop-ack? true}))
