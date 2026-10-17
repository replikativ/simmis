(ns is.simm.ops.worker-credentials
  "Separate, room-attenuated external-worker credentials. No provider login
   or human MCP token is delegated. This module alone installs no endpoint."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [is.simm.model.access :as access]
            [is.simm.model.workers :as workers])
  (:import [java.nio.charset StandardCharsets]
           [java.security MessageDigest]
           [java.util Base64 Date]
           [javax.crypto Mac]
           [javax.crypto.spec SecretKeySpec]))

(def enrollment-tools #{"simmis_worker-pending" "simmis_worker-claim"})
(def shutdown-tools #{"simmis_worker-heartbeat" "simmis_worker-stop"})
(def lease-tools (into shutdown-tools #{"simmis_worker-input" "simmis_worker-complete"}))
(def worker-ops
  {:simmis/worker-pending :enrollment :simmis/worker-claim :enrollment
   :simmis/worker-input :lease :simmis/worker-complete :lease
   :simmis/worker-heartbeat :shutdown :simmis/worker-stop :shutdown})

(defn- hash= [a b]
  (and (string? a) (string? b)
       (MessageDigest/isEqual (.getBytes ^String a StandardCharsets/UTF_8)
                              (.getBytes ^String b StandardCharsets/UTF_8))))

(defn permitted?
  "Enrollment continuously intersects its fixed room with its human enroller's
   CURRENT ReBAC authority. Deleting a party also revokes this delegation."
  [db enrollment]
  (boolean
   (let [by (:simmis.worker/by enrollment) room (:simmis.worker/room enrollment)]
     (and by room (nil? (:simmis.worker/revoked-at enrollment))
          (= :human (d/q '[:find ?type . :in $ ?id
                           :where [?party :party/id ?id] [?party :party/type ?type]] db by))
          (access/can? db by :read {:room room})
          (access/can? db by :merge {:room room})))))

(defn derive-secret
  "Stable, domain-separated enrollment issuance. A lost response can recover
   the SAME worker credential with the same authenticated enrollment intent;
   the database stores only its verifier hash. Host key is never a tool arg."
  [host-secret party room worker]
  (let [hmac (fn [key text]
               (let [mac (Mac/getInstance "HmacSHA256")]
                 (.init mac (SecretKeySpec. ^bytes key "HmacSHA256"))
                 (.doFinal mac (.getBytes ^String text StandardCharsets/UTF_8))))
        key (hmac (.getBytes ^String host-secret StandardCharsets/UTF_8) "simmis/worker-enrollment/v1")]
    (.encodeToString (.withoutPadding (Base64/getUrlEncoder))
                     (hmac key (pr-str [party room worker])))))

(defn enrollment-token [id secret] (str "simmis-worker." id "." secret))
(defn lease-token [run lease secret] (str "simmis-lease." run "." lease "." secret))

(defn- active-lease? [db enrollment row now]
  (boolean
   (and (permitted? db enrollment)
        (= (:simmis.worker/generation enrollment) (:simmis.worker.assignment/generation row))
        (= (:simmis.worker/room enrollment) (:simmis.worker.assignment/room row))
        (contains? #{:claimed :completed} (:simmis.worker.assignment/state row))
        (nil? (:simmis.worker.assignment/cancel-at row))
        (some-> ^Date (:simmis.worker.assignment/expires-at row) (.after ^Date now)))))

(defn authenticate
  "Recognize worker/lease bearers only. The proposed lease proof travels in an
   HTTP authentication header, never MCP arguments or database diagnostics.
   Revoked/expired leases retain ONLY heartbeat/shutdown acknowledgement.
   `now` is a trusted host test seam, never an externally supplied timestamp."
  ([conn request] (authenticate conn request (Date.)))
  ([conn {:keys [bearer headers]} now]
   (try
     (when (and (string? bearer) (<= (count bearer) 256))
       (let [[kind id part secret :as fields] (str/split bearer #"\." -1)
             db @conn]
         (case kind
           "simmis-worker"
           (when (= 3 (count fields))
             (let [id (parse-uuid id) row (workers/enrollment conn id)
                   proposed (get headers "simmis-lease-proof")]
               (when (and (workers/matches-secret? (:simmis.worker/credential-hash row) part)
                          (permitted? db row))
                 (cond-> {:principal/id (str "worker/" id) :principal/kind :worker
                          :principal/mcp? true :principal/scopes #{}
                          :principal/worker id :principal/credential :enrollment
                          :principal/generation (:simmis.worker/generation row)
                          :principal/credential-hash (workers/secret-hash part)
                          :principal/tools (if proposed enrollment-tools #{"simmis_worker-pending"})}
                   proposed (assoc :principal/proof-hash (workers/secret-hash proposed))))))

           "simmis-lease"
           (when (= 4 (count fields))
             (let [id (parse-uuid id) lease (parse-uuid part)
                   row (workers/assignment conn id)
                   enrollment (workers/enrollment conn (:simmis.worker.assignment/worker row))]
               (when (and lease (= lease (:simmis.worker.assignment/lease row))
                          (workers/matches-secret? (:simmis.worker.assignment/secret-hash row) secret))
                 {:principal/id (str "worker/" (:simmis.worker.assignment/worker row) "/lease/" id)
                  :principal/kind :worker :principal/mcp? true :principal/scopes #{}
                  :principal/worker (:simmis.worker.assignment/worker row)
                  :principal/assignment id :principal/lease lease :principal/credential :lease
                  :principal/generation (:simmis.worker.assignment/generation row)
                  :principal/proof-hash (workers/secret-hash secret)
                  :principal/tools (if (active-lease? db enrollment row now) lease-tools shutdown-tools)})))
           nil)))
     (catch Exception _ nil))))

(defn authorize
  "Recheck live ownership and room grants even for an already-open MCP
   session. This authorizer must replace, not supplement, human authority for
   worker principals. It never maps a worker to its enroller's human identity."
  ([conn principal request] (authorize conn principal request (Date.)))
  ([conn principal {:keys [op args]} now]
   (let [db @conn
         worker (workers/enrollment conn (:principal/worker principal))
         row (when-let [id (:principal/assignment principal)] (workers/assignment conn id))
         kind (get worker-ops op)
         enrollment? (= :enrollment (:principal/credential principal))
         owns? (if enrollment?
                 (and (= (:principal/generation principal) (:simmis.worker/generation worker))
                      (hash= (:principal/credential-hash principal) (:simmis.worker/credential-hash worker)))
                 (and (= (:principal/worker principal) (:simmis.worker.assignment/worker row))
                      (= (:principal/generation principal) (:simmis.worker.assignment/generation row))
                      (= (:principal/lease principal) (:simmis.worker.assignment/lease row))
                      (hash= (:principal/proof-hash principal) (:simmis.worker.assignment/secret-hash row))))
         allow? (boolean
                 (and (= :worker (:principal/kind principal)) kind owns?
                      (contains? (:principal/tools principal) (str "simmis_" (name op)))
                      ;; A lease can never substitute another Run handle.
                      (or enrollment? (nil? args) (= (str (:principal/assignment principal)) (:run args)))
                      (case kind
                        :enrollment (and enrollment? (permitted? db worker)
                                         (or (not= op :simmis/worker-claim) (:principal/proof-hash principal)))
                        :lease (and (not enrollment?) (active-lease? db worker row now))
                        :shutdown (not enrollment?)
                        false)))]
     {:allow? allow? :reason (when-not allow? "Worker credential does not authorize this operation")})))
