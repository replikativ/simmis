(ns is.simm.ops.knowledge-publish
  "Host-only durable filing of certified knowledge output. Not an RPC, Run,
   approval mechanism, or file adoption endpoint. The canonical request owns
   one immutable disposition, atomically joined to its optional Proposal."
  (:require [datahike.api :as d]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.proposals :as proposals]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [reason]
  (throw (ex-info "Knowledge output filing was refused" {:type ::refused :reason reason})))

(defn- canonical-connection! [conn]
  (when-not (and conn (identical? conn (sdb/get-conn)))
    (refuse! :noncanonical-connection)))

(defn- request-row [db id]
  (d/pull db '[*] [:work.request/id id]))

(defn- linked-proposal? [db row value]
  ;; Reuse the canonical unique owner resolver and bounded metadata validator.
  ;; Historical retries must not accept a surviving header whose entries or
  ;; immutable metadata were removed/corrupted. No bodies or Attempts are read.
  (when-let [owner (manifests/owner db (:manifest value))]
    (let [manifest (storage/manifest-in db (:manifest value))
          proposal (d/pull db '[:proposal/id :proposal/run :proposal/author :proposal/room]
                           (:proposal-eid owner))]
      (and (= (:proposal value) (:proposal owner) (:proposal/id proposal))
           (= (:work.request/room row) (:room owner) (:room manifest) (:proposal/room proposal))
           (= (:work.request/by row) (:proposal/author proposal))
           (= (:candidate value) (:attempt manifest) (:proposal/run proposal))
           (= (:request value) (:request manifest))
           (= (:basis value) (:basis manifest))
           (= (:output value) (:output manifest))
           (seq (:entries manifest))))))

(defn- complete-disposition! [db row]
  (when-let [value (dispositions/disposition row)]
    (when (and (= :proposed (:kind value)) (not (linked-proposal? db row value)))
      (refuse! :partial-filing))
    value))

(defn- authorize-request! [db principal room-id row]
  (let [party (filing/authorize! db principal room-id)]
    (when-not (and (= room-id (:work.request/room row))
                   (= party (:work.request/by row)))
      (refuse! :request-owner-mismatch))
    party))

(defn- commit-tx
  "Specific serialized writer composition. No nested transaction, Room lock,
   hydration, provider, artifact IO or chat publication is allowed here. Live
   target checks are an observation, never a lease extending past this filing."
  [db principal room-id room prepared entity]
  (binding [ec/*execution-context* (:ctx room)]
    (let [{:keys [request-binding disposition target party]} prepared
          row (request-row db (:request disposition))
          current-party (authorize-request! db principal room-id row)
          _ (when-not (= party current-party) (refuse! :author-changed))
          _ (when-not (= target (filing/current-target db room-id room))
              (refuse! :target-changed))
          existing (complete-disposition! db row)
          disposition-tx (dispositions/tx-data db request-binding disposition)
          proposal-tx (when entity (proposals/filing-tx-data db entity))]
      (case (:kind disposition)
        :no-op
        (when entity (refuse! :unexpected-proposal))
        :proposed
        (do
          (when-not (= (:proposal disposition) (:proposal/id entity))
            (refuse! :proposal-mismatch))
        ;; Neither a Proposal-only nor a receipt-only partial write can be
        ;; silently adopted as this host filing, even with matching UUIDs.
          (when-not (= (boolean existing) (empty? proposal-tx))
            (refuse! :partial-filing))))
      (into (vec proposal-tx) disposition-tx))))

(defn- record! [conn principal room-id room prepared entity]
  (let [value (:disposition prepared)]
    (try
      ;; Capture host handles in this fixed function rather than placing Room
      ;; records/contexts in transaction arguments or persisted entities.
      (d/transact conn [[:db.fn/call (fn [db]
                                       (commit-tx db principal room-id room prepared entity))]])
      (catch InterruptedException error
        (.interrupt (Thread/currentThread))
        (throw error))
      (catch Exception error
        ;; A lost ACK is resolved only by the exact durable joint filing. A
        ;; conflicting or partial row is not permission to execute work again.
        (let [db @conn row (request-row db (:request value))]
          (authorize-request! db principal room-id row)
          (when-not (= value (complete-disposition! db row)) (throw error)))))
    value))

(defn- proposal-entity [prepared retained]
  (let [{:keys [plan party room disposition]} prepared
        manifest (:manifest plan)
        n (count (:entries manifest))]
    (proposals/prepare-filing
     {:id (:proposal plan) :title (str "Wiki update — " n " file" (when (not= 1 n) "s"))
      :summary "Certified workflow output retained for review. Adoption is not yet available."
      :author party :room room :run (:candidate disposition)
      :forks [{:scope (:id manifest) :authority-scope room
               :system-type :file-output :branch (str "files-" (:id manifest))
               :file-output retained}]})))

(defn- announce! [value]
  (when (= :proposed (:kind value))
    ((requiring-resolve 'is.simm.ops.proposal-publication/publish!) (:proposal value)))
  value)

(defn file-completed-request!
  "File one canonical request's certified output, or retry its existing filing.
   Host worker only; accepts no caller-supplied output, Attempt, plan or target.
   Body references and Proposal/disposition commit under the same GC guards.
   A verified empty/unchanged capture records :no-op with no Proposal.

   Existing dispositions are historical attestations, not a pin on the original
   Attempt. After authorization, retry can republish the existing reserved chat
   card without recertifying an expired Attempt or rerunning the Job."
  [conn principal room-id room request-id]
  (when ec/*spin-id* (refuse! :requires-host-worker))
  (when-not (uuid? request-id) (refuse! :invalid-request))
  (canonical-connection! conn)
  (let [db @conn row (request-row db request-id)]
    (authorize-request! db principal room-id row)
    (if-let [existing (complete-disposition! db row)]
      (announce! existing)
      (let [{:keys [plan] :as prepared} (filing/prepare conn principal room-id room request-id)
            value (if plan
                    (storage/publish!
                     conn plan
                     (fn [retained]
                       (record! conn principal room-id room prepared
                                (proposal-entity prepared retained))))
                    (record! conn principal room-id room prepared nil))]
        ;; Only after the joint transaction ACK (or its exact durable recovery).
        (announce! value)))))
