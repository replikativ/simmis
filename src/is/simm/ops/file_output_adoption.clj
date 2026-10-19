(ns is.simm.ops.file-output-adoption
  "INTERNAL preparation and serialized ownership for forthcoming guarded
   adoption. No route, schema installation, target publication, proposal status
   mutation or execution. The
   returned data is NOT a capability: an eventual serialized host writer must
   repeat authority/target checks and compare the exact frozen storage proof."
  (:require [datahike.api :as d]
            [is.simm.model.access :as access]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [malli.core :as m]))

(defn- refuse! [reason]
  (throw (ex-info "Retained file adoption is unavailable"
                  {:type ::unavailable :reason reason})))

(defn- host-connection! [conn]
  (when (simple/on-drain-thread?) (refuse! :requires-host-worker))
  (when-not (and conn (identical? conn (sdb/get-conn))) (refuse! :noncanonical-connection)))

(defn- filing-snapshot
  "Canonical filing integrity only. Callers must authorize their exact verb
   BEFORE invoking this private reader. Never use it as an authority bypass."
  [db party room-id proposal-id]
  (let [p (d/pull db [:db/id :proposal/id :proposal/room :proposal/status
                      :proposal/author :proposal/run :proposal/filing-hash :proposal/review]
                  [:proposal/id proposal-id])]
    (when-not (and (= proposal-id (:proposal/id p)) (= room-id (:proposal/room p))
                   (= :open (:proposal/status p)) (nil? (:proposal/review p)))
      (refuse! :unsupported-proposal))
    ;; Count before component pull: plain/wildcard component pull can allocate
    ;; every child before a caller discovers that the Proposal was oversized.
    (let [links (vec (take 2 (d/datoms db :eavt (:db/id p) :proposal/forks)))]
      (when-not (= 1 (count links)) (refuse! :unsupported-component-set))
      (let [fork (d/pull db [:db/id :proposal.fork/scope :proposal.fork/authority-scope
                             :proposal.fork/branch :proposal.fork/status :proposal.fork/system-type
                             {:proposal.fork/file-output [:file-output/id :file-output/room]}]
                         (:v (first links)))
            scope (:proposal.fork/scope fork)]
        (when-not (and (= :file-output (:proposal.fork/system-type fork))
                       (nil? (:proposal.fork/status fork))
                       (= room-id (:proposal.fork/authority-scope fork))
                       (string? (:proposal.fork/branch fork))
                       (re-matches #"[A-Za-z0-9._-]{1,128}" (:proposal.fork/branch fork)))
          (refuse! :unsupported-component))
        (let [owner (manifests/owner db scope)]
          (when-not (= (select-keys owner [:proposal :room :fork-eid :proposal-eid])
                       {:proposal proposal-id :room room-id :fork-eid (:db/id fork) :proposal-eid (:db/id p)})
            (refuse! :invalid-owner))
          (let [{:keys [manifest] :as retained} (storage/retained-in db scope)
                request (d/pull db '[*] [:work.request/id (:request manifest)])
                disposition (dispositions/disposition request)
                binding {:proposal proposal-id :component scope :manifest (:id manifest)
                         :room room-id :actor party :filing (:proposal/filing-hash p)
                         :disposition (:id disposition) :target (:target manifest)}]
            (when-not (and (= :proposed (:kind disposition))
                           (= proposal-id (:proposal disposition))
                           (= scope (:manifest disposition))
                           (= room-id (:room manifest) (:work.request/room request))
                           (= (:proposal/author p) (:work.request/by request))
                           (= (:proposal/run p) (:candidate disposition) (:attempt manifest))
                           (= (:request manifest) (:request disposition))
                           (= (:basis manifest) (:basis disposition))
                           (= (:output manifest) (:output disposition))
                           (seq (:entries manifest))
                           (publication/host-target? (:target manifest))
                           (m/validate adoptions/binding-schema binding))
              (refuse! :invalid-filing))
            {:binding binding :proposal p :fork fork :retained retained
             :disposition disposition}))))))

(defn snapshot
  "INTERNAL merge-authorized control-DB read. Only one ordinary open file
   component is supported. No bodies are loaded and no ownership is granted.
   Repeat against the serialized writer DB before any journal transition."
  [db party room-id proposal-id]
  (when-not (and (every? uuid? [party room-id proposal-id])
                 (access/can? db party :read {:room room-id})
                 (access/can? db party :write {:proposal proposal-id})
                 (access/can? db party :merge {:room room-id}))
    (refuse! :denied))
  (let [proof (filing-snapshot db party room-id proposal-id)]
    (when-not (access/may-merge-proposal-fork? db party (:fork proof))
      (refuse! :denied))
    proof))

(defn decision-snapshot
  "INTERNAL pure writer guard for retained-file withdrawal/revision requests.
   Preserve each existing public permission resource, including author
   withdrawal without merge authority. No live target lookup, body IO, branch
   mutation or journal ownership recovery. Compose the decision datoms in the
   SAME serialized transaction that calls this function."
  [db party room-id proposal-id {:keys [verb scope branch] :as action}]
  (let [resource (case verb
                   :dismiss-proposal (when (= #{:verb} (set (keys action)))
                                       {:proposal-forks proposal-id})
                   :dismiss-fork (when (and (= #{:verb :scope :branch} (set (keys action)))
                                            (uuid? scope) (or (string? branch) (keyword? branch)))
                                   {:proposal-fork {:proposal proposal-id :scope scope :branch branch}})
                   :request-changes (when (and (every? #{:verb :branch} (keys action))
                                               (or (nil? branch) (string? branch) (keyword? branch)))
                                      {:proposal proposal-id})
                   nil)]
    (when-not (and resource (every? uuid? [party room-id proposal-id])
                   (access/can? db party :write resource))
      (refuse! :denied))
    (let [{:keys [binding fork] :as proof} (filing-snapshot db party room-id proposal-id)
          operation (adoptions/current-operation db (:db/id fork))]
      (when (or (and (= :dismiss-fork verb) (not= scope (:proposal.fork/scope fork)))
                (and branch (not= (name branch) (:proposal.fork/branch fork))))
        (refuse! :unsupported-component))
      (when (and operation
                 (or (not= :aborted (:state operation))
                     (not= (dissoc binding :actor)
                           (select-keys operation (remove #{:actor} (keys binding))))))
        (refuse! :publication-pending))
      proof)))

(defn prepare
  "Host-only read preflight for a NEW adoption. Canonical Room/default target
   verification precedes the first body fetch. Freeze all exact body refs from
   one DB; validate every bounded pair before returning. Concurrent deletion/GC
   refuses, never repairs/reexecutes. Retained evidence outlives the Attempt.
   Party is captured by the trusted authenticated host, never a client field.
   No caller-supplied bytes, manifest, descriptor or claim token is accepted."
  [conn party room-id room proposal-id]
  (host-connection! conn)
  (let [db @conn
        {:keys [binding retained] :as proof} (snapshot db party room-id proposal-id)
        manifest (:manifest retained)]
    (when-not (= (:target binding) (filing/current-target db room-id room))
      (refuse! :target-changed))
    (let [bodies (into (sorted-map)
                       (for [path (keys (:entries manifest))]
                         [path (storage/read-file-in conn db (:id manifest) path)]))]
      (plans/validate-plan! {:manifest manifest :proposal proposal-id :bodies bodies})
      {:proof proof :plan {:manifest manifest :proposal proposal-id :bodies bodies}})))

(defn- prepared! [{:keys [proof plan] :as prepared} party room-id]
  (when-not (and (= #{:proof :plan} (set (keys prepared)))
                 (= party (get-in proof [:binding :actor]))
                 (= room-id (get-in proof [:binding :room]))
                 (= (:manifest plan) (get-in proof [:retained :manifest]))
                 (= (:proposal plan) (get-in proof [:binding :proposal])))
    (refuse! :invalid-preflight))
  (plans/validate-plan! plan)
  prepared)

(defn- claim-tx
  "Fixed writer composition: no artifact IO, hydration, Room lock, nested
   transaction or caller-supplied transaction callback. The preflight is data,
   not authority. Check its COMPLETE proof against this serialized DB value."
  [db party room-id room proof operation]
  (let [current (snapshot db party room-id (get-in proof [:binding :proposal]))]
    (when-not (= proof current) (refuse! :preflight-changed))
    (when-not (= (get-in proof [:binding :target]) (filing/current-target db room-id room))
      (refuse! :target-changed))
    (adoptions/claim-tx-data db (get-in proof [:fork :db/id]) operation)))

(defn claim!
  "INTERNAL trusted-host storage boundary, NOT an endpoint or publication.
   Takes only the host's prepared value; never accept this map from a client.
   Schema must already be installed by the owning host (tests only for now).

   Fresh invocation token, one claim transaction, NO retries. Only its original
   acknowledged report confers permission to continue towards dispatch. Any
   error/lost ACK leaves ownership unacknowledged: expose only operation ID,
   never infer ownership from a live readback. Applying and all competing
   decision paths must be wired before this is used by a public accept action."
  [conn party room-id room prepared]
  (host-connection! conn)
  (prepared! prepared party room-id)
  (let [proof (:proof prepared)
        operation (adoptions/make (:binding proof) (random-uuid) (java.util.Date.))]
    (try
      (let [report (d/transact conn [[:db.fn/call
                                      (fn [db] (claim-tx db party room-id room proof operation))]])]
        (when-not (:db-after report) (refuse! :claim-report-unavailable))
        ;; Only the immutable result of THIS acknowledged native invocation.
        ;; Do not replace it with @conn, even if the same token is readable.
        (adoptions/linked-operation! (:db-after report) (get-in proof [:fork :db/id]) operation)
        (assoc prepared :operation operation))
      (catch Throwable error
        (when (instance? InterruptedException error) (.interrupt (Thread/currentThread)))
        (throw (ex-info "File adoption claim was not acknowledged; do not publish"
                        {:type ::claim-unacknowledged :operation (:id operation)} error))))))
