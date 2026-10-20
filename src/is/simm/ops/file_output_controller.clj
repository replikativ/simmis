(ns is.simm.ops.file-output-controller
  "INTERNAL inactive fixed host orchestration. No route/schema/default change.
   Original ACK values are invocation-local witnesses, never readback tokens or
   client capabilities. Recovery/abort are deliberately not implemented here."
  (:require [dvergr.rooms.files :as files]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-publication :as publication]
            [is.simm.ops.owned-workspace :as owned]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- refuse! [reason]
  (throw (ex-info "Retained-file adoption controller is unavailable" {:type ::unavailable :reason reason})))

(defn- native-writer []
  ;; Older public dependencies remain loadable; no fallback to ambient write!.
  ;; Resolve a fixed native function, not a caller-provided hook or execution lane.
  (when (every? files/write-capabilities
                #{:explicit-workspace :durable-publication :expected-worktree :file-modes :applied-receipt})
    (when-let [writer (ns-resolve 'dvergr.rooms.files 'write-to!)]
      (when (fn? @writer) @writer))))

(defn- pending [operation]
  {:status :pending :operation (:id operation) :recovery :unavailable})

(defn- authorize-original! [conn party room-id room original owned-binding]
  (let [db @conn proof (:proof original)]
    (when-not (= proof (adoption/snapshot db party room-id (get-in proof [:binding :proposal])))
      (refuse! :preflight-changed))
    (adoptions/linked-operation! db (get-in proof [:fork :db/id]) (:operation original))
    (owned/validate! db party :merge room-id room owned-binding)
    true))

(defn adopt!
  "INTERNAL ordinary single-component adoption. Caller supplies transport-
   authenticated party and the canonical live Room; hydrate only after authority
   checks, OUTSIDE this operation's writers. No force, bytes, target, claim token
   or arbitrary callback is accepted. Public adapters remain disabled.

   Only a NORMAL native physical result with applied? true, a winning receipt,
   and THIS invocation's applying ACK reaches original-publisher finalization.
   Throwable :publication data is NEVER receipt evidence. Every postclaim error
   stays pending; no abort, recovery read, native retry or generic acceptance.
   Pure/mock tests of this composition are NOT native ownership evidence."
  [conn party room-id room proposal-id]
  (when (simple/on-drain-thread?) (refuse! :requires-host-worker))
  (when-not (and conn (identical? conn (sdb/get-conn))
                 (every? uuid? [party room-id proposal-id]))
    (refuse! :invalid-host-input))
  (let [db @conn binding (owned/resolve! db party :merge room-id room)
        proof (adoption/snapshot db party room-id proposal-id)
        existing (adoptions/current-operation db (get-in proof [:fork :db/id]))]
    (if (and existing (not= :aborted (:state existing)))
      ;; A new invocation NEVER becomes the publisher by finding its token.
      (pending existing)
      (let [write! (or (native-writer) (refuse! :explicit-writer-unavailable))
            prepared (adoption/prepare conn party room-id room proposal-id)
            _ (when-not (= (:target binding) (get-in prepared [:proof :binding :target]))
                (refuse! :target-changed))
            claimed (adoption/claim! conn party room-id room prepared)
            operation (:operation claimed)
            applying (volatile! nil)]
        (try
          (let [{:keys [identity files options]} (publication/prepare operation (:plan claimed))
                authorize! #(authorize-original! conn party room-id room claimed binding)
                before-dispatch!
                (fn [candidate]
                  (when @applying (refuse! :duplicate-dispatch-callback))
                  ;; This fixed gate validates ownership WITH the applying CAS
                  ;; against the SAME writer DB. No arbitrary tx callback.
                  (let [ack (adoption/bound-applying! conn party room-id room claimed candidate binding)]
                    (vreset! applying ack)
                    true))
                result (write! room (:system binding) files
                               (assoc options :publication {:identity identity :authorize! authorize!
                                                            :before-dispatch! before-dispatch!}))]
            (when-not (and (true? (:applied? result)) @applying)
              (refuse! :physical-result-unavailable))
            (publication/validate-receipt! (:operation @applying) (get-in claimed [:plan :manifest])
                                           (:publication result))
            ;; Only the fixed native function's normal winning result is trusted
            ;; here. Error recovery must later authorize and query scoped DB truth.
            (adoption/finalize! conn @applying (:publication result)))
          (catch Throwable error
            (when (instance? InterruptedException error) (.interrupt (Thread/currentThread)))
            (throw (ex-info "Retained-file adoption remains pending; do not replay publication"
                            (assoc (pending operation) :type ::pending) error))))))))
