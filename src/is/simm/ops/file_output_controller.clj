(ns is.simm.ops.file-output-controller
  "Fixed HOST orchestration; public actions use the strict reviewed entries.
   Original ACK values are invocation-local witnesses, never readback tokens or
   client capabilities. Only original sealed no-entry cleanup is supported;
   positive recovery remains a separate currently authorized operation."
  (:require [dvergr.rooms.files :as files]
            [dvergr.runtime.preparation :as preparation]
            [hasch.core :as hasch]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-publication :as publication]
            [is.simm.ops.file-output-validation :as validation]
            [is.simm.ops.owned-workspace :as owned]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- refuse! [reason]
  (throw (ex-info "Retained-file adoption controller is unavailable" {:type ::unavailable :reason reason})))

(defn- native-writer []
  ;; Older public dependencies remain loadable; no fallback to ambient write!.
  ;; Resolve a fixed native function, not a caller-provided hook or execution lane.
  (when (every? files/write-capabilities
                #{:explicit-workspace :durable-publication :expected-worktree :file-modes :applied-receipt
                  :validated-publication-basis})
    (when-let [writer (ns-resolve 'dvergr.rooms.files 'write-to-completion!)]
      (when (fn? @writer) @writer))))

(defn- pending [operation]
  {:status :pending :operation (:id operation) :recovery :unavailable})

(defn- validation-binding! [binding plan witness]
  ;; Only an invocation-local HOST witness is accepted. Its immutable manifest
  ;; and FULL target, including registry/runtime markers, must describe exactly
  ;; this original plan and owned binding. Nothing comes from transport input.
  (when-not (and (= (:manifest witness) (get-in plan [:manifest :id]))
                 (= (:target witness) (:target binding) (get-in plan [:manifest :target])))
    (refuse! :validation-binding-mismatch))
  witness)

(defn- authorize-original! [conn party room-id room original owned-binding]
  (let [db @conn proof (:proof original)]
    (when-not (= proof (adoption/snapshot db party room-id (get-in proof [:binding :proposal])))
      (refuse! :preflight-changed))
    (adoptions/linked-operation! db (get-in proof [:fork :db/id]) (:operation original))
    (owned/validate! db party :merge room-id room owned-binding)
    true))

(defn- review-revision [prepared witness]
  ;; Unordered maps/sets hash canonically. Scope ordering is not semantic;
  ;; retain every scope while normalizing its acquisition order. No clocks,
  ;; actor-specific authority, native handles or body bytes enter this digest.
  (hasch/uuid [:simmis/reviewed-file-adoption-v1
               (select-keys (get-in prepared [:proof :binding]) [:proposal :component])
               (update witness :typed-scopes
                       #(vec (sort-by (comp str :revision) %)))]))

(defn- preflight! [conn party room-id room proposal-id binding]
  (let [prepared (adoption/prepare conn party room-id room proposal-id)]
    (when-not (= (:target binding) (get-in prepared [:proof :binding :target]))
      (refuse! :target-changed))
    (let [witness (validation-binding! binding (:plan prepared)
                                       (validation/prepare-in (:db binding) (:plan prepared)))]
      {:prepared prepared :witness witness :revision (review-revision prepared witness)})))

(defn- host-input! [conn party room-id proposal-id]
  (when (simple/on-drain-thread?) (refuse! :requires-host-worker))
  (when-not (and conn (identical? conn (sdb/get-conn))
                 (every? uuid? [party room-id proposal-id]))
    (refuse! :invalid-host-input)))

(defn preview-reviewed!
  "HOST current canonical review, not an admission capability. Caller owns the
   outer application lifetime and authorizes metadata before Room hydration.
   No claim, writer or publication occurs. Each acceptance must recompute its
   OWN preflight and compare the returned revision before claiming."
  [conn party room-id room proposal-id]
  (host-input! conn party room-id proposal-id)
  (preparation/with-host-operation!
    room
    (fn [lease]
      (preparation/with-host-current! lease (constantly true))
      (let [db @conn binding (owned/resolve! db party :merge room-id room)
            proof (adoption/snapshot db party room-id proposal-id)
            existing (adoptions/current-operation db (get-in proof [:fork :db/id]))]
        (if (and existing (not= :aborted (:state existing)))
          (pending existing)
          (do
            (or (native-writer) (refuse! :explicit-writer-unavailable))
            (let [{:keys [revision]} (preflight! conn party room-id room proposal-id binding)]
              (preparation/with-host-current! lease (constantly true))
              {:status :reviewed :proposal proposal-id
               :component (get-in proof [:binding :component]) :revision revision})))))))

(defn- adopt-in!
  "INTERNAL ordinary single-component adoption. Caller supplies transport-
   authenticated party and the canonical live Room; hydrate only after authority
   checks, OUTSIDE this operation's writers. No force, bytes, target, claim token
   or arbitrary callback is accepted. Public adapters require reviewed adoption.

   Only a NORMAL native returned completion with applied? true, a winning receipt,
   and THIS invocation's applying ACK reaches original-publisher finalization.
   Throwable :publication data is NEVER receipt evidence. Every postclaim error
   stays pending UNLESS the direct fixed-native sealed not-started return permits
   a strictly authorized exact original claimed->aborted cleanup. Applying never
   aborts. No recovery read, native retry or generic acceptance.
   Pure/mock tests of this composition are NOT native ownership evidence."
  [conn party room-id room proposal-id lease expected-revision]
  (host-input! conn party room-id proposal-id)
  (preparation/with-host-current! lease (constantly true))
  (let [db @conn binding (owned/resolve! db party :merge room-id room)
        proof (adoption/snapshot db party room-id proposal-id)
        existing (adoptions/current-operation db (get-in proof [:fork :db/id]))]
    (if (and existing (not= :aborted (:state existing)))
      ;; A new invocation NEVER becomes the publisher by finding its token.
      (pending existing)
      (let [write! (or (native-writer) (refuse! :explicit-writer-unavailable))
            {:keys [prepared witness revision]} (preflight! conn party room-id room proposal-id binding)
            _ (when (and expected-revision (not= expected-revision revision))
                (refuse! :review-changed))
            _ (preparation/with-host-current! lease (constantly true))
            claimed (if expected-revision
                      (adoption/claim! conn party room-id room prepared revision)
                      (adoption/claim! conn party room-id room prepared))
            operation (:operation claimed)
            applying (volatile! nil)]
        (try
          (validation-binding! binding (:plan claimed) witness)
          (let [{:keys [identity files options]} (publication/prepare operation (:plan claimed))
                authorize! #(do (preparation/with-host-current! lease (constantly true))
                                (authorize-original! conn party room-id room claimed binding))
                before-dispatch!
                (fn [candidate]
                  ;; Only a short lifetime decision holds the Room monitor.
                  ;; Never hold it across control transactions or native IO.
                  (preparation/with-host-current! lease (constantly true))
                  (when @applying (refuse! :duplicate-dispatch-callback))
                  ;; This fixed gate validates ownership WITH the applying CAS
                  ;; against the SAME writer DB. No arbitrary tx callback.
                  (let [ack (adoption/bound-applying! conn party room-id room claimed candidate binding)]
                    (vreset! applying ack)
                    true))
                completion (write! room (:system binding) files
                                   (assoc options :publication {:identity identity :authorize! authorize!
                                                                :validate-basis! #(validation/assert-after! % witness)
                                                                :before-dispatch! before-dispatch!}))]
            (when-not (and (map? completion)
                           (= #{:completion :result :error} (set (keys completion)))
                           (or (nil? (:error completion)) (instance? Throwable (:error completion))))
              (refuse! :physical-result-unavailable))
            (case (:completion completion)
              :returned
              (let [result (:result completion)]
                (when-not (and (true? (:applied? result)) @applying)
                  (refuse! :physical-result-unavailable))
                (publication/validate-receipt! (:operation @applying) (get-in claimed [:plan :manifest])
                                               (:publication result))
                ;; Native private positive evidence wins even if a handler
                ;; subsequently threw. Error data itself is never a receipt.
                (adoption/finalize! conn @applying (:publication result)))
              :not-started
              (do
                (when (or @applying (.isInterrupted (Thread/currentThread)))
                  (refuse! :cleanup-unavailable))
                (adoption/bound-not-started! conn party room-id room claimed binding completion))
              ;; Includes pending and unknown categories. No negative inference
              ;; from result/error maps or absence of a local applying ACK.
              (refuse! :physical-result-unavailable)))
          (catch Throwable error
            (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
            (throw (ex-info "Retained-file adoption remains pending; do not replay publication"
                            (assoc (pending operation) :type ::pending) error))))))))

(defn adopt!
  "INTERNAL inactive adoption, physically owned by the exact Room through exit.
   Caller must already own the outer application/control-connection lifetime.
   No public adapter or automatic acceptance is enabled by this function."
  [conn party room-id room proposal-id]
  (when (simple/on-drain-thread?) (refuse! :requires-host-worker))
  (preparation/with-host-operation!
    room #(adopt-in! conn party room-id room proposal-id % nil)))

(defn adopt-reviewed!
  "HOST explicit reviewed acceptance. The mandatory semantic revision is a
   precondition, NOT authority or a substitute for current authorization.
   Recompute the fixed HOST witness and compare BEFORE claim. No client witness,
   target, bytes, callback or ownership token is accepted. Caller owns the outer
   application lifetime and authorizes metadata before Room hydration."
  [conn party room-id room proposal-id expected-revision]
  (host-input! conn party room-id proposal-id)
  (when-not (uuid? expected-revision) (refuse! :invalid-review-revision))
  (preparation/with-host-operation!
    room #(adopt-in! conn party room-id room proposal-id % expected-revision)))
