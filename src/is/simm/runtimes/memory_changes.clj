(ns is.simm.runtimes.memory-changes
  "Owned, post-durable workspace invalidations. Only routing/listener resources
   live here; files and application history remain in the canonical room world."
  (:require [datahike.api :as d]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as room-store]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as system-db]
            [is.simm.documents.files :as files]
            [is.simm.ops.owned-workspace :as owned]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as proto]
            [taoensso.telemere :as log]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple])
  (:import [java.util.concurrent LinkedBlockingQueue]))

(defn topic [party] (keyword "memory" (str party)))

(defn topic-party [value]
  (when (and (keyword? value) (= "memory" (namespace value)))
    (parse-uuid (name value))))

(defn register-topic! [peer party]
  (when-not (pubsub/topic-registered? peer (topic party))
    (pubsub/register-topic! peer (topic party) {:strategy (proto/pub-sub-only-strategy nil)})))

(defn open!
  "Create one host transport resource. resolve-room and allowed? must resolve
   current authority, not a snapshot captured at subscription. publish! receives
   a party and a room-only hint. Never runs on Datahike's commit loop."
  [{:keys [resolve-room allowed? publish!]}]
  (let [lock (Object.) closed? (atom false) watches (atom {}) interests (atom {}) pending (atom #{})
        queue (LinkedBlockingQueue. 1)
        invalidate! (fn [id token]
                      (when-not @closed?
                        (swap! pending conj [id token])
                        (.offer queue true)))
        worker
        (doto
         (Thread.
          (fn []
            (try
              (loop []
                (.take queue)
                (let [[batch _] (swap-vals! pending (constantly #{}))]
                  (doseq [[id token] batch
                          :let [watch (get @watches id)]
                          :when (and (not @closed?) (= token (:token watch)))]
                    (try
                      (when (identical? (:ctx (:room watch)) (:ctx (resolve-room id)))
                        (doseq [party (:readers watch) :when (allowed? party id)]
                          (publish! party {:kind :memory-changed :room-id id})))
                      (catch Exception e
                        (log/log! {:level :error :id ::invalidation-failed
                                   :msg "Memory invalidation could not be delivered"
                                   :data {:room-id id :exception-class (.getName (class e))}})))))
                (when-not @closed? (recur)))
              (catch InterruptedException _ (.interrupt (Thread/currentThread)))))
          "simmis-memory-changes")
          (.setDaemon true)
          (.start))
        remove-watch! (fn [id]
                        (when-let [{:keys [conn key]} (get @watches id)]
                          (d/unlisten-commits conn key)
                          (swap! watches dissoc id)))]
    {:watch!
     (fn [id room party]
       (locking lock
         (when (or @closed? (not (.isAlive worker)))
           (throw (ex-info "Memory change stream is stopped" {:type ::closed})))
         (when-not (and (identical? (:ctx room) (:ctx (resolve-room id)))
                        (allowed? party id))
           (throw (ex-info "Canonical room read permission required" {:type :not-authorized})))
         (files/with-workspace room
           (fn [conn]
             (let [old (get @watches id)
                   branch (get-in @conn [:config :branch])
                   same? (and (identical? conn (:conn old))
                              (identical? (:ctx room) (:ctx (:room old)))
                              (= branch (:branch old)))]
               (when-not same?
                 (remove-watch! id)
                 (let [token (random-uuid) key [::change id token]]
                   (swap! watches assoc id
                          {:room room :conn conn :key key :token token :branch branch
                           :readers (get-in @interests [id :readers] #{})})
                   (d/listen-commits conn key
                                     (fn [event]
                       ;; A concrete connection's callbacks are not world-local.
                       ;; Capture this canonical branch; never infer it from the
                       ;; writer thread's ambient execution context.
                                       (when (= branch (:branch event))
                                         (invalidate! id token))))))
               (swap! interests update id #(-> (or % {}) (assoc :native-id (:id room))
                                               (update :readers (fnil conj #{}) party)))
               (swap! watches update-in [id :readers] conj party)
               (when-not same? (invalidate! id (get-in @watches [id :token]))))))
         {:watching? true}))
     :unwatch-room!
     (fn [room]
       (locking lock
         (doseq [[id watch] @watches
                 :when (identical? (:ctx room) (get-in watch [:room :ctx]))]
           (remove-watch! id))))
     :close!
     (fn []
       (locking lock
         (reset! closed? true)
         (doseq [id (keys @watches)] (remove-watch! id))
         (reset! interests {})
         (reset! pending #{}))
       (.interrupt worker)
       (.join worker 5000)
       (when (.isAlive worker)
         (throw (ex-info "Memory notifier has not stopped" {:type ::held}))))
     :watch-count #(count @watches)
     :running? #(and (not @closed?) (.isAlive worker))
     :interests #(deref interests)
     :canonical? (fn [id room] (identical? (:ctx room) (:ctx (resolve-room id))))}))

(defn- canonical-control! [conn]
  (when-not (and conn (identical? conn (system-db/get-conn)))
    (throw (ex-info "Memory notifier requires the canonical control connection" {:type ::unavailable}))))

(defn- registered-room [db id]
  (when-let [slug (:room/slug (d/entity db [:room/id id]))]
    (registry/lookup (room-store/slug->room-id slug))))

(defn- same-owned-source? [a b]
  (and a b
       (identical? (:room a) (:room b))
       (identical? (:system a) (:system b))
       (identical? (:conn a) (:conn b))
       (= (:workspace-config a) (:workspace-config b))
       (= (:target a) (:target b))
       (= (:target-stamp a) (:target-stamp b))))

(defn- same-snapshot? [a b]
  ;; A shared Datahike writer may materialize a fresh DB wrapper on every
  ;; dereference. Commit/lineage identify the persisted immutable state; the
  ;; complete runtime config additionally fences logical checkout/config drift.
  ;; Missing/legacy metadata is NOT proof that two snapshots share a basis.
  (let [lineage (get-in a [:meta :datahike/id])
        commit (get-in a [:meta :datahike/commit-id])
        tx (:max-tx a)]
    (and (uuid? lineage) (uuid? commit) (integer? tx) (map? (:config a))
         (= lineage (get-in b [:meta :datahike/id]))
         (= commit (get-in b [:meta :datahike/commit-id]))
         (= tx (:max-tx b))
         (= (:config a) (:config b)))))

(defn open-owned!
  "INACTIVE alternative notifier, not a startup switch or second active service.
   The host supplies its canonical control connection/context and transport
   publisher only. Canonical Room, EACL and owned default resolution are FIXED;
   no caller resolver, selected workspace or retained read authority is used.

   ONE worker owns reconciliation and publication. Commit callbacks only enqueue.
   Default/control/EACL changes reconcile every current interest, including a
   missing target: current room readers receive a ROOM-ONLY hint, not old data.
   Native handles are lifecycle resources, not document/application state."
  [control-conn host-ctx publish!]
  (when (simple/on-drain-thread?)
    (throw (ex-info "Memory notifier needs a host worker" {:type ::unavailable})))
  (canonical-control! control-conn)
  (when-not (and (:fork-id host-ctx) (fn? publish!))
    (throw (ex-info "Memory notifier requires host context and publisher" {:type ::unavailable})))
  (let [lock (Object.) queue (LinkedBlockingQueue. 1)
        state (atom {:closed? false :control-generation 0 :interests {} :watches {}
                     :pending #{} :control-dirty? false})
        control-branch (get-in @control-conn [:config :branch])
        control-key [::owned-control (random-uuid)]
        worker (atom nil)]
    (letfn [(enqueue-room! [id]
              (let [[before _] (swap-vals! state #(if (:closed? %) % (update % :pending conj id)))]
                (when-not (:closed? before) (.offer queue true))))
            (source-event! [id token branch event]
              (let [[before _]
                    (swap-vals! state
                                (fn [s]
                                  (if (and (not (:closed? s)) (= branch (:branch event))
                                           (= token (get-in s [:watches id :token])))
                                    (update s :pending conj id) s)))
                    accepted? (and (not (:closed? before)) (= branch (:branch event))
                                   (= token (get-in before [:watches id :token])))]
                (when accepted? (.offer queue true))
                (boolean accepted?)))
            (control-event! [event]
              (let [[before _]
                    (swap-vals! state
                                (fn [s]
                                  (if (and (not (:closed? s)) (= control-branch (:branch event)))
                                    (-> s (update :control-generation inc) (assoc :control-dirty? true)) s)))
                    accepted? (and (not (:closed? before)) (= control-branch (:branch event)))]
                (when accepted? (.offer queue true))
                (boolean accepted?)))
            (remove-watch! [id]
              ;; Caller holds only this short lifecycle lock. No resolver,
              ;; provider, artifact access or publication runs under it.
              (when-let [{:keys [conn key]} (get-in @state [:watches id])]
                (d/unlisten-commits conn key)
                (swap! state update :watches dissoc id)))
            (observe [id]
              (canonical-control! control-conn)
              (let [input @state interest (get-in input [:interests id]) db @control-conn]
                (when (and interest (not (:closed? input)))
                  (binding [ec/*execution-context* host-ctx]
                    (let [canonical (registered-room db id)
                          readers (set (filter #(access/can? db % :read {:room id}) (:readers interest)))
                          available-room (when (not (identical? canonical (:retired-room interest)))
                                           canonical)
                          source (when (and available-room (seq readers))
                                   (try
                                     (owned/resolve! db (first (sort-by str readers)) :read id available-room)
                                     (catch InterruptedException error (throw error))
                                     (catch Exception _ nil)))]
                      {:id id :db db :control-generation (:control-generation input)
                       :revision (:revision interest) :canonical canonical
                       :readers readers :source-db (:db source) :binding (dissoc source :db)})))))
            (install-observation! [{:keys [id db source-db control-generation revision canonical readers] source :binding}]
              (locking lock
                (let [current @state interest (get-in current [:interests id])]
                  (if-not (and (not (:closed? current)) interest
                               (= revision (:revision interest))
                               (= control-generation (:control-generation current))
                               (same-snapshot? db @control-conn)
                               (or (nil? source) (same-snapshot? source-db @(:conn source)))
                               (identical? canonical (binding [ec/*execution-context* host-ctx]
                                                       (registered-room db id))))
                    (do (enqueue-room! id) nil)
                    (do
                      (when-not (same-owned-source? source (get-in current [:watches id :binding]))
                        (remove-watch! id)
                        (when source
                          (let [conn (:conn source) branch (get-in @conn [:config :branch])
                                token (random-uuid) key [::owned-source id token]]
                            (swap! state assoc-in [:watches id]
                                   {:binding source :conn conn :key key :token token :branch branch})
                            (try (d/listen-commits conn key #(source-event! id token branch %))
                                 (catch Throwable error
                                   ;; Retain no apparently installed row after a
                                   ;; failed listener ACK. Exact removal is safe.
                                   (remove-watch! id)
                                   (throw error))))))
                      ;; Interest is transport intent, not retained authority.
                      ;; Keep reader IDs across revocation so a later control
                      ;; regrant can reconcile automatically. Only this fresh
                      ;; observation's authorized readers may receive a hint.
                      (when-not (seq readers) (remove-watch! id))
                      {:readers readers :db db :canonical canonical
                       :control-generation control-generation
                       :revision revision :target-stamp (:target-stamp source)
                       :watch-token (get-in @state [:watches id :token])
                       :available? (boolean source)})))))
            (validate-installed! [id installed]
              ;; A config/System replacement need not transact the control DB.
              ;; Re-resolve AFTER listener installation, outside the monitor,
              ;; then fence its exact token and snapshot before announcing it.
              (when-let [fresh (observe id)]
                (locking lock
                  (let [s @state live (get-in s [:watches id])]
                    (if (and (not (:closed? s))
                             (= (:revision installed) (:revision fresh) (get-in s [:interests id :revision]))
                             (= (:control-generation installed) (:control-generation fresh) (:control-generation s))
                             (= (:readers installed) (:readers fresh))
                             (identical? (:canonical installed) (:canonical fresh))
                             (same-snapshot? (:db fresh) @control-conn)
                             (= (:watch-token installed) (:token live))
                             (if-let [source (:binding fresh)]
                               (and (same-snapshot? (:source-db fresh) @(:conn source))
                                    (same-owned-source? source (:binding live)))
                               (nil? live)))
                      (do
                        ;; Keep the exact retiring incarnation fenced until a
                        ;; replacement binding has passed EVERY post-install
                        ;; check. Then release its obsolete native context
                        ;; handle; this is not a new transport revision.
                        (when-let [retired (get-in s [:interests id :retired-room])]
                          (when (and (:binding fresh)
                                     (not (identical? retired (:canonical fresh))))
                            (swap! state update-in [:interests id] dissoc :retired-room)))
                        (assoc installed :db (:db fresh)))
                      (do
                        (when (= (:watch-token installed) (:token live)) (remove-watch! id))
                        (enqueue-room! id)
                        nil))))))
            (reconcile! [id]
              ;; Intentionally OUTSIDE the monitor: default resolution may open
              ;; a store. Control generation + immutable DB identity fence the
              ;; short installation phase against intervening commits.
              (when-let [observation (observe id)]
                (when-let [installed (install-observation! observation)]
                  (validate-installed! id installed))))
            (publish-current! [id observation]
              (doseq [party (:readers observation)]
                (canonical-control! control-conn)
                (let [db @control-conn s @state]
                  (when (and (not (:closed? s))
                             (= (:revision observation) (get-in s [:interests id :revision]))
                             (= (:control-generation observation) (:control-generation s))
                             (= (:watch-token observation) (get-in s [:watches id :token]))
                             (same-snapshot? db (:db observation))
                             (binding [ec/*execution-context* host-ctx]
                               (and (identical? (:canonical observation) (registered-room db id))
                                    (access/can? db party :read {:room id}))))
                    ;; A missing target still invalidates independently allowed
                    ;; readers. There is no target/content/capability in a hint.
                    (publish! party {:kind :memory-changed :room-id id})))))
            (run-worker! []
              (try
                (loop []
                  (.take queue)
                  (let [[before _] (swap-vals! state #(assoc % :pending #{} :control-dirty? false))
                        ids (if (:control-dirty? before)
                              (into (:pending before) (keys (:interests before))) (:pending before))]
                    (doseq [id ids :when (not (:closed? @state))]
                      (try (when-let [observation (reconcile! id)] (publish-current! id observation))
                           (catch InterruptedException error (throw error))
                           (catch Exception error
                             ;; Never leave a stale source attached after an
                             ;; unresolvable control owner or listener failure.
                             (locking lock (remove-watch! id))
                             (log/log! {:level :warn :id ::owned-reconciliation-failed
                                        :data {:room-id id :exception-class (.getName (class error))}}))))
                    (when-not (:closed? @state) (recur))))
                (catch InterruptedException _ (.interrupt (Thread/currentThread)))))]
      ;; Install control observation BEFORE interests/worker publication. No
      ;; commit-loop callback ever resolves a Room or queries authorization.
      (d/listen-commits control-conn control-key control-event!)
      (let [thread (Thread. ^Runnable run-worker! "simmis-owned-memory-changes")]
        (.setDaemon thread true)
        (reset! worker thread)
        (try (.start thread)
             (catch Throwable error
               (d/unlisten-commits control-conn control-key)
               (throw error))))
      {:mode :owned
       :same-owner? #(and (identical? control-conn %1) (identical? host-ctx %2) (identical? publish! %3))
       :watch!
       (fn [id room party]
         (when (simple/on-drain-thread?)
           (throw (ex-info "Memory watch requires a host worker" {:type ::unavailable})))
         (canonical-control! control-conn)
         (when-not (and (uuid? id) (uuid? party))
           (throw (ex-info "Exact Memory watch identity required" {:type ::unavailable})))
         (let [db @control-conn]
           (when-not (binding [ec/*execution-context* host-ctx]
                       (and room (:ctx room) (identical? room (registered-room db id))
                            (access/can? db party :read {:room id})))
             (throw (ex-info "Canonical room read permission required" {:type :not-authorized}))))
         (locking lock
           (let [s @state retired (get-in s [:interests id :retired-room])]
             (when (or (:closed? s) (not (.isAlive ^Thread @worker)))
               (throw (ex-info "Memory notifier is stopped" {:type ::closed})))
             (when (identical? room retired)
               (throw (ex-info "Memory Room incarnation is retiring" {:type ::unavailable})))
             (swap! state update-in [:interests id]
                    #(-> (or % {:revision 0 :readers #{}})
                         (update :revision inc) (update :readers conj party)
                         (assoc :native-id (:id room)) (dissoc :retired-room)))))
         (let [observation (reconcile! id)]
           (enqueue-room! id)
           (when-not (and observation (identical? room (:canonical observation))
                          (contains? (:readers observation) party))
             (throw (ex-info "Memory binding changed during watch registration; retry discovery"
                             {:type ::unavailable})))
           (cond-> {:watching? true :available? (:available? observation)}
             (:target-stamp observation) (assoc :target-stamp (:target-stamp observation)))))
       :unwatch-room!
       (fn [room]
         (locking lock
           (doseq [[id interest] (:interests @state)
                   :when (or (identical? room (get-in @state [:watches id :binding :room]))
                             (identical? room (binding [ec/*execution-context* host-ctx]
                                                (registered-room @control-conn id))))]
             (remove-watch! id)
             ;; Before-unregister runs while the old Room can still be found.
             ;; Do not let the worker reattach to that retiring incarnation.
             (swap! state update-in [:interests id]
                    #(-> % (update :revision inc) (assoc :retired-room room)))
             (enqueue-room! id))))
       :refresh-room!
       (fn [room]
         ;; Register hooks enqueue only. They do not resolve an owned store or
         ;; restore a retired incarnation; the worker proves the replacement.
         (locking lock
           (doseq [[id interest] (:interests @state)
                   :when (and (= (:id room) (:native-id interest))
                              (not (identical? room (:retired-room interest))))]
             ;; A hook is only an invalidation hint, never a target selector.
             ;; A stale/unregistered same-id hook cannot replace the canonical
             ;; Room, nor can it erase the exact retiring incarnation marker.
             (swap! state update-in [:interests id :revision] inc)
             (enqueue-room! id))))
       :close!
       (fn []
         (locking lock
           (swap! state assoc :closed? true :pending #{} :control-dirty? false)
           (d/unlisten-commits control-conn control-key)
           (doseq [id (keys (:watches @state))] (remove-watch! id))
           (swap! state assoc :interests {}))
         (.interrupt ^Thread @worker)
         (try (.join ^Thread @worker 5000)
              (catch InterruptedException error
                (.interrupt (Thread/currentThread))
                (throw error)))
         (when (.isAlive ^Thread @worker)
           (throw (ex-info "Owned Memory notifier has not physically stopped" {:type ::held}))))
       :watch-count #(count (:watches @state))
       :running? #(and (not (:closed? @state)) (.isAlive ^Thread @worker))
       ;; :readers here are private transport interests, not allowed principals.
       :interests #(get @state :interests)
       :canonical? (fn [id room]
                     (canonical-control! control-conn)
                     (binding [ec/*execution-context* host-ctx]
                       (identical? room (registered-room @control-conn id))))})))

;; A process I/O owner, deliberately not a room signal or forkable registry.
;; No document data or user draft lives in this handle.
(defonce ^:private service (atom nil))

(defn start! [options]
  (locking service
    (if-let [owner @service]
      (when (or (= :owned (:mode owner)) (not ((:running? owner))))
        (throw (ex-info "Memory notifier must be stopped before replacement" {:type ::held})))
      (reset! service (open! options)))))

(defn start-owned!
  "INACTIVE activation seam: ONE existing service slot, never a parallel or
   silently reused legacy notifier. The runtime switches only after v2 rollout."
  [control-conn host-ctx publish!]
  (locking service
    (if-let [owner @service]
      (when-not (and (= :owned (:mode owner)) ((:running? owner))
                     ((:same-owner? owner) control-conn host-ctx publish!))
        (throw (ex-info "Memory notifier must be stopped before changing mode or owner" {:type ::held})))
      (reset! service (open-owned! control-conn host-ctx publish!)))))

(defn watch! [id room party]
  (if-let [owner @service]
    ((:watch! owner) id room party)
    (throw (ex-info "Memory change service is unavailable" {:type ::unavailable}))))

(defn watch-owned!
  "Fixed v2 adapter seam. Never silently attach an owned client to the legacy
   selected-workspace notifier during an incomplete runtime rollout."
  [id room party]
  (let [owner @service]
    (when-not (= :owned (:mode owner))
      (throw (ex-info "Owned Memory notifier is unavailable" {:type ::unavailable})))
    ((:watch! owner) id room party)))

(defn unwatch-room! [room]
  (when-let [owner @service] ((:unwatch-room! owner) room)))

(defn refresh-room! [room]
  (when-let [owner @service]
    (if-let [refresh (:refresh-room! owner)]
      (refresh room)
      (doseq [[id interest] ((:interests owner))
              :when (and (= (:id room) (:native-id interest))
                         ((:canonical? owner) id room))
              party (:readers interest)]
        (try ((:watch! owner) id room party)
             (catch Exception e
               (log/log! {:level :warn :id ::watch-rebind-failed
                          :msg "Memory subscription could not follow room replacement"
                          :data {:room-id id :exception-class (.getName (class e))}})))))))

(defn stop! []
  (locking service
    (when-let [owner @service]
      ((:close! owner))
      (reset! service nil))))
