(ns is.simm.runtimes.memory-changes
  "Owned, post-durable workspace invalidations. Only routing/listener resources
   live here; files and application history remain in the canonical room world."
  (:require [datahike.api :as d]
            [is.simm.documents.files :as files]
            [kabel.pubsub :as pubsub]
            [kabel.pubsub.protocol :as proto]
            [taoensso.telemere :as log])
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

;; A process I/O owner, deliberately not a room signal or forkable registry.
;; No document data or user draft lives in this handle.
(defonce ^:private service (atom nil))

(defn start! [options]
  (locking service
    (if-let [owner @service]
      (when-not ((:running? owner))
        (throw (ex-info "Memory notifier must be stopped before replacement" {:type ::held})))
      (reset! service (open! options)))))

(defn watch! [id room party]
  (if-let [owner @service]
    ((:watch! owner) id room party)
    (throw (ex-info "Memory change service is unavailable" {:type ::unavailable}))))

(defn unwatch-room! [room]
  (when-let [owner @service] ((:unwatch-room! owner) room)))

(defn refresh-room! [room]
  (when-let [owner @service]
    (doseq [[id interest] ((:interests owner))
            :when (and (= (:id room) (:native-id interest))
                       ((:canonical? owner) id room))
            party (:readers interest)]
      (try ((:watch! owner) id room party)
           (catch Exception e
             (log/log! {:level :warn :id ::watch-rebind-failed
                        :msg "Memory subscription could not follow room replacement"
                        :data {:room-id id :exception-class (.getName (class e))}}))))))

(defn stop! []
  (locking service
    (when-let [owner @service]
      ((:close! owner))
      (reset! service nil))))
