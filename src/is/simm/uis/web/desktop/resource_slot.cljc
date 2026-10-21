(ns is.simm.uis.web.desktop.resource-slot
  "Serialized native construction/publication/retirement for ONE physical store.
   Logical cancellation never releases physical ownership. No writes or timers."
  (:require #?(:clj [clojure.core.async :refer [go <! promise-chan put!]]
               :cljs [clojure.core.async :refer [go <! promise-chan put!] :include-macros true])))

(defn slot []
  {:state (atom {:phase :idle :running? false :drains []})})

(defn- locked [slot f]
  #?(:clj (locking (:state slot) (f)) :cljs (f)))

(defn- failure? [value]
  (instance? #?(:clj Throwable :cljs js/Error) value))

(defn superseded? [value] (= ::superseded (:type (ex-data value))))
(defn- cancelled [] (ex-info "Native resource request superseded" {:type ::superseded}))

(defn- complete! [op value]
  (when (and op (compare-and-set! (:settled? op) false true))
    (put! (:result op) value)))

(defn- current? [slot op]
  (and (= (:id op) (get-in @(:state slot) [:desired :id])) ((:valid? op))))

(declare pump!)

(defn- hold! [slot error failed-open]
  (let [{:keys [desired drains]}
        (locked slot
                #(let [prior @(:state slot)]
                   (swap! (:state slot) (fn [s] (-> s
                                                    (assoc :phase :held :running? false :error error
                                                           :failed-open failed-open :desired nil :drains [])
                                                    (dissoc :last))))
                   prior))]
    (complete! desired error)
    (doseq [ch drains] (put! ch error))))

(defn- plan! [slot]
  (locked
   slot
   #(let [{:keys [desired resource phase drains failed-open]} @(:state slot)]
      (cond
        (= :held phase) (do (swap! (:state slot) assoc :running? false) {:kind :stop})
        (= :recovering phase) {:kind :recover :failed-open failed-open}
        resource
        (if (and desired (= (:id desired) (get-in resource [:owner :id])) (current? slot desired))
          (if (:published? resource)
            (do (swap! (:state slot) assoc :phase :ready :running? false)
                {:kind :ready :op desired :value (:value resource)})
            (do (swap! (:state slot) assoc-in [:resource :published?] true)
                {:kind :publish :op desired :value (:value resource)}))
          {:kind :dispose :resource resource})
        (seq drains)
        (do (swap! (:state slot) assoc :drains []) {:kind :drained :drains drains})
        desired
        (if (current? slot desired)
          (do (swap! (:state slot) assoc :phase :opening) {:kind :open :op desired})
          (do (swap! (:state slot) dissoc :desired)
              {:kind :cancel :op desired}))
        :else
        (do (swap! (:state slot) assoc :phase :idle :running? false :drains [])
            {:kind :idle :drains drains})))))

(defn- worker! [slot]
  (go
    (loop []
      (let [action (volatile! nil)
            continue?
            (try
              (let [{:keys [kind op value resource drains failed-open] :as step} (plan! slot)]
                (vreset! action step)
                (case kind
                  :stop false
                  :idle (do (doseq [ch drains] (put! ch true)) false)
                  :drained (do (doseq [ch drains] (put! ch true)) true)
                  :ready (do (complete! op {:status :ready :resource value :generation (:generation op)}) false)
                  :cancel (do (complete! op (cancelled)) true)
                  :publish (do ((:publish! op) value #(current? slot op)) true)
                  :dispose
                  (let [{:keys [owner value published?]} resource]
                    (when published? ((:unpublish! owner) value))
                    (swap! (:state slot) assoc :phase :retiring)
                    (let [result (<! ((:dispose! owner) value))]
                      (if (true? result)
                        (do (swap! (:state slot) dissoc :resource :error) true)
                        (do (hold! slot (if (failure? result) result
                                            (ex-info "Native cleanup did not confirm completion" {:result result})) nil)
                            false))))
                  :recover
                  (let [result (<! ((:recover! failed-open)))]
                    (if (true? result)
                      (do (swap! (:state slot) #(-> % (dissoc :failed-open :error) (assoc :phase :idle))) true)
                      (do (hold! slot (if (failure? result) result
                                          (ex-info "Constructor cleanup did not confirm completion" {:result result})) failed-open)
                          false)))
                  :open
                  (let [value (<! ((:open! op)))]
                    (if (failure? value)
                      (if (and (:clean-error? op) ((:clean-error? op) value))
                        (do
                          (locked slot #(when (= (:id op) (get-in @(:state slot) [:desired :id]))
                                          (swap! (:state slot) dissoc :desired)))
                          (complete! op value)
                          true)
                        (do (hold! slot value {:error value
                                               :recover! (when-let [recover! (:recover-open! op)] #(recover! value))}) false))
                      (if (nil? value)
                        (do (hold! slot (ex-info "Constructor closed without a resource or cleanup confirmation" {}) nil) false)
                        (do (swap! (:state slot) assoc :resource {:value value :owner op :published? false}) true))))))
              (catch #?(:clj Throwable :cljs :default) error
                (hold! slot error
                       (case (:kind @action)
                         :open {:error error
                                :recover! (when-let [recover! (:recover-open! (:op @action))] #(recover! error))}
                         :recover (:failed-open @action)
                         nil))
                false))]
        (when continue? (recur))))))

(defn- pump! [slot]
  (when (locked slot #(when-not (:running? @(:state slot))
                        (swap! (:state slot) assoc :running? true)
                        true))
    (worker! slot)))

(defn request!
  "Latest desired generation wins; duplicate generation shares one result.
   open!/dispose! return channels; publish!/unpublish! are synchronous identity-
   fenced host callbacks. Failed cleanup or uncertain construction holds only
   this store. clean-error? may prove a failed constructor owns no native work."
  [slot {:keys [generation] :as request}]
  (let [result
        (locked
         slot
         #(let [state @(:state slot) prior (:last state)]
            (cond
              (not (and (some? generation)
                        (every? fn? ((juxt :valid? :open! :dispose! :publish! :unpublish!) request))))
              (doto (promise-chan) (put! (ex-info "A native resource request requires a generation and host callbacks" {})))
              (= :held (:phase state)) (doto (promise-chan) (put! (:error state)))
              (= generation (:generation prior)) (:result prior)
              :else
              (let [op (assoc request :id (random-uuid) :result (promise-chan) :settled? (atom false))]
                (complete! (:desired state) (cancelled))
                (swap! (:state slot) assoc :desired op :last op)
                (:result op)))))]
    (pump! slot)
    result))

(defn retire!
  "Withdraw publication now; join eventual construction AND physical disposal.
   Never call and await this from inside this slot's own native operation."
  [slot]
  (let [result (promise-chan)]
    (locked slot #(let [state @(:state slot)]
                    (complete! (:desired state) (cancelled))
                    (swap! (:state slot) dissoc :desired :last)
                    (if (= :held (:phase state))
                      (put! result (:error state))
                      (swap! (:state slot) update :drains conj result))))
    (pump! slot)
    result))

(defn retry-cleanup!
  "Retry ONLY the retained failed cleanup, not construction, queries or writes."
  [slot]
  (let [result (promise-chan)]
    (locked
     slot
     #(let [{:keys [phase resource failed-open error]} @(:state slot)]
        (cond
          (not= :held phase) (put! result (ex-info "No failed cleanup is held" {}))
          (or resource (:recover! failed-open))
          (swap! (:state slot) (fn [s] (-> s (assoc :phase (if resource :retiring :recovering))
                                           (update :drains conj result))))
          :else (put! result (ex-info "Constructor cleanup is uncertain; reload this client before reopening its store"
                                      {:type ::reload-required} error)))))
    (pump! slot)
    result))
