(ns is.simm.uis.web.desktop.replica-io
  "Native store ownership, independent of UI projections. Same physical store
   constructors and retirement share one slot across epochs and client peers."
  (:require [clojure.core.async :refer [go <! alts! promise-chan put!] :include-macros true]
            [datahike.api :as d]
            [datahike.kabel.connector :as connector]
            [datahike.optimistic :as optimistic]
            [is.simm.uis.web.desktop.resource-slot :as resource]))

(defonce ^:private stores (atom {}))

(defn- identity-key [config]
  ;; Datahike's native store registry is UUID-keyed, regardless of backend/name.
  (or (get-in config [:store :id])
      (throw (ex-info "Replica store requires an explicit native identity" {}))))

(defn- entry-for! [config]
  (let [key (identity-key config)
        shape (select-keys (:store config) [:backend :id :name :path :frontend-config :backend-config])
        entry (or (get @stores key)
                  (let [value {:shape shape :slot (resource/slot) :pending (atom nil)}]
                    (get (swap! stores #(if (contains? % key) % (assoc % key value))) key)))]
    (when-not (= shape (:shape entry))
      (throw (ex-info "Replica identity is already owned by a different store configuration"
                      {:store-id key})))
    entry))

(defn- withdraw-preparation! [entry]
  (when-let [pending @(:pending entry)]
    (reset! (:pending entry) nil)
    (put! (:cancel pending) true)))

(defn overlay!
  "Create the resource-owned native overlay exactly once, before publication."
  [{:keys [conn overlay]}]
  (or @overlay (let [value (optimistic/open conn)] (reset! overlay value) value)))

(defn close-overlay! [{:keys [overlay]}]
  (when-let [value @overlay] (optimistic/close! value)))

(defn- supported-disposal? [config]
  ;; Complete owned release is joined below, but retain the backend allowlist
  ;; verified by our integration tests rather than promising plugin correctness.
  (let [store (:store config)]
    (and (= :tiered (:backend store))
         (= :memory (get-in store [:frontend-config :backend]))
         (= :indexeddb (get-in store [:backend-config :backend])))))

(defn- dispose! [config {:keys [conn] :as handle}]
  (close-overlay! handle)
  (go
    (try
      (when-not (supported-disposal? config)
        (throw (ex-info "Joined native disposal is only verified for memory/IndexedDB replicas" {})))
      (d/release conn)
      (let [closed (<! (connector/await-release! conn))]
        (when-not (true? closed)
          (throw (if (instance? js/Error closed) closed
                     (ex-info "Replica resource cleanup did not confirm completion" {}))))
        true)
      (catch :default error error))))

(defn open!
  "Publish only the latest valid native open. Logical cancellation leaves the
   constructor owned until its returned handle has been disposed. prepare! may
   reauthorize/register a room first; it never opens another client replica."
  [config {:keys [prepare! generation valid? publish! unpublish!]}]
  (try
    (when-not (and (some? generation) (every? fn? [valid? publish! unpublish!]))
      (throw (ex-info "Replica open requires a generation and host callbacks" {})))
    (let [{:keys [slot pending] :as entry} (entry-for! config)]
      (if (= generation (:generation @pending))
        (:result @pending)
        (let [result (promise-chan) cancel (promise-chan)
              request {:generation generation :result result :cancel cancel}
              current? #(and (identical? request @pending) (valid?))
              withdrawn #(ex-info "Replica open was withdrawn before native construction" {::clean? true})]
          (withdraw-preparation! entry)
          (reset! pending request)
          (go
            (try
              ;; Authorization RPCs own NO native resource. A lost RPC must not
              ;; hold the physical lease; retirement also cancels this waiter.
              (let [preparation (if prepare! (prepare!)
                                    (doto (promise-chan) (put! {:ready? true})))
                    [prepared source] (alts! [cancel preparation] :priority true)]
                (put! result
                      (cond
                        (or (= source cancel) (not (current?))) (withdrawn)
                        (:error prepared) (ex-info "Server refused replica preparation" {::clean? true} (:error prepared))
                        (not (:ready? prepared)) (withdrawn)
                        :else
                        (let [native-result
                              (resource/request!
                               slot
                               {:generation generation :valid? current? :publish! publish! :unpublish! unpublish!
                                :dispose! #(dispose! config %)
                                :clean-error? #(or (::clean? (ex-data %)) (connector/cleanup-confirmed? %))
                                :recover-open! connector/recover-connect!
                                :open! (fn []
                                         (go
                                           (if-not (current?) (withdrawn)
                                                   (let [conn (<! (connector/connect-kabel config {:sync? false}))]
                                                     (if (instance? js/Error conn) conn
                                                         (when conn {:conn conn :overlay (atom nil)}))))))})
                              [value source] (alts! [cancel native-result] :priority true)]
                          ;; Logical cancellation is NOT proof of native
                          ;; cleanup. The slot retains that physical lifetime.
                          (if (= source cancel)
                            (ex-info "Replica publication superseded; native ownership retained" {:type ::superseded})
                            value)))))
              (catch :default error
                ;; This boundary may follow native dispatch. Do not turn an
                ;; unexpected host exception into proof of native cleanup.
                (put! result error))))
          result)))
    (catch :default error (doto (promise-chan) (put! error)))))

(defn retire!
  "Return the old physical lifetime's drain; a replacement may queue meanwhile."
  [config]
  (if-let [entry (get @stores (identity-key config))]
    (do (withdraw-preparation! entry) (resource/retire! (:slot entry)))
    (doto (promise-chan) (put! true))))

(defn retire-all! []
  (mapv (fn [entry] (withdraw-preparation! entry) (resource/retire! (:slot entry))) (vals @stores)))

(defn recover! [config]
  (let [entry (entry-for! config)]
    ;; Recovery is explicit and cleanup-only. A subsequent request, even for
    ;; the same epoch, must not reuse the failed logical result.
    (withdraw-preparation! entry)
    (resource/retry-cleanup! (:slot entry))))

(defn status [config]
  (when-let [entry (get @stores (identity-key config))]
    (select-keys @(:state (:slot entry)) [:phase :error])))
