(ns is.simm.ops.worker-wire
  "Explicit structural conversions for the private worker JSON protocol.
   Never keywordize filenames or arbitrary schema/property maps."
  (:require [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-frames :as frames])
  (:import [java.util UUID]))

(defn refuse! []
  (throw (ex-info "Invalid local worker protocol message" {:type ::refused})))

(defn object!
  "Normalize only a finite, locally known set of structural field names."
  [value allowed required]
  (when-not (and (map? value) (every? (set (map name allowed)) (keys value))
                 (every? #(contains? value (name %)) required)) (refuse!))
  (into {} (map (fn [k] [k (get value (name k))]))
        (filter #(contains? value (name %)) allowed)))

(defn encode
  "Convert internal protocol values without dropping keyword namespaces or
   allowing colliding string/keyword fields. Framing supplies size bounds."
  [value]
  (let [remaining (volatile! (:max-nodes frames/policy))]
    (letfn [(visit [v depth]
              (when (or (> depth (:max-depth frames/policy)) (neg? (vswap! remaining dec))) (refuse!))
              (cond
                (or (nil? v) (string? v) (number? v) (boolean? v)) v
                (keyword? v) (subs (str v) 1)
                (uuid? v) (str v)
                (vector? v) (mapv #(visit % (inc depth)) v)
                (map? v) (reduce-kv
                          (fn [out k item]
                            (when-not (or (string? k) (keyword? k)) (refuse!))
                            (let [field (visit k (inc depth))]
                              (when (contains? out field) (refuse!))
                              (assoc out field (visit item (inc depth))))) {} v)
                :else (refuse!)))]
      (visit value 0))))

(defn config! [value]
  (object! value [:adapter :version :model :policy] [:adapter :version :model :policy]))

(defn frozen! [value]
  (try
    (let [f (object! value [:input-id :task :config :files :limits :policy]
                     [:input-id :task :config :files :limits :policy])
          f (-> f
                (update :input-id #(UUID/fromString %))
                (update :config config!)
                (update :limits #(object! % [:timeout-ms :settlement] [:timeout-ms :settlement]))
                (update :policy #(object! % (keys files/policy) (keys files/policy)))
                (update :files
                        (fn [selected]
                          (when-not (map? selected) (refuse!))
                          (into {} (map (fn [[path file]]
                                          [path (object! file [:base64 :bytes :sha256 :mode]
                                                         [:base64 :bytes :sha256 :mode])])) selected))))]
      (files/validate-frozen! f))
    (catch Throwable _ (refuse!))))

(defn request! [value]
  (let [r (object! value [:version :sequence :messages :options] [:version :sequence :messages :options])
        opts (object! (:options r) [:system :tools] [])]
    (when-not (vector? (:messages r)) (refuse!))
    (assoc r
           :messages (mapv (fn [message]
                             (let [m (object! message [:type :role :content :call_id :name :arguments :output] [])]
                               (if (vector? (:content m))
                                 (update m :content #(mapv (fn [part] (object! part [:type :text] [:type :text])) %)) m)))
                           (:messages r))
           :options (if (contains? opts :tools)
                      (do (when-not (vector? (:tools opts)) (refuse!))
                          (update opts :tools #(mapv (fn [tool]
                                                       (object! tool [:name :description :parameters :input_schema] [:name])) %)))
                      opts))))

(defn reply! [value]
  (let [r (object! value [:version :sequence :response] [:version :sequence :response])
        response (object! (:response r) [:content :reasoning :tool-calls :usage :stop-reason :model :id] [:content])]
    (assoc r :response
           (cond-> response
             (:usage response)
             (update :usage #(object! % [:input-tokens :output-tokens :cache-read-tokens :cache-write-tokens
                                         :reasoning-output-tokens] []))
             (:tool-calls response)
             (update :tool-calls
                     (fn [calls]
                       (when-not (vector? calls) (refuse!))
                       (mapv (fn [call]
                               (let [call (object! call [:id :name :input] [:id :name :input])
                                     fields (case (:name call)
                                              "clojure_eval" [:code]
                                              "write_file" [:path :content]
                                              (refuse!))]
                                 (update call :input #(object! % fields fields)))) calls)))))))
