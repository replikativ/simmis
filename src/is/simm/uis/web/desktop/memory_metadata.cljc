(ns is.simm.uis.web.desktop.memory-metadata
  "Pure bounded metadata demand planning. No source bytes, handles or IO.
   Coverage says what an observation contains, never whether it is current."
  (:require [clojure.set :as set]))

(def request-limit 64)

(defn normalize-request [request]
  (when-not (and (map? request) (= #{:folders :paths} (set (keys request)))
                 (every? (fn [k] (let [items (get request k)]
                                  (and (vector? items) (<= (count items) request-limit)
                                       (every? string? items)))) [:folders :paths]))
    (throw (ex-info "Metadata demand must contain at most64 folders and64 paths"
                    {:type ::invalid-demand})))
  (into {} (for [k [:folders :paths]] [k (vec (sort (set (get request k))))])))

(defn- request-union [a b]
  (into {} (for [k [:folders :paths]]
             [k (vec (sort (set/union (set (get a k)) (set (get b k)))))])))

(defn- bounded? [request]
  (every? #(<= (count (get request %)) request-limit) [:folders :paths]))

(defn batches
  "Deterministic first-fit groups of WHOLE owner demands. Capture each owner's
   draft generations separately when admitting a batch. Never truncate or claim
   that different batches are one atomic database snapshot. Invalid demands
   refuse before returning any partial plan."
  [owner-requests]
  (let [demands (mapv (fn [[owner request]] [owner (normalize-request request)])
                      (sort-by (comp pr-str key) owner-requests))]
    (reduce
     (fn [groups [owner request]]
       (if-some [i (first (keep-indexed
                          (fn [i group]
                            (when (bounded? (request-union (:request group) request)) i)) groups))]
         (-> groups (update-in [i :request] request-union request)
                    (update-in [i :owners] conj owner))
         (conj groups {:request request :owners #{owner}})))
     [] demands)))

(defn covered?
  "Exact snapshot coverage: requested nil is known absence; omitted is unknown.
   Caller must FIRST validate its session/socket/home/target/hint epoch. A
   metadata revision UUID is not an ordering or cross-batch freshness proof."
  [snapshot request]
  (and (map? (:folders snapshot)) (map? (:entries snapshot))
       (every? #(contains? (:folders snapshot) %) (:folders request))
       (every? #(contains? (:entries snapshot) %) (:paths request))))
