(ns matter-artifacts (:require [clojure.edn :as edn] [clojure.string :as str]))
(defn text? [x n] (and (string? x) (<= 1 (count x) n) (not (str/blank? x))))
(defn read-one [raw]
  (try
    (when (and (string? raw) (<= (count raw) 131072))
      (with-in-str raw
        (let [eof (volatile! nil) opts {:eof eof :readers {} :default (fn [& _] (throw (ex-info "tag" {})))}
              value (edn/read opts *in*)]
          (when (identical? eof (edn/read opts *in*)) value))))
    (catch Exception _ nil)))
(defn record-for [documents {:keys [document part id]}]
  (get-in documents [document part id]))
(defn citation? [documents c]
  (and (map? c) (= #{:document :part :id :quote} (set (keys c)))
       (every? #(text? (get c %) 512) [:document :part :id]) (text? (:quote c) 2000)
       (let [record (record-for documents c)]
         (and (string? record) (str/includes? record (:quote c))))))
(defn issue? [issue]
  (and (map? issue) (text? (:id issue) 100) (text? (:title issue) 1000)
       (contains? #{:high :medium :low} (:priority issue)) (text? (:recommendation issue) 4000)
       (vector? (:evidence issue)) (<= 1 (count (:evidence issue)) 8)))
(defn check [{:keys [files gold]}]
  (let [memo (get files "/out/red-flag-memo.md")
        tracker (read-one (get files "/out/red-flag-tracker.edn"))
        memo? (and (text? memo 131072) (>= (count memo) 80))
        tracker? (and (map? tracker) (= 1 (:version tracker))
                      (vector? (:issues tracker)) (<= (count (:issues tracker)) 100)
                      (every? issue? (:issues tracker))
                      (= (count (:issues tracker)) (count (set (map :id (:issues tracker))))))
        citations? (and tracker? (every? (partial citation? (:documents gold)) (mapcat :evidence (:issues tracker))))
        checks {:memo-present? (boolean memo?) :tracker-structure? (boolean tracker?)
                :tracker-quotations-valid? (boolean citations?)}]
    {:checks checks :reward (if (every? true? (vals checks)) 1.0 0.0)}))
