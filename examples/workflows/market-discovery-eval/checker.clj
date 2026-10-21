(ns market-checker
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn nonblank? [x] (and (string? x) (not (str/blank? x))))
(defn text-list? [xs]
  (and (vector? xs) (<= 1 (count xs) 100) (every? nonblank? xs)))

(defn check [{:keys [files gold]}]
  (let [raw (get files "/out/analysis.edn")
        parsed (try (when (string? raw) (edn/read-string raw))
                    (catch Exception _ nil))
        claims (if (vector? (:claims parsed)) (:claims parsed) [])
        facts (:facts gold)
        ids (set (keys facts))
        claim-shape? (and (vector? (:claims parsed)) (<= (count claims) 100)
                          (every? (fn [c]
                                    (and (map? c)
                                         (contains? #{:fact :inference :hypothesis} (:kind c))
                                         (nonblank? (:statement c))
                                         (vector? (:source-ids c))
                                         (every? nonblank? (:source-ids c))
                                         (or (= :hypothesis (:kind c))
                                             (seq (:source-ids c)))))
                                  claims))
        known-sources? (and claim-shape? (every? #(every? ids (:source-ids %)) claims))
        grounded-facts? (and claim-shape? (every? (fn [c]
                                  (or (not= :fact (:kind c))
                                      (some #(contains? (get facts % #{}) (:statement c))
                                            (:source-ids c))))
                                claims))
        statements (set (map :statement (filter #(= :fact (:kind %)) claims)))
        checks {:output-contract? (boolean
                                    (and (map? parsed)
                                         (nonblank? (get files "/out/brief.md"))
                                         (text-list? (:unknowns parsed))
                                         (text-list? (:questions parsed))
                                         (text-list? (:next-actions parsed))
                                         claim-shape?))
                :known-source-ids? (boolean known-sources?)
                :fixture-facts-grounded? (boolean grounded-facts?)
                :required-fixture-facts? (boolean (every? statements (:required-facts gold)))
                :honest-status? (boolean
                                  (and (= (:status gold) (:status parsed))
                                       (or (seq facts) (empty? claims))))}]
    {:checks checks :reward (if (every? true? (vals checks)) 1.0 0.0)}))
