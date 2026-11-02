(ns evidence-checker
  (:require [clojure.edn :as edn]))

(defn check [{:keys [files gold]}]
  (let [raw (get files "/out/answer.edn")
        answer (try
                 (when (string? raw)
                   (with-in-str raw
                     (let [eof (volatile! nil)
                           answer (edn/read {:eof eof} *in*)
                           trailing (edn/read {:eof eof} *in*)]
                       (when (identical? eof trailing) answer))))
                 (catch Exception _ nil))
        evidence (:evidence answer)
        shape? (and (map? answer) (= #{:decision :evidence} (set (keys answer)))
                    (contains? #{:supported :refuted :unknown} (:decision answer))
                    (vector? evidence) (<= (count evidence) 4)
                    (every? string? evidence))
        checks {:output-contract? (boolean shape?)
                :decision-correct? (boolean (and shape? (= (:decision gold) (:decision answer))))
                :evidence-grounded? (boolean (and shape? (= (:evidence gold) evidence)))}]
    {:checks checks
     :reward (/ (double (count (filter true? (vals checks)))) (count checks))}))
