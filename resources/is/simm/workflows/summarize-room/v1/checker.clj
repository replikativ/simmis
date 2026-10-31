(ns simmis.workflows.summarize-room-v1
  (:require [clojure.string :as str]))

;; Benchmark-only fragment regression. HOST applies the closed model contract,
;; exact selection membership and retained-original checks before display.
(defn check [{:keys [files gold]}]
  (let [text (get files "/out/digest.edn")
        checks {:expected-output? (= #{"/out/digest.edn"} (set (keys files)))
                :fixture? (= 1 (:version gold))
                :bounded? (and (string? text) (<= (count text) 65536))
                :required? (and (string? text) (every? #(str/includes? text %) (:required gold)))
                :forbidden? (and (string? text) (not-any? #(str/includes? text %) (:forbidden gold)))}]
    {:checks (into {} (map (fn [[key value]] [key (boolean value)])) checks)
     :reward (/ (double (count (filter true? (vals checks)))) (count checks))}))
