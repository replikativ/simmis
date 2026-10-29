(ns is.simm.documents.benchmark-result
  "A compact, immutable chat projection of an already authorized Scorecard."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [is.simm.documents.links :as links]
            [is.simm.documents.scorecard-views :as scorecards]))

(defn- escape-cell [text]
  (-> (str text) (str/replace #"[\\`*_{}\[\]()#+.!|<>~-]" #(str "\\" %))
      (str/replace #"[\r\n]" " ")))

(defn- cell-text [{:keys [kind text unit approximate?]}]
  (if (= :null kind) "Unknown"
      (str (when approximate? "≈") (when (= :usd unit) "$") text
           (when (= :percent unit) "%"))))

(defn content
  "Same typed cells, unknown-cost semantics and provenance as Memory reports.
   No model ranking, significance claim, publication or execution."
  [context summary definition]
  (let [id (parse-uuid (:id summary))
        view (scorecards/prepare
              {:version 1 :id (hasch/uuid [::view context id])
               :source {:kind :scorecard :room :current :scorecard id}
               :presentation :table :limit 8
               :columns [{:kind :candidate} {:kind :attempts} {:kind :passed}
                         {:kind :reward} {:kind :cost} {:kind :notional-cost}]}
              context summary definition)]
    (when-not (and (= :ready (:status view)) (:complete? view))
      (throw (ex-info "Benchmark summary exceeds chat display limits" {:type ::unavailable})))
    (str "Benchmark results\n\n"
         "| Candidate | Attempts | Passed | Mean reward | Model cost | List-price cost |\n"
         "| --- | ---: | ---: | ---: | ---: | ---: |\n"
         (str/join "\n" (map (fn [row]
                               (str "| " (str/join " | " (map (comp escape-cell cell-text) (:cells row))) " |"))
                             (:rows view)))
         (when-let [workflow (get-in view [:provenance :workflow])]
           (str "\n\nCurrent workflow: [" (escape-cell (str/capitalize (str/replace (:name workflow) "-" " ")))
                "](" (links/route {:kind :memory-file :room (:room context)
                                   :path (str "workflows/" (:name workflow) "/workflow.edn")})
                "). This benchmark used a saved revision."))
         "\n\nThis is the retained benchmark result, not a proposal or a claim about untested cases. "
         "Create a Memory report for the full result table and provenance.")))
