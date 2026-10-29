(ns is.simm.documents.benchmark-reports
  "Ordinary Markdown report plans from an already authorized native Scorecard.
   Pure formatting only: no publication, execution, authority or receipt edits."
  (:require [clj-yaml.core :as yaml]
            [clojure.string :as str]
            [hasch.core :as hasch]
            [is.simm.documents.links :as links]
            [is.simm.documents.saved-view-spec :as spec]
            [is.simm.documents.scorecard-views :as scorecards]))

(def version 2)
(def index-path "reports/index.md")
(def report-root "reports/benchmarks")

(defn- report-id [kind context scorecard]
  ;; Presentation revisions must not change a document/view's stable identity.
  (hasch/uuid [::report 1 kind context scorecard]))

(defn- document [id title descriptor introduction & [provenance]]
  (str "---\n" (yaml/generate-string (cond-> {"simmis-id" (str id) "title" title}
                                      provenance (assoc "benchmark-provenance" provenance))) "---\n# " title "\n\n"
       introduction "\n\n```simmis-view\n" (pr-str descriptor) "\n```\n"))

(defn prepare
  "Return deterministic, bounded source plans for ONE native Scorecard and the
   room's report index. Caller must authorize/read the native bounded Scorecard
   first. No file target is selected here. A publisher must capture its target,
   use an absent-entry precondition, and reconcile exact bytes on retry; existing
   authored files must never be overwritten. Opening a report is read-only."
  [context summary definition]
  (when-not (and (map? context) (not (record? context))
                 (= #{:room :store} (set (keys context)))
                 (every? uuid? (vals context)))
    (throw (ex-info "Benchmark report needs a closed native Room/store identity"
                    {:type ::invalid-context})))
  (let [scorecard (when (string? (:id summary)) (parse-uuid (:id summary)))
        descriptor (spec/validate
                    {:version 1 :id (report-id :results context scorecard)
                     :source {:kind :scorecard :room :current :scorecard scorecard}
                     :presentation :table
                     :columns [{:kind :candidate} {:kind :attempts} {:kind :passed}
                               {:kind :pass-rate} {:kind :pass-rate-interval}
                               {:kind :reward} {:kind :cost} {:kind :notional-cost}]})
        prepared (scorecards/prepare descriptor context summary definition)]
    (when-not (and (= :ready (:status prepared)) (:complete? prepared))
      (throw (ex-info "A complete benchmark display is required to prepare its report"
                      {:type ::unavailable :reason (:reason prepared)})))
    (let [provenance (:provenance prepared)
          workflow (:workflow provenance)
          name (when workflow (str/capitalize (str/replace (:name workflow) "-" " ")))
          title (str (or name "Saved") " benchmark results")
          index-view (spec/validate
                      {:version 1 :id (report-id :index-view context nil)
                       :source {:kind :pages :roots [report-root]}
                       :presentation :table :columns [{:kind :title}]
                       :order [{:kind :title :direction :asc}]})
          introduction (str "Benchmark results. The table shows the retained result, "
                            "including unknown costs; editing this document does not change the evidence."
                            (when workflow
                              (str "\n\nCurrent workflow: [" name "](" (links/route {:kind :memory-file :room (:room context)
                                                                                :path (str "workflows/" (:name workflow) "/workflow.edn")})
                                   "). This benchmark used a saved revision."))
                            "\n\nCost is recorded model usage; list-price cost is shown separately. "
                            "Use Save snapshot on the table for portable result values. "
                            "Exact record and revision identities are retained in the document's metadata.")
          metadata (cond-> {"scorecard" (str scorecard)
                            "experiment" (str (:experiment-content-id provenance))
                            "dataset" (str (:dataset-content-id provenance))}
                     workflow (assoc "workflow-revision" (:bundle workflow)))]
      {:version version
       :source (assoc context :scorecard scorecard)
       :report {:path (str report-root "/" scorecard ".md")
                :page (report-id :page context scorecard)
                :text (document (report-id :page context scorecard) title descriptor introduction metadata)}
       :index {:path index-path :page (report-id :index-page context nil)
               :text (document (report-id :index-page context nil) "Reports" index-view
                               "Benchmark reports in this Memory. Open a report for results, workflow revisions and interpretation.")}})))
