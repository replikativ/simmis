(ns is.simm.documents.benchmark-reports-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.benchmark-reports :as reports]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.page-views :as pages]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.saved-view-fences :as fences]
            [is.simm.documents.scorecard-views-test :as fixture]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]))

(deftest report-source-is-deterministic-ordinary-markdown-with-native-provenance
  (let [plan (reports/prepare fixture/context fixture/summary fixture/definition)
        report (:report plan)
        parsed (markdown/parse (:text report))
        views (fences/analyze (:nodes parsed))]
    (is (= plan (reports/prepare fixture/context fixture/summary fixture/definition)))
    (is (= (str "reports/benchmarks/" (:id fixture/summary) ".md") (:path report)))
    (is (str/includes? (:text report) (str fixture/experiment-id)))
    (is (str/includes? (:text report) (str (get-in fixture/definition [:experiment/dataset :dataset/content-id]))))
    (is (not (str/includes? (pr-str plan) "__host_private_credentials__")))
    (is (= 1 (count views)))
    (is (= :ready (:status (first views))))
    (is (= {:kind :scorecard :room :current :scorecard (parse-uuid (:id fixture/summary))}
           (get-in views [0 :descriptor :source])))
    (is (not (str/includes? (:text report) "$0.123456")) "Values are read from evidence, not baked into a second authority")
    (is (not= (:page report)
              (get-in (reports/prepare (assoc fixture/context :store (random-uuid)) fixture/summary fixture/definition)
                      [:report :page])))))

(deftest report-index-is-a-bounded-page-view-over-only-report-pages
  (let [plan (reports/prepare fixture/context fixture/summary fixture/definition)
        index (:index plan)
        view (first (fences/analyze (:nodes (markdown/parse (:text index)))))]
    (is (= "reports/index.md" (:path index)))
    (is (= :ready (:status view)))
    (is (= ["reports/benchmarks"] (get-in view [:descriptor :source :roots])))
    (is (= :pages (get-in view [:descriptor :source :kind])))
    (is (= (:index plan)
           (:index (reports/prepare fixture/context
                                    (assoc fixture/summary :id (str (random-uuid))) fixture/definition))))))

(deftest malformed-or-incomplete-native-input-cannot-become-a-report-plan
  (doseq [[context summary definition]
          [[fixture/context (assoc fixture/summary :id "../other") fixture/definition]
           [(assoc fixture/context :store :not-an-id) fixture/summary fixture/definition]
           [(assoc fixture/context :extra :not-identity) fixture/summary fixture/definition]
           [fixture/context (assoc fixture/summary :cells 0) fixture/definition]
           [fixture/context fixture/summary (assoc fixture/definition :experiment/repetitions 2)]]]
    (is (thrown? clojure.lang.ExceptionInfo (reports/prepare context summary definition)))))

(deftest generated-documents-project-and-the-index-finds-readable-report-titles
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :attribute-refs? false :keep-history? false}
        plan (reports/prepare fixture/context fixture/summary fixture/definition)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (space/provision! conn)
        (doseq [{:keys [path text]} [(:report plan) (:index plan)]]
          (repo/write! conn path (markdown/source-bytes text)))
        (repo/write! conn "README.md" (markdown/source-bytes "# Ordinary imported Markdown\n"))
        (let [before (:max-tx @conn)
              projected (view/prepare @conn profile/selection-options)
              descriptor (-> (:index plan) :text markdown/parse :nodes fences/analyze first :descriptor)
              result (pages/prepare @conn descriptor)]
          (is (= :ready (:status projected)) (pr-str (:diagnostics projected)))
          (is (= :ready (:status result)))
          (is (= 1 (:total result)))
          (is (= (get-in plan [:report :path]) (get-in result [:rows 0 :path])))
          (is (= (get-in plan [:report :page]) (get-in result [:rows 0 :id])))
          (is (str/starts-with? (get-in result [:rows 0 :title]) "research — "))
          (is (= (get-in result [:rows 0 :title]) (get-in result [:rows 0 :cells 0 :text])))
          (is (= (get-in plan [:report :path])
                 (:S.Page/title (d/entity (:db projected) [:entity/uuid (get-in plan [:report :page])]))))
          (is (= before (:max-tx @conn)) "Opening the report index does not write files"))
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest report-plans-preserve-unknown-cost-and-omit-unavailable-workflow-provenance
  (let [summary (assoc-in fixture/summary [:leaderboard 0 :spend :priced?] false)
        definition (dissoc fixture/definition :experiment/metadata)
        plan (reports/prepare fixture/context summary definition)]
    (is (str/includes? (get-in plan [:report :text]) "# Benchmark — "))
    (is (not (str/includes? (get-in plan [:report :text]) "Workflow revision:")))
    (is (not (str/includes? (get-in plan [:report :text]) "$0")))
    (is (str/includes? (get-in plan [:report :text]) ":kind :cost"))))
