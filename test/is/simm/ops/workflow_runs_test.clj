(ns is.simm.ops.workflow-runs-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as store]
            [dvergr.rooms.forks :as forks]
            [dvergr.rooms.files :as room-files]
            [dvergr.system.db :as system-db]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflows :as workflows]
            [jsonista.core :as json]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def source
  {"workflow.edn" "{:title \"Reusable document\" :task \"Write {subject} to /out/result.md\" :params {:subject \"default\"} :capture [\"/out\"]}"
   "checker.clj" "(throw (ex-info \"Checker MUST NOT execute\" {}))"
   "calibration.edn" "{:not-a-valid-calibration true}"
   "gold.edn" "{:expected \"hidden only in benchmarks, NOT here\"}"
   "helpers/data.txt" "pinned helper"
   "fixtures/docs/input.md" "shared fixture"})

(defn snapshot [files]
  {:files files :manifest (mapv (fn [[p s]] {:path p :size (alength (.getBytes s "UTF-8"))}) files)})

(defn input
  ([] (input source))
  ([files]
   {:request-id (random-uuid) :name "echo" :expected-bundle (:id (catalog/bundle "echo" files))
    :model "codex-subscription-sol" :params {"subject" "a literal {subject} value"}
    :limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 4
             :timeout-ms 30000 :settlement "review"}}))

(deftest transport-normalizes-only-keywordized-map-keys-and-refuses-collisions
  (let [raw (assoc (input) :params {:subject "资料"} :inputs {(keyword "/docs/输入.md") "literal"})
        normalized (workflow-runs/transport raw)]
    (is (= {"subject" "资料"} (:params normalized)))
    (is (= {"/docs/输入.md" "literal"} (:inputs normalized)))
    (is (m/validate workflow-runs/schema normalized))
    (is (thrown? clojure.lang.ExceptionInfo
                 (workflow-runs/transport (assoc-in raw [:params "subject"] "duplicate"))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (workflow-runs/transport (assoc raw :eval true))))))

(deftest saved-workflows-pin-inputs-but-use-ordinary-completion-not-authored-grading
  (let [prepared (with-redefs [checkers/run-checker (fn [& _] (throw (ex-info "No checker execution" {})))
                               checkers/calibrate (fn [& _] (throw (ex-info "No calibration execution" {})))]
                   (workflow-runs/prepare (snapshot source) (input)))
        env (:environment prepared)]
    (is (= "Write a literal {subject} value to /out/result.md" (:environment/task env)))
    (is (= env (environment/validate-environment env)))
    (is (= work/completion (:evaluator prepared)))
    (is (= :workflow (:request-kind prepared)))
    (is (= :saved-workflow (:execution-kind prepared)))
    (is (= source (get-in prepared [:basis :source])))
    (is (= {"/docs/input.md" "shared fixture"} (get-in prepared [:basis :inputs])))
    (is (= (str (:expected-bundle (input))) (get-in env [:environment/metadata :workflow :bundle])))
    (is (= (evaluation/world-setup-ref (:world-setup prepared))
           (get-in env [:environment/world :setup])))))

(deftest cases-and-explicit-files-overlay-only-the-selected-frozen-fixtures
  (let [files (merge source {"cases/one/fixtures/docs/input.md" "case one"
                             "cases/two/fixtures/docs/input.md" "case two"})
        request (assoc (input files) :case "one" :inputs {"/docs/input.md" "explicit"
                                                          "/docs/extra.md" "extra"})
        prepared (workflow-runs/prepare (snapshot files) request)]
    (is (= {"/docs/input.md" "explicit" "/docs/extra.md" "extra"} (get-in prepared [:basis :inputs])))
    (doseq [case [nil "unknown"]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (workflow-runs/prepare (snapshot files) (assoc request :case case)))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (workflow-runs/prepare (snapshot source) (assoc (input) :case "one"))))))

(deftest setup-identity-includes-the-named-subtree-not-only-identical-content
  (let [request (input)
        a (workflow-runs/prepare (snapshot source) request)
        b (workflow-runs/prepare (snapshot source) (assoc request :name "another"))]
    (is (= (:id (catalog/bundle "echo" source)) (:id (catalog/bundle "another" source))))
    (is (not= (evaluation/world-setup-ref (:world-setup a))
              (evaluation/world-setup-ref (:world-setup b))))))

(deftest revision-parameters-paths-and-bundle-policies-are-not-silent-fallbacks
  (doseq [request [(assoc (input) :expected-bundle (random-uuid))
                   (assoc (input) :params {"undeclared" "x"})
                   (assoc (input) :params {"subject" 42})
                   (assoc (input) :params {"ns/subject" "x"})
                   (assoc-in (input) [:limits :settlement] "merge")
                   (assoc (input) :provider :custom)]]
    (is (thrown? clojure.lang.ExceptionInfo (workflow-runs/prepare (snapshot source) request))))
  (doseq [path ["/workflows" "/workflows/echo" "/workflows/echo/helpers/data.txt"
                "/.git/config" "/.env" "relative.md" "/docs/../input" "/docs//input" "/docs\\input"]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (workflow-runs/prepare (snapshot source) (assoc (input) :inputs {path "bad"})))))
  (doseq [addition [":profile \"other\"" ":capture [\"/\"]" ":judge {:model \"x\"}"
                    ":web :live" ":fetched false" ":allowed-sources []"]]
    (let [files (assoc source "workflow.edn"
                       (str "{:title \"bad\" :task \"task\" " addition "}"))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (workflow-runs/prepare (snapshot files) (dissoc (input files) :params)))))))

(deftest expanded-task-and-aggregate-input-bounds-are-byte-based
  (let [files (assoc source "workflow.edn"
                     "{:title \"Large\" :task \"{subject}{subject}{subject}{subject}{subject}\" :params {:subject \"default\"}}")
        request (assoc (input files) :params {"subject" (apply str (repeat 1500 "世"))})]
    (is (thrown? clojure.lang.ExceptionInfo (workflow-runs/prepare (snapshot files) request))))
  (is (thrown? clojure.lang.ExceptionInfo
               (workflow-runs/prepare (snapshot source)
                                      (assoc (input) :inputs {"/big" (apply str (repeat 200000 "世"))}))))
  (let [files (assoc source "workflow.edn"
                     (pr-str {:title "Repeated" :task (apply str (repeat 1000 "{subject}"))
                              :params {:subject "default"}}))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (workflow-runs/prepare (snapshot files)
                                        (assoc (input files) :params {"subject" (apply str (repeat 4000 "x"))}))))))

(deftest literal-parameters-cannot-collide-or-produce-undeclared-substitutions
  (doseq [definition [{:title "missing" :task "{missing}"}
                      {:title "namespaced" :task "{subject}" :params {:scope/subject "one" :subject "two"}}
                      {:title "typed" :task "{subject}" :params {:subject 1}}]]
    (let [files (assoc source "workflow.edn" (pr-str definition))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (workflow-runs/prepare (snapshot files) (dissoc (input files) :params))))))
  (let [files (assoc source "workflow.edn"
                     (pr-str {:title "literal" :task "{subject} / {other}"
                              :params {:subject "default" :other "second"}}))
        prepared (workflow-runs/prepare (snapshot files)
                                        (assoc (input files) :params {"subject" "{other}"}))]
    (is (= "{other} / second" (get-in prepared [:environment :environment/task])))))

(deftest aggregate-parameter-and-file-bounds-apply-even-to-unused-defaults
  (let [files (assoc source "workflow.edn"
                     (pr-str {:title "Large parameters" :task "A task"
                              :params (into {} (for [n (range 16)]
                                                 [(keyword (str "p" n)) (apply str (repeat 1500 "世"))]))}))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (workflow-runs/prepare (snapshot files) (dissoc (input files) :params)))))
  (doseq [entries [(into {} (for [n (range 5)] [(str "/large-" n) (apply str (repeat 524288 "a"))]))
                   (into {} (for [n (range 100)] [(str "/small-" n) "a"]))]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (workflow-runs/prepare (snapshot source) (assoc (input) :inputs entries))))))

(defn- realistic-locators! [conn w room]
  (d/transact conn [{:room/id (:room w) :room/slug (store/room-id->slug (:id room))}])
  (d/transact conn (mapv (fn [system]
                           {:system/id (:system/id system)
                            :system/scope (str "/fixture/systems/" (:system/id system))})
                         (filter #(contains? #{:kb :repo} (:system/type %)) (system-db/all-systems)))))

(defn- response [n]
  (let [events (concat
                (when (= n 1)
                  [{:type "response.output_item.done"
                    :item {:type "function_call" :call_id "capture-input" :name "clojure_eval"
                           :arguments (json/write-value-as-string
                                       {:code "(do (babashka.fs/create-dirs \"/out\") (spit \"/out/result.md\" (str (slurp \"/workflows/echo/helpers/data.txt\") \"|\" (slurp \"/docs/input.md\") \"|\" (babashka.fs/exists? \"/workflows/echo/helpers/new.txt\"))))"})}}])
                (when (> n 1) [{:type "response.output_text.delta" :delta "Prepared the result for review."}])
                [{:type "response.completed" :response {:id (str "fixture-" n) :model "gpt-5.6-sol"
                                                        :usage {:input_tokens 100 :output_tokens 20}}}])]
    {:status 200 :body (java.io.ByteArrayInputStream.
                        (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8"))}))

(deftest actual-sci-run-restores-pinned-source-preserves-parent-and-merges-only-on-review
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (realistic-locators! conn w room)
       (workflows/write! room {:principal/id "fixture"} {:name "echo" :files source})
       (workspace/seed! room {"/docs/input.md" "trunk input"})
       (let [request (assoc (input) :inputs {"/docs/input.md" "explicit input"})
             principal (ops/party-principal (:owner w)) host-ctx ec/*execution-context*
             calls (atom 0) changed (atom false) retained (volatile! nil)
             original-snapshot workflows/snapshot
             provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
         (try
           (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits request))})
           (with-redefs [gateway/*request-fn* (fn [_] (response (swap! calls inc)))
                         providers/providers (atom {:codex-subscription provider})
                         checkers/run-checker (fn [& _] (throw (ex-info "Never grade ordinary work" {})))
                         workflows/snapshot (fn [r name]
                                              (let [frozen (original-snapshot r name)]
                                                (when (and (= (:id room) (:id r)) (compare-and-set! changed false true))
                                                  (workflows/write! room {:principal/id "concurrent-editor"}
                                                                    {:name name :files {"helpers/data.txt" "new helper"
                                                                                        "helpers/new.txt" "extra helper"}}))
                                                frozen))]
             (let [admission (workflow-runs/submit! conn host-ctx principal (:room w) room request)
                   status (jobs/status (:job admission) 40000)
                   attempts (store/-list-attempts (:store room) (store/conversation-id room) {})
                   child (first (remove #(= (:job admission) (:run/id %))
                                        (run/runs room {:root-run-id (:job admission) :limit 10})))]
               (vreset! retained (registry/lookup (:run/world child)))
               (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
               (is (:settled? status))
               (is (= 2 @calls))
               (is (= {"/out/result.md" "pinned helper|explicit input|false"}
                      (get-in (first attempts) [:attempt/evidence :files])))
               (is (empty? (workspace/read-tree room "/out")))
               (is (= "trunk input" (get (workspace/read-tree room "/docs") "/docs/input.md")))
               (is (= "new helper" (get-in (original-snapshot room "echo") [:files "helpers/data.txt"])))
               (is (= :review (:run/settlement-status child)))
               (is @retained)
               (testing "exact retry ignores newer files and does not resolve a model again"
                 (with-redefs [workflows/snapshot (fn [& _] (throw (ex-info "No source reread" {})))
                               work/prepare (fn [& _] (throw (ex-info "No model resolution" {})))]
                   (is (= (:job admission) (:job (workflow-runs/submit! conn host-ctx principal (:room w) room request))))))
               (is (= 2 @calls))
               (doseq [changed [(assoc request :case "other") (assoc request :model "other")
                                (assoc request :params {"subject" "other"})
                                (assoc request :inputs {"/docs/input.md" "other"})
                                (assoc request :expected-bundle (random-uuid))
                                (update-in request [:limits :max-dispatches] inc)]]
                 (is (thrown? clojure.lang.ExceptionInfo
                              (workflow-runs/submit! conn host-ctx principal (:room w) room changed))))
               (let [review (forks/review @retained)
                     merged (forks/merge! @retained :expect-state (:state review))]
                 (is (:ok? merged) (pr-str merged))
                 (is (= "explicit input" (get (workspace/read-tree room "/docs") "/docs/input.md")))
                 (is (= "pinned helper" (get-in (original-snapshot room "echo") [:files "helpers/data.txt"])))
                 (is (not (contains? (:files (original-snapshot room "echo")) "helpers/new.txt")))
                 (is (= {"/out/result.md" "pinned helper|explicit input|false"} (workspace/read-tree room "/out"))))))
           (finally
             (when (and @retained (nil? (discourse/settlement @retained))) (forks/discard! @retained)))))))))

(deftest authority-is-rechecked-before-any-source-or-model-access
  (world/with-world [conn w]
    (let [calls (atom 0)]
      (with-redefs [workflows/snapshot (fn [& _] (swap! calls inc))
                    work/prepare (fn [& _] (swap! calls inc))]
        (doseq [party [(:member w) (:non-member w) (:admin w) nil]]
          (is (thrown? clojure.lang.ExceptionInfo
                       (workflow-runs/submit! conn ec/*execution-context* (ops/party-principal party)
                                              (:room w) nil (input)))))
        (is (zero? @calls))
        (is (empty? (requests/requests conn)))))))

(deftest failed-source-materialization-starts-no-model-and-returns-unused-allowance
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (workflows/write! room {:principal/id "fixture"} {:name "echo" :files source})
       (let [request (input) principal (ops/party-principal (:owner w))
             calls (atom 0) allowance (work/resources (:limits request))]
         (resource/mint! room {:id (random-uuid) :resources allowance})
         (with-redefs [gateway/*request-fn* (fn [& _] (swap! calls inc) (throw (ex-info "No model before setup" {})))
                       room-files/write! (fn [& _] (throw (ex-info "Source materialization failed" {})))]
           (let [admitted (workflow-runs/submit! conn ec/*execution-context* principal (:room w) room request)
                 status (jobs/status (:job admitted) 40000)]
             (is (= :failed (:status status)))
             (is (re-find #"Source materialization failed" (or (:error status) "")))
             (is (:settled? status))
             (is (zero? @calls))
             (is (= allowance (resource/balance room)))
             (is (false? (:return-pending? (work/status conn ec/*execution-context* principal (:room w) (:id admitted)))))
             (is (= source (:files (workflows/snapshot room "echo"))))
             (is (empty? (store/-list-attempts (:store room) (store/conversation-id room) {}))))))))))

(deftest pointerless-saved-work-is-recovered-as-interrupted-never-reexecuted
  (world/with-world [conn w]
    (let [input (input)
          principal (ops/party-principal (:owner w))
          claimed (requests/claim! conn {:party (:owner w) :room (:room w) :client-id (:request-id input)
                                         :kind :workflow :limits (update (:limits input) :settlement keyword)
                                         :intent {:submission input}})
          id (get-in claimed [:request :work.request/id])]
      (work/recover! conn ec/*execution-context* (fn [& _] (throw (ex-info "No job to resolve" {}))))
      (is (= :interrupted (:work.request/state (requests/get-request conn id))))
      (with-redefs [workflows/snapshot (fn [& _] (throw (ex-info "No reread" {})))
                    work/prepare (fn [& _] (throw (ex-info "No re-execution" {})))]
        (is (= :interrupted (:state (workflow-runs/submit! conn ec/*execution-context* principal (:room w) nil input))))
        (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                          [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])
        (is (thrown? clojure.lang.ExceptionInfo
                     (workflow-runs/submit! conn ec/*execution-context* principal (:room w) nil input)))))))
