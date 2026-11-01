(ns is.simm.ops.coordinator-benchmark-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [datahike.api :as d]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.subexperiment :as subexperiment]
            [dvergr.runtime.preparation :as preparation]
            [dvergr.agent.roster :as roster]
            [dvergr.catalog.workspace :as catalog]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.room.store.memory :as memory]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-fixture :as fixture]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.coordinator :as coordinator]
            [is.simm.ops.coordinator-benchmark :as benchmark]
            [is.simm.ops.coordinator-sandbox :as sandbox]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-schedule-fixture :as owned-fixture]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.results :as results]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test :as work-fixture]
            [is.simm.ops.workflows :as workflows]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent Semaphore]))

(def files
  {"workflow.edn" "{:title \"Fixed development benchmark\" :task \"Read /docs/input.md and write it to /out/result.md\" :capture [\"/out\"]}"
   "checker.clj" "(ns fixed.checker) (defn check [{:keys [files gold]}] (let [ok (= (:expected gold) (get files \"/out/result.md\"))] {:checks {:correct? ok} :reward (if ok 1.0 0.0)}))"
   "gold.edn" "{:expected \"Owned document\" :private \"PRIVATE-CHECKER-GOLD\"}"
   "fixtures/docs/input.md" "Owned document"})
(def limits {:budget-microdollars 100000 :max-dispatches 12 :max-model-steps 2 :timeout-ms 60000 :settlement "discard"})
(def cell-limits {:budget-microdollars 10000 :max-dispatches 2 :max-model-steps 2 :timeout-ms 10000 :settlement "discard"})

(defn- seed! [conn]
  (doseq [[path text] files] (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8")))
  (repo/stage-all! conn) (repo/commit! conn {:message "Private author checker and gold"}))
(defn- input [host w]
  (let [inspection (get-in (sources/details! host (ops/party-principal (:owner w)) (str (:room w)) {:name "echo"}) [:data :inspection])]
    {:request-id (random-uuid) :task "Exercise two candidate revisions with the admitted development benchmark."
     :model "codex-subscription-sol" :limits limits
     :benchmark {:name "echo" :expected-bundle (parse-uuid (:bundle inspection))
                 :expected-owned-source (parse-uuid (:revision inspection)) :limits cell-limits}}))
(defn- response [events]
  {:status 200 :body (java.io.ByteArrayInputStream.
                      (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8"))})
(defn- tool [code id model]
  (response [{:type "response.output_item.done"
              :item {:type "function_call" :call_id id :name "clojure_eval" :arguments (json/write-value-as-string {:code code})}}
             {:type "response.completed" :response {:id id :model model :usage {:input_tokens 1 :output_tokens 1}}}]))
(defn- final [id model]
  (response [{:type "response.output_text.delta" :delta "Done"}
             {:type "response.completed" :response {:id id :model model :usage {:input_tokens 1 :output_tokens 1}}}]))

(def parent-code
  (str "(do (babashka.fs/create-dirs \"/out\") (spit \"/out/author-answer.txt\" \"AUTHOR-PRIVATE-ANSWER\") "
       "(git/add \"/out/author-answer.txt\") (git/commit \"AUTHOR-PRIVATE-COMMIT\") "
       "(let [team (dvergr.agent/make-agent (dvergr.agent/roster) "
       "{:id :candidate :prompt \"First candidate\" :tools #{:clojure_eval} :sandbox #{:simmis/file-worker} "
       ":model-policy {:provider :codex-subscription :model \"codex-subscription-luna-6\"} "
       ":program {:kind :llm :max-model-steps 2 :auto-compact? false}}) "
       "opts {:resources {\"microUSD\" 10000M \"model-dispatch\" 2M}} "
       "first-result @(simmis.benchmarks/run! team opts) "
       "second-result @(simmis.benchmarks/run! (dvergr.agent/revise-agent team :candidate {:prompt \"Second candidate\"}) opts) "
       "entry1 (first (get-in first-result [:scorecard :scorecard/entries])) "
       "entry2 (first (get-in second-result [:scorecard :scorecard/entries])) "
       "captured (simmis.results/attempt-file (:attempt/id entry1) {:path \"/out/result.md\"})] "
       "(assert (= [1.0 0.0] [(:reward entry1) (:reward entry2)])) "
       "(assert (= \"T3duZWQgZG9jdW1lbnQ=\" (:base64 captured))) "
       "(spit \"/out/benchmark-review.edn\" (pr-str {:rewards [(:reward entry1) (:reward entry2)] "
       ":scorecards [(get-in first-result [:scorecard :scorecard/content-id]) (get-in second-result [:scorecard :scorecard/content-id])] "
       ":captured captured})) :done))"))

(def delegated-code
  (str "(let [team (dvergr.agent/make-agent (dvergr.agent/roster) "
       "{:id :candidate :prompt \"Delegated second revision\" :tools #{:clojure_eval} :sandbox #{:simmis/file-worker} "
       ":model-policy {:provider :codex-subscription :model \"codex-subscription-luna-6\"} "
       ":program {:kind :llm :max-model-steps 2 :auto-compact? false}}) "
       "r @(simmis.benchmarks/run! team {:resources {\"microUSD\" 10000M \"model-dispatch\" 2M}}) "
       "entry (first (get-in r [:scorecard :scorecard/entries]))] "
       "(assert (= 0.0 (:reward entry))) (babashka.fs/create-dirs \"/out\") "
       "(spit \"/out/child-reward.txt\" (str (:reward entry))) "
       "(spit \"/out/child-scorecard.edn\" (pr-str (:scorecard r))) :done)"))

(def nested-parent-code
  (str/replace
   parent-code
   "second-result @(simmis.benchmarks/run! (dvergr.agent/revise-agent team :candidate {:prompt \"Second candidate\"}) opts) "
   (str "second-result (let [child (dvergr.agent/make-agent (dvergr.agent/roster) "
        "{:id :delegate :tools #{:clojure_eval} :sandbox #{:simmis/coordinator} "
        ":model-policy {:provider :codex-subscription :model \"codex-subscription-sol-6.1\"} "
        ":program {:kind :llm :max-model-steps 2 :auto-compact? false}}) "
        "e (dvergr.agent/environment {:id :test/delegate :task \"Run the second candidate against the inherited offer.\" "
        ":verifier {:id :simmis/work-completion :version 1 :basis \"simmis-work-completion-v1\"} "
        ":limits {:timeout-ms 20000 :max-model-steps 2} "
        ":world {:isolation :ctx :settlement :discard :resources {\"microUSD\" 20000M \"model-dispatch\" 4M} "
        ":effects {:admit #{:read :write :spend :network :lifecycle} :speculative true :allow-hosts #{}}}}) "
        "ex (dvergr.agent/experiment {:id :test/delegate "
        ":dataset (dvergr.agent/dataset {:id :test/delegate :environments [e]}) "
        ":candidates [(dvergr.agent/lookup child :delegate)]}) "
        "r @(dvergr.agent/run-experiment! child ex {:resources {\"microUSD\" 20000M \"model-dispatch\" 4M}}) "
        "id (:attempt/id (first (get-in r [:scorecard :scorecard/entries]))) "
        "chunk (simmis.results/attempt-file id {:path \"/out/child-reward.txt\"})] "
        "(assert (= \"MC4w\" (:base64 chunk))) "
        "{:scorecard {:scorecard/entries [{:reward 0.0}] :scorecard/content-id (get-in r [:scorecard :scorecard/content-id])}}) ")))

(defn- candidate-code [text]
  (str "(do (require '[babashka.process :as p] '[clojure.string :as str]) "
       "(assert (not (babashka.fs/exists? \"/workflows/echo/gold.edn\"))) "
       "(assert (not (babashka.fs/exists? \"/out/author-answer.txt\"))) "
       "(assert (not (str/includes? (pr-str (git/log {:n 100})) \"AUTHOR-PRIVATE-COMMIT\"))) "
       "(let [old (try (p/shell \"git show HEAD:workflows/echo/gold.edn\") "
       "(catch Exception _ :denied))] (assert (= :denied old))) "
       "(assert (= \"Owned document\" (slurp \"/docs/input.md\"))) "
       "(babashka.fs/create-dirs \"/out\") (spit \"/out/result.md\" " (pr-str text) "))"))

(deftest fixed-candidates-and-options-refuse-authority-escalation-before-start
  (let [authority {:delegation-policy sandbox/delegation-policy}
        candidate {:id :candidate :program {:kind :echo} :sandbox #{:simmis/file-worker}}
        team #(roster/make-agent (roster/make-roster) %)]
    (is (= #{:simmis/file-worker} (:agent/sandbox (first (roster/agents (benchmark/candidate-team (team candidate) authority))))))
    (doseq [bad [(assoc candidate :sandbox #{:simmis/coordinator})
                 (assoc candidate :program {:kind :sci :source "(ns p) (defn main [] :escape)" :entrypoint 'p/main})
                 (assoc candidate :tools #{:host_shell})
                 (assoc candidate :program {:kind :llm} :model-policy {:provider :claude-code :model "claude-code-haiku"})]]
      (is (thrown? Exception (benchmark/candidate-team (team bad) authority))))
    (doseq [key [:authority :world-parent :verifier :setup :dataset :cases :environment]]
      (is (thrown? Exception (benchmark/invoke! {} nil (fn [] (throw (AssertionError. "No fixture read")))
                                                (fn [] (throw (AssertionError. "No authority read")))
                                                {} {} {:resources {} key :forged}))))))

(defn- exercise-recursive-proof [nested?]
  (owned-fixture/with-fixture
    false
    (fn [{:keys [conn w host room owned attached]}]
      (d/transact conn [{:party/id (:owner w) :party/role :admin}])
      (seed! owned)
      (let [p (ops/party-principal (:owner w)) request (input host w)
            clean (discourse/make-room {:id (keyword (str "coordinator-clean-" (random-uuid))) :store (memory/make)})
            clean-conn (atom nil) slot (Semaphore. 1) retired (atom 0) closed? (atom false)
            parent-calls (atom 0) delegated-calls (atom 0) child-calls (atom 0) completions (atom {}) start jobs/start!
            owned-before (query/worktree @owned) attached-before (query/worktree @attached)
            provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})
            retire! (fn [] (when (compare-and-set! closed? false true)
                             (let [cfg (:config @@clean-conn)]
                               (discourse/close-room! clean) (d/release @clean-conn)
                               (when (and (= :memory (get-in cfg [:store :backend])) (uuid? (get-in cfg [:store :id])))
                                 (d/delete-database cfg)))
                             (swap! retired inc)))]
        (binding [ec/*execution-context* (:ctx clean)]
          (catalog/ensure-workspace! clean)
          (reset! clean-conn (:conn (workspace/current-workspace))))
        (resource/mint! room {:id (random-uuid) :resources (work/resources limits)})
        (with-redefs-fn
          {#'benchmarks/checker-slot slot #'benchmarks/pending-retirements (atom {})
           #'fixture/validate-receipt! (fn [_ _ value] value)
           #'fixture/create! (fn [& _] {:room clean :persistent-id (random-uuid)})
           #'fixture/retire! (fn [& _] (retire!))
           #'providers/providers (atom {:codex-subscription provider})
           #'gateway/*request-fn*
           (fn [http]
             (let [body (json/read-value (:body http) json/keyword-keys-object-mapper) model (:model body)]
               (if (= "gpt-5.6-sol" model)
                 (if (= 1 (swap! parent-calls inc)) (tool (if nested? nested-parent-code parent-code) "parent-benchmark-loop" model) (final "parent-done" model))
                 (if (= "gpt-6.1-sol" model)
                   (if (= 1 (swap! delegated-calls inc)) (tool delegated-code "delegated-benchmark" model)
                       (final "delegated-done" model))
                   (let [n (swap! child-calls inc)]
                     (is (= "gpt-6-luna" model))
                     (is (not (re-find #"PRIVATE-CHECKER-GOLD|AUTHOR-PRIVATE-ANSWER|AUTHOR-PRIVATE-COMMIT" (pr-str body))))
                     (case n
                       1 (tool (candidate-code "Owned document") "correct-output" model)
                       2 (final "correct-done" model)
                       3 (tool (candidate-code "Wrong document") "damaged-output" model)
                       4 (final "damaged-done" model)
                       (throw (ex-info "Unexpected additional dispatch" {:n n}))))))))
           #'jobs/start! (fn [actual-room opts f & callbacks]
                           (apply start actual-room opts
                                  (fn [id] (swap! completions assoc id (get-in @(var-get #'jobs/live) [id :done])) (f id)) callbacks))}
          (fn []
            (try
              (let [result (coordinator/submit-owned! host p (str (:room w)) request)
                    done (get @completions (:job result))]
                (is (true? (deref done 120000 false)) "Outer Job physically joins all descendants")
                (let [status (jobs/status (:job result) 0)
                      attempt (first (filter #(= :simmis/coordinated-work (get-in % [:attempt/environment :environment/id]))
                                             (store/-list-attempts (:store room) (store/conversation-id room) {})))
                      report (some-> (get-in attempt [:attempt/evidence :files "/out/benchmark-review.edn"]) edn/read-string)]
                  (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
                  (is (= [1.0 0.0] (:rewards report))
                      (pr-str {:result-keys (keys (:result status))
                               :attempts (mapv #(select-keys % [:attempt/id :attempt/environment :attempt/evidence :attempt/receipt])
                                               (store/-list-attempts (:store room) (store/conversation-id room) {}))}))
                  (is (= 2 @parent-calls)) (is (= 4 @child-calls))
                  (is (= (if nested? 2 0) @delegated-calls))
                  (is (= 1 @retired)) (is (= 1 (.availablePermits slot)))
                  (is (= owned-before (query/worktree @owned)))
                  (is (= attached-before (query/worktree @attached)))
                  (is (= {resource/microdollars 100000M resource/model-dispatches (if nested? 4M 6M)} (resource/balance room)))
                  (with-redefs [benchmark/compile! (fn [& _] (throw (AssertionError. "Retry cannot read new source")))
                                work/prepare (fn [& _] (throw (AssertionError. "Retry cannot resolve a model")))]
                    (is (= (:job result) (:job (coordinator/submit-owned! host p (str (:room w)) request)))))))
              (finally
               ;; Unknown physical work retains its native owner and fixture.
               ;; Never force-close the test workspace merely to finish teardown.
                (when (every? realized? (vals @completions)) (retire!))))))))))

(deftest offer-gate-and-aggregate-limit-cannot-be-multiplied
  (let [scorer work/completion
        setup (evaluation/make-world-setup {:id :test/fixed :basis :fixture :prepare (fn [_] {})})
        env (environment/make-environment {:id :test/fixed :task :work
                                           :verifier {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"}
                                           :world {:isolation :ctx :settlement :discard}})
        definition (experiment/make-experiment
                    {:id :test/fixed :dataset (experiment/make-dataset {:id :test/fixed :environments [env]})
                     :candidates [{:agent/id :candidate :agent/version 1}]})
        slot (Semaphore. 1)
        offer {:basis {:limits cell-limits} :prepared {:plan {:experiment definition}}
               :evaluator scorer :setup setup :invocation-slot slot}
        authority {:delegation-policy sandbox/delegation-policy}
        current {:control-room :control :work-room :caller :authority authority}
        team (roster/make-agent (roster/make-roster) {:id :candidate :program {:kind :echo} :sandbox #{:simmis/file-worker}})
        opts {:resources {"microUSD" 10000M "model-dispatch" 2M}}
        hooks (atom []) allocations (atom 0)]
    (with-redefs [subexperiment/start! (fn [_ _ _ _ _ _ host]
                                         (swap! hooks conj host)
                                         ((:admit! host) (random-uuid))
                                         (swap! allocations inc)
                                         :owned-native-result)]
      (doseq [bad [(assoc-in opts [:resources "microUSD"] 10001M)
                   (assoc-in opts [:resources "ungranted"] 1M)
                   (assoc opts :parallelism 2)]]
        (is (thrown? Exception (benchmark/invoke! offer :control (constantly :fixture) (fn []) current team bad))))
      (is (thrown? Exception
                   (benchmark/invoke! offer :control (constantly :fixture) (fn []) current
                                      (roster/make-agent team {:id :second :program {:kind :echo} :sandbox #{:simmis/file-worker}}) opts)))
      (is (zero? @allocations))
      (is (= :owned-native-result (benchmark/invoke! offer :control (constantly :fixture) (fn []) current team opts)))
      (is (= 0 (.availablePermits slot)))
      (is (thrown? Exception (benchmark/invoke! offer :control (constantly :fixture) (fn []) current team opts)))
      (is (= 1 @allocations))
      ;; A losing Job still settles; it must not release the winner's permit.
      ((:settled! (second @hooks)) :failed nil)
      (is (= 0 (.availablePermits slot)))
      ((:settled! (first @hooks)) :completed nil)
      ((:settled! (first @hooks)) :completed nil)
      (is (= 1 (.availablePermits slot))))))

(deftest stale-source-and-revoked-operator-refuse-before-claim-or-provider-work
  (owned-fixture/with-fixture
    false
    (fn [{:keys [conn w host room owned]}]
      (d/transact conn [{:party/id (:owner w) :party/role :admin}])
      (seed! owned)
      (let [p (ops/party-principal (:owner w)) request (input host w)
            forbidden (fn [& _] (throw (AssertionError. "No provider preparation before source/authority admission")))]
        (with-redefs [work/prepare forbidden]
          (is (thrown? Exception (coordinator/submit-owned! host p (str (:room w))
                                                            (assoc-in request [:benchmark :expected-owned-source] (random-uuid)))))
          (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]])
          (with-redefs [workflows/snapshot-in (fn [& _] (throw (AssertionError. "No source body before operator authority")))]
            (is (thrown? Exception (coordinator/submit-owned! host p (str (:room w)) request)))))
        (is (empty? (requests/requests conn)))
        (d/transact conn [{:party/id (:owner w) :party/role :admin}])
        (let [[_ base] (#'work-fixture/fixture)]
          (with-redefs [work/prepare (fn [_]
                                       (is (empty? (get @(:meta room) preparation/leases-key))
                                           "Provider preparation must not hold a source-operation lease")
                                       (repo/write! owned "workflows/echo/fixtures/docs/input.md" (.getBytes "New source" "UTF-8"))
                                       base)]
            (is (thrown? Exception (coordinator/submit-owned! host p (str (:room w)) request)))))
        (is (empty? (requests/requests conn)))))))

(deftest coordinator-invokes-two-fixed-benchmarks-with-clean-history-and-conserved-funding
  (exercise-recursive-proof false))

(deftest nested-coordinator-inherits-the-offer-and-keeps-private-history-out-of-candidates
  (exercise-recursive-proof true))
