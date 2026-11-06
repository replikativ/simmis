(ns is.simm.ops.coordinator-review-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.invocations :as invocations]
            [dvergr.agent.public-feedback :as feedback]
            [dvergr.agent.run :as run]
            [dvergr.agent.subexperiment :as subexperiment]
            [dvergr.jobs :as jobs]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.benchmark-source :as retained]
            [is.simm.ops.coordinator-review :as review]
            [is.simm.starter.benchmark-review :as starter]))

(def benchmark-ref {:invocation/id :simmis/admitted-benchmark :invocation/version 1 :invocation/basis "source-a"})
(def resources {"microUSD" 10000M "model-dispatch" 4M})
(def limits {:budget-microdollars 10000 :max-dispatches 4 :max-model-steps 4 :timeout-ms 30000 :settlement "discard"})
(defn compiled []
  (let [source (retained/retain "review-template" (starter/source))]
    {:source source :limits limits :policy review/policy :evaluator (review/evaluator source benchmark-ref)}))
(defn packet [text]
  (let [p {:version 1 :authority :advisory-only :data-use :exposed-development
           :cases [{:id "case-001" :sources [{:id "input-0" :text text}]
                    :reference {:expected text} :submitted-answer {"/out/result.txt" text}}]}]
    (assoc p :packet-id (review/packet-id p))))
(defn team [] (roster/make-agent (roster/make-roster) {:id :reviewer :program {:kind :echo} :sandbox #{:simmis/file-worker}}))
(defn response [p]
  {:version 1 :packet-id (:packet-id p) :grading-changes-applied false
   :cases [{:id "case-001" :findings [{:kind :no-issue :confidence :medium :summary "Matches the source."
                                     :citations [{:source-id "input-0" :quote (get-in p [:cases 0 :sources 0 :text])}]}]
            :next-action :retain :proposed-change nil}]})
(defn env [plan] (get-in plan [:definition :experiment/dataset :dataset/environments 0]))
(defn verify [plan p output status]
  ((:verify (:evaluator plan)) (env plan)
   {:files {"/out/review.edn" (pr-str output)} :review-packet p :review-run-status status}))

(deftest one-stable-verifier-different-packet-bound-environments
  (let [r (compiled) p1 (packet "Monday") p2 (packet "Tuesday")
        a (review/plan r p1 (team) resources) b (review/plan r p2 (team) resources)]
    (is (= (evaluation/evaluator-ref (:evaluator a)) (evaluation/evaluator-ref (:evaluator b))))
    (is (not= (:environment/content-id (env a)) (:environment/content-id (env b))))
    (is (not= (evaluation/world-setup-ref (:setup a)) (evaluation/world-setup-ref (:setup b))))
    (is (not= (get-in a [:definition :experiment/content-id]) (get-in b [:definition :experiment/content-id])))
    (is (= 1.0 (:reward (verify a p1 (response p1) :completed))))
    (is (thrown? Exception (verify a p2 (response p2) :completed)))
    (is (thrown? Exception (verify a (assoc-in p1 [:cases 0 :reference :expected] "changed") (response p1) :completed)))))

(deftest verifier-binds-source-scope-not-only-review-template
  (let [source (retained/retain "review-template" (starter/source))]
    (is (not= (evaluation/evaluator-ref (review/evaluator source benchmark-ref))
              (evaluation/evaluator-ref (review/evaluator source (assoc benchmark-ref :invocation/basis "source-b")))))))

(deftest failed-execution-keeps-structural-facts-with-zero-reward
  (let [r (compiled) p (packet "Monday") plan (review/plan r p (team) resources)
        result (verify plan p (response p) :failed)]
    (is (false? (get-in result [:checks :completed?])))
    (is (every? true? (vals (dissoc (:checks result) :completed?))))
    (is (zero? (:reward result)))
    (is (false? (get-in (verify plan p (assoc (response p) :packet-id "foreign") :completed)
                       [:checks :review-case-binding?])))))

(deftest trusted-setup-expectation-wins-over-candidate-written-data
  (let [r (compiled) p (packet "Monday") ev (:evaluator r)
        observed ((:observe ev) {:default {:review-packet :forged}
                                 :result {:run/status :completed}
                                 :execution/evidence {:files {"/out/review.edn" (pr-str (response p))
                                                              "/out/expected.edn" "forged"}}
                                 :setup/evidence {:review-packet p}})]
    (is (= p (:review-packet observed)))
    (is (nil? (:review-packet ((:observe ev) {:default {:review-packet :forged}}))))))

(deftest review-read-and-authority-overrides-are-not-invocation-options
  (let [started (atom 0)]
    (with-redefs [subexperiment/start! (fn [& _] (swap! started inc))]
      (let [subject {:job (random-uuid) :scorecard (random-uuid) :attempt (random-uuid)}]
        (doseq [opts [(assoc {:subject subject :resources resources} :task "replace")
                      (assoc {:subject subject :resources resources} :world-parent :foreign)
                      (assoc {:subject subject :resources resources} :setup :foreign)
                      {:review-context subject :resources resources}]]
          (is (= :invalid-review-options
                 (try (review/invoke! {} nil (constantly nil) (constantly nil) (fn [& _] nil)
                                      {} (roster/make-roster) opts)
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))
      (is (zero? @started)))))

(deftest malformed-or-unfunded-review-grants-refuse-before-reading-subject
  (let [r (compiled) parent {:authority {} :control-room nil}
        subject {:job (random-uuid) :scorecard (random-uuid) :attempt (random-uuid)}]
    (doseq [bad [{"microUSD" 10000M} {"model-dispatch" 4M} (assoc resources "other" 1)
                 (assoc resources "microUSD" 10001M) (assoc resources "microUSD" ##Inf)]]
      (is (= :review-grant
             (try (review/invoke! {:review r} nil (constantly nil)
                                 #(throw (AssertionError. "No authorization read expected"))
                                 (fn [& _] nil) parent (roster/make-roster) {:subject subject :resources bad})
                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

(deftest reviewer-child-has-no-inherited-review-or-benchmark-capability
  (is (empty? (get-in review/worker-child-options [:experiment-capabilities :invocations])))
  (is (= #{#{:simmis/file-worker}} (get-in review/worker-child-options [:delegation-policy :sandboxes])))
  (is (not (contains? (get-in review/worker-child-options [:delegation-policy :program-kinds]) :sci))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest source-admission-pins-reviewer-and-requires-inspected-calibrated-source
  (let [files (starter/source) revision (random-uuid)
        bundle (retained/reconstruct (retained/retain "review-template" files))
        declaration {:version 1 :data-use "exposed-development" :name "review-template"
                     :expected-bundle (:id bundle) :expected-owned-source revision
                     :model "pinned-model" :limits limits}
        prepared (atom [])]
    (with-redefs [workflows/snapshot-in (fn [& _] {:files files :manifest :manifest})
                  sources/source-id (fn [& _] revision)
                  retained/validate-manifest! (fn [& _])
                  work/prepare (fn [input] (swap! prepared conj input) {:team (team)})]
      (let [r (review/compile-source {} declaration)]
        (is (= "pinned-model" (:model (first @prepared))))
        (is (= limits (:limits r)))
        (is (= #{:clojure_eval :read_file :write_file :edit_file}
               (:agent/tools (first (roster/agents (:team r)))))))
      (reset! prepared [])
      (doseq [[input expected] [[(assoc declaration :expected-owned-source (random-uuid)) :review-source-changed]
                                [(assoc declaration :expected-bundle (random-uuid)) :review-source-changed]
                                [(assoc declaration :data-use "held-out") :invalid-review-admission]
                                [(assoc-in declaration [:limits :settlement] "merge") :invalid-review-admission]]]
        (is (= expected (reason #(review/compile-source {} input)))))
      (is (empty? @prepared) "Invalid admissions never prepare a provider")
      (let [uncalibrated (dissoc files "calibration.edn")
            id (:id (retained/reconstruct (retained/retain "review-template" uncalibrated)))]
        (with-redefs [workflows/snapshot-in (fn [& _] {:files uncalibrated :manifest :manifest})]
          (is (= :review-template-shape
                 (reason #(review/compile-source {} (assoc declaration :expected-bundle id)))))))
      (is (empty? @prepared)))))

(deftest exact-parent-job-cell-and-retained-source-required-for-packet
  ;; Native cell certification has its own identity tests. This matrix isolates
  ;; the Simmis scope gates around that reused validator.
  (let [r (compiled) source (:source r) bundle (retained/reconstruct source)
        parent (random-uuid) job (random-uuid) card-id (random-uuid) attempt (random-uuid)
        experiment-id (random-uuid) room {:ctx nil}
        definition (environment/make-environment {:id :test/source :task "Review this source."
                                                  :verifier {:id :test/check :version 1}
                                                  :metadata {:bundle (str (:id bundle))}})
        base-row {:run/parent parent :run/kind :experiment :run/job-offer (invocations/content-id benchmark-ref)
                  :run/status :completed :run/job-result-state :available :run/job-scorecard card-id
                  :run/job-experiment experiment-id}
        base-card {:scorecard/content-id card-id :scorecard/experiment {:experiment/content-id experiment-id}
                   :scorecard/entries [{:attempt/id attempt}]}
        base-certified {:attempt/environment definition :attempt/evidence {:files {"/out/result.txt" "answer"}}
                        :attempt/receipt {:attempt/status :completed :attempt/reward 0.0}}
        row (atom base-row) status (atom {:settled? true}) card (atom base-card)
        certified (atom base-certified) validator-calls (atom 0)
        offer {:ref benchmark-ref :basis {:source source} :evaluator (:evaluator r)
               :prepared {:plan {:experiment {:experiment/dataset {:dataset/environments [definition]}}}}}
        authority {:control-room room :parent-run parent}
        subject {:job job :scorecard card-id :attempt attempt}
        read-packet #(review/subject-packet offer room authority subject)]
    (with-redefs [run/assert-unlocked-host-thread! (fn [& _])
                  run/cancel-requested? (constantly false)
                  run/run (fn [_ id] (cond (= id parent) {:run/status :running}
                                           (= id job) @row (= id attempt) {:run/parent job}))
                  jobs/status-in (fn [& _] @status)
                  experiment/scorecard-bounded (fn [& _] @card)
                  episode/attempt-bounded (fn [& _] @certified)
                  feedback/project (fn [& _] (swap! validator-calls inc))]
      (is (= {"/out/result.txt" "answer"} (get-in (read-packet) [:cases 0 :submitted-answer])))
      (is (= 1 @validator-calls))
      (doseq [changed [(assoc base-row :run/parent (random-uuid))
                       (assoc base-row :run/kind :ordinary)
                       (assoc base-row :run/job-offer (random-uuid))
                       (assoc base-row :run/status :running)
                       (assoc base-row :run/job-result-state :unavailable)
                       (assoc base-row :run/job-scorecard (random-uuid))]]
        (reset! row changed)
        (is (= :subject-job (reason read-packet))))
      (reset! row base-row)
      (reset! status {:settled? false})
      (is (= :subject-job (reason read-packet)))
      (reset! status {:settled? true})
      (doseq [changed [(assoc base-card :scorecard/content-id (random-uuid))
                       (assoc-in base-card [:scorecard/experiment :experiment/content-id] (random-uuid))
                       (assoc base-card :scorecard/entries [])
                       (update base-card :scorecard/entries into (:scorecard/entries base-card))]]
        (reset! card changed)
        (is (= :subject-card (reason read-packet))))
      (reset! card base-card)
      (reset! certified (assoc-in base-certified [:attempt/environment :environment/metadata :bundle] "foreign"))
      (is (= :subject-environment (reason read-packet)))
      (reset! certified (assoc-in base-certified [:attempt/evidence :files "/out/binary"] (byte-array 1)))
      (is (= :unsupported-captured-files (reason read-packet)))
      (reset! certified base-certified)
      (with-redefs [retained/case-files (fn [& _] {"/oversize" (apply str (repeat 262145 "é"))})]
        (is (= :packet-too-large (reason read-packet)) "UTF-8 byte cap, not character count"))
      (is (= :control-owner
             (reason #(review/subject-packet offer (assoc room :incarnation :replacement) authority subject))))
      (with-redefs [run/run (fn [& _] {:run/status :completed})]
        (is (= :parent-not-running (reason read-packet))))
      (with-redefs [run/cancel-requested? (constantly true)]
        (is (thrown? InterruptedException (read-packet)))))))

(deftest calibration-refusal-precedes-review-candidate-execution
  (let [r (compiled) p (packet "Monday") plan (review/plan r p (team) resources)]
    (with-redefs [is.simm.ops.checkers/calibrate (constantly {:ok? false})]
      (is (= :review-calibration-failed
             (reason #((:prepare (:setup plan)) {})))))))

(deftest review-feedback-does-not-duplicate-near-limit-primary-disclosure
  (let [primary (assoc benchmark-ref :invocation/public-feedback
                      {:version 1 :verifier {:verifier/id :test/check :verifier/version 1} :checks (into {} (map #(vector (keyword (str "check-" %))
                                                                 (apply str (repeat 400 "x"))) (range 8)))})
        r (compiled) reference (review/reference r primary)]
    (is (feedback/valid? (:invocation/public-feedback reference)))
    (is (< (count (pr-str reference)) 4096))
    (is (= limits (get-in reference [:invocation/basis :limits])))))


(deftest operator-pinned-reviewer-cannot-be-replaced-by-caller-team
  (is (= :invalid-review-options
         (reason #(review/invoke! {:review (compiled)} nil (constantly nil)
                                  (fn [] (throw (AssertionError. "No admission before team gate")))
                                  (fn [& _]) {:control-room nil} (team)
                                  {:subject {:job (random-uuid) :scorecard (random-uuid) :attempt (random-uuid)}
                                   :resources resources})))))
