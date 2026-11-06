(ns is.simm.ops.coordinator-review
  "Opt-in development review over existing native child experiments."
  (:require [clojure.string :as str]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.invocations :as invocations]
            [dvergr.agent.public-feedback :as feedback]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.run :as run]
            [dvergr.agent.subexperiment :as subexperiment]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.jobs :as jobs]
            [hasch.core :as hasch]
            [is.simm.ops.benchmark-source :as retained]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent Semaphore]
           [java.util.concurrent.atomic AtomicBoolean]))

(def invocation-id :simmis/admitted-benchmark-review)
(def policy {:version 1 :data-use :exposed-development :subjects :direct-settled-benchmark-child
             :max-packet-bytes 524288 :max-output-files 16 :max-output-bytes 262144
             :grading :structural-only :promotion :human-only})
(def schema
  [:map {:closed true} [:version [:= 1]] [:data-use [:= "exposed-development"]]
   [:name workflows/name-schema] [:expected-bundle :uuid] [:expected-owned-source :uuid]
   [:model [:string {:min 1 :max 200}]]
   [:limits work/limits-schema]])
(def options-schema
  [:map {:closed true}
   [:subject [:map {:closed true} [:job :uuid] [:scorecard :uuid] [:attempt :uuid]]]
   [:resources [:map-of {:min 1} [:or :keyword :string] [:and 'number? 'pos?]]]
   [:parallelism {:optional true} [:= 1]]
   [:result-mode {:optional true} [:= :receipt]]])

(def worker-child-options
  {:experiment-capabilities {:evaluators {} :world-setups {} :invocations {}}
   :delegation-policy {:tools #{:clojure_eval :read_file :write_file :edit_file}
                       :sandboxes #{#{:simmis/file-worker}} :providers work/dispatch-accounted-providers
                       :program-kinds #{:echo :scripted :llm}
                       :max-attempts 1 :max-parallelism 1}})

(defn- refuse! [reason]
  (throw (ex-info (case reason
                   :packet-too-large "Development review packet exceeds its 512 KiB bound; select a smaller source-backed case."
                   :subject-job "Review requires this coordinator's exact settled development benchmark Job."
                   :subject-card "Review subject does not match that Job's certified Scorecard cell."
                   :review-source-changed "Reviewer source changed; inspect it before a new admission."
                   :review-calibration-failed "Reviewer calibration failed before candidate execution."
                   :physical-slot-busy "Another benchmark or review is still physically active."
                   "Development review is not admitted")
                  {:type (case reason :review-source-changed ::conflict :physical-slot-busy ::busy ::refused)
                   :reason reason})))

(defn packet-id [packet]
  (str (hasch/uuid [:simmis/development-review-packet-v1 (dissoc packet :packet-id)])))

(defn validate-expected! [environment packet]
  (when-not (and (map? packet) (= (:packet-id packet) (packet-id packet))
                 (= (:packet-id packet) (get-in environment [:environment/world :setup :setup/basis :packet-id])))
    (refuse! :packet-binding))
  packet)

(defn evaluator [source benchmark-ref]
  (let [bundle (retained/reconstruct source)]
    (evaluation/make-evaluator
     {:id :simmis/development-review-structure :version 1 :tier :ad-hoc
      :basis {:review-source (retained/source-id source) :benchmark-invocation (invocations/content-id benchmark-ref)
              :policy policy :checker-executor checkers/executor-policy}
      :capture (fn [{world :world/room}]
                 {:files (workspace/read-tree world "/out" {:strict? true :max-files (:max-output-files policy)
                                                            :max-bytes (:max-output-bytes policy)})})
      :observe (fn [{:keys [default result] captured :execution/evidence setup :setup/evidence}]
                 (assoc default :files (:files captured) :review-packet (:review-packet setup)
                        :review-run-status (:run/status result)))
      :verify (fn [environment {:keys [files review-packet review-run-status]}]
                (validate-expected! environment review-packet)
                (let [verdict (checkers/run-checker (:checker bundle) {:files files :gold review-packet})
                      completed? (= :completed review-run-status)]
                  (-> verdict
                      (update :checks assoc :completed? completed?)
                      (update :reward #(if completed? % 0.0)))))})))

(defn compile-source [binding admission]
  (when-not (and (m/validate schema admission) (= "discard" (get-in admission [:limits :settlement])))
    (refuse! :invalid-review-admission))
  (let [snapshot (workflows/snapshot-in (:db binding) (:name admission))
        revision (sources/source-id binding (:name admission) (:manifest snapshot))
        source (retained/retain (:name admission) (:files snapshot))
        bundle (retained/reconstruct source)
        definition (:definition bundle)]
    (when-not (and (= revision (:expected-owned-source admission))
                   (= (:id bundle) (:expected-bundle admission)))
      (refuse! :review-source-changed))
    (retained/validate-manifest! source (:manifest snapshot))
    (when-not (and (contains? #{nil "developer"} (:profile definition))
                   (empty? (:cases bundle))
                   (= #{"/inputs/review-packet.edn"} (set (keys (:fixtures bundle))))
                   (string? (:checker bundle)) (not (str/blank? (:checker bundle)))
                   (some? (:calibration bundle))
                   (<= (count (catalog/task bundle {})) 16000))
      (refuse! :review-template-shape))
    (checkers/validate-calibration! bundle)
    (let [prepared (work/prepare {:request-id (hasch/uuid [:simmis/review-candidate-v1 source admission])
                                 :task (catalog/task bundle {}) :model (:model admission) :limits (:limits admission)})
          team (reduce (fn [team agent]
                         (roster/revise-agent team (:agent/id agent)
                                              {:tools #{:clojure_eval :read_file :write_file :edit_file}}))
                       (:team prepared) (roster/agents (:team prepared)))]
      {:source source :origin {:revision revision :manifest (:manifest snapshot)}
       :limits (:limits admission) :policy policy :team team})))

(defn reference [review benchmark-ref]
  (let [verifier (evaluation/evaluator-ref (:evaluator review))
        declaration {:version 1 :verifier verifier
                     :checks {:completed? "Reviewer execution completed; not semantic correctness."
                              :review-structure? "Review output conforms to the admitted structure."
                              :review-case-binding? "Review identifies the exact expected packet and cases."
                              :review-citations? "Quotes are substrings of the named serialized fixture files."
                              :advisory-only? "Review declares no grading changes; proposals remain advisory."}}]
    (when-not (feedback/valid? declaration) (refuse! :invalid-review-feedback))
    {:invocation/id invocation-id :invocation/version 1
     :invocation/basis {:admission-id (hasch/uuid [(:source review) (:origin review) (:limits review) policy benchmark-ref])
                        :benchmark-invocation (invocations/content-id benchmark-ref)
                        :file-verifier verifier :data-use :exposed-development
                        :limits (:limits review)
                        :reviewer-model (get-in (first (roster/agents (:team review))) [:agent/model-policy :model])}
     :invocation/public-feedback declaration}))

(defn- current-parent! [room authority]
  (when-not (and (identical? room (:control-room authority)) (uuid? (:parent-run authority)))
    (refuse! :control-owner))
  (run/assert-unlocked-host-thread! room ::host-worker-required)
  (when (or ec/*spin-id* (.isInterrupted (Thread/currentThread)) (run/cancel-requested? (:parent-run authority)))
    (throw (InterruptedException. "Review invocation cancelled")))
  (when-not (= :running (:run/status (run/run room (:parent-run authority))))
    (refuse! :parent-not-running)))

(defn subject-packet
  "Read one exact direct settled child from the admitted benchmark. HOST only;
   source/checker/reference disclosure is explicit exposed-development policy."
  [offer room authority {:keys [job scorecard attempt]}]
  (binding [ec/*execution-context* (:ctx room)]
    (current-parent! room authority)
    (let [row (run/run room job)
          status (jobs/status-in room job)]
      (when-not (and (= (:parent-run authority) (:run/parent row)) (= :experiment (:run/kind row))
                     (= (invocations/content-id (:ref offer)) (:run/job-offer row))
                     (= :completed (:run/status row)) (true? (:settled? status))
                     (= :available (:run/job-result-state row)) (= scorecard (:run/job-scorecard row)))
        (refuse! :subject-job))
      (let [card (experiment/scorecard-bounded room scorecard)
            entries (filterv #(= attempt (:attempt/id %)) (:scorecard/entries card))]
        (when-not (and (= scorecard (:scorecard/content-id card))
                       (= (:run/job-experiment row) (get-in card [:scorecard/experiment :experiment/content-id]))
                       (= 1 (count entries)) (= job (:run/parent (run/run room attempt))))
          (refuse! :subject-card))
        (let [entry (first entries)
              certified (episode/attempt-bounded room attempt)
              definition (:attempt/environment certified)
              receipt (:attempt/receipt certified)
              source (get-in offer [:basis :source])
              bundle (retained/reconstruct source)
              case-id (get-in definition [:environment/metadata :case])
              ;; Reuse the native exact-cell identity laws; this is not an
              ;; implicit change to the benchmark's public feedback disclosure.
              _ (feedback/project {:version 1 :verifier (evaluation/evaluator-ref (:evaluator offer))
                                   :checks {:completed? "Identity check only."}}
                                  (:run/job-experiment row) entry certified)
              _ (when-not (and (= (str (:id bundle)) (get-in definition [:environment/metadata :bundle]))
                               (some #(= (environment/environment-ref definition) (environment/environment-ref %))
                                     (get-in offer [:prepared :plan :experiment :experiment/dataset :dataset/environments])))
                  (refuse! :subject-environment))
              fixtures (retained/case-files source (if case-id {:kind :named :name case-id} {:kind :default}))
              files (get-in certified [:attempt/evidence :files] {})
              _ (when-not (and (map? files) (<= (count files) 16) (every? string? (vals files)))
                  (refuse! :unsupported-captured-files))
              packet {:version 1 :authority :advisory-only :data-use :exposed-development
                      :quote-policy :literal-serialized-file-text
                      :cases [{:id (or case-id "default") :task-contract (:environment/task definition)
                               :sources (mapv (fn [i [path text]] {:id (str "input-" i) :path path :text text}) (range) fixtures)
                               :submitted-answer files :reference (catalog/gold-of bundle definition)
                               :scoring-contract (:checker bundle)
                               :scoring-params (get-in bundle [:definition :params])
                               :scoring-policy checkers/policy
                               :recorded-results {:status (:attempt/status receipt) :checks (:attempt/checks receipt)
                                                  :reward (:attempt/reward receipt)}
                               :provenance {:job job :scorecard scorecard :attempt attempt
                                            :attempt-content-id (:attempt/content-id certified)
                                            :environment-content-id (:environment/content-id definition)
                                            :source-bundle (:id bundle)}}]}
              packet (assoc packet :packet-id (packet-id packet))
              bytes (.getBytes (pr-str packet) "UTF-8")]
          (when (> (alength bytes) (:max-packet-bytes policy)) (refuse! :packet-too-large))
          (current-parent! room authority)
          packet)))))

(defn plan [review packet team resources]
  (let [template (retained/reconstruct (:source review))
        bundle (catalog/bundle (:name template) {"workflow.edn" (get-in review [:source :files "workflow.edn"])
                                                "fixtures/inputs/review-packet.edn" (pr-str packet)} :program)
        base (catalog/world-setup bundle)
        setup (evaluation/make-world-setup
               {:id :simmis/development-review-fixture :version 1
                :basis {:fixtures (evaluation/world-setup-ref base) :packet-id (:packet-id packet)
                        :review-source (retained/source-id (:source review))}
                :prepare (fn [context]
                           (let [calibration (checkers/calibrate template)]
                             (when-not (:ok? calibration) (refuse! :review-calibration-failed))
                             (run/record-cause! (:run/id context) (get-in packet [:cases 0 :provenance :attempt]))
                             ((:prepare base) context)
                             {:review-packet packet :calibration calibration}))})
        limits (:limits review)
        env (catalog/environment bundle setup (:evaluator review) {:timeout-ms (:timeout-ms limits)})
        verifier (:environment/verifier env)
        env (environment/make-environment
             {:id (:environment/id env) :task (:environment/task env)
              :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier) :basis (:verifier/basis verifier)}
              :world (assoc (:environment/world env) :resources resources :effects work/effects)
              :limits (merge (:environment/limits env) {:max-model-steps (:max-model-steps limits)
                                                       :budget-dollars (/ (get resources "microUSD") 1000000.0)})
              :metadata (:environment/metadata env)})
        definition (experiment/make-experiment
                    {:id :simmis/development-review
                     :dataset (experiment/make-dataset {:id :simmis/development-review :environments [env]})
                     :candidates (roster/agents team) :repetitions 1
                     :metadata {:data-use :exposed-development :review-packet (:packet-id packet)}})]
    {:definition definition :setup setup :evaluator (:evaluator review)}))

(defn invoke!
  [offer original-room fixture-parent authorize! candidate-team
   {:keys [control-room work-room authority]} team opts]
  (when-not (and (m/validate options-schema opts) (identical? original-room control-room)
                 (= team (roster/make-roster)))
    (refuse! :invalid-review-options))
  (let [review (:review offer)
        ceiling (work/resources (:limits review))
        resources (:resources opts)]
    (when-not (and review (= (set (keys ceiling)) (set (keys resources)))
                   (every? (fn [[unit amount]] (and (number? amount) (pos? amount)
                                                   (Double/isFinite (double amount)) (<= amount (get ceiling unit)))) resources))
      (refuse! :review-grant))
    (authorize!)
    (let [team (candidate-team (:team review) authority)
          packet (subject-packet offer control-room authority (:subject opts))
          _ (authorize!)
          _ (run/record-cause! (:parent-run authority) (get-in opts [:subject :attempt]))
          {:keys [definition setup evaluator]} (plan review packet team resources)
          slot (:invocation-slot offer)
          held (AtomicBoolean. false)]
      (when-not (instance? Semaphore slot) (refuse! :missing-physical-owner))
      (subexperiment/start!
       control-room work-room team definition {:resources resources :parallelism 1}
       (assoc authority :experiment-capabilities {:evaluators {(evaluation/evaluator-ref evaluator) evaluator}
                                                  :world-setups {(evaluation/world-setup-ref setup) setup}})
       {:observation-ref (:ref review) :return-receipt? true :world-parent (fixture-parent)
        :candidate-options worker-child-options
        :admit! (fn [_]
                  (authorize!)
                  (when-not (.tryAcquire ^Semaphore slot) (refuse! :physical-slot-busy))
                  (.set held true))
        :settled! (fn [_ _] (when (.compareAndSet held true false) (.release ^Semaphore slot)))}))))
