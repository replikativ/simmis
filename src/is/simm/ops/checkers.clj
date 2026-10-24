(ns is.simm.ops.checkers
  "Trusted-operator checker execution inside Dvergr's owned scoring worker.
   No detached future: physical cleanup remains pending until this returns.
   SCI deadlines/allocation checks are cooperative, NOT hard OS isolation."
  (:require [clojure.string :as str]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.rooms.files :as room-files]
            [dvergr.sandbox :as sandbox]
            [is.simm.ops.knowledge-workspace :as knowledge-workspace]
            [is.simm.ops.wiki-scoring :as wiki-scoring]
            [sci.core :as sci])
  (:import [java.util.concurrent.atomic AtomicBoolean]))

(def policy
  {:version 1 :timeout-ms 20000 :max-allocated-bytes 134217728
   :max-checks 100 :max-check-key-length 100 :capture "/out" :max-files 100 :max-characters 2000000})

(defn- refuse! [message]
  (throw (ex-info message {:type ::invalid})))

(defn run-checker
  "Execute and fully validate one checker ON THE CALLING scoring worker.
   Caller must have explicitly admitted this exact source as trusted operator
   code. Never invoke this on Spindel's event-drain thread. Options are a
   trusted host test seam, not externally supplied checker permissions."
  ([source input] (run-checker source input policy))
  ([source input {:keys [timeout-ms max-allocated-bytes max-checks max-check-key-length deadline-nanos allocation-check prepare-input]}]
   (let [deadline (or deadline-nanos (+ (System/nanoTime) (* 1000000 (long timeout-ms))))
         stopped (AtomicBoolean. false)
         allocation-check (or allocation-check (sandbox/make-resource-limits :max-bytes max-allocated-bytes))
         guard! (fn []
                  (when (or (.get stopped)
                            (.isInterrupted (Thread/currentThread))
                            (<= deadline (System/nanoTime)))
                    (.set stopped true)
                    (refuse! "Checker deadline or interruption reached"))
                  (try (allocation-check)
                       (catch Exception e
                         (.set stopped true)
                         (throw e))))]
     (guard!)
     ;; Both context creation and its allocation-accounting origin belong to
     ;; THIS worker, unlike a context created outside a detached future.
     (let [input (if prepare-input (prepare-input guard! input) input)
           _ (when prepare-input (guard!))
           ctx (sandbox/create-base-ctx :load-fn (constantly nil) :resource-limits guard!)
           {checker-ns :ns} (sci/eval-string+ ctx source)
           _ (guard!)
           checker (sci/eval-string* ctx (str "(ns-resolve '" checker-ns " 'check)"))
           _ (when-not checker (refuse! "Checker defines no check function"))
           verdict (@checker input)
           _ (guard!)
           checks (:checks verdict)
           reward (:reward verdict)]
       ;; Check shape/count BEFORE traversal. Do not realize arbitrary lazy
       ;; values or print an invalid result into an unbounded error message.
       (when-not (and (map? verdict) (map? checks)
                      (<= 1 (count checks) max-checks)
                      (every? (fn [[k v]] (and (keyword? k) (<= (count (str k)) max-check-key-length) (boolean? v))) checks)
                      (number? reward) (Double/isFinite (double reward))
                      (<= 0.0 (double reward) 1.0))
         (refuse! "Checker must return bounded boolean checks and a finite reward in [0,1]"))
       (guard!)
       {:checks (into {} checks) :reward (double reward)}))))

(def calibration-policy
  {:version 2 :budget-scaling :per-answer
   :max-damaged 4 :max-files 100 :max-bytes 2097152 :timeout-ms 20000})

(def structural-calibration-policy
  (assoc calibration-policy :version 3 :max-references 4 :max-damaged 8))

(defn- structural-calibration? [bundle]
  (= 2 (get-in bundle [:calibration :version])))

(defn- validate-structural-calibration! [{:keys [calibration cases] :as bundle}]
  (when-not (wiki-scoring/selected-profile bundle)
    (refuse! "Multi-reference calibration requires a structural checker profile"))
  (let [{:keys [references damaged]} calibration
        p structural-calibration-policy
        named? (fn [k] (and (or (keyword? k) (string? k)) (<= (count (str k)) 100)))
        answer? (fn [answer damaged?]
                  (and (map? answer)
                       (= (set (keys answer)) (if damaged? #{:case :files :loses} #{:case :files}))
                       (if (seq cases) (contains? cases (:case answer)) (nil? (:case answer)))
                       (map? (:files answer)) (<= (count (:files answer)) (:max-files p))
                       (every? (fn [[path text]]
                                 (and (string? path) (<= (count path) 512) (str/starts-with? path "/out/")
                                      (try (= path (room-files/workspace-path path)) (catch Exception _ false))
                                      (string? text))) (:files answer))
                       (or (not damaged?)
                           (let [lost (:loses answer)]
                             (and (vector? lost) (<= 1 (count lost) (:max-checks policy))
                                  (= (count lost) (count (set lost)))
                                  (every? #(and (keyword? %) (<= (count (str %)) (:max-check-key-length policy))) lost))))))]
    (when-not (and (map? calibration) (= #{:version :references :damaged} (set (keys calibration)))
                   (map? references) (<= 1 (count references) (:max-references p))
                   (map? damaged) (<= 1 (count damaged) (:max-damaged p))
                   (every? (fn [[k answer]] (and (named? k) (answer? answer false))) references)
                   (every? (fn [[k answer]] (and (named? k) (answer? answer true))) damaged))
      (refuse! "Invalid bounded multi-reference calibration"))
    (let [answers (concat (vals references) (vals damaged))]
      (when (or (> (reduce + 0 (map #(count (:files %)) answers)) (:max-files p))
                (> (reduce + 0 (for [answer answers text (vals (:files answer))]
                                 (alength (.getBytes ^String text "UTF-8")))) (:max-bytes p)))
        (refuse! "Calibration answers exceed the combined file or byte bound"))))
  calibration)

(defn- validate-legacy-calibration!
  "Pure shape/bounds validation; never executes source. No fixture HTTP/judge
   responses here: this endpoint admits only offline /out answers."
  [{:keys [calibration cases]}]
  (when (some? calibration)
    (let [{:keys [reference damaged] case-id :case} calibration
          files? (fn [files]
                   (and (map? files) (<= (count files) (:max-files calibration-policy))
                        (every? (fn [[path text]]
                                  (and (string? path) (<= (count path) 512)
                                       (str/starts-with? path "/out/")
                                       (try (= path (room-files/workspace-path path))
                                            (catch Exception _ false))
                                       (string? text))) files)))]
      (when-not (and (map? calibration)
                     (every? #{:reference :damaged :case} (keys calibration))
                     (if (seq cases) (contains? cases case-id) (nil? case-id))
                     (files? reference) (map? damaged)
                     (<= 1 (count damaged) (:max-damaged calibration-policy))
                     (every? (fn [[k variant]]
                               (and (or (keyword? k) (string? k)) (<= (count (str k)) 100)
                                    (map? variant) (every? #{:files :loses} (keys variant))
                                    (files? (:files variant))
                                    (vector? (:loses variant))
                                    (<= 1 (count (:loses variant)) (:max-checks policy))
                                    (every? #(and (keyword? %) (<= (count (str %)) (:max-check-key-length policy))) (:loses variant))
                                    (= (count (:loses variant)) (count (set (:loses variant)))))) damaged))
        (refuse! "Invalid offline calibration: reference, 1..4 damaged /out answers and explicit lost checks required"))
      (when (> (reduce + 0 (map count (cons reference (map :files (vals damaged)))))
               (:max-files calibration-policy))
        (refuse! "Calibration answers exceed the total file limit"))
      (when (> (reduce + 0 (for [files (cons reference (map :files (vals damaged)))
                                 text (vals files)]
                             (alength (.getBytes ^String text "UTF-8"))))
               (:max-bytes calibration-policy))
        (refuse! "Calibration answers exceed the byte limit"))))
  calibration)

(defn validate-calibration!
  "Closed offline answer validation without executing checker code."
  [bundle]
  (if (structural-calibration? bundle)
    (validate-structural-calibration! bundle)
    (validate-legacy-calibration! bundle)))

(defn score
  "One scoring path for calibration and verification. Host observation runs
   within the same resource account as authored SCI; fixtures never come from gold."
  ([bundle selection-policy environment files limits]
   (score bundle selection-policy environment files limits {}))
  ([bundle selection-policy environment files limits legacy-input]
   (let [profile (wiki-scoring/selected-profile bundle)
         input (merge {:files files :params (get-in bundle [:definition :params] {})
                       :gold (catalog/gold-of bundle environment) :fetched {} :judgements {}}
                      legacy-input)]
     (run-checker (:checker bundle) input
                  (cond-> limits profile
                    (assoc :prepare-input (partial wiki-scoring/prepare-input bundle selection-policy environment)))))))

(defn calibrate
  "Catalog calibration semantics with bounded inputs and one cumulative
   deadline/allocation account. The account holds one scoring check's budget
   per answer it evaluates (the reference and each damaged answer), so a
   checker that fits a scoring check also fits its calibration. Must run on an
   owned admission/scoring worker."
  ([bundle] (calibrate bundle calibration-policy))
  ([bundle options] (calibrate bundle options nil))
  ([{:keys [calibration definition id] :as bundle} options selection-policy]
   (validate-calibration! bundle)
   (when calibration
     (let [multiple? (structural-calibration? bundle)
           answers (+ (if multiple? (count (:references calibration)) 1) (count (:damaged calibration)))
           per-answer-bytes (or (:max-allocated-bytes options) (:max-allocated-bytes policy))
           limits (assoc policy
                         :deadline-nanos (+ (System/nanoTime) (* 1000000 answers (long (:timeout-ms options))))
                         :allocation-check (sandbox/make-resource-limits :max-bytes (* answers per-answer-bytes)))
           check (fn [case-id files]
                   (score bundle selection-policy {:environment/metadata (cond-> {} case-id (assoc :case case-id))}
                          files limits
                          ;; Preserve the pre-profile calibration contract;
                          ;; verification historically used catalog/gold-of.
                          (if (wiki-scoring/selected-profile bundle) {}
                              {:params (:params definition)
                               :gold (if case-id (get-in bundle [:cases case-id :gold]) (:gold bundle))})))
           references (when multiple?
                        (into {} (for [[k {:keys [case files]}] (:references calibration)] [k (check case files)])))
           reference (when-not multiple? (check (:case calibration) (:reference calibration)))
           reference-reward (if multiple? (apply min (map :reward (vals references))) (:reward reference))
           damaged (into {} (for [[k {:keys [files loses]}] (:damaged calibration)
                                  :let [verdict (check (if multiple? (get-in calibration [:damaged k :case]) (:case calibration)) files)]]
                              [k {:verdict verdict
                                  :ok? (and (every? #(false? (get-in verdict [:checks %])) loses)
                                            (< (:reward verdict) reference-reward))}]))]
       (cond-> {:ok? (and (if multiple? (every? #(every? true? (vals (:checks %))) (vals references))
                             (every? true? (vals (:checks reference))))
                          (every? :ok? (vals damaged)))
                :bundle (str id) :damaged damaged}
         multiple? (assoc :references references)
         (not multiple?) (assoc :reference reference))))))

(defn world-setup
  "Calibrate before candidate execution, on Dvergr's physically owned
   admission worker. Gold and calibration never enter the candidate workspace.
   Knowledge selection is explicit host-admitted metadata, not an assumption
   that a catalog bundle retains its source files."
  ([bundle] (world-setup bundle nil))
  ([bundle selection-policy]
   (validate-calibration! bundle)
   (when selection-policy
     (knowledge-workspace/validate-inputs! (:fixtures bundle))
     (doseq [case (vals (:cases bundle))]
       (knowledge-workspace/validate-inputs! (:fixtures case))))
   (let [profile (wiki-scoring/selected-profile bundle)
         base (cond-> (catalog/world-setup bundle)
                selection-policy knowledge-workspace/with-frozen-staging)]
     (if-not (or (:calibration bundle) profile)
       base
       (evaluation/make-world-setup
        {:id (if (:calibration bundle) :simmis.workflow/calibrated-fixtures :simmis.workflow/structural-fixtures) :version 1
         :basis (cond-> {:fixtures (evaluation/world-setup-ref base)
                         :calibration-policy (if (structural-calibration? bundle) structural-calibration-policy calibration-policy)
                         :checker-policy policy :case (get-in bundle [:calibration :case])}
                  profile (assoc :observation-profile profile :selection-policy selection-policy))
         :prepare (fn [context]
                    (let [_ (when profile
                              ;; Reject broken selected fixture structure before
                              ;; candidate execution, within a scoring account.
                              (run-checker
                               "(ns simmis.fixture-observation) (defn check [_] {:checks {:fixture? true} :reward 1})"
                               {:files {}}
                               (assoc policy :prepare-input
                                      (partial wiki-scoring/prepare-input bundle selection-policy (:environment context)))))
                          report (when (:calibration bundle)
                                   (if profile (calibrate bundle calibration-policy selection-policy) (calibrate bundle)))]
                      (when (and report (not (:ok? report)))
                        (refuse! (str "Checker calibration failed; candidate was not started: "
                                      (if-not (every? true? (vals (get-in report [:reference :checks])))
                                        "reference checks"
                                        (str "damaged answers "
                                             (mapv (comp str key) (remove (comp :ok? val) (:damaged report))))))))
                      (cond-> ((:prepare base) context) report (assoc :calibration report))))})))))

(defn evaluator
  "An ad-hoc evaluator for one pinned offline bundle. No judge calls,
   arbitrary capture roots or hidden provider spend. Calibration is owned by
   the world-setup admission worker, not this scoring function."
  ([bundle] (evaluator bundle nil))
  ([bundle selection-policy]
   (let [profile (wiki-scoring/selected-profile bundle)]
     (evaluation/make-evaluator
      {:id (keyword "simmis.workflow" (:name bundle)) :version 1 :tier :ad-hoc
       :basis (cond-> {:bundle (str (:id bundle)) :checker-policy policy
                       ;; Grading-only parameters also change verifier identity.
                       :params (get-in bundle [:definition :params] {})}
                profile (assoc :observation-profile profile :selection-policy selection-policy))
       :capture (fn [{world :world/room}]
                  {:files (workspace/read-tree world (:capture policy)
                                               {:strict? true :max-files (:max-files policy)
                                                :max-bytes (:max-characters policy)})})
       :observe (fn [{:keys [result default timed-out?] captured :execution/evidence setup :setup/evidence}]
                  (assoc default :files (:files captured) :run-status (:run/status result)
                         :timed-out? (boolean timed-out?)
                         :setup setup :calibration (or (:calibration setup) {:status :not-provided})))
       :verify (fn [environment {:keys [files run-status timed-out?]}]
                 (let [verdict (score bundle selection-policy environment files policy)
                       completed? (= :completed run-status)
                       ;; Only Dvergr's environment-deadline provenance admits
                       ;; partial files; ordinary cancellation remains unscored.
                       scored? (or completed?
                                   (and timed-out? (true? (get-in bundle [:definition :score-on-timeout]))))]
                   (-> verdict
                       (update :checks #(assoc % :completed? (and completed? (get % :completed? true))))
                       (update :reward #(if scored? % 0.0)))))}))))
