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
  ([source input {:keys [timeout-ms max-allocated-bytes max-checks max-check-key-length deadline-nanos allocation-check]}]
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
     (let [ctx (sandbox/create-base-ctx :load-fn (constantly nil) :resource-limits guard!)
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
  {:version 1 :max-damaged 4 :max-files 100 :max-bytes 2097152 :timeout-ms 20000})

(defn validate-calibration!
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

(defn calibrate
  "Catalog calibration semantics with bounded inputs and one cumulative
   deadline/allocation account. Must run on an owned admission/scoring worker."
  ([bundle] (calibrate bundle calibration-policy))
  ([{:keys [calibration checker definition id] :as bundle} options]
   (validate-calibration! bundle)
   (when calibration
     (let [limits (assoc policy
                         :deadline-nanos (+ (System/nanoTime) (* 1000000 (long (:timeout-ms options))))
                         :allocation-check (sandbox/make-resource-limits :max-bytes (:max-allocated-bytes policy)))
           gold (if-let [case-id (:case calibration)] (get-in bundle [:cases case-id :gold]) (:gold bundle))
           check (fn [files] (run-checker checker {:files files :gold gold :params (:params definition)
                                                   :fetched {} :judgements {}} limits))
           reference (check (:reference calibration))
           damaged (into {} (for [[k {:keys [files loses]}] (:damaged calibration)
                                  :let [verdict (check files)]]
                              [k {:verdict verdict
                                  :ok? (and (every? #(false? (get-in verdict [:checks %])) loses)
                                            (< (:reward verdict) (:reward reference)))}]))]
       {:ok? (and (every? true? (vals (:checks reference))) (every? :ok? (vals damaged)))
        :bundle (str id) :reference reference :damaged damaged}))))

(defn world-setup
  "Calibrate before candidate execution, on Dvergr's physically owned
   admission worker. Gold and calibration never enter the candidate workspace.
   Knowledge selection is explicit host-admitted metadata, not an assumption
   that a catalog bundle retains its source files."
  ([bundle] (world-setup bundle nil))
  ([bundle selection-policy]
   (validate-calibration! bundle)
   (let [base (cond-> (catalog/world-setup bundle)
                selection-policy knowledge-workspace/with-frozen-staging)]
     (if-not (:calibration bundle)
       base
       (evaluation/make-world-setup
        {:id :simmis.workflow/calibrated-fixtures :version 1
         :basis {:fixtures (evaluation/world-setup-ref base) :calibration-policy calibration-policy :checker-policy policy
                 :case (get-in bundle [:calibration :case])}
         :prepare (fn [context]
                    (let [report (calibrate bundle)]
                      (when-not (:ok? report)
                        (refuse! (str "Checker calibration failed; candidate was not started: "
                                      (if-not (every? true? (vals (get-in report [:reference :checks])))
                                        "reference checks"
                                        (str "damaged answers "
                                             (mapv (comp str key) (remove (comp :ok? val) (:damaged report))))))))
                      (assoc ((:prepare base) context) :calibration report)))})))))

(defn evaluator
  "An ad-hoc evaluator for one pinned offline bundle. No judge calls,
   arbitrary capture roots or hidden provider spend. Calibration is owned by
   the world-setup admission worker, not this scoring function."
  [bundle]
  (evaluation/make-evaluator
   {:id (keyword "simmis.workflow" (:name bundle)) :version 1 :tier :ad-hoc
    :basis {:bundle (str (:id bundle)) :checker-policy policy
            ;; Grading-only parameters must also change verifier identity.
            :params (get-in bundle [:definition :params] {})}
    :capture (fn [{world :world/room}]
               {:files (workspace/read-tree world (:capture policy)
                                            {:strict? true :max-files (:max-files policy)
                                             :max-bytes (:max-characters policy)})})
    :observe (fn [{:keys [result default] captured :execution/evidence setup :setup/evidence}]
               (assoc default :files (:files captured) :run-status (:run/status result)
                      :setup setup :calibration (or (:calibration setup) {:status :not-provided})))
    :verify (fn [environment {:keys [files run-status]}]
              (let [verdict (run-checker (:checker bundle)
                                         {:files files :params (get-in bundle [:definition :params] {})
                                          :gold (catalog/gold-of bundle environment)
                                          :fetched {} :judgements {}})
                    completed? (= :completed run-status)]
                (-> verdict
                    (update :checks #(assoc % :completed? (and completed? (get % :completed? true))))
                    (update :reward #(if completed? % 0.0)))))}))
