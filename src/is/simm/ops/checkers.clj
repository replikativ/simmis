(ns is.simm.ops.checkers
  "Trusted-operator checker execution inside Dvergr's owned scoring worker.
   No detached future: physical cleanup remains pending until this returns.
   SCI deadlines/allocation checks are cooperative, NOT hard OS isolation."
  (:require [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.sandbox :as sandbox]
            [sci.core :as sci])
  (:import [java.util.concurrent.atomic AtomicBoolean]))

(def policy
  {:version 1 :timeout-ms 20000 :max-allocated-bytes 134217728
   :max-checks 100 :capture "/out" :max-files 100 :max-characters 2000000})

(defn- refuse! [message]
  (throw (ex-info message {:type ::invalid})))

(defn run-checker
  "Execute and fully validate one checker ON THE CALLING scoring worker.
   Caller must have explicitly admitted this exact source as trusted operator
   code. Never invoke this on Spindel's event-drain thread. Options are a
   trusted host test seam, not externally supplied checker permissions."
  ([source input] (run-checker source input policy))
  ([source input {:keys [timeout-ms max-allocated-bytes max-checks]}]
   (let [deadline (+ (System/nanoTime) (* 1000000 (long timeout-ms)))
         stopped (AtomicBoolean. false)
         allocation-check (sandbox/make-resource-limits :max-bytes max-allocated-bytes)
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
                      (every? (fn [[k v]] (and (keyword? k) (boolean? v))) checks)
                      (number? reward) (Double/isFinite (double reward))
                      (<= 0.0 (double reward) 1.0))
         (refuse! "Checker must return bounded boolean checks and a finite reward in [0,1]"))
       (guard!)
       {:checks (into {} checks) :reward (double reward)}))))

(defn evaluator
  "An ad-hoc evaluator for one pinned offline bundle. No judge calls,
   calibration execution, arbitrary capture roots or hidden provider spend."
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
    :observe (fn [{:keys [result default] captured :execution/evidence}]
               (assoc default :files (:files captured) :run-status (:run/status result)))
    :verify (fn [environment {:keys [files run-status]}]
              (let [verdict (run-checker (:checker bundle)
                                         {:files files :params (get-in bundle [:definition :params] {})
                                          :gold (catalog/gold-of bundle environment)
                                          :fetched {} :judgements {}})
                    completed? (= :completed run-status)]
                (-> verdict
                    (update :checks #(assoc % :completed? (and completed? (get % :completed? true))))
                    (update :reward #(if completed? % 0.0)))))}))
