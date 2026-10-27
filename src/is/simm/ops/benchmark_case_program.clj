(ns is.simm.ops.benchmark-case-program
  "Pure HOST reconstruction of a canonical benchmark at retained case origins.
   No allocation, pinning, authorization or execution. The admitting coordinator
   must verify native provenance, retention and cleanup before using this plan."
  (:require [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.roster :as roster]
            [is.simm.model.work-request-checkpoints :as checkpoints]
            [is.simm.ops.benchmark-case-manifest :as manifest]
            [is.simm.ops.knowledge-workspace :as workspace]))

(defn- refuse! [reason]
  (throw (ex-info "Resolved benchmark program refused" {:type ::invalid :reason reason})))

(defn- binding! [ordinal binding]
  (when-not (and (map? binding) (= #{:ordinal :manifest-id :selection} (set (keys binding)))
                 (= ordinal (:ordinal binding)) (uuid? (:manifest-id binding)))
    (refuse! :case-binding))
  (checkpoints/validate-selection! (:selection binding))
  binding)

(defn- environment-spec [definition]
  (let [verifier (:environment/verifier definition)]
    {:id (:environment/id definition) :version (:environment/version definition)
     :task (:environment/task definition) :limits (:environment/limits definition)
     :world (:environment/world definition) :metadata (:environment/metadata definition)
     :verifier (cond-> {:id (:verifier/id verifier) :version (:verifier/version verifier)}
                 (contains? verifier :verifier/basis) (assoc :basis (:verifier/basis verifier)))}))

(defn resolve-plan
  "Replace only per-case setup refs and add declared comparison provenance.
   Rebuild from the ORIGINAL canonical Experiment, not runner/experiment-def:
   the surrounding plan may omit its final request id, repetitions or metadata.
   Case order is the canonical Dataset order. Returned setup capabilities are
   HOST-only; portable case bindings are observations, never creation rights."
  [plan program-id template-id bindings]
  (let [original (experiment/validate-experiment (:experiment plan))
        dataset (:experiment/dataset original)
        definitions (:dataset/environments dataset)
        base (evaluation/validate-world-setup (get-in plan [:capabilities :world-setup]))
        base-ref (evaluation/world-setup-ref base)
        provenance {:version 1 :program-id program-id :template-id template-id
                    :comparison-id (manifest/comparison-id program-id template-id)}]
    (when-not (and (= definitions (:environments plan))
                   (vector? bindings) (= (count definitions) (count bindings))
                   (<= 1 (count bindings) 8)
                   (not (contains? (:capabilities plan) :world-setups))
                   (not (seq (:checkpoints base)))
                   (not (contains? (:experiment/metadata original) :simmis/benchmark-resolution))
                   (every? #(and (= base-ref (get-in % [:environment/world :setup]))
                                 (not (contains? (:environment/metadata %) :simmis/benchmark-case)))
                           definitions))
      (refuse! :original-plan))
    (doseq [[ordinal binding] (map-indexed vector bindings)] (binding! ordinal binding))
    (let [setups (mapv (fn [{:keys [selection]}]
                         (workspace/with-checkpoint
                           (workspace/with-publication-target base (:target selection)) selection))
                       bindings)
          environments
          (mapv (fn [definition setup {:keys [ordinal manifest-id]}]
                  (environment/make-environment
                   (-> (environment-spec definition)
                       (assoc-in [:world :setup] (evaluation/world-setup-ref setup))
                       (update :metadata assoc :simmis/benchmark-case
                               {:version 1 :ordinal ordinal :manifest-id manifest-id :template-id template-id}))))
                definitions setups bindings)
          dataset (experiment/make-dataset
                   {:id (:dataset/id dataset) :version (:dataset/version dataset)
                    :metadata (:dataset/metadata dataset) :environments environments})
          resolved (experiment/make-experiment
                    {:id (:experiment/id original) :version (:experiment/version original)
                     :dataset dataset :candidates (roster/agents (:team plan))
                     :repetitions (:experiment/repetitions original)
                     :metadata (assoc (:experiment/metadata original) :simmis/benchmark-resolution provenance)})]
      (when-not (= (:experiment/candidates original) (:experiment/candidates resolved))
        (refuse! :candidates-changed))
      {:provenance provenance
       :plan (-> plan
                 (assoc :environments environments :experiment resolved)
                 (update :capabilities
                         #(-> % (dissoc :world-setup)
                              (assoc :world-setups (into {} (map (fn [setup]
                                                                   [(evaluation/world-setup-ref setup) setup]) setups))))))})))
