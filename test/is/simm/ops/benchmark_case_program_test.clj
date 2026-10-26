(ns is.simm.ops.benchmark-case-program-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.roster :as roster]
            [is.simm.ops.benchmark-case-program :as resolved]
            [is.simm.ops.benchmarks-test :as benchmarks]
            [is.simm.ops.workflows-test :as workflows]))

(defn- plan []
  (let [files (assoc workflows/fixture "cases/a/gold.edn" "{}" "cases/b/gold.edn" "{}")
        prepared (:plan (benchmarks/prepared files (assoc (benchmarks/input files) :cases ["a" "b"])))
        dataset (experiment/make-dataset
                 {:id :test/dataset :version 9 :environments (:environments prepared)
                  :metadata {:selection {:split "development" :cases ["a" "b"]} :false false}})
        canonical (experiment/make-experiment
                   {:id :test/final-request-id :version 7 :dataset dataset
                    :candidates (roster/agents (:team prepared)) :repetitions 2
                    :metadata {:runtime {:libraries {:fixture "version"}} :false false :zero 0}})]
    (-> prepared
        (assoc :experiment canonical :parent-run (random-uuid) :cleanup-group (random-uuid)
               :id :test/outer-id :repetitions 1 :metadata {:outer "not final"})
        (assoc-in [:capabilities :unrelated] (fn [] :host-only)))))

(defn- selection []
  (let [basis {:store (random-uuid) :repository (random-uuid) :commit (random-uuid)
               :configuration (random-uuid) :backend :memory :branch :case :ref "case"
               :datoms 1 :max-tx 1}]
    {:version 1 :policy :request-evidence-v1 :basis basis
     :origin {:version 1 :kind :datahike :commit (:commit basis)
              :source (select-keys basis [:store :repository])}
     :target (merge (select-keys basis [:store :repository :backend :branch :ref])
                    {:registry-system (random-uuid) :runtime-system "private-fixture"})}))

(defn- bindings [selection]
  (mapv (fn [ordinal] {:ordinal ordinal :manifest-id (random-uuid) :selection selection}) [0 1]))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(deftest canonical-experiment-and-unrelated-execution-policy-survive-resolution
  (let [original (plan) program-id (random-uuid) template-id (random-uuid)
        selected (bindings (selection))
        result (resolved/resolve-plan original program-id template-id selected)
        changed (:plan result) a (:experiment original) b (:experiment changed)
        da (:experiment/dataset a) db (:experiment/dataset b)]
    (is (= (dissoc original :environments :experiment :capabilities)
           (dissoc changed :environments :experiment :capabilities)))
    (is (= (dissoc (:capabilities original) :world-setup)
           (dissoc (:capabilities changed) :world-setups)))
    (is (= (dissoc a :experiment/content-id :experiment/dataset :experiment/metadata)
           (dissoc b :experiment/content-id :experiment/dataset :experiment/metadata)))
    (is (= (:experiment/metadata a) (dissoc (:experiment/metadata b) :simmis/benchmark-resolution)))
    (is (= (:provenance result) (get-in b [:experiment/metadata :simmis/benchmark-resolution])))
    (is (= (dissoc da :dataset/environments :dataset/content-id)
           (dissoc db :dataset/environments :dataset/content-id)))
    (is (= b (experiment/validate-experiment b)))
    (is (not= (:experiment/content-id a) (:experiment/content-id b)))
    (doseq [[before after binding] (map vector (:environments original) (:environments changed) selected)]
      (is (= (dissoc before :environment/world :environment/metadata :environment/content-id)
             (dissoc after :environment/world :environment/metadata :environment/content-id)))
      (is (= (dissoc (:environment/world before) :setup) (dissoc (:environment/world after) :setup)))
      (is (= (:environment/metadata before) (dissoc (:environment/metadata after) :simmis/benchmark-case)))
      (is (= (:manifest-id binding) (get-in after [:environment/metadata :simmis/benchmark-case :manifest-id])))
      (let [ref (get-in after [:environment/world :setup])
            setup (get-in changed [:capabilities :world-setups ref])]
        (is (= ref (evaluation/world-setup-ref setup)))
        (is (= {"private-fixture" (get-in binding [:selection :origin])} (:checkpoints setup)))))
    ;; The two unchanged cases can share one exact native origin/setup while
    ;; retaining distinct Environment identities and case manifest bindings.
    (is (= 1 (count (get-in changed [:capabilities :world-setups]))))
    (is (= 2 (count (set (map :environment/content-id (:environments changed))))))))

(deftest malformed-order-and-ambiguous-source-policies-are-refused
  (let [original (plan) selected (bindings (selection))
        resolve #(resolved/resolve-plan %1 (random-uuid) (random-uuid) %2)]
    (doseq [bad [(subvec selected 0 1) (vec (reverse selected))
                 (assoc-in selected [0 :extra] true) (assoc-in selected [0 :manifest-id] nil)]]
      (is (thrown? clojure.lang.ExceptionInfo (resolve original bad))))
    (is (= :original-plan (reason #(resolve (assoc-in original [:capabilities :world-setups] {}) selected))))
    (is (= :original-plan
           (reason #(resolve (assoc original :environments (vec (reverse (:environments original)))) selected))))
    (let [canonical (:experiment original)
          reserved (experiment/make-experiment
                    {:id (:experiment/id canonical) :version (:experiment/version canonical)
                     :dataset (:experiment/dataset canonical) :candidates (roster/agents (:team original))
                     :repetitions (:experiment/repetitions canonical)
                     :metadata (assoc (:experiment/metadata canonical) :simmis/benchmark-resolution nil)})]
      (is (= :original-plan (reason #(resolve (assoc original :experiment reserved) selected)))
          "A reserved provenance key is refused even when its value is nil"))
    (let [agent (first (roster/agents (:team original)))
          changed-team (roster/revise-agent (:team original) (:agent/id agent)
                                            {:model-policy {:model "other" :provider :openai}})]
      (is (= :candidates-changed (reason #(resolve (assoc original :team changed-team) selected)))
          "Different team content is refused even when its candidate ids match"))))

(deftest portable-setup-reference-does-not-substitute-for-a-valid-host-capability
  (let [original (plan) selected (bindings (selection))
        base (get-in original [:capabilities :world-setup])]
    (doseq [invalid [nil (into {} base) (assoc base :prepare nil)]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (resolved/resolve-plan (assoc-in original [:capabilities :world-setup] invalid)
                                          (random-uuid) (random-uuid) selected))))))
