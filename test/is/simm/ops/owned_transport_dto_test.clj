(ns is.simm.ops.owned-transport-dto-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [dvergr.ops :as ops]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]))

(def schema (conj runs/schema [:expected-owned-source :uuid]))
(defn- input []
  {:room "fixture-room" :name "echo" :request-id (str (random-uuid))
   :expected-bundle (str (random-uuid)) :expected-owned-source (str (random-uuid))
   :model "codex-subscription-sol"
   :limits {:budget-microdollars 10000 :max-dispatches 3 :max-model-steps 3
            :timeout-ms 30000 :settlement "discard"}})
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(deftest normalization-keeps-the-full-guarded-intent-and-canonicalizes-uuid-case
  (let [raw (-> (input)
                (update :request-id str/upper-case)
                (update :expected-bundle str/upper-case)
                (update :expected-owned-source str/upper-case)
                (assoc :case "evidence" :params {:company "ACME"}
                       :inputs {(keyword "/docs/input.md") "evidence"}))
        result (api/owned-transport! schema raw)]
    (is (m/validate schema result))
    (is (= {:company "ACME"} (:params raw)))
    (is (= {"company" "ACME"} (:params result)))
    (is (= {"/docs/input.md" "evidence"} (:inputs result)))
    (is (= (-> raw (dissoc :room :params :inputs)
               (update :request-id parse-uuid) (update :expected-bundle parse-uuid)
               (update :expected-owned-source parse-uuid))
           (dissoc result :params :inputs)))
    (is (not (some #{:expected-owned-source} runs/source-fields)))
    (is (not (m/validate runs/source-schema (select-keys result [:name :expected-bundle :expected-owned-source]))))))

(deftest absent-fields-stay-absent-and-required-guards-are-never-invented
  (let [raw (input) result (api/owned-transport! schema raw)]
    (is (not (contains? result :params)))
    (is (not (contains? result :inputs)))
    (doseq [invalid [(dissoc raw :expected-owned-source)
                     (assoc raw :expected-owned-source nil)
                     (assoc raw :expected-owned-source 17)
                     (assoc raw :expected-owned-source "not-a-uuid")]]
      (is (= 400 (ops/error-status (failure #(api/owned-transport! schema invalid))))))))

(deftest collisions-use-one-normal-result-in-both-map-fields-and-legacy-messages-survive
  (doseq [[field values] [[:params {:company "a" "company" "b"}]
                          [:inputs {(keyword "/docs/a.md") "a" "/docs/a.md" "b"}]]]
    (let [raw (assoc (input) field values)
          internal (-> raw (dissoc :room) (update :request-id parse-uuid)
                       (update :expected-bundle parse-uuid) (update :expected-owned-source parse-uuid))]
      (is (= {:status :invalid :reason :duplicate-key} (runs/transport-result schema internal)))
      (is (= "Duplicate workflow input key" (ex-message (failure #(runs/transport-for schema internal)))))
      (is (= 400 (ops/error-status (failure #(api/owned-transport! schema raw))))))))

(deftest helper-tags-and-unknown-normal-results-cannot-control-the-adapter
  (doseq [tag [:is.simm.ops.owned-workflow-admission/invalid
               :is.simm.ops.owned-workflow-admission/conflict]]
    (with-redefs [runs/transport-result (fn [& _] (throw (ex-info "SECRET native diagnostic" {:type tag :status 409})))]
      (let [error (failure #(api/owned-transport! schema (input)))]
        (is (= 503 (ops/error-status error)))
        (is (not (.contains (str error) "SECRET"))))))
  (doseq [result [nil {:status :absent} {:status :invalid :reason :unknown}
                  {:status :invalid :reason :shape :extra "SECRET"}
                  {:status :ok :input nil} {:status :ok :input "SECRET"}]]
    (with-redefs [runs/transport-result (fn [& _] result)]
      (let [error (failure #(api/owned-transport! schema (input)))]
        (is (= 503 (ops/error-status error)))
        (is (not (.contains (str error) "SECRET"))))))
  (let [raw (input)]
    (with-redefs [runs/transport-result
                  (fn [_ converted] {:status :ok :input (assoc converted :expected-owned-source (random-uuid))})]
      (is (= 503 (ops/error-status (failure #(api/owned-transport! schema raw))))))))

(deftest causal-interruption-is-preserved-before-error-reporting
  (try
    (with-redefs [runs/transport-result (fn [& _] (throw (ex-info "SECRET" {} (InterruptedException. "SECRET"))))]
      (let [error (failure #(api/owned-transport! schema (input)))
            interrupted? (.isInterrupted (Thread/currentThread))]
        (is (= 503 (ops/error-status error)))
        (is interrupted?)))
    (finally (Thread/interrupted))))

(deftest file-key-normalization-preserves-inspected-revision-and-detects-collisions
  (let [revision (str (random-uuid))
        raw {:name "echo" :expected-revision revision :files {:task.md "Hello"}}
        result (workflows/transport-write-result raw)]
    (is (= {:status :ok :input (assoc raw :files {"task.md" "Hello"})} result))
    (is (= revision (get-in result [:input :expected-revision])))
    (is (= {:status :invalid :reason :duplicate-key}
           (workflows/transport-write-result (assoc raw :files {:task.md "a" "task.md" "b"}))))))
