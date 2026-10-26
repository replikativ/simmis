(ns is.simm.ops.knowledge-execution-origin-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.model.knowledge-pins-test :as pins]
            [is.simm.model.knowledge-streams-test :as streams]
            [is.simm.ops.knowledge-batch-inputs :as batches]
            [is.simm.ops.knowledge-execution-origin :as origin]
            [is.simm.starter.wiki-maintenance :as starter]))

(defn- source []
  {"execution-origin.edn" (pr-str origin/policy)
   "input-batching.edn" (pr-str {:version 2 :epoch (random-uuid)
                                 :policy (assoc batches/default-policy :context-paths []
                                                :max-context-files 0 :max-context-bytes 0)})})

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest origin-opt-in-is-closed-single-form-and-has-no-reader-evaluation
  (is (nil? (origin/declaration {})))
  (is (= origin/policy (origin/declaration (source))))
  (doseq [text [nil "" "#=(System/exit 0)" "#unknown {}"
                (str (pr-str origin/policy) " {}")
                (pr-str (assoc origin/policy :commit (random-uuid)))
                (pr-str (assoc origin/policy :version 2))
                (apply str (repeat 4097 " "))]]
    (is (= :invalid-declaration
           (reason #(origin/declaration (assoc (source) "execution-origin.edn" text)))))))

(deftest on-demand-origin-requires-explicit-zero-copied-context
  (doseq [batch-policy [batches/default-policy
                        (assoc batches/default-policy :max-context-files 0 :max-context-bytes 0)
                        (assoc batches/default-policy :context-paths [])]]
    (is (= :copied-context-policy-mismatch
           (reason #(origin/declaration
                     (assoc (source) "input-batching.edn"
                            (pr-str {:version 2 :epoch (random-uuid) :policy batch-policy})))))))
  (is (= :copied-context-policy-mismatch
         (reason #(origin/declaration (dissoc (source) "input-batching.edn"))))))

(deftest copied-context-starter-is-unchanged-and-unsupported-executors-refuse-opt-in
  (let [v4 (starter/source 4)
        opted-in (source)
        batching (batches/declaration opted-in)]
    (is (nil? (origin/declaration v4)))
    (is (nil? (origin/require-copied-context! v4)))
    (is (= :checkpoint-execution-unsupported (reason #(origin/require-copied-context! opted-in))))
    (is (= (select-keys batches/default-policy [:max-trigger-files :max-trigger-bytes])
           (select-keys (:policy batching) [:max-trigger-files :max-trigger-bytes])))))

(deftest generated-file-proposals-remain-the-only-checkpoint-publication-route
  (is (= origin/policy (origin/validate-execution! (source) {:settlement "discard"})))
  (is (= :checkpoint-requires-discard
         (reason #(origin/validate-execution! (source) {:settlement "review"}))))
  (is (nil? (origin/validate-execution! (starter/source 4) {:settlement "review"}))))

(deftest origin-comes-only-from-exact-retained-batch-basis
  (let [basis streams/basis
        target (:target (pins/origin basis))
        proof {:selection {:basis basis} :source-origin {:target target}}
        selected (origin/selection (source) proof)]
    (is (= basis (:basis selected)))
    (is (= target (:target selected)))
    (is (= {:version 1 :kind :datahike :commit (:commit basis)
            :source (select-keys basis [:store :repository])} (:origin selected)))
    (is (nil? (origin/selection {} proof)))
    (is (thrown? clojure.lang.ExceptionInfo (origin/selection (source) {})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (origin/selection (source) (assoc-in proof [:source-origin :target :store] (random-uuid)))))))
