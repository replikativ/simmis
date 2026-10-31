(ns is.simm.ops.knowledge-citations-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.knowledge-citations :as citations]))

(def room #uuid "00000000-0000-4000-8000-000000000001")
(def request-a #uuid "00000000-0000-4000-8000-000000000002")
(def request-b #uuid "00000000-0000-4000-8000-000000000003")

(deftest invocation-citations-do-not-change-the-semantic-program
  (let [original "Inspect original evidence and propose source-backed notes."
        contexts (mapv #(hash-map :version 1 :room room :request %) [request-a request-b])
        inputs (mapv #(hash-map :knowledge/source-task original :knowledge/source-citations %
                                :environment {:environment/task (citations/task original %)}) contexts)]
    (is (not= (get-in (first inputs) [:environment :environment/task])
              (get-in (second inputs) [:environment :environment/task])))
    (is (= [original original] (mapv citations/semantic-task inputs)))
    (is (= original (citations/semantic-task {:environment {:environment/task original}})))
    (doseq [bad [(assoc-in (first inputs) [:knowledge/source-citations :request] request-b)
                 (assoc (first inputs) :knowledge/source-task "Different program")
                 (dissoc (first inputs) :knowledge/source-citations)]]
      (is (thrown? Exception (citations/semantic-task bad))))))

(deftest citation-context-is-closed-and-bounded
  (doseq [context [{:version 1 :room room :request "not UUID"}
                   {:version 1 :room room :request request-a :authority :admin}
                   {:version 2 :room room :request request-a}]]
    (is (thrown? Exception (citations/task "Task" context))))
  (is (thrown? Exception (citations/task (apply str (repeat 16000 "x"))
                                       {:version 1 :room room :request request-a}))))
