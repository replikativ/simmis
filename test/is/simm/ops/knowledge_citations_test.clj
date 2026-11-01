(ns is.simm.ops.knowledge-citations-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.links :as links]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.knowledge-citations :as citations]))

(def room #uuid "00000000-0000-4000-8000-000000000001")
(def request-a #uuid "00000000-0000-4000-8000-000000000002")
(def request-b #uuid "00000000-0000-4000-8000-000000000003")

(deftest invocation-citations-do-not-change-the-semantic-program
  (let [original "Inspect original evidence and propose source-backed notes."
        contexts (vec (for [version [1 2] request [request-a request-b]]
                        {:version version :room room :request request}))
        inputs (mapv #(hash-map :knowledge/source-task original :knowledge/source-citations %
                                :environment {:environment/task (citations/task original %)}) contexts)]
    (is (not= (get-in (first inputs) [:environment :environment/task])
              (get-in (second inputs) [:environment :environment/task])))
    (is (= (repeat 4 original) (mapv citations/semantic-task inputs)))
    (is (= original (citations/semantic-task {:environment {:environment/task original}})))
    (doseq [bad [(assoc-in (first inputs) [:knowledge/source-citations :request] request-b)
                 (assoc (first inputs) :knowledge/source-task "Different program")
                 (dissoc (first inputs) :knowledge/source-citations)]]
      (is (thrown? Exception (citations/semantic-task bad))))))

(deftest citation-context-is-closed-and-bounded
  (doseq [context [{:version 1 :room room :request "not UUID"}
                   {:version 1 :room room :request request-a :authority :admin}
                   {:version 3 :room room :request request-a}
                   nil false []]]
    (is (not (citations/valid-context? context)))
    ;; nil intentionally means no routing for ordinary non-knowledge tasks.
    (when (some? context)
      (is (thrown? Exception (citations/task "Task" context)))))
  (doseq [version [1 2]]
    (is (thrown? Exception (citations/task (apply str (repeat 16000 "x"))
                                         {:version version :room room :request request-a})))
    (is (citations/valid-context? {:version version :room room :request request-a} room request-a))
    (is (not (citations/valid-context? {:version version :room room :request request-a} room request-b)))))

(deftest old-citation-instructions-remain-byte-for-byte-verifiable
  (let [expected (str "Task\n\nHOST citation routing: cite original manifest sources using /room/"
                      room "/request/" request-a "/source?path= followed by the percent-encoded exact :input path from the frozen manifest. "
                      "These links read the retained source revision under current reader authority; "
                      "they remain valid after proposal adoption. Do not substitute a digest for original evidence.")
        context {:version 1 :room room :request request-a}]
    (is (= expected (citations/task "Task" context)))
    (is (= "Task" (citations/semantic-task {:knowledge/source-task "Task" :knowledge/source-citations context
                                            :environment {:environment/task expected}})))))

(deftest new-instructions-provide-a-real-canonical-markdown-destination
  (let [context {:version 2 :room room :request request-a}
        text (citations/task "Task" context)
        targets (mapv :target (markdown/link-references (markdown/parse text)))
        expected (links/request-source-route (assoc context :path "/in/knowledge/sources/example.md"))
        [_ route-room route-request query] (re-matches #"/room/([^/]+)/request/([^/]+)/source\?(.+)" (first targets))]
    (is (= [expected] targets))
    (is (= {:kind :request-source :room (str room) :request (str request-a) :path "/in/knowledge/sources/example.md"}
           (links/parse-request-source-query route-room route-request query)))
    (is (not (str/includes? text "HOST citation routing:")))
    (is (not (str/starts-with? (first targets) "[HOST]")))))
