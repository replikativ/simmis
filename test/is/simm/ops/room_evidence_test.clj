(ns is.simm.ops.room-evidence-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.room-evidence :as evidence]))

(def room (parse-uuid "00000000-0000-0000-0000-000000000001"))
(def source-store (parse-uuid "00000000-0000-0000-0000-000000000002"))
(defn row [id text]
  {:id (parse-uuid (str "00000000-0000-0000-0000-" (format "%012d" id)))
   :timestamp 1 :text text :author room :actor-kind :human :metadata {:message/role :user}})
(defn plan [rows retained]
  (evidence/select-next room source-store rows #(contains? retained (:path %))))

(deftest original-revisions-are-independent-stable-and-ordered
  (let [a (row 1 "same") b (assoc (row 2 "same") :actor-kind :agent)
        p (plan [b a] #{}) artifacts (:artifacts p)
        retained (set (map :path artifacts))]
    (is (= [(:id a) (:id b)] (mapv :message-id artifacts)))
    (is (= p (plan [a b] #{})))
    (is (= 2 (count retained)) "Identical text from distinct originals is independently retained")
    (is (empty? (:artifacts (plan [a b] retained))))
    (is (= [(:id a)] (mapv :message-id (:artifacts (plan [(assoc a :text "corrected") b] retained)))))
    (is (= 1 (count (:artifacts (plan [(assoc-in a [:metadata :message/thread-root-id] source-store)] retained)))))
    (is (not= (:source-revision (first artifacts))
              (:source-revision (evidence/artifact room (random-uuid) a))))))

(deftest bounded-batches-never-skip-oldest-or-oversized-head
  (let [rows (mapv #(row % (str %)) (range 1 25))
        p (plan (reverse rows) #{})]
    (is (= :bounded (:state p)))
    (is (= (mapv :id (take 20 rows)) (mapv :message-id (:artifacts p))))
    (is (= (mapv :id (drop 20 rows))
           (mapv :message-id (:artifacts (plan rows (set (map :path (:artifacts p)))))))))
  (let [p (plan [(row 1 (apply str (repeat (inc evidence/max-source-bytes) "x"))) (row 2 "later")] #{})]
    (is (= :held (:state p)))
    (is (= :oversized-message (get-in p [:held :reason])))
    (is (empty? (:artifacts p)))
    (is (false? (:complete? p)))))

(deftest original-text-remains-inert-and-limitations-honest
  (let [a (evidence/artifact room source-store (row 1 "```\n<script>λ</script>"))]
    (is (.contains (:text a) "````text\n```\n<script>λ</script>\n````"))
    (is (some #{:deletions-and-unavailable-original-revisions-not-enumerated}
              (:limitations (plan [] #{}))))))
