(ns is.simm.ops.file-output-plan-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-outputs :as outputs]))

(defn- with-input [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn {:name "Historical file proposal plans"})
        (repo/write! conn "inbox/source.md" (markdown/source-bytes "Evidence"))
        (repo/write! conn "wiki/old.md" (markdown/source-bytes "Before\r\n"))
        (let [basis (inputs/publication-basis @conn)
              captured (inputs/capture @conn inputs/default-policy)
              base {:request (random-uuid) :room (random-uuid) :attempt (random-uuid)
                    :environment {:fixture :environment} :workflow-bundle (random-uuid)
                    :publication-basis basis :retained-inputs (:inputs captured)}]
          (f conn base))
        (finally (d/release conn))))))

(defn- output! [conn files]
  (doseq [[path text] files] (repo/write! conn (str "out/" path) (markdown/source-bytes text)))
  (outputs/capture-db @conn))

(deftest plan-retains-exact-before-after-without-reading-todays-files
  (with-input
    (fn [conn base]
      (let [output (output! conn {"wiki/old.md" "After\r\n" "wiki/new.md" "New"})
            input (assoc base :output output)]
        (repo/write! conn "wiki/old.md" (markdown/source-bytes "Later human edit"))
        (repo/write! conn "wiki/new.md" (markdown/source-bytes "Human created this later"))
        (let [plan (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Plan read live files")))]
                     (plans/plan input))]
          (is (= :present (get-in plan [:manifest :entries "wiki/old.md" :before :state])))
          (is (= {:state :absent} (get-in plan [:manifest :entries "wiki/new.md" :before])))
          (is (= "Before\r\n" (markdown/decode (outputs/bytes! (get-in plan [:bodies "wiki/old.md" :before])))))
          (is (= "After\r\n" (markdown/decode (outputs/bytes! (get-in plan [:bodies "wiki/old.md" :after])))))
          (is (= (:id (:publication-basis base)) (get-in plan [:manifest :basis])))
          (is (= plan (plans/plan input)))
          (is (not= (:proposal plan) (:proposal (plans/plan (assoc input :attempt (random-uuid)))))))))))

(deftest missing-or-forged-historical-bodies-cannot-be-reconstructed-as-empty
  (with-input
    (fn [conn base]
      (let [input (assoc base :output (output! conn {"wiki/old.md" "After"}))]
        (doseq [retained [{} {"/in/knowledge/sources/wiki/old.md" "Not the original"}]]
          (is (thrown? clojure.lang.ExceptionInfo (plans/plan (assoc input :retained-inputs retained)))))))))

(deftest omitted-and-unchanged-files-are-not-deletions-or-empty-edits
  (with-input
    (fn [conn base]
      (let [input (assoc base :output (output! conn {"wiki/old.md" "Before\r\n"}))
            planned (plans/plan input)]
        (is (empty? (get-in planned [:manifest :entries])))
        (is (empty? (:bodies planned)))))))

(deftest tiny-after-cannot-allocate-over-budget-historical-bodies
  (with-input
    (fn [conn base]
      (let [input (assoc base :output (output! conn {"wiki/old.md" "A"}))
            encode markdown/source-bytes]
        (with-redefs [outputs/policy (assoc outputs/policy :max-total-bytes 1)
                      markdown/source-bytes (fn [text]
                                              (when (= "Before\r\n" text)
                                                (throw (AssertionError. "Allocated BEFORE before metadata cap")))
                                              (encode text))]
          (is (thrown? clojure.lang.ExceptionInfo (plans/plan input))))))))
