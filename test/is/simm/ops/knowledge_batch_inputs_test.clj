(ns is.simm.ops.knowledge-batch-inputs-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.view :as view]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-streams-test :as journal]
            [is.simm.ops.knowledge-batch-inputs :as batches]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.knowledge-changes-test :as source-fixture]
            [is.simm.ops.knowledge-inputs :as inputs]))

(def reserved {:files 0 :bytes 0})
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(defn- fixture
 ([n f] (fixture n (constantly nil) f))
 ([n initialize! f]
  (@#'source-fixture/repository false
   (fn [conn]
     (repo/write! conn "seed.txt" (markdown/source-bytes "Evidence"))
     (repo/write! conn "wiki/company.md" (markdown/source-bytes "Existing company context"))
     (let [entry (files/entry @conn "seed.txt")]
       (d/transact conn
         (mapv (fn [i] {:geschichte.work/path (format "inbox/%03d.md" i)
                        :geschichte.work/content [:geschichte.content/id (:content entry)]
                        :geschichte.work/mode (:mode entry) :geschichte.work/size (:size entry)}) (range n))))
     (initialize! conn)
     (@#'journal/fixture
      (fn [control value]
        (let [value (streams/descriptor (assoc (dissoc value :id) :policy (hasch/uuid inputs/default-policy)))
              inventory (changes/inventory @conn inputs/default-policy)]
          ;; Pure journal/planner fixture: no HOST/native pin authority claim.
          (streams/discover! control value nil
            {:before nil :after (:basis inventory)
             :changes (mapv (fn [[path metadata]] {:path path :kind :added :before nil :after metadata}) (:files inventory))}
            (random-uuid))
          (f conn value (streams/stream @control (:id value)) (streams/pending-items @control (:id value))))))))))

(defn- select [db value expected pending & [policy reservation]]
  (batches/selection db value expected pending inputs/default-policy (or policy batches/default-policy) (or reservation reserved)))

(deftest large-inventory-selects-finite-prefixes-without-canonical-body-projection
  (fixture 250
    (fn [conn value expected pending]
      (let [db @conn]
        (with-redefs [view/selection (fn [& _] (throw (AssertionError. "No canonical 200-document selection")))
                      content/read-by-id (fn [& _] (throw (AssertionError. "Metadata only")))]
          (loop [pending pending total 0 sizes []]
            (let [selected (select db value expected pending)]
              (if (= :empty (:status selected))
                (do (is (= 250 total)) (is (= [32 32 32 32 32 32 32 26] sizes)))
                (let [items (get-in selected [:batch :items])]
                  (is (= (- (count pending) (count items)) (get-in selected [:batch :remaining])))
                  (recur (vec (drop (count items) pending)) (+ total (count items)) (conj sizes (count items))))))))
        (let [selected (select db value expected pending)
              frozen (batches/capture db selected)]
          (is (= 34 (count (get-in frozen [:captured :inputs]))))
          (is (= (:captured frozen) (inputs/from-inputs inputs/default-policy (get-in frozen [:captured :inputs]))))
          (is (= frozen (batches/verify frozen)))
          (repo/write! conn "inbox/000.md" (markdown/source-bytes "Later source"))
          (is (= :source-changed (reason #(batches/capture @conn selected))))
          (is (= "Evidence" (get-in frozen [:captured :inputs "/in/knowledge/sources/inbox/000.md"]))))))))

(deftest contexts-and-workflow-fixtures-have-independent-explicit-budgets
  (fixture 40
    (fn [conn value expected pending]
      (is (= :context-exceeds-budget
             (reason #(select @conn value expected pending (assoc batches/default-policy :max-context-files 0)))))
      (let [no-context (select @conn value expected pending (assoc batches/default-policy :context-paths []))
            constrained (select @conn value expected pending batches/default-policy {:files 68 :bytes 0})]
        (is (not-any? #(= :context (:role %)) (get-in no-context [:batch :evidence :files])))
        (is (= 30 (count (get-in constrained [:batch :items])))))
      (is (= :context-unavailable
             (reason #(select @conn value expected pending (assoc batches/default-policy :context-paths ["wiki/missing.md"]))))))))

(deftest oversized-head-and-stale-pending-items-hold-before-body-reads
  (fixture 2
    (fn [conn value expected pending]
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "No bodies")))]
        (is (= :head-item-exceeds-budget
               (reason #(select @conn value expected pending (assoc batches/default-policy :max-trigger-bytes 1)))))
        (is (= :invalid-pending-order (reason #(select @conn value expected (vec (reverse pending))))))
        (is (= :invalid-pending-item
               (reason #(select @conn value expected (update-in pending [0 :knowledge.item/size] inc)))))))))

(deftest frozen-batch-identity-does-not-accept-tampered-selection-or-bytes
  (fixture 2
    (fn [conn value expected pending]
      (let [frozen (batches/capture @conn (select @conn value expected pending))]
        (is (= :invalid-envelope (reason #(batches/verify (assoc-in frozen [:batch :remaining] 99)))))
        (is (thrown? Exception
                     (batches/verify (assoc-in frozen [:captured :inputs "/in/knowledge/sources/inbox/000.md"] "Forged"))))
        (is (thrown? Exception (batches/verify (update-in frozen [:batch :items] pop))))
        (doseq [mutate [#(assoc-in % [:expected :knowledge.stream/active-batch] (random-uuid))
                        #(assoc % :remaining streams/max-changes)
                        #(assoc-in % [:items 0 :order] (get-in % [:expected :knowledge.stream/next-order]))]]
          (let [changed (mutate (:batch frozen))
                changed (assoc changed :id (hasch/uuid [:simmis/knowledge-batch-selection-v2 (dissoc changed :id)]))]
            (is (thrown? Exception (batches/verify (assoc frozen :batch changed))))))))))

(deftest an-oversized-later-item-does-not-block-the-valid-prefix
  (fixture 2
    #(repo/write! % "inbox/001.md" (byte-array 614400))
    (fn [conn value expected pending]
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "No bodies")))]
        (let [selected (select @conn value expected pending)]
          (is (= ["inbox/000.md"] (mapv :path (get-in selected [:batch :items]))))
          (is (= 1 (get-in selected [:batch :remaining]))))
        (is (= :head-item-exceeds-budget
               (reason #(select @conn value expected (subvec pending 1)))))))))

(deftest total-byte-budget-reserves-ordinary-workflow-inputs
  (fixture 40
    (fn [conn value expected pending]
      (let [reservation {:files 0 :bytes (- 2097152 2000)}
            selected (select @conn value expected pending batches/default-policy reservation)
            frozen (batches/capture @conn selected)
            size (reduce + 0 (map #(alength (.getBytes ^String % "UTF-8")) (vals (get-in frozen [:captured :inputs]))))]
        (is (< 0 (count (get-in selected [:batch :items])) 32))
        (is (<= (+ size (:bytes reservation)) 2097152)))
      (is (= :execution-envelope-exceeds-budget
             (reason #(select @conn value expected pending batches/default-policy {:files 0 :bytes 2097151})))))))
