(ns is.simm.uis.web.desktop.memory-draft-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.memory-editor :as editor]))

(deftest refreshing-a-reverted-draft-does-not-hide-a-successful-pending-save
  (let [file {:path "a.md" :entry {:content :a} :text "A"}
        opened (editor/draft file)
        typed (editor/changed opened "B" {:anchor 1 :head 1})
        reverted (editor/changed typed "A" {:anchor 1 :head 1})
        refreshed (editor/loaded reverted file)
        ack (editor/saved-at-generation refreshed (:generation typed) "B" {:entry {:content :b}})]
    (is (= (:generation typed) (:generation refreshed)))
    (is (= "A" (:text ack)))
    (is (= "B" (get-in ack [:source :text])))
    (is (= {:content :b} (get-in ack [:source :entry])))
    (is (editor/dirty? ack))))

(deftest unknown-mixed-yaml-keys-remain-displayable
  (let [properties {"title" "Readable" 7 "numeric key" false "boolean key" {"custom" "key"} "structured key"}
        entries (editor/property-entries properties)]
    (is (= properties (into {} entries)))
    (is (= 4 (count entries)))
    (is (every? string? (map (comp pr-str key) entries)))))

(deftest code-views-follow-user-preference-with-a-local-override
  (is (= :clojure (editor/code-view {} nil)))
  (is (= :superficie (editor/code-view {} :superficie)))
  (is (= :clojure (editor/code-view {:code-view :clojure} :superficie)))
  (is (= :superficie (editor/code-view {:code-view :superficie} :clojure))))

(deftest repl-syntax-buffers-and-receipts-do-not-rewrite-file-drafts
  (let [state {:drafts {"a.clj" {:text ";; original\n(+ 1 2)"}}
               :repl {:inputs {:clojure {:text "(+ 1 2)"} :superficie {:text "1 + 2"}}}}
        updated (reduce editor/repl-result state (map #(hash-map :id %) (range 25)))]
    (is (= :superficie (editor/repl-syntax state :superficie)))
    (is (= :clojure (editor/repl-syntax (assoc-in state [:repl :syntax] :clojure) :superficie)))
    (is (= "(+ 1 2)" (editor/repl-input state :clojure)))
    (is (= "1 + 2" (editor/repl-input state :superficie)))
    (is (= (:drafts state) (:drafts updated)))
    (is (= 20 (count (get-in updated [:repl :history]))))
    (is (= 24 (get-in updated [:repl :history 0 :id])))))

(deftest evaluation-errors-are-inspectable-receipts-not-transport-refusals
  (let [receipt {:id (random-uuid) :target :human-sci :success false :error "Unresolved symbol"}]
    (is (editor/evaluation-receipt? :repl receipt))
    (is (editor/evaluation-receipt? [:evaluate "a.clj"] receipt))
    (is (not (editor/evaluation-receipt? [:save "a.clj"] receipt)))
    (is (not (editor/evaluation-receipt? :repl {:success false :error "Unauthorized"})))))

(deftest retargeting-retires-file-io-but-keeps-pending-human-evaluation-identities
  (let [slots {:repl :scratch [:evaluate "a.clj"] :file-code
               [:cell "a.md" :revision :span] :saved-cell [:file "a.md"] :body :metadata :discovery}
        state {:target-stamp (random-uuid) :requests slots :loading slots :errors slots}
        result (editor/observe-target state (random-uuid))]
    (doseq [key [:requests :loading :errors]]
      (is (= {:repl :scratch [:evaluate "a.clj"] :file-code [:cell "a.md" :revision :span] :saved-cell}
             (get result key))))))

(deftest repl-namespace-follows-evaluation-unless-the-user-changed-the-target
  (let [receipt {:id (random-uuid) :requested-namespace "user" :namespace "research.new"}]
    (is (= "research.new" (get-in (editor/repl-result {} receipt) [:repl :namespace])))
    (is (= "my.other" (get-in (editor/repl-result {:repl {:namespace "my.other"}} receipt) [:repl :namespace])))))

(deftest remote-deletion-preserves-every-editing-state-and-never-becomes-a-new-file
  (let [file {:path "a.md" :entry {:content :a} :text "Saved"}
        d (-> (editor/draft file)
              (editor/changed "My draft" {:anchor 2 :head 4})
              (assoc :office {:text "edited XML" :saved-text "original XML"}))
        missing (editor/observed-entry d nil)]
    (is (:remote-deleted? missing))
    (is (get-in missing [:incoming :deleted?]))
    (is (= d (select-keys missing (keys d))))
    (is (= missing (editor/use-incoming missing)))
    (is (= (:source d) (:source (editor/observed-entry (editor/draft file) nil))))
    (is (not (:remote-deleted? (editor/observed-entry missing (:entry file)))))
    (is (nil? (:incoming (editor/observed-entry missing (:entry file)))))))

(deftest file-evaluation-receipts-survive-typing-moving-and-bounded-history
  (let [receipt {:id (random-uuid) :target :human-sci :success false :code "(inc x)"}
        state {:drafts {"a.clj" {:text "newer text" :generation 2}}}
        result (editor/file-evaluation-result state "a.clj" 1 "(inc x)" receipt)]
    (is (= "newer text" (get-in result [:drafts "a.clj" :text])))
    (is (true? (get-in result [:evaluations "a.clj" :stale?])))
    (is (= "(inc x)" (get-in result [:evaluation-history 0 :input-text])))
    (let [moved (editor/file-evaluation-result (dissoc state :drafts) "a.clj" 1 "(inc x)" receipt)]
      (is (= (:id receipt) (get-in moved [:evaluation-history 0 :id])))
      (is (true? (get-in moved [:evaluations "a.clj" :stale?]))))
    (is (= 20 (count (:evaluation-history
                     (reduce (fn [s _] (editor/file-evaluation-result s "a.clj" 1 "(inc x)" receipt))
                             result (range 30))))))
    (let [many (reduce (fn [s n] (editor/file-evaluation-result s (str n ".clj") 1 "(inc x)" receipt))
                       state (range 30))]
      (is (= 20 (count (:evaluations many))))
      (is (= 20 (count (:evaluation-history many))))
      (is (not (contains? (:evaluations many) "0.clj")))
      (is (contains? (:evaluations many) "29.clj")))))

(deftest metadata-selection-recovers-an-initial-folder-failure-and-covers-pending-reads
  (is (= {:folders [""] :paths []}
         (editor/metadata-request {:root-expanded? true :errors {[:folder ""] "failed"}})))
  (is (= {:folders ["" "notes" "slow"] :paths ["a.clj" "b.clj"]}
         (editor/metadata-request {:root-expanded? true :focus-folder "notes"
                                   :drafts {"a.clj" {}}
                                   :loading {[:folder "slow"] true [:file "b.clj"] true
                                             [:folder "old"] false}}))))
