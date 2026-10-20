(ns is.simm.uis.web.desktop.memory-target-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.uis.web.desktop.memory-editor :as editor]))

(defn- file [stamp]
  {:path "wiki/page.md" :kind :text :entry {:content :identical} :text "Saved"
   :target-stamp stamp})

(deftest identical-bytes-in-another-target-are-not-the-same-document
  (let [a (random-uuid) b (random-uuid)
        opened (editor/draft (file a))
        local (assoc opened :text "My work")
        incoming (editor/loaded local (file b))
        clean (editor/loaded opened (file b))]
    (is (= "My work" (:text incoming)))
    (is (= (file a) (:source incoming)))
    (is (= (file b) (:incoming incoming)))
    (is (true? (:target-stale? incoming)))
    (is (= b (get-in clean [:source :target-stamp])))
    (is (= 1 (:generation clean)))
    (is (= opened (editor/loaded opened (file a))))))

(deftest a-target-change-invalidates-file-requests-without-losing-human-models
  (let [a (random-uuid) b (random-uuid)
        draft (assoc (editor/draft (file a)) :text "Local draft"
                     :office {:text "Local XML" :saved-text "Old XML"}
                     :rich-base {:models {0 "Local rich model"}})
        state {:target-stamp a :read-epoch 4 :drafts {"wiki/page.md" draft}
               :requests {[:save "wiki/page.md"] :old-save :repl :ongoing-repl}
               :loading {[:save "wiki/page.md"] true :repl true}
               :errors {:repl "A REPL diagnostic" :metadata "Old error"}
               :repl {:history [:existing] :inputs {:clojure {:text "(+ 1 2)"}}}
               :selected "wiki/page.md" :folders {"" {:entries [:old]}}
               :observed-entries {"wiki/page.md" (:entry (file a))}
               :history {"wiki/page.md" [:old]} :history-preview {:old true}
               :search {:entries [:old]} :download {:base64 "old"}
               :workspace {:old true} :permissions {:write? true}
               :sync {:attempted :old} :watch-connection :old}
        next (editor/observe-target state b)
        retained (get-in next [:drafts "wiki/page.md"])]
    (is (= 5 (:read-epoch next)))
    (is (= b (:target-stamp next)))
    (is (= (assoc draft :generation 1 :target-stale? true :current-target-stamp b) retained))
    (is (= (:repl state) (:repl next)))
    (is (= {:repl :ongoing-repl} (:requests next)))
    (is (= {:repl true} (:loading next)))
    (is (= {:repl "A REPL diagnostic"} (:errors next)))
    (is (not (editor/response-current? next [:save "wiki/page.md"] :old-save)))
    (is (editor/response-current? next :repl :ongoing-repl))
    (is (not-any? #(contains? next %)
                  [:folders :workspace :observed-entries :history :history-preview
                   :search :download :permissions :watch-connection :sync]))
    (is (= next (editor/observe-target next b)))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (editor/observe-target state nil)))))

(deftest late-save-cannot-rebaseline-another-target-or-clear-its-conflict
  (let [a (random-uuid) b (random-uuid)
        d (assoc (editor/draft (file a)) :text "Typed")
        old-reply {:entry (:entry (file a)) :target-stamp a}
        next (get-in (editor/observe-target {:target-stamp a :drafts {"wiki/page.md" d}} b)
                     [:drafts "wiki/page.md"])
        late (editor/saved-at-generation next 0 "Sent" old-reply)
        wrong (editor/saved-at-generation d 0 "Sent" (assoc old-reply :target-stamp b))
        conflict (editor/loaded d (file b))
        original-fact (editor/saved conflict "Actually saved in A" old-reply)]
    (is (= (:source d) (:source late) (:source wrong)))
    (is (= old-reply (:earlier-save-receipt late)))
    (is (= b (get-in wrong [:earlier-save-receipt :target-stamp])))
    (is (= (file b) (:incoming original-fact)))
    (is (= "Actually saved in A" (get-in original-fact [:source :text])))
    (is (true? (:target-stale? original-fact)))))

(deftest stamped-identical-metadata-retains-cross-target-incoming-and-human-models
  (let [a (random-uuid) b (random-uuid)
        d (assoc (editor/draft (file a)) :text "My work"
                 :rich-base {:models {0 "Rich work"}}
                 :office {:text "XML work" :saved-text "Old XML"})
        incoming (editor/loaded d (file b))
        observed (editor/observed-entry incoming (:entry (file b)) b)]
    (is (= (:source d) (:source observed)))
    (is (= (:text d) (:text observed)))
    (is (= (:rich-base d) (:rich-base observed)))
    (is (= (:office d) (:office observed)))
    (is (= (file b) (:incoming observed)))
    (is (= b (:incoming-target-stamp observed) (:observed-target-stamp observed)))
    (is (true? (:target-stale? observed)))
    (is (= observed (editor/observed-entry observed (:entry (file a)) a)))
    (is (= b (get-in (editor/use-incoming observed) [:source :target-stamp])))))

(deftest metadata-is-not-an-incoming-body-and-absence-in-b-does-not-delete-a
  (let [a (random-uuid) b (random-uuid)
        d (assoc (editor/draft (file a)) :text "Keep my source"
                 :rich-base {:models {0 "Keep rich"}}
                 :office {:text "Keep XML" :saved-text "Before"})
        current (get-in (editor/observe-target {:target-stamp a :drafts {"wiki/page.md" d}} b)
                        [:drafts "wiki/page.md"])
        present (editor/observed-entry current (:entry (file b)) b)
        absent (editor/observed-entry present nil b)]
    (doseq [observed [present absent]]
      (is (= (:source d) (:source observed)))
      (is (= "Keep my source" (:text observed)))
      (is (= (:office d) (:office observed)))
      (is (= (:rich-base d) (:rich-base observed)))
      (is (nil? (:incoming observed)))
      (is (= observed (editor/use-incoming observed)))
      (is (= observed (editor/observed-entry observed (:entry (file a)) a))))
    (is (contains? absent :incoming-entry))
    (is (nil? (:incoming-entry absent)))
    (is (true? (:incoming-missing? absent)))
    (is (not (:remote-deleted? absent)))
    (is (= b (:incoming-target-stamp absent)))))

(deftest changed-metadata-never-offers-an-outdated-incoming-body
  (let [a (random-uuid) b (random-uuid)
        d (assoc (editor/draft (file a)) :text "Local")
        incoming (editor/loaded d (file b))
        changed (editor/observed-entry incoming {:content :newer-in-b} b)
        deleted (editor/observed-entry incoming nil b)]
    (is (= {:content :newer-in-b} (:incoming-entry changed)))
    (is (nil? (:incoming changed)))
    (is (nil? (:incoming deleted)))
    (is (true? (:incoming-missing? deleted)))
    (is (= (:source d) (:source changed) (:source deleted)))))

(deftest explicit-controller-transition-allows-return-without-guessing-uuid-order
  (let [a (random-uuid) b (random-uuid)
        d (assoc (editor/draft (file a)) :text "Local")
        direct (editor/observed-entry d (:entry (file b)) b)
        returned (get-in (editor/observe-target {:target-stamp b :drafts {"wiki/page.md" direct}} a)
                         [:drafts "wiki/page.md"])
        accepted (editor/observed-entry returned (:entry (file a)) a)]
    (is (= direct (editor/observed-entry direct (:entry (file a)) a)))
    (is (= a (:current-target-stamp accepted) (:observed-target-stamp accepted)))
    (is (nil? (:incoming-target-stamp accepted)))
    (is (nil? (:incoming-entry accepted)))
    (is (= (:source d) (:source accepted)))
    (is (= "Local" (:text accepted)))))

(deftest stamped-same-target-metadata-keeps-the-legacy-conflict-behavior
  (let [a (random-uuid) d (assoc (editor/draft (file a)) :text "Local")]
    (doseq [entry [(:entry (file a)) {:content :changed} nil]]
      (is (= (editor/observed-entry d entry)
             (dissoc (editor/observed-entry d entry a) :observed-target-stamp))))
    (is (nil? (editor/observed-entry nil nil a)))
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
                 (editor/observed-entry d nil nil)))))
