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
    (is (= (assoc draft :generation 1 :target-stale? true) retained))
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
