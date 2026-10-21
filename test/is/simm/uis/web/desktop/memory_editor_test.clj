(ns is.simm.uis.web.desktop.memory-editor-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.memory-editor :as editor]))

(def file {:path "src/example.clj" :kind :text :entry {:content :one} :text "opened"})

(deftest incoming-files-cannot-overwrite-local-work
  (let [draft (editor/changed (editor/draft file) "typing" {:anchor 3 :head 3})
        incoming (assoc file :entry {:content :two} :text "agent edit")
        next (editor/loaded draft incoming)]
    (is (= "typing" (:text next)))
    (is (= incoming (:incoming next)))
    (is (= file (:source next)))
    (is (= draft (editor/loaded draft file))))
  (let [draft (assoc (editor/draft file) :rich-edit {:model {}})]
    (is (= draft (editor/loaded draft file)))
    (is (:rich-edit (editor/loaded draft (assoc file :entry {:content :two}))))))

(deftest an-acknowledgement-advances-only-the-sent-baseline
  (let [draft (editor/changed (editor/draft file) "typed after send" {:anchor 16 :head 16})
        next (editor/saved draft "sent" {:entry {:content :two} :commit "commit"})]
    (is (= "typed after send" (:text next)))
    (is (= "sent" (get-in next [:source :text])))
    (is (= {:content :two} (get-in next [:source :entry])))
    (is (editor/dirty? next))
    (is (= (:selection draft) (:selection next)))))

(deftest stale-analysis-and-stale-requests-do-not-win
  (let [draft (editor/changed (editor/draft file) "new text" {:anchor 0 :head 0})]
    (is (= draft (editor/analyzed draft "opened" {:revision :old})))
    (is (= {:revision :new} (:analysis (editor/analyzed draft "new text" {:revision :new})))))
  (is (editor/response-current? {:requests {:file "new"}} :file "new"))
  (is (not (editor/response-current? {:requests {:file "new"}} :file "old"))))

(deftest an-older-read-cannot-replace-a-new-save
  (let [opened (editor/draft file)
        saved (editor/saved (assoc opened :text "saved") "saved" {:entry {:content :two}})]
    (is (= saved (editor/loaded-at-generation saved (:generation opened) file)))
    (is (= "saved" (:text saved)))))

(deftest moving-a-saved-file-retains-new-typing-and-rich-drafts
  (let [typed (assoc (editor/changed (editor/draft file) "during move" {:anchor 4 :head 4})
                     :rich-edit {:token "region" :model {:type "doc"}})
        moved (editor/moved typed {:path "src/moved.clj" :entry {:content :one}})]
    (is (= "during move" (:text moved)))
    (is (= (:rich-edit typed) (:rich-edit moved)))
    (is (= "src/moved.clj" (get-in moved [:source :path])))
    (is (editor/dirty? moved))))

(deftest a-save-does-not-hide-an-incompatible-incoming-version
  (let [draft (assoc (editor/draft file) :incoming (assoc file :entry {:content :three}))]
    (is (:incoming (editor/saved draft "saved" {:entry {:content :two}})))
    (is (nil? (:incoming (editor/saved draft "saved" {:entry {:content :three}}))))))

(deftest explicit-replacements-advance-generations-and-fence-earlier-saves
  (let [before (assoc (editor/draft file) :generation 5 :incoming (assoc file :entry {:content :new} :text "new"))
        selected (editor/use-incoming before)
        late-ack (editor/saved-at-generation selected 5 "old save" {:entry {:content :old-save}})]
    (is (= 6 (:generation selected)))
    (is (= "new" (:text late-ack)))
    (is (= {:content :new} (get-in late-ack [:source :entry])))
    (is (:earlier-save-receipt late-ack))))

(deftest an-empty-new-file-still-needs-saving
  (is (editor/dirty? (editor/draft {:path "empty.clj" :entry nil :text ""})))
  (is (not (editor/dirty? nil))))
