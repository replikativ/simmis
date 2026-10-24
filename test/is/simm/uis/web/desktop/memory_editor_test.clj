(ns is.simm.uis.web.desktop.memory-editor-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.memory-editor :as editor]))

(def file {:path "src/example.clj" :kind :text :entry {:content :one} :text "opened"})

(deftest selective-office-corrections-retain-edits-and-reject-stale-targets
  (is (nil? (editor/office-corrected nil 0 {} {})))
  (let [stamp (random-uuid) token (random-uuid)
        source {:path "report.docx" :kind :office :entry {:content :one} :text nil :target-stamp stamp}
        correction {:path "report.docx" :token token :original "Original" :text "Edited" :entry (:entry source)}
        draft (assoc (editor/draft source) :office-correction correction
                     :office {:text "XML" :saved-text "XML" :reading-entry (:entry source)
                              :reading {:corrections {:tokens {4 token}}}})
        incoming (assoc source :entry {:content :two})
        reply {:path "report.docx" :entry {:content :saved} :commit :saved :target-stamp stamp}]
    (is (editor/dirty? draft))
    (is (editor/office-correction-current? draft))
    (is (= correction (:office-correction (editor/loaded draft incoming))))
    (is (not (editor/office-correction-current? (editor/loaded draft incoming))))
    (is (not (editor/office-correction-current? (assoc-in draft [:source :path] "renamed.docx"))))
    (let [saved (editor/office-corrected draft (:generation draft) correction reply)]
      (is (nil? (:office saved)))
      (is (nil? (:office-correction saved)))
      (is (= (:entry reply) (get-in saved [:source :entry]))))
    (doseq [incoming-state [{:incoming (assoc source :entry {:content :foreign})}
                            {:incoming-entry {:content :foreign}}]]
      (let [saved (editor/office-corrected (merge draft incoming-state) (:generation draft) correction reply)]
        (is (= incoming-state (select-keys saved (keys incoming-state))))))
    (doseq [newer [(assoc-in draft [:office-correction :text] "Later typing")
                   (assoc-in draft [:office :text] "New XML")
                   (update draft :generation inc)]]
      (let [saved (editor/office-corrected newer (:generation draft) correction reply)]
        (is (= (:office-correction newer) (:office-correction saved)))
        (is (= (:office newer) (:office saved)))
        (is (= (:entry source) (get-in saved [:source :entry])))
        (is (= reply (:earlier-save-receipt saved)))))))

(deftest saved-office-previews-do-not-rebase-or-overwrite-xml-drafts
  (let [source {:path "report.docx" :kind :office :entry {:content :one} :text nil}
        draft (assoc (editor/draft source) :office-mode :xml
                     :office {:part "word/document.xml" :text "Human XML" :saved-text "Saved XML"
                              :selection {:anchor 4 :head 8} :reading-entry (:entry source)})
        reply {:entry {:content :two} :text "Different XML" :part "another.xml"
               :reading {:status :ready :blocks []}}
        updated (editor/office-loaded draft reply true)]
    (is (= (:source draft) (:source updated)))
    (is (= (select-keys (:office draft) [:part :text :saved-text :selection])
           (select-keys (:office updated) [:part :text :saved-text :selection])))
    (is (= :xml (:office-mode updated)))
    (is (= (:entry reply) (get-in updated [:office :reading-entry])))
    (let [initial (editor/office-loaded (editor/draft source) reply true)]
      (is (= "Different XML" (get-in initial [:office :text])))
      (is (= "another.xml" (get-in initial [:office :part])))
      (is (not (editor/dirty? initial))))
    (is (editor/dirty? updated))
    (is (= {:content :two} (editor/office-reading-entry (assoc draft :incoming-entry {:content :two}))))
    (doseq [blocked [{:remote-deleted? true} {:incoming-missing? true} {:target-stale? true} {:incoming {:deleted? true}}]]
      (is (nil? (editor/office-reading-entry (merge draft blocked))))))
  (let [source {:path "report.docx" :kind :office :entry {:content :one} :text nil}
        draft (assoc (editor/draft source) :office-mode :xml
                     :office {:text "Saved" :saved-text "Saved"})
        loaded (editor/loaded draft (assoc source :entry {:content :two}))]
    (is (= :xml (:office-mode loaded)) "A clean metadata refresh retains view preference")
    (is (nil? (:office loaded)) "A new file revision never retains the old preview as current")))

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

(deftest property-controls-use-host-text-and-preserve-stale-drafts
  (let [model {:path "wiki/company.md" :target {:kind :page} :expected {:content :one}
               :revision (random-uuid) :scope-revision (random-uuid) :target-stamp (random-uuid)}
        field {:value 9007199254740992 :input {:text "9007199254740993"}}
        pd (editor/property-draft model {:text "9007199254740995"})]
    (is (= {:text "9007199254740993"} (editor/property-input model field nil)))
    (is (= {:text "9007199254740995"} (editor/property-input model field pd)))
    (is (editor/property-draft-current? model pd))
    (doseq [key [:expected :revision :scope-revision :target-stamp]]
      (is (not (editor/property-draft-current? (assoc model key (random-uuid)) pd))))))

(deftest property-publication-never-silently-rebases-concurrent-source-typing
  (let [opened (editor/draft file)
        reply {:entry {:content :two} :text "Property changed" :analysis {:revision :new}}
        clean (editor/property-saved opened (:generation opened) reply)
        typed (editor/changed opened "Typed during property save" {:anchor 4 :head 4})
        dirty (editor/property-saved typed (:generation opened) reply)
        newer (assoc typed :generation 9)
        late (editor/property-saved newer (:generation opened) reply)]
    (is (= "Property changed" (:text clean)))
    (is (= (:entry reply) (get-in clean [:source :entry])))
    (is (not (editor/dirty? clean)))
    (is (= "Typed during property save" (:text dirty)))
    (is (= (:entry file) (get-in dirty [:source :entry])))
    (is (= "Property changed" (get-in dirty [:incoming :text])))
    (is (= reply (:property-save-receipt dirty)))
    (is (= (dissoc late :earlier-save-receipt) newer))
    (is (= reply (:earlier-save-receipt late)))))

(deftest property-receipts-preserve-newer-incoming-and-deletion-evidence
  (let [reply {:entry {:content :property} :text "Property changed" :target-stamp nil}
        newer (assoc file :entry {:content :newer} :text "Human changed after property")]
    (is (nil? (editor/property-saved nil 0 reply)))
    (doseq [typed? [true false]
            evidence [{:incoming newer} {:incoming-entry {:content :newer}}
                      {:incoming-entry nil :remote-deleted? true}]]
      (let [draft (merge (editor/draft file) evidence)
            draft (if typed? (editor/changed draft "Human typing" nil) draft)
            saved (editor/property-saved draft (:generation draft) reply)]
        (is (= evidence (select-keys saved (keys evidence))))
        (when typed?
          (is (= (:entry file) (get-in saved [:source :entry])))
          (is (= "Human typing" (:text saved))))))))

(deftest property-publication-rebuilds-only-a-clean-rich-baseline
  (let [model {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text "Company"}]}]}
        analysis {:revision :before :semantic {:status :ready} :slots [{:from 0 :to 7 :model model}]}
        opened (editor/draft {:path "wiki/company.md" :kind :markdown :entry {:content :before}
                              :text "Company" :analysis analysis})
        text "---\nrevenue: 42\n---\nCompany"
        updated (assoc analysis :revision :after :slots [{:from 20 :to (count text) :model model}])
        reply {:entry {:content :after} :text text :analysis updated}
        saved (editor/property-saved opened (:generation opened) reply)
        typing (assoc opened :text "Human typing")
        concurrent (editor/property-saved typing (:generation opened) reply)]
    (is (= :rich (:mode saved)))
    (is (not (editor/dirty? saved)))
    (is (= text (get-in saved [:rich-base :text])))
    (is (= updated (get-in saved [:rich-base :analysis])))
    (is (= {20 model} (get-in saved [:rich-base :models])))
    (is (not= (get-in opened [:rich-base :id]) (get-in saved [:rich-base :id])))
    (is (= "Human typing" (:text concurrent)))
    (is (= (:rich-base opened) (:rich-base concurrent)))
    (is (= text (get-in concurrent [:incoming :text])))))

(deftest bundled-page-properties-follow-only-the-accepted-source-entry
  (let [stamp (random-uuid) target {:kind :page}
        source {:entry {:content (random-uuid)} :target-stamp stamp
                :page-properties {:path "a.md" :target target :fields []}}
        state {:connection :socket :target-stamp stamp :read-epoch 2
               :drafts {"a.md" {:source source}}}
        installed (editor/install-page-properties state "a.md" source)
        scope ["a.md" target]]
    (is (= (editor/property-marker state "a.md" target)
           (get-in installed [:property-models scope :request-marker])))
    (is (= stamp (get-in installed [:property-models scope :target-stamp])))
    (is (= state (editor/install-page-properties state "a.md" (assoc source :entry {:content (random-uuid)}))))
    (is (= state (editor/install-page-properties state "a.md" (dissoc source :page-properties))))))
