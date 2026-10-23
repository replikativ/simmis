(ns is.simm.uis.web.desktop.memory-charts-test
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.documents.chart-contract :as contract]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-charts :as charts]))

(def block #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def stamp #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")
(def revision #uuid "dddddddd-dddd-4ddd-8ddd-dddddddddddd")
(def entry {:content revision :size 100 :mode 33188})
(def state
  {:connection :socket-1 :target-stamp stamp :target-verified? true :read-epoch 1 :definition-basis revision
   :drafts {"wiki/a.md"
            {:generation 0 :mode :rich :source
             {:kind :markdown :entry entry :target-stamp stamp
              :analysis {:identity-status :explicit :revision revision :diagnostics []
                         :frontmatter {:properties {"simmis" {"blocks" {(str block) {"types" [(str contract/type-id)]}}}}}
                         :semantic {:blocks [{:id block :identity-status :explicit :children []}]}}}}}})
(def reply
  {:status :ready :path "wiki/a.md" :block block :expected entry :revision revision
   :target-stamp stamp :scope-revision revision :feature {:profile contract/profile :digest revision}
   :spec-path "data/sales.json" :spec-entry entry
   :chart {:profile contract/data-profile :kind :line :series [{:label "A" :points [[0 1]]}]}})

(deftest saved-explicit-type-is-only-a-discovery-hint
  (is (= [block] (charts/blocks (get-in state [:drafts "wiki/a.md"])) ))
  (doseq [path [[:source :entry] [:source :analysis :frontmatter] [:source :analysis :semantic]]]
    (is (empty? (charts/blocks (assoc-in (get-in state [:drafts "wiki/a.md"]) path nil)))))
  (is (empty? (charts/blocks (assoc-in (get-in state [:drafts "wiki/a.md"])
                                      [:source :analysis :identity-status] :provisional)))))

(deftest socket-target-source-generation-and-cross-file-commit-fences
  (let [marker (charts/marker state "wiki/a.md" block)
        populated (assoc-in state [:charts ["wiki/a.md" block]] {:marker marker :reply reply})]
    (is (charts/reply-current? state "wiki/a.md" block marker reply))
    (is (= {(str block) (:chart reply)} (charts/visible populated "wiki/a.md")))
    (doseq [changed [(assoc populated :connection :socket-2)
                     (assoc populated :target-stamp revision)
                     ;; A SPEC-ONLY room commit advances this same epoch even
                     ;; when the JSON file has never been opened in a buffer.
                     (update populated :read-epoch inc)
                     (assoc populated :definition-basis stamp)
                     (assoc-in populated [:drafts "wiki/a.md" :source :entry] nil)
                     (assoc-in populated [:drafts "wiki/a.md" :source :analysis :revision] stamp)]]
      (is (not (charts/reply-current? changed "wiki/a.md" block marker reply)))
      (is (empty? (charts/visible changed "wiki/a.md"))))
    (is (empty? (charts/visible (assoc populated :target-verified? false) "wiki/a.md")))
    (is (empty? (charts/visible (assoc-in populated [:drafts "wiki/a.md" :mode] :source) "wiki/a.md")))
    (is (empty? (charts/visible (assoc-in populated [:drafts "wiki/a.md" :local-source] {}) "wiki/a.md")))
    (let [typing (update-in populated [:drafts "wiki/a.md" :generation] inc)]
      (is (not (charts/reply-current? typing "wiki/a.md" block marker reply)))
      (is (= {(str block) (:chart reply)} (charts/visible typing "wiki/a.md"))
          "continuous prose edits keep the already accepted saved projection"))
    (is (empty? (charts/visible (dissoc populated :charts) "wiki/a.md")))
    (is (= state (dissoc populated :charts)) "selection never mutates another panel's state")))

(deftest autosave-analysis-is-bound-to-saved-bytes-without-replacing-current-typing
  (let [old (get-in state [:drafts "wiki/a.md"])
        analysis (get-in old [:source :analysis])
        newer (assoc analysis :revision stamp)
        saved (-> old (assoc :text "saved prose" :analysis-text "old prose" :analysis analysis
                             :rich-base {:id :original-editor} :selection {:anchor 7 :head 7})
                  (assoc-in [:source :text] "saved prose")
                  (assoc-in [:source :entry :content] stamp))
        typing (assoc saved :text "new typing")
        received (editor/analyzed typing "saved prose" newer)]
    (is (nil? (charts/saved-analysis saved)) "a new content identity cannot reuse the old analysis")
    (is (= newer (charts/saved-analysis received)))
    (is (= (dissoc typing :source) (dissoc received :source))
        "baseline-only analysis leaves prose, analysis, selection and native rich-base alone")
    (is (= [block] (charts/blocks received)))
    (is (= typing (editor/analyzed typing "older baseline" newer)))
    (is (= typing (editor/analyzed typing "saved prose" analysis))
        "matching text without matching content revision does not attach stale analysis")
    (let [property-saved (assoc saved :analysis newer :analysis-text "saved prose")]
      (is (= newer (charts/saved-analysis property-saved)) "property receipts already have matching analysis"))
    (let [new-declarations (assoc newer :frontmatter {:properties {"simmis" {"blocks" {(str block) {"types" [(str contract/type-id)]}}}}})
          previously-plain (assoc-in saved [:source :analysis :frontmatter] nil)
          analyzed (editor/analyzed previously-plain "saved prose" new-declarations)]
      (is (= [block] (charts/blocks analyzed)) "newly saved declarations discover from fresh analysis"))))
