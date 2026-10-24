(ns is.simm.uis.web.desktop.memory-cells-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.cells-test :as fixture]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.uis.web.desktop.memory-cells :as cells]
            [is.simm.uis.web.desktop.memory-editor :as editor]))

(deftest autosave-refreshes-saved-cell-addresses-without-replacing-the-rich-baseline
  (let [text "> opaque\n\nProse\n\n```clj\n(+ 1 2)\n```\n"
        stamp (random-uuid) file (assoc (fixture/saved text) :target-stamp stamp)
        draft (editor/draft file) path (:path file)
        model (assoc-in (get-in draft [:rich-base :document :model]) [:content 1 :content 0 :content 0 :text] "Longer prose")
        edited (editor/document-changed draft (get-in draft [:rich-base :id]) model {})
        sent (:text edited) analysis (files/analyze (:repository-id file) path sent)
        entry (assoc (:entry file) :content (markdown/revision sent) :size (count sent))
        saved-first (-> edited (editor/saved sent {:entry entry}) (editor/analyzed sent analysis))
        analyzed-first (-> edited (editor/analyzed sent analysis) (editor/saved sent {:entry entry}))
        state #(hash-map :target-verified? true :target-stamp stamp :drafts {path %})]
    (is (nil? (cells/visible (state edited) path)))
    (doseq [d [saved-first analyzed-first]]
      (is (= (:rich-base edited) (:rich-base d)) "Keep native undo/selection baseline across save")
      (let [[cell] (:cells (cells/visible (state d) path)) request (cells/request (state d) path (:key cell))]
        (is (some? request))
        (is (= entry (:expected request)))
        (is (= (:revision analysis) (:revision request)))
        (is (= "```clj\n(+ 1 2)\n```" (subs sent (:from request) (:to request))))))
    (is (nil? (cells/visible (assoc (state saved-first) :target-verified? false) path)))
    (is (nil? (cells/visible (state (assoc saved-first :target-stale? true)) path)))
    (is (nil? (cells/visible (state (assoc saved-first :incoming-entry {:content (random-uuid)})) path)))))

(deftest old-cell-status-and-provenance-are-never-attributed-to-a-new-saved-cell
  (let [stamp (random-uuid) file (assoc (fixture/saved "```clj\n(+ 1 2)\n```\n") :target-stamp stamp)
        draft (editor/draft file) path (:path file) revision (get-in file [:entry :content])
        receipt {:source {:kind :saved-markdown-cell :path path :entry (:entry file)
                          :repository-id (:repository-id file) :target-stamp stamp}}
        state {:target-verified? true :target-stamp stamp :drafts {path draft}
               :loading {[:cell path (random-uuid) "cell:0:22"] true}}]
    (is (empty? (:running (cells/visible state path))))
    (is (= :saved-current (cells/receipt-status state receipt)))
    (is (= :draft-changed (cells/receipt-status (assoc-in state [:drafts path :text] "new draft") receipt)))
    (is (= :saved-changed (cells/receipt-status (assoc-in state [:drafts path :source :entry :content] (random-uuid)) receipt)))
    (is (= :unverified (cells/receipt-status (dissoc state :drafts) receipt)))
    (is (= ["cell:0:22"] (:running (cells/visible (assoc state :loading {[:cell path revision "cell:0:22"] true}) path))))))
