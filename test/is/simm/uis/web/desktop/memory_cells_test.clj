(ns is.simm.uis.web.desktop.memory-cells-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.cells-test :as fixture]
            [is.simm.documents.chart-contract :as chart-contract]
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
    (doseq [d [saved-first analyzed-first
               (editor/loaded saved-first (assoc file :entry entry :text sent :analysis analysis))]]
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

(defn- result-state []
  (let [stamp (random-uuid)
        file (assoc (fixture/saved "```clj\n(+ 1 2)\n```\n") :target-stamp stamp)
        path (:path file)
        receipt {:id :chart :success true :presentation {:kind :chart}
                 :source {:kind :saved-markdown-cell :path path :entry (:entry file)
                          :repository-id (:repository-id file) :target-stamp stamp}}]
    {:path path :receipt receipt
     :state {:target-verified? true :target-stamp stamp :drafts {path (editor/draft file)}
             :repl {:history [receipt]}}}))

(deftest only-the-newest-eligible-cell-result-reserves-one-chart-slot
  (let [{:keys [state path receipt]} (result-state)
        budget #(cells/chart-budget (assoc-in state [:repl :history] %) path)
        available chart-contract/max-mounted]
    (is (= (:id receipt) (:presentation-id (cells/visible state path))))
    (is (= (dec available) (budget [receipt])))
    (is (= (dec available) (budget (mapv #(assoc receipt :id %) (range 20))))
        "Retained chart history never reserves twenty mounts")
    (doseq [newest [(assoc receipt :id :text :presentation nil)
                    (assoc receipt :id :table :presentation {:kind :table})
                    (assoc receipt :id :failed :success false)
                    (assoc receipt :id :partial :partial? true)]]
      (is (= available (budget [newest receipt]))
          "An older chart is not promoted past a newer eligible receipt"))
    (is (= (dec available)
           (budget [(assoc-in receipt [:source :entry :content] (random-uuid)) receipt]))
        "A receipt for another saved version is not eligible")
    (doseq [mode [:source :outline]]
      (is (= available (cells/chart-budget (assoc-in state [:drafts path :mode] mode) path))))))

(deftest stale-or-unverified-cell-results-never-reserve-presentation-mounts
  (let [{:keys [state path]} (result-state)]
    (doseq [stale [(assoc state :target-verified? false)
                   (assoc state :target-stamp (random-uuid))
                   (assoc-in state [:drafts path :target-stale?] true)
                   (assoc-in state [:drafts path :remote-deleted?] true)
                   (assoc-in state [:drafts path :incoming-entry] {:content (random-uuid)})
                   (assoc-in state [:drafts path :text] "Unsaved human draft")
                   (assoc-in state [:drafts path :source :entry :content] (random-uuid))]]
      (is (nil? (:presentation-id (cells/visible stale path))))
      (is (= chart-contract/max-mounted (cells/chart-budget stale path))))))

(deftest same-file-observation-keeps-native-owner-and-foreign-identities-do-not
  (let [text "Prose\n" stamp (random-uuid)
        file (assoc (fixture/saved text) :target-stamp stamp)
        opened (editor/draft file)
        retained (assoc opened :generation 3 :incoming-entry {:content :newer}
                              :rich-base (assoc (:rich-base opened) :document-selection {:from 2 :to 2}))
        refreshed (editor/loaded retained file)]
    (is (identical? (:rich-base retained) (:rich-base refreshed)))
    (is (= 3 (:generation refreshed)))
    (is (= {:content :newer} (:incoming-entry refreshed)))
    (doseq [different [(assoc file :repository-id (random-uuid))
                       (assoc file :path "wiki/other.md")
                       (assoc file :target-stamp (random-uuid))]]
      (is (not (identical? (:rich-base retained) (:rich-base (editor/loaded retained different))))))))
