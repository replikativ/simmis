(ns is.simm.uis.web.desktop.memory-render-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.documents.rich-text :as rich]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.views.memory :as memory]))

(defn- text-node [text] {:kind :Text :text text :children []})
(defn- paragraph [text] {:kind :Paragraph :children [(text-node text)]})
(defn- item [kind text & children]
  {:kind kind :children (into [(paragraph text)] children)})

(defn- draft [mode changed?]
  ;; These are the same inert Markdown syntax/model values the host analysis
  ;; supplies. No parser, backend, file writes, or editor bridge is mocked.
  (let [nodes [{:kind :Paragraph
                :children [(text-node (if changed? "Changed paragraph " "Original paragraph "))
                           {:kind :StrongEmphasis
                            :children [{:kind :Emphasis :children [(text-node "bold")]}]}
                           (text-node " and ")
                           {:kind :Emphasis :children [(text-node "italic")]}]}
               {:kind :BulletList
                :children [(item :BulletListItem "Explore the market"
                                 {:kind :BulletList
                                  :children [(item :BulletListItem "Record evidence and sources")
                                             (item :BulletListItem "Compare competing explanations")]})
                           (item :BulletListItem "Review proposals before adoption")]}
               {:kind :OrderedList :start 3
                :children [(item :OrderedListItem "Measure the workflow")
                           (item :OrderedListItem "Schedule the trusted version")]}]
        nodes (mapv (fn [index node] (assoc node :from index :to (inc index))) (range) nodes)
        analysis {:slots (mapv #(assoc (select-keys % [:from :to :kind]) :model (rich/model %)) nodes)
                  :semantic {:status :ready
                             :blocks (mapv (fn [index node]
                                             {:id (str "block-" index) :identity-status :provisional
                                              :from (:from node) :to (:to node)
                                              :syntax [node] :children []}) (range) nodes)}}]
    {:mode mode :text "retained source" :analysis analysis}))

(defn- tree [draft navigation-expanded?]
  (el/div {}
          (el/aside {:data-expanded (str navigation-expanded?)} "Unrelated navigation")
          ;; Exercise the actual Read/Outline entry point, not a second renderer.
          ;; Private function access is deliberate at this regression boundary.
          (memory/document-view "welcome.md" draft {} false)))

(defn- addresses [vnode]
  (keep :addr (tree-seq #(and (map? %) (:children %))
                        #(seq @(:children %)) vnode)))

(def expected-list-paragraphs
  ["Explore the market" "Record evidence and sources" "Compare competing explanations"
   "Review proposals before adoption" "Measure the workflow" "Schedule the trusted version"])

(defn- assert-frame [container vnode changed?]
  (let [addrs (vec (addresses vnode))]
    (is (= (count addrs) (count (set addrs))) "Every rendered element has its own stable address"))
  (is (= expected-list-paragraphs
         (mapv #(.-textContent %) (array-seq (.querySelectorAll container "li > p"))))
      "Nested bullet/ordered list content stays exact, ordered and nonduplicated")
  (is (= 6 (count (array-seq (.querySelectorAll container "li")))))
  (is (= 2 (count (array-seq (.querySelectorAll container "ul")))))
  (is (= "3" (.getAttribute (.querySelector container "ol") "start")))
  (is (= "bold" (.-textContent (.querySelector container "strong"))))
  (is (= ["bold" "italic"]
         (mapv #(.-textContent %) (array-seq (.querySelectorAll container "em")))))
  (is (= 1 (count (array-seq (.querySelectorAll container "p > em > strong")))))
  (is (= "bold" (.-textContent (.querySelector container "p > em > strong")))
      "The existing outer-last mark order is preserved at its exact DOM depth")
  (is (= (str (if changed? "Changed" "Original") " paragraph bold and italic")
         (.-textContent (.querySelector container ".memory-document p")))))

(deftest read-and-outline-own-recursive-elements-across-unrelated-renders
  (doseq [mode [:read :outline]]
    (testing (name mode)
      (let [ctx (context/create-execution-context)
            container (.createElement js/document "div")
            discharge (atom nil)]
        (.appendChild (.-body js/document) container)
        (try
          (binding [ec/*execution-context* ctx]
            (let [initial-draft (draft mode false)
                  original initial-draft
                  initial (tree initial-draft false)]
              (reset! discharge (browser/mount! container initial))
              (assert-frame container initial false)
              (let [last-tree (loop [previous initial frames [true false true]]
                                (if (seq frames)
                                  (let [next-tree (tree initial-draft (first frames))]
                                    (browser/refresh! @discharge previous next-tree)
                                    (assert-frame container next-tree false)
                                    (recur next-tree (next frames)))
                                  previous))
                    changed (tree (draft mode true) true)]
                (is (= original initial-draft) "DOM commits never modify source/analysis values")
                (browser/refresh! @discharge last-tree changed)
                (assert-frame container changed true)
                (let [followup (tree (draft mode true) false)]
                  (browser/refresh! @discharge changed followup)
                  (assert-frame container followup true)))))
          (finally
            (when @discharge (browser/unmount! @discharge))
            (.remove container)
            (context/stop-context! ctx)))))))

(deftest semantic-forms-render-distinct-scopes-and-retain-drafts-without-orphan-duplicates
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        release (atom nil) calls (atom []) path "wiki/Company.md"
        page {:kind :page} block {:kind :block :id (random-uuid)}
        company (random-uuid) experiment (random-uuid) structural (random-uuid)
        field {:id (random-uuid) :label "Revenue" :alias "revenue" :value-type :db.type/long
               :editable? true :present? true :input {:text "0"}}
        baseline {:path path :expected {:content :saved} :revision :source :scope-revision :schema}
        page-model (merge baseline {:target page :assigned-types [company]
                                   :types [{:id company :name "Business/Company"} {:id structural :name "S/Page"}]
                                   :fields [field {:id :optional :label "Unused owner" :reason :missing-source-binding}
                                            {:id :title :label "Generated title" :present? true :reason :generated-property}]})
        block-model (merge baseline {:target block :assigned-types [experiment]
                                    :types [{:id experiment :name "Research/Experiment"}]
                                    :fields [(assoc field :input {:text "9007199254740993"})]})
        draft {:mode :read :text "Saved" :source {:text "Saved" :entry {:content :saved} :kind :markdown}}
        state {:selected path :property-targets {path block}
               :property-models {[path page] page-model [path block] block-model}
               :property-drafts {[path page] {(:id field) (assoc (editor/property-draft page-model {:text "0"}) :alias "revenue")}
                                 [path block] {(:id field) (assoc (editor/property-draft block-model {:text "9007199254740993"}) :alias "revenue")}}}
        a {:property-current? (fn [& _] true)
           :property-input (fn [& args] (swap! calls conj args))
           :open-properties (fn [& args] (swap! calls conj args))}
        frame (fn [state draft]
                (el/div {}
                        (memory/file-header {:room-name "Lab"} state a draft true true true :clojure)
                        (memory/properties-view path draft state a true)
                        (memory/retained-properties-view path state a)
                        (memory/retained-declarations-view path state a)))
        inputs #(vec (array-seq (.querySelectorAll container "input[aria-label=Revenue]")))]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (let [initial (frame state draft)]
          (reset! release (browser/mount! container initial))
          (is (= "Business/Company" (.-textContent (.querySelector container "[aria-label='Assigned page types']"))))
          (is (nil? (.querySelector container "[title='Assigned type: S/Page']")))
          (is (= 1 (count (array-seq (.querySelectorAll container "[aria-label='Page properties']")))))
          (is (= 1 (count (array-seq (.querySelectorAll container "[aria-label='Block properties']")))))
          (is (= ["0" "9007199254740993"] (mapv #(.-value %) (inputs))))
          (is (not (.includes (.-textContent container) "Generated title")))
          (is (nil? (.querySelector container "[aria-label='Retained property drafts']")))
          (is (nil? (.querySelector container "[aria-label='Retained type assignment']")))
          (is (false? (.-open (.querySelector container ".memory-available-properties"))))
          (doseq [[input target] (map vector (inputs) [page block])]
            (.dispatchEvent input (js/Event. "input" #js {:bubbles true}))
            (is (= [path target (:id field) {:text (.-value input)}]
                   (let [[p t f value] (last @calls)] [p t (:id f) value]))))
          (let [focused (second (inputs))
                next-state (assoc state :unrelated :changed)
                next-tree (frame next-state draft)]
            (.focus focused)
            (browser/refresh! @release initial next-tree)
            (is (identical? focused (second (inputs))))
            (is (identical? focused (.-activeElement js/document)))
            (let [dirty-tree (frame next-state (assoc draft :text "Unsaved source"))]
              (browser/refresh! @release next-tree dirty-tree)
              (is (every? #(.-disabled %) (inputs)))
              (is (= ["0" "9007199254740993"] (mapv #(.-value %) (inputs))))
              (is (every? #(.-disabled %)
                          (filter #(= "Apply" (.-textContent %))
                                  (array-seq (.querySelectorAll container "button")))))
              (let [page-only (assoc-in state [:property-targets path] page)
                    page-tree (frame page-only draft)]
                (browser/refresh! @release dirty-tree page-tree)
                (is (= "Business/Company" (.-textContent (.querySelector container "[aria-label='Assigned page types']"))))
                (is (= ["0"] (mapv #(.-value %) (inputs))))
                (is (= 1 (count (array-seq (.querySelectorAll container "[aria-label='Retained property drafts']")))))
                (is (= (:property-drafts state) (:property-drafts page-only))))))))
      (finally
        (when @release (browser/unmount! @release))
        (.remove container)
        (context/stop-context! ctx)))))
