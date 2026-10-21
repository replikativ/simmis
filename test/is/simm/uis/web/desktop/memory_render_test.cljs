(ns is.simm.uis.web.desktop.memory-render-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.documents.rich-text :as rich]
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
