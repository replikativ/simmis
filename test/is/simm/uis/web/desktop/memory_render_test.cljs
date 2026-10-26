(ns is.simm.uis.web.desktop.memory-render-test
  (:require [cljs.test :refer-macros [deftest is testing async]]
            ["@tiptap/pm/history" :refer [undoDepth]]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.saved-view-contract :as view-contract]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.property-controls :as property-controls]
            [is.simm.uis.web.desktop.saved-table-component :as saved-table]
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
        stamp (random-uuid) company (random-uuid) experiment (random-uuid) structural (random-uuid)
        field {:id (random-uuid) :label "Revenue" :alias "revenue" :value-type :db.type/long
               :editable? true :present? true :input {:text "0"}}
        baseline {:path path :target-stamp stamp :expected {:content :saved} :revision :source :scope-revision :schema}
        page-model (merge baseline {:target page :assigned-types [company]
                                   :types [{:id company :name "Business/Company"} {:id structural :name "S/Page"}]
                                   :fields [field {:id :optional :label "Unused owner" :reason :missing-source-binding}
                                            {:id :title :label "Generated title" :present? true :reason :generated-property}]})
        block-model (merge baseline {:target block :assigned-types [experiment]
                                    :types [{:id experiment :name "Research/Experiment"}]
                                    :fields [(assoc field :input {:text "9007199254740993"})]})
        draft {:mode :read :text "Saved" :source {:text "Saved" :entry {:content :saved} :kind :markdown :target-stamp stamp}}
        state {:selected path :target-verified? true :target-stamp stamp :connection :socket :verified-connection :socket
               :property-targets {path block}
               :property-models {[path page] page-model [path block] block-model}
               :property-drafts {[path page] {(:id field) (assoc (editor/property-draft page-model {:text "0"}) :alias "revenue")}
                                 [path block] {(:id field) (assoc (editor/property-draft block-model {:text "9007199254740993"}) :alias "revenue")}}}
        a {:property-current? (fn [& _] true)
           :property-blocked? (constantly false)
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
          (is (= "Company" (.-textContent (.querySelector container "[aria-label='Assigned page types']"))))
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
                (is (= "Company" (.-textContent (.querySelector container "[aria-label='Assigned page types']"))))
                (is (= ["0"] (mapv #(.-value %) (inputs))))
                (is (= 1 (count (array-seq (.querySelectorAll container "[aria-label='Retained property drafts']")))))
                (is (= (:property-drafts state) (:property-drafts page-only))))))))
      (finally
        (when @release (browser/unmount! @release))
        (.remove container)
        (context/stop-context! ctx)))))

(deftest reference-control-uses-names-and-only-compatible-host-options
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        release (atom nil) calls (atom []) target {:kind :page} type (random-uuid)
        id "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa" other "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb"
        field {:id (random-uuid) :label "Founder" :alias "founder" :value-type :db.type/ref
               :editable? true :input {:text id} :required? false
               :relation {:representation :mapping :target id :path-hint "inert-hint.md" :codomain type :clearable? false}}
        model {:reference-options {type {:status :ready :complete? true
                                         :items [{:id id :label "Sam" :path "People/Sam.md"}
                                                 {:id other :label "Chris" :path "People/Chris.md"}]}}}
        a {:property-input (fn [& args] (swap! calls conj [:input args]))
           :property-save (fn [& args] (swap! calls conj [:save args]))
           :open (fn [path] (swap! calls conj [:open path]))}]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! release (browser/mount! container (property-controls/field-view "a.md" target field model {} a true)))
        (let [select (.querySelector container "select")
              buttons (array-seq (.querySelectorAll container "button"))]
          (is (= id (.-value select)))
          (is (not (.-disabled select)))
          (is (not-any? #(= "Clear" (.-textContent %)) buttons))
          (is (not (.includes (.-textContent container) id)) "UUID is an option identity, not a visible form value")
          (.click (first buttons))
          (is (= [:open "People/Sam.md"] (last @calls)) "Actual resolved path is used, never the inert hint")
          (set! (.-value select) other)
          (.dispatchEvent select (js/Event. "change" #js {:bubbles true}))
          (is (= [:input (list "a.md" target field {:text other})] (nth @calls 1)))
          (is (= [:save (list "a.md" target field)] (nth @calls 2)))))
      (finally
        (when @release (browser/unmount! @release))
        (.remove container)
        (context/stop-context! ctx)))))

(deftest footnote-references-remain-visible-in-read-and-structure-views
  (doseq [mode [:read :outline]]
    (let [ctx (context/create-execution-context)
          container (.createElement js/document "div")
          release (atom nil)
          node {:kind :Paragraph :from 0 :to 25
                :children [(text-node "Evidence")
                           {:kind :Emphasis
                            :children [{:kind :Footnote :from 8 :to 19
                                        :footnote {:raw "[^ method ]" :label "method"}}]}]}
          model (rich/model node)
          analysis {:slots [{:from 0 :to 25 :model model}]
                    :semantic {:status :ready :blocks [{:id "provisional" :identity-status :provisional
                                                       :syntax [node] :children []}]}}
          d {:mode mode :text "Evidence*[^ method ]*" :analysis analysis}]
      (.appendChild (.-body js/document) container)
      (try
        (binding [ec/*execution-context* ctx]
          (reset! release (browser/mount! container (tree d false)))
          (is (= "method" (.-textContent (.querySelector container "sup"))))
          (is (= "[^ method ]" (.getAttribute (.querySelector container "sup") "title")))
          (is (= "Footnote method" (.getAttribute (.querySelector container "sup") "aria-label")))
          (is (= "method" (.-textContent (.querySelector container "sup em"))))
          (is (nil? (.querySelector container "script"))))
        (finally
          (when @release (browser/unmount! @release))
          (.remove container)
          (context/stop-context! ctx))))))

(deftest saved-backlinks-render-without-duplicating-or-hijacking-modifier-navigation
  (let [ctx (context/create-execution-context)
        container (.createElement js/document "div") release (atom nil) calls (atom [])
        result {:path "wiki/Source.md" :from 12 :url "/room/source/files?path=wiki%2FSource.md"}
        frame (fn [observation]
                (memory/backlinks-view
                 {:backlinks (fn [] observation)
                  :backlinks-open (fn [open?] (swap! calls conj [:toggle open?]))
                  :retry-backlinks (fn [] (swap! calls conj [:retry]))
                  :open-backlink (fn [value] (swap! calls conj [:open value]))}))]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (let [closed (frame {:open? false})
              ready (frame {:open? true :reply {:status :ready :complete? false :results [result]}})
              failed (frame {:open? true :error "Unavailable"})]
          (reset! release (browser/mount! container closed))
          (is (= "Backlinks" (.-textContent container)))
          (is (nil? (.querySelector container "a")))
          (browser/refresh! @release closed ready)
          (is (.includes (.-textContent container) "Only part of the matching links"))
          (is (= 1 (.-length (.querySelectorAll container "a"))))
          (let [link (.querySelector container "a")
                ordinary (js/MouseEvent. "click" #js {:bubbles true :cancelable true})
                modified (js/MouseEvent. "click" #js {:bubbles true :cancelable true :ctrlKey true})]
            ;; Cancel native navigation only after the actual handler ran.
            (.addEventListener link "click" (fn [event] (.preventDefault event)) #js {:once true})
            (.dispatchEvent link modified)
            (is (empty? (filter #(= :open (first %)) @calls)))
            (.dispatchEvent link ordinary)
            (is (.-defaultPrevented ordinary))
            (is (= [[:open result]] (filterv #(= :open (first %)) @calls))))
          (browser/refresh! @release ready failed)
          (is (nil? (.querySelector container "a")))
          (is (.includes (.-textContent container) "Unavailable"))
          (.click (.querySelector container "button"))
          (is (= [:retry] (last @calls)))))
      (finally
        (when @release (browser/unmount! @release))
        (.remove container)
        (context/stop-context! ctx)))))

(deftest property-revalidation-keeps-the-form-in-place-without-enabling-stale-writes
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        stamp (random-uuid) id (random-uuid) target {:kind :page} release (atom nil)
        model {:target-stamp stamp :request-marker [:socket stamp 1] :expected {:content id}
               :fields [{:id id :label "Revenue" :editable? true :required? true
                         :value-type :db.type/long :input {:text "9007199254740993"}}]}
        state {:target-verified? true :target-stamp stamp :connection :socket :verified-connection :socket
               :property-models {["a.md" target] model}}
        draft {:source {:entry {:content id} :text "saved" :target-stamp stamp} :text "saved"}
        actions (fn [current?] {:property-current? (fn [& _] current?) :property-blocked? (constantly false)})
        frame #(memory/scoped-properties-view "a.md" target draft %1 (actions %2) true)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (let [ready (frame state true)]
          (reset! release (browser/mount! container ready))
          (let [input (.querySelector container "input")
                checking (frame state false)]
            (is (not (.-disabled input)))
            (browser/refresh! @release ready checking)
            (is (identical? input (.querySelector container "input")))
            (is (.-disabled input))
            (is (= "9007199254740993" (.-value input)))
            (is (.includes (.-textContent container) "Checking…"))
            (let [lost (frame (assoc state :target-stamp (random-uuid)) false)]
              (browser/refresh! @release checking lost)
              (is (nil? (.querySelector container "input")))
              (is (not (.includes (.-textContent container) "9007199254740993")))))))
      (finally
        (when @release (browser/unmount! @release))
        (.remove container)
        (context/stop-context! ctx)))))

(deftest receipt-table-data-cannot-create-property-controls-or-html
  (let [ctx (context/create-execution-context)
        container (.createElement js/document "div") discharge (atom nil)
        literal "<img src=x onerror=alert(1)>"
        row {:id (random-uuid) :edit {:path "companies/forged.md"}
             :cells [{:kind :text :text literal} {:kind :number :text "9007199254740993"}]}]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge
                (browser/mount! container
                                (saved-table/view [{:label "Candidate"} {:label "Spend"}] [row] {} {}))))
      (is (= ["Candidate" "Spend"]
             (mapv #(.-textContent %) (array-seq (.querySelectorAll container "th")))))
      (is (= [literal "9007199254740993"]
             (mapv #(.-textContent %) (array-seq (.querySelectorAll container "td")))))
      (is (nil? (.querySelector container "img")) "source text never becomes executable markup")
      (is (nil? (.querySelector container "button, input, select, [contenteditable]"))
          "report data cannot grant page-property editing")
      (finally
        (when @discharge (browser/unmount! @discharge))
        (.remove container)
        (context/stop-context! ctx)))))

(deftest saved-recording-links-preserve-content-preconditions-at-activation
  (let [ctx (context/create-execution-context) container (.createElement js/document "div") discharge (atom nil)
        original (random-uuid) replacement (random-uuid) room (random-uuid) target (random-uuid)
        value {:kind :media :path "captures/session/recordings/one.webm" :content original :size 20 :text "Open recording"}
        navigation {:media-context {:room room :target target}
                    :open (fn [& _] (throw (js/Error. "Recording must not follow a current-file path")))}]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container (saved-table/cell value navigation))))
      (let [link (.querySelector container "a") url (.getAttribute link "href")]
        (is (.includes url (str "content=" original)))
        (is (.includes url (str "target=" target)))
        (is (not (.includes url (str replacement))))
        (is (= url (saved-table/media-url value (:media-context navigation))))
        (is (= url (saved-table/media-url (assoc value :room (random-uuid) :target (random-uuid))
                                          (:media-context navigation))) "cell data cannot redirect its Room or repository target")
        (is (not= url (saved-table/media-url (assoc value :content replacement) (:media-context navigation)))))
      (finally
        (when @discharge (browser/unmount! @discharge))
        (.remove container) (context/stop-context! ctx)))))

(deftest read-mode-places-one-view-between-prose-and-keeps-source-on-demand
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        discharge (atom nil) calls (atom []) path "reports/results.md"
        revision (random-uuid) stamp (random-uuid) view-id (random-uuid)
        before "Before\n\n" fence-text "```simmis-view\r\nvisible source only on demand\r\n```\r\n" after "\nAfter\n"
        source (str before fence-text after) from (count before) to (+ from (count fence-text))
        p (fn [from to value] {:kind :Paragraph :from from :to to :children [(text-node value)]})
        nodes [(p 0 from "Before") {:kind :FencedCodeBlock :from from :to to :language "simmis-view"
                                    :literal "visible source only on demand"}
               (p to (count source) "After")]
        descriptor {:version 1 :id view-id :source {:kind :pages :roots [""] :types []}
                    :presentation :table :columns [{:kind :title}]}
        entry {:from from :to to :view view-id :status :ready :descriptor descriptor}
        analysis {:revision revision :nodes nodes :slots (mapv #(assoc (select-keys % [:from :to :kind]) :model (rich/model %)) nodes)
                  :saved-views [entry]}
        draft {:mode :read :text source :analysis analysis
               :source {:kind :markdown :text source :analysis analysis :target-stamp stamp
                        :entry {:content revision :size (count source) :mode 33188}}}
        state {:target-stamp stamp :target-verified? true :drafts {path draft}}
        a {:toggle-saved-view-source (fn [p id] (swap! calls conj [p id]))}
        frame #(memory/document-view path draft a false nil nil %)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (let [initial (frame state)]
          (reset! discharge (browser/mount! container initial))
          (is (= 1 (.-length (.querySelectorAll container ".memory-saved-view"))))
          (let [regions (array-seq (.querySelectorAll container ".memory-document > .memory-region"))
                before-node (.querySelector (first regions) "p")]
            (is (= 3 (count regions)))
            (is (= "Before" (.-textContent (first regions))))
            (is (.includes (.-textContent (second regions)) "Page table"))
            (is (= "After" (.-textContent (last regions))))
            (is (not (.includes (.-textContent container) "visible source only on demand")))
            (let [button (first (filter #(= "Show source" (.-textContent %)) (array-seq (.querySelectorAll container "button"))))]
              (is (some? button))
              (.click button)
              (is (= [[path view-id]] @calls)))
            (let [shown (frame (assoc-in state [:saved-view-source [path view-id]] true))]
              (browser/refresh! @discharge initial shown)
              (is (= fence-text (.-textContent (.querySelector container ".memory-inline-saved-view pre code"))))
              (is (identical? before-node (.querySelector container ".memory-region p")))
              (is (= source (:text draft)))
              (let [dirty (assoc draft :text (str source "Unsaved edit"))
                    dirty-state (assoc-in state [:drafts path] dirty)
                    next-frame (memory/document-view path dirty a false nil nil dirty-state)]
                (browser/refresh! @discharge shown next-frame)
                (is (nil? (.querySelector container ".memory-inline-saved-view")))
                (is (some? (.querySelector container "section[aria-label='Views from saved version']")))
                (is (.includes (.-textContent container) "visible source only on demand"))
                (is (= (str source "Unsaved edit") (:text dirty))))))))
      (finally
        (when @discharge (browser/unmount! @discharge))
        (.remove container)
        (context/stop-context! ctx)))))


(deftest rich-memory-uses-the-shared-view-and-latest-state-without-another-panel
  (async done
    (let [ctx (context/create-execution-context) container (.createElement js/document "div")
          discharge (atom nil) committed (atom nil) calls (atom []) path "reports/inline.md"
          revision (random-uuid) stamp (random-uuid) view-id (random-uuid)
          source "Before\n\n```simmis-view\nquery\n```\n\nAfter\n"
          before {:kind :Paragraph :from 0 :to 8 :children [(text-node "Before")]}
          fence {:kind :FencedCodeBlock :from 8 :to 33 :language "simmis-view" :literal "query"}
          after {:kind :Paragraph :from 33 :to (count source) :children [(text-node "After")]}
          nodes [before fence after]
          descriptor {:version 1 :id view-id :source {:kind :pages :roots [""] :types []}
                      :presentation :table :columns [{:kind :title}]}
          entry {:from 8 :to 33 :view view-id :status :ready :descriptor descriptor}
          analysis {:revision revision :nodes nodes
                    :slots (mapv #(assoc (select-keys % [:from :to :kind]) :model (rich/model %)) nodes)
                    :saved-views [entry]
                    :semantic {:status :ready :blocks [{:id "provisional" :identity-status :provisional
                                                        :syntax nodes :from 0 :to (count source) :children []}]}}
          draft (editor/begin-rich {:mode :rich :text source :analysis analysis
                                    :source {:kind :markdown :text source :analysis analysis :target-stamp stamp
                                             :entry {:content revision :size (count source) :mode 33188}}})
          state {:target-stamp stamp :target-verified? true :drafts {path draft}}
          a {:cell-config (constantly nil) :block-links (constantly {}) :linked-block (constantly nil)
             :jump (constantly nil) :owner :same-memory-owner
             :toggle-saved-view (fn [p id] (swap! calls conj [p id]))}
          frame #(memory/document-view path draft a false nil nil %)
          finish (fn []
                   (when @discharge
                     (binding [ec/*execution-context* ctx]
                       (org.replikativ.spindel.dom.discharge/call-refs-on-unmount! @committed)
                       (org.replikativ.spindel.dom.commit/retire-dead! @discharge @committed nil)
                       (browser/unmount! @discharge)))
                   (.remove container) (context/stop-context! ctx) (done))]
      (.appendChild (.-body js/document) container)
      (binding [ec/*execution-context* ctx]
        (reset! committed (frame state))
        (reset! discharge (browser/mount! container @committed)))
      (let [native (.querySelector container "simmis-rich-editor") editor (.-simmisRichEditor native)
            before-json (js/JSON.stringify (.getJSON editor))
            refresh (fn [next-state]
                      (binding [ec/*execution-context* ctx]
                        (let [next (frame next-state)]
                          (browser/refresh! @discharge @committed next)
                          (reset! committed next))))]
        (is (= 1 (.-length (.querySelectorAll container ".memory-saved-view"))))
        (is (nil? (.querySelector container "section[aria-label='Views from saved version']")))
        (.click (first (filter #(= "Open view" (.-textContent %))
                              (array-seq (.querySelectorAll container "button")))))
        (is (= [[path view-id]] @calls))
        (let [opened (assoc-in state [:page-view-open [path view-id]] true)
              reply {:profile view-contract/profile :view view-id :path path :target-stamp stamp
                     :descriptor descriptor :status :ready :total 1
                     :columns [{:label "Title"}] :rows [{:id "row" :title "Company" :cells [{:kind :text :text "First result"}]}]}
              replied (assoc-in opened [:saved-views [path view-id] :reply] reply)]
          (refresh replied)
          (js/queueMicrotask
           (fn []
             (try
               (is (identical? editor (.-simmisRichEditor native)))
               (is (= before-json (js/JSON.stringify (.getJSON editor))))
               (is (= 0 (undoDepth (.-state editor))))
               (is (.includes (.-textContent container) "First result"))
               (is (= 1 (.-length (.querySelectorAll container ".memory-saved-view"))))
               (refresh (assoc-in replied [:saved-views [path view-id] :reply :rows 0 :cells 0 :text] "Latest result"))
               (js/queueMicrotask
                (fn []
                  (try
                    (is (.includes (.-textContent container) "Latest result"))
                    (is (identical? editor (.-simmisRichEditor native)))
                    (is (= before-json (js/JSON.stringify (.getJSON editor))))
                    (is (= 0 (undoDepth (.-state editor))))
                    (is (= 1 (count @calls)) "The bridge initiates no additional acquisition")
                    (finally (finish)))))
               (catch :default error (is false (str error)) (finish))))))))))
