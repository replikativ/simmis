(ns is.simm.uis.web.desktop.rich-cells
  "Disposable code node views. Controls/results never enter the Markdown model."
  (:require ["@tiptap/core" :refer [Extension]]
            ["@tiptap/pm/state" :refer [Plugin]]
            [is.simm.documents.code-cells :as cells]
            [is.simm.documents.inline-charts :as inline-charts]
            [is.simm.uis.web.desktop.chart-view :as charts]
            [is.simm.uis.web.desktop.cell-result-view :as cell-result]
            [is.simm.uis.web.desktop.rich-saved-views :as saved-views]
            [is.simm.documents.rich-document :as document]))

(defn- portable [x] (js->clj (js/JSON.parse (js/JSON.stringify x)) :keywordize-keys true))

(defn- position [^js doc path]
  (loop [node doc path path pos 0 root? true]
    (when-let [i (first path)]
      (when (< -1 i (.-childCount node))
        (let [at (+ pos (if root? 0 1)
                    (reduce + 0 (map #(.-nodeSize (.child node %)) (range i))))]
          (if (next path) (recur (.child node i) (next path) at false) at))))))

(defn- context [^js el ^js view]
  (let [doc (.. view -state -doc) raw (.getAttribute el "data-cells")
        baseline (.getAttribute el "data-model") cached (.-simmisCellContext el)]
    (if (and cached (identical? doc (.-doc cached)) (= raw (.-raw cached)) (= baseline (.-baseline cached)))
      (.-value cached)
      (let [config (when raw (js->clj (js/JSON.parse raw) :keywordize-keys true))
            model (when config (portable (.toJSON doc)))
            value (when (and config
                             (= (document/normalized (js->clj (js/JSON.parse baseline) :keywordize-keys true))
                                (document/normalized model))
                             (cells/equivalent-projection? (:model config) model))
                    (assoc config :positions (into {} (keep (fn [cell]
                                                              (when-let [pos (position doc (:node-path cell))]
                                                                [pos cell]))) (:cells config))))]
        (set! (.-simmisCellContext el) #js {:doc doc :raw raw :baseline baseline :value value})
        value))))

(defn- inline-context [^js el ^js view]
  (let [doc (.. view -state -doc) raw (.getAttribute el "data-inline-charts")
        cached (.-simmisInlineChartContext el)]
    (if (and cached (identical? doc (.-doc cached)) (= raw (.-raw cached)))
      (.-positions cached)
      (let [config (if (and cached (= raw (.-raw cached))) (.-config cached)
                       (when raw
                         (update (js->clj (js/JSON.parse raw) :keywordize-keys true)
                                 :entries #(mapv inline-charts/normalize-preview %))))
            matches (when (seq (:entries config))
                      (inline-charts/positions [(portable (.toJSON doc))] (:entries config) (:limit config)))
            positions (into {} (keep (fn [[path preview]]
                                      (when-let [pos (position doc (rest path))] [pos preview]))) matches)]
        (set! (.-simmisInlineChartContext el) #js {:doc doc :raw raw :config config :positions positions})
        positions))))

(defn- saved-context [^js el ^js view]
  (let [doc (.. view -state -doc) raw (.getAttribute el "data-saved-views")
        baseline (.getAttribute el "data-model") cached (.-simmisSavedViewContext el)]
    (if (and cached (identical? doc (.-doc cached)) (= raw (.-raw cached)) (= baseline (.-baseline cached)))
      (.-positions cached)
      (let [positions
            (try
              (let [config (when raw (js->clj (js/JSON.parse raw) :keywordize-keys true))
                    model (when config (portable (.toJSON doc)))]
                (when (and config
                           (= (document/normalized (js->clj (js/JSON.parse baseline) :keywordize-keys true))
                              (document/normalized model))
                           (cells/equivalent-projection? (:model config) model))
                  (into {} (keep (fn [entry]
                                   (when-let [pos (position doc (:path entry))]
                                     [pos entry]))) (:entries config))))
              (catch :default _ nil))]
        (set! (.-simmisSavedViewContext el) #js {:doc doc :raw raw :baseline baseline :positions positions})
        positions))))

(defn refresh! [^js el]
  (when-let [views (.-simmisCellViews el)] (.forEach views (fn [paint] (paint)))))

(defn queue-refresh! [^js el]
  (when-not (.-simmisCellRefreshPending el)
    (let [token #js {} generation (.-simmisCellGeneration el) editor (.-simmisRichEditor el)]
      (set! (.-simmisCellRefreshPending el) token)
      (js/queueMicrotask
       (fn []
         (when (identical? token (.-simmisCellRefreshPending el))
           (set! (.-simmisCellRefreshPending el) nil)
           (when (and generation (identical? generation (.-simmisCellGeneration el))
                      editor (identical? editor (.-simmisRichEditor el)))
             (refresh! el))))))))

(defn set-presenter! [^js el callback]
  (aset el "simmisSavedViewPresenter" callback)
  (queue-refresh! el))

(defn- node-view [^js el]
  (fn [node view get-pos]
    (let [dom (.createElement js/document "div") pre (.createElement js/document "pre")
          code (.createElement js/document "code") chrome (.createElement js/document "div")
          button (.createElement js/document "button") result (.createElement js/document "div")
          chart (.createElement js/document "div") saved (.createElement js/document "div")
          mount-token (str (random-uuid)) destroyed? (volatile! false)
          selected (volatile! nil)
          paint! (fn []
                   (when-not @destroyed?
                   (let [pos (try (get-pos) (catch :default _ nil))
                         config (context el view) cell (when (number? pos) (get (:positions config) pos))
                         receipt (first (filter #(and (= (:from cell) (get-in % [:source :from]))
                                                       (= (:to cell) (get-in % [:source :to]))) (:receipts config)))
                         running? (contains? (set (:running config)) (:key cell))
                         preview (when (number? pos) (get (inline-context el view) pos))
                         entry (when (number? pos) (get (saved-context el view) pos))
                         presented? (try (and entry (saved-views/paint! saved (aget el "simmisSavedViewPresenter")
                                                                      (select-keys entry [:from :to :view]) mount-token))
                                         (catch :default _ (saved-views/clear! saved) false))
                         selection (.. view -state -selection)
                         native (when (number? pos) (.nodeAt (.. view -state -doc) pos))
                         source-selected? (and native (.hasFocus view)
                                               (<= (.-from selection) (+ pos (.-nodeSize native)))
                                               (>= (.-to selection) pos))]
                     (when-not presented? (saved-views/clear! saved))
                     (set! (.-hidden saved) (not presented?))
                     ;; Preserve the original contentDOM. Never fold a selected
                     ;; or composing editor subtree, nor unknown correspondence.
                     (set! (.-hidden pre) (and presented? (not source-selected?) (not (.-composing view))))
                     (set! (.-hidden chart) (nil? preview))
                     (when-not (identical? preview (.-simmisInlinePreview chart))
                       (set! (.-simmisInlinePreview chart) preview)
                       (.replaceChildren chart)
                       (when preview
                         (let [label (.createElement js/document "div") projection (.createElement js/document "div")]
                           (set! (.-textContent label) "Chart preview")
                           (.append chart label projection)
                           (if (= :ready (:status preview))
                             (charts/paint! projection (:data preview))
                             (set! (.-textContent projection) "Chart JSON is invalid or exceeds the supported bounds.")))))
                     (vreset! selected cell)
                     (set! (.-hidden chrome) (nil? cell))
                     (set! (.-disabled button) (or (not (:enabled? config)) (not (:runnable? cell)) running?))
                     (set! (.-textContent button) (if running? "Running…" "Run"))
                     (set! (.-title button) (str "Run this saved Clojure cell in " (:namespace config) ". No room tools are injected."))
                     (set! (.-hidden result) (nil? receipt))
                     (when-not receipt
                       (.replaceChildren result)
                       (set! (.-simmisReceipt result) nil))
                     (when receipt
                       (let [text (str (if (:success receipt) (:value receipt) (:error receipt))
                                       (when (seq (:stdout receipt)) (str "\nstdout\n" (:stdout receipt)))
                                       (when (seq (:stderr receipt)) (str "\nstderr\n" (:stderr receipt))))
                             presentation (when (= (:id receipt) (:presentation-id config)) (:presentation receipt))
                             diagnostic (:presentation-diagnostic receipt)
                             key [(:id receipt) text presentation diagnostic]]
                         (when-not (= key (.-simmisReceipt result))
                           (set! (.-simmisReceipt result) key)
                           (.replaceChildren result)
                           (let [label (.createElement js/document "div") output (.createElement js/document "pre")]
                             (set! (.-textContent label) (str (if (:success receipt) "Result" "Failed") " · " (:namespace receipt)
                                                             (when (:value-truncated? receipt) " · truncated")
                                                             (when (:partial? receipt) " · partial; inspect Room REPL")))
                             (set! (.-textContent output) text)
                             (.append result label)
                             (when presentation
                               (let [projection (.createElement js/document "div")]
                                 (cell-result/paint! projection presentation) (.appendChild result projection)))
                             (when diagnostic
                               (let [note (.createElement js/document "p")]
                                 (set! (.-className note) "memory-note")
                                 (set! (.-textContent note) diagnostic) (.appendChild result note)))
                             (.appendChild result output))))))))
          paint-language! (fn [node]
                            (let [lang (aget (.-attrs node) "language")]
                              (set! (.-className code) (if (seq lang) (str "language-" lang) ""))))]
      (when-not (.-simmisCellViews el) (set! (.-simmisCellViews el) (js/Set.)))
      (.add (.-simmisCellViews el) paint!)
      (set! (.-className dom) "memory-code-cell")
      (set! (.-className chrome) "memory-code-cell-controls")
      (set! (.-className result) "memory-code-cell-result")
      (set! (.-className chart) "memory-inline-chart memory-chart-projection")
      (set! (.-className saved) "memory-inline-saved-view")
      (.setAttribute saved "contenteditable" "false")
      (.setAttribute chrome "contenteditable" "false")
      (.setAttribute result "contenteditable" "false")
      (.setAttribute chart "contenteditable" "false")
      (.setAttribute button "type" "button")
      (.setAttribute button "aria-label" "Run saved Clojure cell")
      (set! (.-onclick button)
            (fn [event]
              (.preventDefault event) (.stopPropagation event)
              ;; Recheck after props/native edits; controller checks context
              ;; state again. This key is correspondence, never authority.
              (paint!)
              (when (and @selected (not (.-disabled button)))
                (.dispatchEvent el (js/CustomEvent. "simmis-run-cell"
                                                   #js {:detail #js {:key (:key @selected)}})))))
      (.append pre code) (.append chrome button) (.append dom pre saved chrome result chart)
      (paint-language! node)
      #js {:dom dom :contentDOM code
           :update (fn [new-node]
                     (when (= "codeBlock" (.. new-node -type -name)) (paint-language! new-node) true))
           :stopEvent (fn [event] (or (.contains chrome (.-target event)) (.contains result (.-target event))
                                    (.contains chart (.-target event)) (.contains saved (.-target event))))
           :ignoreMutation (fn [mutation]
                             (and (not= "selection" (.-type mutation))
                                  (not (.contains code (.-target mutation)))))
           :destroy (fn [] (vreset! destroyed? true) (saved-views/clear! saved) (.delete (.-simmisCellViews el) paint!) (set! (.-onclick button) nil)
                      (.replaceChildren chart) (set! (.-simmisInlinePreview chart) nil))})))

(defn extension [^js el]
  (.create Extension
           #js {:name "simmisSavedCells"
                :addProseMirrorPlugins
                (fn [] #js [(Plugin. #js {:props #js {:nodeViews #js {:codeBlock (node-view el)}}
                                         :view (fn [_] #js {:update (fn [& _] (refresh! el))})})])}))

(defn attach! [^js el]
  (let [listener (fn [event] (when-let [callback (aget el "onsimmis-run-cell")] (callback event)))
        refresh-listener (fn [_] (queue-refresh! el))
        events ["focusin" "focusout" "compositionstart" "compositionend"]]
    (set! (.-simmisCellGeneration el) #js {})
    (set! (.-simmisCellRefreshPending el) nil)
    (.addEventListener el "simmis-run-cell" listener)
    (doseq [event events] (.addEventListener el event refresh-listener))
    (refresh! el)
    (fn [] (.removeEventListener el "simmis-run-cell" listener)
      (doseq [event events] (.removeEventListener el event refresh-listener))
      (set! (.-simmisCellGeneration el) nil)
      (set! (.-simmisCellRefreshPending el) nil)
      (set! (.-simmisSavedViewContext el) nil)
      (set! (.-simmisCellContext el) nil)
      (set! (.-simmisInlineChartContext el) nil))))
