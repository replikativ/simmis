(ns is.simm.uis.web.desktop.editor-jump
  "Native selection adapter. The owning Spindel context admits and acknowledges
   navigation; this element retains only a disposable duplicate-event guard.")

(defn attach!
  "Spindel assigns on-* DOM properties. Browsers recognize only built-in event
   properties, so the foreign adapter bridges its custom events explicitly.
   Read the latest property on delivery: reactive callbacks may change without
   remounting this native editor. All listeners belong to this element."
  [^js el]
  (let [before (fn [event] (when-let [callback (aget el "onsimmis-before-jump")] (callback event)))
        after (fn [event] (when-let [callback (aget el "onsimmis-jumped")] (callback event)))]
    (.addEventListener el "simmis-before-jump" before)
    (.addEventListener el "simmis-jumped" after)
    (fn [] (.removeEventListener el "simmis-before-jump" before)
      (.removeEventListener el "simmis-jumped" after))))

(defn consume! [^js el apply!]
  (when-let [raw (.getAttribute el "data-jump")]
    (let [jump (js/JSON.parse raw)
          token [(.getAttribute el "data-owner") (aget jump "id")]]
      (when (and (not= token (.-simmisAppliedJump el))
                 (.dispatchEvent el (js/CustomEvent. "simmis-before-jump"
                                                      #js {:cancelable true :detail jump})))
        (when-let [selection (apply! jump)]
          (set! (.-simmisAppliedJump el) token)
          (.dispatchEvent el (js/CustomEvent. "simmis-jumped"
                                             #js {:detail #js {:jump jump :selection selection}})))))))

(defn before! [^js event admit!]
  (when (and (= (.-target event) (.-currentTarget event)) admit!)
    (when-not (admit! (js->clj (.-detail event) :keywordize-keys true))
      (.preventDefault event))))

(defn acknowledge! [^js event done!]
  (when (and (= (.-target event) (.-currentTarget event)) done!)
    (done! (js->clj (.. event -detail -jump) :keywordize-keys true)
           (js->clj (.. event -detail -selection) :keywordize-keys true))))
