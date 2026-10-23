(ns is.simm.uis.web.desktop.editor-jump
  "Native selection adapter. The owning Spindel context admits and acknowledges
   navigation; this element retains only a disposable duplicate-event guard.")

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
