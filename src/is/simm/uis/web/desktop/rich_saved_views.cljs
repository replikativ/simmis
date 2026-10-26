(ns is.simm.uis.web.desktop.rich-saved-views
  "Native saved-view islands. Only DOM/discharge handles live here; Memory
   retains all source, query, request, property and edit state."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.dom.addressing :as addressing]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.commit :as commit]
            [org.replikativ.spindel.dom.discharge :as discharge]
            [org.replikativ.spindel.dom.render :as render]))

(defn presenter
  "Capture the original world and trusted UUID entries. Portable native keys
   identify presentation only. A fresh mount token distinguishes native editors
   and repeated occurrences even when their document/view IDs are equal."
  [ctx owner entries render-entry]
  (when-not ctx (throw (ex-info "Saved view requires its original execution context" {})))
  (fn [key token]
    (when-let [entry (some #(when (= [(:from key) (:to key) (:view key)]
                                    [(:from %) (:to %) (str (:view %))]) %) entries)]
      (let [scope (addressing/keyed-child-address :memory-rich-saved-view [(:fork-id ctx) owner token key])]
        (binding [ec/*execution-context* ctx]
          (addressing/with-parent-addr-fn
           scope
           (fn []
             (addressing/with-slot-fn
              0 (fn []
                  (when-let [vdom (render-entry entry)]
                    {:context ec/*execution-context* :scope scope :vdom vdom}))))))))))

(defn clear! [^js target]
  (when-let [{:keys [context discharge vdom]} (.-simmisSavedViewMount target)]
    ;; Retire the handle before callbacks; they may schedule another render.
    (set! (.-simmisSavedViewMount target) nil)
    (binding [ec/*execution-context* context]
      (try
        (discharge/call-refs-on-unmount! vdom)
        (finally
          (try (commit/retire-dead! discharge vdom nil)
               (finally (browser/unmount! discharge) (.replaceChildren target))))))))

(defn paint!
  "Render latest parent-provided presentation. No fetch, subscription, document
   transaction or fallback to an ambient execution context."
  [^js target callback key token]
  (if-let [{:keys [context scope vdom] :as next} (when callback (callback key token))]
    (do
      (when (and (.-simmisSavedViewMount target)
                 (or (not= scope (:scope (.-simmisSavedViewMount target)))
                     (render/root-identity-changed? (:vdom (.-simmisSavedViewMount target)) vdom)))
        (clear! target))
      (binding [ec/*execution-context* context]
        (let [previous (.-simmisSavedViewMount target)
              dom (if previous
                    (do (browser/refresh! (:discharge previous) (:vdom previous) vdom) (:discharge previous))
                    (browser/mount! target vdom))]
          (set! (.-simmisSavedViewMount target) (assoc next :discharge dom))))
      true)
    (do (clear! target) false)))
