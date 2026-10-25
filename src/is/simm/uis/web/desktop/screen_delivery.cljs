(ns is.simm.uis.web.desktop.screen-delivery
  "A frame upload's acknowledgement belongs to its original capture attempt."
  (:require [is.simm.uis.web.desktop.screen-sampling :as sampling]))

(defn submit!
  "Run encoding/upload inside the Promise chain, including synchronous errors.
   Resolve the current capture at completion; stale attempts cannot update it."
  [current-sampler attempt post! on-error]
  (let [finish! (fn [accepted?]
                  (when-let [current (current-sampler)]
                    (swap! current sampling/acknowledge attempt accepted?)))]
    (-> (js/Promise.resolve nil)
        (.then (fn [_] (post!)))
        (.then finish!)
        (.catch (fn [error]
                  (finish! false)
                  (on-error error))))))
