(ns is.simm.uis.web.desktop.disclosure
  "Mirror native disclosure preferences into the owning UI world.")

(defn remember-open! [event remember!]
  #?(:cljs
     (let [target (.-currentTarget event)]
       ;; Native toggle events can be queued after unmount. They must not
       ;; recreate an old room's draft or change a discarded view preference.
       (when (and target (.-isConnected target))
         (remember! (boolean (.-open target)))))
     :clj nil))
