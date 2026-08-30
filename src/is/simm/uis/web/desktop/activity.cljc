(ns is.simm.uis.web.desktop.activity
  "Pure presentation of Dvergr semantic activity facts.

   Activity remains owned by the canonical room message; this namespace only
   chooses compact human-facing text for Simmis surfaces."
  (:require [clojure.string :as str]))

(defn label
  "Compact display label for one semantic activity fact."
  [fact]
  (str/join " · "
            (keep identity
                  [(some-> (or (:activity/verb fact) (:activity/kind fact)) name)
                   (:activity/tool-name fact)
                   (some-> (:activity/status fact) name)])))
