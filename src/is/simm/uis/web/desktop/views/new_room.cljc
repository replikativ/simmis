(ns is.simm.uis.web.desktop.views.new-room
  "Team entry point for canonical project workspace creation."
  (:require #?(:cljs [is.simm.uis.web.desktop.views.workspace-creation :as workspace]
               :clj [org.replikativ.spindel.dom.elements :as el])))

(defn render-new-room-content [data]
  #?(:cljs (workspace/render (assoc data :landing :chat :tab-type :new-room))
     :clj (el/div {} "New team")))
