(ns is.simm.uis.web.desktop.views.new-kb
  "Memory entry point; existing legacy wikis retain their management views."
  (:require #?(:cljs [is.simm.uis.web.desktop.views.workspace-creation :as workspace]
               :clj [org.replikativ.spindel.dom.elements :as el])))

(defn render-new-kb-content [data]
  #?(:cljs (workspace/render (assoc data :landing :memory :tab-type :new-kb))
     :clj (el/div {} "New knowledge workspace")))
