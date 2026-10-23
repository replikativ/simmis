(ns is.simm.uis.web.desktop.chart-view
  "Two DOM adapters for the same closed pure scene: Spindel's document Read
   view and Tiptap's existing foreign NodeView boundary. No renderer runtime."
  (:require [is.simm.documents.chart-scene :as chart]
            [org.replikativ.spindel.dom.addressing :as addressing]
            [org.replikativ.spindel.dom.elements :as el]))

(defn- render-scene [tree]
  (if (string? tree) tree
      (let [addr (addressing/current-element-address {:file "simmis/chart-scene" :line 1 :column 1})]
        (addressing/with-parent-addr-fn
          addr
          #(el/build-element (:tag tree) addr (:attrs tree)
                             (mapv (fn [index child]
                                     (addressing/with-slot-fn index (fn [] (render-scene child))))
                                   (range) (:children tree)))))))

(defn view [data] (render-scene (chart/scene data)))

(def ^:private svg-tags #{:svg :path :circle :rect :text})

(defn- native-node [tree svg?]
  (if (string? tree)
    (.createTextNode js/document tree)
    (let [tag (:tag tree) svg? (or svg? (= :svg tag))
          ;; All tags/attributes come from chart/scene, never from data fields.
          dom (if svg? (.createElementNS js/document "http://www.w3.org/2000/svg" (name tag))
                  (.createElement js/document (name tag)))]
      (when (and svg? (not (contains? svg-tags tag)))
        (throw (js/Error. "Unsupported chart scene element")))
      (doseq [[key value] (:attrs tree)] (.setAttribute dom (name key) (str value)))
      (doseq [child (:children tree)] (.appendChild dom (native-node child svg?)))
      dom)))

(defn paint!
  "Replace one element-owned inert NodeView projection. No listener, timer,
   observer, semantic state, native editor transaction or source mutation."
  [container data]
  (if data
    (.replaceChildren container (native-node (chart/scene data) false))
    (.replaceChildren container)))
