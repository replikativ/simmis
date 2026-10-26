(ns is.simm.model.knowledge-input-check
  "Closed, redacted metadata observation. This is not admission authority."
  (:require [malli.core :as m]))

(def ^:private natural [:int {:min 0}])
(def ^:private facts
  [[:context [:map {:closed true} [:files natural] [:bytes natural]
              [:file-limit natural] [:byte-limit natural]]]
   [:head [:map {:closed true} [:bytes natural] [:file-limit natural] [:batch-byte-limit natural]]]
   [:reserved [:map {:closed true} [:files natural] [:bytes natural]]]
   [:limits [:map {:closed true} [:files natural] [:bytes natural]]]])

(def schema
  (into [:multi {:dispatch :status}]
        (concat
         (for [status [:empty :not-checked :unavailable]]
           [status [:map {:closed true} [:status [:= status]]]])
         [[:held (into [:map {:closed true} [:status [:= :held]]
                        [:code [:enum :context-exceeds-budget :no-trigger-capacity
                                :head-item-exceeds-budget :execution-envelope-exceeds-budget]]]
                       facts)]
          [:fits (into [:map {:closed true} [:status [:= :fits]]
                        [:selected [:map {:closed true} [:files natural] [:bytes natural]]]
                        [:execution [:map {:closed true} [:files natural] [:bytes natural] [:manifest-bytes natural]]]]
                       facts)]])))

(defn valid? [value] (m/validate schema value))
