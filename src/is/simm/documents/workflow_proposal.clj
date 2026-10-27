(ns is.simm.documents.workflow-proposal
  "Inert saved setup intent. Reading it never adopts files or starts work."
  (:require [clojure.edn :as edn])
  (:import [java.io PushbackReader StringReader]))

(defn- invalid! []
  (throw (ex-info "Repair proposal.edn: use one v1 setup map with an action, explicit schedule cadence and optional model, case and finite limits." {})))

(defn read-intent [text]
  (when text
    (when-not (and (string? text) (<= (alength (.getBytes ^String text "UTF-8")) 4096)) (invalid!))
    (try
      (let [reader (PushbackReader. (StringReader. text))
            eof (Object.)
            options {:eof eof :readers {} :default (fn [& _] (invalid!))}
            value (edn/read options reader)]
        (when-not (and (identical? eof (edn/read options reader)) (map? value)
                       (every? #{:version :action :cadence :model :case :limits} (keys value))
                       (= 1 (:version value)) (#{:run :schedule} (:action value))
                       (or (nil? (:model value)) (and (string? (:model value)) (<= 1 (count (:model value)) 200)))
                       (or (nil? (:case value)) (and (string? (:case value)) (<= 1 (count (:case value)) 200)))
                       (if (= :schedule (:action value))
                         (and (map? (:cadence value))
                              (every? #{:interval-ms :every :at :tz :on :on-day} (keys (:cadence value)))
                              (string? (get-in value [:cadence :tz])))
                         (not (contains? value :cadence)))
                       (or (not (contains? value :limits))
                           (and (map? (:limits value))
                                (every? #{:budget-microdollars :max-dispatches :max-model-steps :timeout-ms} (keys (:limits value)))
                                (every? #(and (integer? %) (pos? %) (<= % 5000000)) (vals (:limits value))))))
          (invalid!))
        value)
      (catch Exception _ (invalid!))
      (catch StackOverflowError _ (invalid!)))))
