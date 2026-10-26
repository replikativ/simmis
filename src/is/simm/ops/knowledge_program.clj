(ns is.simm.ops.knowledge-program
  "Shared inert program description from bounded saved workflow bytes.
   No authority, evidence capture, discovery, pin, provider or writer."
  (:require [hasch.core :as hasch]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.workflow-runs :as runs]))

(defn describe
  "HOST supplies authenticated author, canonical room and validated target.
   Source revision/bundle checks belong to that HOST boundary. Snapshot bytes
   remain inert; default parameters define the same program as admission."
  [by room target snapshot name expected-bundle epoch]
  (let [planned (runs/plan-source snapshot {:name name :expected-bundle expected-bundle})
        policy (some-> (:selection-policy planned) inputs/normalize-policy)]
    (when-not policy
      (throw (ex-info "Knowledge policy required" {:type ::missing-policy})))
    (when (contains? (:inputs planned) "/in/knowledge/manifest.edn")
      (throw (ex-info "Benchmark fixtures are not a live stream" {:type ::benchmark-inputs})))
    {:planned planned :policy policy
     :value (streams/descriptor
             {:by by :room room :target (pins/target-id target) :policy (hasch/uuid policy)
              :program (hasch/uuid [:simmis/knowledge-program-v1
                                   (select-keys (:workflow planned) [:name :bundle :case :params])
                                   (:task planned) (:inputs planned)]) :epoch epoch})}))
