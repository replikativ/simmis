(ns is.simm.documents.summary-views
  "Fixed read-only summary table. Caller owns Room, source bytes and target."
  (:require [hasch.core :as hasch]
            [is.simm.documents.saved-view-contract :as contract]
            [is.simm.documents.saved-view-spec :as spec]
            [is.simm.model.summary-periods :as periods]))

(defn- text-cell [text]
  {:kind :text :text text :complete? true})

(defn prepare [descriptor context listing]
  (let [descriptor (spec/validate descriptor)
        source (assoc context :kind :room-summaries)
        selected (vec (take (:limit descriptor) (:rows listing)))
        rows (mapv
              (fn [{:keys [record payload path publication]}]
                (let [period (get-in payload [:selection :period])
                      summary (get-in payload [:report :summary :text])
                      cells (mapv (fn [{:keys [kind]}]
                                    (case kind
                                      :period (if path
                                                {:kind :page :id (:room.summary/node record) :path path
                                                 :text (periods/day-label period)}
                                                (text-cell (periods/day-label period)))
                                      :summary {:kind :text :text (subs summary 0 (min 512 (count summary)))
                                                :complete? (<= (count summary) 512)}
                                      :status (text-cell (if (= :ready (:room.summary/state record)) "Complete" "Partial"))
                                      :originals {:kind :number :text (str (count (get-in payload [:selection :coverage])))}
                                      :model (text-cell (:room.summary/model record))
                                      :publication (text-cell (name publication)))) (:columns descriptor))]
                  (cond-> {:id (:room.summary/revision record) :summary (:room.summary/revision record) :cells cells}
                    path (assoc :path path)))) selected)]
    {:profile contract/profile :status :ready :descriptor descriptor :source source
     :basis (hasch/uuid [:simmis/summary-view-v1 descriptor source
                        (mapv #(select-keys % [:record :path :publication]) selected)])
     :columns (mapv #(assoc % :label (get contract/summary-columns (:kind %))) (:columns descriptor))
     :rows rows :total (:total listing)
     :complete? (and (:complete? listing) (<= (:total listing) (:limit descriptor)))
     :display {:complete? (every? #(every? (fn [cell] (not (false? (:complete? cell)))) (:cells %)) rows)}
     :snapshot? false}))
