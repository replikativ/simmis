(ns is.simm.documents.saved-view-fences
  "Inert saved-fence discovery. This parses declarations only, without reading
   other files, querying a DB, installing schema or evaluating code."
  (:require [is.simm.documents.code-cells :as cells]
            [is.simm.documents.saved-view-contract :as contract]
            #?(:clj [is.simm.documents.saved-view-spec :as spec])))

(defn fence? [node]
  (and (= :FencedCodeBlock (:kind node))
       (= "simmis-view" (cells/language (:language node)))))

#?(:clj
   (defn analyze
     "Only the first four parsed fences are candidates; duplicate identities
      within that mounted prefix are invalid. Later fences stay plain source."
     [nodes]
     (let [entries
           (mapv (fn [node]
                   (let [base (select-keys node [:from :to])]
                     (try (let [descriptor (spec/decode (:literal node))]
                            (assoc base :status :ready :view (:id descriptor) :descriptor descriptor))
                          (catch clojure.lang.ExceptionInfo error
                            (if (= :is.simm.documents.saved-view-spec/refused (:type (ex-data error)))
                              (assoc base :status :invalid :reason :invalid-declaration)
                              (throw error))))))
                 (take contract/max-mounted
                       (filter fence? (mapcat #(tree-seq (comp seq :children) :children %) nodes))))
           counts (frequencies (keep :view entries))]
       (mapv (fn [entry]
               (if (> (get counts (:view entry) 0) 1)
                 (-> entry (dissoc :descriptor) (assoc :status :invalid :reason :duplicate-view-identity))
                 entry)) entries))))
