(ns is.simm.model.file-outputs
  "Typed Proposal-owned file manifests. Exact bodies live behind store refs."
  (:require [datahike.api :as d]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.knowledge-scope :as scope]))

(defn owner
  "Unique canonical Proposal/component owner, or nil. Metadata identity only,
   NOT authorization. A manifest is not an independent room or store scope."
  [db scope]
  (when (uuid? scope)
    (let [rows (vec (take 2 (d/q '[:find ?proposal ?room ?authority ?manifest-room ?fork ?p ?manifest
                                   :in $ ?scope
                                   :where [?fork :proposal.fork/scope ?scope]
                                   [?fork :proposal.fork/system-type :file-output]
                                   [?fork :proposal.fork/authority-scope ?authority]
                                   [?fork :proposal.fork/file-output ?manifest]
                                   [?manifest :file-output/id ?scope]
                                   [?manifest :file-output/room ?manifest-room]
                                   [?p :proposal/forks ?fork]
                                   [?p :proposal/id ?proposal] [?p :proposal/room ?room]] db scope)))
          [proposal room authority manifest-room fork proposal-eid manifest-eid] (first rows)]
      (when (and (= 1 (count rows)) (uuid? proposal) (uuid? room) (= room authority manifest-room))
        {:proposal proposal :room room :fork-eid fork
         :proposal-eid proposal-eid :manifest-eid manifest-eid}))))

(def content-schema
  [:map {:closed true} [:content :uuid] [:bytes [:int {:min 0 :max 1048576}]] [:mode [:= 33188]]])

(def present-schema
  (conj content-schema [:state [:= :present]]))

(def v1-manifest-schema
  [:map {:closed true} [:id :uuid] [:version [:= 1]]
   [:request :uuid] [:room :uuid] [:attempt :uuid] [:environment :uuid]
   [:workflow :uuid] [:basis :uuid] [:output :uuid]
   [:target publication/target-schema]
   [:entries [:map-of {:max 100} [:string {:min 1 :max 512}]
              [:map {:closed true}
               [:before [:or [:map {:closed true} [:state [:= :absent]]] present-schema]]
               [:after content-schema]]]]])

(def manifest-schema
  [:or v1-manifest-schema
   (into [:map {:closed true} [:scope scope/schema]]
         (map #(if (= :version (first %)) [:version [:= 2]] %)
              (drop 2 v1-manifest-schema)))])

(def schema
  (into
   (mapv (fn [[attr type]]
           (cond-> {:db/ident attr :db/valueType type :db/cardinality :db.cardinality/one}
             (= attr :file-output/id) (assoc :db/unique :db.unique/value)))
         [[:file-output/output-root :db.type/string] [:file-output/destination-root :db.type/string]
          [:file-output/id :db.type/uuid] [:file-output/version :db.type/long]
          [:file-output/request :db.type/uuid] [:file-output/room :db.type/uuid]
          [:file-output/attempt :db.type/uuid] [:file-output/environment :db.type/uuid]
          [:file-output/workflow :db.type/uuid] [:file-output/basis :db.type/uuid]
          [:file-output/output :db.type/uuid] [:file-output/repository :db.type/uuid]
          [:file-output/store :db.type/uuid] [:file-output/backend :db.type/keyword]
          [:file-output/branch :db.type/keyword] [:file-output/ref :db.type/string]
          [:file-output/registry-system :db.type/uuid] [:file-output/runtime-system :db.type/string]
          [:file-output.entry/path :db.type/string] [:file-output.entry/before-state :db.type/keyword]
          [:file-output.entry/before-content :db.type/uuid] [:file-output.entry/before-bytes :db.type/long]
          [:file-output.entry/before-mode :db.type/long] [:file-output.entry/after-content :db.type/uuid]
          [:file-output.entry/after-bytes :db.type/long] [:file-output.entry/after-mode :db.type/long]
          [:file-output.entry/body-ref :db.type/store-ref]])
   [{:db/ident :file-output/exclude-prefixes :db/valueType :db.type/string
     :db/cardinality :db.cardinality/many}
    {:db/ident :proposal.fork/file-output :db/valueType :db.type/ref
     :db/cardinality :db.cardinality/one :db/isComponent true}
    {:db/ident :file-output/entries :db/valueType :db.type/ref
     :db/cardinality :db.cardinality/many :db/isComponent true}]))

(defn entity
  "Typed manifest plus one GC-tracked reference per before/after body pair.
   Caller validates and publishes the references under the store's GC guard."
  [manifest refs]
  (merge
   (when (= 2 (:version manifest))
     {:file-output/output-root (get-in manifest [:scope :output-root])
      :file-output/destination-root (get-in manifest [:scope :destination-root])
      :file-output/exclude-prefixes (get-in manifest [:scope :exclude-prefixes])})
   (into {} (for [k [:id :version :request :room :attempt :environment :workflow :basis :output]]
              [(keyword "file-output" (name k)) (get manifest k)]))
   (into {} (map (fn [[k v]] [(keyword "file-output" (name k)) v])) (:target manifest))
   {:file-output/entries
    (mapv (fn [[path {:keys [before after]}]]
            (merge {:file-output.entry/path path :file-output.entry/before-state (:state before)
                    :file-output.entry/body-ref (get refs path)}
                   (into {} (for [[side metadata] [[:before before] [:after after]]
                                  k [:content :bytes :mode] :when (contains? metadata k)]
                              [(keyword "file-output.entry" (str (name side) "-" (name k))) (get metadata k)]))))
          (:entries manifest))}))

(defn metadata
  "Reconstruct portable metadata from typed rows, never from a cached JSON/EDN string."
  [row]
  (merge
   (when (= 2 (:file-output/version row))
     {:scope {:version 2 :output-root (:file-output/output-root row)
              :destination-root (:file-output/destination-root row)
              :exclude-prefixes (vec (sort (:file-output/exclude-prefixes row)))}})
   (into {} (for [k [:id :version :request :room :attempt :environment :workflow :basis :output]]
              [k (get row (keyword "file-output" (name k)))]))
   {:target (into {} (for [k [:repository :store :backend :branch :ref :registry-system :runtime-system]
                           :when (or (not (#{:registry-system :runtime-system} k))
                                     (contains? row (keyword "file-output" (name k))))]
                       [k (get row (keyword "file-output" (name k)))]))
    :entries (into (sorted-map)
                   (map (fn [entry]
                          [(:file-output.entry/path entry)
                           {:before (cond-> {:state (:file-output.entry/before-state entry)}
                                      (= :present (:file-output.entry/before-state entry))
                                      (merge (into {} (for [k [:content :bytes :mode]]
                                                        [k (get entry (keyword "file-output.entry" (str "before-" (name k))))]))))
                            :after (into {} (for [k [:content :bytes :mode]]
                                              [k (get entry (keyword "file-output.entry" (str "after-" (name k))))]))}]))
                   (:file-output/entries row))}))
