(ns is.simm.model.knowledge-publication
  "Portable integrity contract for host-retained wiki occupancy; not authority."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-scope :as scope]
            [is.simm.documents.files :as files]
            [malli.core :as m]))

(def max-bytes (* 8 1024 1024))
(def max-entries 20000)

(defn- wiki-path? [path]
  (and (string? path) (<= 1 (count path) 512)
       (or (= "wiki" path) (str/starts-with? path "wiki/"))
       (not (re-find #"[\\\p{Cntrl}]" path))
       (every? #(and (not (str/blank? %)) (not (#{"." ".."} %)))
               (str/split path #"/" -1))))

(defn host-target?
  "Explicit admission identity, not current permission or adoption authority."
  [target]
  (and (uuid? (:registry-system target)) (string? (:runtime-system target))
       (<= 1 (count (:runtime-system target)) 4096)
       (not (str/blank? (:runtime-system target)))))

(def target-schema
  [:and
   [:map {:closed true}
    [:repository :uuid] [:store :uuid] [:backend :keyword] [:branch :keyword]
    [:ref [:string {:min 1 :max 4096}]]
    [:registry-system {:optional true} :uuid]
    [:runtime-system {:optional true} [:string {:min 1 :max 4096}]]]
   [:fn (fn [target]
          (or (and (not (contains? target :registry-system))
                   (not (contains? target :runtime-system)))
              (host-target? target)))]])

(def v1-schema
  [:map {:closed true}
   [:id :uuid] [:version [:= 1]] [:root [:= "wiki"]]
   [:target target-schema]
   [:files [:map-of {:max max-entries} [:fn wiki-path?]
            [:map {:closed true} [:content :uuid]
             [:size [:int {:min 0 :max Long/MAX_VALUE}]]
             [:mode [:int {:min 0 :max Integer/MAX_VALUE}]]]]]
   [:directories [:vector {:max max-entries} [:fn wiki-path?]]]])

(def v2-schema
  [:map {:closed true} [:id :uuid] [:version [:= 2]] [:root [:string {:max 512}]]
   [:scope scope/schema] [:target target-schema]
   [:files [:map-of {:max max-entries} [:string {:min 1 :max 512}]
            [:map {:closed true} [:content :uuid] [:size [:int {:min 0 :max Long/MAX_VALUE}]]
             [:mode [:int {:min 0 :max Integer/MAX_VALUE}]]]]]
   [:directories [:vector {:max max-entries} [:string {:min 1 :max 512}]]]])

(def schema [:or v1-schema v2-schema])

(defn digest-tag [version]
  (case version 1 :simmis/knowledge-publication-basis-v1 2 :simmis/knowledge-publication-basis-v2))

(defn validate!
  "Validate structure, bounded occupancy and digest before treating as evidence.
   Does not authorize access, prove host origin, or permit adoption."
  [basis]
  (when (= 2 (:version basis))
    (scope/normalize! (:scope basis))
    (when-not (= (:root basis) (get-in basis [:scope :destination-root]))
      (throw (ex-info "Publication scope root mismatch" {:type ::invalid})))
    (doseq [path (concat (keys (:files basis)) (:directories basis))]
      (files/path! path)
      (when-not (or (scope/contains-path? (:root basis) path)
                    (scope/contains-path? path (:root basis)))
        (throw (ex-info "Publication occupancy outside scope" {:type ::invalid})))))
  (when-not (and (m/validate schema basis)
                 (not (str/blank? (get-in basis [:target :ref])))
                 (<= (+ (count (:files basis)) (count (:directories basis))) max-entries)
                 (= (count (:directories basis)) (count (set (:directories basis))))
                 (<= (alength (.getBytes ^String (pr-str basis) "UTF-8")) max-bytes)
                 (= (:id basis)
                    (hasch/uuid [(digest-tag (:version basis)) (dissoc basis :id)])))
    (throw (ex-info "Invalid host publication basis" {:type ::invalid})))
  basis)
