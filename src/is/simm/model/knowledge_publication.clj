(ns is.simm.model.knowledge-publication
  "Portable integrity contract for host-retained wiki occupancy; not authority."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [malli.core :as m]))

(def max-bytes (* 8 1024 1024))
(def max-entries 20000)

(defn- wiki-path? [path]
  (and (string? path) (<= 1 (count path) 512)
       (or (= "wiki" path) (str/starts-with? path "wiki/"))
       (not (re-find #"[\\\p{Cntrl}]" path))
       (every? #(and (not (str/blank? %)) (not (#{"." ".."} %)))
               (str/split path #"/" -1))))

(def target-schema
  [:map {:closed true}
   [:repository :uuid] [:store :uuid] [:backend :keyword] [:branch :keyword]
   [:ref [:string {:min 1 :max 4096}]]])

(def schema
  [:map {:closed true}
   [:id :uuid] [:version [:= 1]] [:root [:= "wiki"]]
   [:target target-schema]
   [:files [:map-of {:max max-entries} [:fn wiki-path?]
            [:map {:closed true} [:content :uuid]
             [:size [:int {:min 0 :max Long/MAX_VALUE}]]
             [:mode [:int {:min 0 :max Integer/MAX_VALUE}]]]]]
   [:directories [:vector {:max max-entries} [:fn wiki-path?]]]])

(defn validate!
  "Validate structure, bounded occupancy and digest before treating as evidence.
   Does not authorize access, prove host origin, or permit adoption."
  [basis]
  (when-not (and (m/validate schema basis)
                 (not (str/blank? (get-in basis [:target :ref])))
                 (<= (+ (count (:files basis)) (count (:directories basis))) max-entries)
                 (= (count (:directories basis)) (count (set (:directories basis))))
                 (<= (alength (.getBytes ^String (pr-str basis) "UTF-8")) max-bytes)
                 (= (:id basis)
                    (hasch/uuid [:simmis/knowledge-publication-basis-v1 (dissoc basis :id)])))
    (throw (ex-info "Invalid host publication basis" {:type ::invalid})))
  basis)
