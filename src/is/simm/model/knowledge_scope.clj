(ns is.simm.model.knowledge-scope
  "Versioned portable publication scope; integrity data, never authority."
  (:require [clojure.string :as str]
            [is.simm.documents.files :as files]
            [malli.core :as m]))

(def schema
  [:map {:closed true} [:version [:= 2]] [:output-root [:= "out/knowledge"]]
   [:destination-root [:string {:max 512}]]
   [:exclude-prefixes [:vector {:max 32} [:string {:min 1 :max 512}]]]])

(def reserved ["workflows" "in" "out" ".git" ".geschichte"])

(defn contains-path? [root path]
  (or (= "" root) (= root path) (str/starts-with? path (str root "/"))))

(defn normalize! [scope]
  (when-not (m/validate schema scope)
    (throw (ex-info "Invalid knowledge output scope" {:type ::invalid})))
  (when-not (= "" (:destination-root scope)) (files/path! (:destination-root scope)))
  (doseq [path (:exclude-prefixes scope)] (files/path! path))
  (when (some #(contains-path? % (:destination-root scope)) reserved)
    (throw (ex-info "Knowledge destination occupies a reserved namespace" {:type ::invalid})))
  (let [normalized (update scope :exclude-prefixes #(vec (sort (distinct %))))]
    (when-not (= scope normalized)
      (throw (ex-info "Knowledge scope must be canonical" {:type ::invalid})))
    scope))

(defn target-path! [scope path]
  (normalize! scope)
  (files/path! path)
  (when-not (and (contains-path? (:destination-root scope) path)
                 (not= (:destination-root scope) path)
                 (re-find #"(?i)\.(md|markdown)$" path)
                 (not-any? #(contains-path? % path) (concat reserved (:exclude-prefixes scope))))
    (throw (ex-info "Output is outside the admitted knowledge scope" {:type ::invalid :path path})))
  path)
