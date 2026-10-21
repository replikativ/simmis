(ns is.simm.documents.metadata
  "Inert, versioned Markdown identity/type/property declarations.
   Input is already safely parsed bounded frontmatter. Nothing here resolves
   schema/relations, installs attributes, evaluates code or writes source.
   A UUID-looking value is NOT inferred to be a reference."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [malli.core :as m]
            [is.simm.documents.identity :as identity]))

(def profile {:id :simmis/markdown-metadata :version 1})
(def ^:private uuid-schema [:fn #(some? (identity/canonical-uuid %))])
(def ^:private alias-schema
  [:and [:string {:min 1 :max 128}]
   [:fn #(and (not (str/blank? %)) (not (#{"simmis" "simmis-id"} %)))]])
(def ^:private scope-schema
  [:map
   ;; clj-yaml represents YAML sequences as lazy Clojure sequences, not
   ;; necessarily vectors. Normalize only our declarations, never raw source.
   ["types" {:optional true} [:sequential {:max 64} uuid-schema]]
   ["properties" {:optional true}
    [:map-of {:max 64} alias-schema [:map ["id" uuid-schema] ["type" uuid-schema]]]]])

(defn- unknown-fields [raw known path]
  (mapv (fn [field] {:kind :unsupported-metadata-field :path (conj path field)})
        (sort-by str (set/difference (set (keys raw)) known))))

(defn- scope [raw values path known]
  (if-not (m/validate scope-schema raw)
    {:diagnostics [{:kind :invalid-metadata-scope :path path}]}
    (let [types (mapv identity/canonical-uuid (get raw "types" []))
          bindings (into (sorted-map)
                         (for [[alias binding] (get raw "properties" {})]
                           [alias {:id (identity/canonical-uuid (get binding "id"))
                                   :declaring-type (identity/canonical-uuid (get binding "type"))}]))
          duplicate-properties (->> bindings vals (map :id) frequencies
                                    (keep (fn [[id n]] (when (> n 1) id))) set)
          diagnostics (vec
                       (concat
                        (unknown-fields raw known path)
                        (when (not= (count types) (count (set types)))
                          [{:kind :duplicate-type-assignment :path (conj path "types")}])
                        (for [[alias binding] bindings :when (contains? duplicate-properties (:id binding))]
                          {:kind :ambiguous-property-alias :path (conj path "properties" alias)})
                        (mapcat (fn [[alias binding]]
                                  (unknown-fields binding #{"id" "type"} (conj path "properties" alias)))
                                (get raw "properties" {}))))]
      {:types types :bindings bindings :values values :diagnostics diagnostics})))

(defn decode
  "Decode declarations only. EXPLICIT-BLOCK-IDS comes from valid paired block
   markers, never provisional syntax IDs. Unknown metadata is retained in :raw;
   unknown reserved semantics diagnose instead of silently acquiring v1 meaning.
   :ready means declarations parsed, NOT a valid typed projection. Trusted
   definitions, type compatibility, ownership and references are separate gates."
  [raw explicit-block-ids]
  (let [raw (if (nil? raw) {} raw) reserved (get raw "simmis")
        base {:profile profile :raw raw :document-id (identity/canonical-uuid (get raw "simmis-id"))}]
    (cond
      (not (and (map? raw) (set? explicit-block-ids) (every? uuid? explicit-block-ids)))
      (assoc base :status :invalid :diagnostics [{:kind :invalid-metadata-input}])

      (and (contains? raw "simmis-id") (nil? (:document-id base)))
      (assoc base :status :invalid :diagnostics [{:kind :invalid-document-id}])

      (not (contains? raw "simmis"))
      (assoc base :status :plain :page {:types [] :bindings {} :values (dissoc raw "simmis-id")}
             :blocks {} :diagnostics [])

      (not (and (map? reserved) (= 1 (get reserved "version"))))
      (assoc base :status :invalid :diagnostics [{:kind :unsupported-metadata-version}])

      (not (and (map? (get reserved "blocks" {})) (<= (count (get reserved "blocks" {})) 20000)))
      (assoc base :status :invalid :diagnostics [{:kind :invalid-block-metadata}])

      :else
      (let [page (scope reserved (dissoc raw "simmis" "simmis-id") ["simmis"] #{"version" "types" "properties" "blocks"})
            block-results
            (mapv (fn [[key value]]
                    (let [id (identity/canonical-uuid key) path ["simmis" "blocks" key]]
                      (if-not (and id (contains? explicit-block-ids id) (map? value)
                                   (map? (get value "values" {})))
                        {:diagnostics [{:kind :invalid-block-metadata :path path}]}
                        (assoc (scope value (get value "values" {}) path #{"types" "properties" "values"}) :id id))))
                  (sort-by (comp str key) (get reserved "blocks" {})))
            typed? (some #(or (seq (:types %)) (seq (:bindings %))) (cons page block-results))
            diagnostics (vec (concat (:diagnostics page) (mapcat :diagnostics block-results)
                                     (when (and typed? (nil? (:document-id base)))
                                       [{:kind :typed-document-needs-explicit-identity}])))]
        (assoc base :status (if (seq diagnostics) :invalid :ready)
               :page (dissoc page :diagnostics)
               :blocks (into {} (keep #(when (:id %) [(:id %) (dissoc % :id :diagnostics)])) block-results)
               :diagnostics diagnostics)))))
