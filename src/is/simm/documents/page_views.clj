(ns is.simm.documents.page-views
  "Fixed read-only page tables from one already-authorized immutable source.
   Returns a fact allowlist, never the derived DB, native entity IDs or write
   commands. Detailed edit models still come from the ordinary property reader."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.documents.canonical :as canonical]
            [is.simm.documents.saved-view-spec :as spec]
            [is.simm.documents.properties :as properties]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]))

(defn- title [path]
  (str/replace (last (str/split path #"/")) #"(?i)\.(md|markdown)$" ""))

(def max-cell-characters 1024)
(def max-display-characters 65536)

(def ^:private known-reasons
  #{:too-many-files :too-many-source-bytes :too-many-worktree-entries :too-many-syntax-nodes
    :unprovisioned-canonical-space :unknown-or-nonentity-type :unsupported-column-property
    :invalid-property-definition :invalid-source :invalid-schema :invalid-typed-declarations
    :canonical-document-needs-explicit-identity :page-not-in-selection})

(def ^:private diagnostic-kinds
  #{:invalid-canonical-document :invalid-selection-constraints :invalid-document-id
    :duplicate-document-id :duplicate-semantic-id :duplicate-block-id :invalid-block-marker
    :nested-block-marker :unclosed-block :unmatched-block-close :ambiguous-property-alias
    :duplicate-type-assignment :invalid-block-metadata :invalid-metadata-input :invalid-metadata-scope
    :typed-document-needs-explicit-identity :unsupported-metadata-field :unsupported-metadata-version
    :missing-required-property :required-owner-managed-property :changed-document-identity
    :removed-explicit-block-identity})

(defn- safe-reason [value fallback]
  (if (and (keyword? value) (contains? known-reasons value)) value fallback))

(defn- safe-diagnostics [source options diagnostics]
  ;; Invalid-source diagnostics may name only actual metadata paths in this
  ;; exact source selection. An arbitrary helper tag/map is never wire data.
  ;; The extra metadata traversal is only needed for the unavailable path.
  (let [paths (set (keys (:files (view/selection source options))))]
    (mapv (fn [diagnostic]
            (let [kind (:kind diagnostic) path (:path diagnostic)]
              (cond-> {:kind (if (and (keyword? kind) (contains? diagnostic-kinds kind)) kind :invalid-document)
                       :reason (safe-reason (:reason diagnostic) :invalid-source)}
                (and (string? path) (<= (count path) 512) (contains? paths path)) (assoc :path path))))
          (take 20 diagnostics))))

(defn- clip [text limit]
  (let [n (min limit (count text))
        n (if (and (pos? n) (< n (count text))
                   (Character/isHighSurrogate (.charAt ^String text (dec n)))) (dec n) n)]
    (subs text 0 n)))

(defn- text-value [text]
  {:kind :text :text (clip text max-cell-characters)
   :complete? (<= (count text) max-cell-characters)})

(defn- reference-id [projected value]
  (when-let [eid (:db/id value)] (:entity/uuid (d/entity projected eid))))

(defn- reference [projected known-pages value]
  (let [id (reference-id projected value)]
    ;; Never expand arbitrary native/governed entities. Only source pages in
    ;; this already selected projection contribute a label or navigation path.
    (if id
      (cond-> {:kind :reference :id id}
        (get known-pages id) (merge (get known-pages id)))
      {:kind :unresolved-reference})))

(defn- scalar [projected known-pages value-type value]
  (cond
    (nil? value) {:kind :null}
    (= :db.type/ref value-type) (reference projected known-pages value)
    (= :db.type/string value-type) (text-value value)
    (= :db.type/boolean value-type) {:kind :boolean :value value}
    (#{:db.type/long :db.type/double} value-type) {:kind :number :text (str value)}
    (= :db.type/instant value-type) {:kind :instant :text (str (.toInstant ^java.util.Date value))}
    (= :db.type/uuid value-type) {:kind :uuid :text (str value)}
    :else {:kind :unsupported}))

(defn- value-key [projected value-type value]
  (cond
    (= :db.type/ref value-type) (str (reference-id projected value))
    (= :db.type/instant value-type) (.getTime ^java.util.Date value)
    (= :db.type/uuid value-type) (str value)
    (#{:db.type/string :db.type/boolean :db.type/long :db.type/double} value-type) value
    :else ""))

(defn- cell [projected known-pages entity binding]
  (let [value (get entity (:attr binding))
        value-type (get-in binding [:schema :db/valueType])]
    (if (= :many (:cardinality binding))
      (let [items (vec (take 17 (sort-by #(value-key projected value-type %) value)))]
        {:kind :list :items (mapv #(scalar projected known-pages value-type %) (take 16 items))
         :complete? (<= (count items) 16)})
      (scalar projected known-pages value-type value))))

(defn- column-bindings [source columns]
  (when (some #(= (:title-property canonical/profile) (:property %)) columns)
    (throw (ex-info "Use the dedicated page title column"
                    {:type :is.simm.documents.properties/invalid-schema :reason :unsupported-column-property})))
  (let [aliases (into {} (keep (fn [{:keys [property]}] (when property [(str property) property]))) columns)]
    (let [compiled (properties/compile-schema source aliases)]
      (when-not (every? (fn [binding]
                          (and (:owned? binding) (not (get-in binding [:schema :db/isComponent]))
                               (nil? (get-in binding [:morphism :morphism/computation]))))
                        (vals (:bindings compiled)))
        (throw (ex-info "Page view columns must be ordinary source properties"
                        {:type :is.simm.documents.properties/invalid-schema
                         :reason :unsupported-column-property})))
      compiled)))

(defn- column [bindings descriptor]
  (if (= :title (:kind descriptor))
    {:kind :title :label (:label descriptor "Page")}
    (let [binding (get-in bindings [:bindings (str (:property descriptor))])
          label (or (:label descriptor) (last (str/split (get-in binding [:morphism :entity/name]) #"/")))]
      {:property (:property descriptor)
       :label (clip label 128) :label-complete? (<= (count label) 128)
       :value-type (get-in binding [:schema :db/valueType]) :cardinality (:cardinality binding)})))

(defn- budget-cell [value remaining]
  (if (= :list (:kind value))
    (let [[items remaining complete?]
          (reduce (fn [[items remaining complete?] item]
                    (let [[bounded remaining item-complete?] (budget-cell item remaining)]
                      [(conj items bounded) remaining (and complete? item-complete?)]))
                  [[] remaining (:complete? value)] (:items value))]
      [(assoc value :items items :complete? complete?) remaining complete?])
    (let [cost (reduce + 0 (map #(count (get value % "")) [:text :label :path]))]
      (cond
        (<= cost remaining) [value (- remaining cost) (not (false? (:complete? value)))]
        (= :text (:kind value))
        (let [text (clip (:text value) remaining)]
          [(assoc value :text text :complete? false) (- remaining (count text)) false])
        ;; Never truncate numeric precision, UUIDs or navigable paths into a
        ;; different value/target. The row's separate identity remains intact.
        :else [{:kind :truncated} remaining false]))))

(defn- render-rows [projected known-pages bindings descriptor rows]
  (reduce
   (fn [{:keys [remaining] :as result} row]
     (let [entity (d/entity projected [:entity/uuid (:id row)])
           [cells remaining complete?]
           (reduce (fn [[cells remaining complete?] column]
                     (let [value (if (= :title (:kind column))
                                   {:kind :page :id (:id row) :path (:path row) :text (:title row)}
                                   (cell projected known-pages entity
                                         (get-in bindings [:bindings (str (:property column))])))
                           [value remaining value-complete?] (budget-cell value remaining)]
                       [(conj cells value) remaining (and complete? value-complete?)]))
                   [[] remaining true] (:columns descriptor))]
       (-> result (update :rows conj (assoc row :cells cells))
           (assoc :remaining remaining) (update :complete? #(and % complete?)))))
   {:rows [] :remaining max-display-characters :complete? true} rows))

(defn- prepared-table [source descriptor]
  (let [options {:roots (get-in descriptor [:source :roots])
                 :schema-revision spec/profile :entity-profile space/profile}
        wanted-scope (properties/compile-scope source {:types (get-in descriptor [:source :types]) :bindings {}})
        prepared (view/prepare source options)
        base {:profile spec/profile :view (:id descriptor) :descriptor descriptor
              :selection {:roots (get-in descriptor [:source :roots])
                          :source-files (count (get-in prepared [:selection :files]))}}]
    (if-not (= :ready (:status prepared))
      (assoc base :status :unavailable :reason :invalid-source :complete? false
             :diagnostics (safe-diagnostics source options (:diagnostics prepared)))
      (let [projected (:db prepared)
            bindings (column-bindings source (:columns descriptor))
            known-pages (into {} (map (fn [{:keys [id path]}] [id {:label (title path) :path path}]))
                              (:documents prepared))
            wanted-types (set (get-in descriptor [:source :types]))
            wanted-page (get-in descriptor [:source :page])
            selected
            (vec (keep (fn [{:keys [id path revision]}]
                         (let [entity (d/entity projected [:entity/uuid id])
                               types (set (keep :entity/uuid (:instance/of-role entity)))]
                           (when (and (or (nil? wanted-page) (= id wanted-page))
                                      (every? types wanted-types))
                             {:id id :path path :title (title path) :revision revision
                              :entry (get-in prepared [:selection :files path])})))
                       (:documents prepared)))
            direction (get-in descriptor [:order 0 :direction])
            ordered (sort (fn [a b]
                            (let [c (compare (str/lower-case (:title a)) (str/lower-case (:title b)))
                                  c (if (= :desc direction) (- c) c)]
                              (if (zero? c) (compare (str (:id a)) (str (:id b))) c))) selected)
            rows (vec (take (:limit descriptor) ordered))]
        (if (and wanted-page (empty? rows))
          (assoc base :status :unavailable :reason :page-not-in-selection :complete? false)
          (let [rendered (render-rows projected known-pages bindings descriptor rows)]
            (assoc base :status :ready :complete? (<= (count selected) (:limit descriptor))
                   :total (count selected) :rows (:rows rendered)
                   :display {:cell-character-limit max-cell-characters :character-budget max-display-characters
                             :used-characters (- max-display-characters (:remaining rendered))
                             :complete? (:complete? rendered)}
                   :basis (hasch/uuid [spec/profile descriptor (:id prepared) (:revision bindings) (:revision wanted-scope)])
                   :columns (mapv #(column bindings %) (:columns descriptor)))))))))

(defn prepare
  "Fixed projection only. Declaration validation precedes source acquisition.
   Known input/schema/bounds failures yield bounded unavailable diagnostics;
   unexpected store/runtime failures propagate to the normal HOST boundary."
  [source raw-descriptor]
  (let [descriptor (spec/validate raw-descriptor)]
    (when-not (= :pages (get-in descriptor [:source :kind]))
      (throw (ex-info "Page projection requires a page source" {:type ::unsupported-source})))
    (try (prepared-table source descriptor)
         (catch clojure.lang.ExceptionInfo error
           (if (contains? #{:is.simm.documents.view/refused
                            :is.simm.documents.canonical/refused
                            :is.simm.documents.properties/invalid-schema} (:type (ex-data error)))
             {:profile spec/profile :view (:id descriptor) :descriptor descriptor
              :status :unavailable :complete? false
              :reason (safe-reason (:reason (ex-data error)) :invalid-property-definition)}
             (throw error))))))
