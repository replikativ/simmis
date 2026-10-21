(ns is.simm.documents.canonical
  "Canonical typed stage of the existing immutable document-view pipeline.
   No source reads/writes, schema installation, relation guessing or deletion."
  (:require [datahike.api :as d]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.constraints :as constraints]
            [is.simm.documents.metadata :as metadata]
            [is.simm.documents.properties :as properties]
            [is.simm.model.seed :as seed]))

(def profile {:id :simmis/canonical-markdown :version 1
              :candidate-codec properties/candidate-profile :constraint-codec constraints/profile
              :page-role (:entity/uuid seed/object-Page)
              :block-role (:entity/uuid seed/object-Block)
              :title-property (:entity/uuid seed/morphism-Page-title)})
(def ^:private max-scopes 256)
(def ^:private max-entities 20000)

(defn- refuse! [reason]
  (throw (ex-info "Canonical knowledge projection refused" {:type ::refused :reason reason})))

(defn declaration
  "Shared implicit structural role for a decoded page/explicit-block scope.
   This constructs declarations only; it does not certify a document or type."
  [kind scope]
  (let [role (case kind :page (:page-role profile) :block (:block-role profile)
                   (refuse! :invalid-scope-kind))]
    {:types (vec (sort (set (conj (:types scope []) role))))
     :bindings (:bindings scope {})}))

(defn compile-declaration
  "Compile against the trusted DB and validate this codec's structural roles.
   Shared by canonical projection and source-property inspection."
  [db declarations]
  (let [compiled (properties/compile-scope db declarations)]
    (doseq [[id name] {(:page-role profile) "S/Page" (:block-role profile) "S/Block"}
            :let [row (get-in compiled [:types id])]
            :when row]
      (when-not (= name (:entity/name row)) (refuse! :changed-structural-role)))
    compiled))

(defn- compile-once [db cache declarations]
  (if-let [compiled (get cache declarations)]
    [compiled cache]
    (do
      (when (>= (count cache) max-scopes) (refuse! :too-many-typed-scopes))
      (let [compiled (compile-declaration db declarations)]
        [compiled (assoc cache declarations compiled)]))))

(defn page-candidate
  "Stage a page scope with its trusted generated path title. Shared by the
   canonical pipeline and host property inspection; no resolution or writes."
  [compiled raw path]
  (let [id (:title-property profile)
        title (get-in compiled [:properties id])
        m (:morphism title)]
    ;; A generated slot has a fixed trusted definition, not a user alias or
    ;; suppression of arbitrary required diagnostics. Its value participates
    ;; in the SAME strict decoding and later uniqueness preflight.
    (when-not (and (= :S.Page/title (:attr title)) (:owned? title)
                   (= "S/Page/title" (:entity/name m))
                   (= (:page-role profile) (get-in m [:morphism/src :entity/uuid]))
                   (= :one (:cardinality title))
                   (= :db.type/string (get-in title [:schema :db/valueType]))
                   (nil? (:morphism/validation m))
                   (nil? (get-in m [:morphism/dst :object/base-type]))
                   (empty? (get-in m [:morphism/dst :object/enum-values])))
      (refuse! :unsupported-generated-title-definition))
    (when (some #(= :S.Page/title (:attr %)) (vals (:bindings compiled)))
      (refuse! :file-binding-targets-generated-title))
    (let [slot [::generated id]]
      ;; The generated title now carries the same pending uniqueness claim as
      ;; every ordinary unique property. No schema constraint is stripped.
      (properties/decode-candidate (update compiled :bindings #(assoc (into {} %) slot title))
                               (assoc (into {} raw) slot path)))))

(defn- resolve-values [db result]
  (if (seq (:diagnostics result))
    result
    (try
      (let [entities (vec (mapcat (fn [document]
                                   (let [canonical (:canonical document)]
                                     (cons {:id (:id document) :types (set (keys (:types canonical)))
                                            :candidate (:candidate canonical)}
                                           (for [[id block] (:blocks canonical)]
                                             {:id id :types (set (keys (:types block))) :candidate (:candidate block)}))))
                                 (:documents result)))
            resolved (constraints/resolve-selection db entities)
            by-id (into {} (map (juxt :id identity) (:entities resolved)))
            resolved-scope (fn [id scope]
                             (-> scope (dissoc :candidate)
                                 (merge (select-keys (get by-id id) [:tempid :values]))))]
        (-> result
            (assoc :dependencies (:dependencies resolved))
            (update :documents
                    (fn [documents]
                      (mapv (fn [document]
                              (update document :canonical
                                      (fn [scope]
                                        (-> (resolved-scope (:id document) scope)
                                            (update :blocks #(into {} (for [[id block] %] [id (resolved-scope id block)])))))))
                            documents)))))
      (catch clojure.lang.ExceptionInfo error
        (if (= :is.simm.documents.constraints/refused (:type (ex-data error)))
          (update result :diagnostics conj {:kind :invalid-selection-constraints :reason (:reason (ex-data error))})
          (throw error))))))

(defn- entity! [db id]
  (when (seq (d/datoms db :avet :entity/uuid id))
    (refuse! :unmigrated-entity-identity-collision)))

(defn source-available?
  "Recheck relevant source ownership without invalidating for unrelated writes.
   Only call on a canonical source snapshot, not this view's own projection."
  [db documents]
  (try
    (let [ids (vec (take (inc max-entities)
                         (mapcat (fn [document]
                                   (cons (:id document)
                                         (for [block (mapcat #(tree-seq (comp seq :children) :children %)
                                                              (get-in document [:semantic :blocks]))
                                               :when (= :explicit (:identity-status block))]
                                           (:id block)))) documents)))]
      (when (> (count ids) max-entities) (refuse! :too-many-canonical-entities))
      (doseq [id ids] (entity! db id))
      (doseq [document documents]
        (when (seq (d/datoms db :avet :S.Page/title (:path document)))
          (refuse! :existing-page-title-collision)))
      true)
    (catch clojure.lang.ExceptionInfo error
      (if (= ::refused (:type (ex-data error))) false (throw error)))))

(defn prepare
  "Decorate parsed documents, preserving the single acquisition/parser pipeline.
   Returns diagnostics and no publishable partial entity DB. All canonical
   identities must be explicit and absent from the canonical base. Syntax and
   provisional groups remain noncanonical. Scope compilation is bounded across
   the WHOLE selection and deduplicated locally, not in global runtime state."
  [db documents]
  (let [result
        (reduce
         (fn [{:keys [cache count] :as state} document]
           (if (seq (:diagnostics state))
             (reduced state)
             (try
             (let [all-blocks (mapcat #(tree-seq (comp seq :children) :children %)
                                      (get-in document [:semantic :blocks]))
                   explicit (filterv #(= :explicit (:identity-status %)) all-blocks)
                   count (+ count 1 (clojure.core/count explicit))
                   _ (when (> count max-entities) (refuse! :too-many-canonical-entities))
                   _ (when-not (= :explicit (:identity-status document))
                       (refuse! :canonical-document-needs-explicit-identity))
                   _ (entity! db (:id document))
                   _ (doseq [block explicit] (entity! db (:id block)))
                   decoded (metadata/decode (get-in document [:frontmatter :properties])
                                            (set (map :id explicit)))
                   _ (when (= :invalid (:status decoded)) (refuse! :invalid-typed-declarations))
                   page (:page decoded)
                   declarations (declaration :page page)
                   [compiled cache] (compile-once db cache declarations)
                   values (page-candidate compiled (:values page) (:path document))
                   blocks-result
                   (reduce (fn [{:keys [cache] :as state} block]
                             (let [scope (get (:blocks decoded) (:id block) {})
                                   declarations (declaration :block scope)
                                   [compiled cache] (compile-once db cache declarations)
                                   values (properties/decode-candidate compiled (:values scope {}))]
                               (-> state (assoc :cache cache)
                                   (assoc-in [:blocks (:id block)]
                                             {:types (:types compiled) :candidate values})
                                   (update :diagnostics into (:diagnostics values)))))
                           {:cache cache :blocks {} :diagnostics []} explicit)
                   diagnostics (into (:diagnostics values) (:diagnostics blocks-result))]
               (-> state
                   (assoc :cache (:cache blocks-result) :count count)
                   (update :documents conj (assoc document :metadata decoded
                                                   :canonical {:types (:types compiled)
                                                               :candidate values
                                                               :blocks (:blocks blocks-result)}))
                   (update :diagnostics into (map #(assoc % :path (:path document)) diagnostics))))
             (catch clojure.lang.ExceptionInfo error
               ;; Only our known declaration/schema refusals are diagnostics;
               ;; storage/parser/runtime failures must not masquerade as input.
               (if (contains? #{::refused :is.simm.documents.properties/invalid-schema}
                              (:type (ex-data error)))
                 (-> state (update :documents conj document)
                     (update :diagnostics conj {:kind :invalid-canonical-document
                                                :path (:path document)
                                                :reason (or (:reason (ex-data error)) :invalid-schema)}))
                 (throw error))))))
         {:documents [] :cache {} :count 0 :diagnostics []} documents)]
    (-> (resolve-values db result)
        (assoc :scopes (mapv (fn [[declarations compiled]]
                              {:declarations declarations :revision (:revision compiled)})
                            (sort-by (comp pr-str key) (:cache result))))
        (dissoc :cache :count))))

(defn current? [db scopes dependencies own-derived?]
  (try
    (and (every? (fn [{:keys [declarations revision]}]
                   (= revision (:revision (properties/compile-scope db declarations)))) scopes)
         (constraints/current? db dependencies own-derived?))
    (catch clojure.lang.ExceptionInfo error
      (if (= :is.simm.documents.properties/invalid-schema (:type (ex-data error)))
        false
        (throw error)))))

(defn block-tx [canonical block]
  (let [tx (blocks/entity-tx (dissoc block :children))
        own (get-in canonical [:blocks (:id block)])]
    (cond-> (dissoc tx :document.block/children)
      own (merge (:values own) {:db/id (:tempid own) :entity/uuid (:id block)
                                :instance/of-role (mapv :db/id (vals (:types own)))})
      (seq (:children block))
      (assoc :document.block/children (mapv #(block-tx canonical %) (:children block))))))

(defn document-tx [document]
  (let [canonical (:canonical document)]
    (merge (:values canonical)
           {:db/id (:tempid canonical) :entity/uuid (:id document)
            :instance/of-role (mapv :db/id (vals (:types canonical)))
            :document/blocks (mapv #(block-tx canonical %) (get-in document [:semantic :blocks]))})))
