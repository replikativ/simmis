(ns is.simm.model.attributes
  "Declaring an attribute in a KB, for a writer that does not get to choose its
   value type.

   An attribute's `:db/valueType` is append-only schema, global to the KB: it
   outlives every fork and every grant, so whoever chooses it fixes the
   attribute for good. A declaration therefore names the attribute's CODOMAIN
   object (`\"S/String\"`, `\"S/Number\"`, `\"S/Page\"` for a reference) and the
   type is DERIVED from it (`schema/codomain->db-type`, `doc/data-model.md`
   \"Schema types\") — the rule a type page's `add-property` follows. Two
   callers share this: a wiki widget (`block-remote/widget-install-attr`) and an
   agent's `kb/install-attr!` (`room-agents`). Who may declare is theirs to
   check (`:merge` on the KB in both); WHAT may be declared is this namespace's."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [is.simm.model.morphism :as mor]
            [is.simm.model.schema :as schema]))

(def reserved-ident-namespaces
  "Namespaces whose attributes the store's own schema owns. A declaration is
   of a new attribute of one's own (`:todo/text`), not of one in these."
  #{"db" "tx" "entity" "instance" "object" "morphism" "block" "kb" "category"})

(defn- invalid! [msg data]
  (throw (ex-info msg (assoc data :type :validation-error))))

(defn existing-value-type
  "The value type of `ident` in `db`, or nil when it is not declared."
  [db ident]
  (d/q '[:find ?vt . :in $ ?i :where [?a :db/ident ?i] [?a :db/valueType ?vt]] db ident))

(defn declaration
  "What declaring `ident` in `db` with `codomain` and `cardinality` (`:one` or
   `:many`, default `:one`) transacts — or that it already exists.

   Returns `{:status :exists :ident :value-type}` for a declared attribute (a
   declaration is idempotent and never changes one), else
   `{:status :new :ident :value-type :tx-data}`. Throws `:validation-error` for
   an unqualified ident, one in a reserved namespace, a codomain that is not
   the NAME of an object in this store (a `:db.type/*` keyword included: the
   caller never chooses the type), or a cardinality other than one/many."
  [db {:keys [ident codomain cardinality]}]
  (when-not (and (keyword? ident) (namespace ident))
    (invalid! "an attribute is a qualified keyword" {:ident (pr-str ident)}))
  (if-let [existing (existing-value-type db ident)]
    {:status :exists :ident ident :value-type existing}
    (let [ns-root (first (str/split (namespace ident) #"\."))]
      (when (or (contains? reserved-ident-namespaces (namespace ident))
                (= "db" ns-root))
        (invalid! "that namespace belongs to the store's own schema" {:ident (str ident)}))
      (when-not (string? codomain)
        (invalid! (str "name the codomain object, e.g. \"S/String\" — the value type is "
                       "derived from it, never sent")
                  {:codomain (pr-str codomain)}))
      (when-not (d/q '[:find ?o . :in $ ?n
                       :where [?o :entity/name ?n] [?o :object/of-category _]]
                     db codomain)
        (invalid! "no such object in this store" {:codomain codomain}))
      (when-not (contains? #{nil :one :many :db.cardinality/one :db.cardinality/many} cardinality)
        (invalid! "cardinality is :one or :many" {:cardinality (pr-str cardinality)}))
      (let [many? (contains? #{:many :db.cardinality/many} cardinality)
            mname (schema/attr-ident->morphism ident)
            _ (when-not (= ident (mor/name->attr-ident mname))
                (invalid! "attribute name does not round-trip to a morphism" {:ident (str ident)}))
            value-type (schema/codomain->db-type codomain)
            morphism? (d/q '[:find ?m . :in $ ?n :where [?m :entity/name ?n]] db mname)]
        {:status :new
         :ident ident
         :value-type value-type
         :tx-data (cond-> [{:db/ident ident
                            :db/valueType value-type
                            :db/cardinality (if many? :db.cardinality/many :db.cardinality/one)
                            :db/doc (str "Property: " mname)}]
                    (not morphism?)
                    (conj {:entity/uuid (random-uuid)
                           :entity/name mname
                           :entity/created-at (java.util.Date.)
                           :morphism/dst [:entity/name codomain]
                           :morphism/cardinality (if many? :many :one)}))}))))
