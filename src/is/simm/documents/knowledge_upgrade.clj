(ns is.simm.documents.knowledge-upgrade
  "HOST-only preserving foundation upgrade for an ordinary Geschichte store.
   Caller owns original App/Room lifetimes, current authority and quiescence.
   No starter content, source edits, interpreter execution or registry mutation."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [datahike.api :as d]
            [dvergr.rooms.file-tx :as file-tx]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]
            [is.simm.model.seed :as seed]))

(def receipt-schema
  [{:db/ident :document.repository/upgrade :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one}
   {:db/ident :document.repository/upgrade-basis :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one}])

(defn- refuse! [reason]
  (throw (ex-info "Knowledge Memory upgrade held" {:type ::held :reason reason})))

(defn basis [db]
  (hasch/uuid [:simmis/knowledge-upgrade-basis-v1 (file-tx/capture-basis db)]))

(defn- shape [attribute]
  (assoc (select-keys attribute [:db/valueType :db/cardinality :db/unique])
         :db/isComponent (boolean (:db/isComponent attribute))))

(def source-attributes
  ;; Git/file state must remain exact. Payloads are addressed by immutable
  ;; content IDs; do not read large video/pack bodies merely to upgrade schema.
  #{:geschichte.repo/id :geschichte.repo/head :geschichte.repo/name :geschichte.repo/merge-parent
    :geschichte.ref/name :geschichte.ref/target
    :geschichte.git.ref/name :geschichte.git.ref/oid :geschichte.git.ref/peeled
    :geschichte.git.ref/source :geschichte.git.ref/symref-target
    :geschichte.stage/path :geschichte.stage/content :geschichte.stage/mode
    :geschichte.stage/size :geschichte.stage/state
    :geschichte.work/path :geschichte.work/content :geschichte.work/mode :geschichte.work/size
    :geschichte.work-dir/path :geschichte.config/key :geschichte.config/value})

(defn source-id [db]
  (let [rows (vec (take 50001 (mapcat (fn [attribute]
                                      (when (contains? (d/schema db) attribute)
                                        (map (fn [row] [(:e row) attribute (:v row)])
                                             (d/datoms db :aevt attribute))))
                                    (sort source-attributes))))]
    (when (> (count rows) 50000) (refuse! :source-metadata-capacity))
    (hasch/uuid [:simmis/knowledge-upgrade-source-v1 (sort-by #(str (hasch/uuid %)) rows)])))

(defn definition-revision []
  (hasch/uuid [:simmis/knowledge-upgrade-definitions-v1 (profile/foundation-schema)
              (walk/postwalk (fn [value] (if (and (map? value) (contains? value :entity/created-at))
                                          (assoc value :entity/created-at (java.util.Date. 0)) value))
                             (seed/definition-transactions))]))

(defn- ordinary! [db]
  (when-not (and (false? (get-in db [:config :attribute-refs?]))
                 (= :write (get-in db [:config :schema-flexibility]))
                 (uuid? (:geschichte.repo/id (query/repository db))))
    (refuse! :unsupported-store))
  ;; Partial/legacy semantic stores need their own migration. Never overwrite
  ;; reserved vocabulary or reinterpret existing authored entities as types.
  (doseq [ident (keys (d/schema db))]
    (when-not (or (str/starts-with? (or (namespace ident) "") "geschichte")
                   (= "db" (namespace ident)))
      (refuse! :not-ordinary-repository))))

(defn prepare
  "Pure bounded preview. `id` and `at` are frozen by the HOST upgrade journal.
   Transaction data is HOST-internal, never accepted back from a client."
  [db id at]
  (when-not (and (uuid? id) (inst? at)) (refuse! :invalid-intent))
  (ordinary! db)
  (let [repository (query/repository db)
        existing (d/schema db)
        required (->> (concat (first (seed/definition-transactions))
                              (profile/foundation-schema) receipt-schema)
                      (reduce (fn [out entry] (assoc out (:db/ident entry) entry)) (sorted-map))
                      vals vec)
        _ (doseq [entry required :let [actual (get existing (:db/ident entry))] :when actual]
            (when-not (= (shape entry) (shape actual)) (refuse! :incompatible-schema)))
        missing (filterv #(not (contains? existing (:db/ident %))) required)
        definitions (walk/postwalk (fn [value]
                                    (if (and (map? value) (contains? value :entity/created-at))
                                      (assoc value :entity/created-at at) value))
                                  (vec (mapcat identity (rest (seed/definition-transactions)))))
        expected (basis db)
        tx (into missing (concat definitions
                                 [{:db/id (:db/id repository)
                                   :document.repository/profile space/profile
                                   :document.repository/upgrade id
                                   :document.repository/upgrade-basis expected}]))
        prospective (:db-after (d/with db tx))
        _ (profile/validate-foundation! prospective)
        preview (view/prepare prospective profile/selection-options)]
    (when-not (= :ready (:status preview))
      (throw (ex-info "Authored Markdown must be repaired before upgrading"
                      {:type ::held :reason :invalid-documents :diagnostics (:diagnostics preview)})))
    {:version 1 :id id :at at :basis expected
     :repository (:geschichte.repo/id repository) :source (source-id db)
     :definition-revision (definition-revision)
     :projection (:id preview) :documents (count (:documents preview))
     :transaction tx}))

(defn- apply-tx [db expected tx]
  ;; Reuse the native writer's commit-id/max-tx guard. Its db.fn/call snapshot
  ;; may already contain transaction-clock bookkeeping; do not derive a new
  ;; guard from op-count/eid or run prospective transactions inside the writer.
  (file-tx/assert-basis! db expected)
  tx)

(defn install!
  "Atomic additive repository phase; stale/invalid plans install nothing.
   No retry after uncertain acknowledgement: recover using the stored receipt."
  [conn {:keys [id at basis]}]
  (let [db @conn
        _ (when-not (= basis (is.simm.documents.knowledge-upgrade/basis db)) (refuse! :source-changed))
        ;; Caller-supplied transaction data is ignored. Recompute the fixed HOST
        ;; foundation against the inspected source before the writer boundary.
        plan (prepare db id at)]
    (d/transact conn [[:db.fn/call apply-tx (file-tx/capture-basis db) (:transaction plan)]]))
  (let [repository (query/repository @conn)]
    (when-not (and (= id (:document.repository/upgrade repository))
                   (= basis (:document.repository/upgrade-basis repository)))
      (refuse! :receipt-mismatch))
    (select-keys repository [:geschichte.repo/id :document.repository/profile
                            :document.repository/upgrade :document.repository/upgrade-basis])))
