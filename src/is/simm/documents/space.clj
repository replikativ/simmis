(ns is.simm.documents.space
  "Explicit provisioning for a NEW, unpublished governed knowledge repository.
   Not a document-open callback, legacy classifier or authorization boundary.
   The caller owns the connection exclusively until provisioning completes."
  (:require [datahike.api :as d]
            [datahike.constants :as constants]
            [geschichte.query :as query]
            [is.simm.documents.repository :as documents]
            [is.simm.model.store :as store]))

(def profile :simmis/canonical-knowledge-v1)
(def schema
  [{:db/ident :document.repository/profile :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}])

(defn provision!
  "Initialize ONLY a pristine connection using the existing canonical store
   installer. No copying a KB, registering a Yggdrasil adapter or publishing a
   store. Failure leaves an unpublished partial store for its owner to dispose;
   retrying/reclassifying that store is intentionally refused. The marker is
   recorded last, not used as a substitute for schema/ownership checks."
  [conn]
  (locking conn
    ;; Emptiness alone would admit a formerly authored/retracted database.
    ;; This first profile deliberately excludes attribute-ref genesis, whose
    ;; implicit schema is physically present in EAVT.
    (when-not (and (false? (get-in @conn [:config :attribute-refs?]))
                   (= :write (get-in @conn [:config :schema-flexibility]))
                   (= {:max-tx constants/tx0 :max-eid constants/e0 :op-count 0}
                      (select-keys @conn [:max-tx :max-eid :op-count]))
                   (empty? (d/schema @conn)) (empty? (d/datoms @conn :eavt)))
      (throw (ex-info "Knowledge provisioning requires a pristine unpublished store"
                      {:type ::not-pristine})))
    (documents/initialize! conn)
    (store/install! conn)
    (d/transact conn (into schema [{:db/id (:db/id (query/repository @conn))
                                   :document.repository/profile profile}]))
    conn))

(defn provisioned? [db]
  (and (contains? (d/schema db) :document.repository/profile)
       (= profile (:document.repository/profile (query/repository db)))))
