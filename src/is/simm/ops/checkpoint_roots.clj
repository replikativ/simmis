(ns is.simm.ops.checkpoint-roots
  "HOST-only physical GC pin mechanics. Callers own durable intent, current
   authority and exclusive creation lifetime; these functions confer none."
  (:require [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [is.simm.ops.knowledge-changes :as changes]
            [konserve.core :as k]))

(defn- refuse! [reason]
  (throw (ex-info "Checkpoint retention is unavailable" {:type ::held :reason reason})))

(def source-keys [:store :backend :branch :configuration])

(defn registry! [db]
  (let [store (:store db) present? (k/exists? store roots/registry-key {:sync? true})
        value (k/get store roots/registry-key nil {:sync? true})]
    (when (and present? (not (map? value))) (refuse! :invalid-root-registry))
    (or value {})))

(defn load-snapshot!
  "Resolve exact published basis on the original physical configuration. No IO
   mutation, native lease or retention guarantee is created by this read."
  [source-db basis]
  (when-not (= (select-keys basis source-keys)
               (select-keys (changes/source-identity source-db) source-keys))
    (refuse! :physical-source-changed))
  (let [snapshot (d/commit-as-db source-db (:commit basis))]
    (when-not snapshot (refuse! :checkpoint-unavailable))
    (when-not (= basis (changes/basis snapshot)) (refuse! :checkpoint-mismatch))
    snapshot))

(defn verify!
  "Read registry AND full immutable pin record; missing/partial results hold.
   Owner is diagnostic integrity data, never permission to inspect this store."
  [source-db basis id owner]
  (let [snapshot (load-snapshot! source-db basis)
        registry (get (registry! source-db) id)
        record (k/get (:store source-db) (roots/record-key id) nil {:sync? true})
        expected (roots/commit-record snapshot {:sync? true})]
    (when-not (and registry record expected) (refuse! :pin-unavailable))
    (when-not (and (= id (:id registry)) (= :pin (:kind registry))
                   (= (roots/record-key id) (:record-key registry))
                   (= (:commit basis) (:commit-id registry))
                   (nil? (:ttl-ms registry)) (not (contains? registry :expires-at))
                   (= owner (:owner registry))
                   (= (:commit basis) (get-in record [:meta :datahike/commit-id]))
                   ;; Published commits must already have timestamps. Do not
                   ;; infer a synthetic/legacy normalization or ignore unknown
                   ;; root/schema/secondary/store-ref fields when comparing.
                   (inst? (get-in expected [:meta :datahike/created-at]))
                   (inst? (get-in expected [:meta :datahike/updated-at]))
                   (= (update expected :meta dissoc :datahike/parents) record))
      (refuse! :pin-mismatch))
    {:basis basis :db snapshot}))

(defn create!
  "One original durable-intent winner only; caller must ensure that lifetime.
   Never invoke on retry/recovery: explicit IDs overwrite. Both reserved keys
   must be absent. A failed/lost ACK is inspected with verify!, never replayed."
  [source-db basis id owner]
  (let [snapshot (load-snapshot! source-db basis)]
    (when-not (= (select-keys basis [:repository :ref])
                 (select-keys (changes/source-identity source-db) [:repository :ref]))
      (refuse! :physical-source-changed))
    (when (or (contains? (registry! source-db) id)
              (k/exists? (:store source-db) (roots/record-key id) {:sync? true}))
      (refuse! :root-already-present))
    (when-not (= id (roots/pin! snapshot {:id id :ttl-ms nil :owner owner} {:sync? true}))
      (refuse! :pin-ack-mismatch))
    (verify! source-db basis id owner)))
