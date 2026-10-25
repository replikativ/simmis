(ns is.simm.ops.knowledge-checkpoint-roots
  "Internal physical retention for an already authorized HOST source binding.
   Never lifted into SCI, registered as a tool, or used as authorization. Root
   IDs are exclusively owned by one durable intent, permanent and never reused.
   No distributed-GC or independent root-replacement guarantee is claimed."
  (:require [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.ops.knowledge-changes :as changes]
            [konserve.core :as k]))

(defn- refuse! [reason]
  (throw (ex-info "Knowledge checkpoint retention is unavailable" {:type ::held :reason reason})))

(defn- source-keys [] [:repository :store :backend :branch :ref :configuration])

(defn- registry! [db]
  (let [store (:store db) present? (k/exists? store roots/registry-key {:sync? true})
        value (k/get store roots/registry-key nil {:sync? true})]
    (when (and present? (not (map? value))) (refuse! :invalid-root-registry))
    (or value {})))

(defn- load! [conn source-db intent]
  (let [body (pins/payload conn intent)
        basis (:basis body) physical (changes/basis source-db)]
    (when-not (= (select-keys basis (source-keys)) (select-keys physical (source-keys)))
      (refuse! :physical-source-changed))
    (let [snapshot (d/commit-as-db source-db (:commit basis))]
      (when-not snapshot (refuse! :checkpoint-unavailable))
      (when-not (= basis (changes/basis snapshot)) (refuse! :checkpoint-mismatch))
      {:body body :basis basis :snapshot snapshot})))

(defn owner
  "Exact immutable diagnostic binding, checked alongside canonical intents and
   physical source ownership. An owner label by itself confers no authority."
  [intent body]
  {:simmis/knowledge-stream (:knowledge.pin/stream intent)
   :simmis/pin-intent (:knowledge.pin/id intent) :claim (:knowledge.pin/claim intent)
   :source (:configuration (:basis body)) :commit (:commit (:basis body))
   :by (get-in body [:stream :by]) :room (get-in body [:stream :room])})

(defn verify!
  "Read and verify registry AND immutable pin record in this exact source.
   Missing/partial/mismatched data holds; it never means no change or permission
   to create a replacement. Returns HOST-only immutable snapshot handles."
  [control-conn source-db id]
  (let [intent (or (pins/intent @control-conn id) (refuse! :missing-intent))
        _ (when-not (#{:creating :pinned :committed :retained} (:knowledge.pin/state intent))
            (refuse! :retired-or-invalid-intent))
        {:keys [body basis snapshot]} (load! control-conn source-db intent)
        registry (get (registry! source-db) id)
        record (k/get (:store source-db) (roots/record-key id) nil {:sync? true})
        expected (roots/commit-record snapshot {:sync? true})]
    (when-not (and registry record expected) (refuse! :pin-unavailable))
    (when-not (and (= id (:id registry)) (= :pin (:kind registry))
                   (= (roots/record-key id) (:record-key registry))
                   (= (:commit basis) (:commit-id registry))
                   (nil? (:ttl-ms registry)) (not (contains? registry :expires-at))
                   (= (owner intent body) (:owner registry))
                   (= (:commit basis) (get-in record [:meta :datahike/commit-id]))
                   ;; Published commits must already have timestamps. Do not
                   ;; infer a synthetic/legacy normalization or ignore unknown
                   ;; root/schema/secondary/store-ref fields when comparing.
                   (inst? (get-in expected [:meta :datahike/created-at]))
                   (inst? (get-in expected [:meta :datahike/updated-at]))
                   (= (update expected :meta dissoc :datahike/parents) record))
      (refuse! :pin-mismatch))
    {:intent intent :body body :basis basis :db snapshot}))

(defn create!
  "Original persisted-intent winner only. A map is not a capability: the fixed
   HOST lifecycle must keep the native side effect exclusive and recheck room
   authorization. Recovery uses verify!, never calls create! on missing roots."
  [control-conn source-db {:keys [intent new?]}]
  (when-not (true? new?) (refuse! :not-creation-winner))
  (when-not (and (= intent (pins/intent @control-conn (:knowledge.pin/id intent)))
                 (= :creating (:knowledge.pin/state intent))
                 (= intent (pins/active @control-conn (:knowledge.pin/stream intent))))
    (refuse! :intent-changed))
  (let [id (:knowledge.pin/id intent)
        {:keys [body snapshot]} (load! control-conn source-db intent)]
    ;; root! overwrites explicit IDs, so never call it after a partial outcome
    ;; or recovery, even if only the record (not registry) became visible.
    (when (or (contains? (registry! source-db) id)
              (k/exists? (:store source-db) (roots/record-key id) {:sync? true}))
      (refuse! :root-already-present))
    (when-not (= id (roots/pin! snapshot {:id id :ttl-ms nil :owner (owner intent body)} {:sync? true}))
      (refuse! :pin-ack-mismatch))
    (verify! control-conn source-db id)))
