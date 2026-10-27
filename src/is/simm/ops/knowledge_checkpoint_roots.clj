(ns is.simm.ops.knowledge-checkpoint-roots
  "Internal physical retention for an already authorized HOST source binding.
   Never lifted into SCI, registered as a tool, or used as authorization. Root
   IDs are exclusively owned by one durable intent, permanent and never reused.
   No distributed-GC or independent root-replacement guarantee is claimed."
  (:require [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-stream-retirement :as retirement]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.checkpoint-roots :as physical]))

(defn- refuse! [reason]
  (throw (ex-info "Knowledge checkpoint retention is unavailable" {:type ::held :reason reason})))

(defn- source-keys [] [:store :backend :branch :configuration])

(defn- registry! [db] (physical/registry! db))

(defn- load! [conn source-db intent]
  (let [body (pins/payload conn intent) basis (:basis body)]
    {:body body :basis basis :snapshot (physical/load-snapshot! source-db basis)}))

(defn owner
  "Exact immutable diagnostic binding, checked alongside canonical intents and
   physical source ownership. An owner label by itself confers no authority."
  [intent body]
  {:simmis/knowledge-stream (:knowledge.pin/stream intent)
   :simmis/pin-intent (:knowledge.pin/id intent) :claim (:knowledge.pin/claim intent)
   :origin (hasch/uuid (:origin body))
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
        {:keys [body basis]} (load! control-conn source-db intent)
        verified (physical/verify! source-db basis id (owner intent body))]
    (assoc verified :intent intent :body body)))

(defn create!
  "Original persisted-intent winner only. A map is not a capability: the fixed
   HOST lifecycle must keep the native side effect exclusive and recheck room
   authorization. Recovery uses verify!, never calls create! on missing roots."
  [control-conn source-db {:keys [intent new?]}]
  (when-not (true? new?) (refuse! :not-creation-winner))
  (streams/admissible! (streams/stream @control-conn (:knowledge.pin/stream intent)))
  (when-not (and (= intent (pins/intent @control-conn (:knowledge.pin/id intent)))
                 (= :creating (:knowledge.pin/state intent))
                 (= intent (pins/active @control-conn (:knowledge.pin/stream intent))))
    (refuse! :intent-changed))
  (let [id (:knowledge.pin/id intent)
        {:keys [body basis]} (load! control-conn source-db intent)]
    (physical/create! source-db basis id (owner intent body))
    (verify! control-conn source-db id)))

(defn release-previous!
  "Internal cleanup on an exclusively owned original physical store. Require
   the exact committed journal and a verified replacement BEFORE releasing its
   predecessor. Absence after uncertain release permits cleanup acknowledgement,
   never infers an old caller ACK or recreates a missing root."
  [control-conn source-db intent]
  (pins/finish-tx @control-conn intent)
  (when-not (= :committed (:knowledge.pin/state intent)) (refuse! :not-committed))
  (verify! control-conn source-db (:knowledge.pin/id intent))
  (let [event (d/pull @control-conn '[*] [:knowledge.discovery/id (:knowledge.pin/discovery intent)])
        old (:knowledge.discovery/before-root event)]
    (when old
      (let [old-intent (pins/intent @control-conn old)]
        (when-not (and (= :retained (:knowledge.pin/state old-intent))
                       (= (:knowledge.pin/stream intent) (:knowledge.pin/stream old-intent)))
          (refuse! :previous-intent-mismatch))
        ;; After a lost release ACK, GC may already have collected the old
        ;; commit. Its full live record is required only while still rooted.
        ;; Always bind the original physical source from the retained payload.
        (when-not (= (select-keys (:basis (pins/payload control-conn old-intent)) (source-keys))
                     (select-keys (changes/source-identity source-db) (source-keys)))
          (refuse! :physical-source-changed))
        (when (contains? (registry! source-db) old)
          (verify! control-conn source-db old)
          (roots/release! source-db old {:sync? true}))
        (when (contains? (registry! source-db) old) (refuse! :release-unconfirmed))))
    true))

(defn release-retiring!
  "Dispose ONLY the recorded cursor root after the canonical stream fence.
   An absent registry entry after a lost ACK needs no old commit: GC may have
   collected it. Retained payload + original physical identity still gate IO.
   CHECK! is a mandatory trusted internal HOST lifetime/authorization boundary,
   never request/provider data. Retain the original physical extent throughout."
  [control-conn source-db expected value check!]
  (when-not (fn? check!) (refuse! :missing-host-boundary))
  (check!)
  (let [{:keys [pin]} (retirement/retiring! @control-conn expected value)
        body (pins/payload control-conn pin)
        id (:knowledge.pin/id pin)]
    (when-not (and (= (:value expected) (:stream body)) (= (:basis expected) (:basis body))
                   (= (select-keys (:basis body) (source-keys))
                      (select-keys (changes/source-identity source-db) (source-keys))))
      (refuse! :physical-source-changed))
    (when (contains? (registry! source-db) id)
      (verify! control-conn source-db id)
      (retirement/retiring! @control-conn expected value)
      ;; Verification can block. A fence or revoked merger must win before the
      ;; native effect starts, even though the original lease is still retained.
      (check!)
      (roots/release! source-db id {:sync? true}))
    (when (contains? (registry! source-db) id) (refuse! :release-unconfirmed))
    (retirement/retiring! @control-conn expected value)
    (check!)
    true))
