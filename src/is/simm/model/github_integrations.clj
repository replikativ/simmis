(ns is.simm.model.github-integrations
  "Git upstream metadata published atomically with Memory membership/outcomes.
   References and hashes confer neither authority nor native GC retention.
   Pure fragments do not certify or perform native publication."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.github-connections :as connections]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (= ident :github.integration/room) (assoc :db/unique :db.unique/value)))
        [[:github.integration/room :db.type/uuid]
         [:github.integration/connection :db.type/store-ref]
         [:github.integration/connection-id :db.type/uuid]
         [:github.integration/import-head :db.type/uuid]
         [:github.integration/import-oid :db.type/string]
         [:github.integration/import-proof :db.type/store-ref]
         [:github.integration/import-proof-id :db.type/uuid]
         [:github.integration/head :db.type/uuid]
         [:github.integration/oid :db.type/string]
         [:github.integration/proof :db.type/store-ref]
         [:github.integration/proof-id :db.type/uuid]
         [:github.integration/sync :db.type/uuid]
         [:github.integration/previous :db.type/uuid]]))

(defn- refuse! [reason]
  (throw (ex-info "Git integration cursor refused" {:type ::refused :reason reason})))

(defn- oid? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{40}" value))))

(defn current-in [db room]
  (when-not (uuid? room) (refuse! :invalid-room))
  (when-let [eid (:db/id (d/entity db [:github.integration/room room]))]
    (d/pull db '[*] eid)))

(defn import-body
  "Closed initial-publication receipt data, not a historical authorization
   certificate. The HOST proves actual source/target correspondence; the
   membership writer binds the exact creation receipt and native metadata."
  [connection by creation head oid]
  (connections/connection! connection)
  (when-not (and (uuid? by) (uuid? head) (oid? oid)
                 (map? creation) (= #{:room/creation-intent :room/creation-basis} (set (keys creation)))
                 (every? uuid? (vals creation)))
    (refuse! :invalid-import))
  {:version 1 :kind :initial-import :room (:room connection) :by by
   :creation creation :connection-id (connections/connection-id connection) :head head :git-oid oid})

(defn import-id [body]
  (when-not (and (map? body)
                 (= #{:version :kind :room :by :creation :connection-id :head :git-oid} (set (keys body)))
                 (= 1 (:version body)) (= :initial-import (:kind body))
                 (every? uuid? (map body [:room :by :connection-id :head]))
                 (oid? (:git-oid body))
                 (map? (:creation body))
                 (= #{:room/creation-intent :room/creation-basis} (set (keys (:creation body))))
                 (every? uuid? (vals (:creation body))))
    (refuse! :invalid-import))
  (hasch/uuid [:simmis/github-initial-import-v1 body]))

(defn initialize-tx
  "Initialize ONLY from a HOST-verified import/integration receipt. Absent cursor
   alone is not permission to infer a base from files, HEAD or ancestry. Replay
   preserves a later cursor and confers no construction or cleanup rights."
  [db room connection-ref connection-id head oid proof-ref proof-id]
  (when-not (and (every? uuid? [room connection-ref connection-id head proof-ref proof-id]) (oid? oid))
    (refuse! :invalid-import))
  (let [immutable {:github.integration/room room
                   :github.integration/connection connection-ref
                   :github.integration/connection-id connection-id
                   :github.integration/import-head head :github.integration/import-oid oid
                   :github.integration/import-proof proof-ref :github.integration/import-proof-id proof-id}]
    (if-let [row (current-in db room)]
      (do (when-not (= immutable (select-keys row (keys immutable))) (refuse! :connection-changed)) [])
      [(merge immutable {:github.integration/head head :github.integration/oid oid
                         :github.integration/proof proof-ref :github.integration/proof-id proof-id})])))

(defn validate-sync-base!
  "Check the exact observed cursor and upstream base in the writer's DB.
   A stale initial cursor is not repaired by guessing from HEAD or ancestry."
  [db expected head oid]
  (when-not (and (map? expected) (uuid? head) (oid? oid))
    (refuse! :invalid-sync))
  (when-not (= expected (current-in db (:github.integration/room expected)))
    (refuse! :cursor-changed))
  (when-not (and (= head (:github.integration/head expected))
                 (= oid (:github.integration/oid expected)))
    (refuse! :unverified-base))
  expected)

(defn- advance-cursor-tx
  [db expected sync-id before-head before-oid head oid proof-ref proof-id]
  (when-not (and (every? uuid? [sync-id head proof-ref proof-id]) (oid? oid))
    (refuse! :invalid-sync))
  (validate-sync-base! db expected before-head before-oid)
  (if (= head before-head)
    (do (when-not (= oid before-oid) (refuse! :inconsistent-head)) [])
    [{:db/id (:db/id expected)
      :github.integration/head head :github.integration/oid oid
      :github.integration/proof proof-ref :github.integration/proof-id proof-id
      :github.integration/sync sync-id
      :github.integration/previous
      (hasch/uuid [:simmis/github-integration-cursor-v1 (dissoc expected :db/id)])}]))

(defn advance-sync-tx
  "Compose with a POSITIVE native sync outcome in the same control writer.
   An observed desired state after an unknown ACK is not such an outcome.
   Unchanged upstream heads leave the cursor and its proof unchanged."
  [db expected sync-id before-head before-oid head oid proof-ref proof-id]
  (advance-cursor-tx db expected sync-id before-head before-oid head oid proof-ref proof-id))

(defn observe-upstream-tx
  "Compose with a NEW positively verified remote-ref observation and its
   retained local Git identity, never an inference from matching local files.
   The HOST authenticates that observation; this does not certify the result
   of a historical send whose acknowledgement was lost."
  [db expected sync-id head oid observation-ref observation-id]
  (advance-cursor-tx db expected sync-id (:github.integration/head expected)
                     (:github.integration/oid expected) head oid observation-ref observation-id))
