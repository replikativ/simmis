(ns is.simm.model.github-integrations
  "Initial Git upstream metadata published atomically with Memory membership.
   References and hashes confer neither authority nor native GC retention.
   This module does not integrate, merge or advance an existing connection."
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
         [:github.integration/proof-id :db.type/uuid]]))

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
