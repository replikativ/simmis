(ns is.simm.model.github-pins
  "Durable intents for retained upstream bases. These records neither grant
   access nor perform native IO. Root IDs are permanent and never reused."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.knowledge-streams :as streams]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (#{:github.pin/id :github.pin/key} ident) (assoc :db/unique :db.unique/value)))
        [[:github.pin/id :db.type/uuid] [:github.pin/key :db.type/uuid]
         [:github.pin/room :db.type/uuid] [:github.pin/claim :db.type/uuid]
         [:github.pin/state :db.type/keyword] [:github.pin/body :db.type/store-ref]
         [:github.pin/body-id :db.type/uuid]]))

(defn- refuse! [reason]
  (throw (ex-info "Git base retention intent refused" {:type ::refused :reason reason})))

(defn by-key [db key]
  (when-let [eid (:db/id (d/entity db [:github.pin/key key]))]
    (d/pull db '[*] eid)))

(defn body-id [body]
  (when-not (and (map? body)
                 (= #{:version :purpose :connection :cursor :selection :root :claim :by} (set (keys body)))
                 (= 1 (:version body)) (= :upstream-base (:purpose body))
                 (every? uuid? (map body [:root :claim :by]))
                 (map? (:cursor body))
                 (map? (:selection body))
                 (= #{:head :git-oid :basis} (set (keys (:selection body)))))
    (refuse! :invalid-intent))
  (connections/connection! (:connection body))
  (streams/validate-basis! (get-in body [:selection :basis]))
  (let [{:keys [connection cursor selection]} body
        {:keys [head git-oid basis]} selection]
    (when-not (and (uuid? head) (string? git-oid) (re-matches #"[0-9a-f]{40}" git-oid)
                   (= (:room connection) (:github.integration/room cursor))
                   (= (connections/connection-id connection) (:github.integration/connection-id cursor))
                   (= head (:github.integration/head cursor))
                   (= git-oid (:github.integration/oid cursor))
                   (= (select-keys (:target connection) [:repository :store :backend])
                      (select-keys basis [:repository :store :backend])))
      (refuse! :binding-mismatch)))
  (when (> (alength (.getBytes ^String (pr-str body) "UTF-8")) 16384)
    (refuse! :intent-too-large))
  (hasch/uuid [:simmis/github-base-pin-v1 body]))

(defn intent-key [body]
  (body-id body)
  ;; Different currently authorized people recover the same intent. The
  ;; original owner remains diagnostic data, never recovery authority.
  (hasch/uuid [:simmis/github-base-retention-v1
               (select-keys body [:purpose :connection :cursor :selection])]))

(defn current-cursor! [db body]
  (body-id body)
  (when-not (= (:cursor body) (integrations/current-in db (get-in body [:connection :room])))
    (refuse! :cursor-changed)))

(defn prepare-tx
  "Compose current HOST authority in this same writer. Exact replay does not
   grant creation rights. Only positive fresh transaction evidence may do so."
  [db body ref]
  (when-not (uuid? ref) (refuse! :invalid-artifact))
  (current-cursor! db body)
  (let [key (intent-key body)]
    (if (by-key db key)
      []
      (do
        (when (d/entity db [:github.pin/id (:root body)]) (refuse! :root-id-reused))
        [{:github.pin/id (:root body) :github.pin/key key
          :github.pin/room (get-in body [:connection :room])
          :github.pin/claim (:claim body) :github.pin/state :creating
          :github.pin/body ref :github.pin/body-id (body-id body)}]))))

(defn confirm-tx
  "After registry AND full native record verification, with current authority.
   Permanent roots are not released by this alpha integration."
  [db expected body]
  (current-cursor! db body)
  (let [row (by-key db (intent-key body))]
    (when-not (and (= expected row)
                   (= (:root body) (:github.pin/id row))
                   (= (:claim body) (:github.pin/claim row))
                   (= (body-id body) (:github.pin/body-id row)))
      (refuse! :intent-changed))
    (case (:github.pin/state row)
      :creating [{:db/id (:db/id row) :github.pin/state :retained}]
      :retained []
      (refuse! :invalid-state))))
