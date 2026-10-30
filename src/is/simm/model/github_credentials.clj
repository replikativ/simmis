(ns is.simm.model.github-credentials
  "Server-only, human-owned Git credential envelopes. Neither Room membership
   nor possession of a connection ID delegates another human's credential.
   Pure fragments require the fixed HOST's current authorization in the writer."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (= ident :github.credential/id) (assoc :db/unique :db.unique/value)))
        [[:github.credential/id :db.type/uuid] [:github.credential/by :db.type/uuid]
         [:github.credential/connection-id :db.type/uuid] [:github.credential/revision :db.type/uuid]
         [:github.credential/state :db.type/keyword] [:github.credential/nonce :db.type/string]
         [:github.credential/ciphertext :db.type/string]]))

(defn- refuse! [reason]
  (throw (ex-info "Git credential is unavailable" {:type ::refused :reason reason})))

(defn id [by connection-id]
  (when-not (every? uuid? [by connection-id]) (refuse! :invalid-identity))
  (hasch/uuid [:simmis/github-credential-v1 by connection-id]))

(defn current-in [db by connection-id]
  (when-let [entity (:db/id (d/entity db [:github.credential/id (id by connection-id)]))]
    (d/pull db '[*] entity)))

(defn binding-of [row]
  (when (= :active (:github.credential/state row))
    (select-keys row [:github.credential/id :github.credential/by
                      :github.credential/connection-id :github.credential/revision])))

(defn current! [db expected]
  (when-not (and (= 4 (count expected)) (every? uuid? (vals expected))
                 (= expected (binding-of (current-in db (:github.credential/by expected)
                                                     (:github.credential/connection-id expected)))))
    (refuse! :credential-changed))
  expected)

(defn replace-tx
  "CAS the complete observed row; callers encrypt before entering the writer.
   An empty or omitted secret is never interpreted as revocation."
  [db by connection-id expected revision {:keys [nonce ciphertext] :as envelope}]
  (let [row (current-in db by connection-id)]
    (when-not (= row expected) (refuse! :credential-changed))
    (when-not (and (uuid? revision) (not= revision (:github.credential/revision row))
                   (= #{:nonce :ciphertext} (set (keys envelope)))
                   (string? nonce) (<= 1 (count nonce) 128)
                   (string? ciphertext) (<= 1 (count ciphertext) 16384))
      (refuse! :invalid-envelope))
    [(cond-> {:github.credential/id (id by connection-id) :github.credential/by by
              :github.credential/connection-id connection-id :github.credential/revision revision
              :github.credential/state :active :github.credential/nonce nonce :github.credential/ciphertext ciphertext}
       row (assoc :db/id (:db/id row)))]))

(defn revoke-tx [db by connection-id expected revision]
  (let [row (current-in db by connection-id)]
    (when-not (and row (= row expected) (uuid? revision) (not= revision (:github.credential/revision row)))
      (refuse! :credential-changed))
    (cond-> [{:db/id (:db/id row) :github.credential/state :revoked :github.credential/revision revision}]
      (:github.credential/nonce row) (conj [:db/retract (:db/id row) :github.credential/nonce (:github.credential/nonce row)])
      (:github.credential/ciphertext row) (conj [:db/retract (:db/id row) :github.credential/ciphertext (:github.credential/ciphertext row)]))))
