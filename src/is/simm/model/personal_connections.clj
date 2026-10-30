(ns is.simm.model.personal-connections
  "Server-only personal provider credentials. Selection by a Memory is a
   separate grant; ownership alone never authorizes remote effects."
  (:require [clojure.string :as str]
            [datahike.api :as d]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (= ident :integration.account/by) (assoc :db/index true)
            (= ident :integration.account/id) (assoc :db/unique :db.unique/value)))
        [[:integration.account/id :db.type/uuid] [:integration.account/by :db.type/uuid]
         [:integration.account/provider :db.type/keyword] [:integration.account/name :db.type/string]
         [:integration.account/revision :db.type/uuid] [:integration.account/state :db.type/keyword]
         [:integration.account/nonce :db.type/string] [:integration.account/ciphertext :db.type/string]]))

(defn- refuse! [] (throw (ex-info "Personal connection changed or is unavailable" {:type ::refused})))

(defn current-in [db by id]
  (when-not (every? uuid? [by id]) (refuse!))
  (let [eid (:db/id (d/entity db [:integration.account/id id]))
        row (when eid (d/pull db '[*] eid))]
    (when (= by (:integration.account/by row)) row)))

(defn binding-of [row]
  (when (and (= :active (:integration.account/state row)) (= :github (:integration.account/provider row)))
    (select-keys row [:integration.account/id :integration.account/by :integration.account/revision])))

(defn current! [db expected]
  (when-not (and (= #{:integration.account/id :integration.account/by :integration.account/revision} (set (keys expected)))
                 (every? uuid? (vals expected))
                 (= expected (binding-of (current-in db (:integration.account/by expected) (:integration.account/id expected)))))
    (refuse!))
  expected)

(defn public-row [row]
  (when row
    {:id (str (:integration.account/id row)) :provider (:integration.account/provider row)
     :name (:integration.account/name row) :revision (str (:integration.account/revision row))
     :configured? (boolean (binding-of row))}))

(defn list-in [db by]
  (when-not (uuid? by) (refuse!))
  (let [entries (vec (take 65 (d/datoms db :avet :integration.account/by by)))]
    (when (> (count entries) 64) (refuse!))
    (let [rows (mapv #(d/pull db [:integration.account/id :integration.account/by :integration.account/provider
                                  :integration.account/name :integration.account/revision :integration.account/state] (:e %)) entries)]
      (mapv public-row (sort-by (juxt :integration.account/name :integration.account/id) rows)))))

(defn replace-tx [db by id expected revision name {:keys [nonce ciphertext] :as envelope}]
  (let [row (current-in db by id)]
    ;; A foreign ID is not an absent, claimable account.
    (when-not (and (= row expected)
                   (or row (nil? (:db/id (d/entity db [:integration.account/id id]))))
                   (uuid? revision) (not= revision (:integration.account/revision row))
                   (string? name) (<= 1 (count name) 80) (= name (str/trim name))
                   (not (re-find #"[\p{Cntrl}]" name))
                   (= #{:nonce :ciphertext} (set (keys envelope)))
                   (string? nonce) (<= 1 (count nonce) 128)
                   (string? ciphertext) (<= 1 (count ciphertext) 16384)) (refuse!))
    (when (and (nil? row) (>= (count (list-in db by)) 64)) (refuse!))
    [(cond-> {:integration.account/id id :integration.account/by by :integration.account/provider :github
              :integration.account/revision revision :integration.account/name name :integration.account/state :active
              :integration.account/nonce nonce :integration.account/ciphertext ciphertext}
       row (assoc :db/id (:db/id row)))]))

(defn revoke-tx [db by id expected revision]
  (let [row (current-in db by id)]
    (when-not (and row (= expected row) (uuid? revision) (not= revision (:integration.account/revision row))) (refuse!))
    (cond-> [{:db/id (:db/id row) :integration.account/state :revoked :integration.account/revision revision}]
      (:integration.account/nonce row) (conj [:db/retract (:db/id row) :integration.account/nonce (:integration.account/nonce row)])
      (:integration.account/ciphertext row) (conj [:db/retract (:db/id row) :integration.account/ciphertext (:integration.account/ciphertext row)]))))
