(ns is.simm.model.screen-grants-test
  "Screen-share grants: attribution, multi-room fan-out, idempotent reopen,
   close, and heartbeat-based staleness (a dead client fails the window SAFE).

   Runs against a throwaway in-memory system DB (the grant schema only), with
   `system-db/get-conn` redefined to it — so the model is testable without a
   booted system."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [is.simm.model.screen-grants :as sg]
            [is.simm.model.system-db :as system-db]
            [datahike.api :as d]))

(def ^:private grant-schema
  [{:db/ident :screen-grant/id :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :screen-grant/party :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :screen-grant/room :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :screen-grant/from :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
   {:db/ident :screen-grant/until :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
   {:db/ident :screen-grant/beat :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
   ;; enough room/party schema to seed a :personal-ai room for the default test
   {:db/ident :party/id :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :room/id :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :room/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
   {:db/ident :room/parties :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}])

(def ^:dynamic *conn* nil)

(use-fixtures :each
  (fn [t]
    (let [cfg {:store {:backend :memory :id (java.util.UUID/randomUUID)}
               :schema-flexibility :write :keep-history? false}]
      (d/create-database cfg)
      (let [c (d/connect cfg)]
        (d/transact c grant-schema)
        (binding [*conn* c]
          (with-redefs [system-db/get-conn (constantly c)]
            (t)))
        (d/release c)))))

(defn- uuid [] (java.util.UUID/randomUUID))

(deftest attribution-and-multi-room-fanout
  (let [alice (uuid) bob (uuid) r1 (uuid) r2 (uuid)]
    (sg/open-grant! alice r1)
    (sg/open-grant! alice r2)
    (sg/open-grant! bob r1)
    (testing "one stream fans out to several rooms"
      (is (= #{r1 r2} (set (sg/active-rooms-for-party alice)))))
    (testing "a room sees every party sharing into it, attributed"
      (is (= #{alice bob} (set (map :party (sg/active-parties-for-room r1))))))
    (testing "an open window has bounds for clipping the archive"
      (is (:from (sg/active-window alice r1))))))

(deftest reopen-is-idempotent-and-close-revokes
  (let [alice (uuid) r1 (uuid)]
    (sg/open-grant! alice r1)
    (sg/open-grant! alice r1)
    (testing "opening an already-open grant does not duplicate it"
      (is (= 1 (count (sg/active-rooms-for-party alice)))))
    (sg/close-grant! alice r1)
    (testing "close revokes the window"
      (is (empty? (sg/active-rooms-for-party alice)))
      (is (nil? (sg/active-window alice r1))))
    (testing "a closed grant reopens cleanly (drops the stale :until)"
      (sg/open-grant! alice r1)
      (is (= [r1] (sg/active-rooms-for-party alice))))))

(deftest personal-room-is-a-standing-default
  (testing "sharing into ANY room also grants the sharer's :personal-ai room, and
            closing the last non-personal grant closes the personal one too"
    (let [alice (uuid) personal (uuid) team (uuid)]
      ;; seed alice + her personal-ai room (she is a member)
      (let [{:keys [tempids]} (d/transact *conn* [{:db/id -1 :party/id alice}])
            alice-eid (get tempids -1)]
        (d/transact *conn* [{:room/id personal :room/type :personal-ai
                             :room/parties [alice-eid]}]))
      (is (= personal (sg/personal-room-id alice)))
      (sg/open-grant-with-personal! alice team)
      (is (= #{team personal} (set (sg/active-rooms-for-party alice)))
          "the team room AND the personal room are both live")
      (sg/close-grant-with-personal! alice team)
      (is (empty? (sg/active-rooms-for-party alice))
          "closing the only non-personal grant closes personal too"))))

(deftest a-stale-heartbeat-fails-the-window-safe
  (let [alice (uuid) r1 (uuid)]
    (sg/open-grant! alice r1)
    (is (seq (sg/active-rooms-for-party alice)))
    (testing "past the staleness horizon the window is treated CLOSED —
              a client that died mid-share stops leaking the stream"
      (with-redefs [sg/stale-after-ms -1]      ; every beat is now 'too old'
        (is (empty? (sg/active-rooms-for-party alice)))
        (is (empty? (sg/active-parties-for-room r1)))
        (is (nil? (sg/active-window alice r1)))))
    (testing "a fresh heartbeat brings it back"
      (is (sg/heartbeat! alice r1))
      (is (= [r1] (sg/active-rooms-for-party alice))))))

(deftest ended-windows-are-kept-as-history
  (testing "reopening after a close opens a NEW window; the ended one stays,
            bounded, so an agent asked after a meeting can read it"
    (let [alice (uuid) r1 (uuid) r2 (uuid)]
      (sg/open-grant! alice r1)
      (sg/close-grant! alice r1)
      (sg/open-grant! alice r1)
      (sg/open-grant! alice r2)
      (let [ws (sg/windows-for-room r1)]
        (is (= 2 (count ws)))
        (is (= [false true] (mapv :active? ws)) "oldest first: ended, then open")
        (is (some? (:until (first ws))) "the ended window is bounded")
        (is (nil? (:until (second ws))) "the open window is not")
        (is (every? #(= alice (:party %)) ws)))
      (is (= 1 (count (sg/windows-for-room r2))) "another room's windows are its own")))
  (testing "a grant whose client died is closed at its last heartbeat when the
            next share opens — never left open-ended in the history"
    (let [bob (uuid) r (uuid)]
      (sg/open-grant! bob r)
      (let [beat (:beat (first (sg/active-parties-for-room r)))]
        (with-redefs [sg/stale-after-ms -1]
          (sg/open-grant! bob r))
        (let [[old] (sg/windows-for-room r)]
          (is (= beat (:until old))))))))
