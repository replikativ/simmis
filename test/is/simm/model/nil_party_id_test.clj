(ns is.simm.model.nil-party-id-test
  "A nil party id must never resolve to SOME party.

   datahike reads a nil `:in` scalar as an unbound variable, so
   `[:find ?e . :in $ ?pid :where [?e :party/id ?pid]]` with `?pid` nil matched
   an arbitrary party. `update-preferred-model!` called with nil wrote the owner
   preference onto Vár. Writes keyed by a nil id now throw; reads answer
   nil/empty."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [dvergr.chat.schema :as chat-schema]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.system-db :as system-db]))

(def ^:dynamic *conn* nil)

(use-fixtures
 :once
 (fn [run]
   (let [cfg {:store {:backend :memory :id (random-uuid)}
              :schema-flexibility :write
              :keep-history? false}]
     (d/create-database cfg)
     (let [conn (d/connect cfg)]
       (chat-schema/ensure-full-schema! conn)
       (d/transact conn system-db/schema)
       ;; dvergr declares :room/parties (dvergr.system.db) — installed here the
       ;; way access_test does, as the ref it is in the shared system DB
       (when-not (d/entity @conn :room/parties)
         (d/transact conn [{:db/ident :room/parties :db/valueType :db.type/ref
                            :db/cardinality :db.cardinality/many}]))
       (binding [*conn* conn]
         (with-redefs [system-db/get-conn (constantly conn)]
           (run)))
       (d/release conn)))))

(defn- seed-party! [kind display-name]
  (let [party-id (random-uuid)]
    (d/transact *conn*
                [{:actor/id (parties/party-id->actor-id party-id)
                  :actor/kind kind
                  :actor/name display-name
                  :actor/status :online
                  :actor/created-at (java.util.Date.)
                  :party/id party-id}])
    party-id))

(defn- party-rows []
  (set (d/q '[:find ?pid ?pref ?name
              :where [?e :party/id ?pid]
                     [(get-else $ ?e :party/preferred-model :none) ?pref]
                     [(get-else $ ?e :actor/name :none) ?name]]
            @*conn*)))

(defn- missing-id? [f]
  (try (f) false
       (catch clojure.lang.ExceptionInfo e
         (= :missing-id (:type (ex-data e))))))

(deftest nil-party-id-writes-throw-and-touch-nothing
  (let [_human (seed-party! :human "Owner")
        _agent (seed-party! :agent "Vár")
        before (party-rows)]
    (testing "update-party! refuses a nil id instead of picking a party"
      (is (missing-id? #(parties/update-party! nil {:party/preferred-model "gpt-*-sol"
                                                    :party/display-name "hijacked"}))))
    (testing "update-preferred-model! refuses a nil owner"
      (is (missing-id? #(parties/update-preferred-model! nil "gpt-*-sol"))))
    (testing "the other party write paths refuse nil too"
      (is (missing-id? #(parties/delete-party! nil)))
      (is (missing-id? #(parties/set-env-var! nil "OPENAI_API_KEY" "x")))
      (is (missing-id? #(parties/delete-env-var! nil "OPENAI_API_KEY")))
      (is (missing-id? #(parties/set-ui-pref! nil :ui-pref/syntax :markdown))))
    (is (= before (party-rows)) "no party row changed")))

(defn- seed-room! [party-id]
  (let [room-id (random-uuid)]
    (d/transact *conn* [{:room/id room-id :room/name "Room" :room/type :group
                         :room/parties [[:party/id party-id]]}])
    room-id))

(defn- seed-kb! [owner-id]
  (let [kb-id (random-uuid)]
    (d/transact *conn* [{:kb/id kb-id :kb/name "Wiki" :kb/owner owner-id
                         :kb/db-scope (random-uuid)}])
    kb-id))

(defn- datoms []
  (set (map (juxt :e :a :v) (d/datoms @*conn* :eavt))))

(deftest nil-party-id-reads-answer-nothing
  ;; Seeded so that every nil lookup below has something it COULD wrongly
  ;; return: on the unguarded code each of these answered a real row.
  (let [pid (seed-party! :human "Someone")
        room-id (seed-room! pid)
        kb-id (seed-kb! pid)
        contact (seed-party! :human "Contact")]
    (d/transact *conn* [{:party/id pid :party/handle "someone" :party/email "someone@example.org"}])
    (parties/add-contact! pid contact)
    (parties/set-ui-pref! pid :ui-pref/syntax :markdown)
    (parties/set-env-var! pid "OPENAI_API_KEY" "secret")
    (is (some? (parties/get-party pid)))
    (is (= pid (:party/id (parties/get-party-by-handle "someone"))))
    (is (seq (parties/get-contacts pid)))
    (is (= {:ui-pref/syntax :markdown} (parties/get-ui-prefs pid)))
    (is (= [room-id] (mapv :room/id (rooms/get-party-rooms pid))))
    (is (= kb-id (:kb/id (kbs/get-kb kb-id))))
    (is (seq (kbs/get-party-kbs pid)))
    (is (nil? (parties/get-party nil)))
    (is (nil? (parties/get-party-by-handle nil)))
    (is (nil? (parties/get-party-by-email nil)))
    (is (empty? (parties/get-env-vars nil)) "another party's secrets are not returned")
    (is (empty? (parties/get-contacts nil)))
    (is (= {} (parties/get-ui-prefs nil)))
    (is (empty? (rooms/get-party-rooms nil)))
    (is (nil? (rooms/get-room nil)))
    (is (empty? (kbs/get-party-kbs nil)))
    (is (nil? (kbs/get-kb nil)))))

(deftest nil-room-and-kb-writes-throw-and-touch-nothing
  (let [pid (seed-party! :human "Member")
        other (seed-party! :human "Other")
        room-id (seed-room! pid)
        kb-id (seed-kb! pid)
        before (datoms)]
    (is (missing-id? #(rooms/add-party! nil other)) "nil room would pick an arbitrary room")
    (is (missing-id? #(rooms/add-party! room-id nil)))
    (is (missing-id? #(kbs/delete-kb! nil)) "nil kb would delete an arbitrary KB")
    (is (missing-id? #(kbs/share-kb! nil other)))
    (is (missing-id? #(kbs/share-kb! kb-id nil)))
    (is (= before (datoms)) "no row changed")))
