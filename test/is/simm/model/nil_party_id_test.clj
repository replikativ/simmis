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

(deftest nil-party-id-reads-answer-nothing
  (let [pid (seed-party! :human "Someone")]
    (parties/set-env-var! pid "OPENAI_API_KEY" "secret")
    (is (some? (parties/get-party pid)))
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
