(ns is.simm.model.summary-records-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.room-summaries-test :as fixture]
            [is.simm.model.summary-records :as records]
            [is.simm.ops.summary-inputs :as inputs]))

(defn- with-store [f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :keep-history? false}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (records/ensure-schema! conn) (f conn)
           (finally (d/release conn) (d/delete-database config))))))

(defn- payload [conn]
  (let [target {:repository (random-uuid) :store (random-uuid) :backend :memory :branch :main :ref "main"}
        inventory (inputs/inventory fixture/room fixture/period
                                    {:source-store (get-in @conn [:config :store :id])
                                     :rows [{:id (random-uuid) :author fixture/room :actor-kind :human
                                             :timestamp (.getTime (:from fixture/period))
                                             :text "The launch date is proposed, not agreed."}]})
        frozen (inputs/freeze inventory #inst "2026-10-31T00:00:00Z" [])]
    {:selection (:selection frozen) :report (fixture/report (:selection frozen)) :inputs (:inputs frozen)
     :coverage-id (:coverage-id frozen) :previous nil :generation 1 :target target
     :created-at #inst "2026-10-31T00:00:01Z"
     :program {:bundle (random-uuid) :model "test" :request (random-uuid) :attempt (random-uuid)
               :target (records/target-id target)}}))

(deftest native-record-retains-exact-bytes-and-idempotent-publication-preserves-time
  (with-store
    (fn [conn]
      (let [value (payload conn) record (records/publish! conn value)
            again (records/publish! conn (assoc value :created-at #inst "2026-11-01T00:00:00Z"))]
        (is (= record again))
        (is (= value (records/payload! conn record)))
        (is (thrown? clojure.lang.ExceptionInfo (records/publish! conn (assoc value :inputs {}))))
        (is (= record (records/latest-in @conn fixture/room (:room.summary/node record))))))))

(deftest late-completion-cannot-overwrite-a-newer-native-head
  (with-store
    (fn [conn]
      (let [first-value (payload conn) first-record (records/publish! conn first-value)
            next-value (-> first-value (assoc :previous (:room.summary/revision first-record) :generation 2)
                           (assoc-in [:program :request] (random-uuid)))
            second-record (records/publish! conn next-value)
            stale (assoc-in first-value [:program :request] (random-uuid))]
        (is (thrown? clojure.lang.ExceptionInfo (records/publish! conn stale)))
        (is (= second-record (records/latest-in @conn fixture/room (:room.summary/node first-record))))
        (is (= first-record (records/publish! conn first-value)))
        (is (= second-record (records/latest-in @conn fixture/room (:room.summary/node first-record))))))))

(deftest projection-claims-serialize-generations-and-never-replay-writing
  (with-store
    (fn [conn]
      (let [value (payload conn) record (records/publish! conn value)
            target (records/target-id (:target value))
            id (random-uuid) path "summaries/test.md"
            intent {:summary.projection/id id :summary.projection/revision (:room.summary/revision record)
                    :summary.projection/room fixture/room :summary.projection/node (:room.summary/node record)
                    :summary.projection/kind :markdown :summary.projection/target target
                    :summary.projection/path path :summary.projection/content (random-uuid)
                    :summary.projection/state :reserved}]
        (records/reserve-projection! conn intent)
        (records/claim-projection! conn id nil)
        (is (= :writing (:summary.projection/state (records/projection-in @conn id))))
        (is (thrown? clojure.lang.ExceptionInfo (records/claim-projection! conn id nil)))
        (let [other (assoc intent :summary.projection/id (random-uuid))]
          (records/reserve-projection! conn other)
          (is (thrown? clojure.lang.ExceptionInfo
                       (records/claim-projection! conn (:summary.projection/id other) id))))
        (records/finish-projection! conn id (random-uuid))
        (is (= :published (:summary.path/state (records/path-in @conn (records/path-id fixture/room target path)))))
        (is (not= target (records/target-id (assoc (:target value) :branch :different))))))))
