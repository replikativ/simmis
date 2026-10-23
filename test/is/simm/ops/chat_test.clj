(ns is.simm.ops.chat-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.authority.world :as world]
            [is.simm.agents.room-agents :as agents]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.chat :as chat]))

(deftest first-authorized-post-prepares-legacy-projections-before-dispatch
  (world/with-world [_ w]
    (let [cfg {:store {:backend :memory :id (random-uuid)}
               :schema-flexibility :read :keep-history? false}
          _ (d/create-database cfg)
          conn (d/connect cfg)
          posted (atom [])]
      (try
        (d/transact conn [{:db/ident :entity/uuid :db/valueType :db.type/uuid
                           :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                          {:db/ident :instance/of-role :db/valueType :db.type/ref
                           :db/cardinality :db.cardinality/one}
                          {:entity/uuid #uuid "00000000-0000-0000-0000-000000000029"}
                          {:entity/uuid #uuid "00000000-0000-0000-0000-00000000002a"}])
        (is (nil? (d/entity @conn [:entity/uuid (:room w)])))
        (is (nil? (d/entity @conn [:entity/uuid (:member w)])))
        (with-redefs [room-dbs/connect-room-database
                      (fn [scope] (is (= (:room-scope w) scope)) conn)
                      agents/post-user-message!
                      (fn [room text author actual-conn id parent source]
                        (is (identical? conn actual-conn))
                        (is (some? (:S.ChatRoom/name (d/entity @conn [:entity/uuid room]))))
                        (is (= "Member" (:S.User/display-name (d/entity @conn [:entity/uuid author]))))
                        (swap! posted conj [room text author id parent source]))]
          (let [id (random-uuid)]
            (chat/post! (principals/party-principal (:member w))
                        {:room (:room w) :id id :content "First post"})
            (is (= [[(:room w) "First post" (:member w) id nil :web]] @posted))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))
