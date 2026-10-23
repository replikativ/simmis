(ns is.simm.uis.chat-history-test
  (:require [cljs.test :refer-macros [deftest is testing]]
            [datahike.api :as d]
            [datahike.core :as dc]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.db-signal :as db-signal]
            [is.simm.uis.web.desktop.chat-remote :as chat-remote]
            [is.simm.uis.web.desktop.tab-heal :as tab-heal]
            [is.simm.uis.web.desktop.views.columns :as columns]
            [is.simm.uis.web.desktop.datahike-query :as query]
            [is.simm.uis.web.desktop.run-detail :as run-detail]))

(def room-id #uuid "d595eb65-c930-4ac1-8a47-bbd11df96104")
(def message-id #uuid "d595eb65-c930-4ac1-8a47-bbd11df96105")
(def author-id #uuid "d595eb65-c930-4ac1-8a47-bbd11df96106")
(def legacy-only-id #uuid "d595eb65-c930-4ac1-8a47-bbd11df96107")

(defn- native-history []
  ;; Real immutable Datahike DB, no server, writable connection or legacy
  ;; ChatRoom/User rows. The caller supplies the authorized room's replica.
  (dc/db-with
   (dc/empty-db {} {:schema-flexibility :read})
   [{:db/ident :entity/uuid :db/valueType :db.type/uuid
     :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
    {:db/ident :chat/id :db/valueType :db.type/uuid
     :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
    {:db/ident :message/chat :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
    {:db/ident :S.Message/room :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
    {:db/ident :S.Message/author :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
    {:db/id -1 :chat/id room-id}
    {:message/id message-id :message/chat -1 :message/content "Native history"
     :message/created-at (js/Date. 1000) :message/role :user
     :message/source-username "Native author"}]))

(deftest native-history-does-not-require-legacy-projections-or-writes
  (let [db (native-history)]
    (is (nil? (d/q '[:find ?e . :in $ ?id :where [?e :entity/uuid ?id]] db room-id)))
    (with-redefs [d/transact (fn [& _] (throw (js/Error. "A read must not transact")))
                  run-detail/query-room-tool-calls (constantly [])]
      (doseq [make-query [query/make-room-messages-query query/make-room-timeline-query]]
        (testing "canonical history remains visible before any legacy viewer setup"
          (let [read! (make-query room-id)
                rows (read! db)]
            (is (= [message-id] (mapv :entity/uuid rows)))
            (is (= "Native history" (:block/content (first rows))))
            (is (= "Native author" (:S.Message/author-name (first rows))))
            (is (= rows (read! db)) "Repeated reads remain side-effect free")
            (is (nil? (read! nil)))))))))

(deftest existing-legacy-enrichment-and-legacy-only-history-are-retained
  (let [db (dc/db-with
            (native-history)
            [{:db/id -1 :entity/uuid room-id}
             {:db/id -2 :entity/uuid author-id :S.User/display-name "Projected author"}
             {:entity/uuid message-id :S.Message/room -1 :S.Message/author -2
              :block/content "Compatibility text" :S.Message/sent-at (js/Date. 1000)
              :S.Message/reasoning "Retained enrichment"}
             {:entity/uuid legacy-only-id :S.Message/room -1 :S.Message/author -2
              :block/content "Historical summary" :S.Message/sent-at (js/Date. 2000)}])]
    (with-redefs [run-detail/query-room-tool-calls (constantly [])]
      (doseq [make-query [query/make-room-messages-query query/make-room-timeline-query]]
        (let [rows ((make-query room-id) db)]
          (is (= [message-id legacy-only-id] (mapv :entity/uuid rows)))
          ;; The compatibility projector still owns enriched display text
          ;; (notably resolved wiki links), while native identity wins.
          (is (= "Compatibility text" (:block/content (first rows))))
          (is (= "Retained enrichment" (:S.Message/reasoning (first rows))))
          (is (= "Historical summary" (:block/content (second rows)))))))))

(deftest unverified-and-missing-room-mappings-never-read-a-warm-foreign-replica
  (let [scope "d595eb65-c930-4ac1-8a47-bbd11df96108"
        data {:room-id (str room-id) :db-scope scope
              :room-name "Room A" :thread-root-id (str message-id)}
        warm {scope {:db (native-history)}}
        calls (atom [])]
    (binding [ec/*execution-context* runtime]
      (with-redefs [db-signal/connect-room! (fn [& _] (swap! calls conj :open))
                    chat-remote/ensure-room! (fn [& _] (swap! calls conj :initialize))
                    d/q (fn [& _] (swap! calls conj :query) (throw (js/Error. "Unverified replica read")))]
        (doseq [kind [:chat :chat-thread] roster [nil {:rooms []}]]
          (is (some? (columns/render-tab-content
                      kind (:data (tab-heal/render-chat-tab {:type kind :data data} roster))
                      nil {} nil nil nil warm))))
        (is (empty? @calls) "Neither an unknown roster nor revocation admits cached history")))))
