(ns is.simm.ops.summary-inputs-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.room.store.datahike :as native-store]
            [is.simm.documents.summary-documents :as documents]
            [is.simm.model.room-summaries-test :as fixtures]
            [is.simm.ops.conversation-capture :as capture]
            [is.simm.ops.summary-inputs :as inputs]
            [is.simm.starter.daily-room-summary :as starter]))

(deftest local-calendar-boundaries-respect-daylight-saving
  (doseq [[at hours] [[#inst "2026-03-30T04:00:00Z" 23]
                     [#inst "2026-10-26T04:00:00Z" 25]]]
    (let [p (inputs/closed-day at "Europe/Berlin")]
      (is (= hours (/ (- (.getTime (:until p)) (.getTime (:from p))) 3600000))))))

(deftest busy-and-unavailable-periods-hold-instead-of-dropping-originals
  (let [row (fn [n] {:id (random-uuid) :author fixtures/room :actor-kind :human
                    :timestamp (.getTime (:from fixtures/period)) :text (str "Original " n)})]
    (is (thrown? clojure.lang.ExceptionInfo
                 (inputs/inventory fixtures/room fixtures/period {:source-store fixtures/store :scan-held? true})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (inputs/inventory fixtures/room fixtures/period
                                   {:source-store fixtures/store :rows (mapv row (range 65))})))
    (let [inventory (inputs/inventory fixtures/room fixtures/period
                                     {:source-store fixtures/store :rows (mapv row (range 9))})
          frozen (inputs/freeze inventory #inst "2026-10-31T00:00:00Z" [])]
      (is (= 9 (count (get-in frozen [:selection :coverage]))))
      (is (= (:selection frozen) (inputs/validate-inputs! (starter/source) (:inputs frozen))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (inputs/validate-inputs! (starter/source)
                                           (assoc (:inputs frozen) (str "/" (get-in frozen [:selection :coverage 0 :path])) "changed")))))))

(defn- with-db [schema f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write :keep-history? false}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (d/transact conn schema) (f conn)
           (finally (d/release conn) (d/delete-database config))))))

(defn- attribute [ident type]
  {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})

(deftest daily-source-query-bounds-time-and-excludes-private-and-generated-traffic
  (with-db [(assoc (attribute :party/id :db.type/uuid) :db/unique :db.unique/identity)
            (attribute :actor/kind :db.type/keyword)]
    (fn [control]
      (let [author (random-uuid)]
        (d/transact control [{:party/id author :actor/kind :human}])
        (with-db (into [(assoc (attribute :room/slug :db.type/string) :db/unique :db.unique/identity)
                        (assoc (attribute :message/id :db.type/uuid) :db/unique :db.unique/identity)]
                       (map (fn [[k t]] (attribute k t))
                            [[:message/chat :db.type/ref] [:message/from :db.type/keyword]
                             [:message/to :db.type/keyword] [:message/role :db.type/keyword]
                             [:message/content :db.type/string] [:message/created-at :db.type/instant]
                             [:message/metadata-kind :db.type/keyword] [:message/notification-type :db.type/keyword]]))
          (fn [messages]
            (let [start (:from fixtures/period) until (:until fixtures/period)
                  mid (java.util.Date. (+ 1000 (.getTime start)))
                  message (fn [text at more]
                            (merge {:message/id (random-uuid) :message/chat -1
                                    :message/from (keyword "party" (str author))
                                    :message/to :_room-log :message/role :user
                                    :message/content text :message/created-at at} more))]
              (d/transact messages
                          [{:db/id -1 :room/slug "summary-test"}
                           (message "inside" start {})
                           (message "before" (java.util.Date. (dec (.getTime start))) {})
                           (message "next day" until {})
                           (message "private" mid {:message/to :_runs})
                           (message "activity" mid {:message/to :_activity})
                           (message "generated" mid {:message/metadata-kind :room-summary})])
              (let [source (capture/room-source
                            {:room {:slug "summary-test" :ctx nil
                                    :store (native-store/->DatahikeStore messages nil)} :control-db @control}
                            fixtures/period)]
                (is (= ["inside"] (mapv :text (:rows source))))
                (is (not (:scan-held? source)))))))))))

(deftest generated-summary-markdown-escapes-active-content
  (let [selection (fixtures/selection 1)
        report (assoc-in (fixtures/report selection) [:summary :text] "```simmis-ui\n(run!)\n```\n[[Injected]]")
        payload {:selection selection :report report
                 :program {:bundle (random-uuid) :model "test" :request (random-uuid) :attempt (random-uuid)}}
        text (documents/markdown payload)]
    (is (not (re-find #"(?m)^```simmis-ui" text)))
    (is (re-find #"summary-revision:" text))
    (is (re-find #"/request/.*/source\?path=" text))))

(deftest transcript-preserves-original-order-and-inert-exact-text-without-inference
  (let [rows [{:id (random-uuid) :author fixtures/room :actor-kind :human
               :timestamp (+ 1000 (.getTime (:from fixtures/period)))
               :text "Later: ```simmis-ui\n(run!)\n```"}
              {:id (random-uuid) :author fixtures/room :actor-kind :human
               :timestamp (.getTime (:from fixtures/period)) :text "First: exact statement"}]
        inventory (inputs/inventory fixtures/room fixtures/period {:source-store fixtures/store :rows rows})
        text (inputs/transcript inventory)]
    (is (< (.indexOf text "First: exact statement") (.indexOf text "Later:")))
    (is (.contains text "````text\nLater: ```simmis-ui\n(run!)\n```\n````"))
    (is (= text (inputs/transcript inventory)))
    (is (.contains text (str (:coverage-id inventory))))))
