(ns is.simm.runtimes.screen-history-test
  "What a room's agent may read of a member's screen stream, now that ENDED
   shares are readable (`screen/history`, `screen/search`): frames inside the
   room's own grant windows — open or ended — and nothing else. A frame outside
   every window, or inside a window granted to ANOTHER room, is never returned.

   Also: a frame whose description failed is stored with its reason and shows
   up as failed, rather than being dropped."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [is.simm.model.screen-grants :as sg]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.task-models :as task-models]
            [is.simm.model.blobs :as blobs]
            [is.simm.model.parties]
            [dvergr.media.vision]
            [dvergr.search.secondary]
            [is.simm.runtimes.screen-intake :as intake]))

(def ^:private grant-schema
  [{:db/ident :screen-grant/id :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :screen-grant/party :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :screen-grant/room :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :screen-grant/from :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
   {:db/ident :screen-grant/until :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
   {:db/ident :screen-grant/beat :db/valueType :db.type/long :db/cardinality :db.cardinality/one}])

(defn- mem-conn [schema]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (doto (d/connect cfg) (d/transact schema))))

(def ^:dynamic *grants* nil)
(def ^:private screens (atom {}))

(use-fixtures :each
  (fn [t]
    (reset! screens {})
    (let [g (mem-conn grant-schema)]
      (binding [*grants* g]
        (with-redefs [system-db/get-conn (constantly g)
                      intake/ensure-screens-conn
                      (fn [party]
                        (or (get @screens party)
                            (get (swap! screens assoc party
                                        (mem-conn @#'intake/screens-schema))
                                 party)))
                      ;; party labels read the system DB's parties
                      is.simm.model.parties/get-party (constantly nil)]
          (t))))))

(defn- grant! [party room from until]
  (d/transact *grants* [{:screen-grant/id (random-uuid) :screen-grant/party party
                         :screen-grant/room room :screen-grant/from from
                         :screen-grant/until until :screen-grant/beat until}]))

(defn- frame! [party at text]
  (#'intake/persist-frame! party {:at at :blob-id (str "blob-" at) :text text}))

(deftest an-ended-share-is-readable-and-nothing-outside-it
  (let [alice (random-uuid) meeting (random-uuid) other (random-uuid)]
    ;; alice shared into the meeting room 1000–2000, into ANOTHER room 3000–4000
    (grant! alice meeting 1000 2000)
    (grant! alice other 3000 4000)
    (frame! alice 500 "before any share: private notes")
    (frame! alice 1500 "quarterly revenue slide")
    (frame! alice 2500 "between shares: personal email")
    (frame! alice 3500 "other room: salary spreadsheet")
    (let [history (intake/room-history meeting {:n 50})]
      (testing "the ended window is readable, attributed"
        (is (= [1500] (mapv :at history)))
        (is (= "quarterly revenue slide" (:text (first history))))
        (is (= (str alice) (:party (first history)))))
      (testing "frames outside every window, or in a window granted to another
                room, are not readable"
        (is (not-any? #{500 2500 3500} (map :at history)))
        (is (= [3500] (mapv :at (intake/room-history other {:n 50}))))))
    (testing "since/until narrow a window, never widen it"
      (is (empty? (intake/room-history meeting {:since 1600 :n 50})))
      (is (empty? (intake/room-history meeting {:since 0 :until 900 :n 50})))
      (is (= [1500] (mapv :at (intake/room-history meeting {:since 0 :until 99999 :n 50})))))
    (testing "the shares list shows the ended window"
      (is (= [{:from 1000 :until 2000 :active? false}]
             (mapv #(select-keys % [:from :until :active?]) (intake/room-shares meeting)))))
    (testing "fulltext is clipped the same way"
      (with-redefs [dvergr.search.secondary/search
                    (fn [db _ _]
                      ;; every stored frame matches; the window filter decides
                      (map (fn [[e]] [e 1.0])
                           (d/q '[:find ?e :where [?e :screenshot/at _]] db)))]
        (is (= [1500] (mapv :at (intake/room-frame-search meeting "anything" 10))))))))

(deftest a-room-with-no-grant-reads-nothing
  (let [alice (random-uuid) stranger-room (random-uuid)]
    (frame! alice 1500 "anything")
    (is (empty? (intake/room-history stranger-room {:n 50})))
    (is (empty? (intake/room-shares stranger-room)))))

(deftest a-failed-description-is-stored-and-shown-as-failed
  (let [alice (random-uuid) room (random-uuid)
        done (promise)]
    (sg/open-grant! alice room)
    (with-redefs [blobs/store! (fn [_ _] {:blob/id "blob-failed"})
                  task-models/model-opts
                  (fn [_] (throw (ex-info "The configured vision model is unusable: no available vision-capable model in accounts/fireworks/models/kimi-*"
                                          {:type :task-model-unavailable})))
                  intake/persist-frame! (let [orig @#'intake/persist-frame!]
                                          (fn [p f] (orig p f) (deliver done f)))]
      (is (= 200 (:status (intake/handle-frame! alice (byte-array 4) "image/jpeg"))))
      (let [f (deref done 10000 ::timeout)]
        (is (not= ::timeout f))
        (is (re-find #"vision-capable" (:failed f)))))
    (testing "the owner's gallery lists it, failed, with the reason"
      (let [[g] (intake/search-own alice "" 10)]
        (is (= "blob-failed" (:blob-id g)))
        (is (nil? (:text g)))
        (is (re-find #"unusable" (:failed g)))))
    (testing "the room's agent sees it as failed, live and in history"
      (is (:failed (first (intake/room-frames room 5))))
      (is (:failed (first (intake/room-history room {:n 5}))))
      (is (re-find #"no description" ((get (intake/sci-namespace room) 'text)))))))

(deftest describe-returning-nothing-is-a-failure-with-a-reason
  (with-redefs [task-models/model-opts (constantly {})
                dvergr.media.vision/describe (fn [& _] nil)]
    (let [[desc failed] (#'intake/describe-frame (byte-array 1) "image/png")]
      (is (nil? desc))
      (is (re-find #"returned no description" failed)))))

(deftest provider-exceptions-become-recordable-failures-without-secret-text
  (doseq [failure [(ex-info "Configure vision, token=do-not-store" {:type :dvergr.media.vision/configuration})
                   (ex-info "secret token=do-not-store" {:type :task-model-unavailable})
                   (java.io.IOException. "provider rejected secret credential")]]
    (with-redefs [task-models/model-opts (constantly {})
                  dvergr.media.vision/describe (fn [& _] (throw failure))]
      (let [[desc failed] (#'intake/describe-frame (byte-array 1) "image/png")]
        (is (nil? desc))
        (is (re-find #"configured vision model" failed))
        (is (not (re-find #"secret|do-not-store" failed)))))))

(deftest provider-exception-retains-the-frame-in-the-owner-gallery
  (let [owner (random-uuid) done (promise)]
    (with-redefs [blobs/store! (fn [_ _] {:blob/id "provider-failed-frame"})
                  task-models/model-opts (constantly {})
                  dvergr.media.vision/describe
                  (fn [& _] (throw (ex-info "secret provider diagnostic" {:type :task-model-unavailable})))
                  intake/persist-frame! (let [persist @#'intake/persist-frame!]
                                          (fn [p f] (persist p f) (deliver done f)))]
      (is (= 200 (:status (intake/handle-frame! owner (byte-array 4) "image/png"))))
      (is (not= ::timeout (deref done 10000 ::timeout))))
    (let [[frame] (intake/search-own owner "" 10)]
      (is (= "provider-failed-frame" (:blob-id frame)))
      (is (nil? (:text frame)))
      (is (re-find #"configured vision model" (:failed frame)))
      (is (not (re-find #"secret" (:failed frame)))))))
