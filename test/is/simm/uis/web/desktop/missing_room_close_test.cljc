(ns is.simm.uis.web.desktop.missing-room-close-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.room-actions :as room-actions]))

(def room-id "stale-room")

(defn- chat-tab [id data]
  {:id id :type :chat :data data})

(defn- close [columns col-id tab-id]
  (room-actions/close-tab-in-layout
   columns col-id tab-id
   (constantly [{:id "replacement-column"
                :width 1.0
                :tabs [(chat-tab "replacement-tab" {})]
                :active-tab "replacement-tab"}])))

(deftest rendered-tab-data-carries-both-parts-of-tab-identity
  (is (= {:room-id room-id :room-missing? true :col-id "right" :tab-id "stale-2"}
         (room-actions/tab-render-data
          "right" (chat-tab "stale-2" {:room-id room-id :room-missing? true})))))

(deftest closing-the-second-missing-copy-leaves-the-first-column-alone
  (let [columns [{:id "left" :width 0.5
                  :tabs [(chat-tab "stale-1" {:room-id room-id :room-missing? true})]
                  :active-tab "stale-1"}
                 {:id "right" :width 0.5
                  :tabs [(chat-tab "healthy" {:room-id "other-room"})
                         (chat-tab "stale-2" {:room-id room-id :room-missing? true})]
                  :active-tab "stale-2"}]
        closed (close columns "right" "stale-2")]
    (is (= ["stale-1"] (mapv :id (get-in closed [0 :tabs])))
        "the first matching stale room is not touched")
    (is (= ["healthy"] (mapv :id (get-in closed [1 :tabs]))))
    (is (= "healthy" (get-in closed [1 :active-tab])))))

(deftest duplicate-tabs-in-one-column-close-by-tab-id-not-first-match
  (let [columns [{:id "one" :width 1.0
                  :tabs [(chat-tab "stale-1" {:room-id room-id :room-missing? true})
                         (chat-tab "stale-2" {:room-id room-id :room-missing? true})]
                  :active-tab "stale-2"}]
        closed (close columns "one" "stale-2")]
    (is (= ["stale-1"] (mapv :id (get-in closed [0 :tabs]))))
    (is (= "stale-1" (get-in closed [0 :active-tab])))))

(deftest closing-an-only-tab-removes-its-column-and-keeps-the-layout-valid
  (let [columns [{:id "healthy-column" :width 0.5
                  :tabs [(chat-tab "healthy" {:room-id room-id})]
                  :active-tab "healthy"}
                 {:id "missing-column" :width 0.5
                  :tabs [(chat-tab "stale" {:room-id room-id :room-missing? true})]
                  :active-tab "stale"}]
        closed (close columns "missing-column" "stale")]
    (is (= ["healthy-column"] (mapv :id closed)))
    (is (= [1.0] (mapv :width closed)))))

(deftest a-healthy-and-missing-tab-for-one-room-remain-distinct
  (let [columns [{:id "one" :width 1.0
                  :tabs [(chat-tab "healthy" {:room-id room-id})
                         (chat-tab "missing" {:room-id room-id :room-missing? true})]
                  :active-tab "missing"}]
        closed (close columns "one" "missing")]
    (is (= ["healthy"] (mapv :id (get-in closed [0 :tabs]))))))

(deftest closing-the-final-tab-restores-the-default-layout
  (let [closed (close [{:id "only" :width 1.0
                        :tabs [(chat-tab "stale" {:room-id room-id :room-missing? true})]
                        :active-tab "stale"}]
                      "only" "stale")]
    (is (= ["replacement-column"] (mapv :id closed)))
    (is (= ["replacement-tab"] (mapv :id (get-in closed [0 :tabs]))))))
