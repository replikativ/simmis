(ns is.simm.uis.web.desktop.room-action-targeting-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.room-actions :as room-actions]))

(def room-a "room-a")
(def col-a "column-a")
(def col-b "column-b")

(deftest room-header-actions-name-the-room-and-column-that-rendered-them
  (doseq [[action tab-type data title]
          [[:settings :room-settings {:room-id room-a} "A Settings"]
           [:screens :screens {:room-id room-a :room-name "A"} "A Screens"]
           [:video :video {:room-id room-a :room-name "A"} "A Call"]
           [:files :files {:room-id room-a} "A Files"]]]
    (testing (name action)
      (let [[actual-type actual-data opts]
            (room-actions/room-header-tab col-a action room-a "A")]
        (is (= tab-type actual-type))
        (is (= data actual-data))
        (is (= title (:title opts)))
        (is (true? (:new-tab? opts)))
        (is (= col-a (:col-id opts)))))))

(def initial-layout
  [{:id col-a :tabs [{:id "a-chat" :type :chat :data {:room-id room-a}}]
    :active-tab "a-chat"}
   {:id col-b :tabs [{:id "b-chat" :type :chat :data {:room-id "room-b"}}]
    :active-tab "b-chat"}])

(deftest explicit-target-adds-the-new-tab-only-to-the-inactive-owner-column
  (let [tab {:id "a-settings" :type :room-settings :data {:room-id room-a}}
        layout (room-actions/add-tab-to-column initial-layout col-a tab)]
    (is (= ["a-chat" "a-settings"] (mapv :id (get-in layout [0 :tabs]))))
    (is (= "a-settings" (get-in layout [0 :active-tab])))
    (is (= (get initial-layout 1) (get layout 1))
        "B remains unchanged even when it was the active column before the click")))

(deftest explicit-target-preserves-active-column-behavior
  (let [tab {:id "b-files" :type :files :data {:room-id "room-b"}}
        layout (room-actions/add-tab-to-column initial-layout col-b tab)]
    (is (= (get initial-layout 0) (get layout 0)))
    (is (= ["b-chat" "b-files"] (mapv :id (get-in layout [1 :tabs]))))
    (is (= "b-files" (get-in layout [1 :active-tab])))))
