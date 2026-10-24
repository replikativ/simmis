(ns is.simm.uis.web.desktop.memory-owner-test
  (:require [is.simm.uis.web.desktop.memory-owner :as owner]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(deftest tab-buffers-keep-their-owner-when-only-the-column-changes
  (let [data {:room-id "room" :col-id "left" :tab-id "file"} key (owner/key "session" data)]
    (is (= ["session" "room" :tab "file"] key))
    (is (= key (owner/key "session" (assoc data :col-id "right"))))
    (is (not= key (owner/key "other-session" data)))
    (is (not= key (owner/key "session" (assoc data :room-id "other-room"))))
    (is (not= key (owner/key "session" (assoc data :tab-id "other-tab"))))))

(deftest non-tab-controllers-keep-their-distinct-explicit-owners
  (is (= ["session" "room" :navigation nil]
         (owner/key "session" {:room-id "room" :col-id :navigation})))
  (is (not= (owner/key "session" {:room-id "room" :col-id :navigation})
            (owner/key "session" {:room-id "room" :col-id :synthetic}))))

(deftest callbacks-locate-only-one-current-memory-tab-in-the-original-room
  (let [tab {:id "file" :type :memory :data {:room-id "room" :path "wiki/a.md"}}
        columns [{:id "right" :tabs [tab]}]]
    (is (= {:col-id "right" :tab tab} (owner/current-tab columns "room" "file")))
    (is (nil? (owner/current-tab [] "room" "file")))
    (is (nil? (owner/current-tab columns "room" nil)))
    (is (nil? (owner/current-tab columns "other-room" "file")))
    (is (nil? (owner/current-tab [{:id "right" :tabs [(assoc tab :type :chat)]}] "room" "file")))
    (is (nil? (owner/current-tab (conj columns {:id "left" :tabs [tab]}) "room" "file")))))
