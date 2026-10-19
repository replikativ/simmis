(ns is.simm.uis.web.desktop.memory-tree-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.memory-tree :as tree]))

(deftest deep-paths-can-be-focused-without-renaming-them
  (is (= [{:label "wiki" :path "wiki"} {:label "team" :path "wiki/team"} {:label "研究" :path "wiki/team/研究"}]
         (tree/breadcrumbs "wiki/team/研究")))
  (is (empty? (tree/breadcrumbs "")))
  (is (= 180 (tree/sidebar-width -10 1400)))
  (is (= 640 (tree/sidebar-width 1000 1400)))
  (is (= 325 (tree/sidebar-width 500 500))))

(deftest file-tabs-are-room-and-column-specific
  (let [columns [{:id :a :tabs [{:id :one :type :memory :data {:room-id :room1 :path "a.md"}}
                               {:id :two :type :memory :data {:room-id :room2 :path "a.md"}}]}
                 {:id :b :tabs [{:id :three :type :memory :data {:room-id :room1 :path "a.md"}}]}]]
    (is (= :one (:id (tree/file-tab columns :a :room1 "a.md"))))
    (is (= :two (:id (tree/file-tab columns :a :room2 "a.md"))))
    (is (= :three (:id (tree/file-tab columns :b :room1 "a.md"))))
    (is (nil? (tree/file-tab columns :a :room1 "b.md")))))
