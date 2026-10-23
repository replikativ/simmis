(ns is.simm.uis.web.desktop.memory-references-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.memory-references :as references]
            [is.simm.uis.web.desktop.routes :as routes]))

(def room #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def repository #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def block #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")
(def revision #uuid "dddddddd-dddd-4ddd-8ddd-dddddddddddd")
(def stamp #uuid "eeeeeeee-eeee-4eee-8eee-eeeeeeeeeeee")
(def path "wiki/Moved.md")

(defn fixture []
  (let [intent {:repository-id repository :block-id block}
        analysis {:revision revision :semantic {:status :ready :blocks [{:id block :identity-status :explicit :from 8 :to 30}]}}
        state {:connection 1 :target-stamp stamp :target-epoch 2 :target-verified? true :selected path
               :drafts {path {:source {:repository-id repository :target-stamp stamp :entry {:content revision}
                                       :text "saved" :analysis analysis} :text "saved"}}}]
    (assoc state :block-reference {:intent intent :id block :request-marker (references/marker state intent)
                                   :reply {:status :resolved :path path :entry {:content revision} :from 999 :to 9999}})))

(deftest jumps-use-the-actually-loaded-revision-not-resolver-offsets
  (let [state (fixture)]
    (is (= {:id (str block) :block-id (str block) :from 8 :to 30} (references/jump state path)))
    (doseq [changed [(assoc-in state [:drafts path :text] "unsaved")
                     (assoc-in state [:block-reference :applied?] true)
                     (assoc state :target-verified? false)
                     (assoc-in state [:drafts path :source :repository-id] room)
                     (assoc-in state [:drafts path :source :entry :content] room)
                     (assoc-in state [:block-reference :reply :entry :content] room)
                     (assoc-in state [:drafts path :incoming-entry] {:content room})
                     (assoc-in state [:drafts path :remote-deleted?] true)
                     (assoc state :connection 2)]]
      (is (nil? (references/jump changed path))))
    (is (= :memory-block (:kind (routes/route->ref (get (references/block-links room path (get-in state [:drafts path])) (str block))))))))

(deftest displayed-file-and-copy-address-change-together
  (let [tab {:type :memory :data {:room-id room :path "wiki/Old.md"}}
        reference (assoc-in tab [:data :reference] {:repository-id repository :block-id block :path-hint "wiki/Old.md"})]
    (is (= path (:path (routes/tab->ref (references/selected-tab tab path false)))))
    (is (= path (:path (routes/tab->ref (references/selected-tab reference path true)))))
    (is (= :memory-block (:kind (routes/tab->ref (references/selected-tab reference path true)))))
    (is (= :memory-file (:kind (routes/tab->ref (references/selected-tab reference path false)))))))

(deftest acknowledgement-is-scoped-to-the-owned-target
  (let [a (assoc-in (fixture) [:block-reference :applied?] true)
        intent (get-in a [:block-reference :intent])
        b (assoc a :target-stamp room)
        attempted-b (assoc-in b [:block-reference] {:intent intent :request-marker (references/marker b intent)})]
    (is (references/completed? a intent))
    (is (= (str block) (references/linked-block a path)))
    (is (nil? (references/linked-block (assoc a :target-stamp room) path)))
    (is (nil? (references/linked-block (assoc-in a [:drafts path :incoming-entry] {:content room}) path)))
    (is (references/completed? (assoc a :connection 9 :target-epoch 3) intent))
    (is (not (references/completed? b intent)))
    (is (not (references/completed? (assoc attempted-b :target-stamp stamp) intent)))))
