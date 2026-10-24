(ns is.simm.uis.web.desktop.memory-backlinks-test
  (:require [is.simm.uis.web.desktop.memory-backlinks :as backlinks]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(def room "39bb0b7b-938d-46d7-8a96-5e2f1061c5a5")
(def connection {:epoch :socket})
(def roster {:rooms [{:room/id room}]})
(def stamp #uuid "e2f38e61-d095-4af6-9e26-dd51d575814e")
(def repository #uuid "c122c4df-7c50-41f9-b046-634cc34395a4")
(def basis #uuid "9ec3553a-8879-4056-8e48-28593b9e2815")
(def state {:selected "wiki/Target.md" :connection connection :verified-connection connection
            :target-verified? true :target-stamp stamp :target-epoch 1 :read-epoch 1 :definition-basis basis
            :drafts {"wiki/Target.md" {:text "Draft" :source {:kind :markdown :target-stamp stamp
                                                           :repository-id repository :entry {:content basis}}}}})

(defn- marker [state] (backlinks/marker room state connection roster))
(defn- cached []
  (assoc state :backlinks {:open? true :marker (marker state)
                           :reply {:status :ready :complete? true :results [] :target-stamp stamp
                                   :repository-id repository :source-basis basis :target {:path "wiki/Target.md"}}}))

(deftest saved-observation-does-not-follow-unsaved-local-typing
  (let [original (cached) typed (assoc-in original [:drafts "wiki/Target.md" :text] "Unsaved change")]
    (is (= (marker original) (marker typed)))
    (is (= (:reply (:backlinks original)) (backlinks/reply typed (marker typed))))
    (is (= {:expected-target-stamp stamp :repository-id repository :target {:kind :file :path "wiki/Target.md"}}
           (backlinks/request (marker typed))))))

(deftest metadata-read-epoch-and-roster-all-retire-the-old-observation
  (doseq [changed [(assoc (cached) :definition-basis repository)
                   (update (cached) :read-epoch inc)
                   (update (cached) :target-epoch inc)
                   (assoc (cached) :target-verified? false)
                   (assoc (cached) :selected "wiki/Other.md")]]
    (is (nil? (backlinks/reply changed (marker changed)))))
  (is (nil? (backlinks/reply (cached) (backlinks/marker room (cached) connection (assoc roster :agents [])))))
  (is (nil? (backlinks/marker room state connection {:rooms []})))
  (is (nil? (backlinks/marker room state {:epoch :new-socket} roster))))

(deftest old-errors-and-loading-are-not-presented-as-current
  (let [old (-> (cached) (assoc-in [:backlinks :error] "Old failure") (assoc-in [:loading :backlinks] true))
        changed (update old :read-epoch inc)
        view (backlinks/observation changed (marker changed))]
    (is (:open? view)) (is (nil? (:reply view))) (is (nil? (:error view))) (is (not (:loading? view))))
  (doseq [path [[:target-stamp] [:repository-id] [:source-basis] [:target :path]]]
    (let [bad (assoc-in (cached) (into [:backlinks :reply] path) nil)]
      (is (nil? (backlinks/reply bad (marker bad)))))))
