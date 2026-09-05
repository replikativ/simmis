(ns is.simm.uis.web.desktop.tab-heal-test
  "Reconciling open tabs against the room roster (BUG-C).

   The bug this pins: a cold-boot deep link to `/room/<id>` opens a chat tab
   BEFORE the roster has arrived, so `refs/ref->tab` resolves no `:db-scope`
   and no room name. The chat column then reports \"Loading messages…\" forever
   — nothing is connecting, so nothing ever arrives — while the sidebar shows
   the very same room, healthy. `heal-chat-tab` is what makes the roster's
   arrival repair that tab, and the rules are pinned here rather than in prose
   because they are what separate \"not yet\" from \"not ever\"."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.tab-heal :as tab-heal]))

(def ^:private room-id  #uuid "a4a148b0-f140-4d88-b76d-ab8999f772da")
(def ^:private scope-id #uuid "46c7d2d8-11a9-3416-9caa-f4be8903e7a0")

(def ^:private personal
  {:room/id room-id
   :room/name "Ada's Assistants"
   :room/type :personal-ai
   :room/content-db-scope scope-id})

(def ^:private rooms [personal])

(defn- chat-tab [data] {:id "t1" :type :chat :title "Chat" :data data})

(def ^:private thread-root #uuid "7c2f0b1e-3d55-4a20-9a71-0c6f2e4b8a13")

(defn- thread-tab [data] {:id "t2" :type :chat-thread :title "Thread" :data data})

(deftest a-deep-link-tab-learns-its-scope-and-name
  (testing "the cold-boot deep link: room-id only, because the URL carries
            nothing else and the roster did not exist yet"
    (let [healed (tab-heal/heal-chat-tab (chat-tab {:room-id (str room-id)})
                                         rooms personal)]
      (is (= (str scope-id) (get-in healed [:data :db-scope]))
          "without this the chat column waits for a replica nobody requested")
      (is (= "Ada's Assistants" (get-in healed [:data :room-name])))
      (is (= "Ada's Assistants" (:title healed))
          "the tab reads \"Chat\" until the roster supplies a name")
      (is (nil? (get-in healed [:data :room-missing?]))))))

(deftest a-tab-that-already-resolved-is-left-alone
  (testing "a healthy tab keeps its own scope and its own label"
    (let [tab (assoc (chat-tab {:room-id (str room-id)
                                :db-scope "other-scope"
                                :room-name "What I called it"})
                     :title "What I called it")]
      (is (= tab (tab-heal/heal-chat-tab tab rooms personal))))))

(deftest the-boot-placeholder-adopts-the-real-room
  (testing "the default layout ships a stand-in tab; its stand-in NAME is a
            stand-in too, so it is replaced rather than kept"
    (let [healed (tab-heal/heal-chat-tab
                   (assoc (chat-tab {:room-id "personal-ai-placeholder"
                                     :room-name "Assistants"})
                          :title "Assistants")
                   rooms personal)]
      (is (= (str room-id) (get-in healed [:data :room-id])))
      (is (= (str scope-id) (get-in healed [:data :db-scope])))
      (is (= "Ada's Assistants" (:title healed)))))
  (testing "with no personal-ai room in the roster there is nothing to adopt"
    (is (get-in (tab-heal/heal-chat-tab
                  (chat-tab {:room-id "personal-ai-placeholder"}) [] nil)
                [:data :room-missing?]))))

(deftest a-room-the-roster-does-not-name-is-a-conclusion
  (testing "the roster is the complete list of openable rooms, so its silence
            about a room is an answer — not a reason to keep spinning"
    (is (get-in (tab-heal/heal-chat-tab
                  (chat-tab {:room-id "00000000-0000-0000-0000-000000000000"})
                  rooms personal)
                [:data :room-missing?])))
  (testing "a tab with no room-id at all reaches the same conclusion (the
            legacy :chat-room backlink opens one carrying only a title)"
    (is (get-in (tab-heal/heal-chat-tab (chat-tab {:room-name "Some room"})
                                        rooms personal)
                [:data :room-missing?]))))

(deftest the-verdict-is-revocable
  (testing "a room shared with you mid-session heals; a condemned tab is not
            condemned forever"
    (let [condemned (chat-tab {:room-id (str room-id) :room-missing? true})
          healed    (tab-heal/heal-chat-tab condemned rooms personal)]
      (is (nil? (get-in healed [:data :room-missing?])))
      (is (= (str scope-id) (get-in healed [:data :db-scope]))))))

(deftest a-thread-tab-is-a-room-tab-and-heals-like-one
  (testing "a deep link to a thread carries a room id and nothing else, so it
            fails the same way a chat deep link does"
    (let [healed (tab-heal/heal-chat-tab
                  (thread-tab {:room-id (str room-id)
                               :thread-root-id (str thread-root)})
                  rooms personal)]
      (is (= (str scope-id) (get-in healed [:data :db-scope]))
          "without this the thread column waits for a replica nobody requested")
      (is (= "Ada's Assistants" (get-in healed [:data :room-name])))
      (is (= "Thread · Ada's Assistants" (:title healed))
          "the label says which room the thread is in, as open-tab! writes it")
      (is (nil? (get-in healed [:data :room-missing?])))))

  (testing "thread identity survives the repair — it is not a room id"
    (let [healed (tab-heal/heal-chat-tab
                  (thread-tab {:room-id (str room-id)
                               :thread-root-id (str thread-root)})
                  rooms personal)]
      (is (= :chat-thread (:type healed)) "a thread tab is never rewritten to :chat")
      (is (= (str thread-root) (get-in healed [:data :thread-root-id])))))

  (testing "a thread in a room the roster does not name is the same conclusion"
    (let [healed (tab-heal/heal-chat-tab
                  (thread-tab {:room-id "11111111-2222-3333-4444-555555555555"
                               :thread-root-id (str thread-root)})
                  rooms personal)]
      (is (true? (get-in healed [:data :room-missing?])))
      (is (= (str thread-root) (get-in healed [:data :thread-root-id]))))))

(deftest non-chat-tabs-are-untouched
  (doseq [tab [{:type :wiki :data {:page-uuid "p" :db-scope "s"}}
               {:type :home :data {}}
               {:type :files :data {:room-id "whatever"}}]]
    (is (= tab (tab-heal/heal-chat-tab tab rooms personal)))))

(deftest healing-walks-the-whole-layout
  (let [cols [{:id "c1" :tabs [(chat-tab {:room-id (str room-id)})
                               {:type :home :data {}}]}
              {:id "c2" :tabs [(chat-tab {:room-id "11111111-2222-3333-4444-555555555555"})]}]
        healed (tab-heal/heal-chat-tabs cols rooms personal)]
    (is (= (str scope-id) (get-in healed [0 :tabs 0 :data :db-scope])))
    (is (= {:type :home :data {}} (get-in healed [0 :tabs 1]))
        "every column and every tab, and only the chat ones changed")
    (is (get-in healed [1 :tabs 0 :data :room-missing?]))))
