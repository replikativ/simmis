(ns is.simm.uis.web.desktop.backlink-target-test
  "What a wiki backlink row opens (SIM-R09).

   The bug this pins: the backlinks panel rendered a `:chat-room` row as a
   clickable link, and the only thing that row carried was a room's DISPLAY
   NAME. Clicking it called `open-tab! :chat {:room-name title}` — a chat tab
   with no room id. Before SIM-R08 that tab sat on \"Loading messages…\"
   forever; after SIM-R08 it reports a missing room. The control never worked
   either way, because a name is not an identity: room names are not unique,
   so there is no room such a row could correctly resolve to.

   These assertions state the line: a row is opened by its UUID or it is not
   opened at all, and nothing here ever consults a title to find a room."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.backlink-target :as bt]
            [is.simm.uis.web.desktop.tab-heal :as tab-heal]))

(def ^:private design-a-id    #uuid "a4a148b0-f140-4d88-b76d-ab8999f772da")
(def ^:private design-a-scope #uuid "46c7d2d8-11a9-3416-9caa-f4be8903e7a0")
(def ^:private design-b-id    #uuid "b1b1b1b1-0000-4000-8000-000000000002")
(def ^:private design-b-scope #uuid "c2c2c2c2-0000-4000-8000-000000000003")

;; TWO ROOMS, ONE NAME. This is the whole argument, so it is the fixture.
(def ^:private rooms
  [{:room/id design-a-id :room/name "Design" :room/content-db-scope design-a-scope}
   {:room/id design-b-id :room/name "Design" :room/content-db-scope design-b-scope}])

(def ^:private roster {:rooms rooms})

(defn- message-backlink [room-id]
  {:type :chat-message
   :title "Design"
   :room-uuid (str room-id)
   :message-uuid "m-1"
   :sent-at 1700000000000})

;; ---------------------------------------------------------------------------
;; The preferred path: identity is a uuid, and it carries the scope with it
;; ---------------------------------------------------------------------------

(deftest a-uuid-backed-backlink-opens-that-room-and-its-scope
  (testing "the room id decides the room, and the roster decides its store —
            a chat tab with no `:db-scope` has no replica to read from"
    (let [scope  (bt/room-scope rooms design-b-id)
          target (bt/target (message-backlink design-b-id) scope)]
      (is (= :open (:action target)))
      (is (= :chat (:tab-type target)))
      (is (= (str design-b-id) (get-in target [:tab-data :room-id])))
      (is (= (str design-b-scope) (get-in target [:tab-data :db-scope])))
      (is (= "m-1" (get-in target [:tab-data :anchor-message]))
          "a message backlink jumps to the message, not just the room"))))

(deftest duplicate-room-names-never-resolve-to-the-wrong-room
  (testing "both rooms are called Design; each backlink opens its OWN room"
    (doseq [[room-id scope] [[design-a-id design-a-scope]
                             [design-b-id design-b-scope]]]
      (let [target (bt/target (message-backlink room-id)
                              (bt/room-scope rooms room-id))]
        (is (= (str room-id) (get-in target [:tab-data :room-id])))
        (is (= (str scope) (get-in target [:tab-data :db-scope]))))))
  (testing "the shared title is never an input to the lookup — a row titled
            after room A but carrying room B's id opens room B"
    (let [target (bt/target (assoc (message-backlink design-b-id)
                                   :title "Design")
                            (bt/room-scope rooms design-b-id))]
      (is (= (str design-b-id) (get-in target [:tab-data :room-id])))))
  (testing "`room-scope` is a lookup by id and knows nothing about names"
    (is (= (str design-a-scope) (bt/room-scope rooms design-a-id)))
    (is (nil? (bt/room-scope rooms #uuid "00000000-0000-0000-0000-000000000000")))
    (is (nil? (bt/room-scope rooms nil)))))

;; ---------------------------------------------------------------------------
;; The legacy name-only row: unavailable, and never guessed at
;; ---------------------------------------------------------------------------

(deftest a-name-only-chat-row-is-unavailable-not-clickable
  (testing "a `:chat-room` row carrying only a title cannot name a room, so
            it is not rendered as a control"
    (let [target (bt/target {:type :chat-room :title "Design"} nil)]
      (is (= :unavailable (:action target)))
      (is (= :room-not-identified (:reason target)))
      (is (= "Design" (:title target)) "it still says what it was called")
      (is (nil? (:tab-data target)) "there is nothing to open")
      (is (not (bt/openable? {:type :chat-room :title "Design"})))))
  (testing "the explanation says why, in the panel's own words"
    (is (re-find #"not unique" (bt/explanation :room-not-identified))))
  (testing "a `:chat-message` row that lost its room id gets the same verdict,
            rather than opening a tab keyed on nothing"
    (is (not (bt/openable? (dissoc (message-backlink design-a-id) :room-uuid))))))

(deftest a-chat-room-row-that-does-carry-a-uuid-opens-normally
  (testing "the verdict is about IDENTITY, not about the row's type — if a
            `:chat-room` row ever carries a room id, it opens that room"
    (let [bl     {:type :chat-room :title "Design" :room-uuid (str design-a-id)}
          target (bt/target bl (bt/room-scope rooms design-a-id))]
      (is (= :open (:action target)))
      (is (= :chat (:tab-type target)))
      (is (= (str design-a-id) (get-in target [:tab-data :room-id])))
      (is (= (str design-a-scope) (get-in target [:tab-data :db-scope])))
      (is (nil? (get-in target [:tab-data :anchor-message]))
          "a room row has no message to anchor on"))))

;; ---------------------------------------------------------------------------
;; A room that is gone, or was never shared: `tab-heal` owns that verdict
;; ---------------------------------------------------------------------------

(deftest a-deleted-or-unshared-room-opens-an-explicitly-missing-tab
  (testing "an id the roster does not name is still an id — the row opens,
            and the tab it opens is marked `:room-missing?`, which the chat
            view states plainly instead of loading forever"
    (let [gone   (message-backlink #uuid "dddddddd-0000-4000-8000-00000000dead")
          target (bt/target gone (bt/room-scope rooms (:room-uuid gone)))
          tab    (tab-heal/reconcile-tab
                   {:id "t1" :type :chat :title (:title target)
                    :data (:tab-data target)}
                   roster)]
      (is (= :open (:action target)) "identity is present, so it is a link")
      (is (nil? (get-in target [:tab-data :db-scope])))
      (is (get-in tab [:data :room-missing?]))))
  (testing "and that verdict is revocable — a room shared with you later heals,
            which is why this panel must NOT condemn an unknown id itself"
    (let [target (bt/target (message-backlink design-a-id) nil)
          tab    (tab-heal/reconcile-tab
                   {:id "t1" :type :chat :title (:title target)
                    :data (assoc (:tab-data target) :room-missing? true)}
                   roster)]
      (is (nil? (get-in tab [:data :room-missing?])))
      (is (= (str design-a-scope) (get-in tab [:data :db-scope]))
          "reconciliation fills the scope the click could not resolve"))))

;; ---------------------------------------------------------------------------
;; Page backlinks — the common case, unchanged
;; ---------------------------------------------------------------------------

(def ^:private page-uuid #uuid "0e0e0e0e-0000-4000-8000-000000000009")

(deftest a-page-backlink-still-opens-its-page
  (let [target (bt/target {:type :page
                           :entity/uuid page-uuid
                           :S.Page/title "Roadmap"}
                          "kb-scope-1")]
    (is (= :open (:action target)))
    (is (= :wiki (:tab-type target)))
    (is (= page-uuid (get-in target [:tab-data :page-uuid])))
    (is (= "Roadmap" (:title target)))
    (is (= "kb-scope-1" (get-in target [:tab-data :db-scope]))
        "page backlinks inherit the tab's scope; without it the page renders
         empty against the shared local db"))
  (testing "with no scope resolved the key is simply absent, not nil"
    (is (not (contains? (:tab-data (bt/target {:type :page
                                               :entity/uuid page-uuid
                                               :S.Page/title "Roadmap"} nil))
                        :db-scope))))
  (testing "a page row with a title but no uuid is unavailable too — it used
            to open a wiki tab on `{:page-uuid nil}`, which renders as an
            empty page and reads as data loss rather than a broken link"
    (let [target (bt/target {:type :page :S.Page/title "Roadmap"} nil)]
      (is (= :unavailable (:action target)))
      (is (= :page-not-identified (:reason target)))
      (is (= "Roadmap" (:title target)))))
  (testing "an untitled page still renders with a name"
    (is (= "Untitled" (:title (bt/target {:type :page} nil))))))

;; ---------------------------------------------------------------------------
;; Render keys — `ifor-each` memoizes on them, so a collision drops rows
;; ---------------------------------------------------------------------------

(deftest every-row-in-a-panel-gets-its-own-key
  (testing "eight messages matched in ONE room are eight rows; keying them by
            the room's title collapsed them into one"
    (let [rows (for [i (range 8)]
                 (assoc (message-backlink design-a-id) :message-uuid (str "m-" i)))]
      (is (= 8 (count (distinct (map bt/render-key rows)))))))
  (testing "keys stay distinct across the kinds a panel mixes"
    (let [rows [{:type :page :entity/uuid page-uuid :S.Page/title "Roadmap"}
                {:type :chat-room :title "Design"}
                {:type :chat-room :title "Design" :room-uuid (str design-a-id)}
                (message-backlink design-a-id)]]
      (is (= 4 (count (distinct (map bt/render-key rows)))))))
  (testing "the same message id in two same-named rooms still keys apart —
            message uuids are unique in practice, so this is belt-and-braces"
    (is (not= (bt/render-key (message-backlink design-a-id))
              (bt/render-key (message-backlink design-b-id)))))
  (testing "the key of a row does not change between renders"
    (let [row (message-backlink design-a-id)]
      (is (= (bt/render-key row) (bt/render-key row))))))
