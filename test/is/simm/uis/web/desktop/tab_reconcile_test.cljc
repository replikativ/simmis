(ns is.simm.uis.web.desktop.tab-reconcile-test
  "Order independence between a chat tab and the room roster.

   `tab_heal_test` pins the RULES — what a tab becomes once the roster is
   known. This pins the TRIGGER: that the rules run no matter which of the two
   facts arrived first.

   The half that shipped in 11dfab9 reconciled on roster arrival only, which
   repairs a cold-boot deep link and nothing else. A stale `/room/<uuid>` link
   clicked an hour into a session arrives when the roster landed long ago and
   will not land again on its own — so no reconciliation ever ran, no
   `:room-missing?` was ever written, and `db-loading?` stayed true until the
   tab was closed. The user-visible symptom is identical to the original bug:
   \"Loading messages…\", forever.

   These tests drive the same two entry points the app does — `reconcile-tab`
   (a tab is born; `signals/open-tab!` calls it) and `reconcile-layout` (the
   roster lands; `user-rooms-sync` calls it) — and assert the invariant that
   spans them: once BOTH facts are known, every chat tab is either resolved
   (a `:db-scope` and a `:room-name`) or explicitly `:room-missing?`, and
   never neither."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.tab-heal :as tab-heal]))

(def ^:private room-id  #uuid "a4a148b0-f140-4d88-b76d-ab8999f772da")
(def ^:private scope-id #uuid "46c7d2d8-11a9-3416-9caa-f4be8903e7a0")
(def ^:private other-id  #uuid "0b7a8ea1-2b1f-4de7-9f0e-9e6f6a4d1c33")
(def ^:private other-scope #uuid "5f1c9a44-2c53-4bd0-8f8a-27b7f2b9c111")

;; A uuid the roster does not name — the stale deep link, the deleted room,
;; the room shared with the OTHER account.
(def ^:private stale-id "deadbeef-0000-4000-8000-000000000000")

(def ^:private personal
  {:room/id room-id :room/name "Ada's Assistants"
   :room/type :personal-ai :room/content-db-scope scope-id})

(def ^:private team
  {:room/id other-id :room/name "Team"
   :room/type :group :room/content-db-scope other-scope})

;; A `load-rooms!` result, which is what `sig/user-rooms` holds.
(def ^:private roster {:rooms [personal team] :agents [] :knowledge-bases []})
(def ^:private roster-without-team {:rooms [personal] :agents []})

(defn- open-tab
  "The tab map `signals/open-tab!` builds, put through the same reconciliation
   that mutator applies before it writes the layout."
  [tab-data roster]
  (tab-heal/reconcile-tab {:id "new-tab" :type :chat :title "Chat" :data tab-data}
                          roster))

(defn- resolved? [tab]
  (and (some? (get-in tab [:data :db-scope]))
       (some? (get-in tab [:data :room-name]))
       (not (get-in tab [:data :room-missing?]))))

(defn- missing? [tab] (true? (get-in tab [:data :room-missing?])))

(defn- settled?
  "The invariant, stated once: resolved or missing, and exactly one of them."
  [tab]
  (not= (resolved? tab) (missing? tab)))

;; -----------------------------------------------------------------------------
;; The two orders
;; -----------------------------------------------------------------------------

(deftest tab-first-then-roster
  (testing "the cold-boot deep link: the tab exists before any roster does"
    (let [born (open-tab {:room-id (str room-id)} nil)]
      (is (nil? (get-in born [:data :db-scope]))
          "nothing to resolve against yet — and no verdict passed either")
      (is (not (missing? born))
          "an unknown roster is not evidence of a missing room")
      (let [cols   [{:id "c1" :tabs [born] :active-tab "new-tab"}]
            healed (tab-heal/reconcile-layout cols roster)
            tab    (get-in healed [0 :tabs 0])]
        (is (settled? tab))
        (is (resolved? tab))
        (is (= (str scope-id) (get-in tab [:data :db-scope])))))))

(deftest roster-first-then-valid-tab
  (testing "the roster landed long ago; opening a real room resolves at birth,
            with no second roster arrival to wait for"
    (let [tab (open-tab {:room-id (str other-id)} roster)]
      (is (settled? tab))
      (is (resolved? tab))
      (is (= (str other-scope) (get-in tab [:data :db-scope]))
          "without a scope the chat column connects no replica and spins")
      (is (= "Team" (:title tab))))))

(deftest roster-first-then-invalid-tab
  (testing "THE UNCOVERED CASE. A stale link opened into a loaded session used
            to produce a tab with a room-id, no scope, and no verdict — which
            renders as \"Loading messages…\" until the tab is closed, because
            nothing further is scheduled to arrive"
    (let [tab (open-tab {:room-id stale-id} roster)]
      (is (settled? tab))
      (is (missing? tab))
      (is (nil? (get-in tab [:data :db-scope]))
          "no scope is invented for a room the roster does not name"))))

(deftest either-order-reaches-the-same-place
  (testing "the tab and the roster commute: the end state does not record
            which one showed up first"
    (doseq [[label data] [["a room the roster names" {:room-id (str other-id)}]
                          ["a room it does not"      {:room-id stale-id}]]]
      (let [;; tab first, roster second
            a (get-in (tab-heal/reconcile-layout
                        [{:id "c1" :tabs [(open-tab data nil)]}]
                        roster)
                      [0 :tabs 0])
            ;; roster first, tab second
            b (open-tab data roster)]
        (is (settled? a) label)
        (is (= a b) label)))))

;; -----------------------------------------------------------------------------
;; Revocability, idempotence, and the absence of a loop
;; -----------------------------------------------------------------------------

(deftest a-later-roster-makes-a-missing-room-valid
  (testing "a room shared with you mid-session heals — the verdict is a
            conclusion from the evidence, and the evidence can change"
    (let [cols      [{:id "c1" :tabs [(open-tab {:room-id (str other-id)} nil)]}]
          condemned (tab-heal/reconcile-layout cols roster-without-team)
          shared    (tab-heal/reconcile-layout condemned roster)]
      (is (missing? (get-in condemned [0 :tabs 0])))
      (is (resolved? (get-in shared [0 :tabs 0])))
      (is (= (str other-scope) (get-in shared [0 :tabs 0 :data :db-scope]))))))

(deftest reconciliation-is-idempotent
  (testing "running it again changes nothing, for every state a tab can be in"
    (doseq [data [{:room-id (str room-id)}
                  {:room-id stale-id}
                  {:room-id "personal-ai-placeholder" :room-name "Assistants"}
                  {:room-name "legacy backlink, no room-id"}]]
      (let [once  (open-tab data roster)
            twice (tab-heal/reconcile-tab once roster)]
        (is (= once twice) (pr-str data))))))

(deftest a-settled-layout-is-returned-unchanged-identity-included
  (testing "no write, so no re-render, so no repeated `ensure-room!` — the
            chat render fires that RPC as a side effect of rendering, and a
            reconciliation that rewrote an equal layout on every roster
            refresh would fire it again each time"
    (let [cols   (tab-heal/reconcile-layout
                   [{:id "c1" :tabs [(open-tab {:room-id (str room-id)} nil)
                                     (open-tab {:room-id stale-id} nil)]}]
                   roster)
          again  (tab-heal/reconcile-layout cols roster)
          third  (tab-heal/reconcile-layout again roster)]
      (is (identical? cols again)
          "`swap!` on an identical value is what keeps this off the render path")
      (is (identical? cols third)))))

(deftest an-unknown-roster-is-never-a-verdict
  (testing "nil is \"not answered yet\"; an empty room list is an answer"
    (is (false? (tab-heal/roster-known? nil)))
    (is (true? (tab-heal/roster-known? {:rooms []})))
    (is (true? (tab-heal/roster-known? [])))
    (let [cols [{:id "c1" :tabs [(open-tab {:room-id stale-id} nil)]}]]
      (is (identical? cols (tab-heal/reconcile-layout cols nil))
          "no roster, no reconciliation, no premature condemnation")
      (is (missing? (get-in (tab-heal/reconcile-layout cols {:rooms []})
                            [0 :tabs 0]))
          "a party with no rooms can open none of them"))))

;; -----------------------------------------------------------------------------
;; More than one column, more than one tab
;; -----------------------------------------------------------------------------

(deftest every-column-and-every-tab-settles
  (testing "a tab is settled by its own room, not by its neighbours — two
            columns, a healthy tab and a stale tab for the same room among
            them (which is how SIM-R07's close-by-identity case arises)"
    (let [cols   [{:id "left"
                   :tabs [(open-tab {:room-id (str room-id)} nil)
                          (open-tab {:room-id stale-id} nil)
                          {:id "w" :type :wiki :data {:page-uuid "p"}}]
                   :active-tab "new-tab"}
                  {:id "right"
                   :tabs [(open-tab {:room-id stale-id} nil)
                          (open-tab {:room-id (str other-id)} nil)]
                   :active-tab "new-tab"}]
          healed (tab-heal/reconcile-layout cols roster)
          chats  (for [col healed, tab (:tabs col) :when (= :chat (:type tab))] tab)]
      (is (= 4 (count chats)))
      (is (every? settled? chats))
      (is (= [true false false true] (mapv resolved? (vec chats)))
          "left: healthy then stale; right: stale then healthy")
      (is (= {:id "w" :type :wiki :data {:page-uuid "p"}}
             (get-in healed [0 :tabs 2]))
          "non-chat tabs are not touched")
      (is (= ["left" "right"] (mapv :id healed))
          "and the column structure is preserved"))))
