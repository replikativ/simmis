(ns is.simm.uis.web.desktop.routes-test
  "The URL contract (#77). `ref->route` / `route->ref` are pure and
   cross-platform precisely so this can run without a DOM, a signal, or a
   browser — the paths are permanent once a link is shared, so they are worth
   pinning directly rather than through the router that will consume them."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.routes :as routes]))

(def ^:private scope "36bb8ed8-8f26-3bdd-929b-c86040a40948")
(def ^:private page  "a1b2c3d4-e5f6-7890-abcd-ef1234567890")
(def ^:private room  "c4be3aa1-2089-41ae-b6a2-102cb046ccf6")
(def ^:private msg   "5751a827-7a6f-385f-82b1-116889c3ff56")
(def ^:private thread "ee810e37-a7fe-4ec8-ae4a-50af80263fbc")
(def ^:private prop  "482998ac-818f-4161-ae29-b786a798e1e6")

(deftest every-addressable-kind-has-a-path
  (testing "a page needs BOTH scope and uuid — identity is (store, uuid)"
    (is (= (str "/page/" scope "/" page)
           (routes/ref->route {:kind :page :scope scope :page page}))))
  (testing "a room is a single id; the scope resolves from the roster"
    (is (= (str "/room/" room) (routes/ref->route {:kind :room :room room}))))
  (testing "a message is an anchor pair"
    (is (= (str "/room/" room "/m/" msg)
           (routes/ref->route {:kind :message :room room :message msg}))))
  (testing "a thread is a room plus stable root"
    (is (= (str "/room/" room "/t/" thread)
           (routes/ref->route {:kind :thread :room room :thread thread}))))
  (testing "files is a view of a room"
    (is (= (str "/room/" room "/files") (routes/ref->route {:kind :files :room room}))))
  (testing "a proposal is a single id"
    (is (= (str "/proposal/" prop) (routes/ref->route {:kind :proposal :id prop})))))

(deftest an-incomplete-ref-has-no-path
  ;; nil, not a partial path: a half-formed URL would open something adjacent
  ;; to what was meant, which is worse than not being a link.
  (is (nil? (routes/ref->route {:kind :page :page page})))          ; no scope
  (is (nil? (routes/ref->route {:kind :page :scope scope})))        ; no page
  (is (nil? (routes/ref->route {:kind :message :room room})))       ; no message
  (is (nil? (routes/ref->route {:kind :thread :room room})))        ; no root
  (is (nil? (routes/ref->route {:kind :proposal})))                 ; no id
  (is (nil? (routes/ref->route {:kind :settings})))                 ; not addressable
  (is (nil? (routes/ref->route {}))))

(deftest round-trip-is-lossless-for-every-kind
  (doseq [r [{:kind :page :scope scope :page page}
             {:kind :room :room room}
             {:kind :message :room room :message msg}
             {:kind :thread :room room :thread thread}
             {:kind :files :room room}
             {:kind :proposal :id prop}]]
    (testing (str (:kind r) " survives ref->route->ref")
      (is (= r (routes/route->ref (routes/ref->route r)))))))

(deftest unrecognised-paths-yield-nil
  (testing "unknown prefixes"
    (is (nil? (routes/route->ref "/")))
    (is (nil? (routes/route->ref "")))
    (is (nil? (routes/route->ref "/settings")))
    (is (nil? (routes/route->ref "/admin/secrets"))))
  (testing "right prefix, wrong arity — must not open a neighbour"
    (is (nil? (routes/route->ref (str "/page/" page))))             ; page without scope
    (is (nil? (routes/route->ref (str "/page/" scope "/" page "/extra"))))
    (is (nil? (routes/route->ref "/room")))
    (is (nil? (routes/route->ref (str "/room/" room "/bogus"))))
    (is (nil? (routes/route->ref (str "/room/" room "/x/" msg))))   ; anchor marker wrong
    (is (nil? (routes/route->ref "/proposal")))
    (is (nil? (routes/route->ref (str "/proposal/" prop "/extra"))))))

(deftest paths-tolerate-cosmetic-variation
  (testing "leading and trailing slashes and doubled separators"
    (is (= {:kind :room :room room} (routes/route->ref (str "room/" room))))
    (is (= {:kind :room :room room} (routes/route->ref (str "/room/" room "/"))))
    (is (= {:kind :proposal :id prop}
           (routes/route->ref (str "//proposal//" prop))))))

(deftest the-retired-prototype-path-is-not-revived
  ;; `#/page/{uuid}` was the old router's whole wiki vocabulary. It cannot
  ;; address a page in this data model — identity is (store, uuid) — so it must
  ;; resolve to nothing rather than to some arbitrary store's page.
  (is (nil? (routes/route->ref (str "/page/" page)))))

;; =============================================================================
;; tab -> ref: the direction the URL writer uses
;; =============================================================================

(deftest tabs-map-back-to-the-ref-they-show
  (testing "wiki needs both halves of page identity"
    (is (= {:kind :page :scope scope :page page}
           (routes/tab->ref {:type :wiki :data {:page-uuid page :db-scope scope}})))
    (is (nil? (routes/tab->ref {:type :wiki :data {:page-uuid page}}))))
  (testing "a chat is a room, unless it is windowed on a message"
    (is (= {:kind :room :room room}
           (routes/tab->ref {:type :chat :data {:room-id room}})))
    (is (= {:kind :message :room room :message msg}
           (routes/tab->ref {:type :chat :data {:room-id room :anchor-message msg}}))))
  (testing "a focused thread retains room and root identity"
    (is (= {:kind :thread :room room :thread thread}
           (routes/tab->ref {:type :chat-thread
                             :data {:room-id room :thread-root-id thread}}))))
  (testing "files"
    (is (= {:kind :files :room room}
           (routes/tab->ref {:type :files :data {:room-id room}}))))
  (testing "ONE proposal is addressable; the LIST is not"
    (is (= {:kind :proposal :id prop}
           (routes/tab->ref {:type :proposals :data {:proposal-id prop}})))
    (is (nil? (routes/tab->ref {:type :proposals :data {}}))))
  (testing "singletons and perspectives stay unaddressable"
    (doseq [t [:settings :admin :home :feed :tasks :schedules :accounting]]
      (is (nil? (routes/tab->ref {:type t :data {}}))
          (str t " must not be addressable")))))

(deftest tab-data-may-carry-uuids-not-strings
  ;; ref->tab builds :page-uuid as a UUID object while :db-scope stays a string,
  ;; so the inverse must not assume either.
  (is (= {:kind :page :scope scope :page page}
         (routes/tab->ref {:type :wiki
                           :data {:page-uuid #uuid "a1b2c3d4-e5f6-7890-abcd-ef1234567890"
                                  :db-scope scope}}))))

(deftest tab-route-round-trips-through-the-url
  ;; The projection the router runs: layout -> ref -> path, and popstate back.
  ;; If this is not stable, the URL and the layout fight.
  (doseq [tab [{:type :wiki :data {:page-uuid page :db-scope scope}}
               {:type :chat :data {:room-id room}}
               {:type :chat :data {:room-id room :anchor-message msg}}
               {:type :chat-thread :data {:room-id room :thread-root-id thread}}
               {:type :files :data {:room-id room}}
               {:type :proposals :data {:proposal-id prop}}]]
    (let [r (routes/tab->ref tab)]
      (is (= r (routes/route->ref (routes/tab->route tab)))
          (str "unstable round trip for " (:type tab))))))

(deftest an-unaddressable-tab-has-no-route
  (is (nil? (routes/tab->route {:type :settings :data {}})))
  (is (nil? (routes/tab->route {:type :proposals :data {}}))))
