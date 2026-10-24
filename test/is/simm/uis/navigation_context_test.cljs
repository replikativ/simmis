(ns is.simm.uis.navigation-context-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.dom.router :as spin-router]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.db-signal :as db]
            [is.simm.uis.web.desktop.datahike-query :as query]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.uis.web.desktop.router :as router])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(def room #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def proposal #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")

(defn- layout []
  (mapv (fn [id] {:id id :width 0.5 :active-tab (str id "-tab")
                  :tabs [{:id (str id "-tab") :type :home :data {}}]})
        ["first" "second"]))

(defn- focused [] (sig/focused-tab @sig/layout-columns @sig/active-column-id))

(defn- with-navigation [f]
  (let [live (context/fork-context runtime/runtime)
        listeners @#'sig/focus-listeners saved-listeners @listeners
        last-synced @#'router/last-synced saved-path @last-synced
        history (atom []) container (.createElement js/document "div")]
    (.setAttribute container "id" "columns-container")
    (doseq [id ["first" "second"]]
      (let [column (.createElement js/document "div")]
        (.setAttribute column "data-col-id" id)
        (set! (.-className column) (if (= "first" id) "column active" "column"))
        (.appendChild container column)))
    (.appendChild (.-body js/document) container)
    (try
      ;; Match the real [router & args] arity: compiled callers invoke the
      ;; variadic entry point directly even when navigating by one path.
      (with-redefs [runtime/runtime live
                    spin-router/navigate! (fn [_ & args] (swap! history conj [:push (first args)]))
                    spin-router/replace! (fn [_ & args] (swap! history conj [:replace (first args)]))]
        (binding [ec/*execution-context* live]
          (reset! sig/layout-columns (layout))
          (reset! sig/active-column-id "first")
          (reset! sig/active-nav-keys #{})
          (reset! sig/chat-scroll-windows {})
          (reset! sig/user-rooms {:rooms [{:room/id room :room/name "Root room"
                                          :room/content-db-scope "root-scope"}]})
          (reset! listeners {})
          (reset! last-synced ::unset)
          ;; The real URL observer derives the focused tab. Only its final
          ;; browser-history device operation is replaced in this fixture.
          (sig/on-focus-change! ::url router/sync-url!)
          (f {:live live :history history :container container})))
      (finally
        (reset! listeners saved-listeners)
        (reset! last-synced saved-path)
        (.remove container)
        (context/close-context! live)))))

(deftest references-and-new-columns-navigate-only-in-the-selected-fork
  (with-navigation
    (fn [{:keys [live history container]}]
      (let [before {:columns @sig/layout-columns :active @sig/active-column-id
                    :nav @sig/active-nav-keys :scroll @sig/chat-scroll-windows}
            fork (context/fork-context live)]
        (try
          (binding [ec/*execution-context* fork]
            (reset! sig/user-rooms {:rooms [{:room/id room :room/name "Fork room"
                                            :room/content-db-scope "fork-scope"}]})
            (refs/open! {:kind :room :room room} {:new-tab? true})
            (is (= "fork-scope" (get-in (focused) [:data :db-scope]))
                "Reference lookup and tab reconciliation both use the fork roster")
            (is (= :end (get @sig/chat-scroll-windows room)))
            (is (= 2 (count (get-in (first @sig/layout-columns) [:tabs]))))
            (refs/open! {:kind :proposal :id proposal} {:new-column? true})
            (is (= 3 (count @sig/layout-columns)))
            (is (= :proposals (:type (focused))))
            (is (contains? @sig/active-nav-keys [:proposals nil]))
            (is (= (str proposal) (get-in (focused) [:data :proposal-id])))
            (sig/set-active-column! "second")
            (is (= "second" @sig/active-column-id)))
          (is (= before {:columns @sig/layout-columns :active @sig/active-column-id
                         :nav @sig/active-nav-keys :scroll @sig/chat-scroll-windows}))
          (is (= "root-scope" (get-in @sig/user-rooms [:rooms 0 :room/content-db-scope])))
          (is (empty? @history) "Fork navigation cannot call the native URL observer")
          (is (.contains (.-classList (.querySelector container "[data-col-id='first']")) "active"))
          (is (not (.contains (.-classList (.querySelector container "[data-col-id='second']")) "active")))
          (finally (context/close-context! fork)))))))

(deftest copied-live-frames-and-unbound-callbacks-still-navigate-the-live-ui
  (with-navigation
    (fn [{:keys [live history container]}]
      (binding [ec/*execution-context* (assoc live ::execution-frame true)]
        (sig/set-active-column! "second")
        (refs/open! {:kind :proposal :id proposal}))
      (is (= "second" @sig/active-column-id))
      (is (= :proposals (:type (focused))))
      (is (= [:push (str "/proposal/" proposal)] (last @history)))
      (is (.contains (.-classList (.querySelector container "[data-col-id='second']")) "active"))
      (binding [ec/*execution-context* nil]
        (refs/open! {:kind :room :room room}))
      (is (= "root-scope" (get-in (focused) [:data :db-scope])))
      (is (= :end (get @sig/chat-scroll-windows room)))
      (is (= [:push (str "/room/" room)] (last @history)))
      ;; open-tab! also supports a raw device callback without refs/open!.
      (binding [ec/*execution-context* nil]
        (sig/open-tab! :proposals {:proposal-id (str proposal)}))
      (is (= :proposals (:type (focused))))
      (is (= [:push (str "/proposal/" proposal)] (last @history))))))

(deftest selecting-existing-tabs-and-singletons-preserves-the-callers-world
  (with-navigation
    (fn [{:keys [live history]}]
      (swap! sig/layout-columns update-in [0 :tabs] conj
             {:id "existing-proposal" :type :proposals :data {:proposal-id (str proposal)}})
      (let [before @sig/layout-columns fork (context/fork-context live)]
        (try
          (binding [ec/*execution-context* fork]
            (sig/set-active-tab! "first" "existing-proposal")
            (is (= "existing-proposal" (get-in @sig/layout-columns [0 :active-tab])))
            (sig/set-active-tab! "first" "first-tab")
            (sig/open-or-activate-tab! :proposals {:proposal-id (str proposal)} {})
            (is (= "existing-proposal" (get-in @sig/layout-columns [0 :active-tab]))))
          (is (= before @sig/layout-columns))
          (is (empty? @history))
          (binding [ec/*execution-context* nil]
            (sig/open-or-activate-tab! :proposals {:proposal-id (str proposal)} {}))
          (is (= "existing-proposal" (get-in @sig/layout-columns [0 :active-tab])))
          (is (= [:push (str "/proposal/" proposal)] (last @history)))
          (finally (context/close-context! fork)))))))

(deftest invalid-column-placement-never-removes-the-source-tab
  (with-navigation
    (fn [_]
      (let [before @sig/layout-columns]
        (is (try (sig/create-column-with-tab! "first" "first-tab" :right) false
                 (catch :default error
                   (= :is.simm.uis.web.desktop.signals/invalid-column-position (:type (ex-data error))))))
        (is (= before @sig/layout-columns))))))

(deftest same-column-and-invalid-tab-moves-preserve-the-original-layout
  (with-navigation
    (fn [{:keys [history]}]
      (let [before @sig/layout-columns]
        (doseq [[from tab to] [["first" "first-tab" "first"]
                               ["first" "missing-tab" "second"]
                               ["first" "first-tab" "missing-column"]
                               ["missing-column" "first-tab" "second"]]]
          (sig/move-tab! from tab to)
          (is (= before @sig/layout-columns)))
        (is (empty? @history))))))

(deftest fork-tab-closes-keep-semantic-state-local-and-do-not-release-live-replicas
  (with-navigation
    (fn [{:keys [live history]}]
      (let [page #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc" calls (atom [])]
        (with-redefs [sig/drive-data (signal live {})
                      query/evict-cache-for-entity! (fn [id] (swap! calls conj [:query id]))
                      db/disconnect-kb! (fn [scope] (swap! calls conj [:disconnect scope]))
                      db/evict-view-db-for-scope! (fn [scope] (swap! calls conj [:view scope]))]
          (swap! sig/layout-columns update-in [0 :tabs] into
                 [{:id "wiki" :type :wiki :data {:page-uuid page :db-scope "kb-scope"}}
                  {:id "chat" :type :chat :data {:room-id (str room) :db-scope "chat-scope"}}
                  {:id "files" :type :files :data {:room-id (str room)}}
                  {:id "memory" :type :memory :data {:room-id (str room)}}])
          (reset! sig/drive-data {(str room) {:retained true}})
          (let [before @sig/layout-columns drive-before @sig/drive-data fork (context/fork-context live)]
            (try
              (binding [ec/*execution-context* fork]
                (sig/close-tab! "first" "wiki")
                (sig/close-tab! "first" "chat")
                (sig/close-tab! "first" "files")
                (sig/close-tab-of-type! :memory)
                (is (= ["first-tab"] (mapv :id (get-in @sig/layout-columns [0 :tabs]))))
                (is (nil? (get @sig/drive-data (str room)))))
              (is (= before @sig/layout-columns))
              (is (= drive-before @sig/drive-data))
              (is (empty? @calls))
              (is (empty? @history))
              (binding [ec/*execution-context* nil] (sig/close-tab! "first" "wiki"))
              (is (= [[:query page] [:disconnect "kb-scope"] [:view "kb-scope"]] @calls))
              (finally (context/close-context! fork)))))))))
