(ns is.simm.uis.web.desktop.conversation-capture-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.uis.conversation-capture-test :as example]
            [is.simm.uis.web.desktop.conversation-capture :as model]
            [is.simm.uis.web.desktop.conversation-capture-actions :as actions]
            [is.simm.uis.web.desktop.conversation-capture-remote :as remote]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.uis.web.desktop.views.conversation-capture :as view]
            [is.simm.runtimes.web :as web]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.dom.browser :as browser])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn- fixture [f]
  (binding [ec/*execution-context* runtime]
    (let [account "account" session {:id (random-uuid)} current-session (atom session)
          user (signal runtime {:id account}) epoch (atom :socket)
          states (signal runtime {})
          layout (signal runtime [{:id :column :active-tab :chat
                                   :tabs [{:id :chat :type :chat :data {:room-id example/id}}]}])
          pending (atom {}) calls (atom []) opened (atom [])
          data {:room-id (str example/id) :room-name "Room" :message-id example/message
                :col-id :column :tab-id :chat
                :frame {:account account :session (:id session) :connection :socket}}
          key (actions/state-key account (:room-id data) example/message)]
      (with-redefs [web/current-session #(deref current-session)
                    web/current-session? #(identical? % @current-session)
                    connection/snapshot #(deref epoch)
                    sig/current-user user
                    sig/conversation-captures states sig/layout-columns layout
                    ;; Match the real function's fixed-prefix variadic ABI;
                    ;; CLJS dispatches its variadic entrypoint directly.
                    navigation/open-file! (fn [room title path & [col stamp]]
                                            (swap! opened conj [room title path col stamp]))
                    remote/preview-conversation-capture!
                    (fn [_ room request]
                      (swap! calls conj [:preview room request])
                      (fn [ok fail] (swap! pending assoc :preview ok :preview-fail fail)))
                    remote/publish-conversation-capture!
                    (fn [_ room request]
                      (swap! calls conj [:publish room request])
                      (fn [ok fail] (swap! pending assoc :publish ok :publish-fail fail)))]
        (f {:a (actions/controller data) :data data :states states :state #(get @states key)
            :epoch epoch :layout layout :pending pending :calls calls :opened opened
            :session current-session :user user})))))

(deftest explicit-publish-once-then-open-only-the-acknowledged-file
  (fixture
   (fn [{:keys [a pending state calls opened]}]
     ((:preview a))
     (is (= 1 (count @calls))) (is (= :preview-loading (:phase (state))))
     ((:preview @pending) example/preview)
     (is (= :preview (:phase (state)))) (is (empty? @opened))
     ((:publish a)) ((:publish a))
     (is (= 1 (count (filter #(= :publish (first %)) @calls))))
     (is (= #{:message-ids :capture-id :expected-target-stamp} (set (keys (last (last @calls))))))
     ((:publish @pending) example/saved)
     (is (= :saved (:phase (state))))
     (is (empty? @opened) "ACK does not steal focus; opening is explicit")
     ((:open a))
     (is (= [[(str example/id) "Room" (model/path-for example/id) :column example/stamp]]
            @opened)))))

(deftest hide-reconnect-and-tab-replacement-retain-the-publication-hold
  (doseq [change [:hide :connection :tab :room :thread :type]]
    (fixture
     (fn [{:keys [a pending state calls epoch layout]}]
       ((:preview a)) ((:preview @pending) example/preview) ((:publish a))
       (case change
         :hide ((:hide a))
         :connection (reset! epoch :new-socket)
         :tab (swap! layout assoc-in [0 :active-tab] :other)
         :room (swap! layout assoc-in [0 :tabs 0 :data :room-id] (random-uuid))
         :thread (swap! layout assoc-in [0 :tabs 0 :data :thread-root-id] (random-uuid))
         :type (swap! layout assoc-in [0 :tabs 0 :type] :memory))
       ((:publish @pending) example/saved)
       (is (= :inspection-required (:phase (model/reconcile (state) @epoch))))
       ((:preview a)) ((:publish a))
       (is (= 2 (count @calls)) "No automatic or click-based replay")))))

(deftest replaced-account-or-session-cannot-settle-or-replay-old-work
  (doseq [change [:account :session]]
    (fixture
     (fn [{:keys [a pending state calls opened session user]}]
       ((:preview a)) ((:preview @pending) example/preview) ((:publish a))
       (let [before (state)]
         (case change
           :account (reset! user {:id "replacement-account"})
           :session (reset! session {:id (random-uuid)}))
         ((:publish @pending) example/saved)
         ((:preview a)) ((:publish a)) ((:open a))
         (is (= before (state)) "An old callback cannot mutate replacement session state")
         (is (= 2 (count @calls)))
         (is (empty? @opened)))))))

(deftest a-simulated-controller-never-dispatches-live-host-effects
  (fixture
   (fn [{:keys [data states calls opened]}]
     (let [child (context/fork-context runtime) before @states]
       (try
         (binding [ec/*execution-context* child]
           (let [a (actions/controller data)]
             ((:preview a)) ((:publish a)) ((:open a))
             (is (= before @states))))
         (is (= before @states))
         (is (empty? @calls)) (is (empty? @opened))
         (finally (context/close-context! child)))))))

(deftest same-home-environment-wrapping-still-allows-the-live-action
  (fixture
   (fn [{:keys [data pending state calls]}]
     (binding [ec/*execution-context* (assoc runtime :bindings
                                            (assoc (:bindings runtime) :capture-probe true))]
       (let [a (actions/controller data)]
         ((:preview a))
         (is (= 1 (count @calls)))
         ((:preview @pending) example/preview)
         (is (= :preview (:phase (state)))))))))

(deftest focused-thread-controller-requires-its-exact-thread
  (fixture
   (fn [{:keys [data layout calls]}]
     (let [root (random-uuid)]
       (swap! layout assoc-in [0 :tabs 0]
              {:id :chat :type :chat-thread :data {:room-id example/id :thread-root-id (str root)}})
       (let [a (actions/controller (assoc data :thread-root-id root))]
         ((:preview a))
         (is (= 1 (count @calls)))
         (swap! layout assoc-in [0 :tabs 0 :data :thread-root-id] (str (random-uuid)))
         ((:preview a))
         (is (= 1 (count @calls))))))))

(deftest a-forged-or-failed-publish-never-opens-a-file
  (fixture
   (fn [{:keys [a pending state opened]}]
     ((:preview a)) ((:preview @pending) example/preview) ((:publish a))
     ((:publish @pending) (assoc example/saved :path "wiki/forged.md"))
     (is (= :inspection-required (:phase (state))))
     (is (empty? @opened)))))

(deftest evidence-preview-is-native-text-not-markdown-or-html
  (fixture
   (fn [{:keys [data]}]
     (let [container (.createElement js/document "div")
           tree (view/panel (assoc data :state {:phase :preview :visible? true :preview example/preview}))
           release (browser/mount! container tree)]
       (try
         (is (nil? (.querySelector container "script")))
         (is (= (:text example/preview) (.-textContent (.querySelector container "pre"))))
         (finally (browser/unmount! release)))))))

(deftest saved-evidence-offers-scoped-workflow-navigation-without-starting-work
  (fixture
   (fn [{:keys [data calls]}]
     (let [container (.createElement js/document "div")
           release (browser/mount! container
                     (view/panel (assoc data :state {:phase :saved :visible? true :saved example/saved})))]
       (try
         (let [link (.querySelector container "a") event (js/MouseEvent. "click" #js {:cancelable true :bubbles true})]
           (is (= (str "/room/" example/id "/workflows/maintain-wiki/run") (.getAttribute link "href")))
           (is (= "Review knowledge update" (.-textContent link)))
           ;; Suppress browser navigation; no preview/publication RPC should be
           ;; attached to this navigation link.
           (.addEventListener link "click" #(.preventDefault %))
           (.dispatchEvent link event)
           (is (empty? @calls)))
         (finally (browser/unmount! release)))))))
