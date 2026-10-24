(ns is.simm.uis.web.desktop.workspace-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.render :as render]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.workspace-actions :as actions]
            [is.simm.uis.web.desktop.workspace-remote :as remote]
            [is.simm.uis.web.desktop.chat-remote :as chat]
            [is.simm.uis.web.desktop.user-rooms-sync :as roster]
            [is.simm.uis.web.desktop.views.workspace-creation :as view]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn- with-client [landing f]
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} party (str (random-uuid))
          type (if (= :memory landing) :new-kb :new-room)
          data {:tab-id :form :col-id :column :tab-type type :landing landing}
          calls (atom {})
          deferred (fn [kind]
                     (fn [_ request]
                       (fn [ok error]
                         (swap! calls update kind (fnil conj []) {:request request :ok ok :error error}))))
          result {:status :created :room {:room/id (str (random-uuid)) :room/name "Research" :room/type :project}}]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/current-user (signal runtime {:id party})
                    sig/workspace-creation-state (signal runtime {})
                    sig/memory-connection (signal runtime {:epoch (random-uuid)})
                    sig/user-rooms (signal runtime {:rooms []})
                    sig/layout-columns (signal runtime [{:id :column :active-tab :form
                                                         :tabs [{:id :form :type type :data {}}]}])
                    connection/snapshot #(binding [ec/*execution-context* runtime] @sig/memory-connection)
                    remote/create-workspace! (deferred :create)
                    remote/observe-workspace-creation! (deferred :observe)
                    chat/list-parties! (fn [& _] (fn [ok _] (ok [])))
                    roster/refresh-user-rooms! (fn ([party] (swap! calls update :roster (fnil conj []) party))
                                                ([party _] (swap! calls update :roster (fnil conj []) party)))]
        (let [a (actions/controller data)]
          (f {:a a :state (:state a) :data data :party party :calls calls :result result
              :last-call #(last (get @calls %))}))))))

(deftest one-submission-survives-reconnect-and-is-only-observed-afterward
  (with-client :chat
    (fn [{:keys [a state calls last-call result party]}]
      ((:set-name a) "Research")
      ((:create a))
      (let [request (get-in (last-call :create) [:request]) callback (:ok (last-call :create))]
        ((:create a)) ((:ensure a))
        (is (= 1 (count (:create @calls))))
        ((:set-name a) "Changed")
        (is (= request (:submission (state))))
        (reset! sig/memory-connection {:epoch (random-uuid)})
        ((:ensure a)) ((:ensure a))
        (is (= :unconfirmed (:phase (state))))
        (is (= 1 (count (:create @calls))))
        ((:check a)) ((:check a))
        (is (= 1 (count (:observe @calls))))
        (is (= request (:request (last-call :observe))))
        ((:ok (last-call :observe)) {:status :preparing})
        (callback result)
        (is (= :created (:phase (state))))
        (is (= [party] (:roster @calls)))
        (callback result)
        (is (= [party] (:roster @calls)))))))

(deftest copied-forms-can-edit-semantics-but-cannot-create-through-live-host
  (with-client :memory
    (fn [{:keys [a data state calls]}]
      ((:set-name a) "Parent draft")
      (let [child (context/fork-context runtime)]
        (try
          (let [forked (binding [ec/*execution-context* child] (actions/controller data))]
            ((:set-name forked) "Simulated research")
            ((:create forked))
            (is (= "Simulated research" (:name ((:state forked)))))
            (is (string? (:error ((:state forked)))))
            (is (nil? (:submission ((:state forked)))))
            (is (= "Parent draft" (:name (state))))
            (is (empty? (:create @calls))))
          (finally (context/close-context! child)))))))

(deftest late-created-receipt-cannot-close-or-navigate-another-form
  (with-client :memory
    (fn [{:keys [a state data calls last-call result]}]
      ((:set-name a) "Research") ((:create a))
      (let [callback (:ok (last-call :create))
            replacement {:id :new-form :type :new-kb :data {}}]
        (reset! sig/layout-columns [{:id :column :tabs [replacement]}])
        (callback result)
        (is (= :created (:phase (state))))
        ((:open a))
        (is (= [replacement] (:tabs (first @sig/layout-columns))))
        (is (nil? ((:state (actions/controller (assoc data :tab-id :new-form))))))))))

(deftest opening-a-created-workspace-follows-its-form-across-columns
  (doseq [landing [:chat :memory]]
    (with-client landing
      (fn [{:keys [a data last-call result]}]
        ((:set-name a) "Research") ((:create a))
        ((:ok (last-call :create)) result)
        (reset! sig/user-rooms {:rooms [(assoc (:room result) :room/content-db-scope (random-uuid))]})
        (reset! sig/layout-columns [{:id :moved :tabs [{:id :form :type (:tab-type data) :data {}}]}])
        ((:open a))
        (let [tabs (:tabs (first @sig/layout-columns)) tab (first tabs)]
          (is (= 1 (count tabs)))
          (is (= (if (= landing :memory) :memory :chat) (:type tab)))
          (is (= (get-in result [:room :room/id]) (get-in tab [:data :room-id])))
          (when (= landing :memory) (is (= "wiki/Welcome.md" (get-in tab [:data :path])))))))))

(deftest mounted-form-freezes-the-request-and-shows-observed-result
  (with-client :memory
    (fn [{:keys [data calls last-call result]}]
      (let [container (.createElement js/document "div")
            handle (render/render-spin! container (view/render data) (browser/make-dom-discharge js/document))]
        (try
          (simple/drain-events! runtime nil)
          (let [input (.querySelector container "input[aria-label='Workspace name']")]
            (set! (.-value input) "Research")
            (.dispatchEvent input (js/Event. "input" #js {:bubbles true})))
          (simple/drain-events! runtime nil)
          (.click (.querySelector container "button.settings-btn--primary"))
          (simple/drain-events! runtime nil)
          (is (= 1 (count (:create @calls))))
          (is (.-disabled (.querySelector container "input[aria-label='Workspace name']")))
          (is (.includes (.-textContent container) "Check creation"))
          ((:ok (last-call :create)) result)
          (simple/drain-events! runtime nil)
          (is (.includes (.-textContent container) "Your workspace is ready."))
          (is (.includes (.-textContent container) "Adding workspace to navigation"))
          (reset! sig/user-rooms {:rooms [(:room result)]})
          (simple/drain-events! runtime nil)
          (is (= "Open workspace" (.-textContent (.querySelector container "button.settings-btn--primary"))))
          (is (not (.-disabled (.querySelector container "button.settings-btn--primary"))))
          (finally ((:stop! handle))))))))

(deftest reconnect-retires-a-lost-observation-without-resending-creation
  (with-client :memory
    (fn [{:keys [a state calls last-call result]}]
      ((:set-name a) "Research") ((:create a))
      ((:check a))
      (let [old (:ok (last-call :observe)) request (:submission (state))]
        (reset! sig/memory-connection {:epoch (random-uuid)})
        (let [before (state)]
          (old result)
          (is (= before (state)) "Socket change fences a read before rendering resumes"))
        ((:ensure a))
        (is (false? (get-in (state) [:loading :observe])))
        (is (= request (:submission (state))))
        ((:check a))
        (is (= 2 (count (:observe @calls))))
        (is (= 1 (count (:create @calls))))
        ((:ok (last-call :observe)) {:status :preparing})
        (let [current (state)]
          (old result)
          (is (= current (state))))
        (is (= :unconfirmed (:phase (state))))))))
