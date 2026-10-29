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
            [is.simm.uis.web.desktop.views.add-source :as add-source]
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
                    sig/active-column-id (signal runtime :column)
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

(deftest github-creation-opens-the-root-without-inventing-a-file
  (with-client :memory
    (fn [{:keys [data last-call result]}]
      (let [a (actions/controller (assoc data :source-kind :github))]
        ((:set-name a) "GitHub source")
        ((:set-repository a) "octocat/hello-world")
        ((:create a))
        ((:ok (last-call :create)) result)
        (reset! sig/user-rooms {:rooms [(:room result)]})
        ((:open a))
        (let [tab (first (:tabs (first @sig/layout-columns)))]
          (is (= :memory (:type tab)))
          (is (= (get-in result [:room :room/id]) (get-in tab [:data :room-id])))
          (is (nil? (get-in tab [:data :path]))))))))

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
          (is (not (.includes (.-textContent container) "Check creation")))
          ((:ok (last-call :create)) result)
          (simple/drain-events! runtime nil)
          (is (.includes (.-textContent container) "Your workspace is ready."))
          (is (.includes (.-textContent container) "Adding workspace to navigation"))
          (reset! sig/user-rooms {:rooms [(:room result)]})
          (simple/drain-events! runtime nil)
          (is (= "Open workspace" (.-textContent (.querySelector container "button.settings-btn--primary"))))
          (is (= 1 (count (:tabs (first @sig/layout-columns)))))
          (is (= :memory (:type (first (:tabs (first @sig/layout-columns))))))
          (is (= (get-in result [:room :room/id])
                 (get-in (first (:tabs (first @sig/layout-columns))) [:data :room-id])))
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

(deftest automatic-navigation-waits-for-receipt-and-roster-and-happens-once
  (doseq [landing [:chat :memory] order [:receipt-first :roster-first]]
    (with-client landing
      (fn [{:keys [a data last-call result calls]}]
        ((:set-name a) "Research") ((:create a))
        (let [publish #(reset! sig/user-rooms {:rooms [(assoc (:room result) :room/content-db-scope (random-uuid))]})
              confirm #((:ok (last-call :create)) result)]
          ((if (= order :receipt-first) confirm publish))
          ((:ensure a))
          (is (= :form (:id (first (:tabs (first @sig/layout-columns))))))
          ((if (= order :receipt-first) publish confirm))
          ((:ensure a)) ((:ensure a))
          (let [tabs (:tabs (first @sig/layout-columns))]
            (is (= 1 (count tabs)))
            (is (= (if (= landing :memory) :memory :chat) (:type (first tabs))))
            (is (= (get-in result [:room :room/id]) (get-in (first tabs) [:data :room-id]))))
          (is (= 1 (count (:create @calls))))
          (is (empty? (:observe @calls))))))))

(deftest completion-does-not-steal-another-column-or-tab
  (doseq [elsewhere [:column :tab]]
    (with-client :memory
      (fn [{:keys [a data last-call result]}]
        ((:set-name a) "Research") ((:create a))
        (let [other {:id :other :type :settings :data {}}]
          (if (= elsewhere :column)
            (do (swap! sig/layout-columns conj {:id :other-column :active-tab :other :tabs [other]})
                (reset! sig/active-column-id :other-column))
            (swap! sig/layout-columns update 0 #(-> % (update :tabs conj other) (assoc :active-tab :other))))
          ((:ok (last-call :create)) result)
          (reset! sig/user-rooms {:rooms [(:room result)]})
          (let [before @sig/layout-columns]
            ((:ensure a))
            (is (= before @sig/layout-columns)))
          (reset! sig/active-column-id :column)
          (swap! sig/layout-columns assoc-in [0 :active-tab] :form)
          ((:ensure a))
          (let [column (first @sig/layout-columns)
                active (some #(when (= (:active-tab column) (:id %)) %) (:tabs column))]
            (is (= :memory (:type active)))))))))

(deftest reconnect-keeps-historical-success-without-automatic-navigation
  (with-client :memory
    (fn [{:keys [a state last-call result calls]}]
      ((:set-name a) "Research") ((:create a))
      (let [old-reply (:ok (last-call :create))]
        (reset! sig/memory-connection {:epoch (random-uuid)})
        ((:ensure a))
        (old-reply result)
        (reset! sig/user-rooms {:rooms [(:room result)]})
        ((:ensure a)) ((:ensure a))
        (is (= :created (:phase (state))))
        (is (= :form (:id (first (:tabs (first @sig/layout-columns))))))
        ((:open a))
        (is (= :memory (:type (first (:tabs (first @sig/layout-columns))))))
        (is (= 1 (count (:create @calls))))))))

(deftest detached-or-replaced-form-cannot-redirect-on-completion
  (doseq [replacement [{:id :form :type :settings :data {}} {:id :new-form :type :new-kb :data {}}]]
    (with-client :memory
      (fn [{:keys [a last-call result]}]
        ((:set-name a) "Research") ((:create a))
        (reset! sig/layout-columns [{:id :column :active-tab (:id replacement) :tabs [replacement]}])
        ((:ok (last-call :create)) result)
        (reset! sig/user-rooms {:rooms [(:room result)]})
        ((:ensure a)) ((:open a))
        (is (= [replacement] (:tabs (first @sig/layout-columns))))))))

(deftest chooser-reuses-its-original-tab-and-does-not-replace-another-column
  (doseq [[kind type target] [[:memory :add-memory :new-kb] [:team :add-team :new-room]]]
    (with-client :memory
      (fn [{:keys [data]}]
        (let [other {:id :other-column :active-tab :other :tabs [{:id :other :type :settings :data {}}]}
              chooser {:id :form :type type :data {}}
              owner {:col-id :column :tab-id :form :tab-type type
                     :ctx runtime :session (web/current-session)}]
          (reset! sig/layout-columns [{:id :column :active-tab :form :tabs [chooser]} other])
          (reset! sig/active-column-id :other-column)
          (@#'add-source/act! {:id (if (= kind :memory) :workspace :team)} owner)
          (is (= 1 (count (:tabs (first @sig/layout-columns)))))
          (is (= :form (:id (first (:tabs (first @sig/layout-columns))))))
          (is (= target (:type (first (:tabs (first @sig/layout-columns))))))
          (is (= other (second @sig/layout-columns)))
          (let [before @sig/layout-columns]
            (@#'add-source/act! {:id :workspace} owner)
            (is (= before @sig/layout-columns) "The detached chooser no longer owns its tab")))))))

(deftest copied-chooser-keeps-its-rendering-context-without-ambient-binding
  (with-client :memory
    (fn [_]
      (let [columns [{:id :column :active-tab :form
                      :tabs [{:id :form :type :add-memory :data {}}]}]]
        (reset! sig/layout-columns columns)
        (let [child (context/fork-context runtime)
              owner {:col-id :column :tab-id :form :tab-type :add-memory
                     :ctx child :session (web/current-session)}]
          (try
            (binding [ec/*execution-context* nil]
              (@#'add-source/act! {:id :workspace} owner))
            (is (= columns @sig/layout-columns))
            (binding [ec/*execution-context* child]
              (is (= :new-kb (get-in @sig/layout-columns [0 :tabs 0 :type]))))
            (finally (context/close-context! child))))))))
