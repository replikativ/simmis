(ns is.simm.uis.schedules-actions-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.render :as render]
            [is.simm.runtimes.web :as web]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.chat-remote :as chat]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.uis.web.desktop.views.schedules :as schedules]
            [is.simm.uis.web.desktop.views.mermaid :as mermaid]
            [org.replikativ.spindel.dom.elements :as el])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(def row {:id "activation" :room-id "room" :room-name "Room" :enabled? true})

(defn deferred [requests]
  (fn [& args]
    (fn [ok error] (swap! requests conj {:ok ok :error error :args args}))))

(defn with-state [f]
  (let [ctx (context/fork-context runtime/runtime)
        session {:id "schedules-ui-fixture"}]
    (try
      (with-redefs [runtime/runtime ctx
                    web/current-session (constantly session)
                    web/current-session? #(= session %)
                    connection/snapshot (constantly {:socket :fixture})
                    sync/ensure! (fn [& _]) sync/retry! (fn [& _])
                    remote/prepare-workflow-metadata-stream!
                    (fn [& _] (fn [ok _] (ok {:watching? true})))]
        (binding [ec/*execution-context* ctx]
          (reset! sig/schedules-data nil)
          (reset! sig/user-rooms nil)
          (reset! sync/state nil)
          (let [ui (schedules/controller)]
            ;; Model the render pass which consumes the changed signal; no
            ;; real network or timers are needed for this controller fixture.
            (f (assoc ui :refresh #(do ((:refresh ui)) ((:observe ui)))
                      :toggle #(do ((:toggle ui) %) ((:observe ui)))) ctx))))
      (finally (context/close-context! ctx)))))

(deftest blocked-deployments-may-be-paused-but-not-resumed
  (doseq [reason [:legacy-recreate :reconciliation-required]]
   (with-state
    (fn [ui _]
      (let [requests (atom []) legacy (assoc row :resume-blocked reason)]
        (with-redefs [remote/set-workflow-schedule-enabled! (deferred requests)]
          ((:toggle ui) (assoc legacy :enabled? false))
          (is (empty? @requests))
          ((:toggle ui) legacy)
          (is (= 1 (count @requests)))
          (is (false? (:enabled? (nth (:args (first @requests)) 2))))))))))

(deftest refresh-cannot-erase-a-pending-toggle-or-restore-its-old-state
  (with-state
    (fn [ui _]
      (let [loads (atom []) toggles (atom [])]
        (with-redefs [chat/load-schedules! (deferred loads)
                      remote/set-workflow-schedule-enabled! (deferred toggles)]
          ((:refresh ui))
          ((:ok (first @loads)) {:workflow-schedules [row]})
          ((:refresh ui))
          ((:toggle ui) row)
          (is (= "activation" (:updating @sig/schedules-data)))
          (binding [ec/*execution-context* nil]
            ((:ok (second @loads)) {:workflow-schedules [(assoc row :enabled? false)]}))
          (is (= "activation" (:updating @sig/schedules-data)))
          ((:refresh ui))
          (is (= 2 (count @loads)))
          (binding [ec/*execution-context* nil]
            ((:ok (first @toggles)) {:id "activation" :enabled? false}))
          ((:observe ui))
          (is (nil? (:updating @sig/schedules-data)))
          (is (false? (get-in @sig/schedules-data [:workflow-schedules 0 :enabled?])))
          (is (= 3 (count @loads))))))))

(deftest replaced-sessions-cannot-publish-or-start-schedule-effects
  (with-state
    (fn [ui _]
      (let [loads (atom []) toggles (atom [])]
        (with-redefs [chat/load-schedules! (deferred loads)
                      remote/set-workflow-schedule-enabled! (deferred toggles)]
          ((:refresh ui))
          (let [before @sig/schedules-data]
            (with-redefs [web/current-session? (constantly false)]
              (binding [ec/*execution-context* nil]
                ((:ok (first @loads)) {:workflow-schedules [row]})
                ((:toggle ui) row)
                ((:refresh ui))))
            (is (= before @sig/schedules-data))
            (is (empty? @toggles))
            (is (= 1 (count @loads)))))))))

(deftest synchronous-rpc-failures-release-only-their-own-operation-slot
  (with-state
    (fn [ui _]
      (with-redefs [chat/load-schedules! (fn [& _] (throw (js/Error. "offline")))
                    remote/set-workflow-schedule-enabled! (fn [& _] (throw (js/Error. "offline")))]
        ((:refresh ui))
        (is (nil? (get-in @sig/schedules-data [:observations :index :active])))
        (is (string? (:error @sig/schedules-data)))
        ((:toggle ui) row)
        (is (nil? (:updating @sig/schedules-data)))
        (is (empty? (:requests @sig/schedules-data)))
        (is (string? (:error @sig/schedules-data)))))))

(deftest normalized-and-legacy-cadences-keep-their-exact-intervals
  (is (= "every 90m" (schedules/cadence-str {:interval-ms 5400000 :tz "UTC"})))
  (is (= "every 60m" (schedules/cadence-str {:every-ms 3600000})))
  (is (= "every week @ 09:00 on monday"
         (schedules/cadence-str {:every :week :at "09:00" :on :monday})))
  (is (= "every month @ 10:30 (day 2)"
         (schedules/cadence-str {:kind :recurring :every :month :at "10:30" :on-day 2})))
  (is (= "once" (schedules/cadence-str {:kind :once}))))

(deftest proposal-result-navigation-only-uses-the-current-owning-session
  (with-state
    (fn [ui _]
      (let [opened (atom []) proposal (str (random-uuid)) before @sig/schedules-data]
        (with-redefs [refs/open! (fn ([reference] (swap! opened conj reference))
                                  ([reference _] (swap! opened conj reference)))]
          ((:open-proposal ui) proposal)
          (is (= [{:kind :proposal :id proposal}] @opened))
          (is (= before @sig/schedules-data) "Navigation neither files output nor changes schedule state")
          (with-redefs [web/current-session? (constantly false)]
            ((:open-proposal ui) proposal))
          (is (= 1 (count @opened))))))))

(deftest scheduled-filing-results-render-without-triggering-an-action
  (async done
    (let [ctx (context/fork-context runtime/runtime) container (.createElement js/document "div")
          opened (atom []) handle (atom nil) proposal (str (random-uuid))
          row {:id "render-fixture" :room-id "room" :room-name "Fixture" :name "maintain-wiki"
               :kind :workflow :enabled? false :cadence {:interval-ms 60000 :tz "UTC"}
               :resume-blocked :reconciliation-required :reason :interrupted
               :revision "fixture-revision" :bundle "fixture-bundle" :next-fire 0
               :on-toggle (fn [& _] (throw (js/Error. "Render changed activation")))
               :on-open-run (fn [& _] (throw (js/Error. "Render opened Run")))
               :on-open-proposal #(swap! opened conj %)
               :occurrences [{:id "proposed" :intended-at 0 :state :linked :coalesced 0
                              :job "fixture-job" :filing {:status :proposed :proposal proposal}}
                             {:id "no-op" :intended-at 1 :state :linked :coalesced 0 :filing {:status :no-op}}]}
          observer (js/MutationObserver.
                    (fn [_ observer]
                      (when-let [button (some #(when (= "Review proposal" (.-textContent %)) %)
                                             (array-seq (.querySelectorAll container "button")))]
                        (when (.includes (.-textContent container) "No wiki changes")
                          (.disconnect observer)
                          (try
                            (is (empty? @opened) "Rendering never collects, opens or adopts anything")
                            (is (.includes (.-textContent container) "Results"))
                            (is (.includes (.-textContent container) "adding allowance does not resolve it"))
                            (let [resume (some #(when (= "Resume" (.-textContent %)) %)
                                               (array-seq (.querySelectorAll container "button")))]
                              (is (some? resume))
                              (is (true? (.-disabled resume))))
                            (.click button)
                            (is (= [proposal] @opened))
                            (finally
                              (when-let [stop (:stop! @handle)] (stop))
                              (.remove container)
                              (context/close-context! ctx)
                              (done)))))))]
      ;; Wait on the actual DOM mutation, not an arbitrary render delay.
      (set! (.. container -style -display) "none")
      (.appendChild (.-body js/document) container)
      (.observe observer container #js {:childList true :subtree true})
      (binding [ec/*execution-context* ctx]
        (reset! handle (render/render-spin! container (spin (schedules/workflow-row row))
                                           (browser/make-dom-discharge js/document)))))))

(deftest workflows-overview-keeps-manual-room-discovery-and-agent-authored-diagrams
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        diagram-calls (atom []) opened (atom []) discharge (atom nil)
        room {:room/id "manual-room" :room/name "Manual only"}
        data {:index-status :ready :workflow-schedules []
              :schedules [{:id "code-task" :room-name "Research" :task "Maintain evidence"
                           :agent-name "Mira" :code "(maintain!)" :next-fire 0}]
              :diagrams {"Research" "flowchart LR; A-->B"}
              :shapes {"code-task" "flowchart LR; Source-->Review"}}]
    (.appendChild (.-body js/document) container)
    (try
      (with-redefs [mermaid/diagram
                    (fn [code] (swap! diagram-calls conj code)
                      (el/div {} "Diagram"))]
        (binding [ec/*execution-context* ctx]
          (reset! discharge (browser/mount! container
                             (schedules/overview data [room]
                               {:open-room (fn [room _] (swap! opened conj room))
                                :toggle (fn [& _] (throw (js/Error. "Opening overview changed a schedule")))} nil)))
          (is (= "Workflows" (.-textContent (.querySelector container "h2"))))
          (is (.includes (.-textContent container) "Mira"))
          (is (.includes (.-textContent container) "Invoked code"))
          (is (= #{"flowchart LR; A-->B" "flowchart LR; Source-->Review"} (set @diagram-calls)))
          (is (empty? @opened))
          (.click (some #(when (= "Manual only" (.-textContent %)) %)
                        (array-seq (.querySelectorAll container "button"))))
          (is (= [room] @opened))))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))

(deftest global-workflows-room-link-uses-the-existing-room-workflows-tab
  (with-state
    (fn [ui _]
      (let [opened (atom [])]
        (with-redefs [sig/open-tab! (fn [type data & options] (swap! opened conj [type data options]))]
          ((:open-room ui) {:room/id "room" :room/name "Research"} nil)
          (is (= :workflows (ffirst @opened)))
          (is (= {:room-id "room" :room-name "Research"} (second (first @opened)))))))))

(deftest invalidated-workflow-index-does-not-present-obsolete-cards-or-diagrams-as-current
  (doseq [cause [:roster :connection]]
    (with-state
      (fn [ui ctx]
        (let [loads (atom []) streams (atom []) replaced? (atom false)
              container (.createElement js/document "div") discharge (atom nil)]
          (.appendChild (.-body js/document) container)
          (try
            (with-redefs [connection/snapshot #(if @replaced? {:socket :replacement} {:socket :fixture})
                          chat/load-schedules! (deferred loads)
                          remote/prepare-workflow-metadata-stream! (deferred streams)
                          mermaid/diagram (fn [_] (throw (js/Error. "Stale diagram was rendered")))]
              ((:refresh ui))
              ((:ok (first @loads)) {:workflow-schedules [(assoc row :name "Obsolete workflow")]
                                    :diagrams {"Removed room" "flowchart LR; Secret-->Output"}})
              (is (= :ready (:index-status @sig/schedules-data)))
              (if (= :connection cause) (reset! replaced? true)
                  (reset! sig/user-rooms {:rooms []}))
              ((:observe ui))
              (is (= :checking (:index-status @sig/schedules-data)))
              (reset! discharge (browser/mount! container (schedules/overview @sig/schedules-data [] ui nil)))
              (is (not (.includes (.-textContent container) "Obsolete workflow")))
              (is (.includes (.-textContent container) "Checking scheduled work"))
              (is (not (.includes (.-textContent container) "No scheduled work.")))
              (is (= 2 (count @loads)))
              ((:ok (last @loads)) {:workflow-schedules [] :schedules [] :diagrams {} :shapes {}})
              (is (= :ready (:index-status @sig/schedules-data))))
            (finally (when @discharge (browser/unmount! @discharge)) (.remove container))))))))
