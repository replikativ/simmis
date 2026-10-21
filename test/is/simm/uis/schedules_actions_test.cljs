(ns is.simm.uis.schedules-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.runtimes.web :as web]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.chat-remote :as chat]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [is.simm.uis.web.desktop.views.schedules :as schedules]))

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
                    web/current-session? #(= session %)]
        (binding [ec/*execution-context* ctx]
          (reset! sig/schedules-data nil)
          (f (schedules/controller) ctx)))
      (finally (context/close-context! ctx)))))

(deftest older-deployments-may-be-paused-but-not-resumed
  (with-state
    (fn [ui _]
      (let [requests (atom []) legacy (assoc row :resume-blocked :legacy-recreate)]
        (with-redefs [remote/set-workflow-schedule-enabled! (deferred requests)]
          ((:toggle ui) (assoc legacy :enabled? false))
          (is (empty? @requests))
          ((:toggle ui) legacy)
          (is (= 1 (count @requests)))
          (is (false? (:enabled? (nth (:args (first @requests)) 2)))))))))

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
        (is (false? (:loading? @sig/schedules-data)))
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
