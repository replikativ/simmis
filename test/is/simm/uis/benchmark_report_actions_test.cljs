(ns is.simm.uis.benchmark-report-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.benchmark-report-actions :as actions]
            [is.simm.uis.web.desktop.benchmark-report-remote :as remote]
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.uis.benchmark-report-state-test :as fixture]
            [is.simm.runtimes.web :as web]))

(defn- with-state [f]
  (let [ctx (context/fork-context runtime/runtime) session {:id (random-uuid)} epoch (atom :first)
        data {:room-id (str fixture/room) :room-name "Room" :tab-id "results"}
        calls (atom {:prepare [] :publish [] :open []})]
    (try
      (with-redefs [web/current-session (constantly session) web/current-session? #(= % session)
                    connection/snapshot #(deref epoch) connection/current? #(= % @epoch)
                    remote/prepare-report! (fn [_ room request]
                                             (let [construction ec/*execution-context*]
                                               (fn [ok error] (swap! calls update :prepare conj {:room room :request request :ok ok :error error
                                                                                                 :construction construction :context ec/*execution-context* :spin ec/*spin-id*}))))
                    remote/publish-report! (fn [_ room request]
                                             (let [construction ec/*execution-context*]
                                               (fn [ok error] (swap! calls update :publish conj {:room room :request request :ok ok :error error
                                                                                                 :construction construction :context ec/*execution-context* :spin ec/*spin-id*}))))
                    navigation/open-file! (fn [room title path & [column stamp]]
                                            (swap! calls update :open conj [room title path column stamp]))]
        (binding [ec/*execution-context* ctx]
          (reset! sig/benchmark-report-publications {})
          (reset! sig/layout-columns [{:id "left" :tabs [{:id "results" :type :leaderboard :data data}]}])
          (f {:ctx ctx :data data :session session :epoch epoch :calls calls
              :state #(get @sig/benchmark-report-publications (actions/owner-key session data))})))
      (finally (context/close-context! ctx)))))

(deftest one-explicit-request-freezes-publication-and-recovers-after-reconnect
  (with-state
    (fn [{:keys [data calls state epoch]}]
      (let [id (str fixture/scorecard) ui (actions/controller data)]
        ((:publish ui) id)
        ((:publish ui) id)
        (is (= 1 (count (:prepare @calls))))
        (is (empty? (:publish @calls)) "Preparation alone writes nothing")
        ((:ok (first (:prepare @calls))) fixture/prepared)
        (let [original (:request (first (:publish @calls)))]
          (is (= (get-in fixture/prepared [:source :scorecard]) (:scorecard original)))
          (is (= (:plan fixture/prepared) (:expected-plan original)))
          (is (= (:target-stamp fixture/prepared) (:expected-target-stamp original)))
          (is (not (contains? original :source)) "Client correspondence is not a transport capability")
          (reset! epoch :second)
          (let [current (actions/controller data)]
            (is (false? (get-in (state) [id :loading?])))
            ((:publish current) id)
            (is (= 1 (count (:prepare @calls))) "Recovery never selects a new target/source")
            (is (= original (:request (second (:publish @calls)))))
            ((:ok (first (:publish @calls))) fixture/receipt)
            (is (nil? (get-in (state) [id :result])) "Old connection cannot acknowledge new request")
            ((:ok (second (:publish @calls))) fixture/receipt)
            (is (= fixture/receipt (get-in (state) [id :result])))
            (is (nil? (get-in (state) [id :pending])))
            (swap! sig/layout-columns #(mapv (fn [column] (assoc column :id "right")) %))
            ((:open current) id)
            (is (= "right" (nth (first (:open @calls)) 3)) "Open follows the original tab after moving columns")))))))

(deftest closing-or-repurposing-the-tab-stops-a-late-prepare-from-writing
  (with-state
    (fn [{:keys [data calls]}]
      (let [id (str fixture/scorecard) ui (actions/controller data)]
        ((:publish ui) id)
        (swap! sig/layout-columns assoc-in [0 :tabs 0 :data :room-id] "other-room")
        ((:ok (first (:prepare @calls))) fixture/prepared)
        (is (empty? (:publish @calls)))
        (reset! sig/layout-columns [])
        ((:publish ui) id)
        (is (= 1 (count (:prepare @calls))))))))

(deftest retained-handler-and-delayed-prepare-dispatch-in-their-original-context
  (with-state
    (fn [{:keys [ctx data calls]}]
      (let [ui (actions/controller data) id (str fixture/scorecard)]
        (binding [ec/*execution-context* runtime/runtime ec/*spin-id* :foreign-spin]
          ((:publish ui) id)
          ((:ok (first (:prepare @calls))) fixture/prepared))
        (is (= 1 (count (:publish @calls))))
        (doseq [call [(first (:prepare @calls)) (first (:publish @calls))]]
          (is (identical? ctx (:construction call)))
          (is (identical? ctx (:context call)))
          (is (nil? (:spin call))))))))
