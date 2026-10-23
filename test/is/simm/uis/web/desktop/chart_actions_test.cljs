(ns is.simm.uis.web.desktop.chart-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.uis.web.desktop.chart-remote :as chart-remote]
            [is.simm.uis.web.desktop.memory-actions :as actions]
            [is.simm.uis.web.desktop.memory-charts-test :as fixture]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.runtimes.web :as web]
            [org.replikativ.spindel.engine.core :as ec])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn- with-controller [f]
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)}
          states (signal runtime {}) chart-calls (atom []) analysis-calls (atom []) metadata-calls (atom [])]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    sync/ensure! (fn [& _])
                    remote/load-memory-metadata! (fn [_ _ request]
                                                   (fn [ok fail] (swap! metadata-calls conj {:request request :ok ok :fail fail})))
                    remote/analyze-memory-text! (fn [_ _ request]
                                                  (fn [ok fail] (swap! analysis-calls conj {:request request :ok ok :fail fail})))
                    chart-remote/load-memory-chart! (fn [_ _ request]
                                                      (fn [ok fail] (swap! chart-calls conj {:request request :ok ok :fail fail})))]
        (let [a (actions/controller {:room-id "chart-room" :col-id :chart-test :state-signal states}) key (:owner a)
              state (-> fixture/state
                        (assoc :connection connection :verified-connection connection :watch-connection connection
                               :target-epoch 0 :selected "wiki/a.md")
                        (assoc-in [:drafts "wiki/a.md" :text] "saved")
                        (assoc-in [:drafts "wiki/a.md" :source :text] "saved"))]
          (swap! states assoc key state)
          (f {:a a :state #(get @states key) :change #(swap! states update key %)
              :charts chart-calls :analyses analysis-calls :metadata metadata-calls :connection connection}))))))

(deftest late-plot-reply-cannot-replace-current-typing
  (with-controller
    (fn [{:keys [a state change charts]}]
      ((:ensure-charts a)) ((:ensure-charts a))
      (is (= 1 (count @charts)))
      (change #(assoc-in % [:drafts "wiki/a.md" :text] "typing"))
      ((:ok (first @charts)) fixture/reply)
      (is (empty? (:charts (state))))
      (is (= "typing" (get-in (state) [:drafts "wiki/a.md" :text])))
      (is (not (get-in (state) [:loading [:chart "wiki/a.md" fixture/block]]))))))

(deftest metadata-advancing-during-a-chart-read-retires-its-loading-slot
  (with-controller
    (fn [{:keys [a state charts metadata connection]}]
      ((:ensure-charts a))
      (let [old (:ok (first @charts)) slot [:chart "wiki/a.md" fixture/block]]
        (is (true? (get-in (state) [:loading slot])))
        ((:ensure-sync a) connection {:connection connection :status :ready :dirty {}})
        ((:ok (first @metadata)) {:target-stamp fixture/stamp :protocol-version 2 :folders {}
                                 :entries {"wiki/a.md" fixture/entry} :definition-basis fixture/revision
                                 :revision :new-epoch})
        (is (false? (get-in (state) [:loading slot])))
        ((:ensure-charts a)) ((:ensure-charts a))
        (is (= 2 (count @charts)))
        (old fixture/reply)
        (is (true? (get-in (state) [:loading slot])) "late old reply cannot finish the replacement request")
        (is (empty? ((:chart-models a) "wiki/a.md")))
        ((:ok (second @charts)) (assoc-in fixture/reply [:chart :series 0 :points] [[0 9]]))
        (is (false? (get-in (state) [:loading slot])))
        (is (= [[0 9]] (get-in ((:chart-models a) "wiki/a.md") [(str fixture/block) :series 0 :points])))))))

(deftest optional-analysis-is-single-flight-and-preserves-concurrent-typing
  (with-controller
    (fn [{:keys [a state change charts analyses]}]
      (let [next-id (random-uuid)]
        (change #(assoc-in % [:drafts "wiki/a.md" :source :entry :content] next-id))
        ((:ensure-charts a)) ((:ensure-charts a))
        (is (= 1 (count @analyses))) (is (empty? @charts))
        ((:fail (first @analyses)) (js/Error. "Unavailable"))
        ((:ensure-charts a)) ((:ensure-charts a))
        (is (= 1 (count @analyses)) "failed analysis cannot trigger its own render retry")
        (change #(update-in % [:drafts "wiki/a.md" :generation] inc))
        ((:ensure-charts a))
        (is (= 2 (count @analyses)))
        (change #(assoc-in % [:drafts "wiki/a.md" :text] "continued typing"))
        ((:ok (second @analyses))
         (assoc (get-in fixture/state [:drafts "wiki/a.md" :source :analysis]) :revision next-id
                :target-stamp fixture/stamp))
        (is (= "continued typing" (get-in (state) [:drafts "wiki/a.md" :text])))
        (is (= next-id (get-in (state) [:drafts "wiki/a.md" :source :analysis :revision])))
        ((:ensure-charts a))
        (is (empty? @charts) "baseline-only analysis cannot dispatch a dirty-draft plot request")))))

(deftest spec-only-room-notification-invalidates-chart-without-opening-the-json-buffer
  (with-controller
    (fn [{:keys [a state charts metadata connection]}]
      (let [stream {:connection connection :status :ready :dirty {}}
            metadata-reply {:target-stamp fixture/stamp :protocol-version 2 :folders {}
                            :entries {"wiki/a.md" fixture/entry} :definition-basis fixture/revision}]
        ((:ensure-sync a) connection stream)
        ((:ok (first @metadata)) (assoc metadata-reply :revision :initial))
        ((:ensure-charts a))
        ((:ok (first @charts)) fixture/reply)
        (is (= 1 (count ((:chart-models a) "wiki/a.md"))))
        ((:ensure-sync a) connection (assoc-in stream [:dirty "chart-room"] 1))
        ((:ok (second @metadata)) (assoc metadata-reply :revision :spec-only-commit))
        (is (empty? ((:chart-models a) "wiki/a.md")))
        ((:ensure-charts a))
        (is (= 2 (count @charts)))
        ((:ok (second @charts)) (assoc-in fixture/reply [:chart :series 0 :points] [[0 7]]))
        (is (= [[0 7]] (get-in ((:chart-models a) "wiki/a.md") [(str fixture/block) :series 0 :points])))
        (is (= #{"wiki/a.md"} (set (keys (:drafts (state))))) "dependency is not an opened editor buffer")))))
