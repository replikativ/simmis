(ns is.simm.uis.observation-io-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.runtimes.web :as web]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.observation-io :as io]))

(defn with-observer [f]
  (let [ctx (context/fork-context runtime/runtime)
        session {:id "observation-fixture"}
        socket (atom :first)]
    (try
      (with-redefs [web/current-session? #(= session %)
                    connection/snapshot #(deref socket)]
        (binding [ec/*execution-context* ctx]
          (reset! sig/schedules-data {:generation 0 :ledger {:request-id :original}})
          (let [read-state #(binding [ec/*execution-context* ctx] @sig/schedules-data)
                update! (fn [g & args] (binding [ec/*execution-context* ctx] (apply swap! sig/schedules-data g args)))
                observer (io/controller ctx session read-state update!)]
            (f {:ctx ctx :session session :socket socket :state read-state :update! update!
                :observer observer :generation #(:generation (read-state))}))))
      (finally (context/close-context! ctx)))))

(defn invoke [requests]
  (fn [] (fn [ok error] (swap! requests conj {:ok ok :error error}))))
(defn consume [state reply] (assoc state :value reply))
(defn fail [state error] (assoc state :error error))

(deftest actual-callbacks-coalesce-and-recheck-dependencies-before-render
  (with-observer
    (fn [{:keys [observer generation update! state]}]
      (let [requests (atom []) observe #(observer :index generation (invoke requests) consume fail)]
        (observe)
        ;; No render/offer between the dependency write and old reply.
        (update! assoc :generation 1)
        (binding [ec/*execution-context* nil] ((:ok (first @requests)) :obsolete))
        (is (nil? (:value (state))))
        (observe)
        (update! assoc :generation 2)
        (observe)
        (is (= 2 (count @requests)))
        ((:ok (second @requests)) :also-obsolete)
        (observe)
        (is (= 3 (count @requests)))
        (binding [ec/*execution-context* nil] ((:ok (nth @requests 2)) :latest))
        (is (= :latest (:value (state))))
        (is (= {:request-id :original} (:ledger (state))))
        (observe)
        (is (= 3 (count @requests)))))))

(deftest replacement-socket-supersedes-hung-native-callback
  (with-observer
    (fn [{:keys [observer generation socket state]}]
      (let [requests (atom []) observe #(observer :index generation (invoke requests) consume fail)]
        (observe)
        (reset! socket :replacement)
        (observe)
        ((:ok (first @requests)) :old)
        (is (nil? (:value (state))))
        ((:ok (second @requests)) :new)
        (is (= :new (:value (state))))))))

(deftest real-copied-context-replaces-inherited-fetch-with-child-owned-frame
  (with-observer
    (fn [{:keys [ctx session observer generation state]}]
      (let [parent-requests (atom []) child-requests (atom [])]
        (observer :index generation (invoke parent-requests) consume fail)
        (let [parent-before (state)
              child (context/fork-context ctx)]
          (try
            (binding [ec/*execution-context* child]
              (let [read-child #(binding [ec/*execution-context* child] @sig/schedules-data)
                    update-child (fn [f & args] (binding [ec/*execution-context* child] (apply swap! sig/schedules-data f args)))
                    child-observer (io/controller child session read-child update-child)]
                (child-observer :index #(:generation (read-child)) (invoke child-requests) consume fail)
                (is (= parent-before (state)))
                (is (not= (get-in parent-before [:observations :index :active :epoch])
                          (get-in (read-child) [:observations :index :active :epoch])))
                (binding [ec/*execution-context* nil] ((:ok (first @child-requests)) :child))
                (is (= :child (:value (read-child))))
                (is (= parent-before (state)))
                ((:ok (first @parent-requests)) :parent)
                (is (= :parent (:value (state))))
                (is (= :child (:value (read-child))))))
            (finally (context/close-context! child))))))))

(deftest failed-observation-needs-an-invalidation-or-explicit-retry
  (with-observer
    (fn [{:keys [observer generation update! state]}]
      (let [requests (atom []) after (atom 0)
            observe #(observer :index generation (invoke requests) consume fail (fn [_] (swap! after inc)))]
        (observe)
        ((:error (first @requests)) (js/Error. "offline"))
        (observe)
        (is (= 1 (count @requests)))
        (is (= 0 @after))
        (is (string? (:error (state))))
        (update! update :generation inc)
        (observe)
        ((:ok (second @requests)) :recovered)
        ((:ok (second @requests)) :duplicate)
        (is (= :recovered (:value (state))))
        (is (= 1 @after))))))

(deftest stale-acknowledgements-do-not-complete-an-unpublished-generation
  (doseq [render-newer? [true false]]
    (with-observer
      (fn [{:keys [observer generation update! state]}]
        (let [requests (atom []) observe #(observer :index generation (invoke requests) consume fail)]
          (observe)
          (update! assoc :generation 1)
          (when render-newer? (observe))
          ;; Exercise both wanted=B/current=A and unrendered current=B.
          (when render-newer? (update! assoc :generation 0))
          ((:ok (first @requests)) :not-published)
          (is (nil? (:value (state))))
          (update! assoc :generation 0)
          (observe)
          (is (= 2 (count @requests)))
          ((:ok (second @requests)) :fresh-a)
          (is (= :fresh-a (:value (state)))))))))
