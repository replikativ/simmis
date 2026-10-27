(ns is.simm.ops.benchmark-checkpoint-adapter-test
  "Admission-boundary tests. Real App tickets, Request rows and Run cancellation;
   private allocation/source descriptors are controlled separately here. These
   do not claim two-case execution or native isolation parity."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.run :as run]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-checkpoint-adapter :as checkpoint]
            [is.simm.ops.benchmark-fixture :as fixture]
            [is.simm.ops.benchmark-fixture-snapshot :as snapshot]
            [is.simm.ops.benchmark-resolution :as resolution]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.work-test :as work-test]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- with-admission [f]
  (world/with-world [conn w]
    (d/transact conn requests/schema)
    (d/transact conn [{:party/id (:owner w) :party/role :admin}])
    (@#'work-test/with-room
     (fn [room]
       (let [nonce (random-uuid)
             claimed (:request (requests/claim! conn {:party (:owner w) :room (:room w) :client-id nonce
                                                      :kind :benchmark :intent {:test :checkpoint-admission}
                                                      :limits {:timeout-ms 1000}}))
             _ (d/transact conn [[:db.fn/call requests/record-fixture-tx claimed (random-uuid)]])
             job (:run/id (run/start! room :test (random-uuid) nil {:kind :experiment}))
             request (requests/record-job! conn (:work.request/id claimed) (:work.request/claim claimed) job)
             app (doto (lifetime/create) lifetime/open!)
             program {:request-kind :benchmark :basis {:submission {:request-id nonce} :benchmark/source {:test true}}}
             original {:room room :room-id (:room w) :party (:owner w)}
             owner {:host {} :handle (str (:room w)) :original original :binding {}}
             principal (ops/party-principal (:owner w))]
         (try
           (f {:conn conn :w w :room room :job job :request request :app app
               :program program :owner owner :principal principal :host-ctx ec/*execution-context*})
           (finally
             (run/finish! job :cancelled)
             (lifetime/fence! app)
             (lifetime/join! app 3000))))))))

(defn- failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error error)))

(deftest original-app-ticket-cannot-be-replaced-by-a-fresh-operation
  (with-admission
    (fn [{:keys [conn w room app program owner principal host-ctx]}]
      (let [contexts (atom 0) admitted (atom 0)]
        (with-redefs [benchmarks/fixture-transport
                      (fn [& _] {:job-context! (fn [_] (swap! contexts inc) ::native-context)
                                 :admit! (fn [_] (swap! admitted inc))
                                 :abandon! (fn [_])})]
          (is (some? (failure #(checkpoint/adapter conn host-ctx room program principal (:room w) owner)))
              "no App operation cannot construct an admission")
          (lifetime/with-operation!
            app
            (fn []
              (let [adapter (checkpoint/adapter conn host-ctx room program principal (:room w) owner)]
               ;; Ordinary reentry intentionally reuses A. Detaching first
               ;; creates an actual fresh B ticket on the same live App.
                (lifetime/call-detached
                 (fn [_]
                   (lifetime/with-operation!
                     app
                     (fn []
                       (doseq [call [#((:job-context! adapter) {}) #((:admit! adapter) (random-uuid))]]
                         (is (= :original-app-unavailable (:reason (ex-data (failure call))))))))))
                (is (zero? @contexts))
                (is (zero? @admitted))
                (is (= ::native-context ((:job-context! adapter) {}))
                    "a losing foreign operation did not consume the original owner")
                ((:abandon! adapter) nil)))))))))

(deftest cancellation-during-c0-stops-before-builder-ready-or-execution
  (with-admission
    (fn [{:keys [conn w room job request app program owner principal host-ctx]}]
      (let [entered (promise) release (promise) builders (atom 0) ready (atom 0) starts (atom 0)]
        (with-redefs [benchmarks/fixture-transport
                      (fn [& _] {:world-parent (constantly room) :admit! (fn [_])
                                 :abandon! (fn [_]) :settled! (fn [_])})
                      room-operation/with-current! (fn [_ _ _ _ f] (f))
                      owned/validate! (fn [& _] true)
                      resolution/publish-preparation! (fn [& _] {:new? true :cases [{}] :plan {:cases [{}]}})
                      fixture/validate-receipt! (fn [_ _ receipt] receipt)
                      snapshot/capture! (fn [& _] (deliver entered true) @release {:db ::unused :c0 {}})
                      evaluation/with-checkpoint-fixture! (fn [& _] (swap! builders inc) (throw (AssertionError. "No builder after cancellation")))
                      resolution/publish-ready! (fn [& _] (swap! ready inc) (throw (AssertionError. "No READY after cancellation")))
                      runner/run-in (fn [& _] (swap! starts inc) (throw (AssertionError. "No execution after cancellation")))]
          (lifetime/with-operation!
            app
            (fn []
              (let [adapter (checkpoint/adapter conn host-ctx room program principal (:room w) owner)
                    canceller (lifetime/call-detached
                               (fn [_]
                                 (future
                                   (when-not (= true (deref entered 5000 ::timeout))
                                     (throw (ex-info "C0 admission did not enter" {})))
                                   (run/cancel-run! job)
                                   (deliver release true))))]
                (try
                  (is (= :job-no-longer-running
                         (:reason (ex-data (failure #((:admit! adapter) job))))))
                  @canceller
                  (is (= :running (:run/status (run/run room job)))
                      "durable running status alone did not authorize continuation")
                  (is (run/cancel-requested? job))
                  (is (= :no-admitted-plan (:reason (ex-data (failure #((:start! adapter) job))))))
                  (is (zero? @builders))
                  (is (zero? @ready))
                  (is (zero? @starts))
                  (finally
                    (deliver release true)
                    @canceller
                    ((:abandon! adapter) nil)))))))))))

(deftest wrong-job-cannot-consume-the-admitted-one-shot-plan
  (with-admission
    (fn [{:keys [conn w room job app program owner principal host-ctx]}]
      (let [starts (atom 0)]
        ;; The case compiler is controlled in this admission-boundary test;
        ;; native two-case execution is verified independently.
        (with-redefs [benchmarks/fixture-transport
                      (fn [& _] {:world-parent (constantly room) :admit! (fn [_])
                                 :abandon! (fn [_]) :settled! (fn [_])})
                      room-operation/with-current! (fn [_ _ _ _ f] (f))
                      owned/validate! (fn [& _] true)
                      resolution/publish-preparation! (fn [& _] {:new? true :cases [] :plan {:cases []}})
                      fixture/validate-receipt! (fn [_ _ receipt] receipt)
                      snapshot/capture! (fn [& _] {:db ::unused :c0 {}})
                      resolution/publish-ready! (fn [& _] {:plan {:test :admitted}})
                      runner/run-in (fn [control plan]
                                      (is (identical? room control))
                                      (is (= job (:parent-run plan)))
                                      (swap! starts inc)
                                      ::started)]
          (lifetime/with-operation!
            app
            (fn []
              (let [adapter (checkpoint/adapter conn host-ctx room program principal (:room w) owner)]
                ((:admit! adapter) job)
                (is (= :no-admitted-plan
                       (:reason (ex-data (failure #((:start! adapter) (random-uuid)))))))
                (is (zero? @starts))
                (is (= ::started ((:start! adapter) job)))
                (is (= :no-admitted-plan (:reason (ex-data (failure #((:start! adapter) job))))))
                (is (= 1 @starts))))))))))
