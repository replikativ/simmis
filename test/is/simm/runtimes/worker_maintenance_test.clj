(ns is.simm.runtimes.worker-maintenance-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.worker-recovery :as recovery]
            [is.simm.ops.worker-api :as api]
            [is.simm.model.system-db :as system-db]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.worker-maintenance :as maintenance]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent CountDownLatch ScheduledThreadPoolExecutor TimeUnit]))

(use-fixtures :each
  (fn [f]
    ;; Never stop or reuse the real development application's host resource.
    (with-redefs-fn {#'maintenance/service (atom nil)}
      #(try (f) (finally (maintenance/stop!))))))

(deftest recovery-is-serial-pending-only-and-one-failure-does-not-starve-others
  (let [ids (repeatedly 4 random-uuid) calls (atom [])
        rows (mapv (fn [id] {:work.request/id id :work.request/kind (if (= id (last ids)) :local-worker-workflow :local-worker)
                             :work.request/return-pending? true}) ids)]
    (with-redefs [requests/requests (fn [conn] (is (= ::conn conn))
                                      (conj rows {:work.request/id (random-uuid) :work.request/kind :task
                                                  :work.request/return-pending? true}
                                            {:work.request/id (random-uuid) :work.request/kind :local-worker
                                             :work.request/return-pending? false}))
                  recovery/recover-request!
                  (fn [conn ctx resolve id]
                    (is (= [::conn context/server-context ::resolver]
                           [conn ctx resolve]))
                    (is (identical? context/server-context ec/*execution-context*))
                    (swap! calls conj id)
                    (case (.indexOf (vec ids) id)
                      0 (throw (ex-info "scripted held request" {}))
                      1 {:state :awaiting-stop}
                      2 {:state :awaiting-world-cleanup}
                      3 {:state :returned}))]
      (is (= {:error 1 :awaiting-stop 1 :awaiting-world-cleanup 1 :returned 1}
             (maintenance/reconcile! ::conn context/server-context ::resolver)))
      (is (= (vec ids) @calls)))))

(deftest service-is-one-owned-daemon-resource-and-stop-is-joined-and-idempotent
  (is (false? (maintenance/running?)))
  (maintenance/start! ::conn context/server-context ::resolver)
  (let [owned @@#'maintenance/service
        executor ^ScheduledThreadPoolExecutor (:executor owned)]
    (is (maintenance/running?))
    (maintenance/start! ::conn context/server-context ::resolver)
    (is (identical? executor (:executor @@#'maintenance/service)))
    (is (= 1 (.size (.getQueue executor))))
    (maintenance/stop!)
    (is (.isTerminated executor))
    (is (false? (maintenance/running?)))
    (is (nil? @@#'maintenance/service))
    (maintenance/stop!)))

(deftest failed-physical-join-retains-handle-and-refuses-replacement
  (maintenance/start! ::conn context/server-context ::resolver)
  (let [owned @@#'maintenance/service]
    (with-redefs-fn {#'maintenance/await-stop! (constantly false)}
      #(is (thrown? clojure.lang.ExceptionInfo (maintenance/stop!))))
    (is (identical? owned @@#'maintenance/service))
    (is (false? (maintenance/running?)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (maintenance/start! ::conn context/server-context ::resolver)))
    (maintenance/stop!)
    (is (nil? @@#'maintenance/service))))

(deftest startup-failure-holds-claims-until-an-actual-successful-retry
  (let [task (atom nil) attempts (atom 0)
        factory (fn []
                  (proxy [ScheduledThreadPoolExecutor] [1]
                    (scheduleWithFixedDelay [runnable initial period unit]
                      (reset! task runnable)
                      (proxy-super scheduleWithFixedDelay runnable initial period unit))))]
    (with-redefs-fn {#'maintenance/new-executor factory
                     #'requests/requests (constantly [])
                     #'recovery/recover! (fn [& _]
                                           (when (= 1 (swap! attempts inc))
                                             (throw (ex-info "scripted initial recovery failure" {}))) [])}
      (fn []
        (maintenance/prepare! ::conn context/server-context ::resolver)
        (is (false? (maintenance/claims-ready?)))
        (maintenance/start! ::conn context/server-context ::resolver)
        (is (maintenance/running?))
        (is (false? (maintenance/claims-ready?)))
        ;; Execute the captured maintenance callback explicitly. No sleeps,
        ;; clock races, retries of execution, or invented readiness evidence.
        (.run ^Runnable @task)
        (is (= 2 @attempts))
        (is (maintenance/claims-ready?))
        (maintenance/stop!)
        (is (false? (maintenance/claims-ready?)))))))

(deftest held-claims-never-disable-existing-lease-heartbeat-or-stop
  (let [calls (atom []) operation (fn [conn principal args] (swap! calls conj args) ::receipt)]
    (with-redefs [system-db/get-conn (constantly ::conn)
                  maintenance/claims-ready? (constantly false)]
      (doseq [op [:simmis/worker-pending :simmis/worker-claim]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (#'api/invoke {:execution-ctx context/server-context} op operation {}))))
      (is (empty? @calls))
      (doseq [op [:simmis/worker-heartbeat :simmis/worker-stop]]
        (is (= ::receipt (#'api/invoke {:execution-ctx context/server-context} op operation {:run "lease"}))))
      (is (= 2 (count @calls))))))

(deftest stopping-the-service-interrupts-and-joins-an-active-owned-task
  (maintenance/start! ::conn context/server-context ::resolver)
  (let [executor ^ScheduledThreadPoolExecutor (:executor @@#'maintenance/service)
        entered (CountDownLatch. 1) done (CountDownLatch. 1) hold (CountDownLatch. 1)]
    (.execute executor ^Runnable
              (fn []
                (try (.countDown entered) (.await hold)
                     (catch InterruptedException _ nil)
                     (finally (.countDown done)))))
    (is (.await entered 5 TimeUnit/SECONDS))
    (maintenance/stop!)
    (is (zero? (.getCount done)))
    (is (.isTerminated executor))))

(deftest unresolved-parent-room-cannot-be-mistaken-for-successful-startup-fencing
  (with-redefs [recovery/recover! (constantly [nil])
                requests/requests (constantly [{:work.request/id (random-uuid) :work.request/kind :local-worker
                                                :work.request/return-pending? true :work.request/room (random-uuid)}])]
    (maintenance/prepare! ::conn context/server-context (constantly nil))
    (is (false? (:claims-ready? @@#'maintenance/service)))
    (maintenance/start! ::conn context/server-context (constantly nil))
    (is (maintenance/running?))
    (is (false? (maintenance/claims-ready?)))))
