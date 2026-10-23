(ns is.simm.runtimes.application-ops-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as dops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.results :as results]
            [is.simm.ops.work-api :as work-api]
            [is.simm.ops.worker-api :as worker-api]
            [is.simm.ops.worker-control :as worker-control]
            [is.simm.model.system-db :as system-db]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.runtimes.workflow-schedules :as schedules]
            [org.replikativ.spindel.engine.impl.simple :as simple])
  (:import [java.util.concurrent CountDownLatch]))

(deftest captured-registration-is-engine-safe-but-native-invocation-is-not
  (let [owner (lifetime/create) host {:test :original} reads (atom 0) calls (atom 0)]
    (lifetime/open! owner)
    (with-redefs-fn {#'application/installation (atom {:host host :owner owner})
                    #'reads/current-host-control-db! (fn [& _] (swap! reads inc))}
      (fn []
        (let [invoke (with-redefs [simple/on-drain-thread? (constantly true)]
                       (let [invoke (application/capture-operation)]
                         (is (thrown? clojure.lang.ExceptionInfo (invoke #(swap! calls inc))))
                         invoke))]
          (is (zero? @reads)) (is (zero? @calls))
          (is (= host (invoke identity)))
          (is (= 1 @reads))
          (lifetime/fence! owner)
          (dotimes [_ 2]
            (is (= :not-ready (try (invoke identity) nil
                                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
          (is (= 1 @reads)))))))

(deftest queued-capture-never-adopts-a-replacement-installation
  (let [old (lifetime/create) replacement (lifetime/create) reads (atom [])
        slot (atom {:host :original :owner old})]
    (lifetime/open! old) (lifetime/open! replacement)
    (with-redefs-fn {#'application/installation slot
                    #'reads/current-host-control-db! (fn [host] (swap! reads conj host))}
      (fn []
        (let [queued (application/capture-operation)]
          (lifetime/fence! old)
          (reset! slot {:host :replacement :owner replacement})
          (is (thrown? clojure.lang.ExceptionInfo (queued identity)))
          (is (empty? @reads))
          (is (= :replacement ((application/capture-operation) identity)))
          (is (= [:replacement] @reads)))))))

(deftest an-entered-captured-operation-is-joined-and-may-finish-nesting
  (let [owner (lifetime/create) slot (atom {:host :original :owner owner})
        entered (promise) gate (CountDownLatch. 1)]
    (lifetime/open! owner)
    (with-redefs-fn {#'application/installation slot
                    #'reads/current-host-control-db! (fn [& _] :current)}
      (fn []
        (let [invoke (application/capture-operation)
              task (future (invoke (fn [_]
                                     (deliver entered true)
                                     (.await gate)
                                     (application/with-operation! identity))))]
          (try
            (is (= true (deref entered 3000 ::timeout)))
            (lifetime/fence! owner)
            (is (thrown? clojure.lang.ExceptionInfo (lifetime/join! owner 0)))
            (is (thrown? clojure.lang.ExceptionInfo (application/capture-operation)))
            (.countDown gate)
            (is (= :original (deref task 3000 ::timeout)))
            (is (nil? (lifetime/join! owner 3000)))
            (finally (.countDown gate) (deref task 10000 nil))))))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(defn- isolated [f]
  (with-redefs-fn {#'application/installation (atom nil)
                   #'schedules/service (atom nil)}
    (fn []
      (let [prior (dops/operations) host @@#'server/embedded-host]
        (try (f)
             (finally
               (application/stop!)
               (doseq [specs [#'results/specifications #'work-api/specifications
                              #'worker-api/specifications #'worker-control/specifications]
                       op (keys @specs)]
                 (if-let [spec (get prior op)]
                   (dops/register-op! op spec)
                   (dops/unregister-op! op)))
               (server/sync-op-tools!)
               (server/set-host! host)))))))

(deftest exact-canonical-host-is-shared-and-expired-ownership-cannot-rebind
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn]}]
     (isolated
      (fn []
        (let [host (application/start!)]
          (is (identical? host (application/host!)))
          (is (identical? host (application/start!)))
          (is (identical? host @@#'server/embedded-host))
          (with-redefs [system-db/get-conn (constantly (atom @conn))]
            (is (thrown? clojure.lang.ExceptionInfo (application/host!)))
            (is (thrown? clojure.lang.ExceptionInfo (application/start!))))
          (is (identical? host @@#'server/embedded-host))))))))

(deftest partial-install-is-not-ready-and-installation-failure-rolls-back
  (@#'fixture/with-fixture
   false
   (fn [_]
     (isolated
      (fn []
        (let [install results/install!]
          (with-redefs [results/install!
                        (fn []
                          (is (thrown? clojure.lang.ExceptionInfo (application/host!)))
                          (install))
                        server/set-host!
                        (fn [host]
                          (when host (throw (ex-info "host installation failed" {}))))]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"host installation failed"
                                  (application/start!)))))
        (is (nil? @@#'application/installation))
        (is (some? (application/start!))))))))

(deftest failed-join-retains-nonready-host-until-a-later-successful-stop
  (@#'fixture/with-fixture
   true
   (fn [_]
     (isolated
      (fn []
        (let [host (application/start!) operations (dops/operations)]
          (with-redefs [schedules/stop! (fn [] (throw (ex-info "join held" {})))]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"join held" (application/stop!)))
            (is (identical? host @@#'server/embedded-host))
            (is (= operations (dops/operations)))
            (is (thrown? clojure.lang.ExceptionInfo (application/host!)))
            (is (thrown? clojure.lang.ExceptionInfo (application/start!))))
          (application/stop!)
          (is (nil? @@#'application/installation))
          (is (nil? @@#'server/embedded-host))
          (is (some? (application/start!)))))))))

(deftest held-scheduler-prevents-new-application-installation
  (isolated
   (fn []
     (with-redefs [schedules/held? (constantly true)]
       (is (thrown? clojure.lang.ExceptionInfo (application/start!))))
     (is (nil? @@#'application/installation)))))

(deftest lifecycle-rejects-the-drain-before-taking-any-resource-lock
  (isolated
   (fn []
     (with-redefs [simple/on-drain-thread? (constantly true)]
       (doseq [f [application/start! application/host! application/quiesce! application/stop!]]
         (is (= ::application/engine-thread
                (:type (ex-data (try (f) nil (catch clojure.lang.ExceptionInfo error error)))))))))))

(deftest unknown-registration-or-host-release-retains-a-nonready-installation
  (doseq [failure [:unregister :host-release]]
    (@#'fixture/with-fixture
     false
     (fn [_]
       (isolated
        (fn []
          (let [host (application/start!)
                set-host! server/set-host!
                uninstall! worker-control/uninstall!]
            (with-redefs [worker-control/uninstall!
                          (fn []
                            (if (= :unregister failure)
                              (throw (ex-info "cleanup held" {}))
                              (uninstall!)))
                          server/set-host!
                          (fn [value]
                            (if (and (nil? value) (= :host-release failure))
                              (throw (ex-info "cleanup held" {}))
                              (set-host! value)))]
              (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cleanup held" (application/stop!)))
              (is (identical? host (:host @@#'application/installation)))
              (is (thrown? clojure.lang.ExceptionInfo (application/host!)))
              (is (thrown? clojure.lang.ExceptionInfo (application/start!))))
            (application/stop!)
            (is (nil? @@#'application/installation))
            (is (nil? @@#'server/embedded-host)))))))))

(deftest operation-self-lifecycle-refuses-before-coordinator-or-fencing
  (@#'fixture/with-fixture
   true
   (fn [_]
     (isolated
      (fn []
        (application/start!)
        (application/with-operation!
          (fn [host]
            (doseq [f [application/start! application/quiesce! application/stop!]]
              (is (= :self-drain (reason f))))
            (is (identical? host (application/host!)))))
        (is (some? (application/host!))))))))

(deftest shutdown-joins-an-operation-without-holding-the-installation-monitor
  (@#'fixture/with-fixture
   true
   (fn [_]
     (isolated
      (fn []
        (let [host (application/start!) entered (promise) fenced (promise)
              gate (CountDownLatch. 1)
              task (future
                     (application/with-operation!
                       (fn [actual]
                         (is (identical? host actual))
                         (deliver entered true)
                         (.await gate)
                        ;; This must return promptly during physical join. A
                        ;; held installation monitor would deadlock shutdown.
                         (is (thrown? clojure.lang.ExceptionInfo (application/host!)))
                         :finished)))]
          (try
            (is (= true (deref entered 3000 ::timeout)))
            (with-redefs [schedules/stop! (fn [] (deliver fenced true))]
              (let [stopped (future (application/stop!) :stopped)]
                (try
                  (is (= true (deref fenced 3000 ::timeout)))
                  (is (not (realized? stopped)))
                  (is (identical? host (:host @@#'application/installation)))
                  (is (= :not-ready (reason #(application/with-operation! (constantly :wrong)))))
                  (.countDown gate)
                  (is (= :finished (deref task 3000 ::timeout)))
                  (is (= :stopped (deref stopped 3000 ::timeout)))
                  (finally (.countDown gate) (deref stopped 10000 nil)))))
            (is (nil? @@#'application/installation))
            (finally (.countDown gate) (deref task 10000 nil)))))))))

(deftest retired-captured-mcp-host-cannot-authorize-against-new-installation
  (@#'fixture/with-fixture
   true
   (fn [_]
     (isolated
      (fn []
        (let [old (application/start!) calls (atom 0)]
          (application/stop!)
          (let [replacement (application/start!)]
            (is (not (identical? old replacement)))
            (with-redefs [dops/authorize! (fn [& _] (swap! calls inc))]
              (is (= :not-ready
                     (reason #(dops/invoke old {:principal/id "queued-human" :principal/kind :human} :model/list {}))))
              (is (zero? @calls)))
            (is (identical? replacement (application/with-operation! identity))))))))))

(deftest failed-host-join-retains-the-stopped-owner-and-registrations
  (@#'fixture/with-fixture
   true
   (fn [_]
     (isolated
      (fn []
        (let [host (application/start!) operations (dops/operations)]
          (with-redefs [lifetime/join! (fn [& _] (throw (ex-info "physical join held" {})))]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"physical join held" (application/stop!)))
            (is (identical? host @@#'server/embedded-host))
            (is (= operations (dops/operations)))
            (is (thrown? clojure.lang.ExceptionInfo (application/host!)))
            (is (thrown? clojure.lang.ExceptionInfo (application/start!))))
          (application/stop!)
          (is (nil? @@#'application/installation))))))))
