(ns is.simm.runtimes.application-ops-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as dops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.results :as results]
            [is.simm.ops.work-api :as work-api]
            [is.simm.ops.worker-api :as worker-api]
            [is.simm.ops.worker-control :as worker-control]
            [is.simm.model.system-db :as system-db]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.workflow-schedules :as schedules]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

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
