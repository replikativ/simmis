(ns is.simm.authority.chart-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.owned-charts :as charts]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.uis.web.desktop.chart-remote :as remote]))

(defn- safe-error [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e
                 {:message (ex-message e) :data (ex-data e) :cause (.getCause e)})))

(deftest helper-messages-data-causes-and-forged-tags-never-cross-the-boundary
  (let [party (random-uuid) room (str (random-uuid))
        failure (fn [& _] (throw (ex-info "SECRET body" {:type :memory/target-changed :secret "SECRET"})))
        expected {:message "This saved chart is unavailable." :data {:type :memory/chart-unavailable} :cause nil}]
    (with-redefs [application/capture-operation failure]
      (is (= expected (safe-error #(remote/prepare-operation party room {})))))
    (with-redefs [application/capture-operation (fn [] failure)]
      (is (= expected (safe-error (remote/prepare-operation party room {})))) )
    (with-redefs [application/capture-operation (fn [] (fn [f] (f :captured-host))) charts/read! failure]
      (is (= expected (safe-error (remote/prepare-operation party room {})))))
    (with-redefs [application/capture-operation (fn [] (fn [_] (throw (ex-info "SECRET" {} (InterruptedException. "SECRET")))))]
      (try
        (Thread/interrupted)
        (let [result (safe-error (remote/prepare-operation party room {}))
              interrupted? (.isInterrupted (Thread/currentThread))]
          ;; Capture before clojure.test's STM counters can consume the flag.
          (Thread/interrupted)
          (is (= expected result))
          (is interrupted?))
        (finally (Thread/interrupted))))))
