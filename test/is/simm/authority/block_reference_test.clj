(ns is.simm.authority.block-reference-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.owned-block-references :as references]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.uis.web.desktop.memory-reference-remote :as remote]))

(deftest helper-exceptions-never-become-wire-data
  (let [party (random-uuid) room (str (random-uuid))
        safe (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e
                                  {:message (ex-message e) :data (ex-data e) :cause (.getCause e)})))
        failure (fn [& _] (throw (ex-info "PRIVATE source" {:type :memory/target-changed :source "PRIVATE"})))
        expected {:message "This reference is unavailable in the current workspace."
                  :data {:type :memory/reference-unavailable} :cause nil}]
    (with-redefs [application/capture-operation failure]
      (is (= expected (safe #(remote/prepare-operation party room {})))))
    (with-redefs [application/capture-operation (fn [] (fn [f] (f :captured-host))) references/read! failure]
      (is (= expected (safe (remote/prepare-operation party room {})))))
    (with-redefs [application/capture-operation
                  (fn [] (fn [_] (throw (ex-info "PRIVATE" {} (InterruptedException. "PRIVATE")))))]
      (try
        (Thread/interrupted)
        (let [result (safe (remote/prepare-operation party room {}))
              interrupted? (.isInterrupted (Thread/currentThread))]
          (Thread/interrupted)
          (is (= expected result))
          (is interrupted?))
        (finally (Thread/interrupted))))))
