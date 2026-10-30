(ns is.simm.ops.github-host-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-service :as service]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.uis.web.desktop.github-remote :as remote]))

(deftest only-synchronous-failure-before-the-git-body-proves-not-started
  (doseq [entered? [false true]]
    (let [calls (atom 0) party (random-uuid) room (random-uuid)]
      (with-redefs [application/capture-operation (fn [] (fn [body] (body ::original)))
                    host/in! (fn [_ _ _ _ body]
                               (if entered? (body {})
                                   (throw (ex-info "Before Git callback" {:type ::admission}))))]
        (let [invoke (host/capture party room :prepare
                                   (fn [_] (swap! calls inc) (throw (ex-info "Inside Git callback" {:type ::effect}))))]
          (try (invoke) (is false "Fixture must fail")
               (catch clojure.lang.ExceptionInfo error
                 (is (= (if entered? ::effect :is.simm.ops.github-host/not-started) (:type (ex-data error))))))
          (is (= (if entered? 1 0) @calls)))))))

(deftest a-construction-refusal-is-reported-as-not-started-only-for-preparation
  (let [party (random-uuid) room (str (random-uuid))]
    (with-redefs [service/prepare (fn [& _]
                                    (throw (ex-info "No original App" {:type :is.simm.ops.github-host/not-started})))]
      (is (= :not-started (:status ((remote/prepare-action party room :prepare-push {})))))
      (is (= :unconfirmed (:status ((remote/prepare-action party room :apply {}))))))))
