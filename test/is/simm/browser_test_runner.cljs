(ns is.simm.browser-test-runner
  "Browser-native tests; completion is an explicit CDP binding event."
  (:require [cljs.test :as ct]
            [shadow.test :as st]
            [shadow.test.env :as env]))

(defn- report! [original event]
  ;; The default reporter owns pass/fail/error counters. Replacing it without
  ;; delegation can report zero failed assertions even when assertions fail.
  (original event)
  (case (:type event)
    :fail (js/console.error (pr-str (select-keys event [:type :message :expected :actual :file :line])))
    :error (js/console.error (pr-str (select-keys event [:type :message :expected :actual :file :line])))
    :end-run-tests
    (let [result (clj->js (assoc (select-keys event [:test :pass :fail :error]) :version 1))]
      (aset js/window "__simmisTestResult" result)
      (when-let [report (aget js/window "__simmisReport")]
        (report (js/JSON.stringify result))))
    nil))

(defn start []
  (env/reset-test-data! (env/get-test-data))
  (let [original ct/report]
    (st/run-all-tests (assoc (ct/empty-env) :report-fn (partial report! original)))))

(defn stop [done] (done))

(defn ^:export init [] (start))
