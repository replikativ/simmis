(ns is.simm.authority.block-reference-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [is.simm.authority.world :as world]
            [is.simm.ops.owned-charts-test :as fixture]
            [is.simm.ops.owned-block-references-test :as request]
            [is.simm.ops.owned-block-references :as references]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.memory-reference-remote :as remote]))

(deftest fixed-reference-read-is-registered-on-cold-start
  (let [form (binding [*read-eval* false]
               (with-open [reader (java.io.PushbackReader. (io/reader (io/resource "is/simm/runtimes/web.clj")))]
                 (read reader)))
        requires (some #(when (and (seq? %) (= :require (first %))) (rest %)) (drop 2 form))]
    (is (some #(= 'is.simm.uis.web.desktop.memory-reference-remote
                  (if (vector? %) (first %) %)) requires))))

(deftest actual-inbound-reference-read-preserves-human-and-owner-boundaries
  (@#'fixture/with-fixture
   (fn [{:keys [host owner w] :as context}]
     (let [r (request/request context)]
       (with-redefs-fn {#'application/installation (atom {:host host :owner owner})}
         #(do
            (doseq [kind [:non-member :anon :admin]]
              (is (world/refused? (world/inbound! (world/principal w kind) "resolve-memory-block!"
                                                {:room-id-str (str (:room w)) :request r}))))
            (is (= :resolved (get-in (world/inbound! (world/principal w :member) "resolve-memory-block!"
                                                   {:room-id-str (str (:room w)) :request r}) [:result :status])))
            (let [queued (remote/prepare-operation (:member w) (str (:room w)) r)
                  replacement (lifetime/create) calls (atom 0)]
              (lifetime/open! replacement)
              (try
                (lifetime/fence! owner)
                (with-redefs-fn {#'application/installation (atom {:host host :owner replacement})
                                #'references/read! (fn [& _] (swap! calls inc))}
                  (fn []
                    (is (thrown? clojure.lang.ExceptionInfo (queued)))
                    (is (zero? @calls))))
                (finally (lifetime/fence! replacement) (lifetime/join! replacement 5000))))))))))

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
