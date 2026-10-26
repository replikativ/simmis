(ns is.simm.authority.page-view-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [is.simm.authority.world :as world]
            [is.simm.ops.owned-saved-views :as views]
            [is.simm.ops.owned-saved-views-test :as fixture]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.saved-view-remote :as remote]))

(deftest production-entry-registers-the-fixed-page-view-read
  (let [form (binding [*read-eval* false]
               (with-open [reader (java.io.PushbackReader. (io/reader (io/resource "is/simm/runtimes/web.clj")))]
                 (read reader)))
        requires (some #(when (and (seq? %) (= :require (first %))) (rest %)) (drop 2 form))]
    (is (some #(= 'is.simm.uis.web.desktop.saved-view-remote (if (vector? %) (first %) %)) requires))))

(deftest inbound-page-views-use-current-read-authority-and-original-app
  (@#'fixture/with-fixture
   (fn [{:keys [host owner w] :as context}]
     (let [request (@#'fixture/request context)]
       (with-redefs-fn {#'application/installation (atom {:host host :owner owner})}
         #(do
            (doseq [kind [:non-member :anon :admin]]
              (is (world/refused? (world/inbound! (world/principal w kind) "load-memory-saved-view!"
                                                 {:room-id-str (str (:room w)) :request request}))))
            (let [result (:result (world/inbound! (world/principal w :member) "load-memory-saved-view!"
                                                 {:room-id-str (str (:room w)) :request request}))]
              (is (= :ready (:status result)))
              (is (= (:expected-target-stamp request) (:target-stamp result))))
            (let [queued (remote/prepare-operation (:member w) (str (:room w)) request)
                  replacement (lifetime/create) calls (atom 0)]
              (lifetime/open! replacement)
              (try
                (lifetime/fence! owner)
                (with-redefs-fn {#'application/installation (atom {:host host :owner replacement})
                                #'views/read! (fn [& _] (swap! calls inc))}
                  (fn []
                    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"This saved view is unavailable" (queued)))
                    (is (zero? @calls))))
                (finally (lifetime/fence! replacement) (lifetime/join! replacement 5000))))))))))

(defn- safe-error [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e
                 {:message (ex-message e) :data (ex-data e) :cause (.getCause e)})))

(deftest arbitrary-helper-tags-and-messages-never-become-the-wire-contract
  (let [party (random-uuid) room (str (random-uuid))
        failure (fn [& _] (throw (ex-info "SECRET source" {:type :memory/target-changed :secret "SECRET"})))
        expected {:message "This saved view is unavailable."
                  :data {:type :memory/saved-view-unavailable} :cause nil}]
    (with-redefs [application/capture-operation failure]
      (is (= expected (safe-error #(remote/prepare-operation party room {})))))
    (with-redefs [application/capture-operation (fn [] failure)]
      (is (= expected (safe-error (remote/prepare-operation party room {})))))
    (with-redefs [application/capture-operation (fn [] (fn [f] (f :captured-host))) views/read! failure]
      (is (= expected (safe-error (remote/prepare-operation party room {})))))
    (with-redefs [application/capture-operation (fn [] (fn [_] (throw (ex-info "SECRET" {} (InterruptedException. "SECRET")))))]
      (try
        (Thread/interrupted)
        (let [result (safe-error (remote/prepare-operation party room {}))
              interrupted? (.isInterrupted (Thread/currentThread))]
          (Thread/interrupted)
          (is (= expected result))
          (is interrupted?))
        (finally (Thread/interrupted))))))
