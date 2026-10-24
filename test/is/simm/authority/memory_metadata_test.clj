(ns is.simm.authority.memory-metadata-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.documents.files :as files]
            [is.simm.authority.world :as world]
            [is.simm.ops.owned-charts-test :as fixture]
            [is.simm.ops.owned-metadata :as metadata]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.memory-remote :as remote]))

(def demand {:folders [""] :paths ["wiki/sales.md" "wiki/missing.md"]})

(deftest queued-metadata-cannot-borrow-a-replacement-application
  (@#'fixture/with-fixture
   (fn [{:keys [host owner w]}]
     (with-redefs-fn {#'application/installation (atom {:host host :owner owner})}
       #(do
          (doseq [kind [:non-member :anon :admin]]
            (is (world/refused? (world/inbound! (world/principal w kind) "load-memory-metadata!"
                                              {:room-id-str (str (:room w)) :request demand}))))
          (is (= 2 (get-in (world/inbound! (world/principal w :member) "load-memory-metadata!"
                                         {:room-id-str (str (:room w)) :request demand}) [:result :protocol-version])))
          (let [queued (remote/prepare-metadata (:member w) (str (:room w)) demand)
                replacement (doto (lifetime/create) lifetime/open!) calls (atom 0)]
            (try
              (lifetime/fence! owner)
              (with-redefs-fn {#'application/installation (atom {:host host :owner replacement})
                              #'metadata/read! (fn [& _] (swap! calls inc))}
                (fn []
                  (is (thrown? clojure.lang.ExceptionInfo (queued)))
                  (is (zero? @calls))))
              (finally (lifetime/fence! replacement) (lifetime/join! replacement 5000)))))))))

(deftest malformed-demand-is-refused-before-capturing-or-queuing-work
  (let [calls (atom 0) party (random-uuid) room (str (random-uuid))]
    (with-redefs [application/capture-operation (fn [] (swap! calls inc))]
      (doseq [[actor handle request] [[nil room demand] [party "not-a-room" demand]
                                      [party room (assoc demand :resolver :injected)]
                                      [party room {:folders (vec (repeat 65 "wiki")) :paths []}]
                                      [party room {:folders [] :paths ["../escape.md"]}]]]
        (is (thrown? clojure.lang.ExceptionInfo (remote/prepare-metadata actor handle request))))
      (is (zero? @calls)))))

(deftest helper-diagnostics-are-not-metadata-and-interruption-is-preserved
  (let [party (random-uuid) room (str (random-uuid))
        safe (fn [f] (try (f) nil (catch clojure.lang.ExceptionInfo e
                                  {:message (ex-message e) :data (ex-data e) :cause (.getCause e)})))
        expected {:message "Memory metadata is unavailable"
                  :data {:type :memory/metadata-unavailable} :cause nil}
        failure (fn [& _] (throw (ex-info "PRIVATE connection" {:source "PRIVATE"})))]
    (with-redefs [application/capture-operation failure]
      (is (= expected (safe #(remote/prepare-metadata party room demand)))))
    (with-redefs [application/capture-operation (fn [] (fn [f] (f :original-host)))
                  metadata/read! failure]
      (is (= expected (safe (remote/prepare-metadata party room demand)))))
    (with-redefs [application/capture-operation
                  (fn [] (fn [_] (throw (ex-info "PRIVATE" {} (InterruptedException. "PRIVATE")))))]
      (try
        (Thread/interrupted)
        (let [result (safe (remote/prepare-metadata party room demand))
              interrupted? (.isInterrupted (Thread/currentThread))]
          (Thread/interrupted)
          (is (= expected result))
          (is interrupted?))
        (finally (Thread/interrupted))))))

(deftest valid-default-retarget-returns-only-the-public-target-conflict
  (@#'fixture/with-fixture
   (fn [{:keys [conn host owner w attached-descriptor]}]
     (let [project files/metadata-in]
       (with-redefs-fn {#'application/installation (atom {:host host :owner owner})}
         #(with-redefs [files/metadata-in
                        (fn [db request]
                          (let [result (project db request)]
                            (d/transact conn [{:room/id (:room w) :room/default-repository
                                              [:system/id (:registry-system attached-descriptor)]}])
                            result))]
            (let [failure (try ((remote/prepare-metadata (:member w) (str (:room w)) demand))
                               nil (catch clojure.lang.ExceptionInfo error error))]
              (is (= {:type :memory/target-changed} (ex-data failure)))
              (is (= "The Memory repository changed; refresh before editing." (ex-message failure)))
              (is (nil? (.getCause failure))))))))))
