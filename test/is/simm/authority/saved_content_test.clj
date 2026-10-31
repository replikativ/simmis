(ns is.simm.authority.saved-content-test
  "Every saved-content endpoint crosses the actual inbound gate and captures
   its original App. One read-only native fixture covers the shared boundary."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.authority.world :as world]
            [is.simm.ops.owned-backlinks :as backlinks]
            [is.simm.ops.owned-backlinks-test :as backlink-fixture]
            [is.simm.ops.owned-block-references :as blocks]
            [is.simm.ops.owned-block-references-test :as block-fixture]
            [is.simm.ops.owned-charts :as charts]
            [is.simm.ops.owned-charts-test :as chart-fixture]
            [is.simm.ops.owned-saved-views :as views]
            [is.simm.ops.owned-saved-views-test :as fixture]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.chart-remote :as chart-remote]
            [is.simm.uis.web.desktop.memory-reference-remote :as reference-remote]
            [is.simm.uis.web.desktop.saved-view-remote :as view-remote]))

(defn- endpoints [context]
  [{:rpc "load-memory-chart!" :status :ready :error-pattern #"This saved chart is unavailable"
    :request (#'chart-fixture/request context)
    :prepare chart-remote/prepare-operation :read-var #'charts/read!}
   {:rpc "load-memory-saved-view!" :status :ready :error-pattern #"This saved view is unavailable"
    :request (#'fixture/request context)
    :prepare view-remote/prepare-operation :read-var #'views/read!}
   {:rpc "resolve-memory-block!" :status :resolved
    :request (block-fixture/request context)
    :prepare reference-remote/prepare-operation :read-var #'blocks/read!}
   {:rpc "load-memory-backlinks!" :status :ready
    :request (backlink-fixture/request context)
    :prepare reference-remote/prepare-backlinks-operation :read-var #'backlinks/read!}])

(deftest actual-inbound-content-reads-preserve-human-and-original-owner-boundaries
  (#'fixture/with-fixture
   (fn [{:keys [host owner w] :as context}]
     (let [handle (str (:room w)) cases (endpoints context)]
       (with-redefs-fn
         {#'application/installation (atom {:host host :owner owner})}
         (fn []
           ;; These calls read only. The shared owner is fenced only after all
           ;; endpoint-specific positive paths and captured closures are ready.
           (doseq [{:keys [rpc status request]} cases]
             (testing rpc
               (doseq [kind [:non-member :anon :admin]]
                 (is (world/refused? (world/inbound! (world/principal w kind) rpc
                                                    {:room-id-str handle :request request}))))
               (let [result (:result (world/inbound! (world/principal w :member) rpc
                                                     {:room-id-str handle :request request}))]
                 (is (= status (:status result)))
                 (is (= (:expected-target-stamp request) (:target-stamp result))))))
           (let [queued (mapv (fn [{:keys [prepare request]}] (prepare (:member w) handle request)) cases)
                 replacement (doto (lifetime/create) lifetime/open!)
                 calls (atom 0)
                 forbidden (fn [& _] (swap! calls inc))]
             (try
               (lifetime/fence! owner)
               (with-redefs-fn
                 (into {#'application/installation (atom {:host host :owner replacement})}
                       (map (fn [{:keys [read-var]}] [read-var forbidden])) cases)
                 (fn []
                   (doseq [[{:keys [rpc error-pattern]} invoke] (map vector cases queued)]
                     (testing rpc
                       (if error-pattern
                         (is (thrown-with-msg? clojure.lang.ExceptionInfo error-pattern (invoke)))
                         (is (thrown? clojure.lang.ExceptionInfo (invoke))))))
                   (is (zero? @calls) "A replacement App must never receive the old queued reads")))
               (finally (lifetime/fence! replacement) (lifetime/join! replacement 5000))))))))))
