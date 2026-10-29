(ns is.simm.ops.benchmark-report-api-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.ops :as native]
            [is.simm.ops.benchmark-reports :as reports]
            [is.simm.ops.benchmark-notifications :as notifications]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-host :as policy]
            [is.simm.ops.work-api :as api]
            [is.simm.runtimes.mcp :as mcp]))

(deftest fixed-report-bindings-preserve-original-handle-and-normalize-only-identities
  (let [room (str (random-uuid)) scorecard (random-uuid) target (random-uuid) plan (random-uuid)
        host {:original-host true} principal {:principal/id "party/test"} calls (atom [])
        prepare (get-in api/specifications [:simmis/benchmark-report-prepare :impl])
        publish (get-in api/specifications [:simmis/benchmark-report-publish :impl])]
    (with-redefs [reads/owned-host? (constantly true)
                  reports/prepare! (fn [& args] (swap! calls conj args) {:prepared true})
                  reports/publish! (fn [& args] (swap! calls conj args) {:published true})]
      (binding [native/*principal* principal]
        (is (= {:prepared true} (prepare host {:room room :scorecard (str scorecard)})))
        (is (= {:published true} (publish host {:room room :scorecard (str scorecard)
                                               :expected-target-stamp (str target) :expected-plan (str plan)})))
        (is (= [[host principal room {:scorecard scorecard}]
                [host principal room {:scorecard scorecard :expected-target-stamp target :expected-plan plan}]]
               (mapv vec @calls)))
        (doseq [bad [{:room room :scorecard (str scorecard)}
                     {:room room :scorecard (str scorecard) :expected-plan (str plan)
                      :expected-target-stamp (str target) :files {"other.md" "injected"}}
                     {:room room :scorecard "not-an-id" :expected-plan (str plan) :expected-target-stamp (str target)}]]
          (is (thrown? clojure.lang.ExceptionInfo (publish host bad))))
        (is (= 2 (count @calls)) "Rejected transport shapes never call the publisher")))))

(deftest report-publication-needs-a-work-credential-and-canonical-human-host
  (doseq [[op tool] [[:simmis/benchmark-report-prepare "simmis_benchmark-report-prepare"]
                     [:simmis/benchmark-result-retry "simmis_benchmark-result-retry"]
                     [:simmis/benchmark-report-publish "simmis_benchmark-report-publish"]]]
    (is (= :write (get-in api/specifications [op :kind])))
    (is (= :write (get-in api/specifications [op :authz :action])))
    (is (= :write (get policy/owned-actions op)))
    (is (contains? mcp/work-tools tool))
    (is (not (contains? mcp/read-tools tool)))
    (is (not (contains? mcp/worker-control-tools tool))))
  (with-redefs [reads/owned-host? (constantly false)
                reports/prepare! (fn [& _] (throw (AssertionError. "Legacy host reached canonical report service")))]
    (is (thrown? clojure.lang.ExceptionInfo
                 ((get-in api/specifications [:simmis/benchmark-report-prepare :impl])
                  {} {:room "native-slug" :scorecard (str (random-uuid))})))))

(deftest notification-retry-uses-original-room-and-exact-request-only
  (let [room (str (random-uuid)) request (random-uuid) calls (atom [])
        retry (get-in api/specifications [:simmis/benchmark-result-retry :impl])]
    (with-redefs [reads/owned-host? (constantly true)
                  notifications/retry! (fn [& args] (swap! calls conj args) {:status :published})]
      (is (= {:status :published} (retry {} {:room room :request (str request)})))
      (is (= room (nth (first @calls) 2)))
      (is (= request (nth (first @calls) 3)))
      (is (thrown? clojure.lang.ExceptionInfo
                   (retry {} {:room room :request (str request) :scorecard (str (random-uuid))})))
      (is (= 1 (count @calls))))))
