(ns is.simm.ops.benchmark-checkpoint-routing-test
  "Real source and canonical claims; allocation/execution deliberately replaced
   to isolate the saved-policy routing boundary. No runner or model proof."
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.benchmark-checkpoint-adapter :as checkpoint]
            [is.simm.ops.benchmark-resolution-test :as source]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.owned-benchmark-admission :as admission]
            [is.simm.ops.owned-benchmark-admission-test :as helper]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(deftest saved-checkpoint-policy-requires-original-app-before-claim
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host owned]}]
     (@#'helper/promote! conn w)
     (let [files (@#'source/source)
           claims (atom 0)]
       (@#'helper/seed! owned files)
       (let [input (@#'helper/input :benchmark host w files)]
         (with-redefs [work/submit-prepared! (fn [& _] (swap! claims inc))]
           (is (some? (@#'helper/failure #(admission/submit-native! host (@#'helper/principal w) (str (:room w)) input))))
           (is (zero? @claims))))))))

(deftest saved-checkpoint-policy-routes-once-and-retry-does-not-recapture
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host owned room]}]
     (@#'helper/promote! conn w)
     (let [files (@#'source/source)
           app (doto (lifetime/create) lifetime/open!)
           host (assoc-in host [:config :host-lifecycle :with-operation] #(lifetime/with-operation! app %))
           seen (atom nil) routed (atom [])
           leased? (atom false)
           with-current! room-operation/with-current!
           manifest-in workflows/manifest-in
           snapshot-in workflows/snapshot-in]
       (try
         (@#'helper/seed! owned files)
         (let [input (@#'helper/input :benchmark host w files)
               principal (@#'helper/principal w)]
           (with-redefs [room-operation/with-current!
                         (fn [host principal handle original f]
                           (with-current! host principal handle original
                             #(try (reset! leased? true) (f)
                                   (finally (reset! leased? false)))))
                         workflows/manifest-in
                         (fn [& args]
                           (is @leased? "source metadata is read under the original Room lease")
                           (apply manifest-in args))
                         workflows/snapshot-in
                         (fn [& args]
                           (is @leased? "source bodies are read under the original Room lease")
                           (apply snapshot-in args))
                         checkpoint/adapter
                         (fn [_ _ control prepared _ _ owner]
                           (is (false? @leased?) "source lease ends before private admission")
                           (is (identical? room control))
                           (is (identical? control (get-in owner [:original :room])))
                           (lifetime/check-current!)
                           (is (some? (get-in prepared [:basis :benchmark/source])))
                           (swap! routed conj owner)
                           prepared)
                         benchmarks/adapter (fn [& _] (throw (AssertionError. "Checkpoint source cannot use copied-context adapter")))
                         work/submit-prepared! (@#'helper/claim-only seen)]
             (let [result (admission/submit-native! host principal (str (:room w)) input)
                   forbidden (fn [& _] (throw (AssertionError. "Retry must be source/allocation free")))]
               (is (= 1 (count @routed)))
               (is (nil? (:job result)))
               (with-redefs [reads/resolve-host-room! forbidden
                             reads/resolve-host-read! forbidden
                             workflows/snapshot-in forbidden
                             checkpoint/adapter forbidden
                             work/submit-prepared! forbidden]
                 (is (= result (admission/submit-native! host principal (str (:room w)) input)))))))
         (finally (lifetime/fence! app) (lifetime/join! app 3000)))))))
