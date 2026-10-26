(ns is.simm.ops.knowledge-checkpoint-execution-test
  "Real native checkpoint admission with a deterministic SCI candidate. No
   provider/model lookup, dispatch, live scheduling or user-store mutations."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.runtime.preparation :as preparation]
            [dvergr.runtime.threads :as threads]
            [geschichte.bytes :as bytes]
            [geschichte.repo :as repo]
            [is.simm.model.system-db :as sdb]
            [is.simm.model.work-request-checkpoints :as retained]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-proof-test :as model-free]
            [is.simm.ops.knowledge-execution-origin :as origin]
            [is.simm.ops.knowledge-workspace :as workspace]
            [is.simm.model.knowledge-pins-test :as pin-fixture]
            [is.simm.model.knowledge-streams-test :as stream-fixture]
            [is.simm.ops.owned-knowledge-admission-test :as fixture]
            [is.simm.ops.owned-knowledge-discovery-test :as discovery-fixture]
            [is.simm.ops.owned-workflow-admission :as admission]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.spin.cps :refer [spin]])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- checkpoint-fixture [f]
  (@#'fixture/fixture
   (fn [{:keys [conn owned input batch-input] :as data}]
     (d/transact conn retained/schema)
     (repo/write! owned "workflows/wiki/execution-origin.edn" (bytes/utf8 (pr-str origin/policy)))
     (repo/write! owned "workflows/wiki/input-batching.edn"
                  (bytes/utf8 (pr-str {:version 2 :epoch (:epoch batch-input)
                                       :policy {:version 1 :max-trigger-files 1 :max-trigger-bytes 1048576
                                                :context-paths [] :max-context-files 0 :max-context-bytes 0}})))
     (repo/write! owned "wiki/file.md" (bytes/utf8 "C1 wiki context"))
     ;; Larger than the previous whole-context copy budget. It is available
     ;; in C1, but this candidate only needs the small page above.
     (repo/write! owned "wiki/large.md" (bytes/utf8 (apply str (repeat 524289 "x"))))
     (let [source (@#'discovery-fixture/request data)
           input (-> input (assoc-in [:limits :settlement] "discard")
                     (assoc :expected-owned-source (:expected-source source)
                            :expected-bundle (:expected-bundle source)))]
       (f (assoc data :input input))))))

(deftest review-world-settlement-is-refused-before-discovery-or-any-pin
  (checkpoint-fixture
   (fn [{:keys [conn host principal handle input]}]
     (let [pins (atom 0) providers (atom 0) jobs (atom 0)]
       (with-redefs [roots/pin! (fn [& _] (swap! pins inc) (throw (AssertionError. "No native pins")))
                     work/prepare (fn [& _] (swap! providers inc) (throw (AssertionError. "No provider preparation")))
                     jobs/start! (fn [& _] (swap! jobs inc) (throw (AssertionError. "No Job admission")))]
         (is (thrown? Exception
                      (admission/submit! host principal handle (assoc-in input [:limits :settlement] "review")))))
       (is (= [0 0 0] [@pins @providers @jobs]))
       (is (empty? (d/q '[:find ?e :where [?e :work.request/id _]] @conn)))))))

(deftest schedule-activation-refuses-review-worlds-before-publication-or-pinning
  (checkpoint-fixture
   (fn [{:keys [conn host principal handle input]}]
     (let [pins (atom 0) providers (atom 0)
           request {:request-id (random-uuid) :kind "workflow" :enabled? false
                    :cadence {:interval-ms 60000 :tz "UTC"}
                    :execution (-> input (dissoc :request-id) (assoc-in [:limits :settlement] "review"))}]
       (with-redefs [roots/pin! (fn [& _] (swap! pins inc) (throw (AssertionError. "No schedule pinning")))
                     work/prepare (fn [& _] (swap! providers inc) (throw (AssertionError. "No provider preparation")))]
         (is (thrown? Exception (schedules/create-owned! host principal handle request))))
       (is (= [0 0] [@pins @providers]))
       (is (empty? (d/q '[:find ?e :where [?e :schedule.activation/id _]] @conn)))))))

(deftest retained-schedule-dispatch-revalidates-settlement-before-discovery
  (let [source {"execution-origin.edn" (pr-str origin/policy)
                "input-batching.edn" (pr-str {:version 2 :epoch (random-uuid)
                                              :policy {:version 1 :max-trigger-files 1 :max-trigger-bytes 1048576
                                                       :context-paths [] :max-context-files 0 :max-context-bytes 0}})}
        target {:kind :workflow :snapshot {:files source} :execution {:limits {:settlement "review"}}}]
    (is (thrown? clojure.lang.ExceptionInfo (#'schedules/batching target)))
    (is (= 1 (get-in (#'schedules/batching (assoc-in target [:execution :limits :settlement] "discard"))
                     [:policy :max-trigger-files])))))

(deftest native-wiki-reads-c1-on-demand-while-parent-remains-c2
  (checkpoint-fixture
   (fn [{:keys [conn host room owned principal handle input w]}]
     (let [input (assoc-in input [:limits :settlement] "discard")
           grant (work/resources (:limits input))
           team (roster/make-agent
                 (roster/make-roster {:id :checkpoint-execution/team})
                 {:id :candidate :tools #{} :sandbox #{}
                  :program {:kind :sci :source "(ns checkpoint.wiki) (defn main [_] (let [text (slurp \"/wiki/file.md\")] (when-not (= \"C1 wiki context\" text) (throw (ex-info \"Wrong initial checkpoint\" {}))) {:observed text}))"
                            :entrypoint 'checkpoint.wiki/main :args [] :timeout-ms 10000}})]
       (with-redefs [work/prepare
                     (fn [input]
                       ;; Frozen batch C1 already exists. Change only ordinary
                       ;; parent context before the native prediction/Run fork.
                       (repo/write! owned "wiki/file.md" (bytes/utf8 "C2 current context"))
                       (-> (@#'model-free/model-free-preparation input)
                           (assoc :team team :agent :candidate)
                           (assoc-in [:basis :candidate] (roster/agent team :candidate))))
                     sdb/get-conn (constantly conn)]
         (let [result (admission/submit! host principal handle input)
               status (jobs/status (:job result) 25000)
               request (requests/get-request conn (:id result))
               journal (retained/intent @conn (:id result))
               retained-body (retained/payload conn journal)
               retained-request (requests/payload conn request)
               batch (proof/request! conn request)
               manifest (get-in batch [:frozen :captured :manifest])]
           (is (= :completed (:status status)))
           (is (:settled? status))
           (is (= :retained (:work.checkpoint/state journal)))
           (is (= (get-in batch [:selection :basis]) (get-in retained-body [:selection :basis])))
           (is (= (get-in retained-request [:input :knowledge/checkpoint]) (:selection retained-body)))
           (is (not= (:work.checkpoint/id journal)
                     (get-in batch [:selection :expected :knowledge.stream/cursor-root])))
           (is (contains? (roots/roots (:store @owned) {:sync? true}) (:work.checkpoint/id journal)))
           (is (empty? (filter #(= :context (:role %)) (:files manifest))))
           (is (= "C2 current context" (bytes/decode-utf8 (repo/read owned "wiki/file.md"))))
           (is (= grant (binding [ec/*execution-context* (:ctx room)] (resource/balance room))))))))))

(defn- pin-failure-gate [fault]
  (checkpoint-fixture
   (fn [{:keys [conn host room owned principal handle input w app-owner attached-descriptor]}]
     (let [pin! roots/pin! allocate! resource/allocate-run!
           pins (atom 0) allocations (atom 0) starts (atom 0)
           grant (work/resources (:limits input))]
       (with-redefs [sdb/get-conn (constantly conn)
                     work/prepare (fn [input]
                                    (assoc (@#'model-free/model-free-preparation input)
                                           :start! (fn [_] (swap! starts inc) (spin :wrong))))
                     resource/allocate-run! (fn [& args] (swap! allocations inc) (apply allocate! args))
                     roots/pin! (fn [db options mode]
                                  (let [id (pin! db options mode)]
                                    (when (get-in options [:owner :simmis/work-request])
                                      (swap! pins inc)
                                      (case fault
                                        :lost-ack (throw (ex-info "Injected Request pin ACK loss" {}))
                                        :revoked (d/transact conn [[:db/add [:party/id (:owner w)] :actor/kind :agent]])
                                        :retargeted (d/transact conn [{:room/id (:room w)
                                                                       :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                                        :fenced @(threads/future (lifetime/fence! app-owner))))
                                    id))]
         (let [outcome (try {:row (admission/submit! host principal handle input)}
                            (catch Exception e {:error e}))
               _ (when-let [row (:row outcome)] (jobs/status (:job row) 25000))
               id (requests/request-id (:owner w) (:room w) (:request-id input))
               request (requests/get-request conn id)
               retained (retained/intent @conn id)]
           (is (some? (:error outcome)) (str fault " must stop admission"))
           (is (= 1 @pins))
           (is (zero? @allocations))
           (is (zero? @starts))
           (is (= :failed (:status (jobs/status (:work.request/job request) 25000))))
           (is (= :creating (:work.checkpoint/state retained)))
           (is (contains? (roots/roots (:store @owned) {:sync? true}) (:work.checkpoint/id retained)))
           (is (= grant (binding [ec/*execution-context* (:ctx room)] (resource/balance room))))))))))

(deftest lost-request-pin-ack-admits-no-resources-or-candidate
  (pin-failure-gate :lost-ack))

(deftest authority-changes-during-pin-io-admit-no-resources-or-candidate
  (doseq [fault [:revoked :retargeted :fenced]] (pin-failure-gate fault)))

(deftest room-close-joins-held-pin-io-and-job-cleanup-before-closing-context
  (checkpoint-fixture
   (fn [{:keys [host room principal handle input]}]
     (let [pin! roots/pin! allocate! resource/allocate-run!
           entered (promise) cancelled (promise) release (CountDownLatch. 1)
           allocations (atom 0) starts (atom 0) worker (atom nil) closing (atom nil)]
       (try
         (with-redefs [work/prepare (fn [input]
                                      (assoc (@#'model-free/model-free-preparation input)
                                             :start! (fn [_] (swap! starts inc) (spin :wrong))))
                       resource/allocate-run! (fn [& args] (swap! allocations inc) (apply allocate! args))
                       roots/pin! (fn [db options mode]
                                    (let [id (pin! db options mode)]
                                      (when (get-in options [:owner :simmis/work-request])
                                        (deliver entered true)
                                        ;; Model a backend that swallows cancellation:
                                        ;; teardown must wait for physical completion.
                                        (loop []
                                          (when-not (try (.await release) true
                                                         (catch InterruptedException _ (deliver cancelled true) false))
                                            (recur))))
                                      id))]
           (reset! worker (threads/future
                            (try (admission/submit! host principal handle input) :unexpected
                                 (catch Exception _ :held))))
           (is (true? (deref entered 15000 false)))
           (let [leases (vec (vals (get @(:meta room) preparation/leases-key)))]
             (is (= 1 (count leases)))
             (reset! closing (threads/future
                               (try (discourse/close-room! room) :closed
                                    (catch Exception _ :held))))
             (is (true? (deref cancelled 5000 false)))
             (is (not (realized? @closing)))
             (is (not (realized? @worker)))
             (doseq [lease leases]
               (is (= :dvergr.runtime.preparation/unconfirmed-drain
                      (try ((:join! lease) 0) nil
                           (catch clojure.lang.ExceptionInfo error (:type (ex-data error)))))))
             (.countDown release)
             (is (= :held (deref @worker 15000 :timeout)))
             (is (= :closed (deref @closing 15000 :timeout)))
             (is (zero? @allocations))
             (is (zero? @starts))
             (doseq [lease leases] (is (nil? ((:join! lease) 1000))))))
         (finally
           (.countDown release)
           (when @worker (deref @worker 15000 nil))
           (when @closing (deref @closing 15000 nil))))))))

(deftest checkpoint-provenance-precedes-preparation-and-is-preserved-by-wrappers
  (let [basis stream-fixture/basis
        selected {:version 1 :policy :request-evidence-v1 :basis basis
                  :target (:target (pin-fixture/origin basis))
                  :origin {:version 1 :kind :datahike :commit (:commit basis)
                           :source (select-keys basis [:store :repository])}}
        system (get-in selected [:target :runtime-system])
        calls (atom 0)
        base (evaluation/make-world-setup {:id :checkpoint/test :basis :fixed
                                           :prepare (fn [_] (swap! calls inc) :prepared)})
        setup (workspace/with-checkpoint base selected)
        expected {system (:origin selected)}]
    (is (= expected (:setup/checkpoints (evaluation/world-setup-ref setup))))
    (doseq [wrapped [(workspace/with-publication-target setup (:target selected))
                     (workspace/with-frozen-staging setup)]]
      (is (= expected (:setup/checkpoints (evaluation/world-setup-ref wrapped)))))
    (doseq [entry [nil {:kind :overlay} {:kind :branch :checkpoint (:origin selected)}
                   {:kind :overlay :checkpoint (assoc (:origin selected) :commit (random-uuid))}]]
      (with-redefs [discourse/fork-descriptor (fn [_] {:fork/systems {system entry}})]
        (is (thrown? clojure.lang.ExceptionInfo ((:prepare setup) {:room :synthetic-proof-only}))))
      (is (zero? @calls)))
    (with-redefs [discourse/fork-descriptor (fn [_] {:fork/systems {system {:kind :overlay :checkpoint (:origin selected)}}})]
      (is (= :prepared ((:prepare setup) {:room :synthetic-proof-only}))))
    (is (= 1 @calls))))
