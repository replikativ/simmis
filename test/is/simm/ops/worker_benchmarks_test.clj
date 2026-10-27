(ns is.simm.ops.worker-benchmarks-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.room.store.memory :as memory]
            [is.simm.authority.world :as world]
            [is.simm.model.workers :as workers]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.benchmark-fixture :as native-fixture]
            [is.simm.ops.worker-benchmarks :as local-benchmarks]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-files-test :as file-fixture]
            [is.simm.ops.worker-service :as service]
            [is.simm.ops.worker-work :as local]
            [is.simm.ops.worker-work-test :as fixture]
            [is.simm.ops.workflows :as workflows]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent LinkedBlockingQueue TimeUnit]))

(def source
  {"workflow.edn" "{:title \"Local benchmark\" :task \"Read /docs/input.md and write /out/result.md\" :capture [\"/out\"]}"
   "checker.clj" "(ns local-checker) (defn check [{:keys [files gold]}] (let [ok (= (:expected gold) (get files \"/out/result.md\"))] {:checks {:answer? ok} :reward (if ok 1.0 0.0)}))"
   "gold.edn" "{:expected \"TOP-SECRET\"}"
   "cases/a/gold.edn" "{:expected \"a\"}"
   "cases/a/fixtures/docs/input.md" "a"
   "cases/b/gold.edn" "{:expected \"b\"}"
   "cases/b/fixtures/docs/input.md" "b"})

(defn input [worker]
  {:request-id (random-uuid) :worker worker :name "echo"
   :expected-bundle (:id (catalog/bundle "echo" source)) :repetitions 1
   :limits (assoc file-fixture/limits :settlement "discard")})

(deftest local-worker-benchmarks-cannot-bypass-evaluation-splits
  (let [source (assoc source "benchmark.edn"
                      "{:version 1 :splits {:development [\"a\"] :held-out [\"b\"]}}")
        request (assoc (input (random-uuid)) :expected-bundle (:id (catalog/bundle "echo" source)))
        prepare #(local-benchmarks/prepare {:files source} % file-fixture/config
                                           {:simmis.worker/generation 1 :simmis.worker/config-id (random-uuid)})]
    (is (thrown? clojure.lang.ExceptionInfo (prepare request)))
    (is (thrown? clojure.lang.ExceptionInfo (prepare (assoc request :split "development" :cases ["b"]))))
    (let [result (prepare (assoc request :split "development"))]
      (is (= {:adapter :local-sci-worker :program :protocol :model (:model file-fixture/config)}
             (get-in result [:plan :experiment :experiment/metadata :simmis/execution])))
      (is (= #{"a"} (set (map :case (vals (get-in result [:basis :frozen]))))))
      (is (= ["a"] (get-in result [:plan :experiment :experiment/metadata :simmis/evaluation :cases])))
      (is (= "development" (get-in result [:plan :experiment :experiment/metadata :simmis/evaluation :split]))))))

(deftest preparation-keeps-private-benchmark-material-out-of-all-worker-inputs
  (let [request (input (random-uuid))
        snapshot {:files source}
        prepared (local-benchmarks/prepare snapshot request file-fixture/config
                                           {:simmis.worker/generation 1 :simmis.worker/config-id (random-uuid)})]
    (is (= 2M (get (:resources prepared) local/admission-unit)))
    (is (= 2 (count (get-in prepared [:basis :frozen]))))
    (doseq [[_ {:keys [frozen]}] (get-in prepared [:basis :frozen])]
      (is (= #{"docs/input.md"} (set (keys (:files frozen)))))
      (is (files/validate-frozen! frozen)))
    (is (= :local-worker-benchmark (:request-kind prepared)))
    (doseq [bad [(assoc request :models ["forbidden"]) (assoc request :params {})
                 (assoc request :cases ["a" "a"]) (assoc request :cases ["unknown"])
                 (assoc-in request [:limits :settlement] "review")]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (local-benchmarks/prepare snapshot bad file-fixture/config {}))))))

(deftest omitted-cases-never-expands-past-the-four-case-bound
  (let [source (reduce (fn [files name]
                         (assoc files (str "cases/" name "/gold.edn") "{:expected \"x\"}"
                                (str "cases/" name "/fixtures/docs/input.md") "x"))
                       source ["c" "d" "e"])
        request (assoc (input (random-uuid)) :expected-bundle (:id (catalog/bundle "echo" source)))]
    (is (thrown? clojure.lang.ExceptionInfo
                 (local-benchmarks/prepare {:files source} request file-fixture/config {})))
    (is (= 4 (count (get-in (local-benchmarks/prepare {:files source}
                                                      (assoc request :cases ["a" "b" "c" "d"])
                                                      file-fixture/config {}) [:basis :frozen]))))))

(deftest failed-fixture-retirement-retains-and-releases-its-exact-physical-permit
  (let [slot (java.util.concurrent.Semaphore. 1) calls (atom 0) id (random-uuid) job (random-uuid)]
    (with-redefs-fn {#'benchmarks/checker-slot slot
                     #'benchmarks/pending-retirements (atom {})
                     #'native-fixture/validate-receipt! (fn [_ _ receipt] receipt)
                     #'native-fixture/create! (fn [& _] {:room {:ctx ec/*execution-context*} :persistent-id (random-uuid)})
                     #'benchmarks/record-fixture! (fn [& _] nil)
                     #'benchmarks/recorded-fixture! (fn [& _] :retired)
                     #'native-fixture/retire! (fn [& _] (when (= 1 (swap! calls inc)) (throw (ex-info "Retirement failed" {}))))
                     #'jobs/status (fn [_] {:settled? true :status :completed})}
      (fn []
        (let [adapter (benchmarks/adapter nil ec/*execution-context* ::room {:request-kind :local-worker-benchmark} {} nil)
              row {:work.request/id id :work.request/job job :work.request/kind :local-worker-benchmark
                   :work.request/return-pending? false}]
          ((:job-context! adapter) row)
          ((:admit! adapter) job)
          (is (zero? (.availablePermits slot)))
          (is (thrown? clojure.lang.ExceptionInfo ((:settled! adapter) job)))
          (is (zero? (.availablePermits slot)))
          (benchmarks/recover-request! nil ec/*execution-context* row)
          (is (= 1 (.availablePermits slot)))
          (is (= 2 @calls))
          (benchmarks/recover-request! nil ec/*execution-context* row)
          (is (= 1 (.availablePermits slot))))))))

(deftest failed-server-calibration-fences-prepared-cells-without-worker-admission
  (world/with-world [conn w]
    (@#'fixture/with-worker
     conn w
     (fn [{:keys [room host-ctx principal enrolled allowance]}]
       (let [clean (discourse/make-room {:id (keyword (str "calibration-fixture-" (random-uuid))) :store (memory/make)})
             files (assoc source "calibration.edn"
                          "{:case \"a\" :reference {\"/out/result.md\" \"wrong\"} :damaged {:bad {:files {\"/out/result.md\" \"also wrong\"} :loses [:answer?]}}}")
             request (assoc (input (:worker enrolled)) :expected-bundle (:id (catalog/bundle "echo" files)))]
         (try
           (binding [ec/*execution-context* (:ctx clean)] (workspace/ensure-workspace! clean))
           (workflows/write! room principal {:name "echo" :files files})
           (d/transact conn [{:party/id (:owner w) :party/role :admin}])
           (local/provision! conn host-ctx principal (:room w) room allowance)
           (with-redefs-fn {#'benchmarks/checker-slot (java.util.concurrent.Semaphore. 1)
                            #'native-fixture/validate-receipt! (fn [_ _ receipt] receipt)
                            #'native-fixture/create! (fn [& _] {:room clean :persistent-id (random-uuid)})
                            #'native-fixture/retire! (fn [& _])}
             (fn []
               (let [submitted (local-benchmarks/submit! conn host-ctx principal (:room w) room request)]
                 (is (:settled? (jobs/status (:job submitted) 30000)))
                 (let [rows (d/q '[:find [(pull ?e [*]) ...] :in $ ?job
                                   :where [?e :simmis.worker.assignment/job ?job]] @conn (:job submitted))]
                   (is (= 2 (count rows)))
                   (is (every? #(= :withdrawn (:simmis.worker.assignment/state %)) rows))
                   (is (not-any? :simmis.worker.assignment/lease rows)))
                 (is (= 2M (get (resource/balance room) local/admission-unit)))
                 (is (empty? (store/-list-scorecards (:store room) (store/conversation-id room) {}))))))
           (finally (discourse/close-room! clean))))))))

(deftest existing-experiment-runner-certifies-two-local-cases-with-server-only-gold
  (world/with-world [conn w]
    (@#'fixture/with-worker
     conn w
     (fn [{:keys [room host-ctx principal enrolled allowance]}]
       (let [clean (discourse/make-room {:id (keyword (str "local-benchmark-clean-" (random-uuid)))
                                         :store (memory/make)})
             retired (atom 0) queued (LinkedBlockingQueue.) listener (random-uuid)
             request (input (:worker enrolled))]
         (try
           (binding [ec/*execution-context* (:ctx clean)] (workspace/ensure-workspace! clean))
           (workflows/write! room principal {:name "echo" :files source})
           (d/transact conn [{:party/id (:owner w) :party/role :admin}])
           (local/provision! conn host-ctx principal (:room w) room allowance)
           (d/listen conn listener
                     (fn [report]
                       (doseq [datom (:tx-data report)
                               :when (and (:added datom) (= :simmis.worker.assignment/state (:a datom))
                                          (= :queued (:v datom)))]
                         (.offer queued (:simmis.worker.assignment/id (d/pull (:db-after report) '[*] (:e datom)))))))
           (with-redefs-fn {#'benchmarks/checker-slot (java.util.concurrent.Semaphore. 1)
                            #'native-fixture/validate-receipt! (fn [_ _ receipt] receipt)
                            #'native-fixture/create! (fn [& _] {:room clean :persistent-id (random-uuid)})
                            #'native-fixture/retire! (fn [& _] (swap! retired inc))}
             (fn []
               (let [submitted (local-benchmarks/submit! conn host-ctx principal (:room w) room request)
                     ids (mapv (fn [_]
                                 (let [id (.poll queued 30 TimeUnit/SECONDS)
                                       row (workers/assignment conn id)
                                       frozen (workers/payload conn row :simmis.worker.assignment/input-ref)
                                       text (String. (files/file-bytes! (get-in frozen [:files "docs/input.md"])) "UTF-8")
                                       lease (@#'fixture/claim! conn enrolled id)]
                                   (is (uuid? id))
                                   (is (uuid? (:simmis.worker.assignment/cell row)))
                                   (is (= #{"docs/input.md"} (set (keys (:files frozen)))))
                                   (service/complete! conn lease {:run (str id)
                                                                  :output {:files {"out/result.md" (files/text-file (if (= text "a") "a" "wrong") 0x81A4)}
                                                                           :summary "candidate answer" :exit-code 0}})
                                   id)) (range 2))]
                 (is (= 2 (count (set ids))))
                 (is (:settled? (jobs/status (:job submitted) 40000)))
                 (let [attempts (store/-list-attempts (:store room) (store/conversation-id room) {})
                       scorecards (store/-list-scorecards (:store room) (store/conversation-id room) {})]
                   (is (= #{0.0 1.0} (set (map #(get-in % [:attempt/receipt :attempt/reward]) attempts)))
                       (pr-str (mapv #(select-keys % [:attempt/receipt :attempt/evidence]) attempts)))
                   (is (= 2 (count attempts)))
                   (is (= 1 (count scorecards)))
                   (is (every? #(false? (get-in % [:attempt/receipt :attempt/metrics :spend :priced?])) attempts)))
                 (is (= 1 @retired))
                 (is (zero? (get (resource/balance room) local/admission-unit 0M)))
                 (is (= {} (workspace/read-tree room "/out")))
                 (workers/revoke! conn (:worker enrolled))
                 (with-redefs [workflows/snapshot (fn [& _] (throw (AssertionError. "No retry reread")))]
                   (is (= (:job submitted) (:job (local-benchmarks/submit! conn host-ctx principal (:room w) room request))))))))
           (finally
             (d/unlisten conn listener)
             (discourse/close-room! clean))))))))
