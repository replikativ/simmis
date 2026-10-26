(ns is.simm.ops.benchmark-case-evidence-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.blob :as blob]
            [dvergr.catalog.workspace :as fixtures]
            [is.simm.model.benchmark-resolutions :as journal]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-case-evidence :as evidence]
            [is.simm.ops.benchmark-case-manifest :as manifest]
            [is.simm.ops.benchmark-source :as source]
            [is.simm.ops.benchmarks-test :as benchmarks]
            [is.simm.ops.workflows-test :as workflows]))

(defn- fixture []
  (let [files (assoc workflows/fixture
                     "cases/a/gold.edn" "{:expected :a}" "cases/b/gold.edn" "{:expected :b}"
                     "cases/a/fixtures/only-a.md" "Selected A"
                     "cases/b/fixtures/only-b.md" "Private B")
        input (assoc (benchmarks/input files) :cases ["a" "b"])
        retained (source/retain "echo" files)
        plan (:plan (source/compile-plan retained (:basis (benchmarks/prepared files input))))
        request {:work.request/by (random-uuid) :work.request/room (random-uuid)
                 :work.request/client-id (:request-id input) :work.request/kind :benchmark
                 :work.request/job (random-uuid) :work.request/claim (random-uuid)
                 :work.request/intent-id (random-uuid) :work.request/state :submitted}
        request (assoc request :work.request/id
                       (requests/request-id (:work.request/by request) (:work.request/room request)
                                            (:work.request/client-id request)))
        id (random-uuid)
        resolution (merge (journal/request-binding request)
                          {:benchmark.resolution/id id :benchmark.resolution/claim (random-uuid)
                           :benchmark.resolution/plan-id (journal/plan-id plan)
                           :benchmark.resolution/semantic-id (:semantic-id plan)
                           :benchmark.resolution/state :preparing})
        row {:benchmark.case/id (journal/case-id id 0) :benchmark.case/resolution id
             :benchmark.case/ordinal 0 :benchmark.case/root (random-uuid)
             :benchmark.case/claim (random-uuid) :benchmark.case/state :planned}
        basis {:store (random-uuid) :repository (random-uuid) :configuration (random-uuid)
               :commit (random-uuid) :backend :memory :branch :baseline :ref "main"
               :datoms 1 :max-tx 1}
        selection {:version 1 :policy :request-evidence-v1 :basis basis
                   :origin {:version 1 :kind :datahike :commit (:commit basis)
                            :source (select-keys basis [:store :repository])}
                   :target (merge (select-keys basis [:store :repository :backend :branch :ref])
                                  {:registry-system (random-uuid) :runtime-system "private-fixture"})}
        baseline (manifest/after-seeding {:version 1 :files {} :directories ["/"]}
                                         {"/AGENTS.md" "Trusted template"} fixtures/fixture-seed-limits)]
    {:request request :resolution resolution :plan plan :row row :source retained
     :c0 {:selection selection :manifest baseline} :selection selection}))

(defn- body [{:keys [request resolution plan row source c0 selection]}]
  (evidence/case-body request resolution plan row source c0 selection))

(defn- validate [body {:keys [request resolution plan row source c0]}]
  (evidence/validate-case! body request resolution plan row source c0))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(deftest regenerated-evidence-keeps-the-template-and-only-selected-inputs
  (let [f (fixture) value (body f)]
    (is (= value (validate value f)))
    (is (= ["/AGENTS.md" "/docs/input.md" "/only-a.md"] (vec (keys (:files (:manifest value))))))
    (is (= (blob/blob-id (.getBytes "Selected A" "UTF-8"))
           (get-in value [:manifest :files "/only-a.md" :content])))
    (is (= {:kind :named :name "a"} (get-in value [:case :source-case])))
    (is (= (manifest/template-id (get-in f [:c0 :manifest])) (get-in value [:template :id])))
    (is (= (:work.request/job (:request f)) (:job (evidence/root-owner value))))
    (is (= (evidence/content-id value) (:evidence (evidence/root-owner value))))
    (is (= (get-in f [:row :benchmark.case/root]) (:simmis/benchmark-case-root (evidence/root-owner value))))))

(deftest lifecycle-progress-does-not-change-historical-evidence
  (let [f (fixture) value (body f)
        settled (-> f (assoc-in [:request :work.request/state] :settled)
                    (assoc-in [:resolution :benchmark.resolution/state] :ready)
                    (assoc-in [:row :benchmark.case/state] :retained))]
    (is (= value (body settled)))
    (is (= value (validate value settled)))))

(deftest self-consistent-outer-hashes-do-not-certify-changed-evidence
  (let [f (fixture) value (body f)]
    (doseq [forged [(assoc value :extra true)
                    (update-in value [:manifest :files] dissoc "/AGENTS.md")
                    (assoc-in value [:case :root] (random-uuid))
                    (assoc-in value [:case :source-case] {:kind :named :name "b"})
                    (assoc-in value [:program-id] (random-uuid))]]
      (is (uuid? (evidence/content-id forged)))
      (is (= :case-integrity (reason #(validate forged f)))))))

(deftest original-input-and-private-workspace-correspondence-is-required
  (let [f (fixture)]
    (doseq [changed [(assoc-in f [:request :work.request/id] (random-uuid))
                     (assoc-in f [:request :work.request/kind] :workflow)
                     (assoc-in f [:row :benchmark.case/id] (random-uuid))
                     (assoc-in f [:row :benchmark.case/ordinal] 2)
                     (assoc-in f [:resolution :benchmark.resolution/claim] nil)
                     (assoc f :source (source/retain "echo" (assoc (get-in f [:source :files])
                                                                   "gold.edn" "{:expected :changed}")))]]
      (is (= :case-binding (reason #(body changed)))))
    (is (= :private-workspace-changed
           (reason #(body (assoc-in f [:selection :target :runtime-system] "author-room")))))
    (is (= :template-shape (reason #(body (assoc-in f [:c0 :extra] true)))))
    (let [changed-plan (assoc-in (:plan f) [:cases 0 :manifest-id] (random-uuid))
          changed (-> f (assoc :plan changed-plan)
                      (assoc-in [:resolution :benchmark.resolution/plan-id] (journal/plan-id changed-plan)))]
      (is (= :source-manifest (reason #(body changed)))))))
