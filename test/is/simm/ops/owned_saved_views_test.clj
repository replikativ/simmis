(ns is.simm.ops.owned-saved-views-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.attempt :as attempt]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.roster :as roster]
            [dvergr.artifact :as artifact]
            [dvergr.room.store :as store]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.capture :as capture]
            [is.simm.documents.capture-test :as capture-data]
            [is.simm.documents.capture-views :as capture-views]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.saved-view-reader :as reader]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-charts-test :as fixture]
            [is.simm.ops.owned-saved-views :as views]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-fixture [f]
  (@#'fixture/with-fixture
   (fn [{:keys [owned] :as context}]
     (let [descriptor {:version 1 :id (random-uuid) :source {:kind :pages :roots ["page-view-data"]}
                       :presentation :table :columns [{:kind :title}]}]
       ;; The inherited attachment fixture intentionally has provisional
       ;; wiki/file.md. Keep it outside this exact valid source selection.
       (repo/write! owned "page-view-data/sales.md"
                    (markdown/source-bytes (str "---\nsimmis-id: " (random-uuid) "\n---\n# Sales\n")))
       (repo/write! owned "views/report.md"
                    (markdown/source-bytes (str "# Saved table\n\n```simmis-view\n" (pr-str descriptor) "\n```\n")))
       (f context)))))

(defn- request [{:keys [host w owned]}]
  (let [binding (reads/resolve-host-read! host (ops/party-principal (:member w)) (str (:room w)))
        file (files/read-file-in @owned "views/report.md")
        fence (first (get-in file [:analysis :saved-views]))]
    (merge {:path "views/report.md" :expected (:entry file) :revision (get-in file [:analysis :revision])
            :expected-target-stamp (:target-stamp binding)} (select-keys fence [:from :to :view]))))

(defn- read! [{:keys [host w]} request]
  (views/read! host (ops/party-principal (:member w)) (str (:room w)) request))

(defn- seed-scorecard! [room]
  ;; Native certified record fixture, not a model trial or a benchmark result
  ;; claimed to have executed. Exercise the same managed blob/projection reader.
  (binding [ec/*execution-context* (:ctx room)]
    (let [agent (-> (roster/make-roster)
                    (roster/make-agent {:id :report/echo :program {:kind :echo}})
                    (roster/agent :report/echo))
          world (environment/make-environment
                 {:id :report/case :task "fixture" :verifier {:id :report/exact :version 1}
                  :world {:isolation :ctx :settlement :review}})
          definition (experiment/make-experiment
                      {:id :report/fixture :dataset (experiment/make-dataset {:id :report/data :environments [world]})
                       :candidates [agent] :repetitions 1})
          run (random-uuid) now (java.util.Date. 1000)
          receipt (environment/make-attempt-receipt
                   world {:run-id run :provider :dvergr :model "echo" :status :completed
                          :started-at 1000 :elapsed-ms 10
                          :metrics {:program-kind :echo :model-resolution :not-applicable
                                    :agent-version 1 :agent-def-hash (hasch/uuid agent) :interpreter-version 5}
                          :checks {:exact? true} :reward 1.0
                          :trace {:runs [{:run/id run :run/status :completed}]}})
          value (attempt/make-attempt world agent receipt
                                      {:trace {:runs [{:run/id run :run/status :completed}]}} :review)
          candidate (first (:experiment/candidates definition))
          scorecard (experiment/make-scorecard
                     definition [{:experiment/job {:candidate/id (:candidate/id candidate)
                                                   :candidate/agent (:candidate/agent candidate)
                                                   :candidate/agent-content-id (:candidate/agent-content-id candidate)
                                                   :environment (environment/environment-ref world) :repetition 0}
                                  :attempt value}])
          st (:store room) cid (store/conversation-id room)]
      (store/-store-run! st cid {:run/id run :run/kind :agent-task :run/room cid :run/actor (:agent/id agent)
                                :run/trigger (random-uuid) :run/status :completed :run/created-at now
                                :run/started-at now :run/updated-at now :run/ended-at (java.util.Date. 1010)
                                :run/agent-version 1 :run/program-kind :echo :run/interpreter-version 5
                                :run/agent-def-hash (hasch/uuid agent)})
      (store/-store-attempt! st cid value)
      (store/-store-scorecard! st cid scorecard)
      (:scorecard/content-id scorecard))))

(defn- scorecard-request! [{:keys [conn w room owned] :as context} id]
  (let [scope (binding [ec/*execution-context* (:ctx room)] (get-in @(:conn (:store room)) [:config :store :id]))
        descriptor {:version 1 :id (random-uuid) :source {:kind :scorecard :room :current :scorecard id}
                    :presentation :table :columns [{:kind :candidate} {:kind :pass-rate} {:kind :cost}]}]
    (d/transact conn [{:room/id (:room w) :room/content-db-scope scope}])
    (repo/write! owned "views/report.md"
                 (markdown/source-bytes (str "# Native fixture\n\n```simmis-view\n" (pr-str descriptor) "\n```\n")))
    (request context)))

(deftest saved-scorecard-is-a-current-room-bounded-native-read-with-no-attempt-payloads
  (with-fixture
    (fn [{:keys [room conn w owned] :as context}]
      ;; Missing and successful immutable reads share the genuine native owner.
      ;; Build a fresh request after changing the source descriptor.
      (let [sent (scorecard-request! context (random-uuid))]
        (with-redefs [experiment/scorecard (fn [& _] (throw (AssertionError. "Unbounded fallback")))]
          (is (= :scorecard-not-found (:reason (read! context sent))))))
      (let [id (seed-scorecard! room) sent (scorecard-request! context id)
            before @owned result
            (with-redefs [artifact/get-value (fn [& _] (throw (AssertionError. "Unbounded artifact read")))
                          store/-load-attempt (fn [& _] (throw (AssertionError. "Attempt payload read")))]
              (read! context sent))]
        (is (= :ready (:status result)))
        (is (= ["report/echo"] (mapv :candidate (:rows result))))
        (is (= "100" (get-in result [:rows 0 :cells 1 :text])))
        (is (= :null (get-in result [:rows 0 :cells 2 :kind])) "unfunded fixture has unknown cost")
        (is (= (:room w) (get-in result [:source :room])))
        (is (= id (get-in result [:source :scorecard])))
        (is (every? #(not-any? (set (keys %)) [:path :edit :receipt]) (:rows result)))
        (is (= before @owned))
        (d/transact conn [{:room/id (:room w) :room/content-db-scope (random-uuid)}])
        (with-redefs [experiment/scorecard-bounded
                      (fn [& _] (throw (AssertionError. "Mismatched store reached payload reader")))]
          (is (thrown? clojure.lang.ExceptionInfo (read! context sent))))))))

(deftest changed-selected-capture-notes-cannot-return-an-earlier-archive-table
  (with-fixture
    (fn [{:keys [owned] :as context}]
      (let [note (capture/observation-note capture-data/session capture-data/segment [])
            descriptor {:version 1 :id (random-uuid)
                        :source {:kind :archived-captures
                                 :roots [(str (capture/folder capture-data/session) "/observations")]}
                        :presentation :table :columns [{:kind :started-at} {:kind :note}]}
            prepare capture-views/prepare calls (atom 0)]
        (repo/write! owned (:path note) (markdown/source-bytes (:text note)))
        (repo/write! owned "views/report.md"
                     (markdown/source-bytes (str "```simmis-view\n" (pr-str descriptor) "\n```\n")))
        (let [sent (request context) result (read! context sent)]
          (is (= :ready (:status result)))
          (is (= :archived-captures (get-in result [:source :kind])))
          (with-redefs [capture-views/prepare
                        (fn [db declaration]
                          (let [result (prepare db declaration)]
                            (when (= 1 (swap! calls inc))
                              (repo/write! owned (:path note) (markdown/source-bytes (str (:text note) "\nChanged evidence\n"))))
                            result))]
            (is (thrown? clojure.lang.ExceptionInfo (read! context sent)))
            (is (= 2 @calls))))))))

(deftest receipt-read-finishes-after-native-io-with-current-authority-and-full-control-basis
  (with-fixture
    (fn [{:keys [room conn w owned attached-descriptor] :as context}]
      (let [id (seed-scorecard! room) sent (scorecard-request! context id)
            bounded artifact/get-bounded-value validate owned/validate! calls (atom 0)]
        (with-redefs [artifact/get-bounded-value
                      (fn [& args]
                        (swap! calls inc)
                        (let [result (apply bounded args)]
                          (repo/write! owned "unrelated.txt" (markdown/source-bytes "New unrelated source"))
                          result))]
          (is (= :ready (:status (read! context sent))))
          (is (= 1 @calls) "unrelated Memory writes never reread the immutable Scorecard"))
        (with-redefs [artifact/get-bounded-value
                      (fn [& args]
                        (let [result (apply bounded args)]
                          (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                          result))]
          (is (thrown? clojure.lang.ExceptionInfo (read! context sent))))
        (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:member w)]]}])
        (reset! calls 0)
        (with-redefs [owned/validate!
                      (fn [& args]
                        (let [result (apply validate args)]
                          (when (= 2 (swap! calls inc))
                            ;; The owned mapping itself did not change. A new
                            ;; alias could be installed in another system, so
                            ;; the final guard conservatively fences ALL late
                            ;; registry changes after physical validation.
                            (d/transact conn [{:system/id (:registry-system attached-descriptor)
                                               :system/scope "/changed-after-last-source-io"}]))
                          result))]
          (is (thrown? clojure.lang.ExceptionInfo (read! context sent)))
          (is (= 2 @calls)))))))

(deftest lease-cancellation-during-the-last-receipt-store-observation-is-not-lost
  (with-fixture
    (fn [{:keys [room] :as context}]
      (let [id (seed-scorecard! room) sent (scorecard-request! context id)
            inspect @#'views/receipt-context calls (atom 0)
            entered (CountDownLatch. 1) release (CountDownLatch. 1)]
        (with-redefs-fn
          {#'views/receipt-context
           (fn [& args]
             (let [result (apply inspect args)]
               (when (= 2 (swap! calls inc))
                 (.countDown entered)
                 (loop []
                   (let [outcome (try (.await release 10 TimeUnit/SECONDS)
                                      (catch InterruptedException _ :interrupted))]
                     (case outcome :interrupted (recur) true nil
                           (throw (ex-info "Final store observation did not resume" {}))))))
               result))}
          (fn []
            (let [worker (future (try (read! context sent) :unexpected-return
                                     (catch Exception _ :refused)))]
              (try
                (let [entered? (.await entered 10 TimeUnit/SECONDS)]
                  (is entered?)
                  (when entered?
                    (let [leases (binding [ec/*execution-context* (:ctx room)]
                                   (vec (vals (get @(:meta room) preparation/leases-key))))]
                      (is (seq leases))
                      (doseq [lease leases] (preparation/cancel! lease)))))
                (finally (.countDown release)))
              (is (= :refused (deref worker 10000 :timeout))))))))))

(deftest page-view-is-human-read-only-and-uses-the-owned-not-selected-source
  (with-fixture
    (fn [{:keys [host w owned attached hydrations owner] :as context}]
      (let [sent (request context) before @owned other @attached result (read! context sent)]
        (is (= :ready (:status result)))
        (is (= ["page-view-data/sales.md"] (mapv :path (:rows result))))
        (is (= (:expected-target-stamp sent) (:target-stamp result)))
        (is (= before @owned)) (is (= other @attached))
        (is (not-any? #(contains? result %) [:db :conn :room :system :compiled :lease :control-db]))
        (let [count-before (count @hydrations)]
          (doseq [principal [(ops/party-principal (:non-member w))
                            (assoc (ops/party-principal (:member w)) :principal/kind :agent)]]
            (is (thrown? clojure.lang.ExceptionInfo (views/read! host principal (str (:room w)) sent))))
          (is (= count-before (count @hydrations))))
        (lifetime/fence! owner)
        (is (thrown? clojure.lang.ExceptionInfo (read! context sent)))))))

(deftest authority-and-source-races-are-checked-after-table-acquisition
  (with-fixture
    (fn [{:keys [conn w owned attached-descriptor] :as context}]
      (let [sent (request context) inspect reader/prepare-pages-in]
        (doseq [change! [(fn [] (repo/write! owned "page-view-data/new.md"
                                           (markdown/source-bytes (str "---\nsimmis-id: " (random-uuid) "\n---\nNew source\n"))))
                         (fn [] (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]]))
                         (fn [] (d/transact conn [{:room/id (:room w) :room/default-repository
                                                   [:system/id (:registry-system attached-descriptor)]}]))]]
          (let [calls (atom 0)]
            (with-redefs [reader/prepare-pages-in (fn [db query]
                                             (let [result (inspect db query)]
                                               (when (= 1 (swap! calls inc)) (change!))
                                               result))]
              (is (thrown? clojure.lang.ExceptionInfo (read! context sent)))))
          ;; Restore membership for the following independent gate; no grant
          ;; or publication is installed by this fixture action.
          (d/transact conn [{:room/id (:room w) :room/parties [[:party/id (:member w)]]}]))))))

(deftest physical-body-read-retains-the-original-app-until-actual-exit
  (with-fixture
    (fn [{:keys [owner] :as context}]
      (let [sent (request context) original files/read-file-in
            entered (CountDownLatch. 1) release (CountDownLatch. 1) first? (atom true)]
        (with-redefs [files/read-file-in
                      (fn [db path]
                        (when (and (= "views/report.md" path) (compare-and-set! first? true false))
                          (.countDown entered)
                          (when-not (.await release 10 TimeUnit/SECONDS)
                            (throw (ex-info "Test body read did not resume" {}))))
                        (original db path))]
          (let [worker (future (read! context sent))]
            (try
              (is (.await entered 10 TimeUnit/SECONDS))
              (lifetime/fence! owner)
              (is (thrown? clojure.lang.ExceptionInfo (lifetime/join! owner 20)))
              (is (not (realized? worker)))
              (finally (.countDown release)))
            (is (= :ready (:status (deref worker 10000 {:status :timeout}))))
            (is (nil? (lifetime/join! owner 5000)))))))))

(deftest final-metadata-gates-follow-the-last-body-read-without-reread-loops
  (doseq [change [:revoke :default :room-cancel :third-source]]
    (with-fixture
      (fn [{:keys [conn w room owned attached-descriptor] :as context}]
        (let [sent (request context) inspect reader/prepare-pages-in calls (atom 0)
              entered (CountDownLatch. 1) release (CountDownLatch. 1)]
          (with-redefs [reader/prepare-pages-in
                        (fn [database query]
                          (let [n (swap! calls inc) result (inspect database query)]
                            (case n
                              1 (repo/write! owned "unrelated.txt" (markdown/source-bytes "Force one revalidation"))
                              2 (do (.countDown entered)
                                    (loop []
                                      (let [outcome (try (.await release 10 TimeUnit/SECONDS)
                                                         (catch InterruptedException _ :interrupted))]
                                        ;; Physical IO may absorb cancellation.
                                        ;; Its valid result must still be gated.
                                        (case outcome :interrupted (recur) true nil
                                              (throw (ex-info "Test read did not resume" {}))))))
                              (throw (AssertionError. "No third body-read loop is permitted")))
                            result))]
            (let [worker (future (try (read! context sent) {:status :unexpected-return}
                                     (catch Exception _ {:status :refused})))]
              (try
                (let [entered? (.await entered 10 TimeUnit/SECONDS)]
                  (is entered?)
                  (when entered?
                    (case change
                      :revoke (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                      :default (d/transact conn [{:room/id (:room w) :room/default-repository
                                                  [:system/id (:registry-system attached-descriptor)]}])
                      :third-source (repo/write! owned "another-unrelated.txt" (markdown/source-bytes "Newer version"))
                      :room-cancel
                      (let [leases (binding [ec/*execution-context* (:ctx room)]
                                     (vec (vals (get @(:meta room) preparation/leases-key))))]
                        (is (seq leases))
                        (doseq [lease leases] (preparation/cancel! lease))))))
                (finally (.countDown release)))
              (is (= {:status :refused} (deref worker 10000 {:status :timeout})))
              (is (= 2 @calls)))))))))
