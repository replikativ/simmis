(ns is.simm.ops.shipped-wiki-schedule-test
  "Shipped source through ordinary scheduling; scripted transport, not model quality.\n   Recovery here is in-process reconciliation, not a fresh-process restart."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [dvergr.agent.attempt :as attempt]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.discourse :as discourse]
            [dvergr.system.rooms :as native-rooms]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-inputs :as knowledge]
            [is.simm.ops.knowledge-output-service :as output]
            [is.simm.ops.file-output-storage :as retained]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.owned-schedule-fixture :as fixture]
            [is.simm.ops.owned-schedule-occurrences-test :as occurrence-fixture]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflow-runs-test :as saved]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.application-ops-test :as app-fixture]
            [is.simm.starter.wiki-maintenance :as starter]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- final-response
  ([] (final-response "No supported wiki change; no output."))
  ([text]
  {:status 200
   :body (java.io.ByteArrayInputStream.
           (.getBytes
             (apply str
                    (map #(str "data: " (json/write-value-as-string %) "\n\n")
                         [{:type "response.output_text.delta" :delta text}
                          {:type "response.completed"
                           :response {:id "maintainer-scripted" :model "gpt-5.6-sol"
                                      :usage {:input_tokens 10 :output_tokens 10}}}]))
             "UTF-8"))}))

(defn- text! [conn path text] (repo/write! conn path (.getBytes ^String text "UTF-8")))
(defn- occurrence [conn row]
  (model/occurrence conn (:schedule.activation/last-occurrence
                          (model/activation conn (:schedule.activation/id row)))))

(deftest historical-whole-selection-maintainer-is-an-ordinary-scheduled-noop
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host owned attached room]}]
     (d/transact conn model/schema)
     (d/transact conn [{:party/id (:owner w) :party/role :admin}])
     ;; Retain the historical whole-selection cursor regression independently
     ;; of the current starter's incremental stream protocol.
     (let [source (starter/source 3) principal (ops/party-principal (:owner w))
           name starter/name calls (atom 0) observed (atom {}) start jobs/start!
           provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
       (doseq [[path text] source] (text! owned (str "workflows/" name "/" path) text))
       (text! owned "wiki/context.md" "Existing context, not a trigger.")
       (let [execution (-> (saved/input source)
                           (dissoc :request-id)
                           (assoc :name name :params {} :expected-bundle (:id (catalog/bundle name source)))
                           (assoc-in [:limits :settlement] "discard")
                           (assoc-in [:limits :max-dispatches] 1)
                           (assoc-in [:limits :max-model-steps] 1)
                           (assoc :expected-owned-source
                                  (parse-uuid (get-in (sources/details! host principal (str (:room w)) {:name name})
                                                     [:data :revision]))))
             row (schedules/create-owned! host principal (str (:room w))
                                          {:request-id (random-uuid) :kind "workflow"
                                           :cadence {:interval-ms 60000 :tz "UTC"} :execution execution})
             due (:schedule.activation/next-fire row)
             later #(java.util.Date. (+ (* % 60000) (.getTime ^java.util.Date due)))]
         (is (= source (get-in (model/managed-target conn row) [:snapshot :files]))
             "The activation retains the exact packaged maintainer, not a substitute workflow")
         (with-redefs [gateway/*request-fn* (fn [& _] (swap! calls inc) (final-response))
                       providers/providers (atom {:codex-subscription provider})
                       native-rooms/room-ctx-for #(when (= (:room w) %) (:ctx room))
                       jobs/start! (fn [actual-room options actual-work & callbacks]
                                     (when-not (identical? actual-room room)
                                       (throw (AssertionError. "Fixture tried to start work in another Room")))
                                     (apply start actual-room options
                                            (fn [id]
                                              (swap! observed assoc id (get-in @(var-get #'jobs/live) [id :done]))
                                              (actual-work id)) callbacks))]
           (try
             (let [balance (resource/balance room)]
               (schedules/tick-owned! host due)
               (is (= :skipped (:schedule.occurrence/state (occurrence conn row))))
               (is (= :no-inputs (:schedule.occurrence/reason (occurrence conn row))))
               (is (= balance (resource/balance room)))
               (is (zero? @calls)))
             (text! owned "inbox/evidence.md" "Synthetic evidence K1.")
             (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits execution))})
             (let [before (@#'occurrence-fixture/state owned) other (@#'occurrence-fixture/state attached)]
               (schedules/tick-owned! host (later 1))
               (let [o (occurrence conn row) rid (:schedule.occurrence/request o)
                     request (requests/get-request conn rid)
                     job (:work.request/job request)
                     status (when job (jobs/status job 40000))]
                 (is (= source (get-in (requests/payload conn request) [:input :source]))
                     "The admitted native Job carries the exact packaged source")
                 (is (uuid? job))
                 (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
                 (when job (@#'occurrence-fixture/join-fixture-jobs! room observed))
                 (@#'app-fixture/isolated
                  (fn []
                    (application/start!)
                    (is (= {:status :no-op :request (str rid)} (output/collect! (:owner w) (:room w) rid)))
                    (schedules/tick-owned! host (later 1))
                    (let [receipt (dispositions/disposition (requests/get-request conn rid))
                          proof (model/processing-proof conn (:schedule.occurrence/id o))
                          messages (count (discourse/messages room))]
                      (is (= :no-op (:kind receipt)))
                      (is (nil? (:proposal receipt)))
                      (is (some? proof))
                      (let [summary (first (schedules/list-owned! host principal (str (:room w))))
                            displayed (first (filter #(= (str rid) (:request %)) (:occurrences summary)))]
                        (is (= {:status :no-op} (:filing displayed)))
                        (is (= (str job) (:job displayed)))
                        (is (= :completed (:status displayed)))
                        (is (true? (:settled? displayed))))
                      (is (= {:status :no-op :request (str rid)} (output/collect! (:owner w) (:room w) rid)))
                      (is (= messages (count (discourse/messages room)))))))
               (is (= before (@#'occurrence-fixture/state owned)))
               (is (= other (@#'occurrence-fixture/state attached)))))
             (let [forbidden (fn [& _] (throw (AssertionError. "Unchanged trigger acquired new work")))]
               (schedules/recover-owned! host)
               (with-redefs [knowledge/capture forbidden resource/balance forbidden
                             runs/prepare forbidden work/submit-prepared! forbidden]
                 (schedules/tick-owned! host (later 2)))
               (is (= :unchanged (:schedule.occurrence/reason (occurrence conn row))))
               (text! owned "wiki/context.md" "Context changed but evidence did not.")
               (with-redefs [knowledge/capture forbidden resource/balance forbidden
                             runs/prepare forbidden work/submit-prepared! forbidden]
                 (schedules/tick-owned! host (later 3)))
               (is (= :unchanged (:schedule.occurrence/reason (occurrence conn row)))))
             (is (= 1 @calls))
             (is (= 1 (count @observed)))
             (finally (@#'occurrence-fixture/join-fixture-jobs! room observed)))))))))

(defn- named-example [path]
  (edn/read-string (slurp (io/file "examples/workflows/wiki-maintenance-named-eval-v2" path))))

(defn- tool-response [code call-id]
  (let [events [{:type "response.output_item.done"
                 :item {:type "function_call" :call_id call-id :name "clojure_eval"
                        :arguments (json/write-value-as-string {:code code})}}
                {:type "response.completed" :response {:id "scheduled-wiki-tool" :model "gpt-5.6-sol"
                                                       :usage {:input_tokens 10 :output_tokens 10}}}]]
    {:status 200 :body (java.io.ByteArrayInputStream.
                       (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8"))}))

(defn- write-response [text]
  (tool-response (str "(do (babashka.fs/create-dirs \"/out/wiki\") (spit \"/out/wiki/Company.md\" " (pr-str text) "))")
                 "scheduled-wiki-write"))

(deftest repeated-tool-failure-holds-the-schedule-without-filing-or-processing
  ;; Both empty capture and a valid partial output must remain unprocessed.
  ;; The successful no-op and proposed-update tests are the positive controls.
  (doseq [partial? [false true]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [conn w host owned room]}]
       (d/transact conn model/schema)
       (d/transact conn [{:party/id (:owner w) :party/role :admin}])
       (let [source (starter/source 2) principal (ops/party-principal (:owner w))
             handle (str (:room w)) name starter/name calls (atom 0) observed (atom {}) start jobs/start!
             fixtures (:files (named-example "fixture-pack.edn"))
             reference (get-in (named-example "calibration.edn") [:references :update :files "/out/wiki/Company.md"])
             provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
         (doseq [[path text] source] (text! owned (str "workflows/" name "/" path) text))
         (doseq [[from to] [["/in/knowledge/sources/wiki/Company.md" "wiki/Company.md"]
                           ["/in/knowledge/sources/inbox/founder-note.md" "inbox/founder-note.md"]]]
           (text! owned to (get fixtures from)))
         (let [execution (-> (saved/input source) (dissoc :request-id)
                             (assoc :name name :params {} :expected-bundle (:id (catalog/bundle name source)))
                             (assoc-in [:limits :settlement] "discard")
                             (assoc-in [:limits :max-dispatches] 5)
                             (assoc-in [:limits :max-model-steps] 5)
                             (assoc :expected-owned-source
                                    (parse-uuid (get-in (sources/details! host principal handle {:name name}) [:data :revision]))))
               row (schedules/create-owned! host principal handle
                                            {:request-id (random-uuid) :kind "workflow"
                                             :cadence {:interval-ms 60000 :tz "UTC"} :execution execution})
               due (:schedule.activation/next-fire row)
               before (@#'occurrence-fixture/state owned)]
           (with-redefs [gateway/*request-fn* (fn [& _]
                                               (let [n (swap! calls inc)]
                                                 (if (and partial? (= 1 n)) (write-response reference)
                                                     (tool-response "(throw (ex-info \"scripted tool failure\" {}))" (str "repeat-" n)))))
                         providers/providers (atom {:codex-subscription provider})
                         native-rooms/room-ctx-for #(when (= (:room w) %) (:ctx room))
                         jobs/start! (fn [actual-room options actual-work & callbacks]
                                       (when-not (identical? actual-room room) (throw (AssertionError. "Wrong fixture Room")))
                                       (apply start actual-room options
                                              (fn [id] (swap! observed assoc id (get-in @(var-get #'jobs/live) [id :done]))
                                                (actual-work id)) callbacks))]
             (try
               (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits execution))})
               (schedules/tick-owned! host due)
               (let [o (occurrence conn row) rid (:schedule.occurrence/request o)
                     job (:work.request/job (requests/get-request conn rid))
                     done (get @observed job)
                     _ (when-not (and (uuid? job) (instance? clojure.lang.IDeref done))
                         (throw (ex-info "Scheduled fixture lacks its original Job completion promise"
                                         {:occurrence o :job job :observed (keys @observed)})))
                     ;; The fixture cleanup helper cancels unfinished Jobs.
                     ;; Await this original completion receipt first so the
                     ;; regression exercises the stop, rather than cancelling
                     ;; the candidate during world setup.
                     _ (when-not (true? (deref done 120000 false))
                         (throw (ex-info "Scheduled fixture did not physically return" {:job job})))
                     _ (@#'occurrence-fixture/join-fixture-jobs! room observed)
                     candidates (binding [ec/*execution-context* (:ctx room)]
                                  (filterv #(= job (:run/parent %)) (run/runs room {:root-run-id job})))
                     candidate (first candidates)
                     _ (when-not (= 1 (count candidates))
                         (throw (ex-info "Scheduled fixture did not produce one candidate"
                                         {:occurrence o :request (requests/get-request conn rid)
                                          :job (when job (jobs/status job)) :calls @calls
                                          :candidates candidates :observed (keys @observed)})))
                     raw-attempt (binding [ec/*execution-context* (:ctx room)]
                                   (episode/attempt room (:run/id candidate)))
                     _ (when-not raw-attempt
                         (throw (ex-info "Scheduled candidate has no certified Attempt"
                                         {:candidate candidate :job (jobs/status job) :calls @calls})))
                     certified (attempt/validate-attempt raw-attempt)]
                 (is (uuid? job))
                 (is (instance? clojure.lang.IDeref (get @observed job)))
                 (is (= 1 (count candidates)))
                 (is (= (if partial? {"/out/wiki/Company.md" reference} {})
                        (get-in certified [:attempt/evidence :files])))
                 (is (= :failed (:run/status candidate)))
                 (is (= :repeated-tool-call (:run/reason candidate)))
                 (is (= :failed (get-in certified [:attempt/receipt :attempt/status])))
                 (is (false? (get-in certified [:attempt/receipt :attempt/checks :completed?])))
                 (schedules/tick-owned! host due)
                 (let [held (model/activation conn (:schedule.activation/id row))
                       balance (resource/balance room)
                       forbidden (fn [& _] (throw (AssertionError. "Failed activation acquired fresh work")))]
                   (is (false? (:schedule.activation/enabled? held)))
                   (is (= :execution-failed (:schedule.activation/reason held)))
                   (is (nil? (:schedule.activation/active-occurrence held)))
                   (is (nil? (:schedule.activation/last-processed-input held)))
                   (is (nil? (model/processing-proof conn (:schedule.occurrence/id o))))
                   (is (nil? (dispositions/disposition (requests/get-request conn rid))))
                   (with-redefs [knowledge/capture forbidden runs/prepare forbidden
                                 work/submit-prepared! forbidden resource/mint! forbidden]
                     (schedules/tick-owned! host (java.util.Date. (+ 60000 (.getTime ^java.util.Date due)))))
                   (is (= balance (resource/balance room)))
                   (is (= before (@#'occurrence-fixture/state owned)))
                   (is (= (if partial? 4 3) @calls)))
                 (is (= 1 (count @observed))))
               (finally (@#'occurrence-fixture/join-fixture-jobs! room observed))))))))))

(deftest exact-shipped-wiki-code-files-a-scheduled-proposal-without-adopting-it
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host owned attached room]}]
     (d/transact conn model/schema)
     (d/transact conn [{:party/id (:owner w) :party/role :admin}])
     (let [source (starter/source 2) principal (ops/party-principal (:owner w))
           handle (str (:room w)) name starter/name calls (atom 0) observed (atom {}) start jobs/start!
           fixtures (:files (named-example "fixture-pack.edn"))
           original (get fixtures "/in/knowledge/sources/wiki/Company.md")
           evidence (get fixtures "/in/knowledge/sources/inbox/founder-note.md")
           reference (get-in (named-example "calibration.edn") [:references :update :files "/out/wiki/Company.md"])
           provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
       (doseq [[path text] source] (text! owned (str "workflows/" name "/" path) text))
       (text! owned "wiki/Company.md" original)
       (text! owned "inbox/founder-note.md" evidence)
       (let [execution (-> (saved/input source) (dissoc :request-id)
                           (assoc :name name :params {} :expected-bundle (:id (catalog/bundle name source)))
                           (assoc-in [:limits :settlement] "discard")
                           (assoc-in [:limits :max-dispatches] 2)
                           (assoc-in [:limits :max-model-steps] 2)
                           (assoc :expected-owned-source
                                  (parse-uuid (get-in (sources/details! host principal handle {:name name}) [:data :revision]))))
             row (schedules/create-owned! host principal handle
                                          {:request-id (random-uuid) :kind "workflow"
                                           :cadence {:interval-ms 60000 :tz "UTC"} :execution execution})
             due (:schedule.activation/next-fire row)
             later #(java.util.Date. (+ (* % 60000) (.getTime ^java.util.Date due)))
             before (@#'occurrence-fixture/state owned) other (@#'occurrence-fixture/state attached)]
         (is (= source (get-in (model/managed-target conn row) [:snapshot :files])))
         (with-redefs [gateway/*request-fn* (fn [& _] (if (= 1 (swap! calls inc)) (write-response reference) (final-response "Prepared supported wiki changes for review.")))
                       providers/providers (atom {:codex-subscription provider})
                       native-rooms/room-ctx-for #(when (= (:room w) %) (:ctx room))
                       jobs/start! (fn [actual-room options actual-work & callbacks]
                                     (when-not (identical? actual-room room) (throw (AssertionError. "Wrong fixture Room")))
                                     (apply start actual-room options
                                            (fn [id] (swap! observed assoc id (get-in @(var-get #'jobs/live) [id :done]))
                                              (actual-work id)) callbacks))]
           (try
             (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits execution))})
             (schedules/tick-owned! host due)
             (let [o (occurrence conn row) rid (:schedule.occurrence/request o)
                   request (requests/get-request conn rid) job (:work.request/job request)
                   status (when job (jobs/status job 40000))]
               (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
               (is (= source (get-in (requests/payload conn request) [:input :source])))
               (when job (@#'occurrence-fixture/join-fixture-jobs! room observed))
               (@#'app-fixture/isolated
                (fn []
                  (application/start!)
                  (let [filed (output/collect! (:owner w) (:room w) rid)]
                    (is (= :proposed (:status filed)))
                    (is (uuid? (parse-uuid (:proposal filed))))
                    (is (= filed (output/collect! (:owner w) (:room w) rid)))
                    (let [receipt (dispositions/disposition (requests/get-request conn rid))
                          pair (retained/read-file conn (:manifest receipt) "wiki/Company.md")]
                      (is (= reference (String. ^bytes (outputs/bytes! (:after pair)) "UTF-8")))))))
               (is (= before (@#'occurrence-fixture/state owned)) "Filing a proposal does not adopt files")
               (is (= other (@#'occurrence-fixture/state attached)))
               (schedules/tick-owned! host due)
               (is (= :proposed (get-in (dispositions/disposition (requests/get-request conn rid)) [:kind]))))
             (let [balance (resource/balance room)
                   forbidden (fn [& _] (throw (AssertionError. "Unchanged trigger acquired work")))]
               (with-redefs [knowledge/capture forbidden resource/balance forbidden runs/prepare forbidden work/submit-prepared! forbidden]
                 (schedules/tick-owned! host (later 1)))
               (is (= :unchanged (:schedule.occurrence/reason (occurrence conn row))))
               (text! owned "wiki/Company.md" reference)
               (with-redefs [knowledge/capture forbidden resource/balance forbidden runs/prepare forbidden work/submit-prepared! forbidden]
                 (schedules/tick-owned! host (later 2)))
               (is (= :unchanged (:schedule.occurrence/reason (occurrence conn row))))
               (schedules/set-enabled-owned! host principal handle (:schedule.activation/id row) false)
               (text! owned "inbox/new-note.md" "New synthetic evidence after pausing.")
               (with-redefs [knowledge/capture forbidden resource/balance forbidden runs/prepare forbidden work/submit-prepared! forbidden]
                 (schedules/tick-owned! host (later 3)))
               (is (= balance (resource/balance room))))
             (is (= 2 @calls))
             (is (= 1 (count @observed)))
             (finally (@#'occurrence-fixture/join-fixture-jobs! room observed)))))))))
