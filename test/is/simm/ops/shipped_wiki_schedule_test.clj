(ns is.simm.ops.shipped-wiki-schedule-test
  "Shipped source through ordinary scheduling; scripted transport, not model quality.\n   Recovery here is in-process reconciliation, not a fresh-process restart."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
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
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-schedule-occurrences-test :as occurrence-fixture]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflow-runs-test :as saved]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.application-ops-test :as app-fixture]
            [is.simm.starter.wiki-maintenance :as starter]
            [jsonista.core :as json]))

(defn- final-response []
  {:status 200
   :body (java.io.ByteArrayInputStream.
           (.getBytes
             (apply str
                    (map #(str "data: " (json/write-value-as-string %) "\n\n")
                         [{:type "response.output_text.delta" :delta "No supported wiki change; no output."}
                          {:type "response.completed"
                           :response {:id "maintainer-noop" :model "gpt-5.6-sol"
                                      :usage {:input_tokens 10 :output_tokens 10}}}]))
             "UTF-8"))})

(defn- text! [conn path text] (repo/write! conn path (.getBytes ^String text "UTF-8")))
(defn- occurrence [conn row]
  (model/occurrence conn (:schedule.activation/last-occurrence
                          (model/activation conn (:schedule.activation/id row)))))

(deftest actual-shipped-maintainer-is-an-ordinary-scheduled-noop
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host owned attached room]}]
     (d/transact conn model/schema)
     (d/transact conn [{:party/id (:owner w) :party/role :admin}])
     (let [source (starter/source) principal (ops/party-principal (:owner w))
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
