(ns is.simm.ops.daily-room-summary-test
  "One native daily workflow. Only provider transport is scripted: no live
   inference, and all admission, SCI, retention and publication remain real."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.system.rooms :as native-rooms]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.summary-documents :as documents]
            [is.simm.model.summary-records :as records]
            [is.simm.model.workflow-schedules :as schedule-model]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-schedule-occurrences-test :as occurrence-fixture]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.shipped-wiki-v6-daily-test :as fixture]
            [is.simm.ops.shipped-wiki-schedule-test :as scripted]
            [is.simm.ops.summary-reader :as reader]
            [is.simm.ops.summary-inputs :as inputs]
            [is.simm.ops.workflow-runs-test :as saved]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.ops.work :as work]
            [is.simm.starter.daily-room-summary :as starter]
            [org.replikativ.spindel.engine.core :as ec]))

(def script
  "(let [s (read-string (slurp \"/in/summary/manifest.edn\")) refs (mapv :source-revision (:coverage s)) originals (mapv #(slurp (str \"/\" (:path %))) (:coverage s))] (when-not (= 1 (count originals)) (throw (ex-info \"Expected one original\" {}))) (spit \"/out/summary.edn\" (pr-str {:version 2 :selection-id (:id s) :summary {:text \"The interview was approved; customer demand remains unvalidated.\" :sources refs} :sections {:decisions [] :proposals [] :observations [] :uncertainty [] :open-work []} :coverage (mapv (fn [id] {:source-revision id :status :selected :note \"Read the precise original.\"}) refs)})) :written)")

(deftest daily-summary-executes-and-publishes-once-and-idle-tick-does-not-spend
  (@#'fixture/with-canonical-fixture
   (fn [{:keys [conn w host owned room]}]
     (d/transact conn schedule-model/schema)
     (d/transact conn [{:party/id (:owner w) :party/role :admin}
                      {:room/id (:room w) :room/content-db-scope (get-in @(:conn (:store room)) [:config :store :id])}])
     (let [source (starter/source) principal (ops/party-principal (:owner w)) handle (str (:room w))
           calls (atom 0) observed (atom {}) start jobs/start!
           provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
       (doseq [[path text] source]
         (repo/write! owned (str "workflows/" starter/name "/" path) (markdown/source-bytes text)))
       (let [execution (-> (saved/input) (dissoc :request-id)
                           (assoc :name starter/name :params {"save-transcript" "true"}
                                  :expected-bundle (:id (catalog/bundle starter/name source :program))
                                  :expected-owned-source (parse-uuid (get-in (sources/details! host principal handle {:name starter/name}) [:data :revision])))
                           (assoc-in [:limits :settlement] "discard")
                           (assoc-in [:limits :max-dispatches] 2)
                           (assoc-in [:limits :max-model-steps] 2)
                           (assoc-in [:limits :timeout-ms] 60000))
             activation (schedules/create-owned! host principal handle
                                                 {:request-id (random-uuid) :kind "workflow"
                                                  :cadence {:interval-ms 60000 :tz "UTC"} :execution execution})
             due (:schedule.activation/next-fire activation)
             period (inputs/closed-day due "UTC")
             message (assoc (discourse/message (keyword "party" (str (:owner w))) :_room-log
                                               "The interview was approved; customer demand is still unvalidated." nil {:role :user})
                            :ts (.getTime (:from period)))]
         (binding [ec/*execution-context* (:ctx room)]
           (is (= :inserted (store/-store-message! (:store room) (:id room) message))))
         (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits execution))})
         (with-redefs [gateway/*request-fn* (fn [& _]
                                           (if (= 1 (swap! calls inc))
                                             (@#'scripted/tool-response script "daily-summary-write")
                                             (@#'scripted/final-response "Saved the exact daily summary.")))
                       providers/providers (atom {:codex-subscription provider})
                       native-rooms/room-ctx-for #(when (= (:room w) %) (:ctx room))
                       jobs/start! (fn [actual options actual-work & callbacks]
                                     (apply start actual options
                                            (fn [id] (swap! observed assoc id (get-in @(var-get #'jobs/live) [id :done]))
                                              (actual-work id)) callbacks))]
           (try
             (schedules/tick-owned! host due)
             (let [a (schedule-model/activation conn (:schedule.activation/id activation))
                   o (schedule-model/occurrence conn (:schedule.activation/active-occurrence a))
                   request (requests/get-request conn (:schedule.occurrence/request o))
                   done (get @observed (:work.request/job request))]
               (when-not (and done (true? (deref done 120000 false)))
                 (throw (ex-info "Daily summary fixture did not return" {:state (:schedule.occurrence/state o) :calls @calls
                                                                        :reason (:schedule.activation/reason a)})))
               (@#'occurrence-fixture/join-fixture-jobs! room observed)
               (schedules/tick-owned! host due)
               (let [result (reader/result! host principal (:room w) (:work.request/id request))
                     selection (:selection result)
                     native (binding [ec/*execution-context* (:ctx room)] @(:conn (:store room)))
                     after (schedule-model/activation conn (:schedule.activation/id activation))
                     balance (resource/balance room)]
                 (is (= 2 @calls))
                 (is (= :room-summary (:kind result)))
                 (is (= 1 (count (:originals result))))
                 (is (files/entry @owned (documents/path selection)))
                 (is (files/entry @owned (documents/transcript-path selection)))
                 (is (= 1 (:total (records/heads-in native (:room w) 8))))
                 (is (nil? (:schedule.activation/active-occurrence after)))
                 (schedules/tick-owned! host (java.util.Date. (+ 60000 (.getTime due))))
                 (is (= 2 @calls))
                 (is (= balance (resource/balance room)))
                 (is (= :unchanged (:schedule.occurrence/reason
                                   (first (schedule-model/occurrences conn (:schedule.activation/id activation))))))))
             (finally (@#'occurrence-fixture/join-fixture-jobs! room observed)))))))))
