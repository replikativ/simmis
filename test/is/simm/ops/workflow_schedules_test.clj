(ns is.simm.ops.workflow-schedules-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.room.store.datahike :as room-store]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.ops.workflows :as workflows]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]))

(def fixture-policy {:version 1 :fixture true})

(deftest external-cadences-normalize-to-the-actual-cron-runtime-contract
  (is (= {:every-ms 60000 :tz "UTC"} (#'schedules/cadence! {:interval-ms 60000 :tz "UTC"})))
  (is (= {:every :week :on :monday :at "09:00" :tz "Europe/Berlin"}
         (#'schedules/cadence! {:every "week" :on "monday" :at "09:00" :tz "Europe/Berlin"})))
  (doseq [cadence [{:interval-ms 60001 :tz "UTC"}
                   {:interval-ms 60000 :tz "Not/AZone"}
                   {:every "day" :at "25:00" :tz "UTC"}
                   {:interval-ms 60000 :every "day" :at "09:00" :tz "UTC"}]]
    (is (= :is.simm.ops.workflow-schedules/invalid
           (try (#'schedules/cadence! cadence) nil
                (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
(def source {"workflow.edn" "{:title \"Scheduled echo\" :task \"Write /out/brief.md\" :capture [\"/out\"]}"
             "fixtures/docs/input.md" "Pinned market evidence"
             "checker.clj" "(ns fixture.checker) (defn check [_] {:checks {:ok true} :reward 1.0})"})

(defn- with-fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}
        ctx (context/create-execution-context)
        owner (random-uuid) room-id (random-uuid)]
    (binding [ec/*execution-context* ctx]
      (d/create-database cfg)
      (let [conn (d/connect cfg)]
        (try
          (d/transact conn (into model/schema requests/schema))
          (let [target {:version 1 :kind :workflow :cadence {:interval-ms 60000 :tz "UTC"}
                        :snapshot {:files source :manifest []} :host-policy fixture-policy
                        :execution {:name "echo" :expected-bundle (:id (catalog/bundle "echo" source))
                                    :model "codex-subscription-luna-6"
                                    :limits {:budget-microdollars 1000 :max-dispatches 2
                                             :max-model-steps 2 :timeout-ms 30000 :settlement "review"}}}
                row (model/create! conn {:id (random-uuid) :room room-id :by owner :target target
                                         :now (java.util.Date. 0) :next-fire (java.util.Date. 60000)})]
            (with-redefs [schedules/host-policy (constantly fixture-policy)
                          schedules/model-policy (constantly nil)
                          schedules/publish-completion! (fn [& _] nil)
                          work/authorize! (fn [& _] owner)
                          resource/balance (fn [_] (work/resources (get-in target [:execution :limits])))
                          jobs/status (constantly nil)]
              (f {:conn conn :ctx ctx :owner owner :room-id room-id :room {:ctx ctx}
                  :row row :target target})))
          (finally (d/release conn)))))))

(defn- pending! [{:keys [conn row target]}]
  (model/claim-due! conn row (:cadence target) (java.util.Date. 60000))
  (first (model/occurrences conn (:schedule.activation/id row))))

(defn- record-request! [{:keys [conn owner room-id target]} occurrence submitted?]
  (let [{:keys [request]} (requests/claim!
                           conn {:party owner :room room-id :client-id (:schedule.occurrence/id occurrence)
                                 :kind :workflow :intent {:submission (:execution target)}
                                 :limits (update (get-in target [:execution :limits]) :settlement keyword)})]
    (if submitted?
      (requests/record-job! conn (:work.request/id request) (:work.request/claim request) (random-uuid))
      request)))

(defn- tick! [{:keys [conn ctx room]} millis]
  (schedules/tick! conn ctx (constantly room) (java.util.Date. millis)))

(deftest completion-publication-retries-the-same-envelope-after-a-lost-acknowledgement
  ;; Save the real function before the ordinary fixture stubs room delivery.
  (let [publish! schedules/publish-completion!]
    (with-fixture
      (fn [{:keys [conn row room] :as fixture}]
        (let [o (pending! fixture) messages (atom []) mark! model/mark-announced!
              failed? (atom false) request {:work.request/job (random-uuid)}]
          (with-redefs [discourse/post! (fn [_ message] (swap! messages conj message))
                        model/mark-announced! (fn [conn oid]
                                                (if (compare-and-set! failed? false true)
                                                  (throw (ex-info "Lost publication acknowledgement" {}))
                                                  (mark! conn oid)))]
            (is (thrown? clojure.lang.ExceptionInfo (publish! conn row o request {:status :completed :finished-at #inst "2026-10-19T08:00:00Z"} room)))
            (publish! conn row o request {:status :completed :finished-at #inst "2026-10-19T08:00:00Z"} room)
            (is (= 2 (count @messages)))
            (is (= (first @messages) (second @messages)))
            (is (= (.getTime #inst "2026-10-19T08:00:00Z") (:ts (first @messages))))
            (is (= :_room-log (:to (first @messages))))
            (is (= (:work.request/job request) (get-in (first @messages) [:metadata :run-id])))
            (is (true? (:schedule.occurrence/announced? (model/occurrence conn (:schedule.occurrence/id o)))))))))))

(deftest recovery-links-committed-request-without-resubmission
  (with-fixture
    (fn [{:keys [conn ctx row] :as fixture}]
      (let [o (pending! fixture) _ (model/admit! conn (:schedule.occurrence/id o))
            request (record-request! fixture o true)]
        (with-redefs [workflow-runs/submit! (fn [& _] (throw (AssertionError. "Recovery must not submit")))]
          (schedules/recover! conn ctx)
          (schedules/recover! conn ctx)
          (let [after (model/occurrence conn (:schedule.occurrence/id o))]
            (is (= :linked (:schedule.occurrence/state after)))
            (is (= (:work.request/id request) (:schedule.occurrence/request after)))
            (is (:schedule.activation/enabled? (model/activation conn (:schedule.activation/id row))))))))))

(deftest a-real-datahike-room-deduplicates-completion-publication-after-a-lost-ack
  (let [publish! schedules/publish-completion!]
    (with-fixture
      (fn [{:keys [conn ctx row] :as fixture}]
        (let [o (pending! fixture)
              room (discourse/make-room {:id (keyword (str "scheduled-result-" (random-uuid)))
                                         :ctx ctx :store (room-store/make conn)})
              mark! model/mark-announced! failed? (atom false)
              request {:work.request/job (random-uuid)}]
          (try
            (with-redefs [model/mark-announced!
                          (fn [conn oid]
                            (if (compare-and-set! failed? false true)
                              (throw (ex-info "Process lost the system-DB acknowledgement" {}))
                              (mark! conn oid)))]
              (is (thrown? clojure.lang.ExceptionInfo (publish! conn row o request {:status :completed :finished-at #inst "2026-10-19T08:00:00Z"} room)))
              (is (= 1 (count (discourse/messages room))))
              (publish! conn row o request {:status :completed :finished-at #inst "2026-10-19T08:00:00Z"} room)
              (is (= 1 (count (discourse/messages room))))
              (is (= (:work.request/job request) (get-in (first (discourse/messages room)) [:metadata :run-id])))
              (is (true? (:schedule.occurrence/announced? (model/occurrence conn (:schedule.occurrence/id o))))))
            (finally (discourse/close-room! room))))))))

(deftest recovery-holds-missing-and-pointerless-admissions
  (doseq [has-request? [false true]]
    (with-fixture
      (fn [{:keys [conn ctx owner room-id row] :as fixture}]
        (let [o (pending! fixture)]
          (model/admit! conn (:schedule.occurrence/id o))
          (when has-request? (record-request! fixture o false))
          (schedules/recover! conn ctx)
          (is (= :interrupted (:schedule.occurrence/state (model/occurrence conn (:schedule.occurrence/id o)))))
          (is (false? (:schedule.activation/enabled? (model/activation conn (:schedule.activation/id row)))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (schedules/set-enabled! conn ctx (principals/party-principal owner) room-id
                                               (:schedule.activation/id row) true))))))))

(deftest overlap-remains-until-job-and-resource-cleanup-both-finish
  (with-fixture
    (fn [{:keys [conn row] :as fixture}]
      (let [o (pending! fixture) request (record-request! fixture o true)
            status (atom {:status :completed :settled? false})]
        (model/admit! conn (:schedule.occurrence/id o))
        (model/finish-admission! conn (:schedule.occurrence/id o) :linked (:work.request/id request) nil)
        (with-redefs [jobs/status (fn [_] @status)
                      workflow-runs/submit! (fn [& _] (throw (AssertionError. "Overlapping admission")))]
          (tick! fixture 120000)
          (is (= :skipped (:schedule.occurrence/state (first (model/occurrences conn (:schedule.activation/id row))))))
          (reset! status {:status :completed :settled? true})
          (tick! fixture 180000)
          (is (= (:schedule.occurrence/id o)
                 (:schedule.activation/active-occurrence (model/activation conn (:schedule.activation/id row)))))
          (requests/mark-returned! conn (:work.request/id request))
          ;; No new due slot: just release the completed occurrence.
          (tick! fixture 180000)
          (is (nil? (:schedule.activation/active-occurrence (model/activation conn (:schedule.activation/id row))))))))))

(deftest revoked-authority-blocks-before-submission
  (with-fixture
    (fn [{:keys [conn row] :as fixture}]
      (with-redefs [work/authorize! (fn [& _] (throw (ex-info "Revoked" {:type ::revoked})))
                    workflow-runs/submit! (fn [& _] (throw (AssertionError. "Revoked schedule submitted")))]
        (tick! fixture 60000)
        (let [after (model/activation conn (:schedule.activation/id row))
              o (first (model/occurrences conn (:schedule.activation/id row)))]
          (is (false? (:schedule.activation/enabled? after)))
          (is (= ::revoked (:schedule.activation/reason after)))
          (is (= :blocked (:schedule.occurrence/state o)))
          (is (empty? (requests/requests conn))))))))

(deftest exhausted-shared-allowance-does-not-admit-or-mint
  (with-fixture
    (fn [{:keys [conn row] :as fixture}]
      (with-redefs [resource/balance (constantly {})
                    resource/mint! (fn [& _] (throw (AssertionError. "Schedule minted allowance")))
                    workflow-runs/submit! (fn [& _] (throw (AssertionError. "Unfunded schedule submitted")))]
        (tick! fixture 60000)
        (let [after (model/activation conn (:schedule.activation/id row))]
          (is (false? (:schedule.activation/enabled? after)))
          (is (= :is.simm.ops.workflow-schedules/budget-exhausted (:schedule.activation/reason after)))
          (is (nil? (:schedule.activation/active-occurrence after)))
          (is (empty? (requests/requests conn))))))))

(deftest retained-source-submission-survives-current-source-edits
  (with-fixture
    (fn [{:keys [conn owner row target] :as fixture}]
      (let [prepared-snapshots (atom [])]
        (with-redefs [workflows/snapshot (fn [& _] (throw (AssertionError. "Read mutable authoring source")))
                      workflow-runs/prepare (fn [snapshot input]
                                              (swap! prepared-snapshots conj snapshot)
                                              {:basis {:submission input}})
                      work/submit-prepared!
                      (fn [c _ _ room-id _ input prepared]
                        (let [{:keys [request]} (requests/claim! c {:party owner :room room-id
                                                                    :client-id (:request-id input) :kind :workflow
                                                                    :intent (:basis prepared)
                                                                    :limits (update (:limits input) :settlement keyword)})]
                          (requests/record-job! c (:work.request/id request) (:work.request/claim request) (random-uuid))))]
          (tick! fixture 60000)
          (is (= [(:snapshot target)] @prepared-snapshots))
          (let [o (first (model/occurrences conn (:schedule.activation/id row)))]
            (is (= :linked (:schedule.occurrence/state o)))
            (is (= 1 (count (requests/requests conn)))))
          (tick! fixture 60000)
          (is (= 1 (count @prepared-snapshots))))))))

(deftest ambiguous-post-submission-response-links-the-exact-committed-request
  (with-fixture
    (fn [{:keys [conn row] :as fixture}]
      (let [calls (atom 0)]
        (with-redefs [workflow-runs/submit!
                      (fn [_ _ _ _ _ input _]
                        (swap! calls inc)
                        (record-request! fixture {:schedule.occurrence/id (:request-id input)} true)
                        (throw (ex-info "Lost successful response" {})))]
          (tick! fixture 60000)
          (tick! fixture 60000)
          (let [o (first (model/occurrences conn (:schedule.activation/id row)))]
            (is (= :linked (:schedule.occurrence/state o)))
            (is (uuid? (:schedule.occurrence/request o)))
            (is (= 1 @calls))
            (is (= 1 (count (requests/requests conn))))))))))
