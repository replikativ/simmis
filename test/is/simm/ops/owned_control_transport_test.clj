(ns is.simm.ops.owned-control-transport-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.ops :as dops]
            [dvergr.mcp.server :as server]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-mcp-transport-test :as wire]
            [is.simm.ops.owned-schedule-admission-test :as schedule-fixture]
            [is.simm.model.workflow-schedules :as schedule-model]
            [is.simm.model.workers :as workers]
            [is.simm.ops.owned-worker-admission :as worker-admission]
            [is.simm.ops.owned-worker-admission-test :as worker-fixture]
            [is.simm.ops.owned-workflow-transport-test :as transport]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.work :as work]
            [is.simm.ops.worker-control :as worker-control]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.worker-work :as worker-work]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as source]
            [is.simm.runtimes.mcp :as mcp]))

(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo e e)))
(defn- principal [w]
  (assoc (ops/party-principal (:owner w)) :principal/mcp? true
         :principal/tools mcp/work-tools :principal/scopes #{}))

(defn- with-worker-api [f]
  (let [previous (dops/operations)]
    (try
      (worker-control/install!)
      (f)
      (finally
        (doseq [op (keys worker-control/specifications)]
          (if-let [spec (get previous op)]
            (dops/register-op! op spec) (dops/unregister-op! op)))
        (server/sync-op-tools!)))))

(deftest guarded-native-execution-refuses-before-room-hydration
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host hydrations]}]
     (@#'transport/with-api
      (fn []
        (let [base {:room (str (:room w)) :name "echo" :request-id (str (random-uuid))
                    :expected-bundle (str (random-uuid)) :model "codex-subscription-sol"
                    :limits {:budget-microdollars 10000 :max-dispatches 3 :max-model-steps 3
                             :timeout-ms 30000 :settlement "discard"}}
              forbidden (fn [& _] (throw (AssertionError. "Selected room lookup forbidden")))]
          (with-redefs [api/room! forbidden]
            (doseq [args [base (assoc base :expected-owned-source (apply str (repeat 36 "x")))]]
              (is (= 400 (dops/error-status
                          (failure #(dops/invoke host (principal w) :simmis/workflow-run args)))))))
          (is (empty? @hydrations))))))))

(deftest present-malformed-owners-never-downgrade-control-routes-to-selected-rooms
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host hydrations]}]
     (@#'transport/with-api
      (fn []
        (let [p (principal w) handle (str (:room w))
              forbidden (fn [& _] (throw (AssertionError. "Invalid owner fell back to Room")))]
          (with-redefs [api/room! forbidden]
            (doseq [owner [nil {} {:control-conn :forged :execution-ctx :forged}]
                    [op args] [[:simmis/work-status {:room handle :id (str (random-uuid))}]
                               [:simmis/workflow-check {:room handle :name "echo"}]
                               [:simmis/workflow-schedule-list {:room handle}]]]
              (is (= 503 (dops/error-status
                          (failure #(dops/invoke (assoc host ::reads/owner owner) p op args)))))))
          (is (empty? @hydrations))))))))

(deftest owned-check-and-keyword-file-save-use-the-same-repository-in-both-orders
  (doseq [order [true false]]
    (@#'fixture/with-fixture
     order
     (fn [{:keys [w host owned attached]}]
       (doseq [[path text] source/fixture]
         (repo/write! owned (str "workflows/echo/" path) (.getBytes ^String text "UTF-8")))
       (repo/stage-all! owned)
       (repo/commit! owned {:message "Seed transport source"})
       (@#'transport/with-api
        (fn []
          (let [p (principal w) handle (str (:room w))
                forbidden (fn [& _] (throw (AssertionError. "Selected read forbidden")))]
            (with-redefs [api/room! forbidden workflows/inspect forbidden workflows/write! forbidden]
              (let [opened (dops/invoke host p :simmis/workflow-check {:room handle :name "echo"})
                    saved (dops/invoke host p :simmis/workflow-write
                                       {:room handle :name "echo" :expected-revision (:revision opened)
                                        :files {:notes.md "Owned notes"}})
                    after (dops/invoke host p :simmis/workflow-check {:room handle :name "echo"})]
                (is (map? saved))
                (is (not= (:revision opened) (:revision after)))
                (is (string? (:revision after)))
                (let [unchanged (dops/invoke host p :simmis/workflow-write
                                             {:room handle :name "echo" :expected-revision (:revision after)
                                              :files {:notes.md "Owned notes"}})]
                  (is (= (:revision after) (get-in unchanged [:workflow :revision]))))
                (is (nil? (repo/read attached "workflows/echo/notes.md")))
                (is (= "Owned notes" (String. ^bytes (repo/read owned "workflows/echo/notes.md") "UTF-8"))))))))))))

(deftest real-worker-retries-precede-new-admission-flags-and-revoked-enrollment
  (doseq [benchmark? [false true]]
    (@#'worker-fixture/with-worker
     true
     (fn [{:keys [conn w host worker request hydrations]}]
       (let [kind (if benchmark? :local-worker-benchmark :local-worker-workflow)
             input (request kind) p (principal w)
             submit (if benchmark? worker-admission/submit-benchmark! worker-admission/submit-workflow!)
             initial (with-redefs [work/submit-prepared! (@#'worker-fixture/claim-only (atom nil))]
                       (submit host p (str (:room w)) input))
             wire (reduce (fn [result field] (update result field str))
                          (assoc input :room (str (:room w)))
                          [:request-id :worker :expected-bundle :expected-owned-source])
             calls (atom 0)
             forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Retry touched fresh work")))]
         (workers/revoke! conn worker)
         (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository]])
         (reset! hydrations [])
         (with-worker-api
           (fn []
             (with-redefs [worker-control/admission-open? forbidden
                           reads/resolve-host-read! forbidden worker-work/enrollment! forbidden
                           work/submit-prepared! forbidden api/room! forbidden
                           server/current-daemon (constantly host)]
               (let [tool (if benchmark? "simmis_local-worker-benchmark" "simmis_local-worker-workflow")]
                 (doseq [tools [mcp/read-tools mcp/work-tools]]
                   (let [send (@#'wire/session "2026-07-28" (assoc p :principal/tools tools))
                         reply (@#'wire/tool send tool wire)]
                     (is (or (:error reply) (get-in reply [:result :isError])))))
                 (doseq [version ["2025-11-25" "2026-07-28"]]
                   (let [send (@#'wire/session version (assoc p :principal/tools mcp/worker-control-tools))
                         result (@#'wire/data (@#'wire/tool send tool wire))]
                     (is (= (str (:id initial)) (:id result)))))))))
         (is (zero? @calls))
         (is (empty? @hydrations)))))))

(deftest actual-schedule-transport-retains-nested-source-guard-and-supports-metadata-pause
  (@#'schedule-fixture/with-fixture
   false
   (fn [{:keys [conn w host input]}]
     (@#'transport/with-api
      (fn []
        (let [p (principal w) handle (str (:room w)) original (input :workflow)
              wire (-> original (assoc :room handle) (update :request-id str)
                       (update-in [:execution :expected-bundle] str)
                       (update-in [:execution :expected-owned-source] str))
              forbidden (fn [& _] (throw (AssertionError. "Schedule used selected Room")))]
          (with-redefs [api/room! forbidden]
            (let [receipt (dops/invoke host p :simmis/workflow-schedule-create wire)
                  id (parse-uuid (:id receipt))
                  row (schedule-model/activation conn id)
                  target (schedule-model/managed-target conn row)]
              (is (= (:execution original) (:execution target)))
              (is (not (contains? (:execution target) :request-id)))
              (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository]])
              (is (= 1 (count (dops/invoke host p :simmis/workflow-schedule-list {:room handle}))))
              (is (false? (:enabled? (dops/invoke host p :simmis/workflow-schedule-set-enabled
                                                  {:room handle :id (str id) :enabled? false}))))))))))))
