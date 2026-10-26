(ns is.simm.authority.owned-workflow-browser-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.ops :as dops]
            [datahike.api :as d]
            [dvergr.rooms.files :as native]
            [geschichte.repo :as repo]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.authority.application-fixture :as application-fixture]
            [is.simm.ops.owned-control-transport-test :as controls]
            [is.simm.ops.owned-schedule-admission-test :as schedule-fixture]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.ops.owned-workflow-transport-test :as transport]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.work :as work]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.worker-control :as worker-control]
            [is.simm.ops.worker-service :as worker-service]
            [is.simm.ops.worker-work :as worker-work]
            [is.simm.runtimes.worker-maintenance :as maintenance]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as source]
            [is.simm.model.rooms :as rooms]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [is.simm.uis.web.desktop.chat-remote :as chat]))

(defn- seed! [conn]
  (doseq [[path text] source/fixture]
    (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8"))))

(defn- with-browser-api [conn room f]
  (application-fixture/with-application
   conn (:ctx room)
   (fn [_] (@#'transport/with-api f))))

(deftest normal-human-browser-reads-are-canonical-without-mcp-or-cold-registry-dependence
  (doseq [order [true false]]
    (@#'fixture/with-fixture
     order
     (fn [{:keys [conn room w owned attached]}]
       (seed! owned)
       (repo/write! attached "workflows/echo/workflow.edn" (.getBytes "OTHER" "UTF-8"))
       (with-browser-api conn room
        (fn []
          (let [forbidden (fn [& _] (throw (AssertionError. "Selected or prehydration lookup")))]
            (with-redefs [dops/resolve-room (constantly nil)
                          api/room! forbidden workflows/details forbidden]
              (let [result (remote/invoke-workflow (:member w) (str (:room w)) :simmis/workflow-source {:name "echo"})]
                (is (= source/fixture (:files result)))
                (is (true? (get-in result [:permissions :write?])))
                (is (false? (get-in result [:permissions :merge?]))))
              (is (thrown? clojure.lang.ExceptionInfo
                           (remote/invoke-workflow (:non-member w) (str (:room w)) :simmis/workflow-source {:name "echo"})))
              (is (thrown? clojure.lang.ExceptionInfo
                           (remote/invoke-workflow (:member w) (str (:room w)) :simmis/workflow-source
                                                   {:name "echo" :room "different"})))))))))))

(deftest browser-save-keeps-the-original-sealed-definition-after-an-independent-edit
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn room w owned]}]
     (seed! owned)
     (repo/stage-all! owned)
     (repo/commit! owned {:message "Seed browser save"})
     (with-browser-api conn room
      (fn []
        (let [handle (str (:room w))
              before (remote/invoke-workflow (:member w) handle :simmis/workflow-source {:name "echo"})
              native-write native/write-to-completion!
              writes (atom 0)]
          (with-redefs [native/write-to-completion!
                        (fn [& args]
                          (let [ack (apply native-write args)]
                            (swap! writes inc)
                            (repo/write! owned "workflows/echo/workflow.edn"
                                         (.getBytes "{:title \"Newer\" :task \"Newer task\"}" "UTF-8"))
                            ack))
                        workflows/details (fn [& _] (throw (AssertionError. "Save reread selected source")))]
            (let [answer (remote/invoke-workflow (:member w) handle :simmis/workflow-write
                                                 {:name "echo" :expected-revision (:revision before)
                                                  :files {"notes.md" "My edit"}})]
              (is (= 1 @writes))
              (is (= (:definition before) (:definition answer)))
              (is (not (:source-changed? answer)))
              (is (string? (get-in answer [:workflow :revision])))))
          (with-redefs [api/owned-workflow-save! (fn [& _] (throw (AssertionError. "Denied caller reached save service")))]
            (is (thrown? clojure.lang.ExceptionInfo
                         (remote/invoke-workflow (:non-member w) handle :simmis/workflow-write
                                                 {:name "echo" :expected-revision (:revision before)
                                                  :files {"notes.md" "Forbidden"}}))))))))))

(deftest dashboard-workflow-schedules-use-owned-metadata-after-default-removal
  (@#'schedule-fixture/with-fixture
   false
   (fn [{:keys [conn w host input]}]
     (let [row (schedules/create-owned! host (principals/party-principal (:owner w))
                                        (str (:room w)) (input :workflow))]
       ;; The generic authority fixture also includes an unprovisioned Scratch
       ;; room. Give it the canonical metadata every provisioned room has;
       ;; no repository or schedule is needed to list an empty room.
       (doseq [room (rooms/get-party-rooms (:owner w)) :when (nil? (:room/slug room))]
         (d/transact conn [{:room/id (:room/id room) :room/slug (str "fixture-" (:room/id room))}]))
       (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository]])
       (@#'transport/with-api
        (fn []
          (let [result (chat/load-schedules-for (:owner w))
                listed (first (:workflow-schedules result))]
            (is (= (str (:schedule.activation/id row)) (:id listed)))
            (is (true? (:enabled? listed)))
            (is (uuid? (parse-uuid (:source-revision listed)))))))))))

(deftest legacy-controls-authorize-before-modeled-cold-room-hydration
  ;; Controlled cold lookup/hydration seam, NOT a physical startup proof. The
  ;; real metadata authorizer, operation declarations and service routes run.
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room]}]
     (with-browser-api conn room
      (fn []
        (@#'controls/with-worker-api
         (fn []
           (let [hydrated? (atom false) hydrations (atom 0) calls (atom 0)
                 record (fn [& _] (swap! calls inc) {:state :fixture})]
             (with-redefs [dops/resolve-room (fn [& _] (when @hydrated? room))
                           room-agents/live-room (fn [id]
                                                   (is (= (:room w) id))
                                                   (swap! hydrations inc)
                                                   (reset! hydrated? true) room)
                           work/provision! record benchmarks/provision-cli! record
                           worker-work/provision! record worker-service/list-own record
                           worker-control/admission-open? (constantly true)
                           maintenance/claims-ready? (constantly true)]
               (doseq [[op args] [[:simmis/work-allowance {:request-id (str (random-uuid)) :budget-microdollars 10000 :max-dispatches 3}]
                                  [:simmis/cli-allowance {:request-id (str (random-uuid)) :max-cli-runs 1}]
                                  [:simmis/local-worker-allowance {:request-id (str (random-uuid)) :max-worker-admissions 1}]
                                  [:simmis/local-worker-list {}]]]
                 (reset! hydrated? false)
                 (let [before @hydrations]
                   (is (thrown? clojure.lang.ExceptionInfo
                                (remote/invoke-workflow (:non-member w) (str (:room w)) op args)))
                   (is (= before @hydrations))
                   (is (= :fixture (:state (remote/invoke-workflow (:owner w) (str (:room w)) op args))))
                   (is (= (inc before) @hydrations))))
               (is (= 4 @calls)))))))))))

(deftest a-slug-remap-during-legacy-hydration-cannot-retarget-funding
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn w room]}]
     (with-browser-api conn room
      (fn []
        (let [handle (:room/slug (d/entity @conn [:room/id (:room w)]))
              hydrated (atom []) funded (atom 0)]
          (with-redefs [room-agents/live-room
                        (fn [id]
                          (swap! hydrated conj id)
                          (d/transact conn [[:db/add [:room/id (:room w)] :room/slug (str "moved-" (:room w))]
                                            [:db/add [:room/id (:other-room w)] :room/slug handle]
                                            [:db/add [:room/id (:other-room w)] :room/parties [:party/id (:owner w)]]])
                          room)
                        work/provision! (fn [& _] (swap! funded inc))]
            (let [error (try
                          (remote/invoke-workflow (:owner w) handle :simmis/work-allowance
                                                  {:request-id (str (random-uuid))
                                                   :budget-microdollars 10000 :max-dispatches 3})
                          nil
                          (catch clojure.lang.ExceptionInfo e e))]
              (is (= 503 (dops/error-status error)))
              (is (= [(:room w)] @hydrated))
              (is (zero? @funded))))))))))
