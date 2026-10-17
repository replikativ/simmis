(ns is.simm.ops.worker-workflow-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.jobs :as jobs]
            [dvergr.agent.run :as run]
            [dvergr.room.registry :as registry]
            [dvergr.resource :as resource]
            [dvergr.substrate.geschichte :as substrate]
            [dvergr.rooms.forks :as forks]
            [geschichte.repo :as repo]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workers :as workers]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-files-test :as file-fixture]
            [is.simm.ops.worker-recovery :as recovery]
            [is.simm.ops.worker-recovery-test :as recovery-fixture]
            [is.simm.ops.worker-service :as service]
            [is.simm.ops.worker-work :as local]
            [is.simm.ops.worker-work-test :as fixture]
            [is.simm.ops.workflow-runs-test :as workflow-fixture]
            [is.simm.ops.workflows :as workflows]
            [muschel.fs :as fs]
            [org.replikativ.spindel.engine.core :as ec]))

(defn request [input]
  (-> (workflow-fixture/input)
      (dissoc :model) (assoc :worker (:worker input) :limits (:limits input))))

(deftest saved-workflow-uses-one-pinned-funded-local-run
  (world/with-world [conn w]
    (@#'fixture/with-worker
     conn w
     (fn [{:keys [room host-ctx principal enrolled input allowance published]}]
       (workflows/write! room principal {:name "echo" :files workflow-fixture/source})
       (binding [ec/*execution-context* (:ctx room)]
         (let [ws (substrate/current-workspace)
               path "workflows/echo/helpers/data.txt"
               entry (get (repo/worktree (:conn ws)) path)]
           (fs/chmod (substrate/filesystem ws) (str "/" path) 0x81ED)
           (forks/commit-paths! room {(str "/" path) (str (:content entry))} "Executable workflow helper")))
       (local/provision! conn host-ctx principal (:room w) room allowance)
       (let [request (request input)
             snapshot workflows/snapshot
             edited? (atom false)
             submitted (with-redefs [workflows/snapshot
                                     (fn [r name]
                                       (let [pinned (snapshot r name)]
                                         (when (and (= (:id r) (:id room)) (compare-and-set! edited? false true))
                                           (workflows/write! room principal
                                                             {:name name :files {"helpers/data.txt" "new source"
                                                                                "helpers/new.txt" "new helper"}}))
                                         pinned))]
                         (local/submit-workflow! conn host-ctx principal (:room w) room request))
             id (deref published 10000 nil)
             lease (@#'fixture/claim! conn enrolled id)
             row (requests/get-request conn (:id submitted))
             payload (requests/payload conn row)
             frozen (get-in payload [:input :frozen])]
         (try
           (is (uuid? id))
           (is (= :local-worker-workflow (:work.request/kind row)))
           (is (= "Write a literal {subject} value to /out/result.md" (:task frozen)))
           (is (= "shared fixture" (String. (files/file-bytes! (get-in frozen [:files "docs/input.md"])) "UTF-8")))
           (is (= "pinned helper" (String. (files/file-bytes! (get-in frozen [:files "workflows/echo/helpers/data.txt"])) "UTF-8")))
           (is (= 0x81ED (get-in frozen [:files "workflows/echo/helpers/data.txt" :mode])))
           (is (not (contains? (:files frozen) "workflows/echo/helpers/new.txt")))
           (let [world (binding [ec/*execution-context* (:ctx room)]
                         (registry/lookup (:run/world (run/run room id))))
                 pinned (snapshot world "echo")]
             (is (= "pinned helper" (get-in pinned [:files "helpers/data.txt"])))
             (is (not (contains? (:files pinned) "helpers/new.txt")))
             (is (= 0x81ED (:mode (first (filter #(= "helpers/data.txt" (:path %)) (:manifest pinned))))))
             (is (= "new source" (get-in (snapshot room "echo") [:files "helpers/data.txt"])))
             (is (= "new helper" (get-in (snapshot room "echo") [:files "helpers/new.txt"]))))
           (is (not (contains? (:files frozen) "private.txt")))
           ;; Ordinary workflow source may include checker/gold, but is not
           ;; graded and never executes the hostile checker in this fixture.
           (is (contains? (:files frozen) "workflows/echo/checker.clj"))
           (service/complete! conn lease {:run (str id)
                                          :output {:files {"out/result.md" (file-fixture/encoded "local workflow result")}
                                                   :summary "done" :exit-code 0}})
           (is (:settled? (jobs/status (:job submitted) 25000)))
           (let [attempt (@#'fixture/stored-attempt room)]
             (is (= 1.0 (get-in attempt [:attempt/receipt :attempt/reward])))
             (is (= :local-worker-workflow (get-in attempt [:attempt/receipt :attempt/metrics :execution-kind])))
             (is (= (file-fixture/encoded "local workflow result")
                    (get-in attempt [:attempt/evidence :files "/out/result.md"]))))
           (is (= {} (workspace/read-tree room "/out")))
           (is (= 1M (get (resource/balance room) local/admission-unit)))
           (workers/revoke! conn (:worker enrolled))
           (with-redefs [workflows/snapshot (fn [& _] (throw (ex-info "No reread" {})))]
             (is (= (:job submitted) (:job (local/submit-workflow! conn host-ctx principal (:room w) room request)))))
           (is (thrown? clojure.lang.ExceptionInfo
                        (local/submit-workflow! conn host-ctx principal (:room w) room (assoc request :params {"subject" "changed"}))))
           ;; Recovery validates only immutable admitted data, not the room.
           (with-redefs [workflows/snapshot (fn [& _] (throw (ex-info "No recovery reread" {})))]
             (is (= frozen (get-in (@#'recovery/intent! conn row) [:assignment :input])))
             (doseq [changed [(update-in payload [:input :snapshot :manifest]
                                        #(mapv (fn [entry] (assoc entry :mode 0x81A4)) %))
                              (assoc-in payload [:input :workflow :name] "tampered")]]
               (with-redefs [requests/payload (fn [& _] changed)]
                 (is (thrown? clojure.lang.ExceptionInfo (@#'recovery/intent! conn row))))))
           (finally
             (service/stop! conn lease {:run (str id)})
             (jobs/status (:job submitted) 25000))))))))

(deftest workflow-authority-precedes-source-and-retries
  (world/with-world [conn w]
    (@#'fixture/with-worker
     conn w
     (fn [{:keys [room host-ctx input]}]
       (with-redefs [workflows/snapshot (fn [& _] (throw (AssertionError. "Unauthorized source read")))]
         (doseq [party [(:member w) (:non-member w) (:admin w)]]
           (is (thrown? clojure.lang.ExceptionInfo
                        (local/submit-workflow! conn host-ctx (ops/party-principal party)
                                                (:room w) room (request input))))))
       (is (empty? (requests/requests conn)))))))

(deftest workflow-recovery-refuses-altered-source-or-provenance
  (let [request (request {:worker (random-uuid) :limits file-fixture/limits})
        snapshot (update (workflow-fixture/snapshot workflow-fixture/source) :manifest
                         #(mapv (fn [entry] (assoc entry :mode 0x81A4)) %))
        prepared (local/workflow-input snapshot request file-fixture/config)]
    (is (= (:frozen prepared) (:frozen (local/workflow-input snapshot request file-fixture/config))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (local/workflow-input (assoc-in snapshot [:files "helpers/data.txt"] "edited") request file-fixture/config)))
    (doseq [extra [{:model "forbidden"} {:task "forbidden"} {:protocol {}}]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (local/workflow-input snapshot (merge request extra) file-fixture/config))))))

(deftest orphaned-saved-workflow-is-fenced-and-cleaned-without-rereading-source
  (world/with-world [conn w]
    (@#'fixture/with-worker
     conn w
     (fn [{:keys [room principal input] :as options}]
       (workflows/write! room principal {:name "echo" :files workflow-fixture/source})
       (let [{:keys [finish! recover! child]}
             (@#'recovery-fixture/orphan
              conn w (assoc options :input (request input) :request-kind :local-worker-workflow
                            :prepare @#'local/prepare-workflow :live-world? true) true)]
         (finish!)
         (with-redefs [workflows/snapshot (fn [& _] (throw (AssertionError. "Recovery must not read current source")))]
           (is (= {:state :returned} (recover!)))
           (is (= :withdrawn (:simmis.worker.assignment/state (workers/assignment conn child))))
           (is (nil? (recover!)))))))))
