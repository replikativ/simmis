(ns is.simm.ops.coordinator-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.program :as program]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.run :as run]
            [dvergr.agent.turn :as turn]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.chat.context :as chat]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.sandbox :as dvergr-sandbox]
            [is.simm.authority.world :as world]
            [is.simm.ops.coordinator :as coordinator]
            [is.simm.ops.coordinator-sandbox :as sandbox]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.results :as results]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test]
            [org.replikativ.spindel.engine.core :as ec]
            [sci.core :as sci])
  (:import [java.nio.charset StandardCharsets]
           [java.util Base64]))

(deftest coordinator-is-an-explicit-pinned-profile-not-a-wider-file-worker
  (let [[input base] (@#'is.simm.ops.work-test/fixture)
        plan (with-redefs [work/prepare (constantly base)] (coordinator/prepare input))
        candidate (roster/agent (:team plan) (:agent plan))]
    (is (= #{sandbox/id} (:agent/sandbox candidate)))
    (is (= (:tools sandbox/delegation-policy) (:agent/tools candidate)))
    (is (= sandbox/delegation-policy (:delegation-policy plan)))
    (is (= :coordinator (:request-kind plan)))
    (is (= input (get-in plan [:basis :submission])))
    (is (= #{(evaluation/evaluator-ref work/completion)}
           (set (keys (get-in plan [:experiment-capabilities :evaluators])))))
    (is (contains? (get-in plan [:environment :environment/world :effects :admit]) :lifecycle))
    (is (= #{} (get-in plan [:environment :environment/world :effects :allow-hosts])))))

(deftest exact-retries-observe-the-original-job-without-resolving-a-model
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (let [[input base] (@#'is.simm.ops.work-test/fixture)
             principal (ops/party-principal (:owner w))
             host-ctx ec/*execution-context*]
         (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits input))})
         (let [submitted (with-redefs [work/prepare (constantly base)]
                           (coordinator/submit! conn host-ctx principal (:room w) room input))]
           (is (:settled? (jobs/status (:job submitted) 25000)))
           (with-redefs [work/prepare (fn [& _] (throw (ex-info "model no longer available" {})))]
             (is (= (:job submitted)
                    (:job (coordinator/submit! conn host-ctx principal (:room w) room input))))
             (is (thrown-with-msg? clojure.lang.ExceptionInfo #"different work"
                                   (coordinator/submit! conn host-ctx principal (:room w) room
                                                        (assoc input :task "changed")))))
           (is (= 1 (count (jobs/jobs room))))))))))

(deftest scoped-captured-reader-is-the-same-lossless-projection-as-mcp
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (sandbox/restrict-team (roster/make-roster))
       (let [parent (:run/id (run/start! room :coordinator (random-uuid) nil))
             sibling (:run/id (run/start! room :unrelated (random-uuid) nil))]
         (try
           (let [team (roster/make-agent (roster/make-roster) {:id :candidate :program {:kind :echo}})
                 make-env (fn [ref] (environment/make-environment
                                     {:id :test/capture :task :work :verifier ref
                                      :world {:isolation :ctx :settlement :discard}}))
                 public-evaluator (assoc work/completion
                                         :capture (fn [_] {:files {"/out/code.clj" "(inc 41)🦋"}}))
                 private-evaluator (evaluation/make-evaluator
                                    {:id :test/private :version 1
                                     :observe (fn [{:keys [default]}] (assoc default :files {"/gold" "secret"}))
                                     :verify (fn [_ _] {:checks {:public? false} :reward 0.0})})
                 public @(evaluation/evaluate
                          room team :candidate
                          (make-env {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"})
                          public-evaluator {:parent-run parent})
                 private @(evaluation/evaluate room team :candidate (make-env {:id :test/private :version 1})
                                               private-evaluator {:parent-run parent})
                 ctx (turn/new-working-ctx
                      {:execution-ctx (:ctx room) :db-conn (:conn (:store room))
                       :room-runtime-id (:id room) :agent-id :coordinator
                       :agent-program-ceiling {:parent-run parent :delegation-policy sandbox/delegation-policy}
                       :sandbox-extensions #{sandbox/id}})
                 interpreter (chat/sci-context-in ctx (:ctx room))
                 recorded (:attempt public)
                 public-id (:run/id public)
                 expression (fn [op id opts] (str "(simmis.results/" op " #uuid \"" id "\" " (pr-str opts) ")"))]
             (try
               (is (= sandbox/delegation-policy (sci/eval-string* interpreter "(dvergr.agent/delegation-policy)")))
               (is (= parent (sci/eval-string* interpreter "(dvergr.agent/current-run-id)")))
               (is (re-find #"base64" (dvergr-sandbox/ns-doc-md interpreter 'simmis.results)))
               (let [chunk (sci/eval-string* interpreter (expression "attempt-file" public-id {:path "/out/code.clj"}))
                     human-shape {:id (str public-id) :run-id (str public-id)
                                  :content-id (str (:attempt/content-id recorded)) :evidence (:attempt/evidence recorded)}]
                 (is (= (results/captured-file human-shape {:path "/out/code.clj"}) chunk))
                 (is (= "(inc 41)🦋" (String. (.decode (Base64/getDecoder) ^String (:base64 chunk)) StandardCharsets/UTF_8)))
                 (is (= :discarded (:run/settlement-status (run/run room public-id))))
                 (is (contains? (:run/caused-by (run/run room parent)) public-id)))
               (doseq [code [(expression "attempt-file" sibling {:path "/out/code.clj"})
                             (expression "attempt-file" (:run/id private) {:path "/gold"})
                             "(dvergr.tasks/list)" "(env/get \"HOME\")" "(dvergr.room/rooms)"
                             "(dvergr.agent/hire! (dvergr.agent/make-agent (dvergr.agent/roster) {:id :escape :program {:kind :echo}}) :escape {:task :escape})"]]
                 (is (thrown? Exception (sci/eval-string* interpreter code))))
               (is (not (contains? (:run/caused-by (run/run room parent)) (:run/id private))))
               (is (= recorded (store/-load-attempt (:store room) (store/conversation-id room) public-id)))
               (finally (chat/release-sci-in! ctx (:ctx room)))))
           (finally (doseq [id [parent sibling]] (run/finish! id :completed)))))))))

(deftest coordinator-profile-without-host-policy-fails-closed
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (sandbox/restrict-team (roster/make-roster))
       (is (thrown? Exception
                    (turn/new-working-ctx {:execution-ctx (:ctx room) :db-conn (:conn (:store room))
                                           :room-runtime-id (:id room) :agent-id :unbounded
                                           :sandbox-extensions #{sandbox/id}})))))))
