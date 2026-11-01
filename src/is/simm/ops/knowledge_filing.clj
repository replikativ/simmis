(ns is.simm.ops.knowledge-filing
  "Host-only certification and planning of retained wiki output. Reads only:
   no Proposal filing, provider resolution, world execution or chat publication.
   The prepared value is NOT a capability: the eventual serialized writer must
   recheck current authority, default identity and the frozen request binding."
  (:require [dvergr.agent.attempt :as attempt]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.room.store :as store]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [is.simm.documents.files :as document-files]
            [is.simm.model.access :as access]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-processing :as processing]
            [is.simm.ops.knowledge-batch-proof :as batch-proof]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.work-request-checkpoints :as checkpoints]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.ops.knowledge-execution-origin :as execution-origin]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-citations :as citations]
            [is.simm.ops.knowledge-coverage :as coverage]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.knowledge-workspace :as workspace]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-request-checkpoints :as request-roots]
            [is.simm.ops.workflow-runs :as workflows]
            [org.replikativ.spindel.engine.core :as ec]))

(def max-runs 1000)

(defn- refuse! [reason]
  (throw (ex-info "Knowledge output is not available for filing"
                  {:type ::refused :reason reason})))

(defn authorize!
  "Pure current control-DB authorization. The submitting party remains the
   author; another party cannot silently impersonate it when filing output."
  [db principal room-id]
  (let [party (access/actor->party-uuid (:principal/id principal))]
    (when-not (and party (uuid? room-id)
                   (access/can? db party :read {:room room-id})
                   (access/can? db party :merge {:room room-id}))
      (refuse! :denied))
    party))

(defn- target-descriptor [{:keys [conn identity]}]
  (let [db @conn]
    (merge {:repository (:geschichte.repo/id (query/repository db))
            :store (get-in db [:config :store :id])
            :backend (get-in db [:config :store :backend])
            :branch (get-in db [:config :branch]) :ref (query/current-ref db)}
           identity)))

(defn current-target
  "Current explicit default repository identity, not its present file bodies.
   Do not upgrade an older unmarked historical basis by matching it here.
   The explicit registry DB arity preserves the filing writer's snapshot."
  ([room-id room]
   (target-descriptor (admission/default-target room-id room)))
  ([registry-db room-id room]
   (target-descriptor (admission/default-target registry-db room-id room))))

(defn verify-frozen-workflow!
  "Verify the retained workflow, selection and inert setup/verifier references.
   No live workspace, provider, Attempt, authorization or settlement access."
  [input]
  (let [definition (environment/validate-environment (:environment input))
        workflow (:workflow input)
        selection (:input-selection workflow)
        _ (when-not (and (= :knowledge (:kind selection))
                         (= :simmis/saved-workflow (:environment/id definition))
                         (= 1 (:environment/version definition))
                         (map? (:source input)) (map? (:inputs input)))
            (refuse! :unsupported-workflow))
        captured (inputs/from-inputs (:policy selection) (:inputs input))
        _ (when-not (= (get-in captured [:manifest :repository])
                       (get-in input [:knowledge/publication-basis :target :repository]))
            (refuse! :input-repository-mismatch))
        ;; This constructor validates retained bytes and builds an inert setup
        ;; capability. It never calls its :prepare function or any provider.
        prepared (workflows/prepare-source {:files (:source input)}
                                           (select-keys (:submission input) workflows/source-fields)
                                           captured)
        target (get-in input [:knowledge/publication-basis :target])
        checkpoint-policy (execution-origin/validate-execution! (:source input) (get-in input [:submission :limits]))
        checkpoint (:knowledge/checkpoint input)
        _ (when-not (= (some? checkpoint-policy) (some? checkpoint))
            (refuse! :checkpoint-policy-mismatch))
        _ (when checkpoint
            (checkpoints/validate-selection! checkpoint)
            (when-not (= target (:target checkpoint)) (refuse! :checkpoint-target-mismatch)))
        initial-setup (if checkpoint
                        (workspace/with-checkpoint (:world-setup prepared) checkpoint)
                        (:world-setup prepared))
        verifier (evaluation/evaluator-ref
                  (outputs/with-publication-target
                   (workflows/completion-evaluator (:source input) workflow (:inputs input)
                                                   (:knowledge/publication-basis input)) target))
        setup (evaluation/world-setup-ref
               (workspace/with-publication-target initial-setup target))]
    (when-not (and (= workflow (:workflow prepared)
                      (get-in definition [:environment/metadata :workflow]))
                   (= (:inputs input) (:inputs prepared))
                   (= (:source input) (:source prepared))
                   (or (not (:knowledge/source-citations input))
                       (= (:task prepared) (:knowledge/source-task input)))
                   (= (:environment/task definition)
                      (workflows/citation-task (:task prepared) (:knowledge/source-citations input)))
                   (= verifier (:environment/verifier definition))
                   (= setup (get-in definition [:environment/world :setup]))
                   (= :ctx (get-in definition [:environment/world :isolation]))
                   (contains? #{:review :discard} (get-in definition [:environment/world :settlement])))
      (refuse! :frozen-workflow-mismatch))
    {:environment definition :workflow-bundle (get-in prepared [:bundle :id])
     :captured captured}))

(defn- certified-candidate! [room row input]
  (when-not (and (:store room) (satisfies? store/PAttemptStore (:store room)))
    (refuse! :missing-attempt-store))
  (binding [ec/*execution-context* (:ctx room)]
    (let [job-id (:work.request/job row)
          job (run/run room job-id)
          requester (:work.request/by row)
          _ (when-not (and (= job-id (:run/id job))
                           (= :workflow (:run/kind job))
                           (= (store/conversation-id room) (:run/room job))
                           (= requester (access/actor->party-uuid (:run/actor job)))
                           ;; This exact root + direct parent edge establishes
                           ;; structural lineage, not mere temporal proximity.
                           (nil? (:run/parent job))
                           (= :completed (:run/status job))
                           (instance? java.util.Date (:run/ended-at job)))
              (refuse! :job-not-completed))
          rows (vec (take (inc max-runs) (run/runs room {:root-run-id job-id :limit (inc max-runs)})))
          direct (filterv #(= job-id (:run/parent %)) rows)
          _ (when-not (and (<= (count rows) max-runs) (= 1 (count direct)))
              (refuse! :ambiguous-direct-candidate))
          candidate (run/run room (:run/id (first direct)))
          agent (:candidate input)
          agent-hash (hasch/uuid agent)
          _ (when-not (and (= (:run/id (first direct)) (:run/id candidate))
                           (= :agent-task (:run/kind candidate))
                           (= job-id (:run/parent candidate))
                           (= (store/conversation-id room) (:run/room candidate))
                           (#{:completed :failed} (:run/status candidate))
                           (instance? java.util.Date (:run/ended-at candidate))
                           (= (:agent/id agent) (:run/actor candidate))
                           (= agent-hash (:run/agent-def-hash candidate))
                           (= (:agent/version agent) (:run/agent-version candidate))
                           (pos-int? (:run/interpreter-version candidate))
                           (= (get-in agent [:agent/program :kind]) (:run/program-kind candidate)))
              (refuse! :candidate-mismatch))
          certified (or (episode/attempt room (:run/id candidate)) (refuse! :missing-attempt))
          _ (attempt/validate-attempt certified)
          receipt (:attempt/receipt certified)
          metrics (:attempt/metrics receipt)
          checks (:attempt/checks receipt)
          trace (vec (take (inc max-runs) (get-in certified [:attempt/evidence :trace :runs])))
          trace-candidates (filterv #(= (:run/id candidate) (:run/id %)) trace)]
      (when-not (and (= (:run/id candidate) (:attempt/run-id certified))
                     (= (:environment input) (:attempt/environment certified))
                     (= agent (:attempt/agent certified))
                     (= agent-hash (:attempt/agent-def-hash certified) (:agent-def-hash metrics))
                     (= (:run/interpreter-version candidate) (:interpreter-version metrics))
                     (= requester (:requester-party metrics))
                     (= (:work.request/id row) (:request-id metrics))
                     (= :saved-workflow (:execution-kind metrics))
                     (= (evaluation/evaluator-tier (outputs/with-capture work/completion))
                        (:verifier-trust metrics))
                     (= (:run/status candidate) (:attempt/status receipt))
                     (map? checks) (seq checks)
                     (true? (:knowledge-workspace? checks))
                     (let [coverage? (some? (coverage/declaration (:source input)))
                           ordinary (if coverage? (dissoc checks :original-evidence-covered?) checks)]
                       (and (or (not coverage?) (boolean? (:original-evidence-covered? checks)))
                            (if (= :completed (:run/status candidate))
                              (and (true? (:completed? ordinary)) (every? true? (vals ordinary))
                                   (or (not coverage?) (:original-evidence-covered? checks)
                                       (zero? (:attempt/reward receipt))))
                              (and (= {:completed? false :knowledge-workspace? true} ordinary)
                                   (zero? (:attempt/reward receipt))))))
                     (<= (count trace) max-runs)
                     (= 1 (count trace-candidates))
                     (= (select-keys candidate [:run/id :run/parent :run/actor :run/status :run/agent-def-hash])
                        (select-keys (first trace-candidates) [:run/id :run/parent :run/actor :run/status :run/agent-def-hash])))
        (refuse! :certification-mismatch))
      certified)))

(defn- certified-request!
  [conn principal room-id room request-id]
  (when-not (uuid? request-id) (refuse! :invalid-request))
  (let [party (authorize! @conn principal room-id)
        row (or (requests/get-request conn request-id) (refuse! :missing-request))
        _ (when-not (and (= room-id (:work.request/room row)) (= party (:work.request/by row)))
            (refuse! :request-owner-mismatch))
        binding (dispositions/request-binding row)
        basis (or (requests/publication-basis conn request-id) (refuse! :missing-publication-basis))
        _ (when-not (= (:work.request/publication-basis-id row) (:id basis))
            (refuse! :publication-basis-mismatch))
        _ (when-not (publication/host-target? (:target basis)) (refuse! :unmarked-publication-basis))
        _ (when-not (= (:target basis) (current-target room-id room)) (refuse! :target-mismatch))
        retained (requests/payload conn row)
        _ (when-not (= (:work.request/intent-id row) (hasch/uuid retained))
            (refuse! :payload-mismatch))
        input (:input retained)
        _ (when-let [context (:knowledge/source-citations input)]
            (when-not (citations/valid-context? context room-id request-id)
              (refuse! :citation-request-mismatch)))
        proof (when (or (:work.request/knowledge-batch row) (:work.request/knowledge-frozen row)
                        (contains? input :knowledge/batch) (contains? input :knowledge/batch-control))
                (batch-proof/request! conn row retained))
        batch (when proof (processing/expected proof))
        _ (when (or (contains? input :knowledge/checkpoint)
                    (contains? (:source input) "execution-origin.edn"))
            (when-not (and proof (= (:knowledge/checkpoint input)
                                   (execution-origin/selection (:source input) proof)))
              (refuse! :checkpoint-batch-mismatch)))
        {:keys [environment workflow-bundle]} (verify-frozen-workflow! input)
        certified (certified-candidate! room row input)
        _ (workspace/validate-proof! (:target basis)
                                     (get-in certified [:attempt/evidence :knowledge-workspace]))]
    {:party party :row row :binding binding :basis basis :input input :batch batch
     :environment environment :workflow-bundle workflow-bundle :certified certified}))

(defn candidate-outcome
  "Read-only strict certification of the exact LIVE knowledge candidate.
   A failed outcome certifies execution failure, NOT publishable output or a
   processed input. Missing, corrupt or inconsistent evidence always throws.
   The caller must separately establish physical settlement and serialize
   the exact returned request binding before changing activation state."
  [conn principal room-id room request-id]
  (let [{:keys [binding certified]} (certified-request! conn principal room-id room request-id)]
    {:outcome (get-in certified [:attempt/receipt :attempt/status])
     :request-binding binding :candidate (:attempt/run-id certified)
     :attempt-content (:attempt/content-id certified)}))

(defn- retained-before-inputs
  "Read only changed existing targets from the Request's verified immutable
   checkpoint when v5/v6 did not copy wiki context into model inputs. Never
   substitute current file contents or create a retention root during review."
  [conn room-id room request-id input basis output]
  (let [retained (:inputs input)
        missing (into (sorted-map)
                      (keep (fn [path]
                              (let [before (inputs/publication-before basis path)]
                                (when (and (:present? before)
                                           (not (contains? retained (str "/in/knowledge/sources/" path))))
                                  [path before]))))
                      (keys (:files output)))]
    (if (or (empty? missing) (not (:knowledge/checkpoint input))) retained
      (do
        (when (or (> (reduce + 0 (map :size (vals missing))) (:max-total-bytes outputs/policy))
                  (some #(> (:size %) (:max-file-bytes outputs/policy)) (vals missing)))
          (refuse! :historical-body-too-large))
        (let [source (admission/default-binding room-id room)
              snapshot (:db (request-roots/verify! conn @(:conn source) request-id))]
          (reduce-kv
           (fn [out path expected]
             (let [actual (document-files/read-file-in snapshot path)]
               (when-not (= (select-keys expected [:content :size :mode]) (:entry actual))
                 (refuse! :historical-body-mismatch))
               (assoc out (str "/in/knowledge/sources/" path) (:text actual))))
           retained missing))))))

(defn prepare
  "Certify ONE canonical request's successful direct durable Attempt and
   prepare exact retained filing data. A failed candidate can never file.
   Current authority/default identity MUST be rechecked by the filing writer."
  [conn principal room-id room request-id]
  (let [{:keys [party row binding basis input environment workflow-bundle certified batch]}
        (certified-request! conn principal room-id room request-id)
        _ (when-not (= :completed (get-in certified [:attempt/receipt :attempt/status]))
            (refuse! :candidate-not-completed))
        output (get-in certified [:attempt/evidence :knowledge-output])
        _ (outputs/validate! output)
        retained-inputs (retained-before-inputs conn room-id room request-id input basis output)
        _ (when (coverage/declaration (:source input))
            (let [policy (get-in input [:workflow :input-selection :policy])
                  captured (inputs/from-inputs policy (:inputs input))]
              (coverage/require-ready! (coverage/with-existing-targets
                                        (coverage/expected (:manifest captured)) basis
                                        (inputs/output-scope policy))
                                       (inputs/output-scope policy)
                                       (:attempt/evidence certified))))
        planned (plans/plan {:request request-id :room room-id :attempt (:attempt/run-id certified)
                             :environment environment :workflow-bundle workflow-bundle
                             :publication-basis basis :output output :retained-inputs retained-inputs})
        proposed? (seq (get-in planned [:manifest :entries]))
        disposition (dispositions/make
                     (cond-> {:request request-id :intent (:work.request/intent-id row)
                              :job (:work.request/job row) :candidate (:attempt/run-id certified)
                              :attempt-content (:attempt/content-id certified)
                              :basis (:id basis) :output (:id output)
                              :kind (if proposed? :proposed :no-op)}
                       batch (assoc :batch (get-in batch [:origin :batch]))
                       proposed? (assoc :manifest (get-in planned [:manifest :id]) :proposal (:proposal planned))) (:version output))]
    {:request-binding binding :disposition disposition :plan (when proposed? planned) :batch batch
     :party party :room room-id :target (:target basis)}))
