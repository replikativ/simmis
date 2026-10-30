(ns is.simm.ops.github-sync
  "Human-reviewed public GitHub fast-forward. Fixed HOST operations reuse the
   original App/Room, native workspace publication and typed control journal.
   No automatic retry, source-selected hooks, executor or funding."
  (:require [datahike.api :as d]
            [datahike.versioning :as versioning]
            [dvergr.artifact :as artifact]
            [geschichte.git.budget :as budget]
            [geschichte.git.transport :as transport]
            [geschichte.workspace :as workspace]
            [is.simm.model.github-sync :as journal]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-connections :as connections]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.github-import :as import]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-roots :as roots]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.github-sync-workspace :as private]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- prepare-tx [db context binding ref id]
  (host/writer-current! context db)
  (journal/prepare-tx db binding ref id))

(defn- transition-tx [db context row state ref id]
  (host/writer-current! context db)
  (when-not (or (:cleanup? context) (= (:party (:resolved context)) (:github.sync/by row)))
    (facts/refuse! :different-owner))
  (let [transition (journal/transition-tx db row state ref id)
        expected (:integration context)
        {:keys [before after]} (:integration-change context)]
    (when (and expected (#{:applying :applied} state))
      (integrations/validate-sync-base! db expected (get-in before [:visible :head]) (:git-oid before)))
    (into transition
          (when (and expected (= :applied state))
            (integrations/advance-sync-tx db expected (:github.sync/id row)
                                          (get-in before [:visible :head]) (:git-oid before)
                                          (get-in after [:visible :head]) (:git-oid after) ref id)))))

(defn- row! [{:keys [conn resolved cleanup?]} id]
  (let [row (journal/sync-in @conn id)]
    (when (and row (not= :fast-forward (journal/kind row))) (facts/refuse! :different-operation))
    (when-not (and row (or cleanup? (= (:party resolved) (:github.sync/by row)))
                   (= (:room-id resolved) (:github.sync/room row)))
      (facts/refuse! :different-intent))
    row))

(defn- read-body! [context row slot]
  (host/current! context)
  (let [ref (get row (keyword "github.sync" (name slot)))
        id (get row (keyword "github.sync" (str (name slot) "-id")))
        body (:value (artifact/get-bounded-value (artifact/datahike-store (:conn context)) ref facts/max-artifact-bytes))]
    (when-not (and body (= id (facts/body-id slot body))) (facts/refuse! :proof-unavailable))
    (host/current! context)
    (when-not (= row (row! context (:github.sync/id row))) (facts/refuse! :intent-changed))
    body))

(defn- transition! [context row state body]
  (host/current! context)
  (let [slot ({:allocated :workspace :ready :ready :applied :outcome :satisfied-observed :outcome :closed :closure :held :incident} state)
        id (when slot (facts/body-id slot body))
        published (volatile! nil)
        write! (fn [ref]
                 (vreset! published ref)
                 (host/current! context)
                 ;; Runtime handles are opaque closure captures, not transaction
                 ;; argument data: error logging must never print the live App,
                 ;; connection, queue and context graph on a guarded refusal.
                 (d/transact (:conn context) [[:db.fn/call #(transition-tx % context row state ref id)]]))
        failure (try
                  (if slot (artifact/publish-value! (artifact/datahike-store (:conn context)) body write!) (write! nil))
                  nil
                  (catch Exception e
                    (when (adoption/causal-interruption? e) (.interrupt (Thread/currentThread)) (throw e)) e))
        observed (row! context (:github.sync/id row))]
    (host/current! context)
    (when-not (and (= state (:github.sync/state observed))
                   (if slot (and (= @published (get observed (keyword "github.sync" (name slot))))
                                 (= id (get observed (keyword "github.sync" (str (name slot) "-id"))))) true))
      (throw (or failure (ex-info "Git sync outcome unconfirmed" {:type ::unconfirmed}))))
    (when slot (when-not (= body (read-body! context observed slot)) (facts/refuse! :proof-changed)))
    observed))

(defn- preparation! [context row]
  (let [body (read-body! context row :preparation)]
    (when-not (and (= 1 (:version body))
                   (= (:origin context) (:origin body))
                   (= (get-in context [:bound :target]) (:target body))
                   (= (get-in context [:bound :target-stamp]) (:target-stamp body))
                   (= (:revision context) (:revision body))
                   (= (:branch body) (facts/branch (:github.sync/id row) (:github.sync/claim row)))
                   (= (select-keys body [:id :by :room :client-id :claim])
                      {:id (:github.sync/id row) :by (:github.sync/by row) :room (:github.sync/room row)
                       :client-id (:github.sync/client-id row) :claim (:github.sync/claim row)}))
      (facts/refuse! :source-settings-changed))
    body))

(defn- workspace-body [row prepared]
  {:version 1 :id (:github.sync/id row) :claim (:github.sync/claim row)
   :preparation-id (:github.sync/preparation-id row) :branch (:branch prepared)
   :checkpoint (get-in prepared [:before :checkpoint]) :target (:target prepared)})

(defn- workspace! [context row prepared]
  (let [receipt (read-body! context row :workspace)]
    (when-not (= receipt (workspace-body row prepared)) (facts/refuse! :workspace-proof-changed))
    receipt))

(defn- ready! [context row]
  (let [prepared (preparation! context row) _ (workspace! context row prepared)
        ready (read-body! context row :ready)]
    (when-not (and (= 1 (:version ready))
                   (= (:github.sync/preparation-id row) (:preparation-id ready))
                   (= (:before prepared) (:before ready))
                   (= (:changes ready) (facts/changes (:before ready) (:after ready))))
      (facts/refuse! :proof-changed))
    (facts/validate-desired! (:before ready) (:after ready))
    [prepared ready]))

(defn- base-retention! [{:keys [conn resolved bound] :as context} source]
  ;; Retention is a prerequisite for future THREE-WAY integration, never a
  ;; new prerequisite for the existing independent FF protocol. Keep optional
  ;; metadata failures inside this boundary and record a portable outcome.
  (try
    (if-let [cursor (integrations/current-in @conn (:room-id resolved))]
      (let [connection (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                                                           (:github.integration/connection cursor) 16384))]
        (when-not (and (= (:target bound) (:target connection))
                       (= (:target-stamp bound) (:target-stamp connection))
                       (= (connections/connection-id connection) (:github.integration/connection-id cursor)))
          (facts/refuse! :source-settings-changed))
        (assoc (select-keys (roots/retain-base! conn source connection (:party resolved)
                                                #(host/current! context) #(host/writer-current! context %))
                            [:root :selection])
               :status :retained))
      {:status :unavailable :reason :integration-unverified})
    (catch Exception error
      (when (adoption/causal-interruption? error)
        (.interrupt (Thread/currentThread))
        (throw error))
      ;; Deliberately no exception message, runtime object or arbitrary data.
      {:status :held :reason :retention-unavailable})))

(defn- prepare! [{:keys [conn resolved bound origin] :as context} request]
  (let [id (journal/request-id (:party resolved) (:room-id resolved) (:client-id request))]
    (if-let [existing (journal/sync-in @conn id)]
      (let [body (preparation! context (row! context id))]
        (when-not (= (:expected-target-stamp request) (:target-stamp body))
          (facts/refuse! :different-intent))
        existing)
      (do
        (when-not (= (:expected-target-stamp request) (:target-stamp bound)) (facts/refuse! :target-changed))
        (when-not (= (:registry-system origin) (get-in bound [:target :registry-system]))
          (facts/refuse! :source-settings-changed))
        (let [source (:db (host/current! context))
              retention (base-retention! context source)
              _ (host/current! context)
              before (facts/capture source origin)
              binding {:id id :by (:party resolved) :room (:room-id resolved)
                       :client-id (:client-id request) :claim (random-uuid)}
              body (merge binding {:version 1 :origin origin :target (:target bound) :target-stamp (:target-stamp bound)
                                   :revision (:revision context) :branch (facts/branch id (:claim binding))
                                   :base-retention retention :before before})
              body-id (facts/body-id :preparation body)
              published (volatile! nil)]
          ;; Ambiguous preparation ACK never grants native creation rights.
          (artifact/publish-value!
           (artifact/datahike-store conn) body
           (fn [ref]
             (vreset! published ref)
             (host/current! context)
             (d/transact conn [[:db.fn/call #(prepare-tx % context binding ref body-id)]])))
          (let [row (row! context id)]
            (when-not (and (= :creating (:github.sync/state row))
                           (= (:claim binding) (:github.sync/claim row))
                           (= @published (:github.sync/preparation row))
                           (= body (preparation! context row)))
              (facts/refuse! :creation-unconfirmed))
            (let [worker (Thread/currentThread) owner (:owner lifetime/*ticket*)
                  allocated (volatile! nil)
                  operation (budget/create {:limits import/limits :timeout-ms 120000
                                            :cancelled? #(or (.isInterrupted worker) (not (lifetime/ready? owner)))})
                  after (try (private/with-connection!
                               (:conn bound) (:lease context) (:branch body) (:checkpoint before) true
                               #(vreset! allocated (transition! context row :allocated (workspace-body row body)))
                               (fn [child]
                                 (host/current! context)
                                 (transport/pull! {:conn child :remote "origin"
                                                   :url (get-in before [:visible :configuration "remote.origin.url"])
                                                   :options {:policy :ff-only :branch (subs (:ref origin) 11) :haves []
                                                             :prefixes [(:ref origin)] :import-budget operation
                                                             :http-body-idle-timeout-ms 15000}})
                                 (budget/check! operation)
                                 (host/current! context)
                                 (facts/validate-desired! before (facts/capture @child origin))))
                             (catch Exception error
                               (when (adoption/causal-interruption? error)
                                 (.interrupt worker) (throw error))
                               (when (and @allocated (= ::private/body-failed (:type (ex-data error)))
                                          (true? (:released? (ex-data error)))
                                          (uuid? (:checkpoint (ex-data error)))
                                          (= (:branch body) (:branch (ex-data error))))
                                 (transition! context @allocated :held
                                              {:version 1 :kind :preparation-failed
                                               :workspace-id (:github.sync/workspace-id @allocated)
                                               :branch (:branch body) :checkpoint (:checkpoint (ex-data error))
                                               :reason (facts/error-reason error)
                                               :released? true}))
                               (throw error)))]
              ;; READY means the physical child connection has returned/released.
              (transition! context @allocated :ready {:version 1 :preparation-id body-id :before before :after after
                                                      :changes (facts/changes before after)}))))))))

(defn- cleanup! [context row prepared proof]
  (let [row (if (= :closing (:github.sync/state row)) row (transition! context row :closing nil))
        parent (get-in context [:bound :conn]) branch (:branch prepared)]
    (host/current! context)
    (private/retire! parent (:lease context) branch (:checkpoint proof))
    (transition! context row :closed {:version 1 :id (:github.sync/id row)
                                      :ready-id (:github.sync/ready-id row) :branch branch :released? true})))

(defn- discard-proof! [context row prepared]
  (let [allocation (workspace! context row prepared)]
    (cond
      (:github.sync/ready row) {:checkpoint (get-in (second (ready! context row)) [:after :checkpoint])}
      (:github.sync/incident row)
      (let [incident (read-body! context row :incident)]
        (when-not (and (= :preparation-failed (:kind incident)) (true? (:released? incident))
                       (= (:branch prepared) (:branch incident)) (uuid? (:checkpoint incident))
                       (= (:github.sync/workspace-id row) (:workspace-id incident)))
          (facts/refuse! :cleanup-unconfirmed))
        (select-keys incident [:checkpoint]))
      (#{:allocated :closing} (:github.sync/state row))
      {:checkpoint (if (and (= :closing (:github.sync/state row))
                            (not (contains? (versioning/branches (get-in context [:bound :conn])) (:branch prepared))))
                     (:checkpoint allocation)
                     (private/retained-checkpoint! (get-in context [:bound :conn]) (:branch prepared) (:checkpoint allocation)))}
      :else (facts/refuse! :cleanup-unconfirmed))))

(defn- refused-publication? [row incident ready]
  (and (= :held (:github.sync/state row)) (= :applying (:github.sync/held-from row))
       (= :publication-refused (:kind incident)) (= :geschichte.workspace/stale-advance (:error-type incident))
       (true? (:released? incident)) (= (:github.sync/ready-id row) (:ready-id incident))
       (= (:github.sync/workspace-id row) (:workspace-id incident))
       (= (get-in ready [:after :checkpoint]) (:checkpoint incident)) (uuid? (:checkpoint incident))))

(defn- act! [context operation request]
  (let [id (or (:sync-id request)
               (journal/request-id (get-in context [:resolved :party]) (get-in context [:resolved :room-id]) (:client-id request)))
        row (if (= :prepare operation) (prepare! context request) (row! context id))]
    (case operation
      (:prepare :observe) row
      :apply
      (let [[prepared ready] (ready! context row)
            cursor (:integration context)
            eligible? (and cursor
                           (= (:github.integration/head cursor) (get-in ready [:before :visible :head]))
                           (= (:github.integration/oid cursor) (get-in ready [:before :git-oid])))
            ;; Existing FF safety comes from its exact native checkpoint, not
            ;; optional cursor metadata. Never infer/repair an already stale or
            ;; absent cursor. Once eligible, retain whole-row CAS throughout.
            context (cond-> (assoc context :integration-change (select-keys ready [:before :after]))
                      (not eligible?) (dissoc :integration))]
        (when-not (and (= :ready (:github.sync/state row)) (= (:ready-id request) (:github.sync/ready-id row)))
          (facts/refuse! :not-ready))
        (let [row (transition! context row :applying nil)]
          (host/current! context)
          (try (private/with-connection!
                 (get-in context [:bound :conn]) (:lease context) (:branch prepared) (get-in ready [:after :checkpoint]) false
                 (fn [child]
                   (when-not (facts/same-visible? (:after ready) (facts/capture @child (:origin context)))
                     (facts/refuse! :private-branch-changed))
                   (host/current! context)
                   (workspace/advance! child (get-in context [:bound :conn])
                                       {:ref (get-in context [:origin :ref])
                                        :expected-checkpoint (get-in ready [:before :checkpoint])})))
               (catch Exception error
                 (when (and (= ::private/body-failed (:type (ex-data error)))
                            (true? (:released? (ex-data error)))
                            (= (get-in ready [:after :checkpoint]) (:checkpoint (ex-data error)))
                            (some #(= :geschichte.workspace/stale-advance (:type (ex-data %)))
                                  (take 16 (take-while some? (iterate ex-cause error)))))
                   ;; This native typed refusal is emitted before publication,
                   ;; including the serialized writer's exact-basis rejection.
                   ;; It differs from an unknown/postcommit ACK failure.
                   (transition! context row :held
                                {:version 1 :kind :publication-refused :error-type :geschichte.workspace/stale-advance
                                 :ready-id (:github.sync/ready-id row) :workspace-id (:github.sync/workspace-id row)
                                 :checkpoint (:checkpoint (ex-data error)) :released? true :reason :target-changed}))
                 (throw error)))
          (when-not (facts/same-visible? (:after ready) (facts/capture (:db (host/current! context)) (:origin context)))
            (facts/refuse! :outcome-unconfirmed))
          (let [row (transition! context row :applied {:version 1 :id id :ready-id (:github.sync/ready-id row)
                                                       :kind :applied})]
            (cleanup! context row prepared {:checkpoint (get-in ready [:after :checkpoint])}))))
      :recover
      (let [prepared (preparation! context row)]
        (case (:github.sync/state row)
          (:applying :held)
          (let [[_ ready] (ready! context row)]
            (when-not (facts/same-visible? (:after ready) (facts/capture (:db (host/current! context)) (:origin context)))
              (facts/refuse! :outcome-unconfirmed))
            (cleanup! context (transition! context row :satisfied-observed
                                           {:version 1 :id id :ready-id (:github.sync/ready-id row) :kind :satisfied-observed})
                      prepared {:checkpoint (get-in ready [:after :checkpoint])}))
          (:applied :satisfied-observed :closing) (cleanup! context row prepared (discard-proof! context row prepared))
          :closed row
          (facts/refuse! :recovery-held)))
      (:discard :retire)
      (let [prepared (preparation! context row)]
        (if (= :closed (:github.sync/state row))
          row
          (do
            (when-not (or (#{:ready :allocated} (:github.sync/state row))
                          (and (= :held (:github.sync/state row)) (= :allocated (:github.sync/held-from row)))
                          (and (:github.sync/incident row)
                               (refused-publication? row (read-body! context row :incident) (second (ready! context row)))))
              (facts/refuse! :not-ready))
            (cleanup! context row prepared (discard-proof! context row prepared))))))))

(defn- dto [context row]
  (let [state (:github.sync/state row)
        retained (:base-retention (preparation! context row))
        outcome (when (:github.sync/outcome row) (read-body! context row :outcome))
        ready (when (:github.sync/ready row) (second (ready! context row)))
        incident (when (:github.sync/incident row) (read-body! context row :incident))]
    (cond-> {:status (case state :creating :preparing :allocated :held :applying :unconfirmed
                           :closed (or (:kind outcome) :closed) state)
             :client-id (str (:github.sync/client-id row))
             :sync-id (str (:github.sync/id row))
             :source (assoc (select-keys (:origin context) [:repository :ref :import-head]) :kind :github)
             :can-discard? (or (#{:ready :allocated} state) (and (= :held state) (= :allocated (:github.sync/held-from row)))
                               (refused-publication? row incident ready))}
      retained (assoc :base-retention (select-keys retained [:status :reason]))
      incident (assoc :reason (:reason incident))
      ready (assoc :ready-id (str (:github.sync/ready-id row)) :changes (:changes ready) :total (count (:changes ready))
                   :before-git-oid (get-in ready [:before :git-oid]) :git-oid (get-in ready [:after :git-oid])
                   :base-head (get-in ready [:before :visible :head]) :head (get-in ready [:after :visible :head])))))

(defn request! [operation request]
  (let [fields (case operation :prepare #{:client-id :expected-target-stamp} :apply #{:client-id :ready-id} :retire #{:sync-id}
                     (:observe :recover :discard) #{:client-id} nil)]
    (when-not (and fields (map? request) (= fields (set (keys request)))
                   (every? #(and (string? %) (uuid? (parse-uuid %)) (= % (str (parse-uuid %)))) (vals request)))
      (facts/refuse! :invalid-request))
    (update-vals request parse-uuid)))

(defn prepare
  "Capture the original App before queueing a fixed authenticated human action."
  [party room-id operation wire-request]
  (let [request (request! operation wire-request)]
    (host/capture
     party room-id operation
     (fn [context]
       (let [id (or (:sync-id request) (journal/request-id party room-id (:client-id request)))]
         (if (and (= :observe operation) (nil? (journal/sync-in (:control-db (:resolved context)) id)))
           {:status :not-observed :client-id (str (:client-id request))}
           (dto context (act! context operation request))))))))

(defn in!
  "Fixed internal dispatch when a caller already holds the original HOST.
   Wire callers must use the captured authenticated entrypoint."
  [context operation request]
  (let [id (or (:sync-id request)
               (journal/request-id (get-in context [:resolved :party]) (get-in context [:resolved :room-id]) (:client-id request)))]
    (if (and (= :observe operation) (nil? (journal/sync-in @(:conn context) id)))
      {:status :not-observed :client-id (str (:client-id request))}
      (dto context (act! context operation request)))))
