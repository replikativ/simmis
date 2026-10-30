(ns is.simm.ops.github-incoming
  "Fixed human HOST preparation and filing of incoming Git changes as ordinary
   frozen Proposals. The journal owns preparation/cleanup; Proposal status owns
   its decision. No imported execution, native retry on observation, or cursor
   advancement during fetch/filing."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [geschichte.git.budget :as budget]
            [geschichte.git.http :as http]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-proposals :as provenance]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.github-candidate-roots :as roots]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-merge :as merge]
            [is.simm.ops.github-native :as native]
            [is.simm.ops.github-review :as review]
            [is.simm.ops.github-roots :as bases]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.github-sync-workspace :as private]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.proposals :as proposals]
            [is.simm.ops.proposal-publication :as publication]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- refuse! [reason]
  (throw (ex-info "Incoming Git changes are held" {:type ::held :reason reason})))

(defn- operation []
  (let [worker (Thread/currentThread) owner (:owner lifetime/*ticket*)]
    (budget/create (assoc (merge/operation-policy) :timeout-ms 120000
                         :cancelled? #(or (.isInterrupted worker) (not (lifetime/ready? owner)))))))

(defn- row! [context id]
  (let [row (journal/sync-in @(:conn context) id)]
    (when-not (and row (= :incoming (journal/kind row))
                   (= (get-in context [:resolved :room-id]) (:github.sync/room row))
                   (or (:cleanup? context) (= (get-in context [:resolved :party]) (:github.sync/by row))))
      (refuse! :different-intent))
    row))

(defn- body! [context row slot]
  (host/current! context)
  (let [ref (get row (keyword "github.sync" (name slot)))
        value (:value (artifact/get-bounded-value (artifact/datahike-store (:conn context)) ref facts/max-artifact-bytes))
        id (if (= :retention slot) (provenance/provenance-id value) (facts/body-id slot value))]
    (when-not (and value (= id (get row (keyword "github.sync" (str (name slot) "-id"))))
                   (= row (row! context (:github.sync/id row))))
      (refuse! :intent-changed))
    value))

(defn- transition!
  ([context row state body] (transition! context row state body (fn [_] [])))
  ([context row state body extra-tx]
  (host/current! context)
  (let [slot ({:allocated :workspace :ready :ready :filing :filing :filed :outcome :held :incident :closed :closure} state)
        report (volatile! nil)
        write! (fn [ref]
                 (vreset! report (d/transact (:conn context)
                             [[:db.fn/call (fn [db]
                                             (host/writer-current! context db)
                                             (into (journal/transition-tx db row state ref (when slot (facts/body-id slot body)))
                                                   (extra-tx db)))]])))]
    (if slot (artifact/publish-value! (artifact/datahike-store (:conn context)) body write!) (write! nil))
    (let [observed (row! context (:github.sync/id row))]
      (when-not (and @report (= row (journal/sync-in (:db-before @report) (:github.sync/id row)))
                     (= observed (journal/sync-in (:db-after @report) (:github.sync/id row)))
                     (= state (:github.sync/state observed)))
        (refuse! :transition-unconfirmed))
      observed))))

(defn- current-cursor! [context db expected]
  (integrations/validate-sync-base! db expected (:github.integration/head expected) (:github.integration/oid expected)))

(defn- preparation! [context row]
  (let [body (body! context row :preparation)]
    (when-not (and (= (:target body) (get-in context [:bound :target]))
                   (= (:target-stamp body) (get-in context [:bound :target-stamp]))
                   (= (:revision body) (:revision context))
                   (= (:origin body) (:origin context))
                   (= (:branch body) (facts/branch (:github.sync/id row) (:github.sync/claim row)))
                   (= (:binding body) {:id (:github.sync/id row) :claim (:github.sync/claim row)
                                      :by (:github.sync/by row) :room (:github.sync/room row)
                                      :client-id (:github.sync/client-id row)}))
      (refuse! :source-settings-changed))
    body))

(defn- blueprint [context row prepared child planned base]
  (let [incoming (repo/commit-by-id child (:upstream-head planned))
        incoming-db (d/commit-as-db @child (:geschichte.commit/snapshot incoming))
        local (d/commit-as-db @child (get-in prepared [:before :checkpoint]))
        candidate (:geschichte.commit/id (repo/head-commit child))
        identity (provenance/incoming-identity
                  (:connection prepared)
                  {:head (get-in prepared [:cursor :github.integration/head])
                   :git-oid (get-in prepared [:cursor :github.integration/oid])
                   :proof-id (get-in prepared [:cursor :github.integration/proof-id])}
                  {:head (:upstream-head planned) :git-oid (:upstream-git-oid planned)})]
    (provenance/validate-provenance!
     {:version 1 :identity identity :key (provenance/incoming-key identity)
      :producer {:sync-id (:github.sync/id row) :claim (:github.sync/claim row) :by (:github.sync/by row)}
      :proposal {:id (random-uuid) :scope (get-in context [:bound :target :registry-system])
                 :review-ref (str "refs/heads/github-review-" (random-uuid))
                 :base (get-in prepared [:before :visible :head]) :head candidate}
      :canonical-basis (changes/basis local)
      :roots {:base {:id (:root base) :head (get-in base [:selection :head]) :basis (get-in base [:selection :basis])}
              :local (roots/descriptor local (get-in prepared [:before :visible :head]))
              :incoming (roots/descriptor incoming-db (:upstream-head planned))
              :candidate (roots/descriptor @child candidate)}})))

(defn- cleanup!
  ([context row prepared checkpoint] (cleanup! context row prepared checkpoint {}))
  ([context row prepared checkpoint extra]
  (let [closing (if (= :closing (:github.sync/state row)) row (transition! context row :closing nil))]
    (host/current! context)
    (private/retire! (get-in context [:bound :conn]) (:lease context) (:branch prepared) checkpoint)
    (transition! context closing :closed (merge extra {:version 1 :id (:github.sync/id row) :released? true :checkpoint checkpoint})))))

(defn- existing-proposal [db key]
  (when-let [reservation (provenance/reservation db key)]
    (let [id (:github.incoming/proposal reservation)
          _ (when-not (uuid? id) (refuse! :incoming-reserved))
          proposal (d/entity db [:proposal/id id])]
      (cond
        (and (= :released (:github.incoming/state reservation)) (nil? (:db/id proposal))) nil
        (= :open (:proposal/status proposal)) (:github.incoming/proposal reservation)
        (#{:dismissed :accepted} (:proposal/status proposal)) nil
        :else (refuse! :incoming-reserved)))))

(defn- prepare-in! [context request]
  (let [control (:conn context) parent (get-in context [:bound :conn])
        id (journal/request-id (get-in context [:resolved :party]) (get-in context [:resolved :room-id]) (:client-id request))]
    (if-let [row (journal/sync-in @control id)]
      (do (preparation! context (row! context id)) row)
      (do
        (when-not (= (:expected-target-stamp request) (get-in context [:bound :target-stamp])) (refuse! :target-changed))
        (let [source (:db (host/current! context))
              {:keys [cursor connection]} (host/connection! context)
              base (bases/retain-base! control source connection (get-in context [:resolved :party])
                                       #(host/current! context) #(host/writer-current! context %))
              before (facts/capture source (:origin context))
              binding {:id id :claim (random-uuid) :by (get-in context [:resolved :party])
                       :room (get-in context [:resolved :room-id]) :client-id (:client-id request)}
              prepared {:version 1 :binding binding :connection connection :cursor cursor
                        :target (get-in context [:bound :target]) :target-stamp (get-in context [:bound :target-stamp])
                        :revision (:revision context) :origin (:origin context) :before before
                        :branch (facts/branch id (:claim binding))}
              report (volatile! nil)]
          (artifact/publish-value!
           (artifact/datahike-store control) prepared
           (fn [ref]
             (host/current! context)
             (vreset! report (d/transact control [[:db.fn/call
                                                  (fn [db] (host/writer-current! context db)
                                                    (current-cursor! context db cursor)
                                                    (journal/prepare-tx db binding ref (facts/body-id :preparation prepared) :incoming))]]))))
          (when-not (and @report (nil? (journal/sync-in (:db-before @report) id))) (refuse! :creation-unconfirmed))
          (let [creating (row! context id) allocated (volatile! nil) operation (operation)
                result (try
                         (private/with-connection!
                           parent (:lease context) (:branch prepared) (:checkpoint before) true
                           #(vreset! allocated (transition! context creating :allocated
                                                           {:version 1 :branch (:branch prepared) :checkpoint (:checkpoint before)}))
                           (fn [child]
                             (host/current! context)
                             (let [fetched (http/fetch! http/send-http child "origin"
                                                        (get-in before [:visible :configuration "remote.origin.url"])
                                                        {:prefixes [(:ref connection)] :haves []
                                                         :import-budget operation :http-body-idle-timeout-ms 15000})
                                   planned (merge/plan-fetched! child fetched
                                                                {:ref (:ref connection) :preparation-id (:github.sync/preparation-id creating)
                                                                 :upstream-base (:github.integration/head cursor) :operation operation})]
                               (if (= (:upstream-head planned) (:github.integration/head cursor))
                                 {:unchanged? true :checkpoint (d/commit-id @child)}
                                 (do
                                   (when-not (get-in planned [:plan :clean?]) (refuse! :conflicts))
                                   (repo/prepare-merge! child (:plan planned))
                                   (repo/commit! child {:message (str "Merge incoming " (get-in connection [:source :repository]))})
                                   (let [body (blueprint context @allocated prepared child planned base)
                                         existing (existing-proposal @control (:key body))
                                         canonical (:db (host/current! context))
                                         inspected (when-not existing (review/inspect canonical
                                                                   (d/commit-as-db canonical (get-in body [:roots :local :basis :commit]))
                                                                   @child body))]
                                     (if existing
                                       {:existing-proposal existing :checkpoint (d/commit-id @child)}
                                       (do
                                     (when-not (= :ready (:status inspected)) (refuse! (:reason inspected)))
                                     (current-cursor! context @control cursor)
                                     (roots/retain! control canonical @allocated body
                                                    #(host/current! context) #(host/writer-current! context %))
                                     {:checkpoint (d/commit-id @child) :proposal (get-in body [:proposal :id])
                                      :changes (:changes inspected)}))))))))
                         (catch Exception error
                           (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)) (throw error))
                           (when (and @allocated (= ::private/body-failed (:type (ex-data error)))
                                      (true? (:released? (ex-data error))) (uuid? (:checkpoint (ex-data error))))
                             (let [current (row! context id)
                                   cause (ex-cause error)]
                               (transition! context current :held
                                            {:version 1 :kind :preparation-failed :released? true
                                             :branch (:branch prepared) :checkpoint (:checkpoint (ex-data error))
                                             :reason (if (= ::held (:type (ex-data cause))) (:reason (ex-data cause)) :preparation-failed)})))
                           (throw error)))]
            (if (or (:unchanged? result) (:existing-proposal result))
              (let [closed (cleanup! context (row! context id) prepared (:checkpoint result)
                                     (when-let [existing (:existing-proposal result)] {:kind :existing-proposal :proposal existing}))]
                (when-let [existing (:existing-proposal result)]
                  (publication/publish-original! existing (:resolved context)))
                closed)
              ;; All native roots exist and the private connection has physically
              ;; returned before READY. A journal read cannot mint new roots.
              (transition! context (row! context id) :ready (assoc result :version 1)))))))))

(defn- filing-entity [row body reservation]
  (let [connection (get-in body [:identity :connection])]
    (proposals/prepare-filing
     {:id (get-in body [:proposal :id]) :title (str "Sync " (get-in connection [:source :repository]))
      :summary "Incoming Git changes merged with the saved Memory. Review this frozen change set before accepting."
      :author (:github.sync/by row) :room (:github.sync/room row)
      :attrs (cond-> {:proposal/github-key (:key body) :proposal/github-provenance (:github.sync/retention row)
                       :proposal/github-provenance-id (provenance/provenance-id body)}
               reservation (assoc :proposal/github-replaces (:github.incoming/proposal reservation)))
      :forks [{:scope (get-in body [:proposal :scope]) :authority-scope (:github.sync/room row)
               :branch (subs (get-in body [:proposal :review-ref]) 11) :system-type :repo :author (:github.sync/by row)
               :base-commit (str (get-in body [:proposal :base])) :head-commit (str (get-in body [:proposal :head]))}]})))

(defn- file-in! [context request]
  (let [id (or (:sync-id request)
               (journal/request-id (get-in context [:resolved :party]) (get-in context [:resolved :room-id]) (:client-id request)))
        row (row! context id) prepared (preparation! context row) ready (body! context row :ready)
        body (body! context row :retention) control (:conn context) parent (get-in context [:bound :conn])]
    (when-not (and (= :ready (:github.sync/state row)) (= (:ready-id request) (:github.sync/ready-id row)))
      (refuse! :not-ready))
    (current-cursor! context @control (:cursor prepared))
    (roots/verify! control (:db (host/current! context)) row body #(host/current! context))
    (let [reservation (provenance/reservation @control (:key body))
          entity (filing-entity row body (when-let [previous (:github.incoming/proposal reservation)]
                                          (when (:db/id (d/entity @control [:proposal/id previous])) reservation)))
          _ (provenance/validate-proposal! body entity)
          intent {:version 1 :proposal entity :provenance-id (provenance/provenance-id body)}
          filing (transition! context row :filing intent
                              #(provenance/reserve-tx % (:key body) (:proposal/id entity) id reservation))
          reserved (provenance/reservation @control (:key body))
          _ (when-not (and (= id (:github.incoming/sync reserved))
                            (= (:proposal/id entity) (:github.incoming/proposal reserved)))
              (refuse! :incoming-reserved))
          result (try (private/with-connection!
                   parent (:lease context) (:branch prepared) (:checkpoint ready) false
                   (fn [child]
                     (native/publish! parent child (get-in body [:proposal :review-ref])
                                      (get-in body [:proposal :head]) (operation) #(host/current! context))))
                      (catch Exception error
                        (let [cause (some #(when (#{:geschichte.workspace/stale-publication :geschichte.workspace/publication-ref-exists
                                                   :is.simm.ops.github-native/held :geschichte.git.budget/exceeded
                                                   :geschichte.git.budget/deadline-exceeded :geschichte.git.budget/cancelled}
                                                 (:type (ex-data %))) %) (take 16 (take-while some? (iterate ex-cause error))))]
                          (when (and cause (= ::private/body-failed (:type (ex-data error)))
                                     (true? (:released? (ex-data error))) (= (:checkpoint ready) (:checkpoint (ex-data error))))
                            (let [incident {:version 1 :kind :filing-refused :released? true :checkpoint (:checkpoint ready)
                                            :reason (if (= :geschichte.workspace/stale-publication (:type (ex-data cause)))
                                                      :target-changed :publication-refused)}]
                              ;; Facts-only finalization of the original known
                              ;; refusal, not a fresh grant after permission loss.
                              (artifact/publish-value!
                               (artifact/datahike-store control) incident
                               (fn [ref]
                                 (d/transact control [[:db.fn/call
                                                       (fn [db]
                                                         (into (journal/transition-tx db filing :held ref (facts/body-id :incident incident))
                                                               (provenance/release-reservation-tx db (:key body) reserved)))]]))))))
                        (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
                        (throw error)))
          _ (when-not (and (true? (:created? result)) (nil? (:old result)) (false? (:forced? result))
                            (= (get-in body [:proposal :review-ref]) (:ref result))
                            (= (get-in body [:proposal :head]) (:new result))
                            (= (get-in body [:roots :candidate :basis :commit]) (:workspace-commit result))
                            (uuid? (:expected-checkpoint result)))
              (refuse! :publication-unconfirmed))
          outcome {:version 1 :kind :filed :proposal (:proposal/id entity) :native result}
          report (volatile! nil)]
      ;; Original positively returned publisher records its admitted fact. No
      ;; fresh permission/selection lookup occurs after the irreversible effect.
      (artifact/publish-value!
       (artifact/datahike-store control) outcome
       (fn [ref]
         (vreset! report
                  (d/transact control
                              [[:db.fn/call (fn [db]
                                              (when-not (= filing (journal/sync-in db id)) (refuse! :intent-changed))
                                              (when-not (= reserved (provenance/reservation db (:key body)))
                                                (refuse! :incoming-reserved))
                                              (current-cursor! context db (:cursor prepared))
                                              (into (journal/transition-tx db filing :filed ref (facts/body-id :outcome outcome))
                                                    (concat (proposals/filing-tx-data db entity)
                                                            [{:db/id (:db/id reserved) :github.incoming/state :filed}])))]]))))
      (when-not @report (refuse! :filing-unconfirmed))
      (let [filed (row! context id)]
        ;; Durable roots own the frozen candidate independently of this private
        ;; preparation branch. Close only after the filing is positively durable.
        (cleanup! context filed prepared (:checkpoint ready))
        (publication/publish-original! (:proposal/id entity) (:resolved context))
        (row! context id)))))

(defn- discard-in! [context row]
  (let [prepared (preparation! context row)
        ready (when (:github.sync/ready row) (body! context row :ready))
        incident (when (:github.sync/incident row) (body! context row :incident))]
    (when-not (or (#{:ready :allocated} (:github.sync/state row))
                  (and (= :held (:github.sync/state row))
                       (or (#{:allocated :retaining} (:github.sync/held-from row))
                           (and (= :filing (:github.sync/held-from row)) (= :filing-refused (:kind incident))))
                       (true? (:released? incident))))
      (refuse! :cleanup-unconfirmed))
    (cleanup! context row prepared (or (:checkpoint ready) (:checkpoint incident)
                                      (private/retained-checkpoint! (get-in context [:bound :conn]) (:branch prepared)
                                                                    (get-in prepared [:before :checkpoint]))))))

(defn- dto [context row]
  (let [ready (when (:github.sync/ready row) (body! context row :ready))
        incident (when (:github.sync/incident row) (body! context row :incident))
        outcome (or (when (:github.sync/outcome row) (body! context row :outcome))
                    (when (:github.sync/closure row)
                      (let [closure (body! context row :closure)]
                        (when (= :existing-proposal (:kind closure)) closure))))
        state (:github.sync/state row)]
    (cond-> {:operation-kind :incoming :client-id (str (:github.sync/client-id row)) :sync-id (str (:github.sync/id row))
             :status (case state :creating :preparing :allocated :held :retaining :held :filing :unconfirmed
                           :closed (if outcome :filed :closed) state)
             :can-discard? (or (= :ready state) (and (= :held state) (true? (:released? incident))))}
      ready (assoc :ready-id (str (:github.sync/ready-id row)) :changes (:changes ready) :total (count (:changes ready)))
      incident (assoc :reason (:reason incident))
      outcome (assoc :proposal-id (str (:proposal outcome))))))

(defn- recover-in! [context row]
  (let [closed (case (:github.sync/state row)
                 :closed row
                 (:filed :closing)
                 (do
                   (when-not (:github.sync/outcome row) (refuse! :recovery-held))
                   (cleanup! context row (preparation! context row) (:checkpoint (body! context row :ready))))
                 (refuse! :recovery-held))
        outcome (or (when (:github.sync/outcome closed) (body! context closed :outcome))
                    (when (:github.sync/closure closed) (body! context closed :closure)))]
    (when-let [proposal (:proposal outcome)]
      (publication/publish-original! proposal (:resolved context)))
    closed))

(defn in!
  "Fixed synchronous internal dispatch inside the original captured HOST."
  [context action request]
  (let [id (or (:sync-id request)
               (journal/request-id (get-in context [:resolved :party]) (get-in context [:resolved :room-id]) (:client-id request)))
        current (journal/sync-in @(:conn context) id)]
    (if (and (= :observe action) (nil? current))
      {:status :not-observed :client-id (str (:client-id request))}
      (dto context
           (case action
             :prepare (prepare-in! context request)
             :apply (file-in! context request)
             :observe (row! context id)
             :recover (recover-in! context (row! context id))
             :retire (let [row (row! context id)]
                       (if (or (:github.sync/outcome row) (= :closed (:github.sync/state row)))
                         (recover-in! context row) (discard-in! context row)))
             :discard (discard-in! context (row! context id))
             (refuse! :recovery-held))))))
