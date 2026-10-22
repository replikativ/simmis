(ns is.simm.ops.file-output-controller-native-probe
  "OPT-IN isolated native adoption diagnostic, not a default test or inference
   benchmark. The producer is synthetic trusted pre-certified input: retention,
   joint filing, authority, native publication and adoption are real. No Attempt,
   agent execution, provider, paid wallet or provisioned-home claim is made."
  (:require [clojure.test :refer [is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as catalog]
            [dvergr.rooms.files :as native-files]
            [dvergr.rooms.publication :as native-publication]
            [dvergr.runtime.preparation :as preparation]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.space :as space]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-controller :as controller]
            [is.simm.ops.file-output-service :as service]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.file-output-publication :as publication]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.owned-workspace-test :as fixture]
            [is.simm.ops.proposals :as proposals]
            [is.simm.agents.room-agents :as live-rooms]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.application-ops-test :as application-fixture]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.adapters.geschichte :as gy]))

(def ^:private owned-source "(ns owned.workflow)\n(def task :owned-only)\n")
(def ^:private attached-source "(ns attached.workflow)\n(def task :never-owned)\n")
(def ^:private file-id #uuid "45da12b7-830a-4116-a9b8-22fe4b25ae23")
(defn- canonical-source [text] (str "---\nsimmis-id: '" file-id "'\n---\n" text))
(def ^:private after-text (canonical-source "# Adopted native output\n\nRetained and merged through the exact owned workspace.\n"))
(def ^:private human-text (canonical-source "# Human refinement\n\nMade after publication; recovery must not overwrite it.\n"))

(defn- with-canonical-fixture [owned-first? f]
  ;; Provision NEW test stores before they are published. Do not reclassify an
  ;; existing legacy repository just to make a new canonical guard pass.
  (let [opened (atom []) seed! @#'fixture/seed!]
    (with-redefs-fn
      {#'catalog/open-memory-workspace!
       (fn [room]
         (let [cfg (assoc workspace/repository-options :attribute-refs? false
                          :store {:backend :memory :id (random-uuid)})]
           (d/create-database cfg)
           (let [conn (d/connect cfg)]
             (swap! opened conj conn)
             (space/provision! conn)
             (binding [ec/*execution-context* (:ctx room)]
               (ygg/register! (gy/create conn {:system-name (str "canonical-probe-" (random-uuid))})))
             conn)))
       #'fixture/seed! (fn [conn text] (seed! conn (canonical-source text)))}
      #(@#'fixture/with-fixture
        owned-first?
        (fn [state]
          (try (f state)
               (finally (doseq [conn @opened] (governance/ungovern! conn)))))))))

(defn- tree [db]
  (into (sorted-map)
        (map (fn [datom] [(:v datom) (files/entry db (:v datom))]))
        (d/datoms db :aevt :geschichte.work/path)))

(defn- fact-tx [db eid attr]
  (:tx (first (d/datoms db :eavt eid attr))))

(defn- seed-source! [conn source]
  (repo/write! conn "workflows/fixture.clj" (markdown/source-bytes source))
  (repo/stage-all! conn)
  (repo/commit! conn {:message "Distinct diagnostic workflow source"}))

(defn- synthetic-file!
  "Trusted TEST producer seam, not a new host authority or certification lane.
   Exact frozen request provenance and retained body refs are joined with the
   Proposal in the actual serialized control writer under real GC guards."
  [{:keys [conn w room owned]} output]
  (let [binding (owned/resolve! @conn (:owner w) :merge (:room w) room)
        captured (inputs/capture (:db binding) inputs/default-policy)
        basis (inputs/publication-basis (:db binding)
                                        (select-keys (:target binding) [:registry-system :runtime-system]))
        input {:workflow {:input-selection {:execution-id (:execution-id captured)}}
               :knowledge/publication-basis basis :fixture :synthetic-pre-certified}
        row (:request (requests/claim! conn {:party (:owner w) :room (:room w)
                                             :client-id (random-uuid) :kind :workflow :intent input}))
        request (:work.request/id row) job (random-uuid) candidate (random-uuid)
        _ (requests/record-job! conn request (:work.request/claim row) job)
        _ (requests/mark-returned! conn request)
        _ (is (= basis (requests/publication-basis conn request)))
        frozen (dispositions/request-binding (requests/get-request conn request))
        capture (outputs/capture-db @output)
        plan (plans/plan {:request request :room (:room w) :attempt candidate
                          :environment {:fixture :synthetic-pre-certified} :workflow-bundle (random-uuid)
                          :publication-basis basis :output capture :retained-inputs (:inputs captured)})
        manifest (:manifest plan)
        value (dispositions/make {:request request :intent (:work.request/intent-id frozen)
                                  :job job :candidate candidate :attempt-content (random-uuid)
                                  :basis (:id basis) :output (:id capture) :kind :proposed
                                  :manifest (:id manifest) :proposal (:proposal plan)})]
    (storage/publish!
     conn plan
     (fn [retained]
       (let [entity (proposals/prepare-filing
                     {:id (:proposal plan) :title "Native adoption diagnostic"
                      :summary "Synthetic pre-certified fixture; actual retained native adoption."
                      :author (:owner w) :room (:room w) :run candidate
                      :forks [{:scope (:id manifest) :authority-scope (:room w)
                               :branch (str "files-" (:id manifest)) :system-type :file-output
                               :file-output retained}]})]
         (d/transact conn [[:db.fn/call
                            (fn [db]
                              (into (proposals/filing-tx-data db entity)
                                    (dispositions/tx-data db frozen value)))]]))))
    (is (= value (dispositions/disposition (requests/get-request conn request))))
    (is (= manifest (storage/manifest conn (:id manifest))))
    {:plan plan :disposition value}))

(defn- adopt-or-recover! [{:keys [conn w room owned]} plan recovery?]
  (if-not recovery?
    {:result (controller/adopt! conn (:owner w) (:room w) room (:proposal plan))
     :publication-db @owned :current-party (:owner w) :expected-text after-text}
    (let [failure (with-redefs [adoption/finalize!
                                (fn [& _]
                                 ;; The real writer has ALREADY returned its
                                 ;; winning receipt. Simulate loss before the
                                 ;; control terminal transaction, not fake IO.
                                  (throw (ex-info "Control finalization unavailable after native publication" {})))]
                    (try (controller/adopt! conn (:owner w) (:room w) room (:proposal plan))
                         nil
                         (catch clojure.lang.ExceptionInfo error error)))
          operation-id (:operation (ex-data failure))
          operation (adoptions/operation @conn operation-id)
          publication-db @owned
          winning (native-publication/recover publication-db (publication/identity operation (:manifest plan)))]
      (is (= ::controller/pending (:type (ex-data failure))))
      (is (= :applying (:state operation)))
      (is (map? winning))
      (is (= winning (publication/validate-receipt! operation (:manifest plan) winning)))
      ;; Change the SAME published file, commit it, then switch logical ref.
      ;; Historical reconciliation must observe the original fact, not replay
      ;; its bytes into this later checkout or require its old HEAD.
      (repo/write! owned "wiki/file.md" (markdown/source-bytes human-text))
      (repo/write! owned "wiki/human.md" (markdown/source-bytes "Later human work\n"))
      (repo/stage-all! owned)
      (repo/commit! owned {:message "Human refinement after native publication"})
      (repo/branch! owned "later-checkout")
      (repo/checkout! owned "later-checkout")
      (is (not= (:publication/ref winning) (query/current-ref @owned)))
      (let [reconciler (random-uuid)
            _ (d/transact conn [{:party/id reconciler}
                                [:db/add [:room/id (:room w)] :room/owner [:party/id reconciler]]
                                [:db/add [:room/id (:room w)] :room/parties [:party/id reconciler]]])
            before @owned
            forbidden (fn [& _] (throw (AssertionError. "Recovery attempted native publication or body IO")))
            recovered (with-redefs [native-files/write-to! forbidden
                                    native-files/write-to-completion! forbidden
                                    native-publication/publish! forbidden
                                    storage/read-file forbidden
                                    storage/read-file-in forbidden]
                        (adoption/recover! conn reconciler (:room w) room (:proposal plan)))
            accepted (adoptions/operation @conn operation-id)]
        (is (= :accepted (:status recovered)))
        (is (false? (:already-accepted? recovered)))
        (is (= (:owner w) (:actor accepted) (:actor recovered)))
        (is (= reconciler (:reconciled-by accepted)))
        (is (= before @owned))
        (let [repeated (with-redefs [native-files/write-to! forbidden
                                     native-files/write-to-completion! forbidden
                                     native-publication/publish! forbidden
                                     storage/read-file forbidden
                                     storage/read-file-in forbidden]
                         (adoption/recover! conn reconciler (:room w) room (:proposal plan)))]
          (is (true? (:already-accepted? repeated)))
          (is (= accepted (adoptions/operation @conn operation-id)))
          (is (= before @owned)))
        {:result {:status (:status recovered) :operation accepted}
         :publication-db publication-db :current-party reconciler :expected-text human-text}))))

(defn- native-probe*
  "Run once with true and once with false for BOTH registration orders.
   Calls the real native writer: an unavailable explicit writer throws rather
   than silently skipping. Uses real canonical control mappings/default grants,
   two registered native systems, current EACL, retention, joint filing, journal,
   native receipt and terminal acceptance. Only upstream file-backed default
   provisioning/UUID-to-context discovery is scoped to the existing memory
   fixture descriptor. This does NOT prove agent certification or source routing."
  [owned-first? recovery?]
  (when-not (and (boolean? owned-first?)
                 (contains? native-files/write-capabilities :explicit-workspace)
                 (ns-resolve 'dvergr.rooms.files 'write-to-completion!))
    (throw (ex-info "Native adoption diagnostic requires the actual explicit native writer"
                    {:type ::unavailable})))
  (let [configs (atom []) room-store-conn (atom nil)]
    (try
      (with-canonical-fixture
        owned-first?
        (fn [{:keys [conn w room owned attached attached-descriptor] :as state}]
          (let [store-conn (:conn (:store room))]
            (when-not store-conn
              (throw (ex-info "Diagnostic requires the actual native room store" {})))
            (reset! room-store-conn store-conn)
            (swap! configs into (map #(get @% :config) [conn owned attached store-conn])))
          (let [cfg (assoc workspace/repository-options :store {:backend :memory :id (random-uuid)})]
            (d/create-database cfg)
            (swap! configs conj cfg)
            (let [output (d/connect cfg)]
              (try
                (repo/init! output {:name "Isolated synthetic producer"})
                (seed-source! owned owned-source)
                (seed-source! attached attached-source)
                (repo/write! output "out/wiki/file.md" (markdown/source-bytes after-text))
                (d/transact conn adoptions/schema)
                (let [owned-before @owned attached-before @attached
                      selected (workspace/selected-system-id)
                      {:keys [plan disposition]} (synthetic-file! state output)
                      proposal (:proposal plan)
                      _ (is (= (:runtime-system attached-descriptor) selected))
                      _ (is (not= (tree owned-before) (tree attached-before)))
                      _ (is (= owned-source (:text (files/read-file-in owned-before "workflows/fixture.clj"))))
                      _ (is (= attached-source (:text (files/read-file-in attached-before "workflows/fixture.clj"))))
                      _ (is (= (tree owned-before) (tree @owned)))
                      _ (is (= (tree attached-before) (tree @attached)))
                      {:keys [result publication-db current-party expected-text]}
                      (adopt-or-recover! state plan recovery?)
                      db @conn operation (:operation result)
                      p (d/pull db '[:proposal/status :proposal/resolved-by :proposal/resolved-at
                                     {:proposal/forks [:db/id :proposal.fork/status
                                                       :proposal/resolved-by :proposal/resolved-at
                                                       {:proposal.fork/file-adoption [:file.adoption/id]}]}]
                                [:proposal/id proposal])
                      fork (first (:proposal/forks p))
                      winning (native-publication/recover @owned (publication/identity operation (:manifest plan)))]
                  (is (= :accepted (:status result) (:state operation)
                         (:proposal/status p) (:proposal.fork/status fork)))
                  (is (uuid? (:id operation)))
                  (is (= operation (adoptions/operation db (:id operation))))
                  (is (= (:id operation) (get-in fork [:proposal.fork/file-adoption :file.adoption/id])))
                  (is (= (:owner w) (:actor operation) (:proposal/resolved-by p) (:proposal/resolved-by fork)))
                  (is (= (:updated-at operation) (:proposal/resolved-at p) (:proposal/resolved-at fork)))
                  (let [journal-eid (:db/id (d/entity db [:file.adoption/id (:id operation)]))
                        proposal-eid (:db/id (d/entity db [:proposal/id proposal]))
                        txs [(fact-tx db journal-eid :file.adoption/state)
                             (fact-tx db proposal-eid :proposal/status)
                             (fact-tx db (:db/id fork) :proposal.fork/status)]]
                    (is (every? integer? txs))
                    (is (apply = txs)))
                  (is (map? winning))
                  (is (= winning (publication/validate-receipt! operation (:manifest plan) winning)))
                  (is (= (:head operation) (:publication/head winning)))
                  (is (false? (:publication/noop? winning)))
                  (let [target-db publication-db
                        ref-eid (:db/id (d/entity target-db [:geschichte.ref/name (:publication/ref winning)]))
                        commit-eid (:db/id (d/entity target-db [:geschichte.commit/id (:publication/commit winning)]))
                        txs [(fact-tx target-db (:db/id winning) :publication/id)
                             (fact-tx target-db ref-eid :geschichte.ref/target)
                             (fact-tx target-db commit-eid :geschichte.commit/id)]]
                    (is (every? integer? txs))
                    (is (apply = txs)))
                  (is (= expected-text (:text (files/read-file-in @owned "wiki/file.md"))))
                  (is (= owned-source (:text (files/read-file-in @owned "workflows/fixture.clj"))))
                  (is (= (tree attached-before) (tree @attached)))
                  (is (= (query/current-ref attached-before) (query/current-ref @attached)))
                  (is (= (query/refs attached-before) (query/refs @attached)))
                  (is (= selected (workspace/selected-system-id)))
                  (is (= disposition (dispositions/disposition
                                      (requests/get-request conn (:request disposition)))))
                  (is (= 1 (count (d/datoms db :aevt :file.adoption/id))))
                  (is (= 1 (count (d/datoms @owned :aevt :publication/id))))
                 ;; Accepted replay is intentionally NOT a second publisher or
                 ;; an accepted read API: the ordinary open-only guard refuses.
                  (let [control-before @conn target-before @owned
                        forbidden (fn [& _] (throw (AssertionError. "Accepted repeat attempted body IO/native dispatch")))]
                    (is (every? integer? [(:max-tx control-before) (:max-tx target-before)]))
                    (with-redefs-fn {#'controller/native-writer forbidden
                                     #'storage/read-file-in forbidden
                                     #'storage/read-file forbidden}
                      #(is (thrown? clojure.lang.ExceptionInfo
                                    (controller/adopt! conn current-party (:room w) room proposal))))
                    (is (= (:max-tx control-before) (:max-tx @conn)))
                    (is (= (:max-tx target-before) (:max-tx @owned)))
                    (is (= (tree target-before) (tree @owned)))
                    (is (= (query/refs target-before) (query/refs @owned)))
                    (is (= operation (adoptions/operation @conn (:id operation))))
                    (is (= 1 (count (d/datoms @conn :aevt :file.adoption/id))))
                    (is (= 1 (count (d/datoms @owned :aevt :publication/id)))))
                  {:owned-first? owned-first? :status (:status result)
                   :operation (:id operation) :proposal proposal :head (:head operation)
                   :positive-recovery? recovery?
                   :synthetic-producer? true :agent-certification-tested? false})
                (finally (d/release output)))))))
      (finally
        ;; The existing fixture has synchronously unregistered and closed its
        ;; Room before returning. Only these captured UUID memory stores are
        ;; deleted; never delete live app homes or re-create stores to inspect.
        (when-let [conn @room-store-conn] (d/release conn))
        (doseq [cfg (distinct @configs)]
          (when-not (and (= :memory (get-in cfg [:store :backend]))
                         (uuid? (get-in cfg [:store :id])))
            (throw (ex-info "Refuse non-fixture cleanup" {:type ::invalid-cleanup})))
          (d/delete-database cfg))))))

(defn native-adoption-probe
  "OPT-IN ordinary native adoption in one registration order. No agent/model
   certification claim: see namespace and native-probe* for fixture boundaries."
  [owned-first?]
  (native-probe* owned-first? false))

(defn native-positive-recovery-probe
  "OPT-IN actual native receipt -> unavailable control finalizer -> current-
   authorized positive reconciliation, preserving later human edits/checkout.
   A different reconciler accepts; repeat observes audit without republishing.
   Run true AND false for both real repository registration orders. Synthetic
   trusted producer remains separate from prior agent certification evidence."
  [owned-first?]
  (native-probe* owned-first? true))

(defn native-reviewed-adoption-probe
  "OPT-IN actual canonical preview -> valid new context -> stale refusal before
   claim -> explicit fresh review -> native publication with immutable review
   audit. Synthetic producer; no agent or paid inference certification claim."
  [owned-first?]
  (let [accepted-revision (atom nil)]
    (with-redefs [controller/adopt!
                  (fn [conn party room-id room proposal]
                    (let [before-control @conn
                          binding (owned/resolve! before-control party :merge room-id room)
                          target (:conn binding) before-target @target
                          preview (controller/preview-reviewed! conn party room-id room proposal)]
                      (is (= :reviewed (:status preview)))
                      (is (= (:max-tx before-control) (:max-tx @conn)))
                      (is (= (:max-tx before-target) (:max-tx @target)))
                      ;; A canonical, identified additional context document is
                      ;; valid, but was not part of the earlier human review.
                      (repo/write! target "wiki/review-context.md"
                                   (markdown/source-bytes
                                    (str "---\nsimmis-id: '" (random-uuid) "'\n---\n# New review context\n")))
                      (let [failure (try (controller/adopt-reviewed! conn party room-id room proposal (:revision preview))
                                         nil (catch clojure.lang.ExceptionInfo error error))]
                        (is (= :review-changed (:reason (ex-data failure)))))
                      (is (empty? (d/datoms @conn :aevt :file.adoption/id)))
                      (is (empty? (d/datoms @target :avet :db/ident :publication/id)))
                      (let [fresh (controller/preview-reviewed! conn party room-id room proposal)]
                        (is (not= (:revision preview) (:revision fresh)))
                        (reset! accepted-revision (:revision fresh))
                        (let [result (controller/adopt-reviewed! conn party room-id room proposal (:revision fresh))]
                          (is (= 2 (get-in result [:operation :version])))
                          (is (= (:revision fresh) (get-in result [:operation :review-revision])))
                          result))))]
      (assoc (native-probe* owned-first? false) :review-revision @accepted-revision))))

(defn native-service-adoption-probe
  "OPT-IN fixed human service over real EACL, canonical filing, application
   installation and Room leases. A blocked retained body read holds shutdown
   until physical exit; after a new installation the explicit review is accepted.
   Only registry UUID->memory fixture hydration is scoped, as above."
  [owned-first?]
  (with-redefs [controller/adopt!
                (fn [conn party room-id room proposal]
                  ;; The shared diagnostic also probes ordinary accepted replay
                  ;; with all body IO forbidden. Do not restart this blocked-
                  ;; read scenario for that deliberately closed second call.
                  (when (= :accepted (:proposal/status (d/pull @conn [:proposal/status] [:proposal/id proposal])))
                    (throw (ex-info "Accepted diagnostic repeat is not a publication" {})))
                  (with-redefs [context/server-context (:ctx room)
                                live-rooms/live-room (fn [id]
                                                       (when-not (= room-id id)
                                                         (throw (ex-info "Wrong fixture Room" {})))
                                                       room)]
                    (@#'application-fixture/isolated
                     (fn []
                       (application/start!)
                       (let [entered (promise) release (promise) joining (promise)
                             read! storage/read-file-in join! lifetime/join!
                             preview
                             (with-redefs [storage/read-file-in
                                           (fn [& args]
                                             (deliver entered true)
                                             @release
                                             (apply read! args))
                                           lifetime/join!
                                           (fn [owner timeout]
                                             (deliver joining true)
                                             (join! owner timeout))]
                               (let [work (future (service/preview! party proposal))]
                                 (try
                                   (is (true? (deref entered 10000 nil)))
                                   (let [shutdown (future (application/quiesce!))]
                                     (try
                                       (is (true? (deref joining 10000 nil)))
                                       (is (not (realized? work)))
                                       (is (not (realized? shutdown)))
                                       (is (= {:status :unavailable} (service/preview! party proposal)))
                                       (finally (deliver release true)))
                                     ;; Actual physical joins, never future
                                     ;; cancellation as an acknowledgement.
                                     @shutdown
                                     @work)
                                   (finally (deliver release true) @work))))]
                         (is (= :reviewed (:status preview)))
                         (application/stop!)
                         (application/start!)
                         (let [result (service/adopt! party proposal (parse-uuid (:revision preview)))
                               operation (when-let [id (:operation result)]
                                           (adoptions/operation @conn (parse-uuid id)))]
                           (is (= :accepted (:status result)))
                           (is (= 2 (:version operation)))
                           (is (= (:revision preview) (str (:review-revision operation))))
                           {:status (:status result) :operation operation}))))))]
    (native-probe* owned-first? false)))

(defn- durable-state [conn]
  (let [db @conn]
    {:config (:config db) :commit (:datahike/commit-id (meta db))
     :max-tx (:max-tx db) :tree (tree db) :refs (query/refs db)}))

(defn- join-native! [^Thread thread]
  (loop [interrupted? false]
    (let [done? (try (.join thread) true (catch InterruptedException _ false))]
      (if done?
        (when interrupted? (.interrupt (Thread/currentThread)))
        (recur true)))))

(defn native-completion-probe
  "OPT-IN actual fixed completion -> original controller handling. Run BOTH
   registration orders for each closed mode. Synthetic producer only; native
   gate, journal, EACL/default binding and publication remain real. No workers,
   providers or scheduled/default activation. A saved continuation is invoked
   only synchronously after the native envelope sealed, never detached."
  [owned-first? mode]
  (when-not (and (boolean? owned-first?)
                 (#{:stale :skip :post-entry :positive-error :replacement :interrupt :lost-applying} mode))
    (throw (ex-info "Unknown native completion diagnostic" {})))
  (let [configs (atom []) chat (atom nil) completed? (atom false)
        result
        (try
          (let [result
                (with-canonical-fixture
                  owned-first?
                  (fn [{:keys [conn w room owned attached attached-descriptor] :as state}]
                    (let [room-conn (:conn (:store room))
                          cfg (assoc workspace/repository-options :store {:backend :memory :id (random-uuid)})]
                      (reset! chat room-conn)
                      (swap! configs into (map #(get @% :config) [conn owned attached room-conn]))
                      (d/create-database cfg)
                      (swap! configs conj cfg)
                      (let [output (d/connect cfg)]
                        (try
                          (repo/init! output {:name "Sealed completion fixture"})
                          (repo/write! output "out/wiki/file.md" (markdown/source-bytes after-text))
                          (d/transact conn adoptions/schema)
                          (let [{:keys [plan disposition]} (synthetic-file! state output)
                                before (durable-state owned) other-before (durable-state attached)
                                selected (workspace/selected-system-id)
                                native native-files/write-to-completion!
                                applying! adoption/bound-applying!
                                saved (atom nil) envelopes (atom []) stale-before (atom nil)
                                run!
                                (fn []
                                  (with-redefs [adoption/bound-applying!
                                                (fn [& args]
                                                  (let [ack (apply applying! args)]
                                                    (when (= :lost-applying mode)
                                                     ;; REAL applying committed,
                                                     ;; but the controller has
                                                     ;; not received its ACK.
                                                      (throw (ex-info "Lost actual applying callback ACK" {})))
                                                    ack))
                                                native-files/write-to-completion!
                                                (fn [r system batch opts]
                                                  (is (some #(= :host-operation (:kind %))
                                                            (vals (get @(:meta room) preparation/leases-key)))
                                                      "actual native dispatch retains its exact Room")
                                                  (when (= :stale mode)
                                                   ;; Race AFTER canonical preflight/claim but before native
                                                   ;; dispatch: retain the native no-entry cleanup scenario.
                                                    (repo/write! owned "wiki/file.md" (markdown/source-bytes human-text))
                                                    (reset! stale-before (durable-state owned)))
                                                  (let [boundary
                                                        (case mode
                                                          (:skip :replacement :interrupt)
                                                          (constantly {:handlers [(fn [effect next]
                                                                                    (reset! saved #(next effect))
                                                                                    (when (= :interrupt mode)
                                                                                      (.interrupt (Thread/currentThread)))
                                                                                    {:applied? true :publication {:publication/id (random-uuid)}})]})
                                                          :post-entry
                                                          (constantly {:handlers [(fn [effect next]
                                                                                    (let [value (next effect)]
                                                                                      (when (= :fs/write (:effect effect))
                                                                                        (throw (ex-info "After actual native write entry" {})))
                                                                                      value))]})
                                                          :positive-error
                                                          (constantly {:handlers [(fn [effect next]
                                                                                    (let [value (next effect)]
                                                                                      (when (= :git/commit (:effect effect))
                                                                                        (throw (ex-info "After actual winning publication" {})))
                                                                                      value))]})
                                                          nil)
                                                        closed (native r system batch (cond-> opts boundary (assoc :boundary boundary)))]
                                                    (swap! envelopes conj closed)
                                                    (when (= :replacement mode)
                                                      (d/transact conn [{:room/id (:room w)
                                                                         :room/default-repository
                                                                         [:system/id (:registry-system attached-descriptor)]}]))
                                                    closed))]
                                    (try (controller/adopt! conn (:owner w) (:room w) room (:proposal plan))
                                         (catch clojure.lang.ExceptionInfo error (ex-data error)))))
                               ;; Interruption executes synchronously on an
                               ;; owned joined thread; never poison REPL caller.
                                answer (if (= :interrupt mode)
                                         (let [answer (promise)
                                               t (Thread. ^Runnable
                                                  (bound-fn []
                                                    (try (deliver answer {:result (run!)
                                                                          :interrupted? (.isInterrupted (Thread/currentThread))})
                                                         (catch Throwable error (deliver answer {:error error})))))]
                                           (.start t)
                                           (try (join-native! t)
                                                (let [value (deref answer 1000 ::missing-after-physical-exit)]
                                                  (is (true? (:interrupted? value)))
                                                  (when-let [error (:error value)] (throw error))
                                                  (:result value))
                                                (finally (join-native! t))))
                                         (run!))
                                operation (:operation answer)
                                op (adoptions/operation @conn (if (map? operation) (:id operation) operation))
                                expected (case mode (:stale :skip) :aborted :positive-error :accepted
                                               :lost-applying :applying :claimed)]
                            (is (= 1 (count @envelopes)))
                            (is (= (case mode (:post-entry :lost-applying) :pending :positive-error :returned :not-started)
                                   (:completion (first @envelopes))))
                            (is (= expected (:state op)))
                            (is (= (if (= :accepted expected) :accepted (if (= :aborted expected) :aborted :pending))
                                   (:status answer)))
                            (when (#{:stale :skip :replacement :interrupt} mode)
                              (is (= (if (= :stale mode) @stale-before before) (durable-state owned))))
                            (is (= other-before (durable-state attached)))
                            (is (= selected (workspace/selected-system-id)))
                            (is (= disposition (dispositions/disposition
                                                (requests/get-request conn (:request disposition)))))
                            (when @saved
                              (is (= :dvergr.rooms.files/closed-forwarding
                                     (try (@saved) nil (catch clojure.lang.ExceptionInfo error (:type (ex-data error))))))
                              (is (= before (durable-state owned))))
                            (when-not (= :accepted expected)
                              (is (= :open (:proposal/status (d/entity @conn [:proposal/id (:proposal plan)])))))
                            {:owned-first? owned-first? :mode mode :state (:state op)
                             :completion (:completion (first @envelopes))})
                          (finally (d/release output)))))))]
            (reset! completed? true)
            result)
          (finally
            ;; Preserve diagnostics instead of releasing/deleting stores when
            ;; the shared physical Room teardown did not return normally.
            (when @completed?
              (when-let [c @chat] (d/release c))
              (doseq [cfg (distinct @configs)]
                (when-not (and (= :memory (get-in cfg [:store :backend])) (uuid? (get-in cfg [:store :id])))
                  (throw (ex-info "Refuse non-fixture cleanup" {})))
                (d/delete-database cfg)))))]
    result))
