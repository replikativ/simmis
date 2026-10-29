(ns is.simm.agents.source-memory
  "Fork-selected adapter for source knowledge. Host policy supplies permission
   and existing-proposal lookup; neither a read nor a missing system hydrates
   a room, opens a writer or creates a proposal."
  (:require [dvergr.effects :as effects]
            [dvergr.runtime.ctx :as runtime-ctx]
            [dvergr.sandbox :as sandbox]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [is.simm.agents.memory-read :as memory]
            [is.simm.agents.memory-write :as writes]
            [is.simm.agents.memory-links :as links]
            [is.simm.documents.execution-view :as execution-view]
            [is.simm.documents.view :as view]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.protocols :as systems]))

(def prompt-block
  (str "\n\nThis room's knowledge is Markdown in its Memory filesystem. Use clojure_eval"
       " to program the memory namespace: (memory/documents), (memory/read \"wiki/Note.md\"),"
       " and (memory/search \"text\"). Inspect function docs in the REPL. memory/read returns"
       " :document with :source and an :entry for concurrency control. Edit with"
       " (memory/write! {:path \"wiki/Note.md\" :expected entry :text source}); use nil"
       " only when the file does not exist. Keep existing metadata and block IDs."
       " The file tools access the same selected worktree."
       " For reviewed changes, start a proposal before editing, then file it."
       " A worktree write is not acceptance or publication."
       " Use (memory/link path) for a room-scoped Markdown link to a shared file."
       " If it returns :unpublished, point the human to the proposal instead;"
       " do not invent a trunk link. Missing links never create pages."
       " Do not use legacy kb/*, wiki/* or dh:// pages for this Memory."))

(defn selection-resolver
  "Stable resolver retaining an interpreter owner/capability identity, not a
   Room or source connection. The capability binding's :simmis/source-memory
   data names {:room-id :agent-id :system-id}. authorize! checks live host
   authority; proposal-workspace returns ONLY an existing proposal workspace.

   Whole-world descendants always use their own registered system. Only the
   ordinary owner world consults proposal-workspace, matching its file-tool
   routing. Missing systems and unrelated caller worlds explicitly refuse.
   Returns a synchronous host-only callback boundary; connections and context
   must not escape into SCI or retained component state. A proposal connection
   is explicitly distinct from the registered canonical system.
   with-selection! is a host-only synchronous gate around selection and use;
   callers decide which operations may be answered/replayed as projections."
  [owner capability-id {:keys [authorize! proposal-workspace with-selection!] :as policy}]
  (let [resolve-binding (runtime-ctx/sandbox-binding-resolver owner capability-id)]
    (fn [consume!]
      (let [selected (runtime-ctx/selected-context owner)
            bound (when (ec/execution-context-bound?) (ec/current-execution-context))]
        (when (and bound (not (runtime-ctx/descendant-context? bound owner)))
          (throw (ex-info "Source knowledge belongs to another world" {:type ::wrong-world})))
        (binding [ec/*execution-context* selected]
          (let [b (resolve-binding)
                {:keys [room-id agent-id system-id] :as stored-target} (:simmis/source-memory b)
                ;; Host-only selection evidence: a whole-world child must not
                ;; consult its ancestor's ordinary proposal registry.
                target (assoc stored-target :owner-world? (= (:fork-id selected) (:fork-id owner)))]
            (when-not (and room-id agent-id system-id (= agent-id (:agent-id b)))
              (throw (ex-info "Source knowledge has no valid world binding" {:type ::missing-binding})))
            (authorize! target)
            (with-selection!
              b
              (fn []
               ;; Lookup by expected identity, never current-system's first-
               ;; registered fallback and never the host room registry.
                (let [system (get (ygg/registered-systems) system-id)]
                  (when-not (and system (= :geschichte (systems/system-type system)))
                    (throw (ex-info "Selected world has no expected source repository"
                                    {:type ::missing-repository :system-id system-id})))
                  (let [canonical (workspace/gy-connection system)
                        proposal (when (= (:fork-id selected) (:fork-id owner))
                                   (proposal-workspace target))
                        conn (if proposal
                               (or (:conn proposal)
                                   (throw (ex-info "Proposal source workspace is unavailable"
                                                   {:type ::missing-proposal-workspace})))
                               canonical)]
                    (when-not conn
                      (throw (ex-info "Source repository has no connection" {:type ::missing-repository})))
                    (when (and proposal
                               (not= (:geschichte.repo/id (query/repository @canonical))
                                     (:geschichte.repo/id (query/repository @conn))))
                      (throw (ex-info "Proposal belongs to a different source repository"
                                      {:type ::wrong-proposal-repository})))
                    (consume! (cond-> {:selected-context selected :target target
                                       :canonical-system system :canonical-conn canonical
                                       :conn conn :proposal proposal :binding b}
                                (:capture-write-route! policy)
                                (assoc :validate-write-route!
                                       ((:capture-write-route! policy) target proposal))))))))))))))

(defn with-workspace-resolver [owner capability-id receipts policy]
  (let [boundary (sandbox/agent-effect-boundary owner capability-id nil receipts)]
    (selection-resolver
     owner capability-id
     (assoc policy :with-selection!
            (fn [binding next!]
              (effects/perform!
               boundary {:effect :room/read :resource {:op :source-memory :room (str (:room-runtime-id binding))}}
               next!))))))

(defn resolver [owner capability-id receipts policy]
  (let [with-workspace! (with-workspace-resolver owner capability-id receipts policy)]
    #(with-workspace!
       (fn [{:keys [canonical-system conn]}]
         (view/current-view conn (execution-view/selection-options canonical-system conn (:options policy)))))))

(defn bindings [owner capability-id receipts policy]
  (let [with-read! (with-workspace-resolver owner capability-id receipts policy)
        reads (merge (memory/bindings (resolver owner capability-id receipts policy))
                     (links/bindings with-read! owner))]
    (if-let [authorize-write! (:authorize-write! policy)]
      (let [boundary (sandbox/agent-effect-boundary owner capability-id nil receipts)
            with-selected! (selection-resolver owner capability-id
                                               (assoc policy :authorize! authorize-write!
                                                      :capture-write-route! (:write-route-guard policy)
                                                      :proposal-workspace (:proposal-workspace! policy)
                                           ;; Selection is not a replayable read. The native
                                           ;; workspace writer owns the mutation effect.
                                                      :with-selection! (or (:with-write-selection! policy)
                                                                           (fn [_ next!] (next!)))))]
        (merge reads (writes/bindings with-selected! boundary authorize-write!)))
      reads)))
