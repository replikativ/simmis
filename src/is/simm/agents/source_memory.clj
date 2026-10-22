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
            [is.simm.documents.execution-view :as execution-view]
            [is.simm.documents.view :as view]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.protocols :as systems]))

(defn resolver
  "Stable resolver retaining an interpreter owner/capability identity, not a
   Room or source connection. The capability binding's :simmis/source-memory
   data names {:room-id :agent-id :system-id}. authorize! checks live host
   authority; proposal-workspace returns ONLY an existing proposal workspace.

   Whole-world descendants always use their own registered system. Only the
   ordinary owner world consults proposal-workspace, matching its file-tool
   routing. Missing systems and unrelated caller worlds explicitly refuse."
  [owner capability-id receipts {:keys [authorize! proposal-workspace options]}]
  (let [resolve-binding (runtime-ctx/sandbox-binding-resolver owner capability-id)
        boundary (sandbox/agent-effect-boundary owner capability-id nil receipts)]
    (fn []
      (let [selected (runtime-ctx/selected-context owner)
            bound (when (ec/execution-context-bound?) (ec/current-execution-context))]
        (when (and bound (not (runtime-ctx/descendant-context? bound owner)))
          (throw (ex-info "Source knowledge belongs to another world" {:type ::wrong-world})))
        (binding [ec/*execution-context* selected]
          (let [b (resolve-binding)
                {:keys [room-id agent-id system-id] :as target} (:simmis/source-memory b)]
            (when-not (and room-id agent-id system-id (= agent-id (:agent-id b)))
              (throw (ex-info "Source knowledge has no valid world binding" {:type ::missing-binding})))
            (authorize! target)
            (effects/perform!
             boundary {:effect :room/read :resource {:op :source-memory :room (str (:room-runtime-id b))}}
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
                   (view/current-view conn (execution-view/selection-options system conn options))))))))))))

(defn bindings [owner capability-id receipts policy]
  (memory/bindings (resolver owner capability-id receipts policy)))
