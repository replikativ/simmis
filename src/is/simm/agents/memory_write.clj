(ns is.simm.agents.memory-write
  "Guarded worktree edits over an explicitly selected source workspace.
   No Room is captured, no canonical logical commit is inferred, and native
   write witnesses never escape to SCI. Proposal/commit orchestration is separate."
  (:require [datahike.blob :as blob]
            [dvergr.rooms.file-tx :as file-tx]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.tools :as tools]
            [is.simm.documents.files :as files]))

(defn- simple-failure? [failure]
  ;; Unknown native ownership/recovery conditions must propagate intact. Only
  ;; small ordinary conditions can be represented as inert caller feedback.
  (and (instance? clojure.lang.ExceptionInfo failure)
       (nil? (ex-cause failure)) (empty? (.getSuppressed ^Throwable failure))
       (every? #{:type :path :expected :found :conflict} (keys (ex-data failure)))
       (every? #(or (nil? %) (string? %) (keyword? %) (number? %) (boolean? %) (uuid? %))
               (vals (ex-data failure)))))

(defn bindings
  "HOST callbacks select a live workspace and recheck write authority. The
   native tool writer applies the selected world's effect policy and exact
   validated basis. Only a native :written witness certifies this invocation."
  [with-workspace! boundary authorize!]
  {'write!
   (fn [request]
     (when-not (= #{:path :expected :text} (set (keys request)))
       (throw (ex-info "Memory writes require path, exact opened entry (or nil), and Markdown text"
                       {:type ::invalid-request})))
     (let [{:keys [path expected bytes]} (files/prepare-file-write request)]
       (when-not (= :markdown (files/file-kind path))
         (throw (ex-info "Use filesystem operations for non-Markdown files" {:type ::unsupported-document})))
       (with-workspace!
         (fn [{:keys [selected-context target canonical-system conn proposal binding validate-write-route!]}]
           (authorize! target)
           (let [ws (or proposal (workspace/workspace-for-system! canonical-system))
                 _ (when-not (identical? conn (:conn ws))
                     (throw (ex-info "Selected source workspace changed" {:type ::changed-workspace})))
                 db @conn
                 _ (when-not (= expected (files/entry db path))
                     (throw (ex-info "Document changed; read it again before editing" {:type ::conflict :path path})))
                 basis (file-tx/capture-basis db)
                 mode (or (:mode expected) 33188)
                 state (volatile! :not-dispatched)
                 guarded-boundary (fn []
                                    (update (boundary) :handlers (fnil conj [])
                                            (fn [effect next!]
                                              (authorize! target)
                                              (when validate-write-route! (validate-write-route!))
                                              (next! effect))))
                 context {:workspace ws :filesystem (workspace/filesystem ws)
                          :execution-ctx selected-context :room-id (:room-runtime-id binding)
                          :effect-boundary guarded-boundary
                          :validated-basis basis :basis-write-state state}]
             (let [outcome (try
                             (tools/workspace-write! context (str "/" path) bytes
                                                     {:expect (some-> expected :content str) :mode mode})
                             {}
                             (catch Throwable failure {:failure failure}))
                   failure (:failure outcome)
                   receipt {:status :written :publication :worktree :path path
                            :entry {:content (blob/blob-id bytes) :size (long (alength ^bytes bytes)) :mode mode}}]
               (when (and failure (not (simple-failure? failure))) (throw failure))
               (case @state
                 :written (cond-> receipt
                            failure (assoc :effect-error {:type (:type (ex-data failure)) :message (ex-message failure)}))
                 :not-dispatched (if failure (throw failure)
                                   (throw (ex-info "The effect policy did not apply this edit" {:type ::not-applied :path path})))
                 :refused (throw (ex-info "The native writer refused the inspected basis" {:type ::conflict :path path} failure))
                 (throw (ex-info "Write acknowledgement is uncertain; inspect before retrying"
                                 {:type ::pending :path path} failure)))))))))})
