(ns is.simm.ops.worker-executor
  "One credential-free Dvergr file worker in a fresh guest-only workspace.

   No daemon, server room lookup, provider login, model transport or physical
   filesystem export lives here. The process embedder installs the forwarding
   provider before execution and owns deadline, cancellation and containment."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.agent.program :as program]
            [dvergr.agent.roster :as roster]
            [dvergr.catalog.workspace :as catalog]
            [dvergr.discourse :as discourse]
            [dvergr.rooms.files :as room-files]
            [dvergr.rooms.forks :as forks]
            [dvergr.room.store.memory :as memory]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.repo :as repo]
            [is.simm.ops.work-sandbox :as sandbox]
            [is.simm.ops.worker-files :as files]
            [muschel.fs :as fs]
            [org.replikativ.spindel.engine.core :as ec]))

(def policy {:version 1 :harness :dvergr :max-model-steps 32 :tools #{:clojure_eval :write_file}
             :sandbox sandbox/policy :accounting :unpriced-local-subscription})

(defn- refuse! []
  (throw (ex-info "Guest worker did not produce a settled bounded result" {:type ::refused})))

(defn- seed! [room frozen]
  (let [descriptors (:files frozen)]
    (when (seq descriptors)
      (room-files/write! room (into {} (map (fn [[p file]] [(str "/" p) {:base64 (:base64 file)}])) descriptors)
                         {:message "Seed selected local worker inputs"})
      (let [ws (workspace/current-workspace)
            filesystem (workspace/filesystem ws)
            modes (into {} (filter (fn [[p file]]
                                     (not= (:mode file) (:mode (get (repo/worktree (:conn ws)) p))))) descriptors)]
        (when (seq modes)
          (doseq [[p file] modes] (fs/chmod filesystem (str "/" p) (:mode file)))
          (forks/commit-paths! room
                               (into {} (map (fn [[p file]] [(str "/" p) (str (blob/blob-id (files/file-bytes! file)))])) modes)
                               "Preserve selected worker input modes"))))))

(defn- export! [room frozen summary]
  (let [tree (repo/worktree (:conn (workspace/current-workspace)))
        original (:files frozen)
        changed (vec
                 (sort
                  (for [[path entry] tree
                        :let [before (get original path)]
                        :when (and (or before (str/starts-with? path "out/"))
                                   (not (and before (= (:mode before) (:mode entry))
                                             (= (:bytes before) (:size entry))
                                             (= (blob/blob-id (files/file-bytes! before)) (:content entry)))))]
                    path)))
        removed (into {} (for [path (keys original) :when (not (contains? tree path))] [path nil]))
        _ (when (> (+ (count changed) (count removed)) (:max-files files/policy)) (refuse!))
        captured (files/freeze room "Capture guest worker result" (:config frozen) changed (:limits frozen))
        output {:files (merge removed (:files captured))
                :summary (subs summary 0 (min 16000 (count summary))) :exit-code 0}]
    (files/validate-output! frozen output)))

(defn execute!
  "Run the existing Dvergr model/tool loop on verified selected files.

   Automatic settlement merges ONLY into this fresh disposable guest parent;
   server-side review/merge authority is unchanged. A completed but unmerged
   guest Run cannot be exported. This API is blocking and belongs on a host
   worker thread or in its dedicated executor process, never a Spindel drain."
  [frozen]
  (files/validate-frozen! frozen)
  (when-not (= "codex" (get-in frozen [:config :adapter])) (refuse!))
  (let [room (discourse/make-room {:id (keyword (str "local-worker-" (random-uuid))) :store (memory/make)})
        connection (volatile! nil)]
    (try
      (binding [ec/*execution-context* (:ctx room)]
        (vreset! connection (:conn (catalog/ensure-workspace! room)))
        (seed! room frozen)
        (let [team (sandbox/restrict-team
                    (roster/make-agent
                     (roster/make-roster {:id :simmis/local-worker})
                     {:id :worker
                      :prompt "Work on the supplied selected files. Use the supplied SCI tools and virtual filesystem, not native host tools. Put additional deliverables under /out/. Report your result when complete."
                      :tools (:tools policy)
                      :model-policy {:provider :codex-subscription :model (get-in frozen [:config :model])}
                      :program {:kind :llm :max-model-steps (:max-model-steps policy) :auto-compact? false}}))
              handle (program/hire! room team :worker {:task (:task frozen) :settlement :automatic})
              ;; The physical runner bounds this entire process. A local
              ;; observer timeout is not proof that the Run or tools stopped.
              result @handle
              durable (program/observe room handle)]
          (when-not (and (= :completed (:run/status result)) (= :merged (:run/settlement-status durable))
                         (string? (:run/value result))) (refuse!))
          (export! room frozen (:run/value result))))
      (finally
        ;; A failed close must NOT release a workspace which may still have
        ;; active work. The owning physical runner retains that exposure.
        (discourse/close-room! room)
        (when @connection (d/release @connection))))))
