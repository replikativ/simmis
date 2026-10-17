(ns is.simm.ops.worker-output
  "Trusted host import of a stopped worker's validated output into its Run
   world. Never a trunk writer, verifier or merge capability."
  (:require [datahike.blob :as blob]
            [dvergr.effects :as effects]
            [dvergr.rooms.files :as room-files]
            [dvergr.rooms.forks :as forks]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.system.rooms :as system-rooms]
            [geschichte.repo :as repo]
            [is.simm.ops.worker-files :as files]
            [muschel.fs :as fs]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- denied! []
  (throw (ex-info "Worker output import is not currently authorized" {:type ::denied})))

(defn import!
  "The adapter supplies a FRESH authority predicate: live exact Run, current
   enrollment/room grants, matching assignment and no cancellation. It is
   checked at entry, every file/commit effect and before returning evidence.
   A stop receipt alone is insufficient. Call only after physical stop.
   `frozen` must be the verified immutable assignment payload, and `world`
   the current canonical Run world, not a reconstructed metadata lookalike.
   The caller's evaluator must separately check exit-code: this importer does
   not interpret a nonzero exit code as successful task execution.

   Content uses the existing bounded atomic room-files batch. Regular-file
   modes use a second gated commit inside the isolated world. A failure can
   leave a partial isolated result, never writes trunk, and must fail the Run;
   this is NOT one atomic transaction spanning both commits."
  [world run-id frozen output permitted?]
  (when-not (and (uuid? run-id) (fn? permitted?) (forks/fork? world) (forks/ctx-fork? world)
                 (= run-id (:run-id @(:meta world))) (:run-world? @(:meta world)))
    (denied!))
  (when-not (permitted?) (denied!))
  (files/validate-output! frozen output)
  (binding [ec/*execution-context* (:ctx world)]
    (let [{:keys [conn] :as ws} (or (workspace/current-workspace)
                                    (throw (ex-info "Run world has no workspace" {:type ::unavailable})))
          filesystem (workspace/filesystem ws)
          system-id (system-rooms/connection-system-id conn)
          boundary (effects/boundary-resolver nil (effects/make-sink)
                                              {:world #(effects/world-handlers (:ctx world))
                                               :world-sink #(effects/world-sink (:ctx world))
                                               :write-refusal (fn [_] (when-not (permitted?)
                                                                        "Worker import authority was revoked"))})
          contents (into {} (map (fn [[path file]] [(str "/" path) (when file {:base64 (:base64 file)})]))
                         (:files output))]
      (locking (workspace/workspace-lock conn)
        (let [written (when (seq contents)
                        (room-files/write! world contents {:message "Import local worker output" :boundary boundary}))
              current (repo/worktree conn)
              modes (into {} (filter (fn [[path file]]
                                       (and file (not= (:mode file) (:mode (get current path))))))
                          (:files output))
              mode-commit
              (when (seq modes)
                (effects/perform!
                 boundary {:effect :git/commit :resource {:message "Preserve local worker file modes" :system-id system-id}}
                 (fn []
                   (doseq [[path file] modes]
                     (effects/perform!
                      boundary {:effect :fs/write :resource {:path (str "/" path) :bytes 0 :system-id system-id}}
                      #(fs/chmod filesystem (str "/" path) (:mode file))))
                   (forks/commit-paths! world
                                        (into {} (map (fn [[path file]]
                                                        [(str "/" path) (str (blob/blob-id (files/file-bytes! file)))])) modes)
                                        "Preserve local worker file modes"))))]
          ;; A filesystem effect may have been answered by a world handler.
          ;; Never mistake an intercepted/no-op chmod or write for the bytes
          ;; and mode actually imported into the isolated repository.
          (let [actual (repo/worktree conn)]
            (doseq [[path file] (:files output)
                    :let [entry (get actual path)]]
              (when-not (if file
                          (and (= (:mode file) (:mode entry)) (= (:bytes file) (:size entry))
                               (= (blob/blob-id (files/file-bytes! file)) (:content entry)))
                          (nil? entry))
                (throw (ex-info "Run workspace does not match the worker output" {:type ::inconsistent})))))
          (when-not (permitted?) (denied!))
          {:files (into {} (keep (fn [[path file]] (when file [(str "/" path) file]))) (:files output))
           :removed (vec (sort (keep (fn [[path file]] (when-not file (str "/" path))) (:files output))))
           :summary (:summary output) :exit-code (:exit-code output)
           :import {:content-commit (:commit written) :mode-commit mode-commit}})))))
