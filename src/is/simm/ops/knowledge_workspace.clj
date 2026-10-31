(ns is.simm.ops.knowledge-workspace
  "Owned input staging in disposable workflow/benchmark worlds. Never clear
   a canonical source room as a substitute for constructing an isolated Run."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.workspace :as room-workspace]
            [dvergr.discourse :as discourse]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [is.simm.documents.execution-view :as execution-view]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.knowledge-scope :as scope]
            [is.simm.model.work-request-checkpoints :as checkpoints]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]
            [yggdrasil.protocols :as protocols]))

(defn expected-proof
  "The portable identity a bound host capture must prove. Constructing this
   value does not execute a world or grant any publication authority."
  [target]
  (when-not (and (publication/host-target? target)
                 (m/validate publication/target-schema target))
    (throw (ex-info "Knowledge execution requires an explicit retained target" {})))
  {:version 1 :target target :selected-system (:runtime-system target)
   :actual (select-keys target [:runtime-system :repository :store :backend])})

(defn validate-proof!
  "Check retained trusted capture evidence, never a model-authored result."
  [target proof]
  (when-not (= (expected-proof target) proof)
    (throw (ex-info "Knowledge execution workspace proof does not match its target" {})))
  proof)

(defn target-proof!
  "Read the selected RUN world's real identity. A coincidentally correct legacy
   fallback is insufficient. Fork branch names differ; store/repository identity
   must not. Returns portable data, no connections or permissions."
  [world target]
  (expected-proof target)
  (binding [ec/*execution-context* (:ctx world)]
    (when-not (and (workspace/workspace-selected?)
                   (= (:runtime-system target) (workspace/selected-system-id)))
      (throw (ex-info "Knowledge execution has no matching explicit workspace selection" {})))
    (let [{:keys [conn system]} (workspace/current-workspace)
          db @conn
          proof {:version 1 :target target :selected-system (workspace/selected-system-id)
                 :actual {:runtime-system (str (protocols/system-id system))
                          :repository (:geschichte.repo/id (query/repository db))
                          :store (get-in db [:config :store :id])
                          :backend (get-in db [:config :store :backend])}}]
      (validate-proof! target proof))))

(defn with-publication-target
  "Bind an existing setup before it constructs tools or stages input. No new
   effects or grants: selection is host-controlled forkable computation state."
  [base target]
  (expected-proof target)
  (evaluation/make-world-setup
   {:id :simmis/knowledge-workspace :version 1
    :checkpoints (:checkpoints base)
    :basis {:base (evaluation/world-setup-ref base) :target target}
    :prepare (fn [{world :room :as context}]
               (binding [ec/*execution-context* (:ctx world)]
                 (workspace/select-workspace! (:runtime-system target))
                 (target-proof! world target)
                 (let [result ((:prepare base) context)]
                   (target-proof! world target)
                   result)))}))

(defn with-checkpoint
  "Select one HOST-retained initial source before preparation, and verify the
   Run's actual native fork provenance. Preparation may legitimately advance
   the child's CID; its immutable creation descriptor remains the proof."
  [base selection]
  (checkpoints/validate-selection! selection)
  (when (seq (:checkpoints base))
    (throw (ex-info "Knowledge setup already selects checkpoint origins" {})))
  (let [system (get-in selection [:target :runtime-system])
        origin (:origin selection)
        selected {system origin}
        setup (evaluation/make-world-setup
               {:id :simmis/knowledge-checkpoint :version 1
                :basis {:base (evaluation/world-setup-ref base) :selection selection}
                :checkpoints selected
                :prepare (fn [{world :room :as context}]
                           (let [entry (get-in (discourse/fork-descriptor world) [:fork/systems system])]
                             (when-not (and (= :overlay (:kind entry)) (= origin (:checkpoint entry)))
                               (throw (ex-info "Knowledge execution has no matching checkpoint fork" {}))))
                           ((:prepare base) context))})]
    ;; Old preparers ignore unknown fields. Refuse explicitly instead of
    ;; permitting a policy-bearing workflow to execute at the current head.
    (when-not (= selected (:setup/checkpoints (evaluation/world-setup-ref setup)))
      (throw (ex-info "Installed harness does not support checkpoint setup identity" {})))
    setup))

(def cleanup-policy
  {:version 2 :roots ["in/knowledge" "out/wiki"] :max-paths files/max-batch-files})

(defn cleanup-policy-for [output-scope]
  (if output-scope
    (do (scope/normalize! output-scope)
        (assoc cleanup-policy :version 3 :roots ["in/knowledge" "out/wiki" "out/knowledge"]))
    cleanup-policy))

(defn validate-inputs!
  "Knowledge inputs cannot masquerade as newly generated deliverables."
  [inputs]
  (doseq [path (keys inputs)]
    (when-not (and (string? path)
                   (try (= path (files/workspace-path path)) (catch Exception _ false)))
      (throw (ex-info "Knowledge inputs require canonical workspace paths" {}))))
  (when (some #(or (= % "/out") (= % "/out/wiki") (str/starts-with? % "/out/wiki/")
                  (= % "/out/knowledge") (str/starts-with? % "/out/knowledge/"))
              (keys inputs))
    (throw (ex-info "Knowledge inputs occupy the reserved output namespace" {})))
  inputs)

(defn- owned-rows [db attr root cleanup-policy]
  (concat (d/datoms db :avet attr root)
          ;; Filter only the exact inclusive outside endpoint AFTER bounding
          ;; this prefix; cleanup-state still bounds meaningful rows globally
          ;; across both roots, independently for files and directories.
          (remove #(= (str root "0") (:v %))
                  (take (inc (:max-paths cleanup-policy))
                        (d/index-range db {:attrid attr :start (str root "/") :end (str root "0")})))))

(defn- cleanup-state [world cleanup-policy]
  (binding [ec/*execution-context* (:ctx world)]
    (let [conn (:conn (workspace/current-workspace))
          _ (when-not conn (throw (ex-info "Knowledge staging has no workspace" {})))
          db @conn
          paths (fn [attr] (vec (take (inc (:max-paths cleanup-policy))
                                      (map :v (mapcat #(owned-rows db attr % cleanup-policy) (:roots cleanup-policy))))))
          file-paths (paths :geschichte.work/path)
          dirs (paths :geschichte.work-dir/path)]
      (when (or (> (count file-paths) (:max-paths cleanup-policy))
                (> (count dirs) (:max-paths cleanup-policy)))
        (throw (ex-info "Inherited knowledge staging exceeds cleanup bounds" {})))
      (when (seq (d/datoms db :avet :geschichte.work/path "out"))
        (throw (ex-info "Knowledge output ancestor is a file" {})))
      {:files file-paths :dirs dirs})))

(defn clear-owned!
  "Clear only disposable knowledge-world staging/output. Empty directories
   without files cannot be removed by the canonical writer: refuse them before
   editing. Re-scan after writing so skipped effects cannot certify stale output."
  ([world] (clear-owned! world nil))
  ([world output-scope]
  (let [cleanup-policy (cleanup-policy-for output-scope)
        {:keys [files dirs]} (cleanup-state world cleanup-policy)]
    (doseq [dir dirs]
      (when-not (some #(str/starts-with? % (str dir "/")) files)
        (throw (ex-info "Inherited knowledge staging contains an unsupported empty directory"
                        {:path dir}))))
    (when (seq files)
      (files/write! world (into {} (map #(vector (str "/" %) nil)) files)
                    {:message "Reset isolated knowledge staging and outputs"}))
    (let [remaining (cleanup-state world cleanup-policy)]
      (when (or (seq (:files remaining)) (seq (:dirs remaining)))
        (throw (ex-info "Knowledge staging cleanup did not empty its owned subtrees" {}))))
    true)))

(defn with-frozen-staging
  "Wrap fixture setup so every benchmark backend clears stale owned inputs
   before seeding its verified case. No global cache or new world authority."
  ([base] (with-frozen-staging base nil))
  ([base output-scope]
  (evaluation/make-world-setup
   {:id :simmis/knowledge-fixtures :version (if output-scope 4 3)
    :checkpoints (:checkpoints base)
    :basis {:base (evaluation/world-setup-ref base) :cleanup (cleanup-policy-for output-scope)
            :source-role execution-view/profile}
    :prepare (fn [{world :room :as context}]
               (room-workspace/ensure-workspace! world)
               (clear-owned! world output-scope)
               (let [result ((:prepare base) context)]
                 (binding [ec/*execution-context* (:ctx world)]
                   (execution-view/install! world))
                 result))})))
