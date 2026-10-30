(ns is.simm.ops.github-native
  "Finite preflight around native workspace publication. This does not make the
   native operation budgeted: it constrains both exact inputs before native
   eager projections and requires a checkpoint guard plus exclusive source."
  (:require [datahike.api :as d]
            [geschichte.git.budget :as budget]
            [geschichte.workspace :as workspace]
            [is.simm.ops.github-merge :as merge]))

(defn- refuse! [reason]
  (throw (ex-info "Git native publication is held" {:type ::held :reason reason})))

(def ^:private bounds
  ;; Scan fixed attributes before query/commits or nested content pulls. All
  ;; counts are aggregate per attribute, including unreachable historical rows.
  {:geschichte.commit/id [4096 :uuid]
   :geschichte.commit/snapshot [4096 :uuid]
   :geschichte.commit/parents [65536 :ref]
   :geschichte.commit/message [4096 65536]
   :geschichte.commit/author [4096 8192]
   :geschichte.commit/time [4096 :instant]
   :geschichte.commit/git-oid [4096 40]
   :geschichte.content/id [50000 :uuid]
   :geschichte.content/base [50000 :ref]
   :geschichte.content/chunks [65536 :ref]
   :geschichte.content/payload [50000 :uuid]
   :geschichte.content/kind [50000 :kind]
   :geschichte.content/size [50000 :size]
   :geschichte.content/depth [50000 :depth]
   :geschichte.content/chunking-algorithm [50000 :keyword]
   :geschichte.content/chunking-version [50000 :size]
   :geschichte.content/chunk-min-size [50000 :size]
   :geschichte.content/chunk-size [50000 :size]
   :geschichte.content/chunk-max-size [50000 :size]
   :geschichte.chunk/index [65536 :size]
   :geschichte.chunk/offset [65536 :size]
   :geschichte.chunk/payload [65536 :uuid]
   :geschichte.chunk/size [65536 :size]})

(defn- scalar! [kind value]
  (when-not
   (if (integer? kind)
     (and (string? value) (<= (count value) kind))
     (case kind
       :uuid (uuid? value)
       :ref (and (integer? value) (pos? value))
       :instant (inst? value)
       :keyword (and (keyword? value) (<= (count (str value)) 128))
       :kind (#{:full :line-delta :chunks} value)
       :size (and (integer? value) (<= 0 value (* 32 1024 1024)))
       :depth (and (integer? value) (<= 0 value 32))))
    (refuse! :invalid-native-metadata)))

(defn- catalog! [db operation]
  (let [total (volatile! 0) string-bytes (volatile! 0)]
    (doseq [[attribute [maximum kind]] bounds]
      (loop [remaining (seq (d/datoms db :aevt attribute)) n 0 per-entity {}]
        (when-let [row (first remaining)]
          (budget/check! operation)
          (when (or (>= n maximum) (> (vswap! total inc) 250000))
            (refuse! :native-metadata-limit))
          (scalar! kind (:v row))
          (when (string? (:v row))
            (when (> (vswap! string-bytes + (alength (.getBytes ^String (:v row) "UTF-8"))) (* 16 1024 1024))
              (refuse! :native-metadata-limit)))
          (let [counts (if (= :geschichte.commit/parents attribute)
                         (update per-entity (:e row) (fnil inc 0)) per-entity)]
            (when (> (get counts (:e row) 0) 16) (refuse! :native-parent-limit))
            (recur (next remaining) (inc n) counts)))))))

(defn inspect!
  "Read-only finite preflight of a clean repository and its complete catalogs.
   Returns the exact native checkpoint; no native effects or authority."
  [db operation]
  (catalog! db operation)
  (let [visible (merge/inspect-source! (reify clojure.lang.IDeref (deref [_] db)) operation)]
    (budget/check! operation)
    (assoc (select-keys visible [:head :ref]) :checkpoint (d/commit-id db))))

(defn publish!
  "HOST only. Caller retains authority and exclusively owns source throughout.
   Always create a fresh review ref; a raced target refuses before projection."
  [target source ref commit operation check!]
  (when-not (= {:id :geschichte.workspace/guarded-publication :version 1
               :create-only? true :expected-checkpoint? true}
              (workspace/publication-profile))
    (refuse! :unsupported-native-runtime))
  (check!)
  (let [source-db @source target-db @target
        inspected (inspect! source-db operation)
        _ (when-not (= commit (:head inspected)) (refuse! :uninspected-source-tip))
        {:keys [checkpoint]} (inspect! target-db operation)]
    (check!)
    (when-not (= source-db @source) (refuse! :private-branch-changed))
    (budget/check! operation)
    (workspace/publish! target source {:ref ref :commit commit :create-only? true
                                       :expected-checkpoint checkpoint})))

(defn advance!
  "HOST only. Source remains exclusively owned; target must still be exactly
   the semantically reviewed snapshot, including non-file definition changes."
  [source target ref checkpoint operation check!]
  (let [profile-fn (ns-resolve 'geschichte.workspace 'advance-profile)]
    (when-not (and profile-fn (= {:id :geschichte.workspace/guarded-advance :version 1
                                 :checkpoint-before-projection? true} (profile-fn)))
      (refuse! :unsupported-native-runtime)))
  (check!)
  (let [source-db @source target-db @target]
    (when-not (= checkpoint (d/commit-id target-db)) (refuse! :target-changed))
    (when-not (= ref (:ref (inspect! source-db operation))) (refuse! :uninspected-source-tip))
    (inspect! target-db operation)
    (check!)
    (when-not (= source-db @source) (refuse! :private-branch-changed))
    (budget/check! operation)
    (workspace/advance! source target {:ref ref :expected-checkpoint checkpoint})))
