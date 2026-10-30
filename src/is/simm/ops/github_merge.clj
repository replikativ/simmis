(ns is.simm.ops.github-merge
  "Bounded private Git merge preparation for the fixed HOST. Caller owns an
   exclusively admitted PRIVATE repository and its native cleanup extent.
   No RPC, canonical publication, imported execution or implicit restoration."
  (:require [datahike.api :as d]
            [geschichte.git.budget :as budget]
            [geschichte.budget :as operation-budget]
            [geschichte.git.client :as client]
            [geschichte.merge :as merge]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.ops.benchmark-case-manifest :as manifests]
            [is.simm.ops.github-import :as import]))

(defn- refuse! [reason]
  (throw (ex-info "Private Git integration is held" {:type ::held :reason reason})))

(defn operation-policy
  "Combine before fetch; overlap keeps the stricter limit, never resets the
   shared handle between transport, materialization and native merge planning."
  []
  (when-not (= {:id :geschichte.merge/bounded-plan :version 1}
               (select-keys (merge/budget-profile) [:id :version]))
    (refuse! :unsupported-merge-runtime))
  (let [native (merge/recommended-budget-policy 120000)]
    (assoc native :limits (merge-with min (:limits native) import/limits))))

(defn- bounded-rows! [db attribute maximum operation]
  (budget/check! operation)
  (let [rows (vec (take (inc maximum) (d/datoms db :avet attribute)))]
    (when (> (count rows) maximum) (refuse! :metadata-limit))
    rows))

(defn- stage! [db operation]
  ;; Bound BEFORE repo/status or checkout can project a complete stage tree.
  (reduce
   (fn [total row]
     (budget/check! operation)
     (let [path (:v row) _ (files/path! path)
           value (d/pull db [:geschichte.stage/state :geschichte.stage/size :geschichte.stage/mode
                             {:geschichte.stage/content [:geschichte.content/id]}] (:e row))
           size (:geschichte.stage/size value)]
       (when-not (#{:present :deleted} (:geschichte.stage/state value)) (refuse! :invalid-stage))
       (if (= :deleted (:geschichte.stage/state value)) total
           (do
             (when-not (and (integer? size) (<= 0 size (:object-bytes import/limits))
                            (#{33188 33261} (:geschichte.stage/mode value))
                            (uuid? (get-in value [:geschichte.stage/content :geschichte.content/id])))
               (refuse! :invalid-stage))
             (let [next (+ total size)]
               (when (> next (:checkout-bytes import/limits)) (refuse! :tree-limit))
               next)))))
   0 (bounded-rows! db :geschichte.stage/path (:checkout-files import/limits) operation)))

(defn- inventory! [db operation]
  (budget/check! operation)
  (let [manifest (manifests/inventory db)]
    (when (or (some #(> (:size %) (:object-bytes import/limits)) (vals (:files manifest)))
              (> (reduce + 0 (map :size (vals (:files manifest)))) (:checkout-bytes import/limits)))
      (refuse! :tree-limit))
    (stage! db operation)
    manifest))

(defn inspect-source!
  "Bound metadata before native full-tree projections. Read-only; the caller
   owns the source lifetime and passes one finite operation budget."
  [conn operation]
  (let [db @conn]
    (doseq [row (bounded-rows! db :geschichte.ref/name 65 operation)]
      (let [ref (:v row)]
        (when-not (and (string? ref) (<= 1 (count ref) 512)) (refuse! :invalid-ref))))
    (doseq [row (bounded-rows! db :geschichte.config/key 128 operation)]
      (budget/check! operation)
      (let [entry (d/pull db [:geschichte.config/key :geschichte.config/value] (:e row))]
        (when-not (and (string? (:geschichte.config/key entry))
                       (<= (count (:geschichte.config/key entry)) 512)
                       (string? (:geschichte.config/value entry))
                       (<= (count (:geschichte.config/value entry)) 4096))
          (refuse! :configuration-limit))))
    (let [manifest (inventory! db operation)
          head (:geschichte.commit/id (repo/head-commit conn))
          commit (when (uuid? head) (query/commit db head))
          snapshot (when-let [id (:geschichte.commit/snapshot commit)] (d/commit-as-db db id))]
      (when-not snapshot (refuse! :head-snapshot-unavailable))
      (stage! snapshot operation)
      (budget/check! operation)
      (let [status (repo/status conn)]
        (when-not (and (:clean? status) (empty? (:untracked status))) (refuse! :dirty)))
      {:head head :ref (repo/current-ref conn) :manifest manifest :configuration (repo/configuration conn)})))

(defn- result-tree! [conn tree operation]
  (when-not (and (map? tree) (<= (count tree) (:checkout-files import/limits)))
    (refuse! :tree-limit))
  (reduce-kv
   (fn [total path {:keys [content size mode]}]
     (budget/check! operation)
     (files/path! path)
     (when-not (and (uuid? content) (integer? size) (<= 0 size (:object-bytes import/limits))
                    (#{33188 33261} mode))
       (refuse! :invalid-tree))
     (let [total (+ total size)]
       (when (> total (:checkout-bytes import/limits)) (refuse! :tree-limit))
       total)) 0 tree)
  (repo/validate-worktree! @conn tree))

(defn plan-fetched!
  "One bounded imported tip vs saved local history. Any exception AFTER native
   materialization begins requires the caller to retain/quarantine its private
   owner; do not promise restoration or replay. Never canonical publication."
  [conn fetched {:keys [ref preparation-id upstream-base operation]}]
  (when-not (and (import/reference? ref) (every? uuid? [preparation-id upstream-base]) operation
                 (map? fetched) (vector? (:refs fetched)) (<= (count (:refs fetched)) 64))
    (refuse! :invalid-preparation))
  (operation-budget/require-limits! operation (:required-limits (merge/budget-profile)))
  (let [before (inspect-source! conn operation)
        incoming-ref (str "refs/heads/simmis-git-incoming-" preparation-id)
        matches (filterv #(= ref (:ref %)) (:refs fetched))
        oid (:oid (first matches))]
    (when-not (and (= ref (:ref before)) (= 1 (count matches))
                   (string? oid) (re-matches #"[0-9a-f]{40}" oid)
                   (nil? (get (query/refs @conn) incoming-ref)))
      (refuse! :invalid-source))
    (let [imported (client/checkout-fetched! conn fetched {:oid oid :ref incoming-ref :import-budget operation})]
      (inspect-source! conn operation)
      (repo/checkout! conn (:ref before))
      (when-not (= before (inspect-source! conn operation)) (refuse! :source-changed))
      (let [plan (merge/plan conn (:head before) (:commit imported) {:merge-budget operation})]
        ;; Strict alpha rule: no separate unbounded ancestry probe or guessed
        ;; base after a rewritten remote/history gap.
        (when-not (= upstream-base (:base plan)) (refuse! :upstream-history-changed))
        (when-not (= oid (:geschichte.commit/git-oid (query/commit @conn (:commit imported))))
          (refuse! :source-changed))
        (result-tree! conn (:tree plan) operation)
        (budget/check! operation)
        {:version 1 :local-ref ref :local-head (:head before)
         :upstream-ref ref :upstream-base upstream-base :upstream-head (:commit imported)
         :upstream-git-oid oid :incoming-ref incoming-ref :plan plan}))))
