(ns is.simm.ops.github-review
  "Read-only semantic review of one frozen Git candidate against current trusted
   definitions. Caller owns and authorizes the original source snapshots. This
   returns comparison data, never publication, retention or acceptance rights."
  (:require [geschichte.content :as content]
            [geschichte.git.budget :as budget]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.github-proposals :as provenance]
            [is.simm.ops.github-merge :as merge]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.knowledge-changes :as changes]))

(def policy :simmis.github/clean-files-with-trusted-markdown-v1)

(defn- refuse! [reason]
  (throw (ex-info "Git candidate review is held" {:type ::held :reason reason})))

(defn- markdown? [path] (boolean (re-find #"(?i)\.(md|markdown)$" path)))

(defn- snapshot! [db expected]
  (when-not (= expected (changes/basis db)) (refuse! :snapshot-changed)))

(defn- wrapper [db]
  ;; The native read API dereferences a connection-shaped value; this wrapper
  ;; is immutable and never lends a writer or creates independent UI state.
  (reify clojure.lang.IDeref (deref [_] db)))

(defn- preflight-canonical-content! [db operation]
  ;; The shared validator has independent fixed document/parser limits, but
  ;; its internal content reader is not operation-budgeted. Validate every
  ;; selected immutable content graph first, including unchanged documents.
  ;; Its subsequent reads use this SAME immutable DB, not a refreshed source.
  (doseq [[_ {:keys [content size]}] (:files (view/selection db profile/selection-options))]
    (budget/check! operation)
    (let [bytes (content/read-by-id (wrapper db) content
                                    {:merge-budget operation :read-db db :sync? true})]
      (when-not (and bytes (= size (alength ^bytes bytes)))
        (refuse! :content-size-mismatch)))))

(defn inspect
  "Validate replacements/creations using CANONICAL types, including unchanged
   documents and typed dependencies. Markdown deletion/moves are explicitly
   held until prospective-set validation supports them. Non-Markdown bytes
   remain inert files. Every acceptance must recompute this exact basis and
   publish with the returned canonical checkpoint guard."
  ([canonical local candidate body] (inspect canonical local candidate body nil))
  ([canonical local candidate body provided-operation]
  (provenance/validate-provenance! body)
  (snapshot! local (get-in body [:roots :local :basis]))
  (snapshot! candidate (get-in body [:roots :candidate :basis]))
  (let [canonical-basis (changes/basis canonical)
        target (get-in body [:identity :connection :target])
        operation (or provided-operation (budget/create (assoc (merge/operation-policy) :timeout-ms 30000)))]
    (when-not (and (= (select-keys target [:repository :store :backend :branch :ref])
                      (select-keys canonical-basis [:repository :store :backend :branch :ref]))
                   (= space/profile (:document.repository/profile (query/repository canonical))))
      (refuse! :source-settings-changed))
    (let [current (merge/inspect-source! (wrapper canonical) operation)
          old (merge/inspect-source! (wrapper local) operation)
          proposed (merge/inspect-source! (wrapper candidate) operation)]
      (when-not (and (= (:head current) (:base (:proposal body)))
                     (= (:head old) (:base (:proposal body)))
                     (= (:head proposed) (:head (:proposal body)))
                     (= (:manifest current) (:manifest old)))
        (refuse! :local-version-changed))
      (let [delta (facts/changes {:visible {:manifest (:manifest old)}}
                                 {:visible {:manifest (:manifest proposed)}})
            deleted (filterv #(and (= :deleted (:kind %)) (markdown? (:path %))) delta)
            replacements (filterv #(and (markdown? (:path %)) (not= :deleted (:kind %))) delta)]
        (if (seq deleted)
          {:status :held :reason :markdown-removal-requires-review
           :changes delta :unsupported (mapv :path deleted)}
          (let [inputs (into (sorted-map)
                             (for [{:keys [path before after]} replacements]
                               (do
                                 (budget/check! operation)
                                 (let [bytes (content/read-by-id (wrapper candidate) (:content after)
                                                                 {:merge-budget operation :read-db candidate :sync? true})]
                                   (when-not (and bytes (= (:size after) (alength ^bytes bytes)))
                                     (refuse! :content-size-mismatch))
                                   [path {:expected before :source (markdown/decode bytes)}]))))
                validation (if (seq inputs)
                             (do (preflight-canonical-content! canonical operation)
                                 (view/validate-changes canonical profile/selection-options inputs))
                             {:kind :files-only :status :valid :basis {} :changes {} :diagnostics []})
                basis {:version 1 :policy policy :proposal (get-in body [:proposal :id])
                       :connection-id (connections/connection-id (get-in body [:identity :connection]))
                       :cursor (get-in body [:identity :cursor]) :incoming (get-in body [:identity :incoming])
                       :local-head (get-in body [:proposal :base]) :candidate-head (get-in body [:proposal :head])
                       :canonical canonical-basis
                       :tree-id (hasch/uuid [:simmis/github-candidate-tree-v1 (:manifest proposed)])
                       :typed-validation (:basis validation) :changes (:changes validation)}
                result {:status (if (= :valid (:status validation)) :ready :held)
                        :reason (when-not (= :valid (:status validation)) :document-validation)
                        :review-id (hasch/uuid [:simmis/github-review-v1 basis])
                        :basis basis :changes delta :diagnostics (:diagnostics validation)}]
            (budget/check! operation)
            (when (> (alength (.getBytes ^String (pr-str result) "UTF-8")) facts/max-artifact-bytes)
              (refuse! :review-limit))
            result)))))))
