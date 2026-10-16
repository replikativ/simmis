(ns is.simm.agents.merger
  "Op-based topic-page merging — the second summarizer.

   Merging is NOT page rewriting: claims extracted from a chat summary
   are classified per linked page as ADD / UPDATE / NOOP ops, each op
   adversarially grounding-checked against the summary text before it
   lands (drop-and-log on failure — the measured toggle that decides
   whether a given model needs reviewing at all). Landed blocks carry a
   stored :block/references ref to the summary RECORD page — provenance
   as backlinks, zero new schema; dh:// record-refs when datahike#852
   ships. Bad merges stay cheap: datahike history + KB branching."
  (:require [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.task-models :as task-models]
            [datahike.api :as d]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [taoensso.telemere :as log]))

(defn- page-blocks [kb-conn title]
  (->> (d/q '[:find ?u ?c :in $ ?t :where
              [?p :S.Page/title ?t] [?b :block/parent ?p]
              [?b :entity/uuid ?u] [?b :block/content ?c]]
            @kb-conn title)
       (mapv (fn [[u c]] {:uuid u :content (str/replace (str c) #"<[^>]+>" "")}))))

(defn- cheap-call
  "One cheap one-shot call on the `:cheap` task model (`task-models`): dvergr's
   default when none is configured, the configured one otherwise. A configured
   model that resolves to nothing answers `{:error reason}` — the shape
   `cheap-llm-call` answers its own failures in — and is logged, never
   swapped for dvergr's default."
  [prompt opts]
  (try
    ((requiring-resolve 'dvergr.tools.llm-call/cheap-llm-call)
     prompt "" (merge opts (task-models/model-opts :cheap)))
    (catch clojure.lang.ExceptionInfo e
      (log/log! {:level :warn :id ::cheap-model-unavailable :data (ex-data e)})
      {:error (ex-message e)})))

(defn- parse-edn-ops [s]
  (try (let [v (edn/read-string (str/trim (str/replace (str s) #"(?s)```(edn)?|```" "")))]
         (when (sequential? v) (filterv map? v)))
       (catch Exception _ nil)))

(defn extract-ops
  "Claims from `summary-text` relevant to page `title` → op list
   [{:op :add|:update :claim \"…\" :basis \"…\" :block-uuid? …}]."
  [title blocks summary-text]
  (let [prompt (str "You maintain the wiki page \"" title "\".\n\nCurrent page blocks:\n"
                    (if (seq blocks)
                      (str/join "\n" (map-indexed (fn [i b] (str i ". " (:content b))) blocks))
                      "(empty)")
                    "\n\nNew conversation summary mentioning this page:\n" summary-text
                    "\n\nExtract facts about \"" title "\" that the page should carry. "
                    "Answer ONLY an EDN vector of ops:\n"
                    "[{:op :add :claim \"one concise sentence\" :basis \"exact quote from the summary\"}\n"
                    " {:op :update :block 0 :claim \"replacement sentence\" :basis \"quote\"}]\n"
                    ":add = genuinely new fact; :update = block N is now wrong/outdated; "
                    "NOTHING new → []. Never restate what the page already says. "
                    "Wrap entities/topics in [[wikilinks]] inside :claim.")
        call #(cheap-call prompt {:max-tokens 1200})
        parse #(when-not (:error %)
                 (->> (parse-edn-ops (:text %))
                      (filterv (fn [o] (and (#{:add :update} (:op o))
                                            (string? (:claim o)))))))]
    ;; reasoning models occasionally burn the budget and emit nothing —
    ;; one retry recovers most empties (logged so flakiness stays visible)
    (or (seq (parse (call)))
        (do (log/log! {:level :debug :id ::extract-retry :data {:page title}})
            (seq (parse (call))))
        [])))

(defn review-op
  "Adversarial grounding check: is `claim` supported by `source-text`?"
  [claim source-text]
  (let [r (cheap-call
           (str "Claim: " claim "\n\nSource:\n" source-text
                "\n\nIs the claim DIRECTLY supported by the source, with no invented "
                "details? Try to refute it. Answer ONLY yes or no.")
           {:max-tokens 300})]           ; reasoning models think before answering
    ;; take the LAST yes/no in the visible text (ignore any leaked deliberation)
    ;;
    ;; LOWERCASE BEFORE COMPARING. The regex is case-insensitive, so it happily
    ;; matched "Yes" — and the comparison was case-SENSITIVE, so "Yes." (the
    ;; form a model overwhelmingly answers with) read as NOT grounded. The
    ;; claim was then dropped and logged under ::grounding-failures, so the
    ;; merge silently lost grounded content and the log said the model had
    ;; failed to ground it.
    (= "yes" (some-> (last (re-seq #"(?i)yes|no" (str (:text r))))
                     str/lower-case))))

(defn apply-op!
  "Land one grounded op on `title`; provenance ref to `summary-uuid`."
  [kb-conn title blocks summary-uuid {:keys [op claim block]}]
  (let [page-uuid (d/q '[:find ?u . :in $ ?t :where
                         [?e :S.Page/title ?t] [?e :entity/uuid ?u]] @kb-conn title)
        content (str "<p>" claim "</p>")]
    (case op
      :add (let [bu (kbs/kb-upsert-knowledge-page! kb-conn title :summary claim)]
             bu)
      :update (when-let [target (:uuid (get blocks block))]
                (d/transact kb-conn [{:entity/uuid target :block/content content}])
                target))
    ;; provenance: the newest block referencing text also refs the summary page
    ;; NOTE: :db/id is not a datom — never pattern-match it in :where
    (when-let [nb (d/q '[:find (max ?b) . :in $ ?pu :where
                         [?p :entity/uuid ?pu] [?b :block/parent ?p]]
                       @kb-conn page-uuid)]
      (let [bu (:entity/uuid (d/pull @kb-conn [:entity/uuid] nb))]
        (d/transact kb-conn [{:entity/uuid bu
                              :block/references [[:entity/uuid summary-uuid]]}])))))

(defn merge-summary!
  "Fold `summary-text` into each of `links` (page titles) in `kb-conn`.
   `review?` gates the grounding check (measured toggle). Returns
   {:merged n :dropped n :per-page {...}}."
  [kb-conn summary-uuid summary-text links & {:keys [review?] :or {review? true}}]
  (reduce
   (fn [acc title]
     (let [blocks (page-blocks kb-conn title)
           ;; skip the summary's own kind — only topic pages get merged
           ops (or (extract-ops title blocks summary-text) [])
           {ok true bad false} (group-by #(or (not review?)
                                              (review-op (:claim %) summary-text)) ops)]
       (doseq [op ok] (apply-op! kb-conn title blocks summary-uuid op))
       (when (seq bad)
         (log/log! {:level :warn :id ::grounding-failures
                    :data {:page title :dropped (mapv :claim bad)}}))
       (-> acc
           (update :merged + (count ok))
           (update :dropped + (count bad))
           (assoc-in [:per-page title] {:applied (count ok) :dropped (count bad)}))))
   {:merged 0 :dropped 0 :per-page {}}
   links))
