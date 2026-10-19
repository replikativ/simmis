(ns is.simm.ops.knowledge-inputs
  "Deterministic, frozen text inputs for ordinary knowledge workflows.
   Capturing does not authorize a caller, submit work, spend, update a cursor
   or retain source blobs. Persist the returned inputs through work admission."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.content :as content]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.view :as view]
            [is.simm.ops.workflow-policy :as workflow-policy]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]))

(def default-policy
  {:version 1 :trigger-roots ["inbox" "transcripts"]
   :context-roots ["wiki"] :exclude-prefixes []})

(def policy-schema
  [:map {:closed true} [:version [:= 1]]
   [:trigger-roots [:vector {:min 1 :max 16} [:string {:min 1 :max 512}]]]
   [:context-roots [:vector {:max 16} [:string {:min 1 :max 512}]]]
   [:exclude-prefixes [:vector {:max 16} [:string {:min 1 :max 512}]]]])

(defn- refuse! [reason]
  (throw (ex-info "Knowledge inputs cannot be captured" {:type ::refused :reason reason})))

(defn- prefixes! [roots]
  (->> roots (map #(str (files/path! %) "/")) distinct sort vec))

(defn- overlap? [a b]
  (or (str/starts-with? a b) (str/starts-with? b a)))

(defn normalize-policy
  "Host-admitted folder policy, not code from a source file. Evidence and
   supporting knowledge are disjoint. Workflow/staging/output subtrees cannot
   become maintenance triggers or context accidentally."
  [policy]
  (when-not (m/validate policy-schema policy) (refuse! :invalid-policy))
  (let [triggers (prefixes! (:trigger-roots policy))
        context (prefixes! (:context-roots policy))
        excluded (prefixes! (:exclude-prefixes policy))]
    (when (some true? (for [a triggers b context] (overlap? a b)))
      (refuse! :overlapping-input-roles))
    (when (some true? (for [a (concat triggers context) b ["workflows/" "in/" "out/"]]
                       (overlap? a b)))
      (refuse! :workflow-staging-is-not-knowledge-input))
    {:version 1
     :trigger-roots (mapv #(subs % 0 (dec (count %))) triggers)
     :context-roots (mapv #(subs % 0 (dec (count %))) context)
     :exclude-prefixes (mapv #(subs % 0 (dec (count %))) excluded)}))

(defn- byte-count [^String text] (alength (.getBytes text "UTF-8")))

(defn selection
  "Metadata-only plan from ONE authorized immutable DB. Trigger identity excludes wiki
   context, so adopting an output cannot cause another scheduled model call.
   Execution identity covers all selected context. The byte copies, including
   manifest, obey ordinary saved-workflow input limits; nothing is truncated.
   Unchanged/empty work can stop here without reading source bodies."
  [db policy]
  (let [policy (normalize-policy policy)
        select (fn [roots]
                 (view/selection db {:roots roots :exclude-prefixes (:exclude-prefixes policy)
                                     :schema-revision :knowledge-inputs-v1}))
        trigger (select (:trigger-roots policy))
        context (select (:context-roots policy))
        trigger-id (hasch/uuid {:version 1 :repository (:repository-id trigger)
                                :policy policy :files (:files trigger)})
        execution-id (hasch/uuid {:trigger trigger-id :context (:files context)})
        entries (->> (concat (map (fn [[path metadata]] [path :trigger metadata]) (:files trigger))
                             (map (fn [[path metadata]] [path :context metadata]) (:files context)))
                     (sort-by first) vec)
        max-files (:max-input-files workflow-policy/policy)
        max-bytes (:max-input-bytes workflow-policy/policy)]
    (when (> (inc (count entries)) max-files) (refuse! :too-many-input-files))
    (doseq [[path _ {:keys [size content]}] entries]
      (when (> size workflows/max-file-bytes) (refuse! :input-file-too-large))
      (when (> (count (str "/in/knowledge/sources/" path)) 512) (refuse! :input-path-too-long))
      (when-not (= size (:geschichte.content/size (d/entity db [:geschichte.content/id content])))
        (refuse! :content-size-mismatch)))
    ;; Reject aggregate source before loading bytes; account for manifest too
    ;; after serialization. The view already verifies regular mode/metadata.
    (when (> (reduce + 0 (map #(get-in % [2 :size]) entries)) max-bytes)
      (refuse! :too-many-input-bytes))
    (let [manifest {:version 1 :repository (:repository-id trigger) :policy policy
                    :trigger-id trigger-id :execution-id execution-id
                    :files (mapv (fn [[path role metadata]]
                                   (merge metadata {:path path :role role
                                                    :input (str "/in/knowledge/sources/" path)})) entries)}
          manifest-text (pr-str manifest)
          total (+ (byte-count manifest-text) (reduce + 0 (map #(get-in % [2 :size]) entries)))]
      (when (> (byte-count manifest-text) workflows/max-file-bytes) (refuse! :manifest-too-large))
      (when (> total max-bytes) (refuse! :too-many-input-bytes))
      {:trigger-id trigger-id :execution-id execution-id :empty? (empty? (:files trigger))
       :manifest manifest :manifest-text manifest-text})))

(defn capture
  "Frozen byte copies from ONE authorized immutable DB. Manual, scheduled and
   benchmark callers pass the same :inputs map to ordinary workflow admission.
   Persist it before execution; this function neither pins blobs nor submits.
   :empty? means no selected trigger evidence."
  [db policy]
  (let [{:keys [manifest manifest-text] :as selected} (selection db policy)]
    (-> selected
        (dissoc :manifest-text)
        (assoc :inputs
               (into (sorted-map "/in/knowledge/manifest.edn" manifest-text)
                     (map (fn [{:keys [input content size]}]
                            (let [bytes (content/read-by-id (atom db) content)]
                              (when-not (and bytes (= size (alength ^bytes bytes)))
                                (refuse! :source-unavailable))
                              [input (markdown/decode bytes)])))
                     (:files manifest))))))
