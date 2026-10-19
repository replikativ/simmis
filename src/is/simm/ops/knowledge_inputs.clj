(ns is.simm.ops.knowledge-inputs
  "Deterministic, frozen text inputs for ordinary knowledge workflows.
   Capturing does not authorize a caller, submit work, spend, update a cursor
   or retain source blobs. Persist the returned inputs through work admission."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.view :as view]
            [is.simm.ops.workflow-policy :as workflow-policy]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m])
  (:import [java.io PushbackReader StringReader]))

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

(defn declaration
  "Optional inert input-selection.edn sidecar. Its exact bytes belong to the
   ordinary bundle identity. No source forms, custom tags or trailing values
   execute; only the closed versioned knowledge selection format is admitted."
  [source]
  (when-let [text (get source "input-selection.edn")]
    (when-not (and (string? text) (<= (count text) 4096))
      (refuse! :invalid-declaration))
    (let [value
          (try
            (with-open [reader (PushbackReader. (StringReader. text))]
              (let [options {:eof ::eof :readers {} :default (fn [& _] (refuse! :invalid-declaration))}
                    value (edn/read options reader)]
                (when-not (= ::eof (edn/read options reader)) (refuse! :invalid-declaration))
                value))
            (catch StackOverflowError _ (refuse! :invalid-declaration))
            (catch Exception _ (refuse! :invalid-declaration)))]
      (when-not (m/validate [:map {:closed true} [:kind [:= :knowledge]] [:policy policy-schema]] value)
        (refuse! :invalid-declaration))
      (normalize-policy (:policy value)))))

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

(defn verify-capture
  "Verify retained frozen evidence without looking up the live room. Content
   hashes check copied bytes, not the truth of an authored claim or authority
   to the original repository. Benchmark fixtures may use the same contract."
  [policy {:keys [manifest inputs trigger-id execution-id empty?] :as captured}]
  (let [policy (normalize-policy policy)
        entry-schema [:map {:closed true}
                      [:path [:string {:min 1 :max 512}]] [:input [:string {:min 1 :max 512}]]
                      [:role [:enum :trigger :context]] [:content :uuid]
                      [:size [:int {:min 0 :max workflows/max-file-bytes}]] [:mode [:= repo/default-mode]]]
        manifest-schema [:map {:closed true} [:version [:= 1]] [:repository :uuid]
                         [:policy policy-schema] [:trigger-id :uuid] [:execution-id :uuid]
                         [:files [:vector {:max 99} entry-schema]]]]
    (when-not (and (map? inputs) (<= (count inputs) (:max-input-files workflow-policy/policy))
                   (every? string? (keys inputs)) (every? string? (vals inputs))
                   (every? #(<= (byte-count %) workflows/max-file-bytes) (vals inputs))
                   (<= (reduce + 0 (map byte-count (vals inputs)))
                       (:max-input-bytes workflow-policy/policy))
                   (m/validate manifest-schema manifest)
                   (= policy (:policy manifest))
                   (= trigger-id (:trigger-id manifest)) (= execution-id (:execution-id manifest))
                   (= (pr-str manifest) (get inputs "/in/knowledge/manifest.edn")))
      (refuse! :invalid-frozen-inputs))
    (let [entries (:files manifest)
          by-role (fn [role]
                    (into (sorted-map)
                          (keep (fn [{:keys [path] :as entry}]
                                  (when (= role (:role entry))
                                    [path (select-keys entry [:content :size :mode])]))) entries))
          trigger (by-role :trigger) context (by-role :context)
          expected-trigger (hasch/uuid {:version 1 :repository (:repository manifest)
                                       :policy policy :files trigger})]
      (when-not (and (= (count entries) (count (set (map :path entries))))
                     (= (set (keys inputs))
                        (conj (set (map :input entries)) "/in/knowledge/manifest.edn"))
                     (= trigger-id expected-trigger)
                     (= execution-id (hasch/uuid {:trigger trigger-id :context context}))
                     (= empty? (clojure.core/empty? trigger)))
        (refuse! :invalid-frozen-identity))
      (doseq [{:keys [path input content size role]} entries]
        (files/path! path)
        (let [prefixes (prefixes! (get policy (if (= role :trigger) :trigger-roots :context-roots)))
              excluded (prefixes! (:exclude-prefixes policy))
              text (get inputs input)
              bytes (.getBytes ^String text "UTF-8")]
          (when-not (and (= input (str "/in/knowledge/sources/" path))
                         (some #(str/starts-with? path %) prefixes)
                         (not-any? #(str/starts-with? path %) excluded)
                         (= size (alength bytes)) (= content (blob/blob-id bytes)))
            (refuse! :invalid-frozen-source))))
      captured)))

(defn from-inputs
  "Reconstruct retained evidence from ordinary workflow/case inputs. This
   never resolves a live repository. Only collector-owned paths are selected;
   the same byte/hash/identity validation applies as to host captures."
  [policy input-files]
  (let [owned (into (sorted-map) (filter #(str/starts-with? (key %) "/in/knowledge/")) input-files)
        text (get owned "/in/knowledge/manifest.edn")]
    (when-not (and (string? text) (<= (byte-count text) workflows/max-file-bytes))
      (refuse! :missing-frozen-manifest))
    (let [manifest
          (try
            (with-open [reader (PushbackReader. (StringReader. text))]
              (let [options {:eof ::eof :readers {} :default (fn [& _] (refuse! :invalid-frozen-manifest))}
                    value (edn/read options reader)]
                (when-not (= ::eof (edn/read options reader)) (refuse! :invalid-frozen-manifest))
                value))
            (catch StackOverflowError _ (refuse! :invalid-frozen-manifest))
            (catch Exception _ (refuse! :invalid-frozen-manifest)))]
      (verify-capture policy {:manifest manifest :inputs owned
                              :trigger-id (:trigger-id manifest) :execution-id (:execution-id manifest)
                              :empty? (not-any? #(= :trigger (:role %)) (:files manifest))}))))
