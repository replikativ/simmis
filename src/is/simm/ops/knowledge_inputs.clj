(ns is.simm.ops.knowledge-inputs
  "Deterministic, frozen text inputs for ordinary knowledge workflows.
   Capturing does not authorize a caller, submit work, spend, update a cursor
   or retain source blobs. Persist the returned inputs through work admission."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.view :as view]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.knowledge-scope :as scope]
            [is.simm.ops.workflow-policy :as workflow-policy]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m])
  (:import [java.io PushbackReader StringReader]))

(def default-policy
  {:version 1 :trigger-roots ["inbox" "transcripts"]
   :context-roots ["wiki"] :exclude-prefixes []})

(def v1-policy-schema
  [:map {:closed true} [:version [:= 1]]
   [:trigger-roots [:vector {:min 1 :max 16} [:string {:min 1 :max 512}]]]
   [:context-roots [:vector {:max 16} [:string {:min 1 :max 512}]]]
   [:exclude-prefixes [:vector {:max 16} [:string {:min 1 :max 512}]]]])

(def v2-policy-schema
  [:map {:closed true} [:version [:= 2]]
   [:trigger-roots [:vector {:min 1 :max 16} [:string {:min 1 :max 512}]]]
   [:context-roots [:vector {:max 16} [:string {:max 512}]]]
   [:exclude-prefixes [:vector {:min 1 :max 16} [:string {:min 1 :max 512}]]]
   [:output-scope scope/schema]])

(def policy-schema [:or v1-policy-schema v2-policy-schema])

(defn output-scope [policy]
  (when (= 2 (:version policy)) (scope/normalize! (:output-scope policy))))

(defn- refuse! [reason]
  (throw (ex-info "Knowledge inputs cannot be captured" {:type ::refused :reason reason})))

(defn- prefixes! [roots]
  (->> roots (map #(if (= "" %) "" (str (files/path! %) "/"))) distinct sort vec))

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
    (when (and (= 1 (:version policy)) (some true? (for [a triggers b context] (overlap? a b))))
      (refuse! :overlapping-input-roles))
    (when (some true? (for [a (concat triggers (remove #(= "" %) context)) b ["workflows/" "in/" "out/"]]
                        (overlap? a b)))
      (refuse! :workflow-staging-is-not-knowledge-input))
    (when (= 2 (:version policy))
      (let [destination (get-in policy [:output-scope :destination-root])
            exclusions (concat scope/reserved (get-in policy [:output-scope :exclude-prefixes]))]
        (output-scope policy)
        (doseq [protected (concat (:trigger-roots policy) (:exclude-prefixes policy))]
          (when (and (or (scope/contains-path? destination protected)
                         (scope/contains-path? protected destination))
                     (not-any? #(scope/contains-path? % protected) exclusions))
            (refuse! :unprotected-source-output-overlap))))
      (when (some #(= "" %) triggers) (refuse! :root-trigger-forbidden)))
    (cond-> {:version (:version policy)
     :trigger-roots (mapv #(subs % 0 (dec (count %))) triggers)
     :context-roots (mapv #(if (= "" %) "" (subs % 0 (dec (count %)))) context)
     :exclude-prefixes (mapv #(subs % 0 (dec (count %))) excluded)}
      (= 2 (:version policy)) (assoc :output-scope (output-scope policy)))))

(defn- byte-count [^String text] (alength (.getBytes text "UTF-8")))

(def max-publication-basis-bytes (* 8 1024 1024))

(defn- path-parents [path]
  (let [parts (str/split path #"/")]
    (mapv #(str/join "/" (take % parts)) (range 1 (count parts)))))

(defn publication-basis
  "HOST-only admission snapshot for eventual /out/wiki -> /wiki proposals.
   Unlike selected model inputs, this covers ALL occupancy under wiki, including
   excluded/non-Markdown files and directories. It reads no content bodies and
   confers no authority by itself. Capture from the SAME authorized immutable DB
   as knowledge inputs; never reconstruct it from a model-authored manifest."
  ([db] (publication-basis db nil))
  ([db identity] (publication-basis db identity nil))
  ([db identity output-scope]
   (when output-scope (scope/normalize! output-scope))
   (when (and (some? identity)
              (not (and (= #{:registry-system :runtime-system} (set (keys identity)))
                        (publication/host-target? identity))))
     (refuse! :invalid-default-target))
   (let [root (if output-scope (:destination-root output-scope) "wiki")
         target (merge {:repository (:geschichte.repo/id (query/repository db))
                        :store (get-in db [:config :store :id])
                        :backend (get-in db [:config :store :backend])
                        :branch (get-in db [:config :branch])
                        :ref (query/current-ref db)} identity)
         rows (fn [attr]
                (if (= "" root)
                  (take (inc files/max-index-entries) (d/datoms db :avet attr))
                  (concat (when output-scope (mapcat #(d/datoms db :avet attr %) (path-parents root)))
                          (d/datoms db :avet attr root)
                          (remove #(= (str root "0") (:v %))
                                  (take (inc files/max-index-entries)
                                        (d/index-range db {:attrid attr :start (str root "/") :end (str root "0")}))))))
         occupancy (vec (take (inc files/max-index-entries)
                              (concat (rows :geschichte.work/path)
                                      (rows :geschichte.work-dir/path))))]
     (when-not (and (every? uuid? ((juxt :repository :store) target))
                    (keyword? (:backend target)) (keyword? (:branch target))
                    (string? (:ref target)) (not (str/blank? (:ref target))))
       (refuse! :missing-publication-target))
     (when (> (count occupancy) files/max-index-entries)
       (refuse! :too-many-publication-entries))
     (let [entries
           (into (sorted-map)
                 (keep (fn [datom]
                         (when (= :geschichte.work/path (:a datom))
                           (let [path (files/path! (:v datom)) metadata (files/entry db path)]
                             (when-not (and (uuid? (:content metadata))
                                            (integer? (:size metadata)) (<= 0 (:size metadata))
                                            (integer? (:mode metadata)))
                               (refuse! :invalid-publication-entry))
                             [path metadata])))) occupancy)
           explicit-dirs (mapv (comp files/path! :v)
                               (filter #(= :geschichte.work-dir/path (:a %)) occupancy))
           directories
           (reduce (fn [dirs path]
                     (let [dirs (conj dirs path)]
                      ;; Deep paths must not expand a bounded worktree scan into
                      ;; an unbounded retained directory inventory.
                       (when (> (+ (count entries) (count dirs)) files/max-index-entries)
                         (refuse! :too-many-publication-entries))
                       dirs))
                   (sorted-set)
                   (filter #(or (scope/contains-path? root %)
                                      (and output-scope (scope/contains-path? % root)))
                           (concat explicit-dirs (mapcat path-parents (concat (keys entries) explicit-dirs)))))
           basis (cond-> {:version (if output-scope 2 1) :root root :target target
                  :files entries :directories (vec directories)}
                   output-scope (assoc :scope output-scope))]
       (when (> (byte-count (pr-str basis)) max-publication-basis-bytes)
         (refuse! :publication-basis-too-large))
       (publication/validate!
        (assoc basis :id (hasch/uuid [(publication/digest-tag (:version basis)) basis])))))))

(defn publication-before
  "Expected state for one exact repository-relative output target, from the
   trusted admission basis rather than today's DB. Missing selected input is
   NOT proof of absence. Directories, unsupported modes and blocked ancestors
   are refused, never silently overwritten or followed."
  [basis path]
  (files/path! path)
  (if (= 2 (:version basis))
    (scope/target-path! (:scope basis) path)
    (when-not (and (= 1 (:version basis)) (= "wiki" (:root basis))
                   (str/starts-with? path "wiki/") (re-find #"(?i)\.(md|markdown)$" path))
      (refuse! :invalid-publication-path)))
  (when (or (contains? (set (:directories basis)) path)
            (some #(contains? (:files basis) %) (path-parents path)))
    (refuse! :publication-path-collision))
  (if-let [metadata (get (:files basis) path)]
    (do
      (when-not (= repo/default-mode (:mode metadata))
        (refuse! :unsupported-publication-target))
      (assoc metadata :present? true))
    {:present? false}))

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

(defn- excluded-roots [policy]
  (cond-> (:exclude-prefixes policy)
    (= 2 (:version policy)) (into scope/reserved)))

(defn role-path? [policy role path]
  (let [within? (fn [roots]
                  (some #(str/starts-with? path (if (= "" %) "" (str % "/"))) roots))]
    (and (within? (get policy (if (= role :trigger) :trigger-roots :context-roots)))
         (not (within? (excluded-roots policy)))
         (or (= :trigger role) (= 1 (:version policy))
             (not (within? (:trigger-roots policy)))))))

(defn selection
  "Metadata-only plan from ONE authorized immutable DB. Trigger identity excludes wiki
   context, so adopting an output cannot cause another scheduled model call.
   Execution identity covers all selected context. The byte copies, including
   manifest, obey ordinary saved-workflow input limits; nothing is truncated.
   Unchanged/empty work can stop here without reading source bodies."
  [db policy]
  (let [policy (normalize-policy policy)
        select (fn [roots]
                 (view/selection db {:roots roots :exclude-prefixes (if (= 2 (:version policy))
                                                                       (vec (remove #{".git" ".geschichte"} (excluded-roots policy)))
                                                                       (:exclude-prefixes policy))
                                     :schema-revision (if (= 2 (:version policy)) :knowledge-inputs-v2 :knowledge-inputs-v1)}))
        trigger (cond-> (select (:trigger-roots policy))
                  (= 2 (:version policy)) (update :files #(into (sorted-map) (filter (fn [[path _]] (role-path? policy :trigger path))) %)))
        context (cond-> (select (:context-roots policy))
                  (= 2 (:version policy)) (update :files #(into (sorted-map) (filter (fn [[path _]] (role-path? policy :context path))) %)))
        trigger-id (hasch/uuid {:version (:version policy) :repository (:repository-id trigger)
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
    (let [manifest {:version (:version policy) :repository (:repository-id trigger) :policy policy
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

(defn selection-of-files
  "Internal explicit frozen-subset plan from ONE authorized immutable DB.
   Does not enumerate the canonical body projection, read bodies or establish
   stream authority. The v1 evidence codec already supports exact subsets;
   live batch ownership is retained separately in its versioned HOST envelope."
  [db policy selected-files]
  (let [policy (normalize-policy policy)
        entry-schema [:map {:closed true} [:path [:string {:min 1 :max 512}]]
                      [:role [:enum :trigger :context]] [:content :uuid]
                      [:size [:int {:min 0 :max workflows/max-file-bytes}]] [:mode [:= repo/default-mode]]]
        _ (when-not (m/validate [:vector {:max 99} entry-schema] selected-files)
            (refuse! :invalid-explicit-selection))
        entries (vec (sort-by :path selected-files))
        _ (when-not (= (count entries) (count (set (map :path entries))))
            (refuse! :duplicate-input-path))
        _ (doseq [{:keys [path role content size] :as entry} entries]
            (files/path! path)
            (when-not (and (re-find #"(?i)\.(md|markdown)$" path)
                           (= (select-keys entry [:content :size :mode]) (files/entry db path))
                           (= size (:geschichte.content/size (d/entity db [:geschichte.content/id content])))
                           (role-path? policy role path)
                           (<= (count (str "/in/knowledge/sources/" path)) 512))
              (refuse! :explicit-source-mismatch)))
        by-role (fn [role]
                  (into (sorted-map)
                        (for [entry entries :when (= role (:role entry))]
                          [(:path entry) (select-keys entry [:content :size :mode])])))
        trigger (by-role :trigger) context (by-role :context)
        repository (:geschichte.repo/id (query/repository db))
        _ (when-not (uuid? repository) (refuse! :invalid-repository))
        trigger-id (hasch/uuid {:version (:version policy) :repository repository :policy policy :files trigger})
        execution-id (hasch/uuid {:trigger trigger-id :context context})
        manifest {:version (:version policy) :repository repository :policy policy
                  :trigger-id trigger-id :execution-id execution-id
                  :files (mapv #(assoc % :input (str "/in/knowledge/sources/" (:path %))) entries)}
        text (pr-str manifest) size (byte-count text)
        total (+ size (reduce + 0 (map :size entries)))]
    (when (> size workflows/max-file-bytes) (refuse! :manifest-too-large))
    (when (> total (:max-input-bytes workflow-policy/policy)) (refuse! :too-many-input-bytes))
    {:trigger-id trigger-id :execution-id execution-id :empty? (empty? trigger)
     :manifest manifest :manifest-text text}))

(defn- capture-selected [db {:keys [manifest manifest-text] :as selected}]
  (-> selected
      (dissoc :manifest-text)
      (assoc :inputs
             (into (sorted-map "/in/knowledge/manifest.edn" manifest-text)
                   (map (fn [{:keys [input content size]}]
                          (let [bytes (content/read-by-id (atom db) content)]
                            (when-not (and bytes (= size (alength ^bytes bytes)))
                              (refuse! :source-unavailable))
                            [input (markdown/decode bytes)])))
                   (:files manifest)))))

(declare verify-capture)

(defn capture-files
  "Freeze an explicit exact subset; no live batch claim or processing receipt.
   Call only while the HOST owns and retains this original source checkpoint."
  [db policy selected-files]
  (verify-capture policy (capture-selected db (selection-of-files db policy selected-files))))

(defn capture
  "Frozen byte copies from ONE authorized immutable DB. Manual, scheduled and
   benchmark callers pass the same :inputs map to ordinary workflow admission.
   Persist it before execution; this function neither pins blobs nor submits.
   :empty? means no selected trigger evidence."
  [db policy]
  (capture-selected db (selection db policy)))

(defn verify-manifest
  "Validate retained evidence metadata without reading source or copied text.
   Identity consistency is not proof of source selection or execution authority."
  [policy manifest]
  (let [policy (normalize-policy policy)
        entry-schema [:map {:closed true}
                      [:path [:string {:min 1 :max 512}]] [:input [:string {:min 1 :max 512}]]
                      [:role [:enum :trigger :context]] [:content :uuid]
                      [:size [:int {:min 0 :max workflows/max-file-bytes}]] [:mode [:= repo/default-mode]]]
        manifest-schema [:map {:closed true} [:version [:enum 1 2]] [:repository :uuid]
                         [:policy policy-schema] [:trigger-id :uuid] [:execution-id :uuid]
                         [:files [:vector {:max 99} entry-schema]]]]
    (when-not (and (m/validate manifest-schema manifest) (= (:version policy) (:version manifest))
                   (= policy (:policy manifest)))
      (refuse! :invalid-frozen-inputs))
    (let [entries (:files manifest)
          by-role (fn [role]
                    (into (sorted-map)
                          (keep (fn [{:keys [path] :as entry}]
                                  (when (= role (:role entry))
                                    [path (select-keys entry [:content :size :mode])]))) entries))
          trigger (by-role :trigger) context (by-role :context)
          expected-trigger (hasch/uuid {:version (:version policy) :repository (:repository manifest)
                                        :policy policy :files trigger})]
      (when-not (and (= (count entries) (count (set (map :path entries))))
                     (= (count entries) (count (set (map :input entries))))
                     (= (:trigger-id manifest) expected-trigger)
                     (= (:execution-id manifest) (hasch/uuid {:trigger expected-trigger :context context})))
        (refuse! :invalid-frozen-identity))
      (doseq [{:keys [path input role]} entries]
        (files/path! path)
        (let [prefixes (prefixes! (get policy (if (= role :trigger) :trigger-roots :context-roots)))
              excluded (prefixes! (:exclude-prefixes policy))]
          (when-not (and (= input (str "/in/knowledge/sources/" path))
                         (role-path? policy role path))
            (refuse! :invalid-frozen-source))))
      manifest)))

(defn verify-capture
  "Verify retained frozen evidence without looking up the live room. Content
   hashes check copied bytes, not the truth of an authored claim or authority
   to the original repository. Benchmark fixtures may use the same contract."
  [policy {:keys [manifest inputs trigger-id execution-id empty?] :as captured}]
  (verify-manifest policy manifest)
  (when-not (and (map? inputs) (<= (count inputs) (:max-input-files workflow-policy/policy))
                 (every? string? (keys inputs)) (every? string? (vals inputs))
                 (every? #(<= (byte-count %) workflows/max-file-bytes) (vals inputs))
                 (<= (reduce + 0 (map byte-count (vals inputs))) (:max-input-bytes workflow-policy/policy))
                 (= trigger-id (:trigger-id manifest)) (= execution-id (:execution-id manifest))
                 (= (pr-str manifest) (get inputs "/in/knowledge/manifest.edn")))
    (refuse! :invalid-frozen-inputs))
  (when-not (and (= (set (keys inputs))
                   (conj (set (map :input (:files manifest))) "/in/knowledge/manifest.edn"))
                 (= empty? (not-any? #(= :trigger (:role %)) (:files manifest))))
    (refuse! :invalid-frozen-identity))
  (doseq [{:keys [input content size]} (:files manifest)]
    (let [bytes (.getBytes ^String (get inputs input) "UTF-8")]
      (when-not (and (= size (alength bytes)) (= content (blob/blob-id bytes)))
        (refuse! :invalid-frozen-source))))
  captured)

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
