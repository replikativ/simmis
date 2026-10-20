(ns is.simm.ops.workflows
  "Workflow bundles are ordinary versioned Geschichte files, not a new
   definition store. Shape inspection never executes their checker source."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [hasch.core :as hasch]
            [is.simm.ops.benchmark-dataset :as dataset]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.nio ByteBuffer]
           [java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets]))

(def max-files 100)
(def max-file-bytes 524288)
(def max-bundle-bytes 2097152)
(def list-page-size 25)
(def max-list-entries 20000)
(def name-schema [:re #"^[a-z0-9][a-z0-9-]{0,63}$"])
(def write-schema
  [:map {:closed true}
   [:name name-schema]
   [:expected-revision {:optional true} :uuid]
   [:files [:map-of {:min 1 :max max-files}
            [:string {:min 1 :max 512}]
            [:maybe [:string {:max max-file-bytes}]]]]])
(def transport-write-schema
  [:map {:closed true}
   [:name name-schema]
   [:expected-revision {:optional true} [:string {:min 36 :max 36}]]
   [:files [:map-of {:min 1 :max max-files}
            [:or [:string {:min 1 :max 512}] :keyword]
            [:maybe [:string {:max max-file-bytes}]]]]])

(defn- invalid! [message]
  (throw (ex-info message {:type ::invalid})))

(defn transport-write
  "Dvergr HTTP keywordizes JSON object keys, including filenames. Normalize
   only those keys; the internal write path still applies its strict schema."
  [input]
  (when-not (m/validate transport-write-schema input) (invalid! "Invalid workflow edits"))
  (cond-> (update input :files
                  (fn [files]
                    (reduce-kv (fn [out key value]
                                 (let [path (if (keyword? key) (subs (str key) 1) key)]
                                   (when (contains? out path) (invalid! "Duplicate workflow filename"))
                                   (assoc out path value))) {} files)))
    (:expected-revision input)
    (assoc :expected-revision (or (parse-uuid (:expected-revision input))
                                  (invalid! "Invalid workflow source revision")))))

(defn- prefix! [name]
  ;; re-matches also rejects a trailing newline (Java's $ alone need not).
  (when-not (and (string? name) (re-matches #"[a-z0-9][a-z0-9-]{0,63}" name))
    (invalid! "Invalid workflow name"))
  (str "/workflows/" name "/"))

(defn- strict-text [bytes]
  (try
    (str (.decode (doto (.newDecoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT)
                    (.onUnmappableCharacter CodingErrorAction/REPORT))
                  (ByteBuffer/wrap bytes)))
    (catch CharacterCodingException _
      (invalid! "Workflow entries must contain valid UTF-8 text"))))

(defn- snapshot-entries!
  "Inspect every prefix path entity, including incomplete rows hidden by a
   worktree inner join. Bound the index BEFORE entity or content access."
  [db name]
  (let [prefix (prefix! name)
        stem (subs prefix 1)
        end (str (subs stem 0 (dec (count stem))) "0")
        ;; Datahike's upper bound is inclusive. Its exact sentinel is outside
        ;; this directory and can only be the LAST row. Take before removing
        ;; that one value so native acquisition remains bounded by101.
        rows (into [] (comp (take (inc max-files)) (remove #(= end (:v %))))
                   (d/index-range db {:attrid :geschichte.work/path :start stem :end end}))]
    (when (> (count rows) max-files) (invalid! "Workflow has too many files"))
    (let [entries (reduce (fn [entries row]
                            (let [path (:v row) entity (d/entity db (:e row))]
                              (when-not (and (string? path) (str/starts-with? path stem)
                                             (= path (:geschichte.work/path entity))
                                             (not (contains? entries path)))
                                (invalid! "Invalid workflow path metadata"))
                              (assoc entries path
                                     {:content (:geschichte.content/id (:geschichte.work/content entity))
                                      :size (:geschichte.work/size entity) :mode (:geschichte.work/mode entity)})))
                          (sorted-map) rows)]
      (when (some #(> (- (count (key %)) (dec (count prefix))) 512) entries)
        (invalid! "Workflow source paths exceed the 512-character limit"))
      (doseq [[path {:keys [size mode content]}] entries]
        (when-not (and (integer? size) (<= 0 size max-file-bytes)
                       (integer? mode) (= 0x8000 (bit-and mode 0xF000)) (uuid? content)
                       (= (str "/" path) (files/workspace-path (str "/" path))))
          (invalid! "Workflow entries must be bounded regular text files")))
      (when (> (reduce + 0 (map (comp :size val) entries)) max-bundle-bytes)
        (invalid! "Workflow exceeds the byte limit"))
        ;; A tiny worktree size must not hide oversized/missing content metadata.
        ;; Validate the entire bounded manifest before reading the first body.
      (doseq [[_ {:keys [size content]}] entries]
        (when-not (= size (:geschichte.content/size (d/entity db [:geschichte.content/id content])))
          (invalid! "Workflow content metadata does not match its pinned size")))
      entries)))

(defn snapshot-in
  "Read manifest, content metadata and bodies from ONE immutable DB. This is a
   storage projector, not authorization, selection or a lease against GC."
  [db name]
  (let [prefix (prefix! name)
        entries (snapshot-entries! db name)
        reader (atom db)]
    {:files
     (into (sorted-map)
           (map (fn [[path {:keys [size mode] id :content}]]
                  (let [^bytes bytes (content/read-by-id reader id)]
                    (when-not (and bytes (= size (alength bytes)))
                      (invalid! "Workflow content does not match its pinned size"))
                    [(subs (str "/" path) (count prefix)) (strict-text bytes)])))
           entries)
     :manifest (mapv (fn [[p entry]]
                       (assoc entry :path (subs (str "/" p) (count prefix)))) entries)}))

(defn snapshot
  "Selected workspace compatibility wrapper, including isolated Run worlds."
  [room name]
  (prefix! name)
  (binding [ec/*execution-context* (:ctx room)]
    (let [conn (or (:conn (workspace/current-workspace))
                   (throw (ex-info "Room has no workspace" {:type ::unavailable})))]
      (snapshot-in @conn name))))

(defn source-revision
  "Opaque source identity, also valid for empty or structurally invalid drafts.
   Separate from the validated bundle's execution identity."
  [name snapshot]
  (hasch/uuid {:name name :manifest (:manifest snapshot)}))

(defn inspect-snapshot [name snapshot]
  (let [problems (catalog/check-files (:files snapshot))]
    (if (seq problems)
      {:ok false :name name :checker-executed? false :problems problems}
      (try
        (let [bundle (catalog/bundle name (:files snapshot))
              evaluation (dataset/manifest (:files snapshot) (keys (:cases bundle)))]
          (when-not (m/validate catalog/Definition (:definition bundle))
            (invalid! "workflow.edn must be a valid workflow definition map"))
          (cond-> {:ok true :name name :bundle (str (:id bundle))
                   :title (get-in bundle [:definition :title])
                   :files (count (:manifest snapshot))
                   :bytes (reduce + 0 (map :size (:manifest snapshot)))
                   :cases (vec (keys (:cases bundle)))
                   :checker-executed? false}
            evaluation (assoc :evaluation evaluation)))
        (catch Exception e
          {:ok false :name name :checker-executed? false :problems [(ex-message e)]})))))

(defn- source-details [name source]
  (let [revision (str (source-revision name source))
        inspection (assoc (inspect-snapshot name source) :revision revision)]
    (cond-> {:name name :revision revision
             :files (:files source) :inspection inspection}
      (:ok inspection)
      (assoc :definition (select-keys (:definition (catalog/bundle name (:files source)))
                                      [:title :task :params :capture])))))

(defn details-in
  "Editable source and inert inspection from one supplied immutable DB."
  [db name]
  (source-details name (snapshot-in db name)))

(defn details
  "One bounded editable source revision and its non-executing inspection.
   A malformed draft still has an identity and can be repaired safely."
  [room name]
  (source-details name (snapshot room name)))

(defn- list-arguments! [{:keys [offset expected-revision] :or {offset 0}}]
  (when-not (and (integer? offset) (<= 0 offset max-list-entries)
                 (or (nil? expected-revision) (uuid? expected-revision)))
    (invalid! "Invalid workflow list page")))

(defn list-page-in
  "Room-local names, including drafts, from a bounded immutable path-index scan.
   No file contents are loaded. Revision pins the sorted name set across pages."
  [db {:keys [offset expected-revision] :or {offset 0} :as request}]
  (list-arguments! request)
  (let [entries (into [] (comp (take (inc max-list-entries))
                               (remove #(= "workflows0" (:v %))))
                      (d/index-range db {:attrid :geschichte.work/path
                                         :start "workflows/" :end "workflows0"}))]
    (when (> (count entries) max-list-entries) (invalid! "Workflow path index exceeds the listing limit"))
    (let [names (vec (sort (into #{}
                                 (keep (fn [datom]
                                         (let [parts (str/split (:v datom) #"/" 3)
                                               name (second parts)]
                                           (when (and (= 3 (count parts))
                                                      (re-matches #"[a-z0-9][a-z0-9-]{0,63}" name)) name))))
                                 entries)))
          revision (hasch/uuid [:workflow-names names])
          end (min (count names) (+ offset list-page-size))]
      (when (and expected-revision (not= expected-revision revision))
        (throw (ex-info "Workflow names changed; reload the list" {:type ::conflict})))
      (when (> offset (count names)) (invalid! "Workflow list offset is outside this revision"))
      (cond-> {:names (subvec names offset end) :offset offset :total (count names)
               :revision (str revision) :checker-executed? false}
        (< end (count names)) (assoc :next-offset end)))))

(defn list-page
  "Selected workspace name listing. Existing content-only page IDs are kept."
  [room request]
  (list-arguments! request)
  (binding [ec/*execution-context* (:ctx room)]
    (let [conn (or (:conn (workspace/current-workspace))
                   (throw (ex-info "Room has no workspace" {:type ::unavailable})))]
      (list-page-in @conn request))))

(defn inspect [room name]
  (:inspection (details room name)))

(defn public-inspection
  "Bound diagnostics and metadata for MCP's finite JSON result envelope.
   Full source and case filenames remain available through source pages."
  [inspection]
  (cond-> (select-keys inspection [:ok :name :bundle :revision :files :bytes :checker-executed? :evaluation])
    (:title inspection) (assoc :title (subs (:title inspection) 0 (min 256 (count (:title inspection)))))
    (contains? inspection :problems)
    (assoc :problems (mapv #(subs % 0 (min 256 (count %))) (take 10 (:problems inspection)))
           :problem-count (count (:problems inspection)))
    (contains? inspection :cases)
    (assoc :cases (vec (take 8 (:cases inspection))) :case-count (count (:cases inspection)))))

(defn public-write [result]
  {:write (assoc (select-keys (:write result) [:commit])
                 :written-count (count (get-in result [:write :written]))
                 :removed-count (count (get-in result [:write :removed])))
   :workflow (public-inspection (:workflow result))})

(defn- source-page*
  "MCP-sized source metadata or bytes of one exact source revision.
   Browser editors can use details directly after the same canonical read gate."
  [source {:keys [name path offset file-offset expected-revision]
           :or {offset 0 file-offset 0}}]
  (when (and expected-revision (not= expected-revision (:revision source)))
    (throw (ex-info "Workflow source changed; reload metadata" {:type ::conflict})))
  (if path
    (do
      (when-not expected-revision (invalid! "Source chunks require expected-revision"))
      (let [text (get-in source [:files path])]
        (when-not text (invalid! "No file in this workflow source revision"))
        (let [bytes (.getBytes ^String text StandardCharsets/UTF_8)
              size (alength bytes)
              end (min size (+ offset 8192))]
          (when-not (<= 0 offset size) (invalid! "Source offset is outside this file"))
          (cond-> {:name name :revision (:revision source) :path path :offset offset :size size
                   :base64 (.encodeToString (java.util.Base64/getEncoder)
                                            (java.util.Arrays/copyOfRange bytes (int offset) (int end)))}
            (< end size) (assoc :next-offset end)))))
    (let [entries (vec (sort-by key (:files source)))
          end (min (count entries) (+ file-offset 10))
          inspection (:inspection source)]
      (when-not (<= 0 file-offset (count entries)) (invalid! "Source file page is outside this revision"))
      (cond-> {:name name :revision (:revision source) :file-offset file-offset :file-count (count entries)
               :files (mapv (fn [[path text]] {:path path :bytes (alength (.getBytes ^String text StandardCharsets/UTF_8))})
                            (subvec entries file-offset end))
               :inspection (-> (select-keys inspection [:ok :name :bundle :checker-executed?])
                               (assoc :problems (mapv #(subs % 0 (min 256 (count %))) (take 10 (:problems inspection)))
                                      :problem-count (count (:problems inspection))))}
        (< end (count entries)) (assoc :next-file-offset end)))))

(defn- source-arguments! [{:keys [name path offset file-offset expected-revision]
                           :or {offset 0 file-offset 0}}]
  (prefix! name)
  (when-not (and (integer? offset) (<= 0 offset max-file-bytes)
                 (integer? file-offset) (<= 0 file-offset max-files)
                 (or (nil? expected-revision)
                     (and (string? expected-revision) (parse-uuid expected-revision)))
                 (or (nil? path) (and (string? path) (<= 1 (count path) 512))))
    (invalid! "Invalid workflow source page"))
  (when (and (or path (pos? file-offset)) (nil? expected-revision))
    (invalid! "Later source pages require expected-revision")))

(defn source-page-in
  "Bounded source metadata/chunks from the supplied immutable DB. Content-only
   source revisions remain unchanged; the host binds target identity separately."
  [db request]
  (source-arguments! request)
  (source-page* (details-in db (:name request)) request))

(defn source-page
  "Selected workspace compatibility source paging, without executing checkers."
  [room request]
  (source-arguments! request)
  (source-page* (details room (:name request)) request))

(defn write!
  "One bounded atomic repository commit, restricted to this bundle's subtree.
   Nil removes a path. Other room files (including AGENTS.md) are not writable
   through this capability. Author identity is supplied by the host."
  [room principal {:keys [name expected-revision] edits :files :as input}]
  (when-not (m/validate write-schema input) (invalid! "Invalid workflow edits"))
  (let [prefix (prefix! name)
        entries (reduce-kv
                 (fn [out path text]
                   (when (str/starts-with? path "/")
                     (invalid! "Workflow paths must be relative to the named bundle"))
                   (let [normalized (subs (files/workspace-path path) 1)
                         target (files/workspace-path (str prefix normalized))]
                     (when (contains? out target) (invalid! "Duplicate normalized workflow path"))
                     (when (and text (> (alength (.getBytes ^String text StandardCharsets/UTF_8)) max-file-bytes))
                       (invalid! "Workflow file exceeds the byte limit"))
                     (assoc out target (when text {:text text}))))
                 {} edits)
        size (reduce + 0 (keep (fn [[_ v]] (some-> (:text v) (.getBytes StandardCharsets/UTF_8) alength)) entries))]
    (when (> size max-bundle-bytes) (invalid! "Workflow edit batch exceeds the byte limit"))
    (binding [ec/*execution-context* (:ctx room)]
      (let [conn (or (:conn (workspace/current-workspace))
                     (throw (ex-info "Room has no workspace" {:type ::unavailable})))]
        (locking (workspace/workspace-lock conn)
          (when (and expected-revision
                     (not= expected-revision (source-revision name (snapshot room name))))
            (throw (ex-info "Workflow source changed; reload before saving"
                            {:type ::conflict :name name})))
          (let [written (files/write! room entries {:message (str "Update workflow " name " by " (:principal/id principal))})]
      ;; A committed edit is acknowledged even when the resulting bundle is
      ;; incomplete/oversized/invalid. Inspection is of the CURRENT snapshot;
      ;; benchmark submission pins and validates its content identity again.
            (try
              (let [source (details room name)]
                {:write written :workflow (:inspection source) :definition (:definition source)})
              (catch Exception e
                {:write written :workflow {:ok false :name name :checker-executed? false
                                           :problems [(ex-message e)]}}))))))))
