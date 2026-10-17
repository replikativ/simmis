(ns is.simm.ops.workflows
  "Workflow bundles are ordinary versioned Geschichte files, not a new
   definition store. Shape inspection never executes their checker source."
  (:require [clojure.string :as str]
            [dvergr.catalog.room :as catalog]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.nio ByteBuffer]
           [java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets]))

(def max-files 100)
(def max-file-bytes 524288)
(def max-bundle-bytes 2097152)
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

(defn snapshot
  "Read ONE immutable worktree manifest, then content by its pinned hashes.
   Concurrent edits cannot mix two bundle revisions. Missing content or a
   size/hash mismatch refuses the read rather than using a newer file."
  [room name]
  (let [prefix (prefix! name)]
    (binding [ec/*execution-context* (:ctx room)]
      (let [conn (or (:conn (workspace/current-workspace))
                     (throw (ex-info "Room has no workspace" {:type ::unavailable})))
            db @conn
            entries (into (sorted-map)
                          (filter (fn [[p _]] (str/starts-with? (str "/" p) prefix)))
                          (query/worktree db))]
        (when (> (count entries) max-files) (invalid! "Workflow has too many files"))
        (when (> (reduce + 0 (map (comp :size val) entries)) max-bundle-bytes)
          (invalid! "Workflow exceeds the byte limit"))
        {:files
         (into (sorted-map)
               (map (fn [[path {:keys [size mode] id :content}]]
                      (when-not (and (integer? size) (<= 0 size max-file-bytes)
                                     (= 0x8000 (bit-and mode 0xF000)))
                        (invalid! "Workflow entries must be bounded regular text files"))
                      (files/workspace-path (str "/" path))
                      (let [^bytes bytes (content/read-by-id conn id)]
                        (when-not (= size (alength bytes))
                          (invalid! "Workflow content does not match its pinned size"))
                        [(subs (str "/" path) (count prefix)) (strict-text bytes)])))
               entries)
         :manifest (mapv (fn [[p entry]]
                           (assoc entry :path (subs (str "/" p) (count prefix)))) entries)}))))

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
        (let [bundle (catalog/bundle name (:files snapshot))]
          (when-not (m/validate catalog/Definition (:definition bundle))
            (invalid! "workflow.edn must be a valid workflow definition map"))
          {:ok true :name name :bundle (str (:id bundle))
           :title (get-in bundle [:definition :title])
           :files (count (:manifest snapshot))
           :bytes (reduce + 0 (map :size (:manifest snapshot)))
           :cases (vec (keys (:cases bundle)))
           :checker-executed? false})
        (catch Exception e
          {:ok false :name name :checker-executed? false :problems [(ex-message e)]})))))

(defn details
  "One bounded editable source revision and its non-executing inspection.
   A malformed draft still has an identity and can be repaired safely."
  [room name]
  (let [source (snapshot room name)
        revision (str (source-revision name source))
        inspection (assoc (inspect-snapshot name source) :revision revision)]
    {:name name :revision revision
     :files (:files source) :inspection inspection}))

(defn inspect [room name]
  (:inspection (details room name)))

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
            {:write written :workflow (try (inspect room name)
                                           (catch Exception e
                                             {:ok false :name name :checker-executed? false
                                              :problems [(ex-message e)]}))}))))))
