(ns is.simm.documents.files
  "Bounded browser document operations over a room's canonical Geschichte
   worktree. No second file store, interpreter, cache or publication engine."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.effects :as effects]
            [dvergr.rooms.files :as room-files]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.identity :as identity]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.rich-text :as rich]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Base64]))

(def max-index-entries 20000)
(def max-open-bytes (* 8 1024 1024))

(defn- refuse! [kind message]
  (throw (ex-info message {:type kind})))

(defn path!
  "Exact relative path; no silent normalization of an editor's identity."
  [path]
  (when-not (and (string? path) (<= 1 (count path) 512)
                 (not (re-find #"[\\\p{Cntrl}]" path))
                 (every? #(and (not (str/blank? %)) (not (#{"." ".."} %)))
                         (str/split path #"/" -1))
                 (= (str "/" path) (room-files/workspace-path path)))
    (refuse! ::invalid "Use an exact relative workspace path"))
  path)

(defn with-workspace [room f]
  (binding [ec/*execution-context* (:ctx room)]
    (let [conn (:conn (workspace/current-workspace))]
      (when-not conn (refuse! ::unavailable "This room has no file workspace"))
      (f conn))))

(defn entry [db path]
  (when-let [eid (:e (first (d/datoms db :avet :geschichte.work/path path)))]
    (let [row (d/pull db '[:geschichte.work/size :geschichte.work/mode
                           {:geschichte.work/content [:geschichte.content/id]}] eid)]
      {:content (get-in row [:geschichte.work/content :geschichte.content/id])
       :size (:geschichte.work/size row) :mode (:geschichte.work/mode row)})))

(defn file-kind [path]
  (cond
    (re-find #"(?i)\.(md|markdown)$" path) :markdown
    (re-find #"(?i)\.(docx|xlsx|pptx|odt|ods|odp)$" path) :office
    (re-find #"(?i)\.(png|jpe?g|gif|webp)$" path) :image
    (re-find #"(?i)\.(clj[cs]?|edn|bb|js|jsx|ts|tsx|json|ya?ml|xml|html?|css|sh|py|sql|csv|txt|log)$" path) :text
    :else :file))

(defn- folder-from-db [db folder]
  (when-not (= "" folder) (path! folder))
  (let [prefix (if (seq folder) (str folder "/") "")
        upper (if (seq folder) (str folder "0") "\uffff")
        rows (vec (take (inc max-index-entries)
                        (d/index-range db {:attrid :geschichte.work/path :start prefix :end upper})))]
    (when (> (count rows) max-index-entries)
      (refuse! ::limit "This folder exceeds the bounded metadata listing limit"))
    (let [items
          (reduce
           (fn [items datom]
             (let [path (:v datom)
                   allowed? (try (path! path) true (catch Exception _ false))]
               (if (and allowed? (str/starts-with? path prefix))
                 (let [[name rest] (str/split (subs path (count prefix)) #"/" 2)
                       key (str prefix name)
                       item (if rest {:path key :name name :kind :folder}
                                {:path key :name name :kind (file-kind path) :entry (entry db path)})
                       prior (get items key)]
                   (when (and prior (not= (= :folder (:kind prior)) (= :folder (:kind item))))
                     (refuse! ::corrupt "A workspace path is both a folder and a file"))
                   (assoc items key item))
                 items))) (sorted-map) rows)]
      {:path folder :entries (vec (sort-by (juxt #(not= :folder (:kind %)) :name) (vals items)))
       :revision (hasch/uuid items)})))

(defn list-folder
  "Metadata only. Every Markdown file is knowledge regardless of folder name.
   A conventional wiki/ folder is organization, not a special storage silo."
  [room folder]
  (with-workspace room #(folder-from-db @% folder)))

(defn metadata
  "One immutable, bounded workspace snapshot. No file bodies or content blobs.
   Missing requested paths are explicit nil entries, not failed file reads."
  [room request]
  (when-not (and (= #{:folders :paths} (set (keys request)))
                 (vector? (:folders request)) (<= (count (:folders request)) 64)
                 (vector? (:paths request)) (<= (count (:paths request)) 64))
    (refuse! ::invalid "Request at most 64 open folders and 64 files"))
  (doseq [folder (:folders request)] (when-not (= "" folder) (path! folder)))
  (doseq [path (:paths request)] (path! path))
  (with-workspace room
    (fn [conn]
      (let [db @conn repository (query/repository db)
            workspace {:repository-id (:geschichte.repo/id repository)
                       :workspace-id (:id (workspace/current-workspace))
                       :branch (get-in db [:config :branch])
                       :ref (:geschichte.repo/head repository)
                       :head (get (query/refs db) (:geschichte.repo/head repository))}
            folders (into {} (map (fn [folder] [folder (folder-from-db db folder)])) (:folders request))
            entries (into {} (map (fn [path] [path (entry db path)])) (:paths request))]
        {:workspace workspace :folders folders :entries entries
         :revision (hasch/uuid [workspace folders entries])}))))

(defn source-bytes
  "Read exactly one captured content ID, checking metadata before allocation."
  [db path selected]
  (when-not selected (refuse! ::missing "This file no longer exists"))
  (when-not (and (uuid? (:content selected)) (integer? (:size selected))
                 (<= 0 (:size selected) max-open-bytes)
                 (= 0x8000 (bit-and (long (:mode selected)) 0xF000)))
    (refuse! ::limit "Only bounded regular files can be opened"))
  (let [size (:geschichte.content/size (d/entity db [:geschichte.content/id (:content selected)]))]
    (when-not (= size (:size selected)) (refuse! ::corrupt "File metadata disagrees with content size")))
  (let [bytes (content/read-by-id (atom db) (:content selected))]
    (when-not (and bytes (= (:size selected) (alength ^bytes bytes)))
      (refuse! ::corrupt "File content does not match its selected revision"))
    bytes))

(defn search-paths [room term]
  (when-not (and (string? term) (<= 1 (count (str/trim term)) 128))
    (refuse! ::invalid "Enter a filename or path, up to 128 characters"))
  (with-workspace room
    (fn [conn]
      (let [rows (vec (take (inc max-index-entries) (d/datoms @conn :avet :geschichte.work/path)))
            needle (str/lower-case (str/trim term))]
        (when (> (count rows) max-index-entries) (refuse! ::limit "Workspace exceeds the interactive path index limit"))
        (let [matches (vec (take 101 (for [row rows :let [path (:v row)]
                                           :when (and (str/includes? (str/lower-case path) needle)
                                                      (not (str/ends-with? path "/.keep"))
                                                      (try (path! path) true (catch Exception _ false)))]
                                       {:path path :name (last (str/split path #"/")) :kind (file-kind path)})))]
          {:query term :entries (vec (take 100 matches)) :more? (> (count matches) 100)})))))

(defn analyze
  "Inert document analysis; never load/require/eval file code or widgets."
  [repository-id path text]
  (path! path)
  (if (= :markdown (file-kind path))
    (let [parsed (markdown/parse text)]
      (let [{:keys [id identity-status diagnostics]} (identity/document repository-id path parsed)
            semantic (when id (blocks/project id parsed))]
        {:kind :markdown :revision (:revision parsed)
         :frontmatter (:frontmatter parsed) :body-from (:body-from parsed) :nodes (:nodes parsed)
         :document-id id :identity-status identity-status
         :semantic semantic
         :slots (mapv (fn [node] (assoc (select-keys node [:from :to :kind]) :model (rich/model node)))
                      (:nodes parsed))
         :diagnostics (into (vec (or (:diagnostics semantic) (:diagnostics parsed))) diagnostics)}))
    {:kind :text :revision (markdown/revision text) :diagnostics []}))

(defn read-file-in
  "Project one file from an explicit immutable target DB. Internal storage
   primitive, NOT an authorization endpoint: the host must resolve and validate
   the canonical target and current authority before supplying this snapshot.
   No workspace selection, connection lookup, interpreter or write is involved."
  [db path]
  (path! path)
  (let [selected (entry db path) bytes (source-bytes db path selected)
        kind (file-kind path)
        text (when (and (not (#{:office :image} kind)) (<= (alength ^bytes bytes) markdown/max-source-bytes))
               (try (markdown/decode bytes) (catch Exception _ nil)))
        repo-id (:geschichte.repo/id (query/repository db))]
    (cond-> {:path path :entry selected :kind (if text (if (= kind :markdown) :markdown :text)
                                                  (if (= kind :text) :file kind))
             :repository-id repo-id :execution-policy :source-only}
      text (assoc :text text :analysis (analyze repo-id path text)))))

(defn read-file [room path]
  (path! path)
  (with-workspace room #(read-file-in @% path)))

(defn history
  "File changes reachable from the current Geschichte HEAD. Metadata only;
   a bounded window, not a claim to enumerate all history or follow renames."
  [room path]
  (path! path)
  (with-workspace room
    (fn [conn]
      (let [captured (atom @conn)
            commits (vec (repo/log captured {:limit 32}))
            selected-entry (fn [commit]
                             (let [tree (repo/tree-at captured commit)]
                               (when (> (count tree) max-index-entries)
                                 (refuse! ::limit "History snapshot exceeds the interactive file limit"))
                               (get tree path)))
            window (take 31 commits)
            unavailable (filter #(nil? (:geschichte.commit/snapshot %)) window)
            changed (for [commit window
                          :when (:geschichte.commit/snapshot commit)
                          :let [parents (:geschichte.commit/parents commit)
                                _ (when (> (count parents) 32) (refuse! ::limit "Too many merge parents for preview"))
                                known-parents (filter :geschichte.commit/snapshot parents)
                                boundary? (not= (count parents) (count known-parents))
                                selected (selected-entry commit)]
                          :when (or boundary? (if (seq parents)
                                                (some #(not= selected (selected-entry %)) known-parents)
                                                (some? selected)))]
                      {:commit (:geschichte.commit/id commit)
                       :boundary? boundary?
                       :parents (mapv :geschichte.commit/id parents)
                       :time (:geschichte.commit/time commit)
                       :message (:geschichte.commit/message commit)
                       :author (:geschichte.commit/author commit) :entry selected})]
        {:path path :versions (vec changed) :scanned (min 31 (count commits))
         :unavailable (mapv :geschichte.commit/id unavailable)
         :more? (> (count commits) 31) :lineage :ancestry}))))

(defn read-history [room path commit-id]
  (path! path)
  (when-not (uuid? commit-id) (refuse! ::invalid "Select a saved revision"))
  (when-not (some #(= commit-id (:commit %)) (:versions (history room path)))
    (refuse! ::conflict "Revision is outside the current history window; refresh history"))
  (with-workspace room
    (fn [conn]
      (let [captured (atom @conn) selected (get (repo/tree-at captured commit-id) path)]
        (if-not selected {:path path :commit commit-id :deleted? true}
                (do
                  (when (> (:size selected) markdown/max-source-bytes)
                    (refuse! ::limit "Historical source exceeds the preview limit"))
                  (when-not (#{:markdown :text} (file-kind path))
                    (refuse! ::invalid "Historical preview currently supports text files"))
                  {:path path :commit commit-id :text (markdown/decode (repo/read-at captured commit-id path))}))))))

(defn read-binary [room path expected]
  (path! path)
  (with-workspace room
    (fn [conn]
      (let [db @conn selected (entry db path)]
        (when-not (= expected selected) (refuse! ::conflict "File changed; reload before downloading"))
        {:path path :entry selected
         :base64 (.encodeToString (Base64/getEncoder) (source-bytes db path selected))}))))

(defn- expected-entry! [value]
  (when-not (or (nil? value)
                (and (map? value) (= #{:content :size :mode} (set (keys value)))
                     (uuid? (:content value)) (integer? (:size value)) (<= 0 (:size value) max-open-bytes)
                     (integer? (:mode value))))
    (refuse! ::invalid "A save requires the exact opened file descriptor"))
  value)

(defn- publication-boundary [room authorize!]
  (when authorize!
    (let [base (room-files/host-boundary room (effects/make-sink))]
      (fn []
        (update (base) :handlers conj
                (fn [effect next]
                  ;; Last gate before the actual commit/write, not a cached
                  ;; permission hint from opening the editor.
                  (authorize!)
                  (next effect)))))))

(defn save-file!
  "One canonical effect-checked writer with the opened revision included in
   its before snapshot. Editing source does not reload an agent interpreter."
  [room {:keys [path expected text base64] :as request} & [{:keys [authorize!]}]]
  (when-not (every? room-files/write-capabilities [:expected-worktree :applied-receipt])
    (refuse! ::unavailable "The file writer must support opened-revision preconditions"))
  (path! path)
  (when-not (and (contains? request :expected)
                 (or (= #{:path :expected :text} (set (keys request)))
                     (= #{:path :expected :base64} (set (keys request)))))
    (refuse! ::invalid "A save contains a path, opened descriptor and one payload"))
  (expected-entry! expected)
  (let [bytes (if (contains? request :text)
                (markdown/source-bytes text)
                (do (when-not (and (string? base64) (<= (count base64) (* 12 1024 1024)))
                      (refuse! ::limit "Upload exceeds the bounded file size"))
                    (try (.decode (Base64/getDecoder) ^String base64)
                         (catch IllegalArgumentException _ (refuse! ::invalid "Invalid file encoding")))))
        _ (when (> (alength ^bytes bytes) max-open-bytes) (refuse! ::limit "File is too large"))
        target (str "/" path)
        written (room-files/write! room {target {:base64 (.encodeToString (Base64/getEncoder) bytes)}}
                                   {:expected-worktree {target expected}
                                    :boundary (publication-boundary room authorize!)
                                    :message (str "Edit " path)})
        _ (when-not (:applied? written) (refuse! ::not-applied "The effect policy did not apply this file save"))
        ;; The canonical writer's in-thunk committed receipt, not a later
        ;; worktree read and not an assumed file mode.
        selected (:entry (first (:written written)))
        _ (when-not (= (blob/blob-id bytes) (:content selected))
            (refuse! ::receipt "The save receipt does not match the submitted bytes"))]
    {:path path :entry selected :commit (:commit written) :saved? true}))

(defn move-file! [room path target expected & [{:keys [authorize!]}]]
  (when-not (every? room-files/write-capabilities [:expected-worktree :applied-receipt :file-modes])
    (refuse! ::unavailable "The file writer must support opened-revision preconditions"))
  (path! path) (path! target) (expected-entry! expected)
  (when (or (nil? expected) (= path target)) (refuse! ::invalid "Choose a different destination for an existing file"))
  (with-workspace room
    (fn [conn]
      (let [db @conn bytes (source-bytes db path expected)
            written (room-files/write! room {(str "/" path) nil
                                             (str "/" target) {:base64 (.encodeToString (Base64/getEncoder) bytes)}}
                                       {:expected-worktree {(str "/" path) expected (str "/" target) nil}
                                        :file-modes {(str "/" target) (:mode expected)}
                                        :boundary (publication-boundary room authorize!)
                                        :message (str "Move " path " to " target)})]
        (when-not (:applied? written) (refuse! ::not-applied "The effect policy did not apply this move"))
        {:path target :entry (:entry (first (:written written))) :commit (:commit written) :moved? true}))))
