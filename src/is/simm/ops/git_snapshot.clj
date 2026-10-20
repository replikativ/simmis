(ns is.simm.ops.git-snapshot
  "Read-only, bounded Git bundles of CURRENT workspace files, not history.
   No commit, checkout, export directory, remote transport or persistent cache."
  (:refer-clojure :exclude [chunk])
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [hasch.core :as hasch]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.security MessageDigest]
           [java.util Arrays Base64]))

(def policy {:version 1 :max-files 512 :max-file-bytes 1048576
             :max-total-bytes 8388608 :max-bundle-bytes 9437184 :chunk-bytes 16384})

(defn- refuse! [message]
  (throw (ex-info message {:type ::invalid})))

(defn- snapshot-entries! [db]
  ;; Bound index acquisition itself, not a fully realized query/worktree. An
  ;; inner-join manifest query would also omit corrupt/incomplete path rows.
  (let [rows (vec (take (inc (:max-files policy))
                        (d/datoms db :avet :geschichte.work/path)))]
    (when (> (count rows) (:max-files policy))
      (refuse! "Workspace exceeds snapshot file limit"))
    (reduce (fn [entries datom]
              (let [path (:v datom)
                    entity (d/entity db (:e datom))
                    entry {:content (:geschichte.content/id (:geschichte.work/content entity))
                           :size (:geschichte.work/size entity)
                           :mode (:geschichte.work/mode entity)}]
                (when-not (and (string? path) (<= (count path) 1024)
                               (= (str "/" path) (try (files/workspace-path (str "/" path))
                                                      (catch Exception _ nil)))
                               (= path (:geschichte.work/path entity))
                               (not (contains? entries path)))
                  (refuse! "Snapshot requires unique canonical file paths"))
                (assoc entries path entry)))
            (sorted-map) rows)))

(defn- snapshot-manifest! [db identity expected]
  (let [entries (snapshot-entries! db)
        id (hasch/uuid (conj identity entries))]
    (when (and expected (not= expected id))
      (throw (ex-info "Workspace changed; restart the snapshot download" {:type ::conflict})))
    (when (> (count entries) (:max-files policy)) (refuse! "Workspace exceeds snapshot file limit"))
    (doseq [[path {:keys [size mode content]}] entries]
      (when-not (and (string? path) (<= (count path) 1024)
                     (= (str "/" path) (try (files/workspace-path (str "/" path))
                                            (catch Exception _ nil)))
                     (contains? #{0x81A4 0x81ED} mode)
                     (uuid? content)
                     (integer? size) (<= 0 size (:max-file-bytes policy)))
        (refuse! "Snapshot requires bounded regular files and canonical accessible paths")))
    (when (> (reduce + 0 (map (comp :size val) entries)) (:max-total-bytes policy))
      (refuse! "Workspace exceeds snapshot byte limit"))
      ;; Validate ALL content metadata before allocating any file body. The
      ;; reader below sees this same immutable DB, including content entities.
    (doseq [[_ {:keys [size content]}] entries]
      (when-not (= size (:geschichte.content/size (d/entity db [:geschichte.content/id content])))
        (refuse! "Snapshot content metadata does not match its pinned size")))
    {:id id :entries entries}))

(defn- snapshot* [db identity expected]
  (let [{:keys [id entries]} (snapshot-manifest! db identity expected)
        reader (atom db)]
    {:id id
     :files (into (sorted-map)
                  (map (fn [[path {:keys [size mode] id :content}]]
                         (let [bytes (content/read-by-id reader id)]
                           (when-not (and bytes (= size (alength ^bytes bytes)))
                             (refuse! "Snapshot content does not match its pinned size"))
                           [path {:bytes bytes :mode mode}]))) entries)}))

(defn- owned-identity! [room-id target-stamp expected]
  (when-not (and (uuid? room-id) (uuid? target-stamp)
                 (or (nil? expected) (uuid? expected)))
    (refuse! "Invalid owned snapshot identity"))
  [:simmis/git-snapshot-owned-v1 policy room-id target-stamp])

(defn snapshot-id-in
  "HOST pure metadata projection: validate the same bounded immutable manifest
   as snapshot-in and return ONLY its identity, without reading file bodies.
   Neither the DB nor its room/stamp inputs grant read authority."
  [db room-id target-stamp]
  (:id (snapshot-manifest! db (owned-identity! room-id target-stamp nil) nil)))

(defn snapshot-in
  "HOST projection of one immutable DB. Identity binds an authenticated host's
   canonical room UUID and opaque target stamp; neither input grants access.
   No selected lookup, retained read lease, or portable catalog identity."
  [db room-id target-stamp expected]
  (snapshot* db (owned-identity! room-id target-stamp expected) expected))

(defn snapshot
  "Selected workspace compatibility wrapper. Preserve the existing v1 export
   identity while pinning manifest AND content metadata to one immutable DB."
  [room expected]
  (binding [ec/*execution-context* (:ctx room)]
    (let [conn (or (:conn (workspace/current-workspace))
                   (throw (ex-info "Room has no workspace" {:type ::unavailable})))]
      (snapshot* @conn [:simmis/git-snapshot policy (:id room)] expected))))

(defn bundle
  "A deterministic root-commit bundle assembled with Geschichte's Git
   object/pack implementation. Deliberately no ancestry projection."
  [{:keys [id files]}]
  (let [objects (atom {})
        add! (fn [type payload]
               (let [oid (object/object-id type payload)]
                 (swap! objects assoc oid {:type type :payload payload}) oid))
        trie (reduce-kv (fn [tree path file] (assoc-in tree (str/split path #"/") file)) {} files)
        tree! (fn tree! [node]
                (when (contains? node :bytes) (refuse! "Workspace file/directory paths conflict"))
                (add! :tree
                      (object/tree-payload
                       (mapv (fn [[name child]]
                               (if (contains? child :bytes)
                                 (do
                                   (when-not (= #{:bytes :mode} (set (keys child)))
                                     (refuse! "Workspace file/directory paths conflict"))
                                   {:name name :mode (Integer/toOctalString (int (:mode child)))
                                    :oid (add! :blob (:bytes child))})
                                 {:name name :mode "40000" :oid (tree! child)})) node))))
        tree (tree! trie)
        commit (add! :commit (object/commit-payload
                              {:tree tree :parents []
                               :author "Simmis Snapshot <snapshot@simm.is> 0 +0000"
                               :committer "Simmis Snapshot <snapshot@simm.is> 0 +0000"
                               :message (str "Simmis workspace snapshot " id "\nNo repository history included.\n")}))
        bytes (object/concat-bytes (object/utf8 (str "# v2 git bundle\n" commit " refs/heads/simmis-snapshot\n\n"))
                                   (pack/encode @objects))]
    (when (> (alength bytes) (:max-bundle-bytes policy)) (refuse! "Encoded bundle exceeds byte limit"))
    {:snapshot id :commit commit :files (count files) :history false :bytes (alength bytes)
     :sha256 (object/bytes->hex (.digest (MessageDigest/getInstance "SHA-256") bytes))
     :data bytes}))

(defn describe [room]
  (dissoc (bundle (snapshot room nil)) :data))

(defn describe-in [db room-id target-stamp]
  (dissoc (bundle (snapshot-in db room-id target-stamp nil)) :data))

(defn- chunk-arguments! [expected offset length]
  (when-not (and (uuid? expected) (integer? offset) (<= 0 offset (:max-bundle-bytes policy))
                 (integer? length) (<= 1 length (:chunk-bytes policy)))
    (refuse! "Invalid snapshot chunk arguments")))

(defn- chunk-result [snapshot offset length]
  (let [{:keys [data bytes] :as result} (bundle snapshot)
        end (min bytes (+ offset length))]
    (when (> offset bytes) (refuse! "Offset exceeds snapshot size"))
    (cond-> (assoc (dissoc result :data) :offset offset
                   :base64 (.encodeToString (Base64/getEncoder) (Arrays/copyOfRange ^bytes data offset end)))
      (< end bytes) (assoc :next-offset end))))

(defn chunk-in [db room-id target-stamp expected offset length]
  (chunk-arguments! expected offset length)
  (chunk-result (snapshot-in db room-id target-stamp expected) offset length))

(defn chunk [room expected offset length]
  (chunk-arguments! expected offset length)
  (chunk-result (snapshot room expected) offset length))
