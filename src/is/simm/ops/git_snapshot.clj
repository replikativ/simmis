(ns is.simm.ops.git-snapshot
  "Read-only, bounded Git bundles of CURRENT workspace files, not history.
   No commit, checkout, export directory, remote transport or persistent cache."
  (:refer-clojure :exclude [chunk])
  (:require [clojure.string :as str]
            [dvergr.rooms.files :as files]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.security MessageDigest]
           [java.util Arrays Base64]))

(def policy {:version 1 :max-files 512 :max-file-bytes 1048576
             :max-total-bytes 8388608 :max-bundle-bytes 9437184 :chunk-bytes 16384})

(defn- refuse! [message]
  (throw (ex-info message {:type ::invalid})))

(defn snapshot
  "Read one immutable worktree and verify every regular file's pinned content.
   Validation is fail-closed: never silently omit inaccessible/unsafe files."
  [room expected]
  (binding [ec/*execution-context* (:ctx room)]
    (let [conn (or (:conn (workspace/current-workspace))
                   (throw (ex-info "Room has no workspace" {:type ::unavailable})))
          entries (into (sorted-map) (query/worktree @conn))
          id (hasch/uuid [:simmis/git-snapshot policy (:id room) entries])]
      (when (and expected (not= expected id))
        (throw (ex-info "Workspace changed; restart the snapshot download" {:type ::conflict})))
      (when (> (count entries) (:max-files policy)) (refuse! "Workspace exceeds snapshot file limit"))
      (doseq [[path {:keys [size mode]}] entries]
        (when-not (and (string? path) (<= (count path) 1024)
                       (= (str "/" path) (try (files/workspace-path (str "/" path))
                                              (catch Exception _ nil)))
                       (contains? #{0x81A4 0x81ED} mode)
                       (integer? size) (<= 0 size (:max-file-bytes policy)))
          (refuse! "Snapshot requires bounded regular files and canonical accessible paths")))
      (when (> (reduce + 0 (map (comp :size val) entries)) (:max-total-bytes policy))
        (refuse! "Workspace exceeds snapshot byte limit"))
      {:id id
       :files (into (sorted-map)
                    (map (fn [[path {:keys [size mode] id :content}]]
                           (let [bytes (content/read-by-id conn id)]
                             (when-not (and bytes (= size (alength ^bytes bytes)))
                               (refuse! "Snapshot content does not match its pinned size"))
                             [path {:bytes bytes :mode mode}]))) entries)})))

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

(defn chunk [room expected offset length]
  (when-not (and (uuid? expected) (integer? offset) (<= 0 offset (:max-bundle-bytes policy))
                 (integer? length) (<= 1 length (:chunk-bytes policy)))
    (refuse! "Invalid snapshot chunk arguments"))
  (let [{:keys [data bytes] :as result} (bundle (snapshot room expected))
        end (min bytes (+ offset length))]
    (when (> offset bytes) (refuse! "Offset exceeds snapshot size"))
    (cond-> (assoc (dissoc result :data) :offset offset
                   :base64 (.encodeToString (Base64/getEncoder) (Arrays/copyOfRange ^bytes data offset end)))
      (< end bytes) (assoc :next-offset end))))
