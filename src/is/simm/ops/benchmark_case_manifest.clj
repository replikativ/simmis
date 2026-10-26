(ns is.simm.ops.benchmark-case-manifest
  "HOST metadata inventory of the whole private candidate filesystem. This
   deliberately includes code and empty directories, without fetching bodies.
   Identities describe visible files, not authority or complete Git history."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as fixtures]
            [dvergr.rooms.files :as paths]
            [hasch.core :as hasch]))

(def policy
  {:version 1 :max-files 1024 :max-directories 4096
   :max-file-bytes 8388608 :max-bytes 67108864})

(defn- refuse! [reason]
  (throw (ex-info "Benchmark filesystem manifest refused"
                  {:type ::invalid :reason reason})))

(defn- path! [path root?]
  (when-not (and (string? path) (<= 1 (count path) 512)
                 (str/starts-with? path "/")
                 (or root? (not= "/" path))
                 (= path (try (paths/workspace-path path root?) (catch Exception _ nil))))
    (refuse! :noncanonical-path))
  path)

(defn- add-directory [directories path]
  (let [result (conj directories path)]
    (when (> (count result) (:max-directories policy))
      (refuse! :directory-limit))
    result))

(defn- add-parents [directories path]
  (loop [directories directories path path]
    (let [index (.lastIndexOf ^String path "/")]
      (if (pos? index)
        (let [parent (subs path 0 index)]
          (recur (add-directory directories parent) parent))
        (add-directory directories "/")))))

(defn- file! [entry]
  (when-not (and (map? entry) (= #{:content :size :mode} (set (keys entry)))
                 (uuid? (:content entry))
                 (contains? #{33188 33261} (:mode entry))
                 (integer? (:size entry)) (<= 0 (:size entry) (:max-file-bytes policy)))
    (refuse! :invalid-file)))

(defn- normalize! [files directories]
  (when-not (and (map? files) (<= (count files) (:max-files policy))
                 (coll? directories) (<= (count directories) (:max-directories policy)))
    (refuse! :entry-limit))
  (reduce-kv
   (fn [total path entry]
     (path! path false)
     (file! entry)
     (let [total (+ total (:size entry))]
       (when (> total (:max-bytes policy)) (refuse! :byte-limit))
       total)) 0 files)
  (let [directories (reduce (fn [result path]
                              (path! path true)
                              (add-parents (add-directory result path) path))
                            (sorted-set "/") directories)
        directories (reduce add-parents directories (keys files))]
    (when (some #(contains? files %) directories) (refuse! :file-directory-collision))
    {:version 1 :files (into (sorted-map) files) :directories (vec directories)}))

(defn- checked! [manifest]
  (when-not (and (map? manifest) (= #{:version :files :directories} (set (keys manifest)))
                 (= 1 (:version manifest)))
    (refuse! :manifest-shape))
  (let [normalized (normalize! (:files manifest) (:directories manifest))]
    (when-not (= manifest normalized) (refuse! :noncanonical-manifest))
    normalized))

(defn- rows! [db attribute maximum]
  ;; Bound index acquisition before any entity lookup. An inner join would
  ;; silently omit malformed path rows and could hide unexpected files.
  (let [rows (vec (take (inc maximum) (d/datoms db :avet attribute)))]
    (when (> (count rows) maximum) (refuse! :entry-limit))
    rows))

(defn inventory
  "Capture complete visible primary-repository metadata from one immutable DB.
   Does not require its live branch configuration to equal a stored commit's
   configuration: the coordinator verifies checkpoint provenance separately."
  [db]
  (let [files (reduce
               (fn [files row]
                 (let [relative (:v row)
                       _ (when-not (and (string? relative) (<= 1 (count relative) 511))
                           (refuse! :invalid-file-path))
                       path (path! (str "/" relative) false)
                       entity (d/entity db (:e row))
                       entry {:content (:geschichte.content/id (:geschichte.work/content entity))
                              :size (:geschichte.work/size entity)
                              :mode (:geschichte.work/mode entity)}]
                   (when-not (and (string? relative) (= relative (:geschichte.work/path entity))
                                  (not (contains? files path)))
                     (refuse! :invalid-file-path))
                   (file! entry)
                   (when-not (= (:size entry)
                                (:geschichte.content/size
                                 (d/entity db [:geschichte.content/id (:content entry)])))
                     (refuse! :content-size-mismatch))
                   (assoc files path entry)))
               (sorted-map) (rows! db :geschichte.work/path (:max-files policy)))
        directories (mapv (fn [row]
                            (when-not (and (string? (:v row)) (<= (count (:v row)) 511))
                              (refuse! :invalid-directory-path))
                            (str "/" (:v row)))
                          (rows! db :geschichte.work-dir/path (:max-directories policy)))]
    (normalize! files directories)))

(defn after-seeding
  "Expected whole filesystem after the existing bounded fixture seeder. Keep
   baseline modes, overwrite only selected files and create their parents."
  [baseline files limits]
  (let [baseline (checked! baseline)
        prepared (fixtures/prepare-fixture-files files limits)
        entries (reduce-kv
                 (fn [entries path entry]
                   (assoc entries path
                          (assoc (select-keys entry [:content :size])
                                 :mode (get-in baseline [:files path :mode] 33188))))
                 (:files baseline) prepared)]
    (normalize! entries (:directories baseline))))

(defn template-id [manifest]
  (hasch/uuid [:simmis/benchmark-template-v1 policy (checked! manifest)]))

(defn comparison-id [program-id template-id]
  (when-not (and (uuid? program-id) (uuid? template-id)) (refuse! :comparison-identity))
  (hasch/uuid [:simmis/benchmark-comparison-v1 program-id template-id]))
