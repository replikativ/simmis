(ns is.simm.ops.worker-files
  "Explicit frozen inputs and bounded regular-file worker output. No archive,
   remote URL, host path, symlink or arbitrary room read/write operation."
  (:require [clojure.string :as str]
            [dvergr.rooms.files :as room-files]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.git.object :as object]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [jsonista.core :as json]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.nio.charset StandardCharsets]
           [com.fasterxml.jackson.databind SerializationFeature]
           [java.security MessageDigest]
           [java.util Arrays Base64]))

(def policy {:version 2 :max-files 100 :max-file-bytes 1048576 :max-total-bytes 2097152
             :max-input-json-bytes 4194304 :chunk-bytes 16384 :lease-ttl-ms 30000})
(def config-schema
  [:map {:closed true}
   [:adapter [:enum "codex" "claude-code"]]
   [:version [:string {:min 1 :max 200}]]
   [:model [:string {:min 1 :max 200}]]
   [:policy [:= "local-trusted-v1"]]])
(def limits-schema
  [:map {:closed true} [:timeout-ms [:int {:min 1000 :max 600000}]]
   [:settlement [:enum "review" "discard"]]])
(def file-schema
  [:map {:closed true} [:base64 [:string {:max 1398104}]]
   [:bytes [:int {:min 0 :max 1048576}]] [:sha256 [:re #"^[0-9a-f]{64}$"]]
   [:mode [:enum 0x81A4 0x81ED]]])
(def output-schema
  [:map {:closed true}
   [:files [:map-of {:max 100} [:string {:min 1 :max 512}] [:maybe file-schema]]]
   [:summary [:string {:max 16000}]]
   [:exit-code [:int {:min 0 :max 255}]]])
(def transport-output-schema
  ;; Dvergr's HTTP decoder keywordizes JSON object keys recursively. Only
  ;; filename keys differ from the internal format; descriptor keys stay closed.
  [:map {:closed true}
   [:files [:map-of {:max 100} [:or [:string {:min 1 :max 512}] :keyword] [:maybe file-schema]]]
   [:summary [:string {:max 16000}]]
   [:exit-code [:int {:min 0 :max 255}]]])
(def ^:private mapper
  (doto (json/object-mapper {:encode-key-fn name})
    (.configure SerializationFeature/ORDER_MAP_ENTRIES_BY_KEYS true)))

(defn- refuse! [message] (throw (ex-info message {:type ::invalid})))
(defn transport-output [output]
  (when-not (m/validate transport-output-schema output) (refuse! "Invalid bounded worker output"))
  (update output :files
          (fn [files]
            (reduce-kv (fn [out key value]
                         (let [path (if (keyword? key) (subs (str key) 1) key)]
                           (when (contains? out path) (refuse! "Duplicate worker output filename"))
                           (assoc out path value))) {} files))))
(defn sha256 [^bytes bytes]
  (object/bytes->hex (.digest (MessageDigest/getInstance "SHA-256") bytes)))

(defn file-bytes!
  "Decode one bounded immutable regular-file descriptor, verifying its bytes.
   Used both at worker intake and when reading captured Attempt evidence."
  [file]
  (when-not (m/validate file-schema file) (refuse! "Invalid bounded file descriptor"))
  (let [bytes (try (.decode (Base64/getDecoder) ^String (:base64 file))
                   (catch IllegalArgumentException _ (refuse! "File base64 is invalid")))
        size (alength bytes)]
    (when-not (and (= size (:bytes file)) (= (:sha256 file) (sha256 bytes))
                   (<= size (:max-file-bytes policy)))
      (refuse! "File does not match its byte limit, size or digest"))
    bytes))

(defn path!
  "Canonical relative file names only. Sensitive paths use the same policy as
   ordinary room file access; control characters are never local filenames."
  [path]
  (when-not (and (string? path) (<= 1 (count path) 512)
                 (not (str/starts-with? path "/"))
                 (not (re-find #"[\p{Cntrl}]" path))
                 (= (str "/" path) (try (room-files/workspace-path path) (catch Exception _ nil))))
    (refuse! "Worker paths must be canonical accessible relative filenames"))
  path)

(declare freeze-files)

(defn freeze
  "Freeze explicitly selected files from ONE immutable manifest, never a
   recursive directory or the author's entire room. Content IDs are verified
   by Geschichte; the payload is portable base64, not a live store handle."
  [room task config paths limits]
  (when-not (and (string? task) (<= 1 (count task) 16000)
                 (m/validate config-schema config) (m/validate limits-schema limits)
                 (vector? paths) (<= (count paths) (:max-files policy))
                 (= (count paths) (count (set paths))))
    (refuse! "Invalid worker task inputs"))
  (doseq [path paths] (path! path))
  (binding [ec/*execution-context* (:ctx room)]
    (let [conn (or (:conn (workspace/current-workspace))
                   (throw (ex-info "Room has no workspace" {:type ::unavailable})))
          tree (query/worktree @conn)
          selected (into (sorted-map) (map (fn [p] [p (get tree p)]) paths))]
      (doseq [[_ {:keys [size mode]}] selected]
        (when-not (and (integer? size) (<= 0 size (:max-file-bytes policy))
                       (contains? #{0x81A4 0x81ED} mode))
          (refuse! "Selected worker inputs must exist and be bounded regular files")))
      (when (> (reduce + 0 (map (comp :size val) selected)) (:max-total-bytes policy))
        (refuse! "Selected inputs exceed total byte limit"))
      (let [files (into (sorted-map)
                        (map (fn [[path {:keys [size mode] id :content}]]
                               (let [^bytes bytes (content/read-by-id conn id)]
                                 (when-not (and bytes (= size (alength bytes)))
                                   (refuse! "Input content does not match pinned size"))
                                 [path {:bytes size :mode mode :sha256 (sha256 bytes)
                                        :base64 (.encodeToString (Base64/getEncoder) bytes)}]))) selected)]
        (freeze-files task config files limits)))))

(defn input-chunk [frozen offset length]
  (when-not (= (:input-id frozen) (hasch/uuid [:simmis/worker-input-v1 (dissoc frozen :input-id)]))
    (refuse! "Frozen input does not match its content identity"))
  (when-not (and (integer? offset) (<= 0 offset (:max-input-json-bytes policy))
                 (integer? length) (<= 1 length (:chunk-bytes policy)))
    (refuse! "Invalid worker input chunk"))
  (let [bytes (.getBytes (json/write-value-as-string frozen mapper) StandardCharsets/UTF_8)
        size (alength bytes) end (min size (+ offset length))]
    (when (or (> size (:max-input-json-bytes policy)) (> offset size))
      (refuse! "Input encoding or offset exceeds its byte limit"))
    (cond-> {:input-id (:input-id frozen) :bytes size :sha256 (sha256 bytes) :offset offset
             :base64 (.encodeToString (Base64/getEncoder) (Arrays/copyOfRange bytes offset end))}
      (< end size) (assoc :next-offset end))))

(declare validate-output!)

(defn validate-frozen!
  "Validate an executor's portable input BEFORE creating a guest workspace.
   This does not authorize a server claim or current room access. The owning
   controller must already have verified its assignment, configuration and lease."
  [frozen]
  (when-not (and (map? frozen)
                 (= #{:input-id :task :config :files :limits :policy} (set (keys frozen)))
                 (uuid? (:input-id frozen)) (string? (:task frozen)) (<= 1 (count (:task frozen)) 16000)
                 (m/validate config-schema (:config frozen)) (m/validate limits-schema (:limits frozen))
                 (= policy (:policy frozen)))
    (refuse! "Invalid frozen worker inputs"))
  (validate-output! frozen {:files (:files frozen) :summary "" :exit-code 0})
  (when (some nil? (vals (:files frozen))) (refuse! "Frozen input files must exist"))
  (when-not (= (:input-id frozen) (hasch/uuid [:simmis/worker-input-v1 (dissoc frozen :input-id)]))
    (refuse! "Frozen worker input identity differs"))
  frozen)

(defn validate-output!
  "Check all output before publication. Only selected inputs may be edited or
   deleted; additional deliverables must live under out/. No other room file
   is writable by this worker, even in the isolated result world."
  [frozen output]
  (when-not (m/validate output-schema output) (refuse! "Invalid bounded worker output"))
  (reduce-kv
   (fn [total path file]
     (path! path)
     (when-not (or (contains? (:files frozen) path) (and file (str/starts-with? path "out/")))
       (refuse! "Worker output is outside selected input files and out/ deliverables"))
     (if-not file total
             (let [bytes (file-bytes! file)
                   size (alength bytes) total (+ total size)]
               (when-not (<= total (:max-total-bytes policy))
                 (refuse! "Worker output exceeds limits or does not match its size/digest"))
               total))) 0 (:files output))
  ;; Refuse file/directory collisions before any later import operation.
  (let [written (into (set (keys (:files frozen)))
                      (keep (fn [[path file]] (when file path))) (:files output))
        written (reduce disj written (keep (fn [[path file]] (when-not file path)) (:files output)))]
    (doseq [path written parent (butlast (reductions #(str %1 "/" %2) (str/split path #"/")))]
      (when (contains? written parent) (refuse! "Worker output contains file/directory collisions"))))
  output)

(defn freeze-files
  "Construct the SAME portable input format from already selected immutable
   descriptors. No room read, provider resolution or execution. Workflow and
   benchmark adapters must select inputs BEFORE calling this; never pass gold
   or checker material from an authoring room implicitly."
  [task config files limits]
  (when-not (and (string? task) (<= 1 (count task) 16000)
                 (m/validate config-schema config) (m/validate limits-schema limits)
                 (m/validate output-schema {:files files :summary "" :exit-code 0})
                 (not-any? nil? (vals files)))
    (refuse! "Invalid portable worker inputs"))
  ;; Verify actual bytes and aggregate bounds before hashing the input; a
  ;; forged small :bytes field must not admit a large encoded allocation.
  (validate-output! {:files files} {:files files :summary "" :exit-code 0})
  (let [frozen {:task task :config config :files (into (sorted-map) files) :limits limits :policy policy}]
    (validate-frozen! (assoc frozen :input-id (hasch/uuid [:simmis/worker-input-v1 frozen])))))
