(ns is.simm.ops.benchmark-source
  "Bounded immutable HOST source for benchmark preparation. Retaining a
   bundle is not permission to execute its checker or expose its gold to Runs."
  (:require [clojure.string :as str]
            [datahike.blob :as blob]
            [dvergr.agent.experiment :as experiment]
            [dvergr.catalog.room :as catalog]
            [dvergr.rooms.files :as paths]
            [hasch.core :as hasch]
            [is.simm.model.benchmark-resolutions :as resolutions]
            [is.simm.ops.workflows :as workflows])
  (:import [java.nio CharBuffer]
           [java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets]))

(defn- refuse! [reason]
  (throw (ex-info "Retained benchmark source is invalid" {:type ::invalid :reason reason})))

(defn- utf8-size! [text maximum]
  (when-not (and (string? text) (<= (count text) maximum)) (refuse! :source-size))
  (try
    (let [encoder (doto (.newEncoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT)
                    (.onUnmappableCharacter CodingErrorAction/REPORT))
          size (.remaining (.encode encoder (CharBuffer/wrap ^String text)))]
      (when (> size maximum) (refuse! :source-size))
      size)
    (catch CharacterCodingException _ (refuse! :invalid-utf8))))

(defn- files! [files]
  (when-not (and (map? files) (<= 1 (count files) workflows/max-files))
    (refuse! :source-count))
  (reduce-kv
              (fn [total path text]
                (when-not (and (string? path) (<= 1 (count path) 512)
                               (not (str/starts-with? path "/"))
                               (= (str "/" path) (paths/workspace-path path)))
                  (refuse! :source-path))
                (utf8-size! path 2048)
                (let [total (+ total (utf8-size! text workflows/max-file-bytes))]
                  (when (> total workflows/max-bundle-bytes) (refuse! :source-size))
                  total)) 0 files)
  (into (sorted-map) files))

(defn- bundle! [name files]
  (when-not (and (string? name) (re-matches #"[a-z0-9][a-z0-9-]{0,63}" name))
    (refuse! :source-name))
  (catalog/bundle name (files! files)))

(defn retain
  "Return inert exact source suitable for the Request's existing store-ref
   payload. This performs no filesystem IO, checker evaluation or allocation."
  [name files]
  (let [bundle (bundle! name files)]
    {:version 1 :name name :bundle (:id bundle) :files (:files bundle)}))

(defn reconstruct
  "Rebuild only from retained bytes. A self-consistent outer artifact hash is
   insufficient: validate bounds and reconstruct the catalog content identity."
  [source]
  (when-not (and (map? source) (= #{:version :name :bundle :files} (set (keys source)))
                 (= 1 (:version source)) (uuid? (:bundle source)))
    (refuse! :source-shape))
  (let [bundle (bundle! (:name source) (:files source))]
    (when-not (= (:id bundle) (:bundle source)) (refuse! :source-identity))
    bundle))

(defn source-id [source]
  (reconstruct source)
  (hasch/uuid [:simmis/benchmark-source-v1 source]))

(defn validate-manifest!
  "Verify retained source bytes against the original owned file manifest.
   Geschichte logical content IDs use the public blob-id of complete bytes,
   independently of chunk/delta storage. Modes remain validated metadata."
  [source manifest]
  (reconstruct source)
  (when-not (and (vector? manifest)
                 (= (count (:files source)) (count manifest))
                 (= (vec (sort (keys (:files source)))) (mapv :path manifest)))
    (refuse! :source-manifest))
  (doseq [{:keys [path content size mode] :as entry} manifest]
    (let [bytes (.getBytes ^String (get-in source [:files path]) StandardCharsets/UTF_8)]
      (when-not (and (= #{:path :content :size :mode} (set (keys entry)))
                     (= (blob/blob-id bytes) content) (integer? size) (= (alength bytes) size)
                     (integer? mode) (<= 0 mode Integer/MAX_VALUE)
                     (= 0x8000 (bit-and mode 0xF000)))
        (refuse! :source-manifest))))
  manifest)

(defn case-files
  "Candidate-visible fixture bytes only: shared plus the selected case.
   Checker/gold/calibration/source-pack bytes stay in the HOST bundle. Names
   are explicitly tagged, so a case named default never means the default."
  [source case]
  (let [bundle (reconstruct source)
        known (:cases bundle)
        selected (cond
                   (= case {:kind :default})
                   (do (when (seq known) (refuse! :unknown-case)) nil)
                   (and (map? case) (= #{:kind :name} (set (keys case)))
                        (= :named (:kind case)) (string? (:name case))
                        (contains? known (:name case)))
                   (get known (:name case))
                   :else (refuse! :unknown-case))]
    (into (sorted-map) (merge (:fixtures bundle) (:fixtures selected)))))

(defn compile-plan
  "Compile the original pure benchmark basis into inert preparation identity.
   Remove only the Request nonce and its derived Experiment name/content hash;
   preserve every candidate, dataset, runtime, split, policy and limit field.
   This stable grouping identity never permits result reuse or dispatch."
  [source basis]
  (reconstruct source)
  (when-not (and (map? basis) (= #{:submission :experiment :team :limits :policy} (set (keys basis)))
                 (map? (:submission basis)) (uuid? (get-in basis [:submission :request-id]))
                 (= (keyword "room-workflow" (str "request-" (get-in basis [:submission :request-id])))
                    (get-in basis [:experiment :experiment/id]))
                 (= (:name source) (get-in basis [:submission :name]))
                 (= (:bundle source) (get-in basis [:submission :expected-bundle])))
    (refuse! :benchmark-basis))
  (experiment/validate-experiment (:experiment basis))
  (let [source-id (source-id source)
        tagged-cases (mapv (fn [environment]
                            (let [metadata (:environment/metadata environment)]
                              (if (contains? metadata :case)
                                {:kind :named :name (:case metadata)}
                                {:kind :default})))
                          (get-in basis [:experiment :experiment/dataset :dataset/environments]))
        _ (when-not (<= 1 (count tagged-cases) 8) (refuse! :benchmark-cases))
        cases (mapv (fn [ordinal case]
                      {:ordinal ordinal :case case
                       :manifest-id (hasch/uuid [:simmis/benchmark-case-fixtures-v1
                                                (case-files source case)])})
                    (range) tagged-cases)
        semantic {:version 1 :source-id source-id
                  :substitution :clean-selected-fixture-checkpoint-v1 :cases cases
                  :benchmark (-> basis
                                 (update :submission dissoc :request-id)
                                 (update :experiment dissoc :experiment/id :experiment/content-id))}
        plan {:version 1 :source-id source-id
              :semantic-id (hasch/uuid [:simmis/benchmark-semantic-v1 semantic])
              :substitution :clean-selected-fixture-checkpoint-v1 :cases cases}]
    (resolutions/validate-plan! plan)
    {:source source :semantic semantic :plan plan}))
