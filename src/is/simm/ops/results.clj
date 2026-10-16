(ns is.simm.ops.results
  "Bounded views of files already captured in a certified Dvergr Attempt.
   Never reads the current workspace and never creates a second result store."
  (:require [datahike.blob :as blob]
            [dvergr.mcp.server :as mcp-server]
            [dvergr.ops :as dops]
            [is.simm.ops.dvergr :as ops]
            [jsonista.core :as json]
            [malli.core :as m])
  (:import [java.nio.charset StandardCharsets]
           [java.util Arrays Base64]))

(def max-chunk-bytes 16384)
(def max-files 100)

(def ^:private attempt-args
  [[:room [:string {:min 1 :max 200}]]
   [:id [:string {:min 1 :max 36}]]])

(def files-schema
  (into [:map {:closed true}] (conj attempt-args
                                    [:offset {:optional true} [:int {:min 0 :max Long/MAX_VALUE}]])))

(def file-schema
  (into [:map {:closed true}] (into attempt-args
                                    [[:path [:string {:min 1 :max 2048}]]
                                     [:offset {:optional true} [:int {:min 0 :max Long/MAX_VALUE}]]
                                     [:length {:optional true} [:int {:min 1 :max max-chunk-bytes}]]])))

(defn- checked! [schema args]
  (when-not (and (m/validate schema args) (parse-uuid (:id args)))
    (dops/op-error! "Invalid recorded-result arguments" ::invalid 400)))

(defn- attempt! [ctx principal {:keys [room id]}]
  (or (ops/invoke ctx principal :attempt/detail {:room room :id id})
      (dops/op-error! "No such Attempt in this room" ::not-found 404)))

(defn- text-bytes [text]
  (when-not (string? text)
    (dops/op-error! "This captured value is not a text-file artifact" ::unsupported 422))
  (.getBytes ^String text StandardCharsets/UTF_8))

(defn- identity-fields [attempt]
  (select-keys attempt [:id :content-id :run-id]))

(defn attempt-files
  "List recorded file paths, paged, without returning their contents."
  [ctx principal {:keys [offset] :or {offset 0} :as args}]
  (checked! files-schema args)
  (let [attempt (attempt! ctx principal args)
        files (get-in attempt [:evidence :files] {})
        paths (vec (sort (keys files)))
        page (reduce (fn [{:keys [entries chars] :as page} path]
                       (when-not (and (string? path) (<= 1 (count path) 2048))
                         (dops/op-error! "Captured file path is outside the supported bounds"
                                         ::unsupported 422))
                       (let [bytes (text-bytes (get files path))
                             entry {:path path :size (alength ^bytes bytes)
                                    :content-id (str (blob/blob-id bytes))}
                             n (count (json/write-value-as-string entry))]
                         (if (> (+ chars n) 24000)
                           (reduced page)
                           {:entries (conj entries entry) :chars (+ chars n)})))
                     {:entries [] :chars 0} (take max-files (drop offset paths)))
        next-offset (+ offset (count (:entries page)))]
    (cond-> {:attempt (identity-fields attempt)
             :total (count paths) :offset offset
             :files (:entries page)}
      (< next-offset (count paths)) (assoc :next-offset next-offset))))

(defn attempt-file
  "Read exact captured UTF-8 bytes as a bounded base64 chunk. Byte offsets,
   not character offsets; concatenate decoded bytes before decoding UTF-8."
  [ctx principal {:keys [path offset length] :or {offset 0 length max-chunk-bytes} :as args}]
  (checked! file-schema args)
  (let [attempt (attempt! ctx principal args)
        files (get-in attempt [:evidence :files] {})]
    (when-not (contains? files path)
      (dops/op-error! "No such file in this Attempt's captured evidence" ::not-found 404))
    (let [^bytes bytes (text-bytes (get files path))
          size (alength bytes)]
      (when (> offset size)
        (dops/op-error! "Offset is outside the captured file" ::invalid 400))
      (let [end (+ offset (min length (- size offset)))]
        (cond-> {:attempt (identity-fields attempt) :path path
                 :content-id (str (blob/blob-id bytes))
                 :encoding "base64" :media-type "text/plain; charset=utf-8"
                 :size size :offset offset :length (- end offset)
                 :base64 (.encodeToString (Base64/getEncoder)
                                          (Arrays/copyOfRange bytes (int offset) (int end)))}
          (< end size) (assoc :next-offset end))))))

(def specifications
  {:simmis/attempt-files
   {:doc "List immutable output files captured by one certified Attempt, not today's workspace. Returns at most 100 paths; offset pages."
    :kind :read :schema files-schema
    :impl (fn [host args] (attempt-files (:execution-ctx host) dops/*principal* args))}
   :simmis/attempt-file
   {:doc "Read a captured Attempt file as at most 16384 bytes of base64. Follow next-offset; concatenate decoded bytes before UTF-8 decoding. Immutable even after workspace changes/world discard."
    :kind :read :schema file-schema
    :impl (fn [host args] (attempt-file (:execution-ctx host) dops/*principal* args))}})

(defn install! []
  (doseq [[op spec] specifications] (dops/register-op! op spec))
  (mcp-server/sync-op-tools!)
  nil)

(defn uninstall! []
  (doseq [op (keys specifications)] (dops/unregister-op! op))
  (mcp-server/sync-op-tools!)
  nil)
