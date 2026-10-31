(ns is.simm.ops.summary-inputs
  "Pure daily inventory and frozen contextual input construction. No provider,
   scheduler, cursor, database mutation or publication authority."
  (:require [datahike.blob :as blob]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.room-summaries :as summaries]
            [is.simm.model.summary-periods :as periods]
            [is.simm.model.summary-envelope :as envelope]
            [is.simm.ops.room-evidence :as evidence])
  (:import [java.time Instant]))

(def policy {:version 2 :period :day :input summaries/manifest-path :output summaries/output-path})

(defn- refuse! [reason]
  (throw (ex-info "Daily summary inputs are held" {:type ::held :reason reason})))

(def read-value envelope/read-value)

(defn declaration [source]
  (when-let [text (get source "summary-policy.edn")]
    (when-not (= policy (read-value text 1024)) (refuse! :invalid-policy))
    policy))

(def closed-day periods/closed-day)
(def day-label periods/day-label)

(defn in-period? [period row]
  (and (<= (.getTime ^java.util.Date (:from period)) (:timestamp row))
       (< (:timestamp row) (.getTime ^java.util.Date (:until period)))))

(defn inventory
  "Select ALL current public revisions in the closed period or hold visibly.
   Never trim a busy day to the first eight/64 entries and call it complete."
  [room period {:keys [source-store source-basis rows scan-held?]}]
  (when scan-held? (refuse! :source-scan-limit))
  (let [selected (vec (sort-by (juxt :timestamp (comp str :id)) (filter #(in-period? period %) rows)))]
    (when (> (count selected) summaries/max-originals) (refuse! :period-capacity))
    (when (some :held selected) (refuse! :unsupported-original))
    (let [artifacts (mapv #(evidence/artifact room source-store %) selected)
          bytes (reduce + 0 (map #(evidence/byte-count (:text %)) artifacts))]
      (when (> bytes summaries/max-input-bytes) (refuse! :period-byte-limit))
      {:room room :store source-store :source-basis source-basis :period period :rows selected :artifacts artifacts
       :coverage-id (hasch/uuid [:simmis/daily-originals-v1 room source-store period
                                (mapv #(select-keys % [:message-id :source-revision :content-id]) artifacts)])})))

(defn transcript
  "A deterministic generated document over the same inventory, independent of
   inference. Source history is explicitly current revisions at the cutoff."
  [{:keys [room store period rows coverage-id]}]
  (str "---\nsimmis-id: " (hasch/uuid [:simmis/daily-transcript-v1 room store period])
       "\nsimmis-generated: conversation-transcript\nsource-room: " room
       "\nsource-store: " store "\ncoverage-id: " coverage-id
       "\n---\n# Conversation · " (day-label period) "\n\n"
       "Current public message revisions at collection; not a complete edit/deletion history.\n\n"
       (apply str
              (map (fn [{:keys [id author timestamp text]}]
                     (let [fence (apply str (repeat (max 3 (inc (reduce max 0 (map count (re-seq #"`+" text))))) "`"))]
                       (str "## " (Instant/ofEpochMilli timestamp) "\n\n"
                            "Author: " author " · Message: " id "\n\n"
                            fence "text\n" text "\n" fence "\n\n"))) rows))))

(defn freeze
  "Build the v2 manifest file and exact input bytes. Context entries have
   :source metadata plus :text; their native ownership is the caller's duty."
  [{:keys [room store period artifacts] :as inventory} cutoff context]
  (let [rows (zipmap (map :id (:rows inventory)) (:rows inventory))
        originals (mapv (fn [a]
                          {:source-id (:message-id a) :source-revision (:source-revision a)
                           :path (:path a) :file-content (blob/blob-id (markdown/source-bytes (:text a)))
                           :kind :conversation
                           :authority (let [row (get rows (:message-id a))]
                                        (if (= ":tool-result" (get-in row [:metadata ":message/role"]))
                                          :tool
                                          (case (:actor-kind row) :human :human :agent :assistant :unknown)))}) artifacts)
        selected (summaries/selection {:room room :store store :period period :cutoff cutoff
                                       :inventory :current-revisions :complete? true
                                       :coverage originals :context (mapv :source context)})
        entries (into (sorted-map summaries/manifest-path (pr-str selected))
                      (concat (map (fn [a] [(str "/" (:path a)) (:text a)]) artifacts)
                              (map (fn [{:keys [source text]}] [(str "/" (:path source)) text]) context)))]
    (envelope/validate! selected entries)
    {:selection selected :inputs entries :coverage-id (:coverage-id inventory)}))

(defn validate-inputs!
  "Validate only complete frozen bytes; never resolve live sources here."
  [source entries]
  (when (declaration source)
    (let [selected (summaries/validate-selection! (read-value (get entries summaries/manifest-path) 65536))]
      (envelope/validate! selected (envelope/extract selected entries)))))
