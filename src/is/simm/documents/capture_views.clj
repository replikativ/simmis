(ns is.simm.documents.capture-views
  "Read-only archive tables over existing grouped observation notes. No personal
   capture service, media body reads, transcription, publication or page projection."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.query :as query]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.media :as media]
            [is.simm.documents.saved-view-contract :as contract]
            [is.simm.documents.saved-view-spec :as spec])
  (:import [java.time Instant]))

(def max-index-entries 256)
(def max-notes 128)
(def max-source-bytes (* 8 1024 1024))

(defn- refuse! [reason]
  (throw (ex-info "Capture archive view is unavailable" {:type ::refused :reason reason})))

(defn- regular? [entry]
  (and (map? entry) (uuid? (:content entry)) (integer? (:size entry))
       (<= 0 (:size entry) Long/MAX_VALUE) (= 33188 (:mode entry))))

(defn- selection [db roots]
  (let [paths
        (mapcat (fn [root]
                  (let [upper (str root "0")
                        rows (vec (take (inc max-index-entries)
                                        (d/index-range db {:attrid :geschichte.work/path
                                                           :start (str root "/") :end upper})))]
                    ;; Bound the raw native index before filtering files.
                    (when (> (count rows) max-index-entries) (refuse! :archive-index-limit))
                    (for [row rows :let [path (:v row)]
                          :when (and (not= path upper) (= :markdown (files/file-kind path)))]
                      {:path path :root root :entry (files/entry db path)}))) roots)
        selected (vec paths)]
    (when (> (count selected) max-notes) (refuse! :archive-note-limit))
    (doseq [{:keys [path entry]} selected]
      (files/path! path)
      (when-not (and (regular? entry) (<= (:size entry) (min files/max-open-bytes markdown/max-source-bytes)))
        (refuse! :invalid-note-metadata)))
    ;; Validate the whole selected byte budget before the first body read.
    (when (> (reduce + 0 (map (comp :size :entry) selected)) max-source-bytes)
      (refuse! :archive-byte-limit))
    selected))

(defn- parse-id [value]
  (when (and (string? value) (= 36 (count value))) (parse-uuid value)))

(defn- instant [value]
  (when-not (and (string? value) (<= 1 (count value) 40)) (refuse! :invalid-note))
  (try
    (let [time (Instant/parse value) millis (.toEpochMilli time)]
      (when (neg? millis) (refuse! :invalid-note))
      {:text (str time) :millis millis})
    (catch Exception _ (refuse! :invalid-note))))

(defn- time? [value] (and (integer? value) (<= 0 value Long/MAX_VALUE)))

(defn- asset [db session-root kind value]
  (let [path (get value "path") content (parse-id (get value "content")) size (get value "size")
        prefix (str session-root (if (= :video kind) "/recordings/" "/images/"))]
    (when-not (and (map? value) (string? path) (<= (count path) 512)
                   content (time? size) (str/starts-with? path prefix)
                   (not (str/includes? (subs path (count prefix)) "/"))
                   (= kind (media/kind path)))
      (refuse! :invalid-media-reference))
    (files/path! path)
    (let [current (files/entry db path)]
      {:path path :content content :size size
       :available? (and (regular? current) (= content (:content current)) (= size (:size current)))})))

(defn- note [db {:keys [root path entry]}]
  (let [parsed (markdown/parse (markdown/decode (files/source-bytes db path entry)))
        properties (get-in parsed [:frontmatter :properties])
        id (parse-id (get properties "simmis-id"))
        session (parse-id (get properties "capture-session"))
        owner (parse-id (get properties "capture-owner"))
        session-root (subs root 0 (- (count root) (count "/observations")))
        filename (subs path (inc (count root)))
        start (instant (get properties "started-at")) until (instant (get properties "until"))
        screenshots (get properties "screenshots") recording (get properties "recording")]
    (when-not (and (empty? (get-in parsed [:frontmatter :diagnostics]))
                   (= "screen-observation" (get properties "source-kind"))
                   (= "server-received" (get properties "timestamp-basis"))
                   (= "estimated" (get properties "recording-alignment"))
                   id owner session (str/ends-with? session-root (str "--" session))
                   (re-matches #"[0-9]{2}-[0-9]{2}-[0-9]{2}--[0-9]{6}\.md" filename)
                   (<= (:millis start) (:millis until))
                   (sequential? screenshots) (<= (bounded-count 33 screenshots) 32)
                   (= (count screenshots) (count (set (map #(get % "id") screenshots))))
                   (every? #(and (parse-id (get % "id")) (time? (get % "at"))
                                  (<= (:millis start) (get % "at")) (< (get % "at") (:millis until))) screenshots)
                   (time? (get recording "duration-ms")) (time? (get recording "offset-ms"))
                   (= (get recording "duration-ms") (- (:millis until) (:millis start))))
      (refuse! :invalid-note))
    (let [assets (mapv #(asset db session-root :image %) screenshots)
          video (asset db session-root :video recording)]
      {:id id :path path :entry entry :session session :started-at (:text start) :until (:text until)
       :from (:millis start) :duration-ms (- (:millis until) (:millis start))
       :screenshots (count screenshots) :available-screenshots (count (filter :available? assets))
       :recording video :alignment "Estimated"})))

(defn- cell [row kind]
  (case kind
    :started-at {:kind :instant :text (:started-at row)}
    :until {:kind :instant :text (:until row)}
    :duration {:kind :number :text (.toPlainString (.stripTrailingZeros (.movePointLeft (bigdec (:duration-ms row)) 3)))}
    :screenshots {:kind :text :text (str (:available-screenshots row) " / " (:screenshots row)) :complete? true}
    :note {:kind :page :path (:path row) :text "Open observations"}
    :recording (if (get-in row [:recording :available?])
                 (assoc (select-keys (:recording row) [:path :content :size]) :kind :media :text "Open recording")
                 {:kind :text :text "Original unavailable" :complete? true})
    :session {:kind :uuid :text (str (:session row))}
    :alignment {:kind :text :text (:alignment row) :complete? true}))

(defn prepare
  "Bound the entire explicit archive selection before displaying a row prefix.
   Source frontmatter is evidence, never permission to a personal capture DB."
  [db raw-descriptor]
  (let [descriptor (spec/validate raw-descriptor)]
    (when-not (= :archived-captures (get-in descriptor [:source :kind])) (refuse! :invalid-source))
    (let [selected (selection db (get-in descriptor [:source :roots]))
          rows (mapv #(note db %) selected)
          repository (:geschichte.repo/id (query/repository db))]
      (when-not (uuid? repository) (refuse! :invalid-source))
      (when-not (= (count rows) (count (set (map :id rows)))) (refuse! :duplicate-note-identity))
      {:profile contract/profile :view (:id descriptor) :descriptor descriptor :status :ready
       :source (assoc (:source descriptor) :repository repository)
       :basis (hasch/uuid [:simmis/capture-view-v1 repository descriptor
                          (mapv #(select-keys % [:id :path :entry :recording :available-screenshots]) rows)])
       :total (count rows) :complete? (<= (count rows) (:limit descriptor))
       :display {:complete? (<= (count rows) (:limit descriptor))}
       :columns (mapv #(assoc % :label (or (:label %) (get contract/capture-columns (:kind %)))) (:columns descriptor))
       :rows (mapv (fn [row] {:id [(:id row) (:path row)] :path (:path row)
                             :cells (mapv #(cell row (:kind %)) (:columns descriptor))})
                   (take (:limit descriptor) (sort-by (juxt :from (comp str :id) :path) rows)))})))
