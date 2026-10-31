(ns is.simm.documents.summary-documents
  "Deterministic plain Markdown projections, not summary authority. Generated
   source text cannot become executable fences or active custom components."
  (:require [clojure.string :as str]
            [hasch.core :as hasch]
            [datahike.blob :as blob]
            [is.simm.documents.markdown :as md]
            [is.simm.documents.links :as links]
            [is.simm.model.conversation-digest :as digest]
            [is.simm.model.room-summaries :as summaries]
            [is.simm.model.summary-periods :as periods]))

(defn escaped [text]
  (-> text (str/replace "&" "&amp;") (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace #"[\\`*_{}\[\]()#!|~$>+\-]" #(str "\\" %))))

(defn path [selection]
  (str "summaries/conversations/" (:room selection) "/" (periods/day-label (:period selection)) ".md"))

(defn transcript-path [selection]
  (str "conversations/" (:room selection) "/" (periods/day-label (:period selection)) ".md"))

(defn source-link [room request source]
  (links/request-source-route {:room room :request request :path (str "/in/summary/sources/" (:path source))}))

(def labels {:decisions "Decisions" :proposals "Proposals" :observations "Observations"
             :uncertainty "Uncertainty" :open-work "Open questions and work"})

(defn markdown [{:keys [selection report program]}]
  (summaries/validate-report! selection report)
  (let [revision (summaries/revision-id selection report program)
        node (summaries/node-id selection)
        by-id (into {} (map (juxt :source-revision identity))
                    (into (:coverage selection) (:context selection)))
        citations (fn [item]
                    (str/join " "
                              (map-indexed (fn [index id]
                                             (str "[Source " (inc index) "](" (source-link (:room selection)
                                                                                                (:request program) (get by-id id)) ")"))
                                           (:sources item))))]
    (str "---\nsimmis-id: " node "\nsimmis-generated: room-summary\nsummary-revision: " revision
         "\nsource-room: " (:room selection) "\nsummary-kind: " (name (get-in selection [:period :kind]))
         "\nsummary-timezone: " (pr-str (get-in selection [:period :timezone]))
         "\nsummary-collected-at: " (.toInstant ^java.util.Date (:cutoff selection))
         "\nsummary-inventory: current-revisions"
         "\nsummary-complete: " (boolean (summaries/ready? selection report))
         "\n---\n# Conversation summary · " (periods/day-label (:period selection)) "\n\n"
         (when-not (summaries/ready? selection report) "**Incomplete summary: unresolved evidence remains.**\n\n")
         (escaped (get-in report [:summary :text])) "\n\n" (citations (:summary report)) "\n\n"
         (apply str (for [key digest/section-keys :let [items (get-in report [:sections key])] :when (seq items)]
                      (str "## " (get labels key) "\n\n"
                           (apply str (for [item items]
                                        (str "- " (escaped (:text item)) " " (citations item) "\n"))) "\n")))
         "Generated from retained originals and separately identified prior context. "
         "This is a read-only projection, not an adopted wiki update or factual certification.\n")))

(defn context-source [row payload]
  (let [text (markdown payload)]
    {:source {:source-id (:room.summary/node row)
              :source-revision (:room.summary/revision row)
              :path (str "summary-context/" (:room.summary/revision row) ".md")
              :file-content (blob/blob-id (md/source-bytes text))
              :kind :summary :authority :unknown :through (:room.summary/until row)}
     :text text}))

(def index-path "summaries/index.md")

(defn index-markdown [repository]
  (str "---\nsimmis-id: " (hasch/uuid [:simmis/summary-index-page-v1 repository])
       "\n---\n# Conversation summaries\n\n"
       "Generated daily summaries for the room from which this Memory is opened. "
       "Original messages remain available through each summary’s citations.\n\n"
       "```simmis-view\n"
       (pr-str {:version 1 :id (hasch/uuid [:simmis/summary-index-view-v1 repository])
                :source {:kind :room-summaries :room :current} :presentation :table
                :columns [{:kind :period} {:kind :summary} {:kind :status}
                          {:kind :originals} {:kind :publication}] :limit 32})
       "\n```\n\n"
       "Daily summaries are configured in Room settings and are off by default. "
       "This index is editable; generated summary files preserve the native result and are read only.\n"))
