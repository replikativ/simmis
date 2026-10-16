(ns is.simm.runtimes.transcripts
  "Meeting transcripts into their room: from the jigasi call transcriber, and
   from an audio file someone asks to have transcribed.

   ONE shape for both. A transcript is

   - a Markdown file on the room's drive, `transcripts/<yyyy-MM-dd-HHmm>-<name>.md`
     — the DOCUMENT wiki pages cite as their source (`[[transcripts/…md]]`), and
     what `transcript/read` returns to an agent; and
   - a room message (`📞 Call transcript`, metadata `:kind :transcript`) so the
     people in the room see it arrive. It is posted to the room log, not to the
     agents: an hour-long call must not wake every auto-respond agent. Ask an
     agent (the Meeting scribe persona) to record it.

   Sources:

   - The WATCHER (`start!`): jigasi's transcriber saves one file per finished
     conference into SIMMIS_TRANSCRIPTS_DIR. The meeting name IS the simmis room
     id (the :video tab mints room-bound meeting tokens), so the file maps
     straight to its room. Started at boot only when that variable is set.
   - TRANSCRIBE (`transcribe-drive-file!`): an explicit action on an audio file
     already on the room drive — \"Transcribe\" in Files, or
     `(transcript/transcribe! \"/drive/call.m4a\")` in an agent's sandbox. Never
     automatic on upload. Speech-to-text is `dvergr.audio.stt/transcribe` (Groq
     / OpenAI whisper with a real key, else local faster-whisper via
     scripts/transcribe.py). A file over `chunk-threshold-bytes` is cut into
     `chunk-seconds` pieces with ffmpeg first — the hosted whisper APIs refuse
     uploads over 25 MB; `stt` does not chunk."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as rtc]
            [taoensso.telemere :as log])
  (:import [java.nio.file FileSystems Path StandardWatchEventKinds WatchEvent WatchService]))

;; =============================================================================
;; Posting one transcript
;; =============================================================================

(def transcript-dir
  "The drive directory transcripts land in."
  "transcripts")

(defn- slug [s]
  (let [s (-> (str s) str/lower-case
              (str/replace #"\.[a-z0-9]{1,5}$" "")
              (str/replace #"[^a-z0-9]+" "-")
              (str/replace #"^-+|-+$" ""))]
    (if (str/blank? s) "call" (subs s 0 (min 60 (count s))))))

(defn transcript-file-name
  "`<yyyy-MM-dd-HHmm>-<slug>-<uuid>.md`: distinct ingestions never overwrite
   one another, even for the same title and timestamp."
  [title at]
  (str (.format (java.text.SimpleDateFormat. "yyyy-MM-dd-HHmm") (java.util.Date. (long at)))
       "-" (slug title) "-" (random-uuid) ".md"))

(defn- markdown [title body at source]
  (str "# " title "\n\n"
       "- recorded: " (.format (java.text.SimpleDateFormat. "yyyy-MM-dd HH:mm") (java.util.Date. (long at))) "\n"
       "- source: " source "\n\n"
       body "\n"))

(defn post-transcript!
  "Store `body` as `transcripts/<…>.md` on `room-uuid`'s drive and announce it
   in the room as a transcript message authored by `author` (a party uuid).
   `source` says where it came from (\"jigasi\", \"transcribed from call.m4a\").
   Returns {:path \"transcripts/….md\" :message-id uuid}."
  [room-uuid author body {:keys [title source at]
                          :or {title "Call transcript"}}]
  (let [at (or at (System/currentTimeMillis))
        live-room (requiring-resolve 'is.simm.agents.room-agents/live-room)
        room (or (live-room room-uuid)
                 (throw (ex-info "No live room to post the transcript into"
                                 {:type :no-live-room :room room-uuid})))
        file-name (transcript-file-name title at)
        path (str transcript-dir "/" file-name)
        bytes (.getBytes ^String (markdown title body at source) "UTF-8")]
    (binding [rtc/*execution-context* (:ctx room)]
      ((requiring-resolve 'is.simm.model.drives/store-in-room!)
       room-uuid [transcript-dir] file-name bytes :mime "text/markdown" :source :transcript))
    (let [msg-id ((requiring-resolve 'is.simm.agents.room-agents/post-room-record!)
                  room-uuid author
                  (str "📞 Call transcript — " title "\n"
                       "Saved to /drive/" path " (" source ")\n\n" body)
                  :transcript)]
      (log/log! {:level :info :id ::transcript-posted
                 :data {:room room-uuid :path path :chars (count body)}})
      {:path path :message-id msg-id})))

;; =============================================================================
;; Audio → text
;; =============================================================================

(def chunk-threshold-bytes
  "Above this the audio is cut into pieces first: Groq and OpenAI whisper
   refuse uploads over 25 MB, and `stt/transcribe` sends the file whole."
  (* 20 1024 1024))

(def chunk-seconds 600)

(defn- ffmpeg-split!
  "Cut `in` into `chunk-seconds` mono 16 kHz mp3 pieces in `dir`; the paths,
   in order. Throws when ffmpeg is missing or fails — a long file is never
   sent whole to a backend that would refuse it."
  [^java.io.File in ^java.io.File dir]
  (let [pattern (str (io/file dir "chunk-%04d.mp3"))
        {:keys [exit err]} (try
                             (shell/sh "ffmpeg" "-hide_banner" "-loglevel" "error"
                                       "-i" (str in) "-vn" "-ac" "1" "-ar" "16000"
                                       "-f" "segment" "-segment_time" (str chunk-seconds)
                                       "-c:a" "libmp3lame" "-b:a" "48k" pattern)
                             (catch java.io.IOException e
                               {:exit -1 :err (str "ffmpeg is not installed: " (ex-message e))}))]
    (when-not (zero? exit)
      (throw (ex-info (str "Could not split the audio for transcription: " (str/trim (str err)))
                      {:type :audio-split-failed})))
    (->> (.listFiles dir)
         (filter #(str/starts-with? (.getName ^java.io.File %) "chunk-"))
         (sort-by #(.getName ^java.io.File %))
         vec)))

(defn- stt! [^bytes bytes mime]
  ((requiring-resolve 'dvergr.audio.stt/transcribe) {:bytes bytes :mime mime}))

(defn transcribe-bytes
  "Audio bytes → transcript text. Throws `:transcription-failed` when no
   speech-to-text backend produced text (stt logs which ones it tried)."
  [^bytes bytes mime file-name]
  (let [texts (if (<= (alength bytes) chunk-threshold-bytes)
                [(stt! bytes mime)]
                (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                                    "simmis-stt" (make-array java.nio.file.attribute.FileAttribute 0)))
                      in (io/file dir (str "in-" (slug file-name)))]
                  (try
                    (io/copy bytes in)
                    (mapv (fn [^java.io.File f]
                            (stt! (java.nio.file.Files/readAllBytes (.toPath f)) "audio/mpeg"))
                          (ffmpeg-split! in dir))
                    (finally
                      (doseq [^java.io.File f (reverse (file-seq dir))] (.delete f))))))
        text (str/join "\n\n" (remove str/blank? texts))]
    (when (some str/blank? texts)
      (log/log! {:level :warn :id ::chunk-without-text
                 :data {:file file-name :chunks (count texts)
                        :empty (count (filter str/blank? texts))}}))
    (when (str/blank? text)
      (throw (ex-info (str "No speech-to-text backend produced a transcript of " file-name
                           " (tried Groq, OpenAI, native, local faster-whisper — see the server log)")
                      {:type :transcription-failed :file file-name})))
    (when (some str/blank? texts)
      (throw (ex-info (str "Incomplete transcript of " file-name
                           ": one or more audio chunks produced no text; retry from the original audio")
                      {:type :transcription-failed :file file-name
                       :chunks (count texts)
                       :missing-chunks (into [] (keep-indexed (fn [i s] (when (str/blank? s) i))) texts)})))
    text))

(defn audio-file?
  "Whether a drive node looks like audio (or a video whose soundtrack can be
   transcribed). By mime when the node has one, else by extension."
  [{:fs.node/keys [mime name]}]
  (boolean
   (or (some-> mime (str/starts-with? "audio/"))
       (some-> mime (str/starts-with? "video/"))
       (re-find #"(?i)\.(mp3|m4a|wav|ogg|oga|opus|webm|flac|aac|mp4|mkv|mov)$" (str name)))))

(defn transcribe-drive-file!
  "Transcribe an audio file on `room-uuid`'s drive — `file` is a drive path
   (\"/drive/call.m4a\" or \"call.m4a\") or a node uuid — and post the result
   as a transcript authored by `author`. Synchronous: call it off the request
   thread. Returns {:path :message-id} or throws with the reason."
  [room-uuid author file]
  (let [live-room (requiring-resolve 'is.simm.agents.room-agents/live-room)
        room (or (live-room room-uuid)
                 (throw (ex-info "No live room" {:type :no-live-room :room room-uuid})))
        [node bytes] (binding [rtc/*execution-context* (:ctx room)]
                       (let [conn ((requiring-resolve 'is.simm.model.drives/room-conn) room-uuid)
                             node (when conn
                                    (if (uuid? file)
                                      ((requiring-resolve 'is.simm.model.drives/stat) conn file)
                                      ((requiring-resolve 'is.simm.model.drives/resolve-path)
                                       conn (str/replace (str file) #"^/?(drive/)?" ""))))]
                         [node (when (and node (= :file (:fs.node/kind node)))
                                 ((requiring-resolve 'is.simm.model.drives/read-file)
                                  conn (:fs.node/id node)))]))
        rel (if (uuid? file) (:fs.node/name node) (str/replace (str file) #"^/?(drive/)?" ""))]
    (cond
      (nil? node) (throw (ex-info (str "No file " (if (uuid? file) file (str "/drive/" rel)))
                                  {:type :not-found :file (str file)}))
      (not (audio-file? node)) (throw (ex-info (str "/drive/" rel " is not an audio file")
                                               {:type :not-audio :path rel}))
      (nil? bytes) (throw (ex-info (str "/drive/" rel " has no content") {:type :not-found :path rel})))
    (let [text (transcribe-bytes bytes (or (:fs.node/mime node) "audio/ogg") (:fs.node/name node))]
      (post-transcript! room-uuid author text
                        {:title (str/replace (:fs.node/name node) #"\.[^.]+$" "")
                         :source (str "transcribed from /drive/" rel)}))))

(defn transcribe-in-background!
  "`transcribe-drive-file!` on a worker, reporting a failure INTO the room as a
   note to `author` — the person who asked is told, not just the log."
  [room-uuid author file label]
  (future
    (try
      (transcribe-drive-file! room-uuid author file)
      (catch Throwable t
        (log/log! {:level :warn :id ::transcription-failed
                   :data {:room room-uuid :file (str file) :error (ex-message t)}})
        (try
          ((requiring-resolve 'is.simm.agents.room-agents/post-room-record!)
           room-uuid author
           (str "Transcription of " label " failed: " (ex-message t))
           :transcript-failed)
          (catch Throwable t2
            (log/log! {:level :error :id ::failure-note-failed
                       :data {:room room-uuid :error (ex-message t2)}})))))))

;; =============================================================================
;; jigasi transcripts — the watcher
;; =============================================================================

(def ^:private uuid-re
  #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}")

(defn parse-txt-transcript
  "jigasi's LocalTxtTranscriptHandler output (the docker image hardcodes
   SAVE_TXT): a prose header naming the room + timestamped utterance lines.
   The room uuid comes from the file path or the content."
  [path content]
  (when-let [room (or (re-find uuid-re (str path)) (re-find uuid-re (str content)))]
    {:room-name room :text (str/trim (str content))}))

(defn parse-json-transcript
  "jigasi's saved-transcript JSON → {:room-name :lines [{:name :text}]}, or nil
   when it names no room. Throws on malformed JSON (a file still being written
   is retried on its next modify event)."
  [content]
  (let [m (json/read-value content json/keyword-keys-object-mapper)
        room (or (:room_name m) (:roomName m) (:room m))
        lines (->> (or (:events m) [])
                   (filter #(= "SPEECH" (str/upper-case (str (or (:event %) (:type %) "")))))
                   (keep (fn [ev]
                           (let [name (or (get-in ev [:participant :name]) (:name ev) "unknown")
                                 text (or (some->> (:transcript ev) (keep :text) first)
                                          (:text ev))]
                             (when (seq (str text))
                               {:name name :text (str/trim (str text))}))))
                   vec)]
    (when room {:room-name (str room) :lines lines})))

(defn- transcript-body [{:keys [text lines]}]
  (or text
      (when (seq lines)
        (str/join "\n" (map #(str "**" (:name %) "**: " (:text %)) lines)))))

(defn ingest-file!
  "Post one jigasi transcript file into its room. Returns the post result, or a
   keyword saying why it was skipped. The author is the room's creator — jigasi
   is not a party, and the transcript speaks for the room's own meeting."
  [^java.io.File f]
  (let [path (.getAbsolutePath f)
        content (slurp f)
        t (if (str/ends-with? path ".json")
            (parse-json-transcript content)
            (parse-txt-transcript path content))
        room-uuid (some-> (:room-name t) str/lower-case parse-uuid)
        room (when room-uuid ((requiring-resolve 'is.simm.model.rooms/get-room) room-uuid))
        body (when t (transcript-body t))]
    (cond
      (nil? room-uuid) (do (log/log! {:level :info :id ::unmapped-room
                                      :msg "Transcript for a non-simmis meeting — skipped"
                                      :data {:file path}})
                           :unmapped)
      (nil? room) (do (log/log! {:level :warn :id ::room-not-found
                                 :data {:room room-uuid :file path}})
                      :room-not-found)
      (str/blank? body) (do (log/log! {:level :info :id ::empty-transcript :data {:file path}})
                            :empty)
      :else (post-transcript! room-uuid (:room/created-by room) body
                              {:title (str (or (:room/name room) "Call") " call")
                               :source (str "jigasi " (.getName f))
                               :at (.lastModified f)}))))

(defonce ^:private watcher (atom nil))

(defn transcripts-dir [] (System/getenv "SIMMIS_TRANSCRIPTS_DIR"))

(defn- transcript-file? [^java.io.File f]
  (and (.isFile f)
       (let [n (.getName f)] (or (str/ends-with? n ".json") (str/ends-with? n ".txt")))))

(defn- handle-event!
  "One create/modify event. A file is posted once (by path). JSON that does not
   parse yet is still being written: it is left unposted and its next modify
   event tries again."
  [posted ^java.io.File f]
  (when (and (transcript-file? f) (not (contains? @posted (.getAbsolutePath f))))
    (try
      (let [r (ingest-file! f)]
        (swap! posted conj (.getAbsolutePath f))
        r)
      (catch com.fasterxml.jackson.core.JsonProcessingException _
        (log/log! {:level :debug :id ::incomplete-json :data {:file (str f)}}))
      (catch Throwable t
        (swap! posted conj (.getAbsolutePath f))
        (log/log! {:level :error :id ::ingest-failed
                   :data {:file (str f) :error (ex-message t)}})))))

(defn start!
  "Watch `dir` (default SIMMIS_TRANSCRIPTS_DIR) for new jigasi transcripts, with
   a filesystem WatchService — files already there are history and are not
   re-posted. Idempotent; returns :started, or nil (with one info log) when no
   directory is configured."
  ([] (start! (transcripts-dir)))
  ([dir]
   (cond
     (str/blank? (str dir))
     (log/log! {:level :info :id ::disabled
                :msg "Transcript watcher off: SIMMIS_TRANSCRIPTS_DIR is not set"})

     @watcher :already-running

     :else
     (let [d (io/file dir)
           _ (.mkdirs d)
           ^Path p (.toPath d)
           ^WatchService ws (.newWatchService (FileSystems/getDefault))
           posted (atom (into #{} (comp (filter transcript-file?)
                                        (map #(.getAbsolutePath ^java.io.File %)))
                              (.listFiles d)))
           running (atom true)]
       (.register p ws (into-array [StandardWatchEventKinds/ENTRY_CREATE
                                    StandardWatchEventKinds/ENTRY_MODIFY]))
       (reset! watcher {:dir (str d) :service ws :running running})
       (doto (Thread.
              (fn []
                (try
                  ;; `take` blocks until an event; `stop!` closes the
                  ;; service, which ends the wait with the exception below
                  (while @running
                    (let [k (.take ws)]
                      (doseq [^WatchEvent ev (.pollEvents k)
                              :let [ctx (.context ev)]
                              :when (instance? Path ctx)]
                        (handle-event! posted (.toFile (.resolve p ^Path ctx))))
                      (.reset k)))
                  (catch java.nio.file.ClosedWatchServiceException _ nil)
                  (catch Throwable t
                    (log/log! {:level :error :id ::watcher-died :data {:error (ex-message t)}}))))
              "simmis-transcripts-watcher")
         (.setDaemon true)
         (.start))
       (log/log! {:level :info :id ::started
                  :msg "Transcript watcher started" :data {:dir (str d)}})
       :started))))

(defn stop! []
  (when-let [{:keys [running ^WatchService service]} @watcher]
    (reset! running false)
    (.close service)
    (reset! watcher nil)
    :stopped))

;; =============================================================================
;; The agent's `transcript/*` vocabulary
;; =============================================================================

(defn- room-transcripts [room-uuid]
  (let [room ((requiring-resolve 'is.simm.agents.room-agents/live-room) room-uuid)]
    (when room
      (binding [rtc/*execution-context* (:ctx room)]
        (let [conn ((requiring-resolve 'is.simm.model.drives/room-conn) room-uuid)
              dir (some-> conn ((requiring-resolve 'is.simm.model.drives/resolve-path) transcript-dir))]
          (when dir
            [conn (->> ((requiring-resolve 'is.simm.model.drives/ls) conn (:fs.node/id dir))
                       (remove #(= :dir (:fs.node/kind %))))]))))))

(defn sci-namespace
  "`transcript/*` for an agent in `room-uuid`, acting as `agent-uuid`."
  [room-uuid agent-uuid]
  {'list (fn []
           (->> (second (room-transcripts room-uuid))
                (map (fn [n] {:path (str "/drive/" transcript-dir "/" (:fs.node/name n))
                              :name (:fs.node/name n)
                              :size (:fs.node/size n)}))
                (sort-by :name #(compare %2 %1))
                vec))
   'read (fn [path]
           (let [name (last (str/split (str path) #"/"))
                 [conn nodes] (room-transcripts room-uuid)
                 node (some #(when (= name (:fs.node/name %)) %) nodes)]
             (when node
               (let [room ((requiring-resolve 'is.simm.agents.room-agents/live-room) room-uuid)]
                 (binding [rtc/*execution-context* (:ctx room)]
                   (some-> ((requiring-resolve 'is.simm.model.drives/read-file) conn (:fs.node/id node))
                           (String. "UTF-8")))))))
   'transcribe! (fn [path]
                  (transcribe-drive-file! room-uuid agent-uuid path))})

(def prompt-block
  "What the agent is told about `transcript/*`. Kept next to the code."
  (str "\n\nMEETING TRANSCRIPTS are files on the room drive under /drive/transcripts/"
       " (a call transcribed by jigasi, or an audio file someone asked to have"
       " transcribed). In clojure_eval:\n"
       "- `(transcript/list)` — [{:path :name :size}], newest first\n"
       "- `(transcript/read \"/drive/transcripts/2026-10-16-1400-standup.md\")` — the text\n"
       "- `(transcript/transcribe! \"/drive/call.m4a\")` — transcribe an audio file"
       " on the drive (slow: minutes for an hour of audio) → {:path :message-id};"
       " only when someone asks for it.\n"
       "Cite a transcript in a wiki page as [[transcripts/<file>.md]]."))
