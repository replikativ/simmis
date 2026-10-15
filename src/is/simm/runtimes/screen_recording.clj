(ns is.simm.runtimes.screen-recording
  "Continuous screen RECORDING (Track 4e) — the archive tier under the live
   screenshot tier.

   Two tiers, deliberately, because they do different jobs:

     screen-intake   a described frame every ~8s behind a scene-change gate.
                     Cheap, immediate, small enough to sit in a tool result.
                     Agents read it NOW, through `screen_look`. It DISCARDS —
                     that is its purpose.

     screen-recording (this)  the raw stream, gapless, kept. Nobody reads it
                     live. It exists to be RE-INTERPRETED later, by whatever
                     the best VLM is that month, at whatever cadence the
                     question needs.

   The two share a clock, and that is the whole point: a screenshot's
   `:screenshot/at` minus a session's `:recording.session/started-at` is a
   TIMECODE into the video. The cheap description index we already build thus
   becomes a searchable index INTO the raw recording — search a phrase, land at
   the moment. Neither tier gives you that alone.

   Wire format. The browser's MediaRecorder emits chunks that are NOT
   independently playable: the first carries the container header, the rest are
   raw cluster continuations. So chunks are APPENDED, in order, into one file
   per segment — and one lost chunk corrupts everything after it. Hence
   segments (default 5 min, rotated client-side): a valid file on its own, a
   dropped chunk costs one segment rather than the session, and segments
   finalize and post-process in parallel. The price is a seam of a frame or two
   at each rotation — cheap against losing an hour.

   Layout:  data/simmis-recordings/<owner-uuid>/<session-uuid>/seg-<n>.<ext>

   OWNERSHIP (doc/archive/screen-capture-scoping.md): a recording is the OWNER's archive,
   keyed by the sharing party (from the JWT), not by a room. Rooms see slices of
   it only through an active screen-share grant (v2 for the room-side playback;
   v1 keys the store per owner so that view is a query, not a migration). The
   route is authenticated (`hauth/require-auth`); the owner is the caller."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [datahike.api :as d]
            [is.simm.model.blobs :as blobs]
            [is.simm.model.system-db :as system-db]
            [taoensso.telemere :as log])
  (:import [java.io File FileOutputStream]))

(def ^:private root "data/simmis-recordings")

;; ---------------------------------------------------------------------------
;; Schema — recordings live in the room's SCREENS db, beside the descriptions
;; they index (one clock, one store, one query).
;; ---------------------------------------------------------------------------

(def recording-schema
  [{:db/ident :recording.session/id :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :recording.session/started-at :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :recording.session/mime :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :recording.session/width :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :recording.session/height :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :recording.session/fps :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :recording.session/ended-at :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}

   {:db/ident :recording.segment/session :db/valueType :db.type/ref
    :db/cardinality :db.cardinality/one}
   {:db/ident :recording.segment/idx :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   ;; ms since the SESSION start — the seam between the two tiers
   {:db/ident :recording.segment/offset-ms :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :recording.segment/duration-ms :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :recording.segment/bytes :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one}
   {:db/ident :recording.segment/blob-id :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :recording.segment/status :db/valueType :db.type/keyword
    :db/cardinality :db.cardinality/one}])

(defonce ^:private conns (atom {}))

(defn- cfg [owner-uuid]
  {:store {:backend :file
           :path (str root "/" owner-uuid "/db")
           ;; STABLE store id, derived from the room. A fresh randomUUID per
           ;; call means every connect describes the store differently — which
           ;; is at best meaningless and at worst how a writer ends up fatally
           ;; confused about which store it is writing.
           :id (java.util.UUID/nameUUIDFromBytes
                 (.getBytes (str "simmis-recordings/" owner-uuid) "UTF-8"))}
   :schema-flexibility :write
   :keep-history? false
   :crypto-hash? true})

(defn- conn-for
  "The room's RECORDINGS db.

   Deliberately its own store rather than the screens DB it indexes. Recordings
   have a different lifecycle — orders of magnitude larger, destined for tiered
   /object storage, and prunable on a retention policy — while descriptions are
   small, hot, and permanent. The `locate` seam joins them by TIME, which needs
   no shared store: a wall-clock timestamp is the only thing they must agree on,
   and that they cannot help but share."
  [owner-uuid]
  (or (get @conns owner-uuid)
      (locking conns
        (or (get @conns owner-uuid)
            (let [c (cfg owner-uuid)
                  _ (when-not (d/database-exists? c) (d/create-database c))
                  conn (d/connect c)]
              (d/transact conn recording-schema)
              (swap! conns assoc owner-uuid conn)
              conn)))))

(defn- transact!
  "Transact, surviving a shut-down writer.

   Datahike kills a store's writer on a fatal error and then fails EVERY
   subsequent transaction with 'Writer is shut down ... release and reconnect'.
   A recording that captured perfectly and then lost its metadata to a poisoned
   connection is the worst of both worlds — the bytes are on disk and nothing
   knows they exist. So we do what the error asks."
  [owner-uuid tx]
  (try
    (d/transact (conn-for owner-uuid) tx)
    (catch Throwable t
      (if (re-find #"(?i)writer is shut down" (str (ex-message t) (ex-cause t)))
        (do (log/log! {:level :warn :id ::writer-restart
                       :data {:owner owner-uuid}}
                      "Datahike writer was shut down; reconnecting")
            (when-let [old (get @conns owner-uuid)]
              (try (d/release old) (catch Throwable _ nil)))
            (swap! conns dissoc owner-uuid)
            (d/transact (conn-for owner-uuid) tx))
        (throw t)))))

;; ---------------------------------------------------------------------------
;; Open segment files — appended chunk by chunk as they arrive
;; ---------------------------------------------------------------------------

;; [session-uuid seg-idx] → {:out FileOutputStream :path str :bytes long :next-seq long}
(defonce ^:private open-segments (atom {}))

(defn- ext-for [mime]
  (cond
    (str/includes? (str mime) "mp4")  "mp4"
    (str/includes? (str mime) "webm") "webm"
    :else "bin"))

(defn- segment-path [owner-uuid session-id idx mime]
  (str root "/" owner-uuid "/" session-id "/seg-" idx "." (ext-for mime)))

(defn- ffprobe-duration-ms
  "Actual duration of a finalized segment. The browser does not tell us — and
   the timecode index is only as good as this number."
  [path]
  (try
    (let [{:keys [out exit]} ((requiring-resolve 'clojure.java.shell/sh)
                              "ffprobe" "-v" "error"
                              "-show_entries" "format=duration"
                              "-of" "default=noprint_wrappers=1:nokey=1"
                              path)]
      (when (zero? exit)
        (some-> (str/trim out) parse-double (* 1000) long)))
    (catch Throwable _ nil)))

(defn- remux!
  "Rewrite the segment through ffmpeg (stream copy) so it carries a duration and
   seek cues. MediaRecorder output has neither — it is written for streaming,
   not seeking, and a browser will refuse to scrub it."
  [path mime]
  (try
    (let [fixed (str path ".fixed." (ext-for mime))
          {:keys [exit]} ((requiring-resolve 'clojure.java.shell/sh)
                          "ffmpeg" "-y" "-v" "error" "-i" path "-c" "copy" fixed)]
      (if (and (zero? exit) (.exists (File. fixed)))
        (do (io/copy (File. fixed) (File. path))
            (.delete (File. fixed))
            true)
        false))
    (catch Throwable t
      (log/log! {:level :warn :id ::remux-failed :data {:path path :error (ex-message t)}})
      false)))

;; ---------------------------------------------------------------------------
;; Ingest
;; ---------------------------------------------------------------------------

(defn start-session!
  "Open a recording session for `owner-uuid` (the authenticated sharer). Returns
   a ring response."
  [owner-uuid {:keys [session-id mime width height fps]}]
  (cond
    (nil? owner-uuid)
    {:status 401 :body "unauthenticated"}

    ;; a malformed id would parse to nil and create the session row with no id
    ;; under the owner's recordings root
    (nil? (some-> session-id str parse-uuid))
    {:status 400 :body "malformed session-id"}

    :else
    (let [sid (parse-uuid (str session-id))
          now (System/currentTimeMillis)]
      (.mkdirs (File. (str root "/" owner-uuid "/" sid)))
      (transact! owner-uuid
                  [{:recording.session/id sid
                    :recording.session/started-at now
                    :recording.session/mime (or mime "video/webm")
                    :recording.session/width (or width 0)
                    :recording.session/height (or height 0)
                    :recording.session/fps (or fps 0)}])
      (log/log! {:level :info :id ::session-started
                 :data {:owner owner-uuid :session sid :mime mime
                        :res (str width "x" height) :fps fps}})
      {:status 200 :body "started"})))

(defn append-chunk!
  "Append one MediaRecorder chunk to its segment file.

   `seq` is the chunk's position within the segment. Chunks MUST land in order —
   a container's clusters are not commutative — so an out-of-order chunk is
   REJECTED rather than written: a visibly missing segment beats a silently
   corrupt one that only fails months later, in the training set."
  [owner-uuid ^bytes bytes {:keys [session-id segment seq mime]}]
  (let [sid (parse-uuid (str session-id))
        idx (long (or segment 0))
        seq-n (long (or seq 0))
        k [sid idx]]
    (if (nil? owner-uuid)
      {:status 401 :body "unauthenticated"}
      (let [seg (or (get @open-segments k)
                    (let [path (segment-path owner-uuid sid idx mime)]
                      (.mkdirs (.getParentFile (File. path)))
                      (let [s {:out (FileOutputStream. path true)
                               :path path
                               :bytes 0
                               :next-seq 0
                               :owner owner-uuid
                               :session sid
                               :idx idx
                               :mime mime
                               :opened-at (System/currentTimeMillis)}]
                        (swap! open-segments assoc k s)
                        s)))]
        (cond
          (< seq-n (:next-seq seg))
          {:status 200 :body "duplicate"}          ; a retry of a chunk we have

          (> seq-n (:next-seq seg))
          (do (log/log! {:level :error :id ::chunk-out-of-order
                         :data {:session sid :segment idx
                                :expected (:next-seq seg) :got seq-n}})
              {:status 409 :body "out of order"})

          :else
          (do (.write ^FileOutputStream (:out seg) bytes)
              (swap! open-segments update k
                     #(-> % (update :bytes + (alength bytes))
                            (update :next-seq inc)))
              {:status 200 :body "ok"}))))))

(defn finalize-segment!
  "Close a segment: flush, remux for seekability, hash into the blob store, and
   record it against the session with its offset from session start."
  [owner-uuid {:keys [session-id segment offset-ms]}]
  (let [sid (parse-uuid (str session-id))
        idx (long (or segment 0))
        k [sid idx]]
    (if-let [seg (get @open-segments k)]
      (do
        (.close ^FileOutputStream (:out seg))
        (swap! open-segments dissoc k)
        (future
          (try
            (let [{:keys [path mime]} seg
                  _ (remux! path mime)
                  dur (ffprobe-duration-ms path)
                  bytes (.length (File. path))
                  blob (blobs/store! (java.nio.file.Files/readAllBytes
                                       (.toPath (File. path)))
                                     (or mime "video/webm"))]
              (transact! owner-uuid
                          [{:recording.segment/session [:recording.session/id sid]
                            :recording.segment/idx idx
                            :recording.segment/offset-ms (long (or offset-ms 0))
                            :recording.segment/duration-ms (long (or dur 0))
                            :recording.segment/bytes bytes
                            :recording.segment/blob-id (:blob/id blob)
                            :recording.segment/status :ready}])
              (log/log! {:level :info :id ::segment-finalized
                         :data {:owner owner-uuid :session sid :segment idx
                                :mb (format "%.1f" (/ bytes 1048576.0))
                                :duration-s (when dur (long (/ dur 1000)))
                                :blob (:blob/id blob)}}))
            (catch Throwable t
              (log/log! {:level :error :id ::finalize-failed
                         :data {:session sid :segment idx :error (str t)}}))))
        {:status 200 :body "finalizing"})
      {:status 404 :body "no such open segment"})))

(defn end-session!
  [owner-uuid {:keys [session-id]}]
  (let [sid (parse-uuid (str session-id))]
    (transact! owner-uuid
                [{:recording.session/id sid
                  :recording.session/ended-at (System/currentTimeMillis)}])
    (log/log! {:level :info :id ::session-ended :data {:owner owner-uuid :session sid}})
    {:status 200 :body "ended"}))

;; ---------------------------------------------------------------------------
;; The seam: a moment in the descriptions → a moment in the video
;; ---------------------------------------------------------------------------

(defn locate
  "Where in the recordings does wall-clock `at-ms` fall?
   → {:session :segment :blob-id :offset-in-segment-ms} or nil.

   This is what makes the two tiers one system: every description we ever wrote
   carries `:screenshot/at`, so every description is addressable IN the video."
  [owner-uuid at-ms]
  (let [db @(conn-for owner-uuid)
        sessions (d/q '[:find ?sid ?start
                        :where
                        [?s :recording.session/id ?sid]
                        [?s :recording.session/started-at ?start]]
                      db)]
    (when-let [[sid start] (->> sessions
                                (filter (fn [[_ start]] (<= start at-ms)))
                                (sort-by second >)
                                first)]
      (let [want (- at-ms start)
            segs (d/q '[:find ?idx ?off ?dur ?blob
                        :in $ ?sid
                        :where
                        [?s :recording.session/id ?sid]
                        [?e :recording.segment/session ?s]
                        [?e :recording.segment/idx ?idx]
                        [?e :recording.segment/offset-ms ?off]
                        [?e :recording.segment/blob-id ?blob]
                        [(get-else $ ?e :recording.segment/duration-ms 0) ?dur]]
                      db sid)]
        (when-let [[idx off _dur blob]
                   (->> segs
                        (filter (fn [[_ off dur _]]
                                  (and (<= off want)
                                       (or (zero? dur) (< want (+ off dur))))))
                        (sort-by second >)
                        first)]
          {:session sid
           :segment idx
           :blob-id blob
           :offset-in-segment-ms (- want off)})))))

;; ---------------------------------------------------------------------------
;; Owner gallery: list / delete recording sessions (doc/archive/screen-capture-scoping.md)
;; ---------------------------------------------------------------------------

(defn list-sessions
  "The OWNER's recording sessions, newest first, each with its ready segments
   (blob-id + timing) so the client can play or delete them."
  [owner-uuid]
  (let [db @(conn-for owner-uuid)]
    (->> (d/q '[:find ?sid ?start ?mime
                :where [?s :recording.session/id ?sid]
                       [?s :recording.session/started-at ?start]
                       [(get-else $ ?s :recording.session/mime "video/webm") ?mime]]
              db)
         (map (fn [[sid start mime]]
                (let [segs (->> (d/q '[:find ?idx ?blob ?dur ?off ?bytes
                                       :in $ ?sid
                                       :where [?s :recording.session/id ?sid]
                                              [?e :recording.segment/session ?s]
                                              [?e :recording.segment/idx ?idx]
                                              [?e :recording.segment/blob-id ?blob]
                                              [(get-else $ ?e :recording.segment/duration-ms 0) ?dur]
                                              [(get-else $ ?e :recording.segment/offset-ms 0) ?off]
                                              [(get-else $ ?e :recording.segment/bytes 0) ?bytes]]
                                     db sid)
                                (map (fn [[idx blob dur off bytes]]
                                       {:idx idx :blob-id blob :duration-ms dur
                                        :offset-ms off :bytes bytes}))
                                (sort-by :idx)
                                vec)]
                  {:session (str sid) :started-at start :mime mime
                   :segments segs
                   :total-bytes (reduce + 0 (map :bytes segs))})))
         (sort-by :started-at >)
         vec)))

(defn delete-session!
  "Delete one of the OWNER's recording sessions: retract its session+segment
   datoms and remove the raw segment files on disk. The CAS blobs are left for
   GC (content-addressed). Returns {:deleted-segments n}."
  [owner-uuid session-id-str]
  (system-db/require-id! :owner owner-uuid)
  ;; BEFORE any query or file op. A malformed id parses to nil; datahike reads a
  ;; nil `:in` binding as unbound, so both queries below matched ALL of the
  ;; owner's sessions, and `dir` became the owner's whole recordings directory,
  ;; deleted recursively.
  (let [sid (system-db/require-id! :session-id (some-> session-id-str str parse-uuid))
        db @(conn-for owner-uuid)
        seg-eids (d/q '[:find [?seg ...] :in $ ?sid
                        :where [?s :recording.session/id ?sid]
                               [?seg :recording.segment/session ?s]]
                      db sid)
        s-eid (d/q '[:find ?s . :in $ ?sid
                     :where [?s :recording.session/id ?sid]] db sid)
        dir (File. (str root "/" owner-uuid "/" sid))]
    (when (.exists dir)
      (doseq [f (reverse (file-seq dir))] (.delete f)))
    (when (or s-eid (seq seg-eids))
      (transact! owner-uuid
                 (mapv (fn [e] [:db/retractEntity e])
                       (cond-> (vec seg-eids) s-eid (conj s-eid)))))
    (log/log! {:level :info :id ::session-deleted
               :data {:owner owner-uuid :session sid :segments (count seg-eids)}})
    {:deleted-segments (count seg-eids)}))
