(ns is.simm.uis.web.desktop.screen-recorder
  "Continuous screen recording (Track 4e) — the archive tier beside the live
   screenshot tier.

   Hangs a MediaRecorder on the SAME getDisplayMedia stream the screenshot loop
   is already sampling, so recording costs one encoder, not a second capture.
   The screenshot path is untouched: it keeps its 8s scene-change gate for the
   agents, while this keeps everything for later.

   ONE session per capture (doc/archive/screen-capture-scoping.md): the recording is the
   OWNER's archive, keyed server-side by the authenticated party (from the JWT),
   not by a room — so the upload URLs carry no room, and this holds a single
   session rather than a per-room map.

   Segmented, ~5 min apiece. MediaRecorder chunks are NOT independently
   playable — the first carries the container header, the rest are raw cluster
   continuations — so they are appended in order, and one lost chunk corrupts
   all that follow. A segment bounds that blast radius to itself, and lets
   segments upload and post-process in parallel. Cost: a seam of a frame or two
   at each rotation, which is cheap against losing an hour of capture.

   `start!` takes the stream rather than acquiring one, so a canvas
   captureStream can drive the whole path in a test without a screen picker.

   The binary chunk upload stays HTTP (blob size); the finalize marker rides the
   SAME queue so it lands AFTER the chunks it closes."
  (:require [clojure.string :as str]
            [is.simm.uis.web.desktop.login :as login]
            [is.simm.uis.web.desktop.remote :as rem]))

(defonce ^:private session (atom nil))   ; the single recording session, or nil

(def ^:private default-segment-ms (* 5 60 1000))
(def ^:private chunk-ms 2000)            ; upload granularity: a crash loses ≤2s

(def ^:private codec-preference
  ;; Hardware first: encoding a 5120x1440 desktop in software (VP9) pins a core
  ;; and drops frames, while H.264 goes to the GPU on any machine we care about.
  ;;
  ;; avc3, NOT avc1 — and this is a trap, because `isTypeSupported` says TRUE to
  ;; both. avc1 keeps the codec description (SPS/PPS) in the container header
  ;; only, so a chunked recording whose encoder re-describes itself dies:
  ;;   "the codec description is not supposed to change during the entire
  ;;    recording ... consider switching to avc3"
  ;; and Chrome then emits NO data at all — a silent, empty recording. avc3
  ;; carries the parameter sets IN-BAND, which is what a stream of independent
  ;; chunks requires. Same hardware encoder, one of them just cannot be cut.
  ["video/mp4;codecs=avc3"
   "video/webm;codecs=h264"
   "video/webm;codecs=vp9"
   "video/webm;codecs=vp8"
   "video/webm"])

(defn pick-mime
  "The best container/codec this browser will actually give us."
  []
  (or (first (filter #(and (exists? js/MediaRecorder)
                           (.isTypeSupported js/MediaRecorder %))
                     codec-preference))
      "video/webm"))

(defn recording? [] (some? @session))

;; ---------------------------------------------------------------------------
;; Ordered upload — clusters are not commutative
;; ---------------------------------------------------------------------------

(defn- post-item
  "One queued item → its request. A :finalize item rides the SAME queue as the
   chunks, which is the only way to be sure it lands AFTER them: finalizing from
   MediaRecorder's onstop closed the segment server-side while its data was
   still queued in the browser, and the segment came out as its 261-byte init
   header with the video dropped on the floor."
  [{:keys [kind blob session-id segment seq-n mime offset-ms]}]
  (if (= kind :finalize)
    (js/fetch (str "/screen-recordings/finalize"
                   "?session=" session-id
                   "&segment=" segment
                   "&offset=" offset-ms)
              #js {:method "POST" :headers (login/auth-headers)})
    (js/fetch (str "/screen-recordings/chunk"
                   "?session=" session-id
                   "&segment=" segment
                   "&seq=" seq-n
                   "&mime=" (js/encodeURIComponent mime))
              #js {:method "POST"
                   :headers (login/auth-header-map
                              #js {"Content-Type" "application/octet-stream"})
                   :body blob})))

(declare stop!)

(defn- drain!
  "Post queued items strictly in order, one in flight at a time. Chunks are
   container clusters: they do not commute, and the server rejects an
   out-of-order one rather than corrupt the file — so we must never race them."
  []
  (let [{:keys [queue draining?]} @session]
    (when (and queue (seq @queue) (not @draining?))
      (reset! draining? true)
      (-> (post-item (first @queue))
          (.then (fn [resp]
                   (reset! draining? false)
                   (if (.-ok resp)
                     (do (swap! queue rest)
                         (drain!))
                     ;; Refused. Re-posting the same item at once (what this
                     ;; did) is a tight loop that never lands; the items after
                     ;; it cannot land either — chunks do not commute. Drop
                     ;; the queue, stop recording, and say so.
                     (do (reset! queue [])
                         (rem/report-error! "Screen recording stopped: the server refused a segment."
                                            (ex-info (str "HTTP " (.-status resp))
                                                     {:error (str "HTTP " (.-status resp))}))
                         (stop!)))))
          (.catch (fn [e]
                    (js/console.warn "[rec] upload failed, retrying:" e)
                    (reset! draining? false)
                    (js/setTimeout #(drain!) 2000)))))))

(defn- enqueue! [chunk]
  (when-let [{:keys [queue]} @session]
    (swap! queue concat [chunk])
    (drain!)))

;; ---------------------------------------------------------------------------
;; Segment lifecycle
;; ---------------------------------------------------------------------------

(declare start-segment!)

(defn- finalize-segment! [idx offset-ms]
  (when-let [{:keys [session-id]} @session]
    (enqueue! {:kind :finalize
               :session-id session-id
               :segment idx
               :offset-ms offset-ms})))

(defn- start-segment! []
  (when-let [{:keys [stream mime session-id started-at segment-ms seg-idx bitrate]} @session]
    (let [idx @seg-idx
          offset-ms (- (js/Date.now) started-at)
          rec (js/MediaRecorder. stream
                                 (clj->js (cond-> {:mimeType mime}
                                            bitrate (assoc :videoBitsPerSecond bitrate))))
          seq-n (atom 0)]
      (set! (.-ondataavailable rec)
            (fn [e]
              (let [blob (.-data e)]
                (when (pos? (.-size blob))
                  (enqueue! {:kind :chunk
                             :blob blob
                             :session-id session-id
                             :segment idx
                             :seq-n (let [s @seq-n] (swap! seq-n inc) s)
                             :mime mime})))))
      (set! (.-onstop rec)
            (fn []
              (js/setTimeout #(finalize-segment! idx offset-ms) 0)))
      (.start rec chunk-ms)
      (swap! session assoc :recorder rec)
      (let [t (js/setTimeout
                (fn []
                  (when (recording?)
                    (swap! seg-idx inc)
                    (.stop rec)                ; flushes a final chunk, fires onstop
                    (start-segment!)))
                segment-ms)]
        (swap! session assoc :rotate-timer t)))))

;; ---------------------------------------------------------------------------
;; Public
;; ---------------------------------------------------------------------------

(defn start!
  "Record `stream` into the caller's OWN archive (owner from the JWT). Returns
   the session id. opts: :segment-ms (default 5 min) :bitrate"
  [stream & [{:keys [segment-ms bitrate]}]]
  (when-not (recording?)
    (let [session-id (str (random-uuid))
          mime (pick-mime)
          track (first (.getVideoTracks stream))
          settings (when track (.getSettings track))
          width (or (some-> settings .-width) 0)
          height (or (some-> settings .-height) 0)
          fps (or (some-> settings .-frameRate) 0)]
      (reset! session
              {:session-id session-id
               :stream stream
               :mime mime
               :started-at (js/Date.now)
               :segment-ms (or segment-ms default-segment-ms)
               :bitrate bitrate
               :seg-idx (atom 0)
               :queue (atom [])
               :draining? (atom false)})
      (-> (js/fetch "/screen-recordings/start"
                    #js {:method "POST"
                         :headers (login/auth-header-map
                                    #js {"Content-Type" "application/json"})
                         :body (js/JSON.stringify
                                 #js {:session session-id
                                      :mime mime
                                      :width width
                                      :height height
                                      :fps (js/Math.round fps)})})
          (.then (fn [resp]
                   (if (.-ok resp)
                     (start-segment!)
                     (do (reset! session nil)
                         (rem/report-error! "Screen recording did not start."
                                            (ex-info (str "HTTP " (.-status resp))
                                                     {:error (str "HTTP " (.-status resp))}))))))
          (.catch (fn [e]
                    (reset! session nil)
                    (rem/report-error! "Screen recording did not start." e))))
      (js/console.log "[rec] recording as" mime
                      (str width "x" height "@" (js/Math.round fps) "fps"))
      session-id)))

(defn stop!
  "Stop recording, flushing and finalizing the segment in flight."
  []
  (when-let [{:keys [recorder rotate-timer session-id]} @session]
    (when rotate-timer (js/clearTimeout rotate-timer))
    (when (and recorder (not= "inactive" (.-state recorder)))
      (.stop recorder))
    ;; Close out only once the queue is EMPTY — the last segment's chunks and
    ;; its finalize are still in flight. Polling the queue (not a fixed sleep)
    ;; is what makes this correct on a slow link.
    (let [poll (atom nil)]
      (reset! poll
        (js/setInterval
          (fn []
            (let [{:keys [queue]} @session]
              (when (or (nil? queue) (empty? @queue))
                (js/clearInterval @poll)
                (-> (js/fetch (str "/screen-recordings/end?session=" session-id)
                              #js {:method "POST" :headers (login/auth-headers)})
                    (.then (fn [resp]
                             (when-not (.-ok resp)
                               (rem/report-error! "The recording may not have been closed."
                                                  (ex-info (str "HTTP " (.-status resp))
                                                           {:error (str "HTTP " (.-status resp))})))))
                    (.catch (fn [e] (rem/report-error! "The recording may not have been closed." e))))
                (reset! session nil)
                (js/console.log "[rec] stopped"))))
          250)))))

(defn status
  "What is being recorded right now — for the UI indicator and for tests."
  []
  (when-let [{:keys [session-id mime started-at seg-idx queue]} @session]
    {:session session-id
     :mime mime
     :elapsed-ms (- (js/Date.now) started-at)
     :segment @seg-idx
     :pending-chunks (count @queue)}))
