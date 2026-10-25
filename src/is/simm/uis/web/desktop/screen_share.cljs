(ns is.simm.uis.web.desktop.screen-share
  "Share the local screen — ONE capture, per-room GRANTS
   (doc/archive/screen-capture-scoping.md).

   getDisplayMedia → hidden <video> → periodic canvas snapshot behind a
   scene-change gate → POST JPEG to /screen-frames (the caller's OWN stream;
   identity from the JWT, no room in the path). The server describes each frame
   with a VLM into the user's per-user buffer; which rooms may see it is decided
   by GRANTS, not by where it was posted.

   The share button is a GRANT toggle, not a capture switch. There is exactly one
   getDisplayMedia session: the first room you share into starts it (browser
   picker); every other room's toggle just opens/closes a grant over that same
   capture and heartbeats it; turning off the last grant stops the capture. So
   {personal, R2, R5} can receive your screen at once, and dropping one leaves
   the others untouched.

   The SAME stream feeds screen-recorder (Track 4e), the archive tier."
  (:require [is.simm.uis.web.desktop.screen-recorder :as rec]
            [is.simm.uis.web.desktop.login :as login]
            [is.simm.uis.web.desktop.chat-remote :as chat-remote]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.uis.web.desktop.screen-sampling :as sampling]
            [is.simm.uis.web.desktop.screen-delivery :as delivery]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.runtimes.web :as web]
            [org.replikativ.spindel.engine.core :as rtc]))

(declare stop-all!)

;; The single capture session: {:stream :video :interval :last-sig :last-post} or nil.
(defonce ^:private capture (atom nil))
;; room-id → {:heartbeat interval-id} for every room currently granted.
(defonce ^:private grants (atom {}))

(def ^:private capture-interval-ms 2000) ;; cheap local inspection, not inference
(def ^:private grant-beat-ms 10000)  ;; keep the server-side grant window fresh

(defn sharing? [room-id] (contains? @grants room-id))
(defn any-active? [] (some? @capture))

;; ---------------------------------------------------------------------------
;; Frame sampling — one video, posted to the user stream
;; ---------------------------------------------------------------------------

(defn- frame-signature
  "Tiny grayscale thumbnail — cheap local frame fingerprint."
  [video]
  (let [c (js/document.createElement "canvas")
        w sampling/signature-width h sampling/signature-height]
    (set! (.-width c) w) (set! (.-height c) h)
    (let [ctx (.getContext c "2d")]
      (.drawImage ctx video 0 0 w h)
      (let [d (.-data (.getImageData ctx 0 0 w h))
            n (* w h)
            out (js/Array. n)]
        (dotimes [i n]
          (aset out i (+ (* 0.3 (aget d (* i 4)))
                         (* 0.6 (aget d (+ (* i 4) 1)))
                         (* 0.1 (aget d (+ (* i 4) 2))))))
        (vec out)))))

(defn- post-frame! [video]
  (let [vw (.-videoWidth video) vh (.-videoHeight video)]
    (if (pos? vw)
      ;; Keep text readable at near-native resolution; cadence controls cost.
      (let [scale (min 1 (js/Math.sqrt (/ 5500000 (* vw vh))))
            c (js/document.createElement "canvas")]
        (set! (.-width c) (js/Math.round (* vw scale)))
        (set! (.-height c) (js/Math.round (* vh scale)))
        (.drawImage (.getContext c "2d") video 0 0 (.-width c) (.-height c))
        (js/Promise.
         (fn [resolve reject]
           (.toBlob c
                    (fn [blob]
                      (if blob
                        (-> (js/fetch "/screen-frames"
                                      #js {:method "POST"
                                           :headers (login/auth-header-map
                                                      #js {"Content-Type" "image/jpeg"})
                                           :body blob})
                            (.then (fn [resp]
                                     (cond
                                       (= 200 (.-status resp)) (resolve true)
                                       (= 202 (.-status resp)) (resolve false)
                                       :else (reject (ex-info (str "HTTP " (.-status resp))
                                                              {:error (str "HTTP " (.-status resp))})))))
                            (.catch reject))
                        (resolve false)))
                    "image/jpeg" 0.85))))
      (js/Promise.resolve false))))

;; ---------------------------------------------------------------------------
;; Grants — open/heartbeat/close a room's window onto the running capture
;; ---------------------------------------------------------------------------

;; Grants are CONTROL-plane, so RPCs over distributed-scope — not HTTP glue.
;; From a DOM/interval callback the spin-remote needs the execution-context
;; bound, or it silently no-ops (feedback_spin_remote_ctx_binding).
(defn- rpc!
  "Call a grant RPC; `on-done` gets nil on success or the failure — a thrown
   one, or a refusal ANSWERED as `{:status :error …}` (not a member, unknown
   room), which used to count as success: the share button lit up while the
   room saw nothing."
  [rpc-fn room-id on-done]
  (binding [rtc/*execution-context* runtime]
    (let [s (rpc-fn web/server-id room-id)]
      (s (fn [r] (on-done (rem/result-failure r)))
         (fn [e] (on-done (or e (ex-info "grant call failed" {}))))))))

(declare stop!)

(defn- grant-open! [room-id on-done] (rpc! chat-remote/open-screen-grant! room-id on-done))

(defn- grant-beat!
  "Keep the grant alive. A refused heartbeat means the room no longer sees the
   screen: stop sharing into it and say so, rather than keep a lit button."
  [room-id]
  (rpc! chat-remote/screen-grant-heartbeat! room-id
        (fn [err]
          (when-let [{:keys [on-change]} (and err (get @grants room-id))]
            (rem/report-error! "Screen sharing into this room stopped." err)
            (stop! room-id on-change)))))

(defn- grant-close! [room-id]
  (rpc! chat-remote/close-screen-grant! room-id
        (fn [err]
          (when err
            (rem/report-error! "The room may still see your screen: closing the share failed." err)))))

;; ---------------------------------------------------------------------------
;; Capture lifecycle
;; ---------------------------------------------------------------------------

(defn- stop-capture! []
  (when-let [{:keys [stream interval]} @capture]
    (js/clearInterval interval)
    (rec/stop!)
    (doseq [t (.getTracks stream)] (.stop t))
    (reset! capture nil)
    (js/console.log "[screen-share] capture stopped")))

(defn- ensure-capture!
  "Resolve a promise once a capture session is running. Starts getDisplayMedia
   (browser picker) only if none is active; reuses the running stream otherwise.
   `on-fail` is called if the user denies the picker."
  [on-fail]
  (if @capture
    (js/Promise.resolve true)
    (-> (.getDisplayMedia js/navigator.mediaDevices
                          ;; 10fps: the recorder rides this same track.
                          #js {:video #js {:frameRate 10} :audio false})
        (.then
         (fn [stream]
           (let [video (js/document.createElement "video")
                 sampler (atom (sampling/initial (random-uuid)))]
             (set! (.-srcObject video) stream)
             (set! (.-muted video) true)
             (.play video)
             ;; the user can end the share from the browser's own bar → drop ALL grants
             (when-let [track (first (.getVideoTracks stream))]
               (set! (.-onended track) (fn [] (stop-all!))))
             (let [interval
                   (js/setInterval
                    (fn []
                      (when (>= (.-readyState video) 2)
                        (let [sig (frame-signature video)
                              prior @sampler
                              next (sampling/request prior (js/Date.now) sig)]
                          (when-not (identical? prior next)
                            (reset! sampler next)
                            (delivery/submit! #(:sampling @capture) (:pending next)
                                              #(post-frame! video)
                                              #(rem/report-error! "Your screen is not reaching the server." %))))))
                    capture-interval-ms)]
               (reset! capture {:stream stream :interval interval :video video :sampling sampler})
               ;; Archive tier — same stream, one encoder. Recording is ALWAYS on
               ;; while sharing: the raw video is training data for dynamic
               ;; computer-use models, and it is the owner's own archive (per-user,
               ;; playable and deletable in the Screens tab).
               (rec/start! stream)
               (js/console.log "[screen-share] capture started")
               true))))
        (.catch (fn [e]
                  (js/console.log "[screen-share] capture not started:" (.-message e))
                  (when on-fail (on-fail))
                  (js/Promise.reject e))))))

(defn stop!
  "Stop sharing into room-id: close its grant. If it was the last grant, stop the
   capture. `on-change` is called with false."
  [room-id on-change]
  (when-let [{:keys [heartbeat]} (get @grants room-id)]
    (js/clearInterval heartbeat))
  (grant-close! room-id)
  (swap! grants dissoc room-id)
  (when (empty? @grants) (stop-capture!))
  (js/console.log "[screen-share] stopped sharing into" room-id)
  (when on-change (on-change false)))

(defn stop-all!
  "Drop every grant and stop the capture (e.g. the user ended the OS share)."
  []
  (doseq [[room-id {:keys [heartbeat]}] @grants]
    (js/clearInterval heartbeat)
    (grant-close! room-id))
  (reset! grants {})
  (stop-capture!))

(defn start!
  "Share the screen into room-id: ensure one capture is running, then open (and
   heartbeat) a grant for this room. `on-change` is called with true once the
   grant is live (false on deny)."
  [room-id on-change]
  (when-not (sharing? room-id)
    (-> (ensure-capture! (fn [] (when on-change (on-change false))))
        (.then
         (fn [_]
           ;; Lit only once the server has opened the grant.
           (grant-open! room-id
                        (fn [err]
                          (if err
                            (do (rem/report-error! "Could not share your screen into this room." err)
                                (when (empty? @grants) (stop-capture!))
                                (when on-change (on-change false)))
                            (let [beat (js/setInterval #(grant-beat! room-id) grant-beat-ms)]
                              (swap! grants assoc room-id {:heartbeat beat :on-change on-change})
                              ;; A newly granted room needs an observation inside
                              ;; its own window even if the screen stays quiet.
                              ;; Keep the shared rate limit across all rooms.
                              (when-let [sampler (:sampling @capture)]
                                (swap! sampler sampling/refresh))
                              (when on-change (on-change true))))))))
        (.catch (fn [_] nil)))))
