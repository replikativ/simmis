(ns is.simm.uis.web.desktop.result-actions
  "On-demand result I/O. UI state stays in the caller's execution context."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.results-remote :as remote]
            [is.simm.uis.web.desktop.captured-result :as result]
            [is.simm.runtimes.web :as web]))

(defn state-key [room run attempt]
  [(:id (web/current-session)) (str room) (str run) (str attempt)])

(defn- bytes! [chunk]
  (let [source (:base64 chunk) s (js/atob source)]
    (when-not (= source (js/btoa s)) (throw (js/Error. "Invalid recorded bytes")))
    (js/Uint8Array.from s (fn [c] (.charCodeAt c 0)))))

(defn- request! [k slot invoke consume & [preserve?]]
  (let [ctx (or ec/*execution-context* runtime)
        session (web/current-session)
        token (str (random-uuid))
        current? (fn [] (and (web/current-session? session)
                             (= token (get-in @sig/captured-results [k slot :request]))))
        update! (fn [f] (binding [ec/*execution-context* ctx]
                          (when (current?) (swap! sig/captured-results update-in [k slot] f))))]
    (binding [ec/*execution-context* ctx]
      (when-let [url (get-in @sig/captured-results [k slot :url])]
        (js/URL.revokeObjectURL url))
      (swap! sig/captured-results update-in [k slot]
             #(merge (when preserve? (dissoc % :error :url)) {:request token :loading? true}))
      (letfn [(fail! [_] (update! #(assoc % :loading? false :error "Could not read this recorded result. Check room access and try again.")))
              (step! [offset]
                (when (binding [ec/*execution-context* ctx] (current?))
                  (try
                    (let [s (invoke offset)]
                      (s (fn [reply]
                           (binding [ec/*execution-context* ctx]
                             (when (current?)
                               (try (consume reply offset update! step!) (catch :default e (fail! e))))))
                         fail!))
                    (catch :default e (fail! e)))))]
        (step! 0)))))

(defn manifest! [room run attempt]
  (let [k (state-key room run attempt)]
    (request! k :manifest
              #(remote/load-attempt-files! web/server-id (str room) (str attempt) %)
              (fn [page _ update! _]
                (when-not (= (str attempt) (get-in page [:attempt :id])) (throw (js/Error. "Wrong Attempt")))
                (update! #(assoc % :loading? false :data page))))))

(defn manifest-page! [room run attempt offset]
  ;; Pagination is explicit. Preserve prior pages while fetching the next one.
  (binding [ec/*execution-context* (or ec/*execution-context* runtime)]
    (let [k (state-key room run attempt)
          prior (get-in @sig/captured-results [k :manifest :data])]
      (request! k :manifest
                (fn [_] (remote/load-attempt-files! web/server-id (str room) (str attempt) offset))
                (fn [page _ update! _]
                  (when-not (and (= (str attempt) (get-in page [:attempt :id])) (= offset (:offset page)))
                    (throw (js/Error. "Wrong result page")))
                  (update! #(assoc % :loading? false :data (update page :files (fn [xs] (into (:files prior) xs))))))
                true))))

(defn preview-text
  "Decode either legacy text or descriptor-backed bytes as inert UTF-8.
   Drop an incomplete final codepoint in a bounded preview; reject invalid
   sequences and binary control characters. No filename/MIME trust needed."
  [bytes partial?]
  (try
    (let [text (.decode (js/TextDecoder. "utf-8" #js {:fatal true}) bytes #js {:stream (boolean partial?)})]
      (when-not (re-find #"[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]" text) text))
    (catch :default _ nil)))

(defn preview! [room run attempt entry]
  (request! (state-key room run attempt) :preview
            #(remote/load-attempt-file! web/server-id (str room) (str attempt) (:path entry) %)
            (fn [chunk offset update! _]
              (let [bytes (bytes! chunk)]
                (when-not (result/chunk-valid? (str attempt) entry offset chunk (.-length bytes))
                  (throw (js/Error. "Invalid recorded chunk")))
                (let [text (preview-text bytes (:next-offset chunk))]
                  (update! #(assoc % :loading? false :path (:path entry) :text text
                                   :truncated? (some? (:next-offset chunk)))))))))

(defn publish-download!
  "Browser I/O boundary, called only after the complete byte stream validates.
   The returned URL belongs to the authenticated result view until replaced."
  [parts path]
  (let [blob (js/Blob. parts #js {:type "application/octet-stream"})
        url (js/URL.createObjectURL blob)
        a (.createElement js/document "a")]
    (try
      (set! (.-href a) url)
      (set! (.-download a) (result/download-name path))
      (.appendChild (.-body js/document) a)
      (.click a)
      url
      (catch :default e (js/URL.revokeObjectURL url) (throw e))
      (finally (.remove a)))))

(defn download! [room run attempt entry]
  ;; Partial bytes never become a download. Every chunk is separately authorized
  ;; and late replies cannot publish into a new account/session or selection.
  (let [parts (array)]
    (request! (state-key room run attempt) :download
              #(remote/load-attempt-file! web/server-id (str room) (str attempt) (:path entry) %)
              (fn [chunk offset update! next!]
                (let [bytes (bytes! chunk)]
                  (when-not (result/chunk-valid? (str attempt) entry offset chunk (.-length bytes))
                    (throw (js/Error. "Invalid recorded chunk")))
                  (.push parts bytes)
                  (if-let [offset (:next-offset chunk)]
                    (next! offset)
                    (let [url (publish-download! parts (:path entry))]
                      (when-not (update! #(assoc % :loading? false :path (:path entry) :complete? true :url url))
                        (js/URL.revokeObjectURL url)))))))))
