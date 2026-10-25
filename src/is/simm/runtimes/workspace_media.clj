(ns is.simm.runtimes.workspace-media
  "Read-only media from the currently authorized canonical room workspace.
   Content IDs are preconditions, never capabilities."
  (:require [dvergr.runtime.preparation :as preparation]
            [geschichte.content :as content]
            [is.simm.documents.files :as files]
            [is.simm.documents.media-range :as ranges]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.http-auth :as auth]
            [is.simm.runtimes.media-cache :as cache]
            [org.replikativ.spindel.engine.core :as ec]
            [taoensso.telemere :as log]))

(defn- refuse! [reason]
  (throw (ex-info "Workspace media refused" {:type ::refused :reason reason})))

(defn- metadata! [binding path expected-content expected-target]
  (when-not (= expected-target (:target-stamp binding)) (refuse! :changed))
  (let [metadata (files/media-metadata-in (:db binding) path)]
    (when-not (= expected-content (get-in metadata [:entry :content])) (refuse! :changed))
    (when (> (get-in metadata [:entry :size]) Integer/MAX_VALUE) (refuse! :too-large))
    metadata))

(defn response-in
  "Internal transport projection under an already admitted App/Room extent.
   HEAD and unsatisfiable ranges do not materialize content."
  [resource binding path expected-content expected-target request]
  (let [metadata (metadata! binding path expected-content expected-target)
        size (get-in metadata [:entry :size]) etag (str "\"" expected-content "\"")
        {:keys [status offset length content-range]}
        (ranges/plan size (:request-method request) (get-in request [:headers "range"])
                     (get-in request [:headers "if-range"]) etag)
        headers (cond-> {"Content-Type" (get-in metadata [:media :content-type])
                         "Content-Length" (str length) "Accept-Ranges" "bytes" "ETag" etag
                         "Cache-Control" "private, no-store"
                         "X-Content-Type-Options" "nosniff"
                         "Content-Security-Policy" "sandbox"
                         "Referrer-Policy" "no-referrer"}
                  content-range (assoc "Content-Range" content-range))]
    {:status status :headers headers
     :body (when (and (not= :head (:request-method request)) (not= 416 status))
             (cache/slice (cache/materialize! resource expected-content size
                                              #(content/copy-by-id! (:conn binding) expected-content %))
                          offset length))}))

(defn- serve! [host principal room-handle path expected-content expected-target request]
  (let [resolved (reads/resolve-host-read-room! host principal room-handle)
        room (:room resolved)]
    (binding [ec/*execution-context* (:execution-ctx host)]
      (preparation/with-host-operation!
        room
        (fn [lease]
          (preparation/with-host-current! lease (constantly true))
          (reads/validate-host-read-room! host principal room-handle resolved)
          (let [binding (reads/resolve-host-read! host principal room-handle)]
            (when-not (identical? room (:room binding)) (refuse! :changed))
            (let [response (clojure.core/binding [ec/*execution-context* (:ctx room)]
                             (response-in (:media-cache host) binding path expected-content expected-target request))
                  current (reads/validate-host-read-room! host principal room-handle resolved)
                  verified (owned/validate! (:control-db current) principal :read (:room-id binding) room binding)]
              (metadata! verified path expected-content expected-target)
              (preparation/with-host-current! lease (constantly response)))))))))

(defn handle [request]
  (let [party (auth/blob-reader-party-id request)
        {:strs [path content target]} (:query-params request)
        expected-content (when (string? content) (parse-uuid content))
        expected-target (when (string? target) (parse-uuid target))]
    (cond
      (nil? party) {:status 401 :body "authentication required"}
      (not (#{:get :head} (:request-method request))) {:status 405 :body "read only"}
      (not (and expected-content expected-target (string? path))) {:status 400 :body "invalid media reference"}
      :else
      (try
        (application/with-operation!
          #(serve! % {:principal/kind :human :principal/id (str "party/" party)}
                   (get-in request [:path-params :room]) path expected-content expected-target request))
        (catch Throwable error
          (when (instance? InterruptedException error) (.interrupt (Thread/currentThread)))
          (log/log! {:level :warn :id ::read-refused :error error})
          ;; Do not disclose whether a denied room contains a guessed path/hash.
          {:status 409 :headers {"Cache-Control" "private, no-store"}
           :body "Media unavailable; reopen the file in its current workspace."})))))
