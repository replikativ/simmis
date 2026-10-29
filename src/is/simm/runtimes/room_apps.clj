(ns is.simm.runtimes.room-apps
  "Public committed room websites and separate authenticated worktree previews.
   App lifetime covers each read; scripts always execute in an opaque origin."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.repo :as repo]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.documents.files :as files]
            [ring.util.mime-type :as mime]
            [dvergr.web.apps :as apps]
            [is.simm.model.access :as access]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.http-auth :as auth]
            [org.replikativ.spindel.engine.core :as ec]
            [taoensso.telemere :as log]))

(def ^:private isolation-headers
  {"Content-Security-Policy" "sandbox allow-scripts" "Referrer-Policy" "no-referrer"
   "X-Content-Type-Options" "nosniff"})

(defn- response [status text]
  {:status status :headers {"Content-Type" "text/plain; charset=utf-8"} :body text})

(defn- authorize! [host party room-id]
  (when-not (and (uuid? room-id)
                 (access/can? (reads/current-host-control-db! host) party :read {:room room-id}))
    (throw (ex-info "Room app read refused" {:type ::denied}))))

(defn- unavailable? [error]
  (or (instance? java.io.IOException error)
      (contains? #{:is.simm.runtimes.application-ops/held
                   :is.simm.runtimes.operation-lifetime/refused
                   :is.simm.ops.owned-reads/refused} (:type (ex-data error)))))

(defn preview [request]
  (let [party (auth/party-id request)
        result
        (if-not party
          (response 401 "authentication required")
          (try
            (application/with-operation!
              (fn [host]
                (binding [ec/*execution-context* (:execution-ctx host)]
                  (let [served-room (volatile! nil)
                        result (or (apps/handle
                                    (update request :uri #(str/replace-first % #"^/apps-preview/" "/apps/")) nil
                                    (fn [row _slug]
                                      ;; Native resolution happens ONCE. A prechecked
                                      ;; slug cannot donate its room's authority.
                                      (authorize! host party (:room/id row))
                                      (vreset! served-room (:room/id row))))
                                   (response 404 "not found"))]
                    (reads/current-host-control-db! host)
                    (when-let [room @served-room] (authorize! host party room))
                    result))))
            (catch Throwable error
              (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
              (cond
                (= ::denied (:type (ex-data error))) (response 403 "room read refused")
                (unavailable? error) (do (log/log! {:level :warn :id ::read-unavailable :error error})
                                         (response 503 "room app unavailable"))
                :else (do (log/log! {:level :error :id ::serve-failed :error error})
                          (response 500 "room app failed"))))))]
    (update result :headers
            #(cond-> (merge % isolation-headers)
               (get % "Location") (update "Location" (fn [location]
                                                       (str/replace-first location #"^/apps/" "/apps-preview/")))))))


(def max-public-file-bytes (* 16 1024 1024))

(defn- published-source! [db slug]
  (let [row (d/entity db [:room/slug slug]) id (:room/id row)
        room (when (uuid? id) (room-agents/registered-room id))]
    ;; A fork's mutable world is a private preview, never a public deployment.
    (when-not (and room (nil? (:parent-id room)))
      (throw (ex-info "No published room" {:type ::not-found})))
    (let [binding (admission/default-binding db id room)]
      (assoc binding :room-id id :room room))))

(defn- public-path [uri]
  (let [[_ slug relative] (re-matches #"/apps/([^/]+)(?:/(.*))?" (or uri ""))
        decode #(java.net.URLDecoder/decode (str/replace % "+" "%2B") "UTF-8")
        slug (when slug (decode slug))
        relative (when relative (decode relative))]
    (when (or (str/blank? slug) (re-find #"[/\\\p{Cntrl}]" slug))
      (throw (ex-info "Invalid website path" {:type ::not-found})))
    (when relative
      (let [path (str "app/" (if (str/blank? relative) "index.html"
                                 (if (str/ends-with? relative "/") (str relative "index.html") relative)))]
        (when (some #(str/starts-with? % ".") (str/split path #"/"))
          (throw (ex-info "Private website path" {:type ::not-found})))
        (files/path! path)))
    {:slug slug :relative relative}))

(defn handle
  "Anonymous access publishes ONLY app/ from the canonical default repo's main
   commit. No bearer, worktree, branch selector, fallback repo or interpreter."
  [request]
  (let [result
        (try
          (if-not (#{:get :head} (:request-method request))
            (assoc (response 405 "GET and HEAD only") :headers {"Allow" "GET, HEAD"})
            (application/with-operation!
              (fn [host]
                (binding [ec/*execution-context* (:execution-ctx host)]
                  (let [{:keys [slug relative]} (public-path (:uri request))
                        db (reads/current-host-control-db! host)
                        source (published-source! db slug)]
                    (preparation/with-host-operation!
                      (:room source)
                      (fn [lease]
                        (preparation/with-host-current! lease (constantly true))
                        (let [;; This holder is immutable; reads cannot follow a
                              ;; concurrent checkout or mix two accepted versions.
                              snapshot (atom @(:conn source))
                              head (get (repo/refs snapshot) "refs/heads/main")
                              path (str "app/" (if (str/blank? relative) "index.html"
                                                   (if (str/ends-with? relative "/") (str relative "index.html") relative)))
                              entry (when head (get (repo/tree-at snapshot head) path))
                              result
                              (cond
                                (nil? relative) {:status 301 :headers {"Location" (str (:uri request) "/")} :body ""}
                                (or (nil? entry) (not (and (integer? (:mode entry)) (= 0100000 (bit-and 0170000 (:mode entry)))))) (response 404 "not found")
                                (> (:size entry) max-public-file-bytes) (response 413 "website asset exceeds size limit")
                                :else
                                (let [bytes (when (= :get (:request-method request)) (repo/read-at snapshot head path))]
                                  {:status 200
                                   :headers {"Content-Type" (or (mime/ext-mime-type path) "application/octet-stream")
                                             "Content-Length" (str (:size entry))
                                             "Cache-Control" "no-cache"
                                             "ETag" (str "\"" head "-" (:content entry) "\"")}
                                   :body (if bytes (java.io.ByteArrayInputStream. ^bytes bytes) "")}))
                              current (published-source! (reads/current-host-control-db! host) slug)]
                          (when-not (and (= (:room-id source) (:room-id current))
                                         (= (:registry-system source) (:registry-system current))
                                         (identical? (:conn source) (:conn current)))
                            (throw (ex-info "Published room mapping changed" {:type ::not-found})))
                          (preparation/with-host-current! lease (constantly true))
                          result))))))))
          (catch Throwable error
            (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
            (cond
              (or (= ::not-found (:type (ex-data error)))
                  (= :is.simm.documents.files/invalid (:type (ex-data error)))
                  (instance? IllegalArgumentException error)) (response 404 "not found")
              (unavailable? error) (response 503 "room app unavailable")
              :else (do (log/log! {:level :error :id ::public-serve-failed :error error})
                        (response 503 "room app unavailable")))))]
    ;; An opaque-origin script may fetch these already-public assets. Never
    ;; grant credentials or CORS access to Room APIs/private preview routes.
    (update result :headers merge isolation-headers {"Access-Control-Allow-Origin" "*"})))
