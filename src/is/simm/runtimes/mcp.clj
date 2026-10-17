(ns is.simm.runtimes.mcp
  "Opt-in, loopback-only MCP over the existing Simmis rooms. Credentials
   default to read-only; bounded work requires an explicit work credential.
   Neither exposes the raw database writer or eval.
   Credentials are short-lived, MCP-only capabilities for an existing human.
   The listener is a host resource, not forkable application state."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [dvergr.mcp.http :as http]
            [dvergr.mcp.server :as server]
            [dvergr.substrate.config :as config]
            [is.simm.model.parties :as parties]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.results :as results]
            [is.simm.ops.work-api :as work-api]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.http-auth :as http-auth]
            [is.simm.runtimes.instance :as instance]
            [kabel.auth.jwt :as jwt]
            [taoensso.telemere :as log]))

(def audience "simmis-mcp")
(def legacy-read-tools
  "The immutable ceiling of credentials issued before explicit capabilities."
  #{"room_list" "room_detail" "room_messages" "run_list" "run_detail"})

(def read-tools
  "Deliberately selected room/workflow/result reads, not all upstream reads.
   Workspace reads are current files; Attempt evidence is a recorded result.
   Excludes host-path exports, raw evaluation, writes and agent administration."
  (into legacy-read-tools
        #{"room_files" "room_read" "room_diff" "room_review" "room_wallet"
          "job_list" "job_status" "attempt_list" "attempt_detail" "review_list"
          "scorecard_list" "scorecard_detail" "experiment_progress" "experiment_report"
          "catalog_list" "simmis_attempt-files" "simmis_attempt-file"}))

(def work-tools
  (into read-tools #{"simmis_post" "simmis_task" "simmis_work-allowance"
                     "simmis_work-status" "job_cancel"
                     "simmis_workflow-write" "simmis_workflow-check" "simmis_benchmark"}))

(defn- credential-tools [claims]
  (if (and (not (contains? claims :capability-version))
           (not (contains? claims :tools)))
    legacy-read-tools
    (let [tools (:tools claims)
          ceiling (case (:capability-version claims)
                    1 read-tools
                    2 (when (= "work" (:mode claims)) work-tools)
                    nil)]
      (when (and ceiling
                 (vector? tools) (seq tools) (<= (count tools) (count ceiling))
                 (every? string? tools) (set/subset? (set tools) ceiling))
        (set tools)))))

(defn- signing-key []
  (let [mac (javax.crypto.Mac/getInstance "HmacSHA256")]
    (.init mac (javax.crypto.spec.SecretKeySpec.
                (.getBytes ^String (get-in (auth/get-auth-config) [:jwt :secret]) "UTF-8")
                "HmacSHA256"))
    (.doFinal mac (.getBytes "simmis/mcp/v1" "UTF-8"))))

(defn- human? [party-id]
  (= :human (:party/type (parties/get-party party-id))))

(defn mint-token!
  "Trusted issuer: a one-hour MCP credential for an existing human party.
   Used by the operator REPL and the authenticated self-exchange route, never
   a tool. Returns {:token :expires-in}; never log it. Creates no grant."
  ([party-id] (mint-token! party-id :read))
  ([party-id mode]
   (when-not (and (uuid? party-id) (human? party-id))
     (throw (ex-info "MCP credentials require an existing human party" {:type ::invalid-party})))
   (when-not (contains? #{:read :work} mode)
     (throw (ex-info "Unknown MCP access mode" {:type ::invalid-mode})))
   (let [now (quot (System/currentTimeMillis) 1000)
         claims (cond-> {:iss audience :aud audience :sub (str party-id)
                         :iat now :exp (+ now 3600)
                         :capability-version (if (= :work mode) 2 1)
                         :tools (vec (sort (if (= :work mode) work-tools read-tools)))}
                  (= :work mode) (assoc :mode "work"))]
     {:token (jwt/sign-hs256 (signing-key) claims)
      :expires-in 3600 :mode (name mode)})))

(defn authenticator
  "Build the transport authenticator. Ordinary web, blob, local-admin and
   Dvergr run credentials are not accepted by this initial human-read surface.
   Recheck the party each request, so deleting it revokes access."
  []
  (let [validate (jwt/build-bearer-validator
                  {:alg :HS256 :secret (signing-key) :leeway-seconds 0
                   :required-claims {:iss audience :aud audience}})]
    (fn [{:keys [bearer]}]
      (try
        (when (and (string? bearer) (not (str/blank? bearer)))
          (when-let [claims (validate {:headers {"authorization" (str "Bearer " bearer)}})]
            (when (and (integer? (:iat claims)) (integer? (:exp claims))
                       (<= 1 (- (:exp claims) (:iat claims)) 3600))
              (when-let [tools (credential-tools claims)]
                (when-let [id (some-> (:sub claims) str parse-uuid)]
                  (when (human? id)
                    (assoc (ops/party-principal id)
                           :principal/mcp? true
                           :principal/scopes (if (set/subset? tools read-tools)
                                               #{:dvergr/read-only} #{})
                           ;; A hard ceiling even when a client requests admin profile.
                           :principal/tools tools)))))))
        (catch Exception _ nil)))))

(defonce ^:private listener (atom nil))

(defn configured-port
  "The opt-in MCP base port, plus the instance offset; nil means disabled."
  []
  (when-let [value (or (some-> (System/getenv "SIMMIS_MCP_PORT") str/trim not-empty)
                       (get-in (config/config) [:mcp :port]))]
    (let [port (+ (Long/parseLong (str value)) (instance/port-offset))]
      (when-not (<= 1 port 65535)
        (throw (ex-info "MCP port must be between 1 and 65535" {:type ::invalid-port})))
      port)))

(defn token-route
  "Exchange a validated web bearer for a one-hour MCP token (read by default).
   JSON {\"mode\":\"work\"} explicitly opts into bounded work; old sessions
   and credentials do not silently gain writes.
   Identity comes only from authentication, never from the request body.
   This issues credentials; the MCP transport itself remains loopback-only."
  []
  ["/mcp-token"
   {:post {:auth :authenticated
           :handler
           (fn [req]
             (let [id (http-auth/party-id req)
                   body (:body req)
                   mode (get body :mode "read")
                   headers {"Content-Type" "application/json"
                            "Cache-Control" "no-store"}]
               (cond
                 (nil? id) {:status 401 :headers headers
                            :body {:error "authentication required"}}
                 (not (human? id)) {:status 403 :headers headers
                                    :body {:error "an existing human account is required"}}
                 (nil? (configured-port)) {:status 404 :headers headers
                                           :body {:error "private MCP is disabled"}}
                 (not (and (or (nil? body) (map? body))
                           (every? #{:mode} (keys body))
                           (contains? #{"read" "work"} mode)))
                 {:status 400 :headers headers :body {:error "invalid MCP access mode"}}
                 :else {:status 200 :headers headers
                        :body (assoc (mint-token! id (keyword mode)) :token-type "Bearer")})))}}])

(defn start!
  "Start the configured listener once. The bind is not configurable: SSH
   forwarding is the supported access path for this private first surface."
  []
  (locking listener
    (when-not @listener
      (when-let [port (configured-port)]
        (let [authenticate (authenticator)]
          (server/set-host! (ops/host context/server-context))
          (try
            (results/install!)
            (work-api/install!)
            (let [started (http/start! {:bind "127.0.0.1" :port port
                                        :profile :admin :toolsets #{:host}
                                        :authenticate authenticate})]
              ;; Never retain/log the transport's unused local-admin token.
              (reset! listener (select-keys started [:port :stop]))
              (log/log! {:level :info :id ::started
                         :data {:bind "127.0.0.1" :port port :default-access :read}}))
            (catch Exception e
              (results/uninstall!)
              (work-api/uninstall!)
              (server/set-host! nil)
              (throw e))))))))

(defn stop! []
  (locking listener
    (when-let [{:keys [stop]} @listener]
      (stop)
      (reset! listener nil)
      (results/uninstall!)
      (work-api/uninstall!)
      (server/set-host! nil))))
