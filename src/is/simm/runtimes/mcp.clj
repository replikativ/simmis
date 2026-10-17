(ns is.simm.runtimes.mcp
  "Opt-in, loopback-only MCP over the existing Simmis rooms. Credentials
   default to read-only; bounded work requires an explicit work credential.
   Neither exposes the raw database writer or eval.
   Credentials are short-lived, MCP-only capabilities for an existing human.
   The listener is a host resource, not forkable application state."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [dvergr.mcp.http :as http]
            [dvergr.substrate.config :as config]
            [is.simm.model.parties :as parties]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.worker-credentials :as worker-credentials]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.http-auth :as http-auth]
            [is.simm.runtimes.instance :as instance]
            [kabel.auth.jwt :as jwt]
            [org.replikativ.spindel.engine.core :as ec]
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
          "catalog_list" "simmis_attempt-files" "simmis_attempt-file"
          "simmis_workspace-bundle" "simmis_workspace-bundle-chunk"}))

(def work-tools
  (into read-tools #{"simmis_post" "simmis_task" "simmis_work-allowance"
                     "simmis_work-status" "job_cancel"
                     "simmis_workflow-write" "simmis_workflow-check" "simmis_workflow-run"
                     "simmis_workflow-list" "simmis_workflow-source"
                     "simmis_benchmark"}))

(def worker-control-tools
  ;; Private operator-helper session only. Never widen existing work tokens.
  (into read-tools #{"job_cancel" "simmis_work-status"
                     "simmis_local-worker-list"
                     "simmis_local-worker-enroll" "simmis_local-worker-revoke"
                     "simmis_local-worker-allowance" "simmis_local-worker-task"
                     "simmis_local-worker-workflow" "simmis_local-worker-benchmark"
                     "simmis_workflow-list" "simmis_workflow-source" "simmis_workflow-check"}))

(defn- credential-tools [claims]
  (if (and (not (contains? claims :capability-version))
           (not (contains? claims :tools)))
    legacy-read-tools
    (let [tools (:tools claims)
          ceiling (case (:capability-version claims)
                    1 read-tools
                    2 (when (= "work" (:mode claims)) work-tools)
                    3 (when (= "worker-control" (:mode claims)) worker-control-tools)
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
   (when-not (contains? #{:read :work :worker-control} mode)
     (throw (ex-info "Unknown MCP access mode" {:type ::invalid-mode})))
   (let [now (quot (System/currentTimeMillis) 1000)
         [version tools] (case mode :read [1 read-tools] :work [2 work-tools]
                               :worker-control [3 worker-control-tools])
         claims (cond-> {:iss audience :aud audience :sub (str party-id)
                         :iat now :exp (+ now 3600)
                         :capability-version version :tools (vec (sort tools))}
                  (not= :read mode) (assoc :mode (name mode)))]
     {:token (jwt/sign-hs256 (signing-key) claims)
      :expires-in 3600 :mode (name mode)})))

(defn- human-authenticator
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

(defn authenticator
  "Disjoint credential-format dispatch. A recognized worker prefix NEVER
   falls through to human/admin auth, including expired/revoked/bad bearers.
   Worker shutdown transport remains available when new admission is disabled."
  []
  (let [human (human-authenticator)]
    (fn [{:keys [bearer] :as request}]
      (if (and (string? bearer)
               (or (str/starts-with? bearer "simmis-worker.")
                   (str/starts-with? bearer "simmis-lease.")))
        (try
          (binding [ec/*execution-context* context/server-context]
            (worker-credentials/authenticate (system-db/get-conn) request))
          (catch Exception _ nil))
        (human request)))))

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
                           (contains? #{"read" "work" "worker-control"} mode)))
                 {:status 400 :headers headers :body {:error "invalid MCP access mode"}}
                 :else {:status 200 :headers headers
                        :body (assoc (mint-token! id (keyword mode)) :token-type "Bearer")})))}}])

(defn start!
  "Start the configured listener once. The bind is not configurable: SSH
   forwarding is the supported access path for this private first surface.
   Application operation registrations are owned by web application startup."
  []
  (locking listener
    (when-not @listener
      (when-let [port (configured-port)]
        (let [authenticate (authenticator)
              started (http/start! {:bind "127.0.0.1" :port port
                                    :profile :admin :toolsets #{:host}
                                    :authenticate authenticate})]
          ;; Never retain/log the transport's unused local-admin token.
          (reset! listener (select-keys started [:port :stop]))
          (log/log! {:level :info :id ::started
                     :data {:bind "127.0.0.1" :port port :default-access :read}}))))))

(defn stop! []
  (locking listener
    (when-let [{:keys [stop]} @listener]
      (stop)
      (reset! listener nil))))
