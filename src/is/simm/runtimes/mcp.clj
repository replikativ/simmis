(ns is.simm.runtimes.mcp
  "Opt-in, loopback-only MCP over the existing Simmis rooms. This first
   surface is read-only; it does not expose the raw database writer or eval.
   Credentials are short-lived, MCP-only capabilities for an existing human.
   The listener is a host resource, not forkable application state."
  (:require [clojure.string :as str]
            [dvergr.mcp.http :as http]
            [dvergr.mcp.server :as server]
            [dvergr.substrate.config :as config]
            [is.simm.model.parties :as parties]
            [is.simm.ops.dvergr :as ops]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.instance :as instance]
            [kabel.auth.jwt :as jwt]
            [taoensso.telemere :as log]))

(def audience "simmis-mcp")
(def read-tools #{"room_list" "room_detail" "room_messages" "run_list" "run_detail"})

(defn- signing-key []
  (let [mac (javax.crypto.Mac/getInstance "HmacSHA256")]
    (.init mac (javax.crypto.spec.SecretKeySpec.
                 (.getBytes ^String (get-in (auth/get-auth-config) [:jwt :secret]) "UTF-8")
                 "HmacSHA256"))
    (.doFinal mac (.getBytes "simmis/mcp/v1" "UTF-8"))))

(defn- human? [party-id]
  (= :human (:party/type (parties/get-party party-id))))

(defn mint-token!
  "Operator REPL only: a one-hour MCP credential for an existing human party.
   Returns {:token :expires-in}. Never log the result or expose this as a tool.
   No account, membership or grant is created."
  [party-id]
  (when-not (and (uuid? party-id) (human? party-id))
    (throw (ex-info "MCP credentials require an existing human party" {:type ::invalid-party})))
  (let [now (quot (System/currentTimeMillis) 1000)]
    {:token (jwt/sign-hs256 (signing-key)
                           {:iss audience :aud audience :sub (str party-id)
                            :iat now :exp (+ now 3600)})
     :expires-in 3600}))

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
              (when-let [id (some-> (:sub claims) str parse-uuid)]
                (when (human? id)
                  (assoc (ops/party-principal id)
                         :principal/scopes #{:toolset/rooms :toolset/runs}
                         ;; A hard ceiling even when a client requests admin profile.
                         :principal/tools read-tools))))))
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
            (let [started (http/start! {:bind "127.0.0.1" :port port
                                        :profile :readonly :authenticate authenticate})]
              ;; Never retain/log the transport's unused local-admin token.
              (reset! listener (select-keys started [:port :stop]))
              (log/log! {:level :info :id ::started
                         :data {:bind "127.0.0.1" :port port :read-only? true}}))
            (catch Exception e
              (server/set-host! nil)
              (throw e))))))))

(defn stop! []
  (locking listener
    (when-let [{:keys [stop]} @listener]
      (stop)
      (reset! listener nil)
      (server/set-host! nil))))
