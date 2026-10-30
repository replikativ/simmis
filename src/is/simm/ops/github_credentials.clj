(ns is.simm.ops.github-credentials
  "Fixed HOST credential storage and exact-destination transport. Secret values
   never enter Room files, preparation artifacts, receipts or diagnostics."
  (:require [datahike.api :as d]
            [geschichte.git.http :as http]
            [is.simm.documents.github-source :as github]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.github-credentials :as credentials]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.mail-credentials :as envelope]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.github-host :as host])
  (:import [java.nio.charset StandardCharsets]
           [java.util Base64]))

(defn- refuse! [reason]
  (throw (ex-info "GitHub credential operation refused" {:type ::refused :reason reason})))

(defn- identity! [context]
  (let [{:keys [cursor connection]} (host/connection! context)]
    {:by (get-in context [:resolved :party]) :connection-id (connections/connection-id connection)
     :connection connection :cursor cursor}))

(defn status!
  "Only this human's configured/revision status, never the encrypted envelope."
  [context]
  (let [{:keys [by connection-id connection]} (identity! context)
        row (credentials/current-in @(:conn context) by connection-id)]
    {:configured? (boolean (credentials/binding-of row)) :connection-id (str connection-id)
     :repository (get-in connection [:source :repository]) :ref (:ref connection)
     :target-stamp (str (get-in context [:bound :target-stamp]))
     :revision (some-> (:github.credential/revision row) str)}))

(defn- expected! [row revision]
  (when-not (= revision (:github.credential/revision row)) (refuse! :credential-changed)))

(defn save!
  "Called only by the authenticated original HOST, with an observed revision
   and one transient token. Reuses the server's existing authenticated envelope
   encryption, bound to a domain-separated credential UUID and identity body."
  [context expected-connection revision token]
  (when-not (and (or (nil? revision) (uuid? revision))
                 (string? token) (re-matches #"[A-Za-z0-9_]{20,512}" token))
    (refuse! :invalid-credential))
  (let [{:keys [by connection-id cursor]} (identity! context)
        _ (when-not (= expected-connection connection-id) (refuse! :connection-changed))
        row (credentials/current-in @(:conn context) by connection-id)
        _ (expected! row revision)
        id (credentials/id by connection-id)
        encrypted (envelope/encrypt id {:version 1 :kind :github :by by :connection-id connection-id :token token})
        next-revision (random-uuid)]
    (d/transact (:conn context)
                [[:db.fn/call
                  (fn [db]
                    (host/writer-current! context db)
                    (when-not (= cursor (integrations/current-in db (:github.integration/room cursor)))
                      (refuse! :connection-changed))
                    (credentials/replace-tx db by connection-id row next-revision encrypted))]])
    (status! context)))

(defn revoke! [context expected-connection revision]
  (let [{:keys [by connection-id]} (identity! context)
        _ (when-not (= expected-connection connection-id) (refuse! :connection-changed))
        row (credentials/current-in @(:conn context) by connection-id)]
    (expected! row revision)
    (d/transact (:conn context)
                [[:db.fn/call (fn [db] (host/writer-current! context db)
                                (credentials/revoke-tx db by connection-id row (random-uuid)))]])
    (status! context)))

(defn writer-current!
  "Additional dispatch admission for an authenticated transport context. No
   secret is captured by the control-writer callback. Internal unauthenticated
   fixture calls do not acquire a credential through this helper."
  [context db]
  (when-let [expected (:git-credential context)] (credentials/current! db expected)))

(defn with-transport!
  "Use this human's live credential for only the canonical connection URL and
   Git endpoints. The fixed callback is synchronous and cannot escape its
   original operation. No request-selected transport, token or actor."
  [context body]
  (let [{:keys [by connection-id connection]} (identity! context)
        row (credentials/current-in @(:conn context) by connection-id)
        expected (or (credentials/binding-of row) (refuse! :credential-required))
        secret (try (envelope/decrypt (credentials/id by connection-id)
                                      (:github.credential/nonce row) (:github.credential/ciphertext row))
                    (catch Exception _ (refuse! :credential-unavailable)))
        _ (when-not (and (= (select-keys secret [:version :kind :by :connection-id])
                            {:version 1 :kind :github :by by :connection-id connection-id})
                         (string? (:token secret)) (re-matches #"[A-Za-z0-9_]{20,512}" (:token secret)))
            (refuse! :credential-unavailable))
        base (github/public-url (:source connection))
        destinations #{[:get (str base "/info/refs?service=git-receive-pack")]
                       [:post (str base "/git-receive-pack")]
                       [:get (str base "/info/refs?service=git-upload-pack")]
                       [:post (str base "/git-upload-pack")]}
        authorization (str "Basic " (.encodeToString (Base64/getEncoder)
                                                     (.getBytes (str "x-access-token:" (:token secret)) StandardCharsets/UTF_8)))
        send! (fn [request]
                (host/current! context)
                (credentials/current! @(:conn context) expected)
                (when-not (destinations [(:method request) (:url request)]) (refuse! :destination-refused))
                (try
                  ;; Geschichte's default Java HTTP client never follows redirects.
                  (http/send-http (update request :headers assoc "Authorization" authorization))
                  (catch Exception error
                    (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
                    ;; Do not attach provider exception data or request headers.
                    (refuse! :request-failed))))]
    (body (assoc context :git-credential expected :git-send send!))))
