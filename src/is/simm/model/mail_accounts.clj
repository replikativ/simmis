(ns is.simm.model.mail-accounts
  "Briefkasten-backed mail knowledge sources.

   Each account is a `:system/type :mail` registry resource with its own
   Briefkasten Datahike/Lucene/CAS state. IMAP owns folders, messages and flags;
   Simmis owns account visibility, room grants and future annotations."
  (:require [is.simm.runtimes.instance :as instance]
            [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.substrate.config :as dcfg]
            [dvergr.system.db :as sdb]
            [is.simm.model.mail-credentials :as credentials]
            [is.simm.model.system-db :as system-db]
            [org.replikativ.briefkasten.core :as briefkasten]
            [org.replikativ.briefkasten.imap :as imap]
            [taoensso.telemere :as log])
  (:import [java.util Date UUID]
           [java.util.concurrent Executors ScheduledExecutorService ThreadFactory TimeUnit]))

(defonce ^:private handles (atom {}))
(defonce ^:private workers (atom {}))
(def ^:private default-sync-seconds 300)

(defn- account-key [account-id] (keyword "simmis.mail" (str account-id)))
(defn- mail-root [] (or (System/getenv "SIMMIS_MAIL_DATA_DIR") (instance/data-path "simmis-mail")))
(defn- data-path [account-id] (str (mail-root) "/" account-id))

(defn- public-account [account]
  (when account
    (-> account
        (dissoc :db/id :mail-account/secret-nonce :mail-account/secret-ciphertext)
        (assoc :mail-account/credentials-configured?
               (boolean (:mail-account/secret-ciphertext account)))
        ;; ISO-8601, not Date/toString (which browsers cannot parse) — the
        ;; same encoding as `is.simm.uis.web.desktop.remote/wire-instant`.
        (update :mail-account/created #(some-> ^Date % .toInstant str))
        (update :mail-account/last-sync #(some-> ^Date % .toInstant str)))))

(defn- require-account [account-id]
  (or (when-let [conn (and (some? account-id) (system-db/get-conn))]
        (d/q '[:find (pull ?a [*]) . :in $ ?id
               :where [?a :mail-account/id ?id]] @conn account-id))
      (throw (ex-info "Unknown mail account" {:mail-account/id account-id}))))

(defn get-account [account-id] (some-> (require-account account-id) public-account))

(defn list-accounts
  "List public account metadata. With owner-id, only that owner's accounts."
  [& [owner-id]]
  (when-let [conn (system-db/get-conn)]
    (->> (if owner-id
           (d/q '[:find [(pull ?a [*]) ...] :in $ ?owner
                  :where [?a :mail-account/owner ?owner]] @conn owner-id)
           (d/q '[:find [(pull ?a [*]) ...]
                  :where [?a :mail-account/id _]] @conn))
         (mapv public-account))))

(defn owned-by? [account-id party-id]
  (= party-id (:mail-account/owner (require-account account-id))))

(defn- validate-config! [{:keys [email imap]}]
  (when-not (and (string? email) (str/includes? email "@"))
    (throw (ex-info "A valid account email is required" {:field :email})))
  (doseq [[field pred message]
          [[:host #(and (string? %) (not (str/blank? %))) "IMAP host is required"]
           [:port #(and (integer? %) (<= 1 % 65535)) "IMAP port must be between 1 and 65535"]
           [:user #(and (string? %) (not (str/blank? %))) "IMAP user is required"]
           [:pass #(and (string? %) (not (str/blank? %))) "IMAP password is required"]]]
    (when-not (pred (get imap field))
      (throw (ex-info message {:field (keyword "imap" (name field))}))))
  true)

(defn test-connection!
  "Verify credentials and return the server's folder names without persisting."
  [config]
  (validate-config! config)
  (let [store (imap/connect! (:imap config))]
    (try
      {:success true :folders (imap/list-folders store)}
      (finally (imap/disconnect! store)))))

(defn- secret-config [account]
  (credentials/decrypt (:mail-account/id account)
                       (:mail-account/secret-nonce account)
                       (:mail-account/secret-ciphertext account)))

(defn open-account!
  "Open (or reuse) the Briefkasten handle for a persisted account."
  [account-id]
  (or (get @handles account-id)
      (locking handles
        (or (get @handles account-id)
            (let [account (require-account account-id)
                  secret (secret-config account)
                  handle (briefkasten/create-account!
                          (assoc secret
                                 :id (account-key account-id)
                                 :uuid account-id
                                 :email (:mail-account/email account)
                                 :data-path (data-path account-id)))]
              (swap! handles assoc account-id handle)
              handle)))))

(defn- set-sync-state! [account-id status & [error]]
  (when-let [conn (system-db/get-conn)]
    (let [eid [:mail-account/id account-id]
          current (:mail-account/error (require-account account-id))
          tx (cond-> [[:db/add eid :mail-account/status status]]
               (= status :ready) (conj [:db/add eid :mail-account/last-sync (Date.)])
               current (conj [:db/retract eid :mail-account/error current])
               error (conj [:db/add eid :mail-account/error (str error)]))]
      (d/transact conn tx))))

(defn sync-now!
  "Run one authoritative IMAP → Briefkasten projection."
  [account-id]
  (let [account (require-account account-id)
        folders (seq (:mail-account/folders account))]
    (set-sync-state! account-id :syncing)
    (try
      (briefkasten/sync! (open-account! account-id) :folders folders)
      (set-sync-state! account-id :ready)
      {:success true}
      (catch Throwable t
        (set-sync-state! account-id :error (.getMessage t))
        (log/log! {:level :error :id ::mail-sync-failed
                   :data {:account-id account-id} :error t})
        (throw t)))))

(defn- daemon-factory [account-id]
  (reify ThreadFactory
    (newThread [_ runnable]
      (doto (Thread. runnable (str "simmis-mail-sync-" account-id))
        (.setDaemon true)))))

(defn start-sync!
  "Start one supervised, fixed-delay worker per account. Idempotent."
  ([account-id] (start-sync! account-id default-sync-seconds))
  ([account-id interval-seconds]
   (or (get @workers account-id)
       (let [executor (Executors/newSingleThreadScheduledExecutor
                       (daemon-factory account-id))
             task #(try (sync-now! account-id)
                        (catch Throwable _))]
         (.scheduleWithFixedDelay ^ScheduledExecutorService executor
                                  ^Runnable task 0 interval-seconds TimeUnit/SECONDS)
         (get (swap! workers #(if (contains? % account-id)
                                % (assoc % account-id executor))) account-id)))))

(defn stop-sync! [account-id]
  (when-let [^ScheduledExecutorService executor (get @workers account-id)]
    (.shutdownNow executor)
    (swap! workers dissoc account-id))
  true)

(defn save-account!
  "Create or update an owner's account. The complete config is encrypted; the
   returned map contains no host, username, password, nonce or ciphertext."
  [owner-id {:keys [id name email folders] :as config}]
  (validate-config! config)
  (let [account-id (or id (random-uuid))
        display-name (or (some-> name str/trim not-empty) email)
        existing (when id (require-account id))
        _ (when (and existing (not= owner-id (:mail-account/owner existing)))
            (throw (ex-info "Only the account owner may change its connection"
                            {:mail-account/id id})))
        db-scope (or (:mail-account/db-scope existing) account-id)
        system-id (or (:mail-account/system-id existing)
                      (sdb/register-system! {:type :mail
                                             :name display-name
                                             :scope (str db-scope)
                                             :owner-id owner-id}))
        {:keys [nonce ciphertext]} (credentials/encrypt account-id
                                                        (select-keys config [:email :imap :smtp]))
        entity {:mail-account/id account-id
                :mail-account/name display-name
                :mail-account/email email
                :mail-account/owner owner-id
                :mail-account/created (or (:mail-account/created existing) (Date.))
                :mail-account/db-scope db-scope
                :mail-account/system-id system-id
                :mail-account/status :configured
                :mail-account/secret-nonce nonce
                :mail-account/secret-ciphertext ciphertext}
        conn (or (system-db/get-conn)
                 (throw (ex-info "System DB is not initialized" {})))
        old-folders (set (:mail-account/folders existing))
        new-folders (set (remove str/blank? folders))
        folder-retracts (mapv #(vector :db/retract [:mail-account/id account-id]
                                       :mail-account/folders %) old-folders)
        folder-adds (mapv #(vector :db/add [:mail-account/id account-id]
                                   :mail-account/folders %) new-folders)]
    (stop-sync! account-id)
    (when-let [handle (get @handles account-id)]
      (briefkasten/close! handle)
      (swap! handles dissoc account-id))
    (d/transact conn (into [entity] (concat folder-retracts folder-adds)))
    (start-sync! account-id)
    (public-account (require-account account-id))))

(defn list-folders [account-id]
  (briefkasten/list-folders (open-account! account-id)))

(defn list-messages [account-id folder {:keys [limit offset]
                                        :or {limit 50 offset 0}}]
  (briefkasten/list-messages (open-account! account-id) folder
                             :limit limit :offset offset))

(defn read-message [account-id folder uid]
  (briefkasten/read-message (open-account! account-id)
                            {:folder folder :uid uid}))

(defn search [account-id query {:keys [limit] :or {limit 50}}]
  (briefkasten/search (open-account! account-id) query :limit limit))

(defn attach-to-room!
  ([room-id account-id] (attach-to-room! room-id account-id :read))
  ([room-id account-id permission]
   (sdb/attach! room-id (:mail-account/system-id (require-account account-id)) permission)))

(defn detach-from-room! [room-id account-id]
  (sdb/detach! room-id (:mail-account/system-id (require-account account-id))))

(defn close-all! []
  (doseq [account-id (keys @workers)] (stop-sync! account-id))
  (doseq [[_ handle] @handles] (briefkasten/close! handle))
  (reset! handles {})
  true)

(defn start-all!
  "Hydrate supervisors for every configured account after a server restart."
  []
  (doseq [{:mail-account/keys [id]} (list-accounts)]
    (start-sync! id))
  true)

(defn sync-enabled?
  "Should boot start IMAP sync for the stored accounts? `SIMMIS_MAIL_SYNC`
   decides when it is set (\"false\" turns it off); otherwise
   `:mail {:sync? false}` in the config. Default ON: that is what a production
   server does. An isolated or test instance turns it off, so it does not poll
   real mailboxes with credentials it may have been handed."
  ([] (sync-enabled? (System/getenv "SIMMIS_MAIL_SYNC") (dcfg/config)))
  ([env config]
   (if-let [v (some-> env str/trim not-empty)]
     (not= "false" (str/lower-case v))
     (not (false? (get-in config [:mail :sync?]))))))

(defn boot!
  "`start-all!` when mail sync is enabled; one info line when it is not.
   Returns `:started` or `:disabled`."
  ([] (boot! (sync-enabled?)))
  ([enabled]
   (if enabled
     (do (start-all!) :started)
     (do (log/log! {:level :info :id ::sync-disabled
                    :msg (str "Mail sync is OFF (SIMMIS_MAIL_SYNC=false or :mail {:sync? false}): "
                              "stored mail accounts are not polled at boot.")})
         :disabled))))
