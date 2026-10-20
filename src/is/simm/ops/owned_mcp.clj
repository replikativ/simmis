(ns is.simm.ops.owned-mcp
  "Inactive human MCP host binding for canonical owned Memory reads.
   No startup installation, credential widening, agent routing or write API."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.rooms.files :as files]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(def ^:private room-read-ops #{:room/files :room/read :catalog/list})
(def ^:private denial {:allow? false :reason "Canonical room read refused"})

(defn- refuse! []
  (throw (ex-info "Canonical room read refused" {:type ::refused})))

(defn- bounded-text? [value maximum]
  (and (string? value) (<= 1 (count value) maximum)
       (not (str/blank? value)) (not (re-find #"[\p{Cntrl}]" value))))

(defn- current-db! [conn host-ctx]
  ;; live-room itself uses context/server-context. An ambient/fork context
  ;; cannot replace this actual host owner, even when its values look alike.
  (when (or (simple/on-drain-thread?)
            (not (and conn host-ctx (identical? conn (system-db/get-conn))
                      (identical? host-ctx context/server-context))))
    (refuse!))
  @conn)

(defn- human! [db principal]
  (let [candidate (when (and (true? (:principal/mcp? principal))
                             (= :human (:principal/kind principal))
                             (string? (:principal/id principal)))
                    (access/actor->party-uuid (:principal/id principal)))
        party (when (and (uuid? candidate) (= (:principal/id principal) (str "party/" candidate)))
                candidate)
        entity (when (uuid? party) (d/entity db [:party/id party]))]
    (when-not (and (uuid? party) (:db/id entity)
                   (= :human (or (:actor/kind entity) (:party/type entity))))
      (refuse!))
    party))

(defn- canonical-room! [db handle]
  (when-not (bounded-text? handle 1024) (refuse!))
  ;; Java UUID/fromString accepts abbreviations such as 1-1-1-1-1. Only the
  ;; canonical wire shape reserves UUID semantics; ordinary slugs stay slugs.
  (let [id (when (re-matches #"(?i)[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" handle)
             (parse-uuid handle))
        rows (if id
               (d/q '[:find ?e ?id ?slug :in $ ?id
                      :where [?e :room/id ?id] [?e :room/slug ?slug]] db id)
               (d/q '[:find ?e ?id ?slug :in $ ?slug
                      :where [?e :room/id ?id] [?e :room/slug ?slug]] db handle))
        [eid room-id slug] (when (= 1 (count rows)) (first rows))]
    ;; Reverse uniqueness is explicit too: a UUID request does not authorize a
    ;; slug shared by another room. Nil IDs never enter a database query.
    (when-not (and eid (uuid? room-id) (bounded-text? slug 1024)
                   (= #{[eid room-id]}
                      (set (d/q '[:find ?e ?id :in $ ?slug
                                  :where [?e :room/slug ?slug] [?e :room/id ?id]] db slug))))
      (refuse!))
    {:room-id room-id :slug slug}))

(defn- authorized-room! [db principal handle]
  (let [party (human! db principal)
        {:keys [room-id] :as resolved} (canonical-room! db handle)]
    (when-not (access/can? db party :read {:room room-id}) (refuse!))
    resolved))

(defn- global-catalog? [{:keys [op action resource args] :as request}]
  (and (= :catalog/list op) (= :read action) (= [:global] resource)
       (or (not (contains? request :args))
           (and (map? args) (not (contains? args :room))))))

(defn- original-handle! [{:keys [action resource args] :as request}]
  (let [[kind handle :as descriptor] resource]
    (when-not (and (= :read action) (vector? resource) (= 2 (count descriptor))
                   (= :room kind) (bounded-text? handle 1024)
                   (or (not (contains? request :args))
                       (and (map? args) (= handle (:room args)))))
      (refuse!))
    handle))

(defn- authorize [conn host-ctx principal {:keys [op] :as request}]
  (try
    (binding [ec/*execution-context* host-ctx]
      (let [db (current-db! conn host-ctx)]
        (if-not (ops/external-arguments-valid? principal request)
          denial
          (if (or (not (contains? room-read-ops op)) (global-catalog? request)
                  (#{:worker :agent} (:principal/kind principal)))
            ;; Preserve worker/agent and all unrelated op authority; NEVER map
            ;; those credentials to their enroller/owner's canonical Memory.
            (ops/authorize principal request)
            (do (authorized-room! db principal (original-handle! request))
                {:allow? true})))))
    (catch InterruptedException _
      (.interrupt (Thread/currentThread))
      denial)
    (catch Throwable _ denial)))

(defn- read-request! [{:keys [operation room path offset length expected-target-stamp] :as request}]
  (let [allowed (case operation
                  :list #{:operation :room :path :offset :expected-target-stamp}
                  :read #{:operation :room :path :offset :length :expected-target-stamp}
                  :resource #{:operation :room :path :expected-target-stamp}
                  nil)]
    (when-not (and allowed (map? request) (every? allowed (keys request))
                   (bounded-text? room 1024)
                   (if (= :list operation)
                     (or (not (contains? request :path))
                         (and (string? path) (<= (count path) 4096)))
                     (and (string? path) (<= 1 (count path) 4096)))
                   (or (not (contains? request :offset))
                       (and (integer? offset) (<= 0 offset files/max-file-bytes)))
                   (or (not (contains? request :length))
                       (and (integer? length) (<= 1 length files/max-read-bytes)))
                   (or (not (contains? request :expected-target-stamp))
                       (bounded-text? expected-target-stamp 256)))
      (refuse!))
    ;; Refuse invalid/sensitive paths before a valid reader can hydrate storage.
    (files/workspace-path (or path "/") (= :list operation))))

(defn- canonical-binding!
  "ONE host-only rich binding. Future export/catalog adapters may reuse this
   fixed composition; it is not a second configurable callback or RPC seam."
  [conn host-ctx principal handle]
  (binding [ec/*execution-context* host-ctx]
    (let [before (current-db! conn host-ctx)
          {:keys [room-id]} (authorized-room! before principal handle)
          room (rooms/live-room room-id)
          after (current-db! conn host-ctx)
          current (authorized-room! after principal handle)]
      (when-not (and room (= room-id (:room-id current))) (refuse!))
      (owned/resolve! after principal :read room-id room))))

(defn host
  "Construct an INACTIVE per-host canonical human MCP read binding. Caller owns
   the current canonical connection/context lifetime. Nothing installs it.
   No global policy, cached allow decision, parent selection or room mutation."
  [canonical-control-conn host-ctx]
  (-> (ops/host host-ctx)
      (assoc-in [:config :auth :authorize]
                (partial authorize canonical-control-conn host-ctx))
      (assoc-in [:config :room-files :resolve-read]
                (fn [principal request]
                  (read-request! request)
                  (let [{:keys [db target-stamp]}
                        (canonical-binding! canonical-control-conn host-ctx principal (:room request))]
                    {:db db :target-stamp (str target-stamp)})))))
