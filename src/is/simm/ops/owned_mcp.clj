(ns is.simm.ops.owned-mcp
  "Inactive human MCP host binding for canonical owned Memory reads.
   No startup installation, credential widening, agent routing or write API."
  (:require [clojure.string :as str]
            [dvergr.rooms.files :as files]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-reads :as reads]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private room-read-ops #{:room/files :room/read :catalog/list})
(def ^:private described-read-ops
  #{:simmis/workspace-bundle :simmis/workspace-bundle-chunk
    :simmis/workflow-list :simmis/workflow-source})
(def ^:private denial {:allow? false :reason "Canonical room read refused"})

(defn- refuse! []
  (throw (ex-info "Canonical room read refused" {:type ::refused})))

(defn- bounded-text? [value maximum]
  (and (string? value) (<= 1 (count value) maximum)
       (not (str/blank? value)) (not (re-find #"[\p{Cntrl}]" value))))

(defn- mcp-human! [principal]
  ;; Transport policy stays HERE. Shared browser reads never fabricate this
  ;; marker; canonical human identity/EACL is checked by the fixed service.
  (when-not (and (true? (:principal/mcp? principal))
                 (= :human (:principal/kind principal)))
    (refuse!)))

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
      (let [db (reads/current-control-db! conn host-ctx)]
        (if-not (ops/external-arguments-valid? principal request)
          denial
          (cond
            ;; This NEW inactive host exports owned human Memory only. Never
            ;; allow then downgrade a worker/agent to selected repository bytes.
            (and (contains? described-read-ops op) (#{:worker :agent} (:principal/kind principal))) denial
            (or (not (contains? (into room-read-ops described-read-ops) op)) (global-catalog? request)
                (#{:worker :agent} (:principal/kind principal)))
            ;; Preserve worker/agent and all unrelated op authority; NEVER map
            ;; those credentials to their enroller/owner's canonical Memory.
            (ops/authorize principal request)
            :else (do (mcp-human! principal)
                      (if (contains? described-read-ops op)
                  ;; Legacy host descriptors normalize UUID to slug. ONLY
                  ;; these fixed reads compare both mappings in ONE control DB;
                  ;; the original DTO handle remains the sole body selector.
                        (let [{:keys [action resource args]} request
                              [kind handle] resource]
                          (when-not (and (= :read action) (vector? resource) (= 2 (count resource))
                                         (= :room kind) (bounded-text? handle 1024))
                            (refuse!))
                          (let [descriptor (reads/authorize-read! db principal handle)]
                            (when (contains? request :args)
                              (when-not (and (map? args)
                                             (= (:room-id descriptor)
                                                (:room-id (reads/authorize-read! db principal (:room args)))))
                                (refuse!)))))
                        (reads/authorize-read! db principal (original-handle! request)))
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

(defn host
  "Construct an INACTIVE per-host canonical human MCP read binding. Caller owns
   the current canonical connection/context lifetime. Nothing installs it.
   Owned exports/workflow inspections are human-only; legacy hosts and unrelated worker/agent
   delegation are unchanged. No cached allow decision or selection mutation."
  [canonical-control-conn host-ctx]
  (-> (ops/host host-ctx)
      (assoc :is.simm.ops.owned-reads/owner (reads/host-owner canonical-control-conn host-ctx))
      (assoc-in [:config :auth :authorize]
                (partial authorize canonical-control-conn host-ctx))
      (assoc-in [:config :room-files :resolve-read]
                (fn [principal request]
                  (read-request! request)
                  (mcp-human! principal)
                  (let [{:keys [db target-stamp]}
                        (reads/resolve-read! canonical-control-conn host-ctx principal (:room request))]
                    {:db db :target-stamp (str target-stamp)})))))
