(ns is.simm.uis.web.desktop.workflows-remote
  "Authenticated browser adapters over the canonical bounded workflow ops.
   Capture caller identity before moving synchronous host work off the drain."
  (:refer-clojure :exclude [await])
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            [org.replikativ.spindel.effects.await :refer [await]]
            #?(:clj [org.replikativ.spindel.blocking :as blocking])
            #?(:clj [org.replikativ.spindel.engine.impl.simple :as simple])
            #?(:clj [datahike.api :as d])
            #?(:clj [dvergr.ops :as dops])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.model.system-db :as system-db])
            #?(:clj [is.simm.model.db :as db])
            #?(:clj [is.simm.runtimes.memory-changes :as memory-changes])
            #?(:clj [is.simm.agents.room-agents :as rooms])
            #?(:clj [is.simm.ops.dvergr :as ops])
            #?(:clj [is.simm.ops.owned-reads :as owned-reads])
            #?(:clj [is.simm.ops.owned-workflows :as owned-workflows])
            #?(:clj [is.simm.ops.owned-workflow-host :as workflow-host])
            #?(:clj [is.simm.ops.work-api :as work-api])
            #?(:clj [is.simm.ops.workflows :as workflows])
            #?(:clj [is.simm.ops.knowledge-output-service :as knowledge-output])
            #?(:clj [is.simm.ops.wiki-setup :as wiki-setup])
            #?(:clj [is.simm.runtimes.context :as context])))

#?(:clj
   (defn- permissions [db party room-id]
     {:write? (access/can? db party :write {:room room-id})
      :merge? (access/can? db party :merge {:room room-id})
      :fund? (= party (d/q '[:find ?owner . :in $ ?id
                             :where [?r :room/id ?id] [?r :room/owner ?p] [?p :party/id ?owner]] db room-id))
      :operator? (= :admin (d/q '[:find ?role . :in $ ?id
                                  :where [?p :party/id ?id] [?p :party/role ?role]] db party))}))

#?(:clj
   (defn- read-owned-workflow-on-host [host principal handle operation args]
     (let [{:keys [data read-context]}
           (try
             ((case operation :simmis/workflow-list owned-workflows/list!
                    :simmis/workflow-source owned-workflows/details!)
              host principal handle args)
             (catch Throwable error (work-api/owned-workflow-read-error! error)))
           {:keys [control-db party room-id]} read-context]
       (try
         ;; Hints describe this exact capture, not authority for a later command.
         (assoc data :permissions (permissions control-db party room-id))
         (catch Throwable error
           (work-api/owned-workflow-read-error!
            (ex-info "Permission projection unavailable" {} error)))))))

#?(:clj
   (defn invoke-owned-workflow-read
     "HOST-only normal-human reader.
      Fixed domain read authority, not an MCP identity or generic op capability."
     [party original-room-handle operation args]
     (when-not (uuid? party)
       (dops/op-error! "Authentication required" ::owned-read-denied 403))
     (when (simple/on-drain-thread?)
       (dops/op-error! "Owned workflow reads require a host worker" ::owned-read-unavailable 503))
     (when-not (and (#{:simmis/workflow-list :simmis/workflow-source} operation)
                    (map? args) (not (contains? args :room))
                    (string? original-room-handle) (<= 1 (count original-room-handle) 200))
       (dops/op-error! "Invalid owned workflow read" ::owned-read-invalid 400))
     (let [host (try
                  (let [conn (system-db/get-conn) host-ctx context/server-context]
                    ;; The CLJS reader also parses this skipped server form;
                    ;; its alias map has no server-only owned-reads alias.
                    {:execution-ctx host-ctx :is.simm.ops.owned-reads/owner (owned-reads/host-owner conn host-ctx)})
                  (catch Throwable error
                    (work-api/owned-workflow-read-error! (ex-info "Host owner unavailable" {} error))))
           principal (ops/party-principal party)]
       (read-owned-workflow-on-host host principal original-room-handle operation args))))

#?(:clj
   (def ^:private workflow-operations
     #{:simmis/workflow-list :simmis/workflow-source :simmis/workflow-write
       :simmis/workflow-run :simmis/benchmark :simmis/cli-benchmark
       :simmis/work-allowance :simmis/cli-allowance
       :simmis/local-worker-list :simmis/local-worker-workflow
       :simmis/local-worker-benchmark :simmis/local-worker-allowance
       :simmis/workflow-schedule-create :simmis/workflow-schedule-list
       :simmis/workflow-schedule-set-enabled}))

#?(:clj
   (defn- hydrate-control-room! [host principal handle operation wire]
     ;; Only these four legacy controls require a physical native Room. Fixed
     ;; metadata authorization precedes hydration; owned retries never enter.
     (let [before (work-api/owned-helper-value! #(owned-reads/preflight-host-read! host principal handle))]
       (dops/authorize! host principal operation wire)
       (work-api/owned-helper-value!
        (fn []
          (when-not (rooms/live-room (:room-id before))
            (throw (ex-info "Native control room unavailable" {})))
          (let [after (owned-reads/preflight-host-read! host principal handle)]
            (when-not (= (select-keys before [:party :room-id]) (select-keys after [:party :room-id]))
              (throw (ex-info "Canonical control room changed" {})))
            (:room-id before)))))))

#?(:clj
   (defn invoke-workflow [party room-id operation args]
     (when-not (uuid? party) (throw (ex-info "Authentication required" {:error :authentication-required})))
     (when (simple/on-drain-thread?)
       (throw (ex-info "Workflow actions require a host worker" {:type :validation-error})))
     (when-not (and (contains? workflow-operations operation) (map? args)
                    (not (contains? args :room)) (string? room-id) (<= 1 (count room-id) 200))
       (dops/op-error! "Invalid workflow arguments" ::invalid 400))
     (context/with-server-context
       (let [ctx context/server-context
             host (work-api/owned-helper-value! #(workflow-host/host (system-db/get-conn) ctx))
             principal (ops/party-principal party) wire (assoc args :room room-id)]
         (case operation
           (:simmis/workflow-list :simmis/workflow-source)
           (do (dops/authorize! host principal operation wire)
               (read-owned-workflow-on-host host principal room-id operation args))

           :simmis/workflow-write
           (do
             ;; The canonical registered operation guard runs BEFORE the same
             ;; fixed save service used by MCP, whose bounded receipt omits the
             ;; definition needed by this editor. Never call a raw op impl.
             (dops/authorize! host principal operation wire)
             (let [saved (work-api/owned-workflow-save! host principal room-id args)]
               (work-api/owned-helper-value!
                #(let [{:keys [control-db party room-id]} (owned-reads/preflight-host-read! host principal room-id)]
                   (assoc (workflows/public-write {:write (:write saved) :workflow (get-in saved [:source :inspection])})
                          :definition (get-in saved [:source :definition])
                          :permissions (permissions control-db party room-id))))))

           (let [handle (if (contains? workflow-host/legacy-room-actions operation)
                          (str (hydrate-control-room! host principal room-id operation wire))
                          room-id)
                 reply (dops/invoke host principal operation (assoc args :room handle))]
             (work-api/owned-helper-value!
              #(let [{:keys [control-db party room-id]} (owned-reads/preflight-host-read! host principal handle)]
                 (if (map? reply)
                   (assoc reply :permissions (permissions control-db party room-id))
                   reply)))))))))

#?(:clj
   (defn- workflow-on-worker [room-id operation args]
     ;; A fresh resource Spin, awaited ONCE by its inbound remote body. Never
     ;; derive the caller later on the worker or share/cache a live invocation.
     (let [party (access/authenticated-party-id)]
       (blocking/blocking #(invoke-workflow party room-id operation args)))))

#?(:clj
   (defn prepare-metadata-stream-for!
     "Fixed host adapter: caller identity is captured by the authenticated RPC.
      Registration grants no read or mutation authority and opens no Room."
     [party]
     (context/with-server-context
       (memory-changes/watch-control-owned! party)
       (memory-changes/register-topic! @db/server-peer party)
       {:watching? true})))

(defn-spin-remote prepare-workflow-metadata-stream!
  [server-id]
  (spin-remote server-id []
               #?(:clj (let [party (access/authenticated-party-id)]
                         (await (blocking/blocking #(prepare-metadata-stream-for! party))))
                  :cljs nil)))

(defn-spin-remote load-workflow-list!
  [server-id room-id-str options]
  (spin-remote server-id [room-id-str options]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/workflow-list options))
                  :cljs nil)))

(defn-spin-remote load-workflow-source!
  [server-id room-id-str workflow-name]
  (spin-remote server-id [room-id-str workflow-name]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/workflow-source {:name workflow-name}))
                  :cljs nil)))

(defn-spin-remote save-workflow-source!
  [server-id room-id-str edits]
  (spin-remote server-id [room-id-str edits]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/workflow-write edits))
                  :cljs nil)))

(defn-spin-remote submit-workflow-run!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/workflow-run request))
                  :cljs nil)))

#?(:clj
   (defn- wire-uuid [value]
     (when (and (string? value)
                (re-matches #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}" value))
       (parse-uuid value))))

(defn-spin-remote inspect-wiki-maintenance-setup!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
               #?(:clj (let [party (access/authenticated-party-id) room-id (wire-uuid room-id-str)]
                         (await (blocking/blocking #(wiki-setup/inspect! party room-id))))
                  :cljs nil)))

(defn-spin-remote install-wiki-maintenance!
  [server-id room-id-str]
  (spin-remote server-id [room-id-str]
               #?(:clj (let [party (access/authenticated-party-id) room-id (wire-uuid room-id-str)]
                         (await (blocking/blocking #(wiki-setup/install! party room-id))))
                  :cljs nil)))

(defn-spin-remote collect-workflow-wiki-output!
  [server-id room-id-str request-id-str]
  (spin-remote server-id [room-id-str request-id-str]
               #?(:clj (let [party (access/authenticated-party-id)
                             room-id (wire-uuid room-id-str)
                             request-id (wire-uuid request-id-str)]
                         (await (blocking/blocking #(knowledge-output/collect! party room-id request-id))))
                  :cljs nil)))

(defn-spin-remote submit-workflow-benchmark!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/benchmark request))
                  :cljs nil)))

(defn-spin-remote provision-workflow-allowance!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/work-allowance request))
                  :cljs nil)))

(defn-spin-remote submit-cli-workflow-benchmark!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/cli-benchmark request))
                  :cljs nil)))

(defn-spin-remote provision-cli-workflow-allowance!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/cli-allowance request))
                  :cljs nil)))

(defn-spin-remote load-local-workflow-workers!
  [server-id room-id-str options]
  (spin-remote server-id [room-id-str options]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/local-worker-list options))
                  :cljs nil)))

(defn-spin-remote submit-local-workflow-run!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/local-worker-workflow request))
                  :cljs nil)))

(defn-spin-remote submit-local-workflow-benchmark!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/local-worker-benchmark request))
                  :cljs nil)))

(defn-spin-remote provision-local-workflow-allowance!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/local-worker-allowance request))
                  :cljs nil)))

(defn-spin-remote create-workflow-schedule!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/workflow-schedule-create request))
                  :cljs nil)))

(defn-spin-remote set-workflow-schedule-enabled!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (await (workflow-on-worker room-id-str :simmis/workflow-schedule-set-enabled request))
                  :cljs nil)))
