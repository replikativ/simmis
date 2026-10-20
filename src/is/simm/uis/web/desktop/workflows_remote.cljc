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
            #?(:clj [is.simm.agents.room-agents :as rooms])
            #?(:clj [is.simm.ops.dvergr :as ops])
            #?(:clj [is.simm.ops.owned-reads :as owned-reads])
            #?(:clj [is.simm.ops.owned-workflows :as owned-workflows])
            #?(:clj [is.simm.ops.work-api :as work-api])
            #?(:clj [is.simm.ops.workflows :as workflows])
            #?(:clj [is.simm.runtimes.context :as context])))

#?(:clj
   (defn invoke-owned-workflow-read
     "DORMANT HOST-only normal-human reader. No remote or action calls this yet.
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
                    {:execution-ctx host-ctx ::owned-reads/owner (owned-reads/host-owner conn host-ctx)})
                  (catch Throwable error
                    (work-api/owned-workflow-read-error! (ex-info "Host owner unavailable" {} error))))
           principal (ops/party-principal party)]
       (let [{:keys [data read-context]}
             (try
               ((case operation :simmis/workflow-list owned-workflows/list!
                      :simmis/workflow-source owned-workflows/details!)
                host principal original-room-handle args)
               (catch Throwable error (work-api/owned-workflow-read-error! error)))
             {:keys [control-db party room-id]} read-context]
         (try
           ;; Permission hints share the exact immutable control snapshot of
           ;; this capture. They are not authority for a later write or run.
           (assoc data :permissions
                  {:write? (access/can? control-db party :write {:room room-id})
                   :merge? (access/can? control-db party :merge {:room room-id})
                   :fund? (= party (d/q '[:find ?owner . :in $ ?id
                                          :where [?r :room/id ?id] [?r :room/owner ?p] [?p :party/id ?owner]] control-db room-id))
                   :operator? (= :admin (d/q '[:find ?role . :in $ ?id
                                               :where [?p :party/id ?id] [?p :party/role ?role]] control-db party))})
           ;; Permission projection is OUTSIDE the normalized service boundary;
           ;; arbitrary helper tags here cannot manufacture a public conflict.
           (catch Throwable error
             (work-api/owned-workflow-read-error! (ex-info "Permission projection unavailable" {} error))))))))

#?(:clj
   (defn invoke-workflow [party room-id operation args]
     (when-not party (throw (ex-info "Authentication required" {:error :authentication-required})))
     (when (simple/on-drain-thread?)
       (throw (ex-info "Workflow actions require a host worker" {:type :validation-error})))
     (context/with-server-context
       (let [id (java.util.UUID/fromString room-id)
             room (rooms/live-room id)]
         (when-not room (throw (ex-info "The room is not live" {:error :room-not-live})))
         (let [reply (ops/invoke context/server-context (ops/party-principal party)
                                 operation (assoc args :room (str id)))
               db @(system-db/get-conn)]
           (assoc (case operation
                    :simmis/workflow-source (workflows/details room (:name args))
                    :simmis/workflow-write
                    (let [source (try (workflows/details room (:name args)) (catch Exception _ nil))]
                      (if (and source (:revision source)
                               (= (:revision source) (get-in reply [:workflow :revision])))
                        (assoc reply :definition (:definition source) :workflow (:inspection source))
                        (assoc reply :source-changed? true)))
                    reply)
                  :permissions
                  {:write? (access/can? db party :write {:room id})
                   :merge? (access/can? db party :merge {:room id})
                   :fund? (= party (d/q '[:find ?owner . :in $ ?id
                                          :where [?r :room/id ?id] [?r :room/owner ?p] [?p :party/id ?owner]] db id))
                   :operator? (= :admin (d/q '[:find ?role . :in $ ?id
                                               :where [?p :party/id ?id] [?p :party/role ?role]] db party))}))))))

#?(:clj
   (defn- workflow-on-worker [room-id operation args]
     ;; A fresh resource Spin, awaited ONCE by its inbound remote body. Never
     ;; derive the caller later on the worker or share/cache a live invocation.
     (let [party (access/authenticated-party-id)]
       (blocking/blocking #(invoke-workflow party room-id operation args)))))

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
