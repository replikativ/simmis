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
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.model.system-db :as system-db])
            #?(:clj [is.simm.agents.room-agents :as rooms])
            #?(:clj [is.simm.ops.dvergr :as ops])
            #?(:clj [is.simm.ops.workflows :as workflows])
            #?(:clj [is.simm.runtimes.context :as context])))

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
