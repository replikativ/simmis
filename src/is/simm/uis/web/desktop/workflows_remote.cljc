(ns is.simm.uis.web.desktop.workflows-remote
  "Authenticated browser adapters over the canonical bounded workflow ops."
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
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
     (context/with-server-context
       (let [id (java.util.UUID/fromString room-id)
             room (rooms/live-room id)]
         (when-not room (throw (ex-info "The room is not live" {:error :room-not-live})))
         (let [reply (ops/invoke context/server-context (ops/party-principal party)
                                 operation (assoc args :room (name (:id room))))
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

(defn-spin-remote load-workflow-list!
  [server-id room-id-str options]
  (spin-remote server-id [room-id-str options]
               #?(:clj (invoke-workflow (access/authenticated-party-id) room-id-str :simmis/workflow-list options)
                  :cljs nil)))

(defn-spin-remote load-workflow-source!
  [server-id room-id-str workflow-name]
  (spin-remote server-id [room-id-str workflow-name]
               #?(:clj (invoke-workflow (access/authenticated-party-id) room-id-str :simmis/workflow-source {:name workflow-name})
                  :cljs nil)))

(defn-spin-remote save-workflow-source!
  [server-id room-id-str edits]
  (spin-remote server-id [room-id-str edits]
               #?(:clj (invoke-workflow (access/authenticated-party-id) room-id-str :simmis/workflow-write edits)
                  :cljs nil)))

(defn-spin-remote submit-workflow-run!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (invoke-workflow (access/authenticated-party-id) room-id-str :simmis/workflow-run request)
                  :cljs nil)))

(defn-spin-remote submit-workflow-benchmark!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (invoke-workflow (access/authenticated-party-id) room-id-str :simmis/benchmark request)
                  :cljs nil)))

(defn-spin-remote provision-workflow-allowance!
  [server-id room-id-str request]
  (spin-remote server-id [room-id-str request]
               #?(:clj (invoke-workflow (access/authenticated-party-id) room-id-str :simmis/work-allowance request)
                  :cljs nil)))
