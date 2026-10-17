(ns is.simm.ops.work-sandbox
  "The deliberately narrow namespace surface of isolated file/code workers.
   Ordinary room participants keep their richer room programming environment."
  (:require [clojure.string :as str]
            [dvergr.agent.roster :as roster]
            [dvergr.sandbox.extensions :as extensions]
            [sci.core :as sci]))

(def id :simmis/file-worker)
(def policy {:version 2 :surface :isolated-files
             :no-shared-registries true :no-room-discovery true :no-host-env true :no-host-mail true
             :no-host-books true})

(defn- denied [& _]
  (throw (ex-info "File workers cannot access shared host registries, room APIs, host books or host environment"
                  {:type ::denied})))

(defn install!
  "Applied after standard namespaces on EVERY working-context rebind. No host
   connection, credential or trunk-room closure is captured by this policy."
  [ctx _]
  (doseq [namespace (sci/eval-string* ctx "(mapv ns-name (all-ns))")
          :when (or (str/starts-with? (str namespace) "dvergr.")
                    (contains? '#{room actors agents tasks skills scheduler env intake.mail kontor} namespace))]
    (let [names (sci/eval-string* ctx (str "(vec (keys (ns-publics '" namespace ")))"))]
      (sci/add-namespace! ctx namespace (zipmap names (repeat denied)))))
  ctx)

(defn restrict-team
  "Versioned sandbox policy is part of every candidate's AgentDef and inputs
   identity. Rebuild/revise canonical definitions, never mutate their hashes."
  [team]
  (extensions/register! id {:version (:version policy) :basis policy
                            :install 'is.simm.ops.work-sandbox/install!})
  (reduce (fn [roster agent]
            ;; This endpoint selects a complete surface, not an arbitrary
            ;; extension union whose install ordering could restore a grant.
            (roster/revise-agent roster (:agent/id agent) {:sandbox #{id}}))
          team (roster/agents team)))
