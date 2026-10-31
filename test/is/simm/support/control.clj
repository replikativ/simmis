(ns is.simm.support.control
  "Real control-only authority facts for tests that must stop before native
   Room/source acquisition. An opaque owner identity is not an execution context
   and must never reach an effect, Job, provider or resource operation."
  (:require [datahike.api :as d]
            [dvergr.system.db :as native-db]
            [is.simm.authority.world :as world]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn principal [party]
  {:principal/id (str "party/" party) :principal/kind :human
   :principal/actor (keyword "party" (str party)) :principal/scopes #{}})

(defn with-control
  ([f] (with-control [] f))
  ([extra-schema f]
   (let [cfg {:store {:backend :memory :id (random-uuid)}
              :schema-flexibility :write :keep-history? false}]
     (d/create-database cfg)
     (let [conn (d/connect cfg)
           owner (doto (lifetime/create) lifetime/open!)]
       (try
         (d/transact conn (concat native-db/schema native-db/actor-schema))
         (d/transact conn (#'system-db/merge-schemas @#'system-db/core-schema requests/schema extra-schema))
         (let [w (world/seed! conn) token (Object.)
               host {:execution-ctx token ::reads/owner (reads/host-owner conn token)
                     :config {:host-lifecycle {:with-operation #(lifetime/with-operation! owner %)}}}
               p (principal (:owner w)) handle (str (:room w))]
           (d/transact conn [{:room/id (:room w) :room/slug (str "control-" (:room w))}
                             {:room/id (:other-room w) :room/slug (str "other-" (:other-room w))}])
           (with-redefs [system-db/get-conn (constantly conn)
                         native-db/get-conn (constantly conn)
                         context/server-context token]
             ;; A broken owner must not make negative tests pass before their
             ;; intended injected stage. This is the real production preflight.
             (when-not (= {:room-id (:room w) :party (:owner w)}
                          (select-keys (reads/preflight-host-read! host p handle) [:room-id :party]))
               (throw (ex-info "Control fixture failed its positive owner preflight" {})))
             (f {:conn conn :w w :host host :principal p :handle handle})))
         (finally (lifetime/fence! owner) (lifetime/join! owner 5000) (d/release conn) (d/delete-database cfg)))))))
