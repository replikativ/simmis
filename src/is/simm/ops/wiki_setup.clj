(ns is.simm.ops.wiki-setup
  "Explicit inactive starter setup. Inspection describes current saved source,
   not an installation receipt; missing source never resolves an uncertain write."
  (:require [dvergr.runtime.preparation :as preparation]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-authoring :as authoring]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.starter.install-wiki :as installer]
            [is.simm.starter.wiki-maintenance :as starter]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- unavailable! []
  (throw (ex-info "Wiki maintenance setup is unavailable" {:type ::unavailable})))

(defn- inspect-source! [host principal handle]
  (let [binding (reads/resolve-host-install! host principal handle)
        snapshot (workflows/snapshot-in (:db binding) starter/name)]
    (case (installer/decision snapshot (starter/source))
      :install :missing
      :already-installed :installed
      :customized :customized)))

(defn- perform! [party room-id install?]
  (try
    (when-not (every? uuid? [party room-id]) (unavailable!))
    (application/with-operation!
      (fn [host]
        (let [principal {:principal/kind :human :principal/id (str "party/" party)}
              handle (str room-id)
              _ (reads/preflight-host-install! host principal handle)
              resolved (reads/resolve-host-install-room! host principal handle)
              room (:room resolved)]
          (binding [ec/*execution-context* (:execution-ctx host)]
            (preparation/with-host-operation!
              room
              (fn [lease]
                (preparation/with-host-current! lease (constantly true))
                (reads/validate-host-install-room! host principal handle resolved)
                (let [host (assoc host ::reads/install-room-binding resolved)
                      status (if install?
                               (case (:status (installer/install! host principal handle))
                                 :installed :installed
                                 :already-installed :installed
                                 :customized :customized
                                 (unavailable!))
                               (inspect-source! host principal handle))]
                  {:status status :name starter/name :activation :unchanged
                   :observation (if install? :acknowledged :current-source)})))))))
    (catch Throwable error
      (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
      {:status (if (= ::authoring/pending (:type (ex-data error))) :pending :unavailable)})))

(defn inspect! [authenticated-party room-id]
  (perform! authenticated-party room-id false))

(defn install! [authenticated-party room-id]
  (perform! authenticated-party room-id true))
