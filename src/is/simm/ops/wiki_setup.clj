(ns is.simm.ops.wiki-setup
  "Explicit inactive starter setup. Inspection describes current saved source,
   not an installation receipt; missing source never resolves an uncertain write."
  (:require [dvergr.runtime.preparation :as preparation]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-authoring :as authoring]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.starter.install-wiki :as installer]
            [is.simm.starter.wiki-maintenance :as starter]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- unavailable! []
  (throw (ex-info "Wiki maintenance setup is unavailable" {:type ::unavailable})))

(defn- failure [error]
  (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
  {:status (if (= ::authoring/pending (:type (ex-data error))) :pending :unavailable)})

(defn- perform! [enter party room-id operation request]
  (try
    (when-not (every? uuid? [party room-id]) (unavailable!))
    (enter
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
                      result (case operation
                               :inspect (installer/inspect! host principal handle)
                               :install (installer/install! host principal handle)
                               :upgrade (installer/upgrade! host principal handle request)
                               (unavailable!))
                      _ (reads/validate-host-install-room! host principal handle resolved)
                      status (case (:status result)
                               (:installed :already-installed :upgraded) :installed
                               (:missing :customized :upgrade-available :changed) (:status result)
                               (unavailable!))]
                  (preparation/with-host-current!
                   lease
                   #(assoc (select-keys result [:revision :installed-version :shipped-version])
                           :status status :name starter/name :activation :unchanged
                           :observation (if (= :inspect operation) :current-source :acknowledged))))))))))
    (catch Throwable error (failure error))))

(defn- prepare [party room-id operation request]
  (try
    (when-not (every? uuid? [party room-id]) (unavailable!))
    (let [enter (application/capture-operation)]
      (fn [] (perform! enter party room-id operation request)))
    (catch Throwable error (constantly (failure error)))))

(defn prepare-inspect [party room-id] (prepare party room-id :inspect nil))
(defn prepare-install [party room-id] (prepare party room-id :install nil))
(defn prepare-upgrade [party room-id request] (prepare party room-id :upgrade request))

(defn inspect! [authenticated-party room-id]
  (perform! application/with-operation! authenticated-party room-id :inspect nil))

(defn install! [authenticated-party room-id]
  (perform! application/with-operation! authenticated-party room-id :install nil))

(defn upgrade! [authenticated-party room-id request]
  (perform! application/with-operation! authenticated-party room-id :upgrade request))
