(ns is.simm.ops.github-host
  "Fixed HOST extent shared by Git preparation and reviewed settlement.
   Internal synchronous bodies receive original native handles, never creation
   rights from a journal read. No body or authority is supplied by the wire."
  (:require [datahike.api :as d]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.access :as access]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.ops.github-import :as import]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- origin-in! [db room-id]
  (let [room (d/entity db [:room/id room-id])]
    (facts/origin! {:repository (:room/github-source room) :ref (:room/github-ref room)
                    :import-head (:room/github-import-head room)
                    :registry-system (:system/id (:room/github-repository room))})))

(defn- cleanup-authority! [context db]
  (when (and (:cleanup? context)
             (not (access/can? db (get-in context [:resolved :party]) :admin
                               {:room (get-in context [:resolved :room-id])})))
    (facts/refuse! :cleanup-denied)))

(defn current! [{:keys [host principal handle resolved bound lease origin] :as context}]
  (lifetime/check-current!)
  (preparation/with-host-current! lease (constantly true))
  (reads/validate-host-room! host principal handle resolved)
  (let [db (reads/current-host-control-db! host)]
    (cleanup-authority! context db)
    (when-not (= origin (origin-in! db (:room-id resolved))) (facts/refuse! :source-settings-changed))
    (owned/validate! db principal :merge (:room-id resolved) (:room resolved) bound)))

(defn writer-current! [{:keys [host principal handle resolved bound origin ticket revision] :as context} db]
  ;; Writer uses its own immutable DB, no source dereference or hydration.
  (when-not (and (lifetime/retained? ticket) (lifetime/ready? (:owner ticket)))
    (facts/refuse! :application-retired))
  (reads/validate-host-room-in! host principal handle resolved db)
  (cleanup-authority! context db)
  (owned/validate-control-in! db principal (:room-id resolved) bound revision)
  (when-not (= origin (origin-in! db (:room-id resolved))) (facts/refuse! :source-settings-changed)))

(defn- arguments! [party room-id operation body]
  (when-not (and (uuid? party) (uuid? room-id)) (facts/refuse! :authentication-required))
  (when-not (and (fn? body)
                 (#{:observe :prepare :apply :recover :discard :retire :review :accept :dismiss} operation))
    (facts/refuse! :invalid-request)))

(defn in!
  "Run synchronously INSIDE an already captured original App operation. This
   never recaptures a replacement App. Fixed internal bodies only; no request
   selected or retained callbacks. The original App remains physically counted
   while only cold native construction is detached."
  [host party room-id operation body]
  (arguments! party room-id operation body)
  (lifetime/check-current!)
  (when (and (ec/execution-context-bound?)
             (not (identical? (ec/home-context (ec/current-execution-context))
                              (ec/home-context (:execution-ctx host)))))
    (facts/refuse! :wrong-host))
  ;; Public entry points still authenticate humans. Delegated admission needs
  ;; its own real Run/grant handoff, not impersonation.
  (let [principal {:principal/kind :human :principal/id (str "party/" party)}
        handle (str room-id)
        resolved (lifetime/call-detached
                  (fn [restore!]
                    (restore! lifetime/check-current!)
                    (let [resolved (reads/resolve-host-room! host principal handle)]
                      (restore! lifetime/check-current!)
                      resolved)))]
    (binding [ec/*execution-context* (:execution-ctx host)]
      (preparation/with-host-operation!
        (:room resolved) (if (= :observe operation) {} {:exclusive-key :is.simm.ops.github-sync/sync})
        (fn [lease]
          (let [control (reads/current-host-control-db! host)
                bound (owned/resolve! control principal :merge room-id (:room resolved))
                context {:host host :principal principal :handle handle :resolved resolved :lease lease
                         :ticket lifetime/*ticket* :conn (:control-conn (::reads/owner host)) :cleanup? (= :retire operation)
                         :bound bound :origin (origin-in! control room-id)
                         :integration (integrations/current-in control room-id)
                         :revision (owned/control-revision control room-id (get-in bound [:target :registry-system]))}]
            (current! context)
            (if (= :observe operation)
              (body context)
              (import/with-admission! #(body context)))))))))

(defn capture
  "Capture before blocking; effects share the existing capacity1 admission.
   Observe is bounded/read-only and does not acquire the effect permit."
  [party room-id operation body]
  (arguments! party room-id operation body)
  (let [invoke (application/capture-operation)]
    (fn [] (invoke #(in! % party room-id operation body)))))
