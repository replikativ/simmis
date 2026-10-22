(ns is.simm.ops.knowledge-output-service
  "Fixed human collection of an existing workflow request's certified output.
   Never starts work, recaptures inputs, selects an Attempt, or adopts files."
  (:require [datahike.api :as d]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.application-ops :as application]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- refuse! []
  (throw (ex-info "Wiki output is unavailable for review" {:type ::unavailable})))

(defn- header! [db principal room-id request-id]
  ;; Current human and room authority precede request metadata and hydration.
  (reads/authorize-read! db principal (str room-id))
  (let [party (filing/authorize! db principal room-id)
        row (d/pull db [:work.request/id :work.request/by :work.request/room
                        :work.request/kind :work.request/state :work.request/job
                        :work.request/publication-basis-id]
                    [:work.request/id request-id])]
    (when-not (and (= request-id (:work.request/id row))
                   (= party (:work.request/by row)) (= room-id (:work.request/room row))
                   (= :workflow (:work.request/kind row))
                   (= :submitted (:work.request/state row))
                   (uuid? (:work.request/job row)) (uuid? (:work.request/publication-basis-id row)))
      (refuse!))))

(defn- wire-result [request-id value]
  (when-not (and (= request-id (:request value)) (#{:proposed :no-op} (:kind value))
                 (if (= :proposed (:kind value)) (uuid? (:proposal value)) (nil? (:proposal value))))
    (refuse!))
  (cond-> {:status (:kind value) :request (str request-id)}
    (:proposal value) (assoc :proposal (str (:proposal value)))))

(defn collect!
  "Authenticated UUIDs only. Retry observes the canonical immutable disposition;
   an unknown response is not permission to rerun inference. Failed certification
   (including an unfinished Run) returns no output or existence diagnostics."
  [party room-id request-id]
  (try
    (when (or (simple/on-drain-thread?) (not-every? uuid? [party room-id request-id]))
      (refuse!))
    (application/with-operation!
      (fn [host]
        (let [principal {:principal/kind :human :principal/id (str "party/" party)}
              conn (system-db/get-conn)]
          (header! (reads/current-host-control-db! host) principal room-id request-id)
          (let [resolved (reads/resolve-host-room! host principal (str room-id))
                room (:room resolved)]
            (binding [ec/*execution-context* (:execution-ctx host)]
              (preparation/with-host-operation!
                room
                (fn [lease]
                  (preparation/with-host-current! lease (constantly true))
                  (header! (reads/current-host-control-db! host) principal room-id request-id)
                  (reads/validate-host-room! host principal (str room-id) resolved)
                  (wire-result request-id
                               (publish/file-completed-request! conn principal room-id room request-id)))))))))
    (catch Throwable error
      (when (adoption/causal-interruption? error)
        (.interrupt (Thread/currentThread)))
      {:status :unavailable})))
