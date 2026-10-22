(ns is.simm.ops.file-output-service
  "Fixed human Proposal actions. Application ownership precedes metadata;
   current authority and closed component shape precede Room hydration; the
   exact Room owns every native/body operation. No client-selected targets."
  (:require [datahike.api :as d]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-controller :as controller]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.application-ops :as application]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! []
  (throw (ex-info "Reviewed file proposal is unavailable" {:type ::unavailable})))

(defn- principal [party]
  {:principal/kind :human :principal/id (str "party/" party)})

(defn- header! [db party proposal action]
  (when-not (every? uuid? [party proposal]) (refuse!))
  (let [room (:proposal/room (d/pull db [:proposal/room] [:proposal/id proposal]))]
    (when-not (uuid? room) (refuse!))
    ;; Canonical human identity and room read authority, with no hydration.
    (reads/authorize-read! db (principal party) (str room))
    ;; Both paths validate the bounded SINGLE retained-file component and its
    ;; immutable filing. Historical reconciliation deliberately is not open-only.
    ((if (= :reconcile action) adoption/reconciliation-snapshot adoption/snapshot)
     db party room proposal)
    room))

(defn- wire-result [result]
  ;; Explicit finite DTO: never forward the prepared witness, DB, body bytes,
  ;; claim token, throwable or native handles returned by an internal helper.
  (when-not (#{:reviewed :accepted :pending :aborted} (:status result)) (refuse!))
  (let [raw-operation (or (:operation result) (:operation-id result))
        operation (if (map? raw-operation) (:id raw-operation) raw-operation)]
    (cond-> {:status (:status result)}
      (uuid? (:proposal result)) (assoc :proposal (str (:proposal result)))
      (uuid? (:component result)) (assoc :component (str (:component result)))
      (uuid? (:revision result)) (assoc :revision (str (:revision result)))
      (uuid? operation) (assoc :operation (str operation))
      (true? (:recovered? result)) (assoc :recovered? true)
      (true? (:already-accepted? result)) (assoc :already-accepted? true))))

(defn- failure-result [error]
  (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
  (let [{:keys [type reason operation]} (ex-data error)]
    (cond
      (and (uuid? operation)
           (#{::controller/pending ::adoption/claim-unacknowledged
              ::adoption/recovery-unacknowledged} type))
      {:status :pending :operation (str operation)}
      (and (= ::controller/unavailable type) (= :review-changed reason))
      {:status :review-changed}
      :else {:status :unavailable})))

(defn- perform! [party proposal action revision]
  (try
    ;; Never look up the connection, Proposal or Room before admission to the
    ;; embedding lifetime. A stopped old installation cannot donate authority.
    (application/with-operation!
      (fn [host]
        (let [conn (system-db/get-conn)
              db (reads/current-host-control-db! host)
              room-id (header! db party proposal action)
              resolved (reads/resolve-host-room! host (principal party) (str room-id))
              room (:room resolved)]
          (binding [ec/*execution-context* (:execution-ctx host)]
            (preparation/with-host-operation!
              room
              (fn [lease]
                (preparation/with-host-current! lease (constantly true))
                (when-not (= room-id (header! (reads/current-host-control-db! host) party proposal action))
                  (refuse!))
                (reads/validate-host-room! host (principal party) (str room-id) resolved)
                (wire-result
                 (case action
                   :preview (controller/preview-reviewed! conn party room-id room proposal)
                   :adopt (controller/adopt-reviewed! conn party room-id room proposal revision)
                   :reconcile (adoption/recover! conn party room-id room proposal)))))))))
    (catch Throwable error (failure-result error))))

(defn preview! [authenticated-party proposal-id]
  (perform! authenticated-party proposal-id :preview nil))

(defn adopt! [authenticated-party proposal-id expected-revision]
  ;; A malformed revision must not hydrate or claim, even a pending operation.
  (if (uuid? expected-revision)
    (perform! authenticated-party proposal-id :adopt expected-revision)
    {:status :unavailable}))

(defn reconcile! [authenticated-party proposal-id]
  (perform! authenticated-party proposal-id :reconcile nil))
