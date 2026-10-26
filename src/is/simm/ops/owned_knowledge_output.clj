(ns is.simm.ops.owned-knowledge-output
  "Fixed original-host knowledge output collection and root-job descriptors.
   No inference, arbitrary Attempt selection, source recapture or file adoption."
  (:require [datahike.api :as d]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- refuse! []
  (throw (ex-info "Wiki output is unavailable for review" {:type ::unavailable})))

(def ^:private header-fields
  [:work.request/id :work.request/by :work.request/room :work.request/kind
   :work.request/state :work.request/job :work.request/publication-basis-id])

(defn- authorize! [db principal room-id]
  (reads/authorize-read! db principal (str room-id))
  (filing/authorize! db principal room-id))

(defn- header! [db principal room-id request-id]
  ;; Current human read/merge authority precedes request metadata and hydration.
  (let [party (authorize! db principal room-id)
        row (d/pull db header-fields [:work.request/id request-id])]
    (when-not (and (= request-id (:work.request/id row))
                   (= party (:work.request/by row)) (= room-id (:work.request/room row))
                   (= :workflow (:work.request/kind row))
                   (= :submitted (:work.request/state row))
                   (uuid? (:work.request/job row)) (uuid? (:work.request/publication-basis-id row)))
      (refuse!))
    row))

(defn- wire-result [request-id value]
  (when-not (and (= request-id (:request value)) (#{:proposed :no-op} (:kind value))
                 (if (= :proposed (:kind value)) (uuid? (:proposal value)) (nil? (:proposal value))))
    (refuse!))
  (cond-> {:status (:kind value) :request (str request-id)}
    (:proposal value) (assoc :proposal (str (:proposal value)))))

(defn- unavailable [value f]
  (try (f)
       (catch Throwable error
         (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
         value)))

(defn collect!
  "Caller retains the ORIGINAL application admission. Only its fixed owner
   connection/context and actual registered Room may enter existing filing.
   Retry observes the canonical disposition; this never starts or adopts work."
  [host principal room-id request-id]
  (unavailable
   {:status :unavailable}
   (fn []
     (when (or (simple/on-drain-thread?) (not-every? uuid? [room-id request-id])) (refuse!))
     (lifetime/check-current!)
     (header! (reads/current-host-control-db! host) principal room-id request-id)
     (let [conn (:control-conn (::reads/owner host))
           resolved (reads/resolve-host-room! host principal (str room-id))
           room (:room resolved)]
       (when-not conn (refuse!))
       (binding [ec/*execution-context* (:execution-ctx host)]
         (let [value (room-operation/with-current!
                      host principal (str room-id) resolved
                      (fn []
                        (header! (reads/current-host-control-db! host) principal room-id request-id)
                        (publish/file-completed-request-unannounced! conn principal room-id room request-id)))]
           ;; A proposal observer can fork immediately after native visibility.
           ;; Finish all source/Attempt leases before that message is posted.
           (lifetime/check-current!)
           (header! (reads/current-host-control-db! host) principal room-id request-id)
           (reads/validate-host-room! host principal (str room-id) resolved)
           (publish/announce-filed-original! conn principal resolved value)
           (wire-result request-id value)))))))

(defn- job-request! [db job-id]
  (let [matches (d/q '[:find [?id ...] :in $ ?job
                       :where [?e :work.request/job ?job] [?e :work.request/id ?id]] db job-id)]
    (when-not (= 1 (count matches)) (refuse!))
    (first matches)))

(defn inspect-job!
  "Metadata only: root workflow Job -> one trusted canonical request. Count all
   matching headers before ownership checks; never trust mutable Run metrics or
   select the latest Attempt. Eligible data is not collection authority."
  [host principal room-id job-id]
  (unavailable
   {:status :none}
   (fn []
     (when (or (simple/on-drain-thread?) (not-every? uuid? [room-id job-id])) (refuse!))
     (lifetime/check-current!)
     (let [db (reads/current-host-control-db! host)]
       (authorize! db principal room-id)
       (let [id (job-request! db job-id) row (header! db principal room-id id)]
         (lifetime/check-current!)
         (let [current (reads/current-host-control-db! host)]
           (authorize! current principal room-id)
           (when-not (and (= id (job-request! current job-id))
                          (= row (header! current principal room-id id))) (refuse!)))
         {:status :eligible :request (str id) :job (str job-id)})))))
