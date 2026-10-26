(ns is.simm.ops.knowledge-stream-status
  "Fixed personal saved-workflow observation. No discovery, evidence capture,
   reservation, provider, recovery, funding, activation or publication."
  (:require [datahike.api :as d]
            [dvergr.catalog.room :as catalog]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.documents.view :as source-view]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.knowledge-batch-inputs :as batch-inputs]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.knowledge-program :as program]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private uuid-text [:re #"(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"])
(def request-schema
  [:map {:closed true} [:name workflows/name-schema]
   [:expected-source uuid-text] [:expected-bundle uuid-text]])

(defn- unavailable! []
  (throw (ex-info "Workflow stream observation unavailable" {:type ::unavailable})))

(defn- sanitized [body]
  (try (body)
       (catch Throwable error
         (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
         (if (= ::reads/refused (:type (ex-data error)))
           (throw (ex-info "Room permission required" {:type :not-authorized}))
           (unavailable!)))))

(defn- freshness [db stream]
  ;; Failure of the stronger published-state check must not erase known journal
  ;; facts or accidentally treat a same-CID unpublished value as current.
  (try
    (let [basis (changes/basis db)]
      {:status (if (= [(streams/source-id basis) (:commit basis) (:max-tx basis)]
                      [(:knowledge.stream/source stream) (:knowledge.stream/cursor-commit stream)
                       (:knowledge.stream/cursor-tx stream)])
                 :current :changed)})
    (catch Throwable error
      (when (adoption/causal-interruption? error)
        (.interrupt (Thread/currentThread))
        (throw error))
      {:status :unknown})))

(defn- active-batch [db descriptor stream]
  (if-let [id (:knowledge.stream/active-batch stream)]
    (let [b (when (uuid? id) (batches/batch db id))
          phase (:knowledge.batch/state b)
          rid (:knowledge.batch/request b)
          request (when (uuid? rid) (requests/request-in db rid))
          reverse-requests (when (uuid? id) (vec (take 2 (d/datoms db :avet :work.request/knowledge-batch id))))
          linked? (and (= 1 (count reverse-requests)) (= (:db/id request) (:e (first reverse-requests)))
                       (uuid? rid) (= :submitted (:work.request/state request))
                       (= :workflow (:work.request/kind request))
                       (uuid? (:work.request/client-id request))
                       (= rid (requests/request-id (:by descriptor) (:room descriptor) (:work.request/client-id request)))
                       (= [(:by descriptor) (:room descriptor) id (:knowledge.batch/frozen-id b)
                           (:knowledge.batch/request-intent b)]
                          [(:work.request/by request) (:work.request/room request) (:work.request/knowledge-batch request)
                           (:work.request/knowledge-frozen request) (:work.request/intent-id request)])
                       (every? uuid? [(:knowledge.batch/frozen-id b) (:knowledge.batch/request-intent b)]))]
      (if (and (every? uuid? [id (:knowledge.batch/selection b) (:knowledge.batch/selection-id b)
                              (:knowledge.batch/claim b)])
               (= id (:knowledge.batch/selection-id b))
               (= [(:id descriptor) (:by descriptor) (:room descriptor) (:target descriptor)]
                  [(:knowledge.batch/stream b) (:knowledge.batch/by b) (:knowledge.batch/room b) (:knowledge.batch/target b)])
               (#{:capturing :ready :submitted :review} phase)
               (or (and (= :capturing phase)
                        (not-any? #(contains? b %) [:knowledge.batch/frozen :knowledge.batch/frozen-id
                                                   :knowledge.batch/scheduled-occurrence]))
                   (and (not= :capturing phase) (every? uuid? [(:knowledge.batch/frozen b) (:knowledge.batch/frozen-id b)])))
               (if (#{:submitted :review} phase)
                 linked?
                 (and (empty? reverse-requests)
                      (not-any? #(contains? b %) [:knowledge.batch/request :knowledge.batch/request-intent]))))
        {:status :observed :phase phase}
        {:status :unavailable}))
    {:status :none}))

(defn observe-in
  "HOST internal over captured source and one immutable control DB. Descriptor
   belongs to the authenticated viewer. Counts describe the pending journal,
   excluding claimed items; they do not certify that another batch can run."
  [control source-db descriptor]
  (if-let [stream (streams/stream control (:id descriptor))]
    (do
      (streams/validate-binding! descriptor stream)
      (let [pending (streams/pending-items control (:id descriptor))]
        {:status :observed :lifecycle (:knowledge.stream/state stream)
         :pending {:count (count pending) :complete? true :limit streams/max-changes}
         :freshness (freshness source-db stream)
         :batch (active-batch control descriptor stream)}))
    {:status :not-discovered}))

(defn- source-id [bound name]
  (sources/source-id bound name (workflows/manifest-in (:db bound) name)))

(defn read-on-host!
  "Caller owns the original App operation. Normal read authority suffices;
   physical Room lifetime covers source IO through final current validation."
  [host principal handle {:keys [name expected-source expected-bundle] :as request}]
  (when-not (m/validate request-schema request) (unavailable!))
  (lifetime/check-current!)
  (let [original (reads/resolve-host-read-room! host principal handle)
        room (:room original)]
    (binding [ec/*execution-context* (:execution-ctx host)]
      (preparation/with-host-operation!
       room
       (fn [lease]
         (preparation/with-host-current! lease (constantly true))
         (reads/validate-host-read-room! host principal handle original)
         (let [bound (reads/resolve-host-read! host principal handle)]
           (when-not (and (identical? room (:room bound))
                          (= (parse-uuid expected-source) (source-id bound name))) (unavailable!))
           (let [snapshot (workflows/snapshot-in (:db bound) name)
                 _ (when-not (= (parse-uuid expected-bundle) (:id (catalog/bundle name (:files snapshot))))
                     (unavailable!))
                 declaration (batch-inputs/declaration (:files snapshot))
                 ;; No selector comes from the browser beyond the named saved
                 ;; source preconditions; author, target and epoch are HOST facts.
                 result (if declaration
                          (let [described (program/describe (:party bound) (:room-id bound) (:target bound)
                                                            snapshot name (parse-uuid expected-bundle) (:epoch declaration))
                                control (reads/current-host-control-db! host)]
                            (observe-in control (:db bound) (:value described)))
                          {:status :not-enabled})
                 validation-db (reads/current-host-control-db! host)
                 current (owned/validate! validation-db principal :read (:room-id bound) room bound)]
             (when-not (source-view/same-source? (:db bound) (:db current)) (unavailable!))
             ;; Target validation can perform native metadata IO. Its authority
             ;; decision cannot cross a later control/registry change.
             (let [control (reads/current-host-control-db! host)]
               (reads/validate-host-read-room-in! host principal handle original control)
               (when-not (source-view/same-source? validation-db control) (unavailable!)))
             (preparation/with-host-current!
              lease #(do (lifetime/check-current!)
                         (assoc result :name name :source-revision expected-source :bundle expected-bundle))))))))))

(defn prepare-status
  "Capture original App before queueing. No client author/stream/epoch selector."
  [party handle request]
  (sanitized
   (fn []
     (when-not (and (uuid? party) (string? handle) (<= 1 (count handle) 200)
                    (m/validate request-schema request)) (unavailable!))
     (let [enter (application/capture-operation)
           principal {:principal/kind :human :principal/id (str "party/" party)}]
       (fn [] (sanitized #(enter (fn [host] (read-on-host! host principal handle request)))))))))
