(ns is.simm.model.work-request-checkpoints
  "HOST-only Request evidence retention journal. Separate from discovery
   cursors and execution status. No native root IO or authorization occurs here."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [malli.core :as m]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (#{:work.checkpoint/id :work.checkpoint/request} ident)
            (assoc :db/unique :db.unique/value)))
        [[:work.checkpoint/id :db.type/uuid]
         [:work.checkpoint/request :db.type/uuid]
         [:work.checkpoint/claim :db.type/uuid]
         [:work.checkpoint/state :db.type/keyword]
         [:work.checkpoint/payload :db.type/store-ref]
         [:work.checkpoint/payload-id :db.type/uuid]]))

(def selection-schema
  [:map {:closed true}
   [:version [:= 1]] [:policy [:= :request-evidence-v1]]
   [:origin [:map {:closed true} [:version [:= 1]] [:kind [:= :datahike]]
             [:commit :uuid]
             [:source [:map {:closed true} [:store :uuid] [:repository :uuid]]]]]
   [:basis :map]
   [:target [:and publication/target-schema [:fn publication/host-target?]]]])

(defn- refuse! [reason]
  (throw (ex-info "Work request checkpoint is unavailable" {:type ::held :reason reason})))

(defn validate-selection! [selection]
  (when-not (m/validate selection-schema selection) (refuse! :invalid-selection))
  (let [{:keys [origin basis target]} selection]
    (streams/validate-basis! basis)
    (when-not (and (= (:commit basis) (:commit origin))
                   (= (:source origin) (select-keys basis [:store :repository]))
                   (= (select-keys basis [:store :repository :backend :branch :ref])
                      (select-keys target [:store :repository :backend :branch :ref])))
      (refuse! :selection-mismatch)))
  selection)

(defn intent [db request-id]
  (when-let [eid (d/q '[:find ?e . :in $ ?id :where [?e :work.checkpoint/request ?id]] db request-id)]
    (d/pull db '[*] eid)))

(defn- request! [db expected]
  (let [current (requests/request-in db (:work.request/id expected))]
    (when-not (and (= current expected) (= :submitted (:work.request/state current))
                   (uuid? (:work.request/job current)))
      (refuse! :request-changed))
    current))

(defn prepare-tx
  "Serialized insert. Only the original fresh root/token winner may pin once;
   possession of an existing Request claim never grants creation on retry."
  [db expected digest ref root claim]
  (request! db expected)
  (if-let [existing (intent db (:work.request/id expected))]
    (do (when-not (= digest (:work.checkpoint/payload-id existing))
          (refuse! :intent-changed))
        [])
    (do
      (when (d/q '[:find ?e . :in $ ?root :where [?e :work.checkpoint/id ?root]] db root)
        (refuse! :root-id-reused))
      [{:work.checkpoint/id root :work.checkpoint/claim claim
        :work.checkpoint/request (:work.request/id expected)
        :work.checkpoint/state :creating
        :work.checkpoint/payload ref :work.checkpoint/payload-id digest}])))

(defn- request-selection! [conn request]
  (let [retained (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                                                     (:work.request/payload-ref request) (* 128 1024 1024)))]
    (when-not (and (= (:work.request/intent-id request) (hasch/uuid retained))
                   (= (:work.request/kind request) (:kind retained))
                   (= (:work.request/id request)
                      (requests/request-id (:work.request/by request) (:work.request/room request)
                                           (:work.request/client-id request))))
      (refuse! :request-integrity))
    (let [input (:input retained) origin (:knowledge/batch input)
          batch (:work.request/knowledge-batch request) frozen (:work.request/knowledge-frozen request)]
      (when (or (contains? input :knowledge/batch) batch frozen)
        (when-not (and (m/validate batches/origin-schema origin)
                       (= (:batch origin) batch) (= (:frozen origin) frozen))
          (refuse! :batch-linkage-mismatch))))
    (let [selection (validate-selection! (get-in retained [:input :knowledge/checkpoint]))
          publication (publication/validate! (get-in retained [:input :knowledge/publication-basis]))]
      (when-not (and (= (:target selection) (:target publication))
                     (= (:id publication) (:work.request/publication-basis-id request)))
        (refuse! :publication-target-mismatch))
      selection)))

(defn- propagate-interruption! [error]
  (when (some #(instance? InterruptedException %)
              (take-while some? (iterate ex-cause error)))
    (.interrupt (Thread/currentThread))
    (throw error)))

(defn- read-intent! [conn request]
  (try
    (intent @conn (:work.request/id request))
    (catch Exception error
      (propagate-interruption! error)
      (throw error))))

(defn prepare!
  "Persist exact Request/Job/source before pin IO. Native effects remain the
   fixed HOST caller's responsibility. After an ambiguous control write, read
   back this attempt's fresh token; do not infer ownership from Request claim."
  [conn request]
  (request! @conn request)
  (let [selection (request-selection! conn request)
        body {:version 1 :request (:work.request/id request)
              :request-intent (:work.request/intent-id request)
              :request-claim (:work.request/claim request)
              :job (:work.request/job request) :selection selection}
        digest (hasch/uuid body) root (random-uuid) claim (random-uuid)]
    (try
      (artifact/publish-value!
       (artifact/datahike-store conn) body
       (fn [ref] (d/transact conn [[:db.fn/call prepare-tx request digest ref root claim]])))
      (catch Exception e
        (propagate-interruption! e)
        (when-not (= digest (:work.checkpoint/payload-id (read-intent! conn request)))
          (throw e))))
    (let [row (read-intent! conn request)]
      (when-not (= digest (:work.checkpoint/payload-id row)) (refuse! :intent-changed))
      {:intent row :new? (and (= root (:work.checkpoint/id row))
                              (= claim (:work.checkpoint/claim row)))})))

(defn payload
  "Canonical retained binding. Integrity is checked against today's Request,
   while execution state may have settled. No live-source fallback or expiry."
  [conn row]
  (when-not (= row (intent @conn (:work.checkpoint/request row))) (refuse! :intent-changed))
  (let [body (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                                                 (:work.checkpoint/payload row) 16384))
        request (requests/get-request conn (:work.checkpoint/request row))
        selection (request-selection! conn request)]
    (when-not (and (= (:work.checkpoint/payload-id row) (hasch/uuid body))
                   (= body {:version 1 :request (:work.request/id request)
                            :request-intent (:work.request/intent-id request)
                            :request-claim (:work.request/claim request)
                            :job (:work.request/job request) :selection selection}))
      (refuse! :payload-mismatch))
    body))

(defn mark-retained-tx
  "HOST-only after full physical readback. No expiry at Job settlement: an
   explicit future evidence-retirement policy must govern root release."
  [db expected]
  (let [row (intent db (:work.checkpoint/request expected))]
    (when-not (= row expected) (refuse! :intent-changed))
    (case (:work.checkpoint/state row)
      :creating [{:db/id (:db/id row) :work.checkpoint/state :retained}]
      :retained []
      (refuse! :invalid-state))))
