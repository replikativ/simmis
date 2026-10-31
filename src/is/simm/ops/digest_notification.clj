(ns is.simm.ops.digest-notification
  "Explicit durable navigation digest projection. Existing Request result fields
   own delivery; a claimed or uncertain native post is never replayed."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.datahike]
            [dvergr.runtime.preparation :as preparation]
            [hasch.core :as hasch]
            [is.simm.model.access :as access]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.ops.room-digest :as digest]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(def max-envelope-bytes 32768)
(def binding-keys [:work.request/id :work.request/by :work.request/room :work.request/client-id
                   :work.request/kind :work.request/claim :work.request/intent-id :work.request/payload-ref :work.request/job])

(defn- refuse! [reason]
  (throw (ex-info "Digest notification unavailable" {:type ::unavailable :reason reason})))

(defn message-id [request digest-id]
  (hasch/uuid [:simmis/conversation-digest-message-v1 request digest-id]))

(defn- current! [db principal expected store]
  (let [row (requests/request-in db (:work.request/id expected)) room (:work.request/room expected)]
    (reads/authorize-write! db principal (str room))
    (when-not (and (= (select-keys expected binding-keys) (select-keys row binding-keys))
                   (= :workflow (:work.request/kind row))
                   (= (:work.request/id row) (requests/request-id (:work.request/by row) room (:work.request/client-id row)))
                   (= store (:room/content-db-scope (d/entity db [:room/id room]))))
      (refuse! :request-changed))
    row))

(defn reserve-tx [db principal expected store ref]
  (let [row (current! db principal expected store)]
    (when (:work.request/result-scorecard row) (refuse! :result-fields-in-use))
    (if-let [existing (:work.request/result-message row)]
      (do (when-not (and (= existing ref) (= store (:work.request/result-store row))
                         (#{:digest-reserved :digest-posting :digest-published} (:work.request/result-status row)))
            (refuse! :result-fields-in-use)) [])
      (do
        (when (some #(contains? row %) [:work.request/result-status :work.request/result-store :work.request/result-at
                                       :work.request/result-published?])
          (refuse! :result-fields-in-use))
        [{:db/id (:db/id row) :work.request/result-message ref :work.request/result-store store
          :work.request/result-status :digest-reserved :work.request/result-published? false
          :work.request/result-at (java.util.Date.)}]))))

(defn claim-tx [db principal expected store ref]
  (let [row (current! db principal expected store)]
    (when-not (and (= ref (:work.request/result-message row))
                   (= store (:work.request/result-store row))
                   (= :digest-reserved (:work.request/result-status row))
                   (false? (:work.request/result-published? row)))
      (refuse! :inspection-required))
    [[:db/add (:db/id row) :work.request/result-status :digest-posting]]))

(defn mark-tx [db principal expected store ref]
  (let [row (current! db principal expected store)]
    (when-not (and (= ref (:work.request/result-message row))
                   (= :digest-posting (:work.request/result-status row)))
      (refuse! :inspection-required))
    [[:db/add (:db/id row) :work.request/result-published? true]
     [:db/add (:db/id row) :work.request/result-status :digest-published]]))

(defn publish!
  "A current Room writer explicitly keeps one already certified digest. Never
   grants allowance, invokes inference or retries an uncertain native post."
  [host principal handle {:keys [request-id digest-id] :as request}]
  (when-not (and (= #{:request-id :digest-id} (set (keys request)))
                 (every? uuid? [request-id digest-id])) (refuse! :invalid-request))
  (operation/with-app!
   host
   (fn []
     (let [original (reads/resolve-host-write-room! host principal handle)
           room (:room original) room-id (:room-id original)
           conn (:control-conn (::reads/owner host))
           expected (requests/request-in (reads/current-host-control-db! host) request-id)
           _ (when-not (= room-id (:work.request/room expected)) (refuse! :request-mismatch))
           _ (when-not (instance? dvergr.room.store.datahike.DatahikeStore (:store room)) (refuse! :source-unavailable))
           store (binding [ec/*execution-context* (:ctx room)]
                   (get-in @(:conn (:store room)) [:config :store :id]))
           check! #(do (lifetime/check-current!)
                       (reads/validate-host-write-room! host principal handle original)
                       (current! (reads/current-host-control-db! host) principal expected store))
           _ (check!)
           result (digest/result! host principal handle {:request-id request-id})
           _ (when-not (and (= :verified (:status result)) (= room-id (:room result))
                            (= request-id (:request result)) (= digest-id (:digest-id result)))
               (refuse! :digest-changed))
           timestamp (:work.request/created-at expected)
           _ (when-not (inst? timestamp) (refuse! :invalid-request-time))
           id (message-id request-id digest-id)
           envelope {:id id :from :simmis/conversation-digests :to :_room-log
                     :content (get-in result [:digest :summary :text])
                     :ts (.getTime ^java.util.Date timestamp) :in-reply-to nil :thread-root-id id
                     :metadata {:role :system :kind :conversation-digest
                                :notification/type :task-complete :notification/task request-id
                                :object {:kind :conversation-digest :id digest-id}
                                :source :simmis/conversation-digest :run-id (:work.request/job expected)}}]
       (check!)
       (when-not (:work.request/result-message (check!))
         (artifact/publish-value!
          (artifact/datahike-store conn) envelope
          (fn [ref] (d/transact conn [[:db.fn/call reserve-tx principal expected store ref]]))))
       (let [row (check!) ref (:work.request/result-message row)
             retained (:value (artifact/get-bounded-value (artifact/datahike-store conn) ref max-envelope-bytes))]
         (when-not (= envelope retained) (refuse! :digest-changed))
         (case (:work.request/result-status row)
           :digest-published
           (do (when-not (true? (:work.request/result-published? row)) (refuse! :inspection-required))
               {:status :published :request request-id :digest-id digest-id :message id})
           :digest-posting {:status :inspection-required :request request-id :digest-id digest-id :message id}
           :digest-reserved
           (do
             ;; Claim delivery once under original native lifetime, then release
             ;; preparation before posting: subscribers may fork on visibility.
             (binding [ec/*execution-context* (:execution-ctx host)]
               (preparation/with-host-operation!
                room
                (fn [lease]
                  (check!)
                  (preparation/with-host-current! lease (constantly true))
                  (d/transact conn [[:db.fn/call claim-tx principal expected store ref]]))))
             (locking (:meta room)
               (check!)
               (binding [ec/*execution-context* (:ctx room)]
                 (discourse/post! room (discourse/map->Message retained))))
             (check!)
             (d/transact conn [[:db.fn/call mark-tx principal expected store ref]])
             (check!)
             {:status :published :request request-id :digest-id digest-id :message id})
           (refuse! :result-fields-in-use)))))))
