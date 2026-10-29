(ns is.simm.ops.benchmark-notifications
  "A durable conversational projection, not a second benchmark engine.
   Completion records an immutable envelope before posting. Notification failure
   never changes execution/settlement; explicit retry posts that same envelope."
  (:require [datahike.api :as d]
            [dvergr.agent.experiment :as experiment]
            [dvergr.artifact :as artifact]
            [dvergr.discourse :as discourse]
            [dvergr.ops :as native]
            [dvergr.room.store.datahike]
            [dvergr.runtime.preparation :as preparation]
            [hasch.core :as hasch]
            [is.simm.documents.benchmark-result :as result]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(def kinds #{:benchmark :cli-benchmark})
(def max-envelope-bytes 131072)

(defn- refuse! []
  (throw (ex-info "Benchmark notification unavailable" {:type ::unavailable})))

(defn- binding-fields [row]
  (select-keys row [:work.request/id :work.request/by :work.request/room
                    :work.request/client-id :work.request/claim :work.request/intent-id
                    :work.request/job :work.request/kind]))

(defn- current! [db principal expected store]
  (let [row (requests/request-in db (:work.request/id expected))
        party (access/actor->party-uuid (:principal/id principal))
        room (:work.request/room row)]
    (when-not (and row (contains? kinds (:work.request/kind row))
                   (= (binding-fields expected) (binding-fields row))
                   (= party (:work.request/by row))
                   (= (:work.request/id row)
                      (requests/request-id party room (:work.request/client-id row)))
                   (access/can? db party :read {:room room})
                   (access/can? db party :write {:room room})
                   (= store (:room/content-db-scope (d/entity db [:room/id room]))))
      (refuse!))
    row))

(defn- store-id! [room]
  (when-not (instance? dvergr.room.store.datahike.DatahikeStore (:store room)) (refuse!))
  (let [id (binding [ec/*execution-context* (:ctx room)]
             (get-in @(:conn (:store room)) [:config :store :id]))]
    (when-not (uuid? id) (refuse!))
    id))

(defn record-result-tx
  "Job-owned completion bookkeeping, before optional delivery. No permission
   grant or user code; preserves the exact outcome through App shutdown."
  [db expected store outcome scorecard at]
  (let [row (requests/request-in db (:work.request/id expected))
        facts (cond-> {:work.request/result-store store :work.request/result-status outcome}
                scorecard (assoc :work.request/result-scorecard scorecard))]
    (when-not (and row (= (binding-fields expected) (binding-fields row))
                   (contains? kinds (:work.request/kind row)) (uuid? store) (inst? at)
                   (uuid? (:work.request/job row))
                   (#{:completed :incomplete :failed :cancelled} outcome)
                   (= (= :completed outcome) (uuid? scorecard))) (refuse!))
    (if (:work.request/result-at row)
      (do (when-not (= facts (select-keys row (keys facts))) (refuse!)) [])
      [(merge {:db/id (:db/id row) :work.request/result-at at} facts)])))

(defn record-result!
  "Fixed native Job callback: keep inert IDs before retiring the actual result.
   Failure retains the Job's existing recovery owner; it never reruns work."
  [conn expected room status value expected-experiment]
  (let [store (store-id! room)
        id (when (= :completed status) (get-in value [:scorecard :scorecard/content-id]))
        outcome (if (= :completed status) (if (uuid? id) :completed :incomplete) status)]
    (when-not (and (contains? kinds (:work.request/kind expected))
                   (or (not= :completed status)
                       (and (= expected-experiment (get-in value [:experiment :experiment/content-id]))
                            (= (:work.request/job expected) (get-in value [:execution :cleanup-group]))
                            (or (nil? id) (uuid? id))))) (refuse!))
    (d/transact conn [[:db.fn/call record-result-tx expected store outcome id (java.util.Date.)]])))

(defn reserve-tx
  "Pure fixed journal writer. First envelope wins; different outcomes refuse."
  [db principal expected store outcome scorecard ref]
  (let [row (current! db principal expected store)
        facts (cond-> {:work.request/result-store store :work.request/result-status outcome}
                scorecard (assoc :work.request/result-scorecard scorecard))]
    (when-not (and (:work.request/result-at row)
                   (= facts (select-keys row (keys facts)))
                   (#{:completed :incomplete :failed :cancelled} outcome)
                   (= (= :completed outcome) (uuid? scorecard))) (refuse!))
    (if (:work.request/result-message row)
      (do (when-not (= facts (select-keys row (keys facts))) (refuse!)) [])
      [(merge {:db/id (:db/id row) :work.request/result-message ref
               :work.request/result-published? false} facts)])))

(defn- mark-tx [db principal expected store ref]
  (let [row (current! db principal expected store)]
    (when-not (= ref (:work.request/result-message row)) (refuse!))
    (if (:work.request/result-published? row) []
        [[:db/add (:db/id row) :work.request/result-published? true]])))

(defn- post! [conn principal expected room store check!]
  (check!)
  (let [row (current! @conn principal expected store)
        _ (when-not (= store (:work.request/result-store row)) (refuse!))
        ref (:work.request/result-message row)
        envelope (:value (artifact/get-bounded-value (artifact/datahike-store conn) ref max-envelope-bytes))]
    (when-not (and (map? envelope)
                   (= (hasch/uuid [::message (:work.request/id row)]) (:id envelope))
                   (= :simmis/benchmarks (:from envelope)) (= :_room-log (:to envelope))
                   (= :benchmark/result (get-in envelope [:metadata :kind]))
                   (= (:id envelope) (:thread-root-id envelope))
                   (= (when-let [id (:work.request/result-scorecard row)] {:kind :benchmark-result :id id})
                      (get-in envelope [:metadata :object]))
                   (= (:work.request/job row) (get-in envelope [:metadata :run-id]))
                   (= :system (get-in envelope [:metadata :role]))
                   (string? (:content envelope)) (<= (count (:content envelope)) max-envelope-bytes))
      (refuse!))
    (check!)
    (when-not (:work.request/result-published? row)
      (binding [ec/*execution-context* (:ctx room)]
        (discourse/post! room (discourse/map->Message envelope)))
      (check!)
      (d/transact conn [[:db.fn/call mark-tx principal expected store ref]]))
    {:request (:work.request/id row) :message (:id envelope)
     :status :published :scorecard (:work.request/result-scorecard row)}))

(defn complete!
  "Fixed Job callback, under a new ticket on its ORIGINAL installation.
   Caller supplies the exact original Room/Request/Job binding. Job bookkeeping
   has already retained the actual runner result's exact inert identity.
   No live-room lookup, inference, report creation or resource settlement."
  [conn principal room-id room expected]
  (lifetime/check-current!)
  (let [request-id (:work.request/id expected)
        job-id (:work.request/job expected)
        store (store-id! room)
        check! #(do (lifetime/check-current!) (current! @conn principal expected store))]
    (when-not (and (= room-id (:work.request/room expected))
                   (uuid? job-id)) (refuse!))
    (if (:work.request/result-message (check!))
      (post! conn principal expected room store check!)
      (let [prepared
            (preparation/with-host-operation!
              room
              (fn [lease]
                (let [check! #(do (check!) (preparation/with-host-current! lease (constantly true)))
                      row (current! @conn principal expected store)
                      _ (when-not (and (:work.request/result-at row)
                                       (= store (:work.request/result-store row))) (refuse!))
                      id (:work.request/result-scorecard row)
                      retained (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                                                                   (:work.request/payload-ref expected) (* 32 1024 1024)))
                      _ (when-not (= (:work.request/intent-id expected) (hasch/uuid retained)) (refuse!))
                      card (when (uuid? id)
                             (binding [ec/*execution-context* (:ctx room)] (experiment/scorecard-bounded room id)))
                      _ (when (and id (not (and card
                                                (= (:scorecard/experiment card) (get-in retained [:input :experiment]))))) (refuse!))
                      outcome (:work.request/result-status row)
                      content (if card
                                (result/content {:room room-id :store store} (native/scorecard-summary card)
                                                (:scorecard/experiment card))
                                (str "Benchmark " (name outcome) ". No complete Scorecard was produced. "
                                     "Open the Run for its recorded status; this is not a zero score."))
                      message-id (hasch/uuid [::message request-id])
                      envelope {:id message-id :from :simmis/benchmarks :to :_room-log
                                :content content :timestamp (.getTime ^java.util.Date (:work.request/result-at row))
                                :in-reply-to nil :thread-root-id message-id
                                :metadata (cond-> {:role :system :kind :benchmark/result :run-id job-id}
                                            card (assoc :object {:kind :benchmark-result :id id}))}]
                  (check!)
                  (when-not (:work.request/result-message row)
                    (artifact/publish-value!
                     (artifact/datahike-store conn) envelope
                     (fn [ref]
                       (d/transact conn [[:db.fn/call reserve-tx principal expected store outcome id ref]]))))
                  (check!)
                  true)))]
      ;; Native post owns delivery admission. Release preparation before it:
      ;; subscribers may fork after durable visibility.
        (when prepared (post! conn principal expected room store check!))))))

(defn retry!
  "Explicit human retry of an already reserved notification. Never runs work
   or reconstructs a missing result from the newest Scorecard."
  [host principal handle request-id]
  (when-not (uuid? request-id) (refuse!))
  (operation/with-app!
    host
    (fn []
      (let [original (reads/resolve-host-write-room! host principal handle)
            conn (system-db/get-conn)
            expected (requests/get-request conn request-id)
            room (:room original) store (store-id! room)]
        (when-not (and (= (:room-id original) (:work.request/room expected))
                       (:work.request/result-at expected)
                       (= store (:work.request/result-store expected))) (refuse!))
        (reads/validate-host-write-room! host principal handle original)
        (let [reply (complete! conn principal (:room-id original) room expected)]
          (reads/validate-host-write-room! host principal handle original)
          reply)))))
