(ns is.simm.ops.owned-saved-views
  "Fixed saved-view read within the original App and physical native Room.
   Source scopes come from the saved fence; neither scopes nor IDs grant edits."
  (:require [datahike.api :as d]
            [dvergr.agent.experiment :as experiment]
            [dvergr.ops :as ops]
            [dvergr.room.store.datahike]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.documents.files :as files]
            [is.simm.documents.saved-view-contract :as contract]
            [is.simm.documents.saved-view-reader :as reader]
            [is.simm.documents.scorecard-views :as scorecard-views]
            [is.simm.documents.view :as source-view]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.ops.owned-workspace :as owned]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [reason]
  (throw (ex-info "Owned saved view was refused" {:type ::refused :reason reason})))

(defn- receipt-context [room room-id]
  (let [store (:store room)]
    (when-not (instance? dvergr.room.store.datahike.DatahikeStore store)
      (refuse! :unsupported-receipt-store))
    (let [store-id (binding [ec/*execution-context* (:ctx room)]
                     (get-in @(:conn store) [:config :store :id]))]
      (when-not (uuid? store-id) (refuse! :invalid-receipt-store))
      {:room room-id :store store-id})))

(defn- validate-receipt-context! [control-db context]
  (when-not (= (:store context)
               (:room/content-db-scope (d/entity control-db [:room/id (:room context)])))
    (refuse! :receipt-store-changed)))

(defn- inspect-in [host principal handle original-room lease bound query]
  (let [resolved (reader/resolve-in (:db bound) query)]
    (case (get-in resolved [:descriptor :source :kind])
      :pages (reader/prepare-pages-in (:db bound) resolved)
      :scorecard
      (let [room (:room bound) context (receipt-context room (:room-id bound))
            control-db (reads/current-host-control-db! host)
            _ (reads/validate-host-read-room-in! host principal handle original-room control-db)
            _ (validate-receipt-context! control-db context)
            _ (preparation/with-host-current! lease (constantly true))
            scorecard (binding [ec/*execution-context* (:ctx room)]
                        (experiment/scorecard-bounded room (get-in resolved [:descriptor :source :scorecard])))]
        (merge resolved
               (if scorecard
                 (scorecard-views/prepare (:descriptor resolved) context
                                         (ops/scorecard-summary scorecard) (:scorecard/experiment scorecard))
                 {:profile contract/profile
                  :source (assoc context :kind :scorecard :scorecard (get-in resolved [:descriptor :source :scorecard]))
                  :status :unavailable :reason :scorecard-not-found :snapshot? true}))))))

(defn read! [host principal handle {:keys [expected-target-stamp] :as request}]
  (when-not (and (map? request) (uuid? expected-target-stamp)) (refuse! :invalid-request))
  (let [query (dissoc request :expected-target-stamp)]
    (reader/validate-request! query)
    (operation/with-app!
     host
     (fn []
       (let [resolved (reads/resolve-host-read-room! host principal handle)
             room (:room resolved)]
         (binding [ec/*execution-context* (:execution-ctx host)]
           (preparation/with-host-operation!
            room
            (fn [lease]
              (preparation/with-host-current! lease (constantly true))
              (reads/validate-host-read-room! host principal handle resolved)
              (let [bound (reads/resolve-host-read! host principal handle)]
                (when-not (and (identical? room (:room bound))
                               (= expected-target-stamp (:target-stamp bound)))
                  (refuse! :target-changed))
                (let [result (inspect-in host principal handle resolved lease bound query)
                      receipt? (= :scorecard (get-in result [:source :kind]))]
                  (preparation/with-host-current! lease (constantly true))
                  (reads/validate-host-read-room! host principal handle resolved)
                  (let [current (owned/validate! (reads/current-host-control-db! host) principal :read
                                                 (:room-id bound) room bound)]
                    ;; Changed source or trusted definitions cannot be silently
                    ;; returned as the current table. The response stays a DTO.
                    (when-not (if receipt?
                                (= (:expected query) (files/entry (:db current) (:path query)))
                                (or (source-view/same-source? (:db bound) (:db current))
                                    (= result (inspect-in host principal handle resolved lease current query))))
                      (refuse! :source-changed))
                    ;; A second inspection may itself block on body IO. Its
                    ;; result cannot carry the earlier read/default decision.
                    ;; End with metadata-only checks; a third source version
                    ;; holds rather than starting an unbounded reread loop.
                    (preparation/with-host-current! lease (constantly true))
                    (reads/validate-host-read-room! host principal handle resolved)
                    (let [validation-db (reads/current-host-control-db! host)
                          final-binding (owned/validate! validation-db principal :read
                                                         (:room-id bound) room bound)]
                      (when-not (source-view/same-source? (:db current) (:db final-binding))
                        (refuse! :source-changed))
                      (preparation/with-host-current! lease (constantly true))
                      ;; All source/store observations precede the final control
                      ;; snapshot. A valid earlier ACL cannot cross blocking IO.
                      (when (and receipt? (not= (select-keys (:source result) [:room :store])
                                               (receipt-context room (:room-id bound))))
                        (refuse! :receipt-store-changed))
                      (let [control-db (reads/current-host-control-db! host)]
                        (reads/validate-host-read-room-in! host principal handle resolved control-db)
                        ;; Full target resolution also checks physical aliases.
                        ;; Its decision cannot cross any later registry change,
                        ;; including a newly introduced alias outside its map.
                        (when-not (source-view/same-source? validation-db control-db)
                          (refuse! :control-changed))
                        (when receipt? (validate-receipt-context! control-db (:source result))))
                      (preparation/with-host-current! lease (constantly true))
                      (assoc result :target-stamp (:target-stamp bound))))))))))))))
