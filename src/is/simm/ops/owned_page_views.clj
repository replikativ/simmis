(ns is.simm.ops.owned-page-views
  "Fixed saved-view read within the original App and physical native Room.
   Source scopes come from the saved fence; neither scopes nor IDs grant edits."
  (:require [dvergr.runtime.preparation :as preparation]
            [is.simm.documents.page-view-reader :as reader]
            [is.simm.documents.view :as source-view]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.ops.owned-workspace :as owned]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [reason]
  (throw (ex-info "Owned page view was refused" {:type ::refused :reason reason})))

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
                (let [result (reader/inspect-in (:db bound) query)]
                  (preparation/with-host-current! lease (constantly true))
                  (reads/validate-host-read-room! host principal handle resolved)
                  (let [current (owned/validate! (reads/current-host-control-db! host) principal :read
                                                 (:room-id bound) room bound)]
                    ;; Changed source or trusted definitions cannot be silently
                    ;; returned as the current table. The response stays a DTO.
                    (when-not (or (source-view/same-source? (:db bound) (:db current))
                                  (= result (reader/inspect-in (:db current) query)))
                      (refuse! :source-changed))
                    ;; A second inspection may itself block on body IO. Its
                    ;; result cannot carry the earlier read/default decision.
                    ;; End with metadata-only checks; a third source version
                    ;; holds rather than starting an unbounded reread loop.
                    (preparation/with-host-current! lease (constantly true))
                    (reads/validate-host-read-room! host principal handle resolved)
                    (let [final-binding (owned/validate! (reads/current-host-control-db! host) principal :read
                                                         (:room-id bound) room bound)]
                      (when-not (source-view/same-source? (:db current) (:db final-binding))
                        (refuse! :source-changed)))
                    (preparation/with-host-current! lease (constantly true))
                    (reads/validate-host-read-room! host principal handle resolved)
                    (assoc result :target-stamp (:target-stamp bound)))))))))))))
