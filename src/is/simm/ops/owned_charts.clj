(ns is.simm.ops.owned-charts
  "Fixed human read of a saved chart from the current owned-default repository.
   Application and native Room lifetime enclose every body/definition read."
  (:require [dvergr.runtime.preparation :as preparation]
            [is.simm.documents.chart :as chart]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [reason]
  (throw (ex-info "Owned chart read was refused" {:type ::refused :reason reason})))

(defn read!
  "HOST entry, never called on the engine drain. Expected stamps only constrain
   the actual owned target; clients cannot supply spec paths, worlds or schemas.
   A response is data, not retained authority or an executable definition."
  [host principal handle {:keys [expected-target-stamp] :as request}]
  (when-not (and (map? request)
                 (= #{:path :block :expected :revision :expected-target-stamp} (set (keys request)))
                 (uuid? expected-target-stamp))
    (refuse! :invalid-request))
  (let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (when-not (fn? with-operation) (refuse! :missing-lifetime))
    (with-operation
      (fn []
        (let [resolved (reads/resolve-host-read-room! host principal handle)
              room (:room resolved)]
          (binding [ec/*execution-context* (:execution-ctx host)]
            (preparation/with-host-operation!
              room
              (fn [lease]
                (preparation/with-host-current! lease (constantly true))
                (reads/validate-host-read-room! host principal handle resolved)
                (let [bound (reads/resolve-host-read! host principal handle)
                      _ (when-not (and (identical? room (:room bound))
                                       (= expected-target-stamp (:target-stamp bound)))
                          (refuse! :target-changed))
                      query (dissoc request :expected-target-stamp)
                      result (chart/inspect-in (:db bound) query)]
                  (preparation/with-host-current! lease (constantly true))
                  (reads/validate-host-read-room! host principal handle resolved)
                  (let [current (owned/validate! (reads/current-host-control-db! host) principal :read
                                                 (:room-id bound) room bound)]
                    ;; Unrelated writes are allowed; changed source, definitions
                    ;; or spec content refuse before a stale DTO can be returned.
                    (when-not (= result (chart/inspect-in (:db current) query))
                      (refuse! :source-changed))
                    (assoc result :target-stamp (:target-stamp bound))))))))))))
