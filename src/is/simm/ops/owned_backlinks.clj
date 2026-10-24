(ns is.simm.ops.owned-backlinks
  "Fixed human canonical-backlink read in the canonical owned-default repository."
  (:require [dvergr.runtime.preparation :as preparation]
            [is.simm.documents.block-reference :as reference]
            [is.simm.documents.backlinks :as backlinks]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [reason]
  (throw (ex-info "Owned backlink read refused" {:type ::refused :reason reason})))

(defn read! [host principal handle {:keys [expected-target-stamp] :as request}]
  (when-not (and (map? request) (uuid? expected-target-stamp)) (refuse! :invalid-request))
  (let [query (backlinks/request! (dissoc request :expected-target-stamp))
        with-operation (get-in host [:config :host-lifecycle :with-operation])]
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
                      selected (reference/selection (:db bound))
                      result (backlinks/selected (:db bound) selected (str (:room-id bound)) query)]
                  (preparation/with-host-current! lease (constantly true))
                  (reads/validate-host-read-room! host principal handle resolved)
                  (let [current (owned/validate! (reads/current-host-control-db! host) principal :read
                                                 (:room-id bound) room bound)]
                    ;; Prove the same complete source set without parsing twice.
                    ;; JSON/binary/accounting changes do not invalidate a link.
                    (when-not (= selected (reference/selection (:db current)))
                      (refuse! :source-changed))
                    (let [reply (assoc result :target-stamp (:target-stamp bound))]
                      (when (> (alength ^bytes (json/write-value-as-bytes reply))
                               (:max-reply-bytes backlinks/profile))
                        (refuse! :reply-limit))
                      reply)))))))))))
