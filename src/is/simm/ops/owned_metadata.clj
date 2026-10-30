(ns is.simm.ops.owned-metadata
  "Fixed human metadata observation under the original App and actual Room.
   One immutable source snapshot; current authority/default gates at publication."
  (:require [dvergr.runtime.preparation :as preparation]
            [datahike.api :as d]
            [geschichte.query :as query]
            [is.simm.documents.files :as files]
            [is.simm.documents.github-source :as github]
            [is.simm.model.access :as access]
            [is.simm.model.github-sync :as github-sync]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.ops.owned-files :as owned-files]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- github-source [control bound]
  (let [row (d/entity control [:room/id (:room-id bound)])
        repository (:room/github-source row)
        ref (:room/github-ref row)
        import-head (:room/github-import-head row)
        registry-system (:system/id (:room/github-repository row))
        active (when repository (github-sync/active-in control (:room-id bound)))
        cursor (when repository (integrations/current-in control (:room-id bound)))
        proof (when cursor (first (d/datoms control :eavt (:db/id cursor) :github.integration/proof)))
        at (when proof (:db/txInstant (d/entity control (:tx proof))))]
    (when (and repository
               (= repository (try (github/repository! repository) (catch Exception _ nil)))
               (string? ref) (<= 12 (count ref) 512)
               (uuid? import-head) (uuid? registry-system))
      (cond-> {:kind :github :repository repository :ref ref :import-head import-head
               :available? (and (= registry-system (get-in bound [:target :registry-system]))
                                (= ref (query/current-ref (:db bound))))}
        (and (inst? at) (:github.integration/oid cursor))
        (assoc :last-sync {:at (str (.toInstant ^java.util.Date at)) :git-oid (:github.integration/oid cursor)})
        active (assoc :sync-active? true :sync-kind (github-sync/kind active))
        (and active (or (not= :push (github-sync/kind active))
                        (#{:creating :retaining :ready :pushed :rejected :observed} (:github.sync/state active)))
             (access/can? control (:party bound) :admin {:room (:room-id bound)}))
        (assoc :sync-id (str (:github.sync/id active)) :can-retire? true)
        (= (:party bound) (:github.sync/by active))
        (assoc :sync-client-id (str (:github.sync/client-id active)))))))

(defn read! [host principal handle request]
  (when (simple/on-drain-thread?)
    (throw (ex-info "Memory metadata requires a host worker" {:type :validation-error})))
  (files/metadata-request! request)
  (let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (when-not (fn? with-operation)
      (throw (ex-info "Memory metadata is unavailable" {:type :memory/metadata-unavailable})))
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
                (let [bound (reads/resolve-host-read! host principal handle)]
                  (when-not (identical? room (:room bound))
                    (throw (ex-info "Memory metadata is unavailable" {:type :memory/metadata-unavailable})))
                  (let [result (owned-files/metadata-projection (:db bound) (:target-stamp bound) request)]
                    (preparation/with-host-current! lease (constantly true))
                    (reads/validate-host-read-room! host principal handle resolved)
                    (let [control (reads/current-host-control-db! host)
                          _ (owned/validate! control principal :read (:room-id bound) room bound)
                          party (:party bound) resource {:room (:room-id bound)}]
                      ;; Same-target writes may make this an older observation.
                      ;; It remains one coherent snapshot; hints drive catch-up.
                      (let [reply (assoc result :protocol-version 2
                                         :github-source (github-source control bound)
                                         :permissions {:write? (access/can? control party :write resource)
                                                       :evaluate? (access/can? control party :merge resource)})]
                        ;; Default validation may perform IO. Cancellation after
                        ;; the earlier fence must still refuse final publication.
                        (preparation/with-host-current! lease (constantly reply))))))))))))))
