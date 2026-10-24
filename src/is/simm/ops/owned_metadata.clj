(ns is.simm.ops.owned-metadata
  "Fixed human metadata observation under the original App and actual Room.
   One immutable source snapshot; current authority/default gates at publication."
  (:require [dvergr.runtime.preparation :as preparation]
            [is.simm.documents.files :as files]
            [is.simm.model.access :as access]
            [is.simm.ops.owned-files :as owned-files]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

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
                                         :permissions {:write? (access/can? control party :write resource)
                                                       :evaluate? (access/can? control party :merge resource)})]
                        ;; Default validation may perform IO. Cancellation after
                        ;; the earlier fence must still refuse final publication.
                        (preparation/with-host-current! lease (constantly reply))))))))))))))
