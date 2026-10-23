(ns is.simm.ops.owned-cells
  "Saved-cell preflight and entry checks under the original host/Room owners.
   Prepared values retain native handles privately; only :input is receipt data."
  (:require [dvergr.runtime.preparation :as preparation]
            [is.simm.documents.cells :as cells]
            [is.simm.documents.files :as files]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- refuse! [reason]
  (throw (ex-info "The saved code cell changed or is unavailable. Review the current document before running it."
                  {:type ::refused :reason reason})))

(defn validate!
  "Call under the actual Room lease; execution also holds the human interpreter
   gate, while preflight holds its short read lease. The
   immutable content ID proves the already-derived literal still names the
   saved source; do not reparse a different current fence at old offsets."
  [host principal handle {:keys [room room-binding file-binding request] :as prepared}]
  (lifetime/check-current!)
  (when-not (identical? host (::host prepared)) (refuse! :owner-changed))
  (reads/validate-host-room! host principal handle room-binding)
  (let [current (owned/validate! (reads/current-host-control-db! host) principal :read
                                 (:room-id file-binding) room file-binding)]
    (when-not (and (= (:expected-target-stamp request) (:target-stamp current))
                   (= (:repository-id request) (get-in current [:target :repository]))
                   (= (:expected request) (files/entry (:db current) (:path request))))
      (refuse! :source-changed)))
  true)

(defn prepare!
  "Short saved-source read INSIDE the captured original application operation.
   Releases its read lease before dispatch/await. Execution later acquires its
   own exact Room lease and revalidates this prepared source at interpreter entry."
  [host principal handle request]
  (lifetime/check-current!)
  (cells/validate-request! request)
  (let [selected (reads/resolve-host-room! host principal handle)
        room (:room selected)]
    (preparation/with-host-operation!
      room
      (fn [lease]
        (preparation/with-host-current! lease (constantly true))
        (reads/validate-host-room! host principal handle selected)
        (let [bound (reads/resolve-host-read! host principal handle)]
          (when-not (and (identical? room (:room bound))
                         (= (:expected-target-stamp request) (:target-stamp bound))
                         (= (:repository-id request) (get-in bound [:target :repository]))
                         (= (:expected request) (files/entry (:db bound) (:path request))))
            (refuse! :target-changed))
          (let [file (files/read-file-in (:db bound) (:path request))
                prepared {:room room :room-binding selected :file-binding bound :request request
                          :input (update (cells/inspect file request) :source assoc
                                         :room-id (:room-id selected) :room-incarnation (:incarnation room))
                          ::host host}]
            (preparation/with-host-current! lease (constantly true))
            (validate! host principal handle prepared)
            prepared))))))
