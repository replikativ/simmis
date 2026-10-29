(ns is.simm.ops.github-connection
  "Fixed HOST capture of a newly imported ordinary Memory connection.
   No hydration, source fetch, cursor inference or artifact publication."
  (:require [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.workspace-intent :as creation]
            [is.simm.ops.github-import :as import]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn prepare-import!
  "Caller retains original App and the freshly prepared native Room. Resolve
   only that Room's exact owned default; compare the validated initial Git
   metadata again before constructing inert connection/receipt artifacts."
  [conn creator request room-id room source-proof]
  (lifetime/check-current!)
  (let [control @conn
        bound (owned/resolve! control creator :merge room-id room)
        metadata (import/source-metadata! (:conn bound) request)]
    (when-not (and (= metadata (dissoc source-proof :registry-system))
                   (= (:registry-system source-proof) (get-in bound [:target :registry-system])))
      (facts/refuse! :import-correspondence))
    (let [connection (connections/connection!
                      {:version 1 :room room-id :source (:source request) :ref (:ref metadata)
                       :target (:target bound) :target-stamp (:target-stamp bound)})
          receipt (integrations/import-body connection creator (creation/receipt creator request)
                                             (:head metadata) (:git-oid metadata))]
      (lifetime/check-current!)
      (owned/validate! @conn creator :merge room-id room bound)
      {:connection connection :connection-id (connections/connection-id connection)
       :receipt receipt :receipt-id (integrations/import-id receipt)})))
