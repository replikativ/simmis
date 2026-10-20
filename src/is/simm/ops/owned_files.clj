(ns is.simm.ops.owned-files
  "Inactive fixed host file actions. No external adapter/routing is installed.
   Expected stamps are preconditions, never target selectors or capabilities."
  (:require [is.simm.documents.files :as files]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.owned-workspace :as owned]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- refuse! [reason]
  (throw (ex-info "Owned file action was refused" {:type ::refused :reason reason})))

(defn save!
  "INTERNAL authenticated-host edit/create/upload vertical. Resolve the owned
   default after current read/write authorization; require its opened stamp
   before decoding any payload. Revalidate original handles/configuration and
   current authority at native effect gates. No client target or callback.

   The receipt carries the ORIGINAL target stamp even if selection/default or
   authority changes after physical publication. It records that save, not a
   new read of a potentially replacement target. Generic Room REPL is unchanged."
  [conn principal room-id room request]
  (when (simple/on-drain-thread?) (refuse! :requires-host-worker))
  (when-not (and conn (identical? conn (sdb/get-conn))
                 (map? request) (uuid? (:expected-target-stamp request)))
    (refuse! :invalid-host-input))
  (let [binding (owned/resolve! @conn principal :write room-id room)]
    (when-not (= (:expected-target-stamp request) (:target-stamp binding))
      (refuse! :target-changed))
    (let [authorize! #(do (owned/validate! @conn principal :write room-id room binding) true)
          result (files/save-file-to! room (:system binding)
                                      (dissoc request :expected-target-stamp)
                                      {:authorize! authorize!})]
      (assoc result :target-stamp (:target-stamp binding)))))
