(ns is.simm.ops.owned-files
  "Inactive fixed host file actions. No external adapter/routing is installed.
   Expected stamps are preconditions, never target selectors or capabilities."
  (:require [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.office :as office]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.owned-workspace :as owned]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- refuse! [reason]
  (throw (ex-info "Owned file action was refused" {:type ::refused :reason reason})))

(defn- binding! [conn principal action room-id room request stamped?]
  (when (simple/on-drain-thread?) (refuse! :requires-host-worker))
  (when-not (and conn (identical? conn (sdb/get-conn)) (map? request)
                 (or (not stamped?) (uuid? (:expected-target-stamp request))))
    (refuse! :invalid-host-input))
  (let [binding (owned/resolve! @conn principal action room-id room)]
    (when (and stamped? (not= (:expected-target-stamp request) (:target-stamp binding)))
      (refuse! :target-changed))
    binding))

(defn- closed! [request fields stamped?]
  (when-not (and (map? request)
                 (= (cond-> fields stamped? (conj :expected-target-stamp)) (set (keys request))))
    (refuse! :invalid-host-input)))

(defn- project! [conn principal room-id room request keys stamped? projection]
  (closed! request keys stamped?)
  (let [binding (binding! conn principal :read room-id room request stamped?)]
    (assoc (projection (:db binding)) :target-stamp (:target-stamp binding))))

(defn metadata!
  "Fresh discovery, not a stale-buffer operation. One stamped immutable DB."
  [conn principal room-id room request]
  (let [result (project! conn principal room-id room request #{:folders :paths} false
                         #(files/metadata-in % request))]
    (update result :revision #(hasch/uuid [:simmis/owned-memory-metadata-v1 (:target-stamp result) %]))))

(defn folder! [conn principal room-id room request]
  (project! conn principal room-id room request #{:folder} false
            #(files/list-folder-in % (:folder request))))

(defn search! [conn principal room-id room request]
  (project! conn principal room-id room request #{:query} false
            #(files/search-paths-in % (:query request))))

(defn read! [conn principal room-id room request]
  (project! conn principal room-id room request #{:path} false
            #(files/read-file-in % (:path request))))

(defn history! [conn principal room-id room request]
  (project! conn principal room-id room request #{:path} true
            #(files/history-in % (:path request))))

(defn history-file! [conn principal room-id room request]
  (project! conn principal room-id room request #{:path :commit} true
            #(files/read-history-in % (:path request) (:commit request))))

(defn download! [conn principal room-id room request]
  (project! conn principal room-id room request #{:path :expected} true
            #(files/read-binary-in % (:path request) (:expected request))))

(defn office! [conn principal room-id room request]
  (project! conn principal room-id room request #{:path :expected :part} true
            #(office/inspect-in % (:path request) (:expected request) (:part request))))

(defn- save-bound! [conn principal room-id room binding request]
  (let [authorize! #(do (owned/validate! @conn principal :write room-id room binding) true)
        result (files/save-file-to! room (:system binding) request {:authorize! authorize!})]
    (assoc result :target-stamp (:target-stamp binding))))

(defn office-save!
  "Prepare and publish with ONE original binding, never a second target lookup."
  [conn principal room-id room request]
  (closed! request #{:path :expected :part :text} true)
  (let [binding (binding! conn principal :write room-id room request true)
        prepared (office/prepare-save-in (:db binding) (dissoc request :expected-target-stamp))]
    (save-bound! conn principal room-id room binding prepared)))

(defn save!
  "INTERNAL authenticated-host edit/create/upload vertical. Resolve the owned
   default after current read/write authorization; require its opened stamp
   before decoding any payload. Revalidate original handles/configuration and
   current authority at native effect gates. No client target or callback.

   The receipt carries the ORIGINAL target stamp even if selection/default or
   authority changes after physical publication. It records that save, not a
   new read of a potentially replacement target. Generic Room REPL is unchanged."
  [conn principal room-id room request]
  (let [binding (binding! conn principal :write room-id room request true)]
    (save-bound! conn principal room-id room binding (dissoc request :expected-target-stamp))))
