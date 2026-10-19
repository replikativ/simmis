(ns is.simm.ops.file-output-review
  "Retained-file inspection through its canonical Proposal owner. No live Run,
   target workspace lookup, certification claim or publication authority."
  (:require [is.simm.model.access :as access]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.documents.markdown :as markdown]))

(defn- unavailable! []
  (throw (ex-info "Retained file review is unavailable" {:type ::unavailable})))

(defn- binding!
  [conn party scope expected-proposal]
  (do
    (when-not (and (uuid? party) (uuid? scope)
                   (or (nil? expected-proposal) (uuid? expected-proposal)))
      (unavailable!))
    (let [db @conn
          {:keys [proposal room] :as owner} (manifests/owner db scope)]
      (when-not (and owner
                     (or (nil? expected-proposal) (= expected-proposal proposal))
                     (access/can? db party :read {:room room}))
        (unavailable!))
      {:proposal proposal :room room})))

(defn- retained! [conn party scope expected-proposal]
  (let [{:keys [room] :as owner} (binding! conn party scope expected-proposal)
        manifest (storage/manifest conn scope)]
    (when-not (and (= scope (:id manifest)) (= room (:room manifest))) (unavailable!))
    (assoc owner :manifest manifest)))

(defn summary
  "Authorized metadata-only projection. Current target compatibility has NOT
   been checked; retained evidence is not an expired execution capability."
  [party scope]
  (let [conn (system-db/get-conn)
        {:keys [manifest]} (retained! conn party scope nil)]
    {:scope scope :system-type :file-output :retained? true
     :settlement-supported? false :target-compatibility :not-checked
     :settlement-reason "Review only — file adoption is not available."
     :files (mapv (fn [[path {:keys [before after]}]]
                    {:path path :status (if (= :absent (:state before)) :added :modified)
                     :before-bytes (:bytes before) :after-bytes (:bytes after)})
                  (:entries manifest))
     :counts {:files-changed (count (:entries manifest))}}))

(defn read-file
  "One authorized exact before/after pair. External callers address BOTH the
   Proposal and component, never an unscoped store reference or artifact UUID.
   The explicit party argument is host-only: capture authenticated identity at RPC
   entry, before asynchronous FRP continuations, never from a client argument."
  [party proposal scope path]
  (when-not (uuid? proposal) (unavailable!))
  (let [conn (system-db/get-conn)
        _ (retained! conn party scope proposal)
        pair (storage/read-file conn scope path)]
    {:path path
     :before (some-> (:before pair) outputs/bytes! markdown/decode)
     :after (markdown/decode (outputs/bytes! (:after pair)))}))
