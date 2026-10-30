(ns is.simm.ops.file-output-review
  "Retained-file inspection through its canonical Proposal owner. No live Run,
   target workspace lookup, certification claim or publication authority."
  (:require [datahike.api :as d]
            [is.simm.model.access :as access]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.documents.links :as links]
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

(defn- manifest-summary [scope manifest]
  {:scope scope :system-type :file-output :retained? true
   :settlement-supported? false :target-compatibility :not-checked
   :settlement-reason "Review only — file adoption is not available."
   :files (mapv (fn [[path {:keys [before after]}]]
                  {:path path :status (if (= :absent (:state before)) :added :modified)
                   :before-bytes (:bytes before) :after-bytes (:bytes after)})
                (:entries manifest))
   :counts {:files-changed (count (:entries manifest))}})

(defn summary
  "Authorized metadata-only projection. Current target compatibility has NOT
   been checked; retained evidence is not an expired execution capability."
  [party scope]
  (let [{:keys [manifest]} (retained! (system-db/get-conn) party scope nil)]
    (manifest-summary scope manifest)))

(defn audit
  "Exact read-only retained Proposal projection, including after resolution.
   Invisible, absent and non-retained Proposals all return nil. No body reads,
   live branch access, model calls or settlement capabilities."
  [party proposal]
  (when (and (uuid? party) (uuid? proposal))
    (let [conn (system-db/get-conn) db @conn
          eid (:e (first (d/datoms db :avet :proposal/id proposal)))
          room (:proposal/room (when eid (d/pull db [:proposal/room] eid)))]
      (when (and (uuid? room) (access/can? db party :read {:room room}))
        (let [links (vec (take 101 (d/datoms db :eavt eid :proposal/forks)))]
          (when (> (count links) 100) (unavailable!))
          (let [forks (keep (fn [link]
                              (let [f (d/pull db [:proposal.fork/scope :proposal.fork/branch
                                                  :proposal.fork/system-type :proposal.fork/status] (:v link))]
                                (when (= :file-output (:proposal.fork/system-type f)) f))) links)]
            (when (seq forks)
              (let [p (d/pull db [:proposal/title :proposal/summary :proposal/status] eid)]
                {:id (str proposal) :title (:proposal/title p) :summary (:proposal/summary p)
                 :status (:proposal/status p)
                 :forks (mapv (fn [f]
                                (let [scope (:proposal.fork/scope f)
                                      {:keys [manifest]} (retained! conn party scope proposal)]
                                  (assoc (manifest-summary (str scope) manifest)
                                         :branch (:proposal.fork/branch f)
                                         :status (:proposal.fork/status f)))) forks)}))))))))

(defn source-citations
  "Bounded Markdown syntax only, not a source manifest or authority. Code,
   comments and unused reference definitions never become citations."
  [text]
  (try
    (let [paths (->> (markdown/link-references (markdown/parse text))
                     (filter #(and (= :link (:kind %)) (links/source-path? (:target %))))
                     (map :target) distinct (take 65) vec)]
      {:status :parsed :paths (vec (take 64 paths)) :truncated? (> (count paths) 64)})
    (catch clojure.lang.ExceptionInfo _ {:status :unavailable})))

(defn read-file
  "One authorized exact before/after pair. External callers address BOTH the
   Proposal and component, never an unscoped store reference or artifact UUID.
   The explicit party argument is host-only: capture authenticated identity at RPC
   entry, before asynchronous FRP continuations, never from a client argument."
  [party proposal scope path]
  (when-not (uuid? proposal) (unavailable!))
  (let [conn (system-db/get-conn)
        _ (retained! conn party scope proposal)
        pair (storage/read-file conn scope path)
        after (markdown/decode (outputs/bytes! (:after pair)))]
    {:path path
     :before (some-> (:before pair) outputs/bytes! markdown/decode)
     :after after :source-citations (source-citations after)}))
