(ns is.simm.ops.file-output-storage
  "Host-only persistence plumbing for Proposal-owned immutable file evidence.
   This is not an authorization, filing or publication-to-workspace endpoint."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.ops.file-output-plan :as plans]))

(defn publish!
  "Publish each file's BEFORE+AFTER pair under a GC guard, then call publish
   with a typed manifest entity. That callback MUST synchronously finish the
   canonical Proposal transaction; it must not enqueue or return a future.
   No manifest row is written separately, so no second outbox is needed."
  [conn plan publish]
  (plans/validate-plan! plan)
  (let [store (artifact/datahike-store conn)]
    (letfn [(step [entries refs]
              (if-let [[path pair] (first entries)]
                (artifact/publish-value! store pair
                                         (fn [ref] (step (next entries) (assoc refs path ref))))
                (publish (manifests/entity (:manifest plan) refs))))]
      (step (seq (:bodies plan)) {}))))

(defn- row! [conn id]
  (when-not (uuid? id) (throw (ex-info "Exact manifest UUID required" {:type ::invalid})))
  (let [db @conn eid (:e (first (d/datoms db :avet :file-output/id id)))]
    (when-not eid (throw (ex-info "Retained file manifest is unavailable" {:type ::missing})))
    ;; Bound component expansion before pull allocates child rows. Don't use
    ;; wildcard pull here: component attrs recursively expand automatically.
    (let [entries (vec (take 101 (d/datoms db :eavt eid :file-output/entries)))]
      (when (> (count entries) 100)
        (throw (ex-info "Retained manifest exceeds its entry bound" {:type ::corrupt})))
      (assoc (d/pull db '[:file-output/id :file-output/version :file-output/request :file-output/room
                         :file-output/attempt :file-output/environment :file-output/workflow
                         :file-output/basis :file-output/output :file-output/repository :file-output/store
                         :file-output/backend :file-output/branch :file-output/ref] eid)
             :file-output/entries
             (mapv #(d/pull db '[:file-output.entry/path :file-output.entry/before-state
                                :file-output.entry/before-content :file-output.entry/before-bytes
                                :file-output.entry/before-mode :file-output.entry/after-content
                                :file-output.entry/after-bytes :file-output.entry/after-mode
                                :file-output.entry/body-ref] (:v %)) entries)))))

(defn- checked-metadata [row]
  (let [manifest (manifests/metadata row)]
    (when-not (= (count (:file-output/entries row)) (count (:entries manifest)))
      (throw (ex-info "Retained file paths are not unique" {:type ::corrupt})))
    (plans/validate-manifest! manifest)))

(defn manifest
  "Bounded metadata only; no file body fetch. Caller authorizes the request."
  [conn id]
  (checked-metadata (row! conn id)))

(defn read-file
  "Fetch just one retained file pair, validating it against typed metadata.
   No reads from the source room, live world or original Attempt store."
  [conn id path]
  (let [row (row! conn id) metadata (checked-metadata row)
        entry (get-in metadata [:entries path])]
    (when-not entry (throw (ex-info "File is not in this manifest" {:type ::missing})))
    (let [stored (first (filter #(= path (:file-output.entry/path %)) (:file-output/entries row)))
          ref (:file-output.entry/body-ref stored)]
      (when-not (uuid? ref) (throw (ex-info "File body reference is missing" {:type ::corrupt})))
      (plans/validate-pair! entry (artifact/get-value (artifact/datahike-store conn) ref)))))
