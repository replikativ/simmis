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

(defn- row-in! [db id]
  (when-not (uuid? id) (throw (ex-info "Exact manifest UUID required" {:type ::invalid})))
  (let [eid (:e (first (d/datoms db :avet :file-output/id id)))]
    (when-not eid (throw (ex-info "Retained file manifest is unavailable" {:type ::missing})))
    ;; Bound component expansion before pull allocates child rows. Don't use
    ;; wildcard pull here: component attrs recursively expand automatically.
    (let [entries (vec (take 101 (d/datoms db :eavt eid :file-output/entries)))]
      (when (> (count entries) 100)
        (throw (ex-info "Retained manifest exceeds its entry bound" {:type ::corrupt})))
      (assoc (d/pull db '[:file-output/output-root :file-output/destination-root :file-output/exclude-prefixes
                          :file-output/id :file-output/version :file-output/request :file-output/room
                          :file-output/attempt :file-output/environment :file-output/workflow
                          :file-output/basis :file-output/output :file-output/repository :file-output/store
                          :file-output/backend :file-output/branch :file-output/ref
                          :file-output/registry-system :file-output/runtime-system] eid)
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

(defn retained-in
  "Bounded validated metadata and exact GC-tracked body references from ONE
   immutable DB. No body reads or authorization. This snapshot is NOT a blob
   retention pin; missing concurrently collected bytes must fail closed."
  [db id]
  (let [row (row-in! db id)]
    (when-not (every? #(uuid? (:file-output.entry/body-ref %)) (:file-output/entries row))
      (throw (ex-info "Retained file body reference is missing" {:type ::corrupt})))
    {:manifest (checked-metadata row)
     :body-refs (into (sorted-map)
                      (map (juxt :file-output.entry/path :file-output.entry/body-ref))
                      (:file-output/entries row))}))

(defn manifest-in
  "Bounded validated metadata from ONE immutable DB, including a writer's
   serialized snapshot. No artifact/connection reads or authorization."
  [db id]
  (:manifest (retained-in db id)))

(defn manifest
  "Bounded metadata only; no file body fetch. Caller authorizes the request."
  [conn id]
  (manifest-in @conn id))

(defn read-file-in
  "Fetch one exact retained pair named by an immutable DB snapshot. The caller
   authorizes that snapshot first. Missing bytes fail closed: a DB snapshot is
   not a GC pin. No source room, live world or Attempt store is consulted."
  [conn db id path]
  (let [{:keys [manifest body-refs]} (retained-in db id)
        entry (get-in manifest [:entries path])]
    (when-not entry (throw (ex-info "File is not in this manifest" {:type ::missing})))
    (plans/validate-pair! entry (artifact/get-value (artifact/datahike-store conn) (get body-refs path)))))

(defn read-file
  "Fetch one retained pair from the current snapshot after authorization."
  [conn id path]
  (read-file-in conn @conn id path))
