(ns is.simm.agents.memory-read
  "Read-only source-document vocabulary over a caller-resolved knowledge view.
   No connection, interpreter, evaluator or repository writer is exposed."
  (:require [clojure.string :as str]))

(defn- snapshot! [resolve-view!]
  (let [view (resolve-view!)]
    (when-not (and (map? view) (#{:ready :invalid :pending} (:status view)))
      (throw (ex-info "Memory resolver must return an explicit knowledge view"
                      {:type ::invalid-view})))
    view))

(defn- receipt [view]
  ;; A content hash identifies bytes; it is not itself a storage-GC lease.
  (assoc (select-keys view [:status :id :selection :diagnostics :reason])
         :retention :source-lifetime))

(defn- descriptor [doc]
  (select-keys doc [:id :path :content :revision :identity-status]))

(defn bindings
  "Every public call resolves current authority and ONE ambient-world view
   exactly once. Saved functions retain only the resolver, never a DB value.
   Non-ready views return explicit diagnostics, never successful empty memory.
   Search is literal, case-sensitive and bounded; offsets are UTF-16 source
   offsets. Receipts identify a selection but do not promise archival retention."
  [resolve-view!]
  {'documents
   (fn []
     (let [view (snapshot! resolve-view!)]
       (cond-> (receipt view)
         (= :ready (:status view))
         (assoc :documents (mapv descriptor (:documents view))))))

   'read
   (fn [path]
     (when-not (and (string? path) (<= 1 (count path) 512))
       (throw (ex-info "Expected a repository-relative document path" {:type ::invalid-path})))
     (let [view (snapshot! resolve-view!)]
       (cond-> (receipt view)
         (= :ready (:status view))
         (assoc :entry (get-in view [:selection :files path])
                :document
                (when-let [doc (some #(when (= path (:path %)) %) (:documents view))]
                  (merge (descriptor doc)
                         (select-keys doc [:source :semantic :properties])))))))

   'search
   (fn [text]
     (when-not (and (string? text) (<= 1 (count text) 256))
       (throw (ex-info "Expected literal search text of 1–256 characters"
                       {:type ::invalid-search})))
     (let [view (snapshot! resolve-view!)]
       (cond-> (receipt view)
         (= :ready (:status view))
         (assoc :matches
                (->> (:documents view)
                     (mapcat
                      (fn [{:keys [source] :as doc}]
                        (letfn [(matches [from]
                                  (lazy-seq
                                   (when-let [at (str/index-of source text from)]
                                     (cons (assoc (descriptor doc)
                                                  :from at :to (+ at (count text))
                                                  :snippet (subs source (max 0 (- at 60))
                                                                 (min (count source)
                                                                      (+ at (count text) 60))))
                                           (matches (+ at (count text)))))))]
                          (matches 0))))
                     (take 50) vec)))))})
