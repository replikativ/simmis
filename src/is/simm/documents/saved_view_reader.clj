(ns is.simm.documents.saved-view-reader
  "Resolve a fixed saved-view declaration from an exact saved Markdown fence.
   Request identity constrains source; clients cannot provide a replacement
   query, descriptor, renderer or writable property binding."
  (:require [is.simm.documents.files :as files]
            [is.simm.documents.page-views :as views]))

(defn- refuse! [reason]
  (throw (ex-info "Saved view was refused" {:type ::refused :reason reason})))

(defn validate-request! [request]
  (when-not (and (map? request)
                 (= #{:path :expected :revision :from :to :view} (set (keys request)))
                 (uuid? (:revision request)) (uuid? (:view request))
                 (integer? (:from request)) (integer? (:to request))
                 (<= 0 (:from request) (:to request) files/max-open-bytes)
                 (map? (:expected request))
                 (= #{:content :size :mode} (set (keys (:expected request))))
                 (uuid? (get-in request [:expected :content]))
                 (integer? (get-in request [:expected :size]))
                 (<= 0 (get-in request [:expected :size]) files/max-open-bytes)
                 (= 33188 (get-in request [:expected :mode])))
    (refuse! :invalid-request))
  (files/path! (:path request))
  request)

(defn resolve-in
  "Resolve ONE exact saved fence, without projecting other documents or reading
   a receipt store. Only the first four parsed fences can be opened."
  [source request]
  (validate-request! request)
  (let [file (files/read-file-in source (:path request))
        analysis (:analysis file)]
    (when-not (and (= :markdown (:kind file)) (= (:expected request) (:entry file))
                   (= (:revision request) (:revision analysis)))
      (refuse! :source-changed))
    (let [entries (:saved-views analysis)
          matches (filter #(= (select-keys request [:from :to :view]) (select-keys % [:from :to :view])) entries)
          entry (when (= 1 (count matches)) (first matches))]
      (when-not (= :ready (:status entry)) (refuse! :invalid-or-missing-fence))
      (merge (select-keys request [:path :expected :revision :from :to])
             (select-keys entry [:view :descriptor])))))

(defn prepare-pages-in
  "Project pages for an already resolved saved declaration."
  [source resolved]
  (merge resolved (views/prepare source (:descriptor resolved))))

(defn inspect-pages-in
  "Compose saved-fence resolution with the fixed page projection. Receipt
   sources use their own already-authorized native Room adapter."
  [source request]
  (prepare-pages-in source (resolve-in source request)))
