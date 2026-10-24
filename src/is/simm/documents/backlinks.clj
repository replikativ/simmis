(ns is.simm.documents.backlinks
  "Bounded backlinks for canonical Memory URLs in the current owned workspace.
   Relative/wiki spellings and frozen-input citations are not guessed into live
   files. Results describe one source selection, not a permanent/global index."
  (:require [is.simm.documents.block-reference :as source]
            [is.simm.documents.files :as files]
            [is.simm.documents.links :as links]
            [is.simm.documents.markdown :as markdown]
            [hasch.core :as hasch]
            [jsonista.core :as json]))

(def profile {:id :simmis/canonical-memory-backlinks :version 1
              :scope :current-workspace-canonical-urls
              :syntax markdown/syntax-profile :max-links 5000
              :max-results 50 :max-reply-bytes 131072 :reply-envelope-reserve-bytes 256})

(defn- refuse! [reason]
  (throw (ex-info "Memory backlinks unavailable" {:type ::refused :reason reason})))

(defn request! [{:keys [repository-id target] :as request}]
  (when-not (and (map? request) (= #{:repository-id :target} (set (keys request)))
                 (uuid? repository-id) (map? target)
                 (case (:kind target)
                   :file (and (= #{:kind :path} (set (keys target)))
                              (links/path? (:path target))
                              (= :markdown (files/file-kind (:path target)))
                              (try (files/path! (:path target)) true (catch Exception _ false)))
                   :block (and (= #{:kind :id} (set (keys target))) (uuid? (:id target)))
                   false))
    (refuse! :invalid-request))
  request)

(defn- explicit-blocks [semantic]
  (filterv #(= :explicit (:identity-status %))
           (mapcat #(tree-seq (comp seq :children) :children %) (:blocks semantic))))

(defn- source-target [room path blocks reference]
  (let [containing (first (sort-by #(- (:to %) (:from %))
                                  (filter #(<= (:from %) (:from reference) (:to reference) (:to %)) blocks)))]
    (cond-> {:kind :memory-file :room room :path path}
      containing (assoc :kind :memory-block :block (str (:id containing))))))

(defn- canonical-target [room repository paths blocks reference]
  (when (= :link (:kind reference))
    (when-let [target (links/parse-route (:target reference))]
      (when (= room (:room target))
        (case (:kind target)
          :memory-file (when (contains? paths (:path target)) {:kind :file :path (:path target)})
          :memory-block (when (= (str repository) (:repository target))
                          (when-let [block (get blocks (parse-uuid (:block target)))]
                            {:kind :block :id (:id block) :path (:path block)}))
          nil)))))

(defn selected
  "HOST-only query over a verified selected snapshot. Parsed trees stay local;
   reply count and encoded bytes are bounded independently from source limits."
  [db selection room query]
  (let [{:keys [repository-id target]} (request! query)]
    (when-not (and (links/uuid-text? room) (= repository-id (:repository-id selection)))
      (refuse! :repository-changed))
    (let [documents (:documents (source/scan-selected db selection))
          by-path (into (sorted-map) (map (fn [document]
                                          [(:path document) (assoc document :blocks (explicit-blocks (:semantic document)))])) documents)
          explicit (mapcat (fn [[path document]] (map #(assoc % :path path) (:blocks document))) by-path)
          identities (concat (keep #(when (= :explicit (get-in % [:document :identity-status]))
                                     (get-in % [:document :id])) documents) (map :id explicit))
          _ (when-not (= (count identities) (count (set identities))) (refuse! :ambiguous-source-identities))
          by-block (into {} (map (juxt :id identity)) explicit)
          target-path (case (:kind target) :file (:path target) :block (:path (get by-block (:id target))))
          found? (contains? by-path target-path)
          basis (hasch/uuid [profile selection])
          base {:profile profile :repository-id repository-id :source-basis basis
                :source-count (count documents) :target (cond-> target target-path (assoc :path target-path))
                :status (if found? :ready :missing) :complete? true :results []}
          references (for [[path {:keys [entry parsed blocks]}] by-path
                           reference (markdown/link-references parsed)]
                       {:path path :entry entry :parsed parsed :blocks blocks :reference reference})
          bounded (vec (take (inc (:max-links profile)) references))]
      (when (> (count bounded) (:max-links profile)) (refuse! :too-many-links))
      (if-not found?
        base
        (reduce
         (fn [reply {:keys [path entry parsed blocks reference]}]
           (let [destination (canonical-target room repository-id by-path by-block reference)
                 matches? (case (:kind target)
                            :file (= target-path (:path destination))
                            :block (= (:id target) (:id destination)))]
             (if-not matches?
               reply
               (let [origin (assoc (source-target room path blocks reference) :repository (str repository-id))
                     url (links/route origin)
                     _ (when-not url (refuse! :unaddressable-source))
                     result (cond-> {:path path :entry entry :revision (:revision parsed)
                                     :from (:from reference) :to (:to reference) :url url}
                              (:block origin) (assoc :block-id (parse-uuid (:block origin))))
                     proposed (update reply :results conj result)]
                 (if (or (> (count (:results proposed)) (:max-results profile))
                         (> (+ (:reply-envelope-reserve-bytes profile)
                               (alength ^bytes (json/write-value-as-bytes proposed))) (:max-reply-bytes profile)))
                   (reduced (assoc reply :status :limited :complete? false))
                   proposed))))) base bounded)))))

(defn in [db room query]
  (selected db (source/selection db) room query))
