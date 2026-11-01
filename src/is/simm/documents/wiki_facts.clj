(ns is.simm.documents.wiki-facts
  "Bounded data-only structural observations for offline wiki scoring.
   No schema installation, truth judgement, publication permission or SCI parser."
  (:require [hasch.core :as hasch]
            [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.identity :as identity]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.metadata :as metadata]))

(def profile
  {:id :simmis/wiki-structural-facts :version 1
   :syntax markdown/syntax-profile :blocks blocks/profile :metadata metadata/profile
   :max-files 100 :max-file-bytes 524288 :max-total-bytes 2097152
   :max-blocks 20000 :max-links 20000})

(defn- refuse! [reason]
  (throw (ex-info "Wiki structural facts could not be constructed" {:type ::refused :reason reason})))

(def citation-profile
  (assoc profile :version 2
         :links :ordinary-markdown-links
         :prose :visible-text-excluding-code-html-and-image-subtrees))

(defn- observation-profile [version]
  (case version 1 profile 2 citation-profile (refuse! :unsupported-observation-version)))

(def ^:private excluded-prose-kinds
  #{:Code :FencedCodeBlock :IndentedCodeBlock :Image :ImageRef :WikiImage
    :HtmlInline :HtmlBlock :HtmlCommentBlock :HtmlInnerBlock})

(defn- visible-nodes [roots guard!]
  (mapcat (fn [node]
            (guard!)
            (when-not (contains? excluded-prose-kinds (:kind node))
              (cons node (visible-nodes (:children node) guard!)))) roots))

(defn- nodes [roots guard!]
  (map (fn [node] (guard!) node)
       (mapcat #(tree-seq (comp seq :children) :children %) roots)))

(defn- topology [roots parent guard!]
  (mapcat (fn [block]
            (guard!)
            (let [explicit? (= :explicit (:identity-status block))
                  id (when explicit? (str (:id block)))]
              (concat (when explicit? [{:id id :parent parent}])
                      (topology (:children block) (or id parent) guard!)))) roots))

(defn- declarations [scope guard!]
  (guard!)
  {:types (mapv str (:types scope))
   :properties (into (sorted-map)
                     (for [[alias binding] (:bindings scope)]
                       (do (guard!)
                           [alias {:id (str (:id binding)) :type (str (:declaring-type binding))}])))})

(defn document
  "Observe one source without returning its AST, values or native objects.
   The guard belongs to the caller's already-started scoring resource account.
   Relative ordering covers explicit blocks, independent of provisional nodes."
  ([repository path source guard!] (document repository path source guard! 1))
  ([repository path source guard! version]
   (observation-profile version)
   (when-not (fn? guard!) (refuse! :missing-resource-guard))
   (guard!)
   (when-not (and (uuid? repository) (string? path) (<= 1 (count path) 512)
                  (string? source))
     (refuse! :invalid-document))
   (if (> (alength (.getBytes ^String source "UTF-8")) (:max-file-bytes profile))
     {:path path :valid? false :diagnostic-kinds [:file-too-large]}
     (let [parsed (markdown/parse source)
           _ (guard!)
           document (identity/document repository path parsed)
           projection (when (:id document) (blocks/project (:id document) parsed))
           explicit (vec (topology (:blocks projection) nil guard!))
           _ (when (> (count explicit) (:max-blocks profile)) (refuse! :too-many-blocks))
           decoded (metadata/decode (get-in parsed [:frontmatter :properties])
                                    (set (map (comp parse-uuid :id) explicit)))
           _ (guard!)
           visible (when (= 2 version) (vec (visible-nodes (:nodes parsed) guard!)))
           links (if (= 1 version)
                   (vec (keep :url (nodes (:nodes parsed) guard!)))
                   (into [] (comp (filter #(contains? #{:Link :LinkRef} (:kind %))) (keep :url)) visible))
           _ (when (> (count links) (:max-links profile)) (refuse! :too-many-links))
           diagnostics (vec (sort (set (map :kind (concat (:diagnostics parsed) (:diagnostics document)
                                                          (:diagnostics projection) (:diagnostics decoded))))))
           page-id (when (= :explicit (:identity-status document)) (str (:id document)))]
       (cond-> {:path path :revision (str (:revision parsed)) :page-id page-id
                :identity-status (:identity-status document)
                :valid? (boolean (and (not= :invalid (:identity-status document))
                                      (= :ready (:status projection)) (#{:plain :ready} (:status decoded))
                                      (empty? diagnostics)))
                :diagnostic-kinds diagnostics :metadata-status (:status decoded)
                :metadata-digest (str (hasch/uuid (:raw decoded)))
                :declarations {:page (declarations (:page decoded) guard!)
                               :blocks (into (sorted-map)
                                             (for [[id scope] (:blocks decoded)]
                                               [(str id) (declarations scope guard!)]))}
                :blocks explicit :links links
                :ids (into (cond-> [] page-id (conj page-id)) (map :id explicit))}
         (= 2 version) (assoc :prose-text (str/join " " (keep :text visible))))))))

(defn documents
  "Observe a bounded map of Markdown sources in stable path order. No IO."
  ([repository sources guard!] (documents repository sources guard! 1))
  ([repository sources guard! version]
   (let [selected (observation-profile version)]
     (guard!)
     (when-not (and (map? sources) (<= (count sources) (:max-files profile))
                    (every? (fn [[path source]] (and (string? path) (string? source))) sources))
       (refuse! :invalid-sources))
     (let [size (reduce (fn [total source]
                          (guard!)
                          (let [total (+ total (alength (.getBytes ^String source "UTF-8")))]
                            (when (> total (:max-total-bytes profile)) (refuse! :too-many-bytes))
                            total)) 0 (vals sources))]
       (guard!)
       {:profile selected :source-bytes size
        :files (into (sorted-map)
                     (for [[path source] (sort-by key sources)]
                       [path (document repository path source guard! version)]))}))))
