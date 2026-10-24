(ns is.simm.documents.cells
  "Derive bounded Clojure cells from immutable saved Markdown, never client code.
   The owned host separately authorizes and revalidates the original file entry."
  (:require [clojure.string :as str]
            [is.simm.documents.code-cells :as cells]
            [is.simm.documents.evaluation :as evaluation]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]))

(def max-source-span 65536)

(defn- refuse! [reason message]
  (throw (ex-info message {:type ::refused :reason reason})))

(defn validate-request!
  [{:keys [path repository-id expected revision from to block namespace expected-target-stamp] :as request}]
  (when-not (and (map? request)
                 (= #{:path :repository-id :expected :revision :from :to :block :namespace :expected-target-stamp}
                    (set (keys request)))
                 (string? path) (= :markdown (files/file-kind path))
                 (uuid? repository-id) (uuid? revision) (uuid? expected-target-stamp)
                 (or (nil? block) (uuid? block))
                 (map? expected) (= #{:content :size :mode} (set (keys expected)))
                 (uuid? (:content expected)) (integer? (:size expected))
                 (<= 0 (:size expected) markdown/max-source-bytes)
                 (integer? (:mode expected)) (<= 0 (:mode expected) 65535)
                 (= 0x8000 (bit-and (long (:mode expected)) 0xF000))
                 (integer? from) (integer? to) (<= 0 from) (< from to)
                 (<= to markdown/max-source-bytes) (<= (- to from) max-source-span))
    (refuse! :invalid-request "Choose a bounded saved Markdown code cell."))
  (files/path! path)
  (evaluation/validate-repl! {:text "()" :syntax :clojure :namespace namespace})
  request)

(defn- explicit-owner [analysis from to]
  (->> (get-in analysis [:semantic :blocks])
       (mapcat #(tree-seq (comp seq :children) :children %))
       (filter #(and (= :explicit (:identity-status %)) (<= (:from %) from to (:to %))))
       (sort-by #(- (:to %) (:from %)))
       first :id))

(defn inspect
  "Certify exactly one parsed fence in the supplied saved file. Source offsets
   are UTF-16; nested Markdown prefixes are interpreted by the parser. Preserve
   both the original source span and the exact parser-derived executed literal."
  [{:keys [repository-id path entry kind text analysis]} request]
  (validate-request! request)
  (when-not (and (= :markdown kind) (string? text)
                 (= repository-id (:repository-id request)) (= path (:path request))
                 (= entry (:expected request)) (= (:revision analysis) (:revision request))
                 (= (:revision analysis) (markdown/revision text))
                 (= :ready (get-in analysis [:semantic :status])))
    (refuse! :source-changed "The saved Markdown changed or its structure is invalid. Review it before running the cell."))
  (let [{:keys [from to]} request
        matches (filter #(and (= :FencedCodeBlock (:kind %)) (= from (:from %)) (= to (:to %)))
                        (mapcat #(tree-seq (comp seq :children) :children %) (:nodes analysis)))
        node (when (= 1 (count matches)) (first matches))
        owner (explicit-owner analysis from to)
        code (:literal node)]
    (when-not (and node (cells/clojure-fence? node)
                   (or (nil? (:block request)) (= owner (:block request))))
      (refuse! :unsupported-cell "Only the exact saved clj or clojure fenced cell can be run."))
    (when-not (and (string? code) (<= 1 (count code) cells/max-code-characters) (not (str/blank? code)))
      (refuse! :cell-limit "A Clojure cell must contain code and be at most 16K characters. Move larger programs to a file."))
    ;; Reuse the human REPL's namespace and source validation, without claiming
    ;; that an ordinary scratch request is certified saved source.
    (evaluation/validate-repl! {:text code :syntax :clojure :namespace (:namespace request)})
    {:code code
     :source {:kind :saved-markdown-cell :repository-id repository-id :path path :entry entry
              :document-id (:document-id analysis) :document-identity-status (:identity-status analysis)
              :revision (:revision analysis) :target-stamp (:expected-target-stamp request)
              :from from :to to :block owner :language (cells/language (:language node))
              :backend :clj :original (subs text from to)}}))
