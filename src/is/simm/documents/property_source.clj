(ns is.simm.documents.property-source
  "Pure lossless replacement of existing scalar frontmatter leaves. Not a
   schema guard, writer, authorization boundary or general YAML serializer."
  (:require [clojure.string :as str]
            [is.simm.documents.markdown :as markdown])
  (:import [java.io StringReader]
           [org.yaml.snakeyaml Yaml LoaderOptions]
           [org.yaml.snakeyaml.constructor SafeConstructor]
           [org.yaml.snakeyaml.error YAMLException]
           [org.yaml.snakeyaml.nodes Node MappingNode SequenceNode ScalarNode Tag]))

(def profile {:id :simmis/markdown-property-source :version 1 :positions :utf16
              :features #{:existing-scalar-replacement}})
(def ^:private max-nodes 20000)

(defn- refuse! [reason]
  (throw (ex-info "This property requires source editing" {:type ::unsupported :reason reason})))

(defn- compose [raw]
  (let [policy (markdown/yaml-options)
        options (doto (LoaderOptions.)
                  (.setAllowDuplicateKeys (:allow-duplicate-keys policy))
                  (.setAllowRecursiveKeys (:allow-recursive-keys policy))
                  (.setMaxAliasesForCollections (:max-aliases-for-collections policy))
                  (.setNestingDepthLimit (:nesting-depth-limit policy))
                  (.setCodePointLimit (:code-point-limit policy)))]
    (try (.compose (Yaml. (SafeConstructor. options)) (StringReader. raw))
         (catch YAMLException _ (refuse! :unsupported-yaml-structure)))))

(defn- children [node]
  (cond
    (instance? MappingNode node) (mapcat (fn [tuple] [(.getKeyNode tuple) (.getValueNode tuple)]) (.getValue ^MappingNode node))
    (instance? SequenceNode node) (.getValue ^SequenceNode node)
    :else []))

(defn- source-tree! [root]
  ;; Parser-node identity is local to this one call; no editor/global state.
  (let [seen (java.util.IdentityHashMap.)]
    (loop [pending [root], remaining max-nodes]
      (when-let [^Node node (peek pending)]
        (when (or (zero? remaining) (.containsKey seen node) (.getAnchor node))
          (refuse! :aliased-anchored-or-oversized-yaml))
        (.put seen node true)
        (recur (into (pop pending) (children node)) (dec remaining))))))

(defn- leaf! [root path]
  (reduce (fn [node key]
            (when-not (instance? MappingNode node) (refuse! :not-a-mapping-path))
            (or (some (fn [tuple]
                        (let [k (.getKeyNode tuple)]
                          (when (and (instance? ScalarNode k) (= Tag/STR (.getTag ^Node k))
                                     (= key (.getValue ^ScalarNode k)))
                            (.getValueNode tuple)))) (.getValue ^MappingNode node))
                (refuse! :missing-property))) root path))

(defn- utf16 [^String raw mark]
  (.offsetByCodePoints raw 0 (.getIndex mark)))

(defn scalar-span
  "Find one existing mapping leaf only AFTER the strict Markdown/YAML parse.
   SnakeYAML marks count code points; convert against the ORIGINAL raw YAML.
   Anchors/aliases, multiline styles and explicit tags deliberately refuse."
  [parsed path]
  (when-not (and (vector? path) (<= 1 (count path) 30) (every? string? path)
                 (empty? (:diagnostics parsed)) (:frontmatter parsed))
    (refuse! :invalid-source-or-path))
  (let [{:keys [raw value-from]} (:frontmatter parsed)
        tree (compose raw)
        _ (source-tree! tree)
        node (leaf! tree path)]
    (when-not (and (instance? ScalarNode node)
                   (contains? #{"PLAIN" "SINGLE_QUOTED" "DOUBLE_QUOTED"} (.name (.getScalarStyle ^ScalarNode node))))
      (refuse! :not-a-single-line-scalar))
    (let [from (+ value-from (utf16 raw (.getStartMark ^Node node)))
          to (+ value-from (utf16 raw (.getEndMark ^Node node)))
          text (subs (:source parsed) from to)]
      (when (or (= from to) (re-find #"[\r\n]" text) (str/starts-with? text "!"))
        (refuse! :multiline-or-explicitly-tagged-scalar))
      {:from from :to to :style (.name (.getScalarStyle ^ScalarNode node))
       :value (get-in parsed (into [:frontmatter :properties] path))})))

(defn- scalar-text [value style]
  (cond
    (nil? value) "null"
    (boolean? value) (str value)
    (and (integer? value) (<= Long/MIN_VALUE value Long/MAX_VALUE)) (str (long value))
    (and (number? value) (Double/isFinite (double value))) (str (double value))
    (string? value) (if (and (= "SINGLE_QUOTED" style) (not (re-find #"[\p{Cntrl}]" value)))
                      (str "'" (str/replace value "'" "''") "'")
                      (pr-str value))
    :else (refuse! :unsupported-scalar-value)))

(defn replace-scalar
  "Patch exactly one value token in an expected revision. No-op values return
   original bytes. Reparse verifies the ONE intended metadata change and an
   unchanged body; no source publication occurs here."
  [source revision path value]
  (let [parsed (markdown/parse source)]
    (when-not (= revision (:revision parsed))
      (throw (ex-info "Property source changed" {:type ::conflict})))
    (let [{:keys [from to style] old :value} (scalar-span parsed path)]
      (if (= old value)
        parsed
        (let [after (markdown/patch source revision [{:from from :to to :text (scalar-text value style)}])]
          (when-not (and (empty? (:diagnostics after))
                         (= (assoc-in (get-in parsed [:frontmatter :properties]) path value)
                            (get-in after [:frontmatter :properties]))
                         (= (subs source (:body-from parsed)) (subs (:source after) (:body-from after))))
            (refuse! :scalar-did-not-roundtrip))
          after)))))
