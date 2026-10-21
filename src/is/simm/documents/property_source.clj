(ns is.simm.documents.property-source
  "Pure lossless edits of bounded frontmatter tokens and missing entries. Not a
   schema guard, writer, authorization boundary or general YAML serializer."
  (:require [clojure.string :as str]
            [is.simm.documents.markdown :as markdown])
  (:import [java.io StringReader]
           [org.yaml.snakeyaml Yaml LoaderOptions]
           [org.yaml.snakeyaml.constructor SafeConstructor]
           [org.yaml.snakeyaml.error YAMLException]
           [org.yaml.snakeyaml.nodes Node MappingNode SequenceNode ScalarNode Tag]))

(def profile {:id :simmis/markdown-property-source :version 2 :positions :utf16
              :features #{:existing-scalar-replacement :missing-mapping-insertion :type-sequence-replacement}})
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

(defn- checked-source [source revision path]
  (let [parsed (markdown/parse source)]
    (when-not (= revision (:revision parsed))
      (throw (ex-info "Property source changed" {:type ::conflict})))
    (when-not (and (vector? path) (<= 1 (count path) 30)
                   (every? #(and (string? %) (<= 1 (count %) 128)) path)
                   (empty? (:diagnostics parsed)) (:frontmatter parsed))
      (refuse! :invalid-source-or-path))
    parsed))

(defn- new-value-text [value]
  ;; Serialize NEW data only, never existing mappings/comments. JSON-style flow
  ;; syntax is also safe YAML. Roundtrip checks below reject lossy numeric input.
  (let [remaining (volatile! 1000)
        bytes-left (volatile! markdown/max-source-bytes)
        output (StringBuilder.)]
    (letfn [(emit! [^String text]
              (when (neg? (vswap! bytes-left - (alength (.getBytes text java.nio.charset.StandardCharsets/UTF_8))))
                (refuse! :oversized-new-value))
              (.append output text))
            (render [v depth]
              (when (or (> depth 20) (neg? (vswap! remaining dec)))
                (refuse! :oversized-new-value))
              (cond
                (map? v)
                (do (when-not (and (<= (count v) 64)
                                   (every? #(and (string? %) (<= 1 (count %) 128)) (keys v)))
                      (refuse! :unsupported-new-mapping))
                    (emit! "{")
                    (doseq [[index [k item]] (map-indexed vector (sort-by key v))]
                      (when (pos? index) (emit! ", "))
                      (emit! (pr-str k)) (emit! ": ") (render item (inc depth)))
                    (emit! "}"))
                (vector? v)
                (do (when (> (count v) 64) (refuse! :oversized-new-value))
                    (emit! "[")
                    (doseq [[index item] (map-indexed vector v)]
                      (when (pos? index) (emit! ", "))
                      (render item (inc depth)))
                    (emit! "]"))
                (and (string? v) (> (count v) 65536)) (refuse! :oversized-new-value)
                :else (emit! (scalar-text v "DOUBLE_QUOTED"))))]
      (render value 0)
      (str output))))

(defn- checked-patch [parsed path value patch]
  (let [after (markdown/patch (:source parsed) (:revision parsed) [patch])]
    (when-not (and (empty? (:diagnostics after))
                   (= (assoc-in (get-in parsed [:frontmatter :properties]) path value)
                      (get-in after [:frontmatter :properties]))
                   (= (subs (:source parsed) (:body-from parsed))
                      (subs (:source after) (:body-from after))))
      (refuse! :declaration-did-not-roundtrip))
    after))

(defn insert-value
  "Insert one MISSING metadata path. Existing ancestor mappings and their bytes
   remain intact; only missing descendants are synthesized. Not a schema or
   authorization check. Existing values (including null) are never replaced."
  [source revision path value]
  (let [parsed (checked-source source revision path)
        {:keys [raw value-from]} (:frontmatter parsed)
        tree (compose raw)
        newline (or (re-find #"\r\n|\n|\r" source) "\n")]
    (source-tree! tree)
    (loop [node tree, keys-left path]
      (when-not (or (nil? node) (instance? MappingNode node)) (refuse! :not-a-mapping-path))
      (let [key (first keys-left)
            tuples (when node (.getValue ^MappingNode node))
            existing (some (fn [tuple]
                             (let [k (.getKeyNode tuple)]
                               (when (and (instance? ScalarNode k) (= Tag/STR (.getTag ^Node k))
                                          (= key (.getValue ^ScalarNode k)))
                                 (.getValueNode tuple)))) tuples)]
        (if existing
          (if (next keys-left) (recur existing (next keys-left)) (refuse! :existing-property))
          (let [inserted (if (next keys-left) (assoc-in {} (vec (next keys-left)) value) value)
                entry (str (pr-str key) ": " (new-value-text inserted))
                [offset text]
                (cond
                  (nil? node) [value-from (str entry newline)]
                  (= "FLOW" (.name (.getFlowStyle ^MappingNode node)))
                  (let [start (utf16 raw (.getStartMark ^Node node))]
                    (when-not (= \{ (.charAt ^String raw start)) (refuse! :unsupported-mapping-form))
                    [(+ value-from start 1) (str entry (when (seq tuples) ", "))])
                  (seq tuples)
                  (let [key-node (.getKeyNode (first tuples))
                        start (utf16 raw (.getStartMark ^Node key-node))
                        line-start (inc (max (.lastIndexOf ^String raw "\n" (dec start))
                                             (.lastIndexOf ^String raw "\r" (dec start))))
                        indent (subs raw line-start start)]
                    (when-not (re-matches #" *" indent) (refuse! :unsupported-mapping-form))
                    [(+ value-from start) (str entry newline indent)])
                  :else (refuse! :unsupported-mapping-form))]
            (checked-patch parsed path value {:from offset :to offset :text text})))))))

(defn set-types
  "Replace only a selected explicit type sequence, or insert it when missing.
   Existing sequences containing comments require Source editing: comments are
   never silently consumed. This does not decide which types are assignable."
  [source revision path types]
  (when-not (and (vector? types) (<= (count types) 64) (every? string? types)
                 (= "types" (last path)))
    (refuse! :invalid-type-sequence))
  (let [parsed (checked-source source revision path)
        metadata (get-in parsed [:frontmatter :properties])
        parent (get-in metadata (pop path))]
    (if-not (and (map? parent) (contains? parent (peek path)))
      (insert-value source revision path types)
      (if (= types (get-in metadata path))
        parsed
        (let [{:keys [raw value-from]} (:frontmatter parsed)
              tree (compose raw)
              _ (source-tree! tree)
              node (leaf! tree path)]
          (when-not (and (instance? SequenceNode node)
                         (every? #(and (instance? ScalarNode %) (= Tag/STR (.getTag ^Node %)))
                                 (.getValue ^SequenceNode node)))
            (refuse! :unsupported-type-sequence))
          (doseq [^Node child (.getValue ^SequenceNode node)]
            (when (str/starts-with? (subs raw (utf16 raw (.getStartMark child))
                                          (utf16 raw (.getEndMark child))) "!")
              (refuse! :commented-or-tagged-type-sequence)))
          (let [from (+ value-from (utf16 raw (.getStartMark ^Node node)))
                end (+ value-from (utf16 raw (.getEndMark ^Node node)))
                token (subs source from end)
                ;; Block sequence end marks may include the next line's indent.
                ;; Leave that exact suffix in place, including CRLF and spaces.
                core (str/replace token #"\s+\z" "")
                to (+ from (count core))]
            (when (or (str/includes? token "#") (str/starts-with? core "!"))
              (refuse! :commented-or-tagged-type-sequence))
            ;; YAML permits an indentless BLOCK sequence beneath a mapping
            ;; key, but a FLOW sequence needs greater indentation than its key.
            (checked-patch parsed path types
                           {:from from :to to
                            :text (str (when (= "BLOCK" (.name (.getFlowStyle ^SequenceNode node))) "  ")
                                       (new-value-text types))})))))))
