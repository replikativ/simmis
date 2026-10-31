(ns is.simm.ops.knowledge-outputs
  "Exact, bounded wiki deliverables captured before a Run world is settled.
   Portable evidence only: no live handles, publication or source-room writes."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.model.knowledge-scope :as scope]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.knowledge-workspace :as knowledge-workspace]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Arrays Base64]))

(def policy {:version 1 :max-files 100 :max-file-bytes 1048576 :max-total-bytes 2000000})

(defn- refuse! [reason]
  (throw (ex-info "Wiki output cannot be certified" {:type ::refused :reason reason})))

(defn target-path!
  "Exact target-relative Markdown name, never an absolute filesystem path."
  [path]
  (files/path! path)
  (when-not (and (str/starts-with? path "wiki/") (re-find #"(?i)\.(md|markdown)$" path))
    (refuse! :unsupported-path))
  path)

(def descriptor-schema
  [:map {:closed true} [:content :uuid]
   [:bytes [:int {:min 0 :max (:max-file-bytes policy)}]]
   [:mode [:= 33188]] [:base64 [:string {:max 1398104}]]])

(defn bytes!
  "Revalidate retained portable evidence, including exact strict UTF-8 bytes."
  [descriptor]
  (when-not (m/validate descriptor-schema descriptor) (refuse! :invalid-descriptor))
  (let [bytes (try (.decode (Base64/getDecoder) ^String (:base64 descriptor))
                   (catch IllegalArgumentException _ (refuse! :invalid-base64)))]
    (when-not (and (= (:bytes descriptor) (alength ^bytes bytes))
                   (= (:content descriptor) (blob/blob-id bytes))
                   (= (:base64 descriptor) (.encodeToString (Base64/getEncoder) bytes))
                   (<= (alength ^bytes bytes) (:max-file-bytes policy)))
      (refuse! :content-mismatch))
    (try
      (when-not (Arrays/equals ^bytes bytes ^bytes (markdown/source-bytes (markdown/decode bytes)))
        (refuse! :utf8-roundtrip))
      (catch java.nio.charset.CharacterCodingException _ (refuse! :invalid-utf8)))
    bytes))

(defn digest-tag [version]
  (case version 1 :simmis/knowledge-output-v1 2 :simmis/knowledge-output-v2))

(defn validate!
  [captured]
  (when-not (and (map? captured) (= (if (= 2 (:version captured)) #{:version :id :files :scope} #{:version :id :files}) (set (keys captured)))
                 (#{1 2} (:version captured)) (uuid? (:id captured)) (map? (:files captured))
                 (<= (count (:files captured)) (:max-files policy)))
    (refuse! :invalid-capture))
  (when (= 2 (:version captured)) (scope/normalize! (:scope captured)))
  (reduce-kv (fn [total path descriptor]
               (if (= 2 (:version captured)) (scope/target-path! (:scope captured) path) (target-path! path))
               (let [total (+ total (alength ^bytes (bytes! descriptor)))]
                 (when (> total (:max-total-bytes policy)) (refuse! :too-many-bytes))
                 total)) 0 (:files captured))
  (when-not (= (:id captured) (hasch/uuid [(digest-tag (:version captured)) (dissoc captured :id)]))
    (refuse! :capture-mismatch))
  captured)

(defn capture-db
  "ONE immutable Geschichte DB. Preflight all path/mode/size metadata before
   reading any body. Logical content reads handle chunked/delta files."
  ([db] (capture-db db nil))
  ([db output-scope]
  (when output-scope (scope/normalize! output-scope))
  (let [root (if output-scope (:output-root output-scope) "out/wiki")
        endpoint (str root "0")]
  (when (some #(seq (d/datoms db :avet :geschichte.work/path %)) ["out" root])
    (refuse! :file-at-output-ancestor))
  (let [rows (into [] (comp (take (inc (:max-files policy)))
                            (remove #(= endpoint (:v %))))
                   (d/index-range db {:attrid :geschichte.work/path :start (str root "/") :end endpoint}))
        _ (when (> (count rows) (:max-files policy)) (refuse! :too-many-files))
        entries (into (sorted-map)
                      (map (fn [row]
                             (let [source (:v row) target (if output-scope
                                                          (let [relative (subs source (inc (count root)))
                                                                dest (:destination-root output-scope)]
                                                            (str (when (seq dest) (str dest "/")) relative))
                                                          (subs source 4))
                                   entry (files/entry db source)]
                               (files/path! source)
                               (if output-scope (scope/target-path! output-scope target) (target-path! target))
                               (when-not (and (= 33188 (:mode entry)) (uuid? (:content entry))
                                              (integer? (:size entry))
                                              (<= 0 (:size entry) (:max-file-bytes policy))
                                              (= (:size entry)
                                                 (:geschichte.content/size
                                                  (d/entity db [:geschichte.content/id (:content entry)]))))
                                 (refuse! :unsupported-file))
                               [target entry]))) rows)]
    (doseq [path (keys entries)
            :let [parts (str/split path #"/")
                  parents (map #(str/join "/" (take % parts)) (range 1 (count parts)))]]
      (when (or (some #(contains? entries %) parents)
                (seq (d/datoms db :avet :geschichte.work-dir/path (if output-scope (str root "/" (subs path (if (seq (:destination-root output-scope)) (inc (count (:destination-root output-scope))) 0))) (str "out/" path)))))
        (refuse! :file-directory-collision)))
    (when (> (reduce + 0 (map (comp :size val) entries)) (:max-total-bytes policy))
      (refuse! :too-many-bytes))
    (let [reader (atom db)
          captured (cond-> {:version (if output-scope 2 1)
                    :files (into (sorted-map)
                                 (map (fn [[path {:keys [content size mode]}]]
                                        (let [bytes (content/read-by-id reader content)
                                              descriptor {:content content :bytes size :mode mode
                                                          :base64 (when bytes (.encodeToString (Base64/getEncoder) bytes))}]
                                          (bytes! descriptor)
                                          [path descriptor]))) entries)}
                     output-scope (assoc :scope output-scope))]
      (assoc captured :id (hasch/uuid [(digest-tag (:version captured)) captured])))))))

(defn with-capture
  "Versioned knowledge completion over an existing host verifier. Generic work
   and benchmark verifier identities stay unchanged. Called only for workflows
   declaring knowledge selection; fixtures capture evidence but gain no binding."
  ([base] (with-capture base nil))
  ([base output-scope]
  (when output-scope (scope/normalize! output-scope))
  (evaluation/make-evaluator
   {:id :simmis/knowledge-completion :version (if output-scope 2 1)
    :basis {:base (evaluation/evaluator-ref base) :capture (cond-> policy output-scope (assoc :version 2 :scope output-scope))}
    :tier (evaluation/evaluator-tier base)
    :capture (fn [{world :world/room :as context}]
               (binding [ec/*execution-context* (:ctx world)]
                 (let [conn (:conn (workspace/current-workspace))]
                   (when-not conn (refuse! :missing-workspace))
                   (let [captured (capture-db @conn output-scope)
                         legacy (when-let [capture (:capture base)] (capture context))]
                     (assoc legacy :knowledge-output captured)))))
    :observe (fn [context]
               (assoc ((:observe base) context)
                      :knowledge-output (get-in context [:execution/evidence :knowledge-output])))
    :verify (:verify base)})))

(defn with-publication-target
  "Bind any existing host evaluator, including the worker-specific one, to its
   actual selected workspace. Preserve base observation and grading; the extra
   proof is trusted capture evidence, never inferred from model text."
  [base target]
  (knowledge-workspace/expected-proof target)
  (evaluation/make-evaluator
   {:id :simmis/knowledge-workspace-completion :version 1
    :basis {:base (evaluation/evaluator-ref base) :target target}
    :tier (evaluation/evaluator-tier base)
    :capture (fn [{world :world/room :as context}]
               (binding [ec/*execution-context* (:ctx world)]
                 (let [proof (knowledge-workspace/target-proof! world target)
                       captured (when-let [capture (:capture base)] (capture context))]
                   (knowledge-workspace/target-proof! world target)
                   (assoc captured :knowledge-workspace proof))))
    :observe (fn [context]
               (assoc ((:observe base) context)
                      :knowledge-workspace (get-in context [:execution/evidence :knowledge-workspace])))
    :verify (fn [context evidence]
              (let [verdict ((:verify base) context evidence)
                    valid? (= (knowledge-workspace/expected-proof target) (:knowledge-workspace evidence))]
                (assoc verdict :checks (assoc (:checks verdict) :knowledge-workspace? valid?)
                       :reward (if valid? (:reward verdict) 0.0))))}))
