(ns is.simm.documents.starter-bundle
  "Bounded inert sandbox source data. Never a runtime classpath dependency.
   Integrity is not authority: a host supplying an arbitrary bundle owns its
   provenance claim. The packaged bundle additionally matches a reviewed pin."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.io PushbackReader StringReader]
           [java.nio ByteBuffer CharBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]
           [java.security MessageDigest]))

(def upstream "https://github.com/replikativ/dvergr-sandbox")
(def commit "6e5315f7f96146bfb9205bacd20c868ba1766ae4")
(def resource "is/simm/starter/dvergr-sandbox.edn")
(def packaged-digest "577b757c7d94e0ac0af4b93c63ca9755565a77144760aa7c07954eaa87312506")
(def max-files 128)
(def max-file-bytes (* 2 1024 1024))
(def max-total-bytes (* 8 1024 1024))
(def max-resource-bytes (* 16 1024 1024))

(defn- refuse! [reason]
  (throw (ex-info "Sandbox starter bundle refused" {:type ::refused :reason reason})))

(defn utf8-bytes [text]
  (when-not (and (string? text) (<= (count text) max-resource-bytes)) (refuse! :invalid-text))
  (let [buffer (.encode (doto (.newEncoder StandardCharsets/UTF_8)
                          (.onMalformedInput CodingErrorAction/REPORT)
                          (.onUnmappableCharacter CodingErrorAction/REPORT))
                        (CharBuffer/wrap text))
        bytes (byte-array (.remaining buffer))]
    (.get buffer bytes)
    bytes))

(defn decode-utf8 [bytes]
  (str (.decode (doto (.newDecoder StandardCharsets/UTF_8)
                  (.onMalformedInput CodingErrorAction/REPORT)
                  (.onUnmappableCharacter CodingErrorAction/REPORT))
                (ByteBuffer/wrap bytes))))

(defn sha256 [bytes]
  (apply str (map #(format "%02x" (bit-and 255 %)) (.digest (MessageDigest/getInstance "SHA-256") bytes))))

(defn allowed-path? [path]
  (and (string? path) (<= 1 (count path) 512)
       (not (re-find #"[\\\p{Cntrl}]" path))
       (every? #(and (not (str/blank? %)) (not (str/starts-with? % "."))) (str/split path #"/" -1))
       (or (contains? #{"user.clj" "AGENTS.md" "README.md" "LICENSE"} path)
           (boolean (re-matches #"doc/[^/]+\.md" path))
           (and (str/starts-with? path "dvergr/") (str/ends-with? path ".clj")))))

(defn- digest [files]
  (sha256 (utf8-bytes (pr-str (mapv (fn [[path row]] [path (:mode row) (:size row) (:sha256 row)])
                                    (sort-by key files))))))

(defn verify!
  "Return immutable validated data. Self-consistency is not Git attestation;
   only trusted build acquisition establishes the original source provenance."
  [bundle]
  (when-not (and (map? bundle) (= #{:version :upstream :commit :files :digest} (set (keys bundle)))
                 (= 1 (:version bundle)) (= upstream (:upstream bundle)) (= commit (:commit bundle))
                 (map? (:files bundle)) (<= 1 (count (:files bundle)) max-files))
    (refuse! :invalid-manifest))
  (let [sizes (for [[path row] (:files bundle)]
                (do
                  (when-not (and (allowed-path? path) (map? row)
                                 (= #{:mode :size :sha256 :text} (set (keys row)))
                                 (= 33188 (:mode row)) (integer? (:size row))
                                 (<= 0 (:size row) max-file-bytes) (string? (:text row))
                                 (<= (count (:text row)) max-file-bytes))
                    (refuse! :invalid-file))
                  (let [bytes (utf8-bytes (:text row))]
                    (when-not (and (= (:size row) (alength bytes)) (= (:sha256 row) (sha256 bytes)))
                      (refuse! :file-integrity-mismatch))
                    (alength bytes))))]
    (when (> (reduce + 0 sizes) max-total-bytes) (refuse! :too-many-bytes))
    (when-not (= (:digest bundle) (digest (:files bundle))) (refuse! :manifest-integrity-mismatch))
    bundle))

(defn assemble
  "Build tooling/test helper. Caller owns upstream provenance; values are
   immutable strict UTF-8 text, not borrowed mutable byte arrays."
  [texts]
  (when-not (and (map? texts) (<= 1 (count texts) max-files)) (refuse! :invalid-files))
  (let [files (into (sorted-map)
                    (map (fn [[path text]]
                           (when-not (allowed-path? path) (refuse! :invalid-path))
                           (let [bytes (utf8-bytes text)]
                             [path {:mode 33188 :size (alength bytes) :sha256 (sha256 bytes) :text text}]))) texts)]
    (verify! {:version 1 :upstream upstream :commit commit :files files :digest (digest files)})))

(defn read-packaged!
  "Acquire data BEFORE Room intent. Bound serialized bytes before EDN parsing,
   refuse tags/trailing forms, and require the build-produced reviewed digest."
  []
  (let [url (or (io/resource resource) (refuse! :missing-resource))
        bytes (with-open [input (io/input-stream url)] (.readNBytes input (inc max-resource-bytes)))]
    (when (> (alength bytes) max-resource-bytes) (refuse! :oversized-resource))
    (with-open [reader (PushbackReader. (StringReader. (decode-utf8 bytes)))]
      (let [options {:readers {} :default (fn [& _] (refuse! :tagged-resource)) :eof ::eof}
            bundle (verify! (edn/read options reader))]
        (when-not (= ::eof (edn/read options reader)) (refuse! :trailing-resource))
        (when-not (= packaged-digest (:digest bundle)) (refuse! :unreviewed-resource))
        bundle))))

(defn adapt
  "Pure canonical Markdown adaptation. Only the trusted pinned plain-Markdown
   format is supported, not arbitrary frontmatter import. Original code/license
   bytes stay exact; modifications and original/installed hashes are explicit."
  [bundle repository-id]
  (verify! bundle)
  (when-not (uuid? repository-id) (refuse! :invalid-repository-id))
  (let [files (into (sorted-map)
                    (map (fn [[path {:keys [text]}]]
                           [path
                            (if-not (str/ends-with? path ".md") text
                                    (do
                                      (when (or (str/starts-with? text "\uFEFF")
                                                (re-find #"\A---(?:\r\n|\r|\n)" text))
                                        (refuse! :unsupported-template-frontmatter))
                                      (let [id (java.util.UUID/nameUUIDFromBytes
                                                (utf8-bytes (str repository-id "\n" commit "\n" path)))
                                            note (when (contains? #{"AGENTS.md" "README.md"} path)
                                                   (str "> Simmis installation note: `user.clj` is available source, "
                                                        "not auto-loaded or executed by this host. "
                                                        "Use explicit SCI evaluation/require. "
                                                        "The upstream auto-load claim below does not apply here.\n\n"))]
                                        (str "---\nsimmis-id: '" id "'\n---\n"
                                             "<!-- Modified by Simmis packaging: document identity added. "
                                             "Original/installed hashes: code/starter-provenance.edn. -->\n\n"
                                             note text))))]))
                    (:files bundle))]
    {:files files
     :provenance {:version 1 :upstream upstream :commit commit :manifest-digest (:digest bundle)
                  :modifications "Markdown receives repository-scoped identity; AGENTS/README clarify non-execution."
                  :files (into (sorted-map)
                               (map (fn [[path text]]
                                      (let [installed (sha256 (utf8-bytes text)) original (get-in bundle [:files path :sha256])]
                                        [path {:mode 33188 :original-sha256 original :installed-sha256 installed
                                               :modified? (not= original installed)}]))) files)}}))
