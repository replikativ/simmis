(ns is.simm.media.office-test
  "Drives the whole office-document path the way an AGENT drives it: a REAL
   .docx and .pptx (written by POI) and a minimal .odt (a hand-built ODF zip),
   opened through a REAL SCI sandbox with `office/*` + `clojure.zip` installed —
   open → read text → replace text / structural-edit a part → save a patched
   copy → confirm the edit landed AND that every untouched part is byte-identical.

   Fixtures are built with POI (already a dependency) rather than LibreOffice, so
   the suite has no external-tool dependency. The filesystem is a temp dir;
   `office/sci-namespace` takes the fs as a parameter precisely so the model is
   testable without booting a room."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [is.simm.media.office :as office]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [clojure.zip]
            [sci.core :as sci])
  (:import [java.io File FileOutputStream ByteArrayOutputStream]
           [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.util.zip ZipOutputStream ZipEntry]
           [org.apache.poi.xwpf.usermodel XWPFDocument]
           [org.apache.poi.xslf.usermodel XMLSlideShow XSLFTextBox]))

;; =============================================================================
;; Fixtures — one real doc per format, each carrying the phrase we edit
;; =============================================================================

(def ^:private phrase "Revenue grew 18% in Q3, driven by the EU segment.")

(defn- write-docx! ^File [^File dir]
  (let [f (io/file dir "report.docx")]
    (with-open [doc (XWPFDocument.)
                out (FileOutputStream. f)]
      (-> (.createParagraph doc) .createRun (.setText phrase))
      (.write doc out))
    f))

(defn- write-pptx! ^File [^File dir]
  (let [f (io/file dir "deck.pptx")]
    (with-open [ppt (XMLSlideShow.)
                out (FileOutputStream. f)]
      (let [slide (.createSlide ppt)
            ^XSLFTextBox tb (.createTextBox slide)]
        (.setText tb phrase))
      (.write ppt out))
    f))

(defn- write-odt! ^File [^File dir]
  (let [f (io/file dir "notes.odt")
        content (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                     "<office:document-content"
                     " xmlns:office=\"urn:oasis:names:tc:opendocument:xmlns:office:1.0\""
                     " xmlns:text=\"urn:oasis:names:tc:opendocument:xmlns:text:1.0\">"
                     "<office:body><office:text>"
                     "<text:p>" phrase "</text:p>"
                     "</office:text></office:body></office:document-content>")
        manifest (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                      "<manifest:manifest"
                      " xmlns:manifest=\"urn:oasis:names:tc:opendocument:xmlns:manifest:1.0\">"
                      "<manifest:file-entry manifest:full-path=\"/\""
                      " manifest:media-type=\"application/vnd.oasis.opendocument.text\"/>"
                      "<manifest:file-entry manifest:full-path=\"content.xml\""
                      " manifest:media-type=\"text/xml\"/></manifest:manifest>")]
    (with-open [out (FileOutputStream. f)
                zos (ZipOutputStream. out)]
      ;; mimetype FIRST and STORED, per the ODF package rule
      (let [mt (.getBytes "application/vnd.oasis.opendocument.text" "UTF-8")
            e (doto (ZipEntry. "mimetype")
                (.setMethod ZipEntry/STORED)
                (.setSize (alength mt))
                (.setCompressedSize (alength mt))
                (.setCrc (let [c (java.util.zip.CRC32.)] (.update c mt) (.getValue c))))]
        (.putNextEntry zos e) (.write zos mt) (.closeEntry zos))
      (doseq [[nm ^String s] [["content.xml" content] ["META-INF/manifest.xml" manifest]]]
        (.putNextEntry zos (ZipEntry. nm))
        (.write zos (.getBytes s "UTF-8"))
        (.closeEntry zos)))
    f))

(def ^:dynamic *dir* nil)

(use-fixtures :each
  (fn [t]
    (let [dir (.toFile (Files/createTempDirectory "simmis-office-test"
                                                  (make-array FileAttribute 0)))
          room (discourse/make-room {:id (keyword (str "office-test-" (random-uuid))) :store (memory/make)})]
      (try
        (write-docx! dir) (write-pptx! dir) (write-odt! dir)
        (binding [*dir* dir ec/*execution-context* (:ctx room)] (t))
        (finally
          (discourse/close-room! room)
          (run! io/delete-file (reverse (file-seq dir))))))))

;; =============================================================================
;; The agent's sandbox: a real SCI ctx with office/* + clojure.zip over the dir
;; =============================================================================

(defn- temp-fs []
  {:read-bytes (fn [path]
                 (let [f (io/file *dir* (str/replace path #"^/" ""))]
                   (when (.exists f) (Files/readAllBytes (.toPath f)))))
   :stat (fn [path]
           (let [f (io/file *dir* (str/replace path #"^/" ""))]
             (when (.exists f) {:size (.length f) :type :file})))
   :write-file! (fn [path ^bytes bs _mime]
                  (let [f (io/file *dir* (str/replace path #"^/" ""))]
                    (io/make-parents f)
                    (with-open [o (FileOutputStream. f)] (.write o bs))
                    {:fs.node/id (random-uuid) :fs.node/name path}))})

(defn- sandbox []
  (let [ctx (sci/init {})]
    (sci/add-namespace! ctx 'office (office/sci-namespace (temp-fs) :test-room))
    (sci/add-namespace! ctx 'clojure.zip
                        (sci/copy-ns clojure.zip (sci/create-ns 'clojure.zip nil)))
    ctx))

(defn- ev [ctx code] (sci/eval-string* ctx code))

(defn- read-fixture-parts [name]
  (office/read-parts (Files/readAllBytes (.toPath (io/file *dir* name)))))

(deftest office-staging-follows-the-execution-world
  (let [api (office/sci-namespace (temp-fs) :room)
        other (office/sci-namespace (temp-fs) :room)
        h ((api 'open) "/report.docx")
        fork (ygg/fork!)
        copied? (volatile! false)]
    (try
      (is (= :unknown-handle (:error ((other 'parts) h))))
      (is (not (contains? ((other 'parts) h) :open)) "no handle enumeration")
      (binding [ec/*execution-context* (:child-ctx fork)]
        (is (= 1 (:replaced ((api 'replace-text) h "18%" "40%"))))
        (is (str/includes? (:text ((api 'text) h)) "40%")))
      (is (str/includes? (:text ((api 'text) h)) "18%") "child edits do not affect the parent")
      (let [[a b] (ygg/copy-fork! fork 2)]
        (vreset! copied? true)
        (try
          (binding [ec/*execution-context* (:child-ctx a)]
            (is (= true (:closed ((api 'close) h))))
            (is (= :unknown-handle (:error ((api 'text) h)))))
          (binding [ec/*execution-context* (:child-ctx b)]
            (is (str/includes? (:text ((api 'text) h)) "40%"))
            (is (= 1 (:replaced ((api 'replace-text) h "40%" "50%")))))
          (is (str/includes? (:text ((api 'text) h)) "18%"))
          (finally (ygg/discard-fork! a) (ygg/discard-fork! b))))
      (finally
        ;; copy-fork! consumes the source handle on success.
        (when-not @copied? (ygg/discard-fork! fork))))))

(deftest saved-office-function-selects-child-staging
  (let [ctx (sandbox)
        _ (ev ctx "(def h (office/open \"/report.docx\")) (defn change [] (office/replace-text h \"18%\" \"31%\"))")
        fork (ygg/fork!)]
    (try
      (binding [ec/*execution-context* (:child-ctx fork)]
        (is (= 1 (:replaced (ev ctx "(change)"))))
        (is (str/includes? (:text (ev ctx "(office/text h)")) "31%")))
      (is (str/includes? (:text (ev ctx "(office/text h)")) "18%"))
      (finally (ygg/discard-fork! fork)))))

(deftest office-staging-rejects-concurrent-overwrite
  (let [api (office/sci-namespace (temp-fs) :room)
        h ((api 'open) "/report.docx")
        tree ((api 'xml) h "word/document.xml")
        emit office/emit-xml
        interfere? (atom true)]
    (with-redefs [office/emit-xml (fn [tree]
                                  (when (compare-and-set! interfere? true false)
                                    ((api 'replace-text) h "18%" "22%"))
                                  (emit tree))]
      (is (= :stale-handle (:error ((api 'set-xml!) h "word/document.xml" tree)))))
    (is (str/includes? (:text ((api 'text) h)) "22%"))))

(deftest office-tree-bounds-precede-emission
  (let [tree (nth (iterate #(hash-map :tag :p :content [%]) "text") 258)]
    (is (= :xml-too-complex
           (:error (try (office/emit-xml tree) (catch clojure.lang.ExceptionInfo e (ex-data e)))))))
  (is (= :xml-too-complex
         (:error (try (office/emit-xml {:tag :p :content (repeat "x")})
                      (catch clojure.lang.ExceptionInfo e (ex-data e)))))))

(deftest office-handle-limits-and-reinstallation-are-world-local
  (let [owner (ec/current-execution-context) cap (random-uuid)
        with-environment! (fn [f] (f {:ctx (ec/current-execution-context) :fs (temp-fs) :scope cap}))
        api (office/sci-namespace owner cap with-environment!)
        h ((api 'open) "/report.docx")
        installed-again (office/sci-namespace owner cap with-environment!)
        child (ygg/fork!)]
    (try
      (is (str/includes? (:text ((installed-again 'text) h)) "18%"))
      (with-redefs [office/max-handles 1]
        (is (= :office-limit (:error ((api 'open) "/report.docx"))))
        (binding [ec/*execution-context* (:child-ctx child)]
          ((api 'close) h)
          (is (string? (:handle ((api 'open) "/report.docx")))))
        (is (str/includes? (:text ((api 'text) h)) "18%")))
      (with-redefs [office/max-retained-bytes 1]
        (is (= :office-limit (:error ((api 'open) "/report.docx")))))
      (finally (ygg/discard-fork! child)))))

;; =============================================================================
;; Container: hardened read / write
;; =============================================================================

(deftest read-parts-preserves-order-and-write-parts-round-trips
  (let [parts (read-fixture-parts "notes.odt")]
    (testing "mimetype is the FIRST entry (ODF requires it)"
      (is (= "mimetype" (first (keys parts)))))
    (testing "write→read is a fixpoint on names, order and bytes"
      (let [back (office/read-parts (office/write-parts parts))]
        (is (= (vec (keys parts)) (vec (keys back))))
        (is (every? (fn [[k v]] (java.util.Arrays/equals ^bytes v ^bytes (get back k))) parts))))))

(deftest unzip-refuses-zip-slip
  (let [slip (let [out (ByteArrayOutputStream.)]
               (with-open [z (ZipOutputStream. out)]
                 (.putNextEntry z (ZipEntry. "../escape.xml"))
                 (.write z (.getBytes "<a/>")) (.closeEntry z))
               (.toByteArray out))]
    (is (= :unsafe-entry
           (:error (try (office/read-parts slip)
                        (catch clojure.lang.ExceptionInfo e (ex-data e))))))))

(deftest unzip-enforces-bomb-caps
  (testing "too many entries is refused before OOM"
    (with-redefs [office/max-entries 2]
      (let [many (let [out (ByteArrayOutputStream.)]
                   (with-open [z (ZipOutputStream. out)]
                     (dotimes [i 5]
                       (.putNextEntry z (ZipEntry. (str "p" i ".xml")))
                       (.write z (.getBytes "<a/>")) (.closeEntry z)))
                   (.toByteArray out))]
        (is (= :too-many-entries
               (:error (try (office/read-parts many)
                            (catch clojure.lang.ExceptionInfo e (ex-data e)))))))))
  (testing "an entry inflating past the per-entry cap is refused"
    (with-redefs [office/max-entry-uncompressed 16]
      (let [big (let [out (ByteArrayOutputStream.)]
                  (with-open [z (ZipOutputStream. out)]
                    (.putNextEntry z (ZipEntry. "big.xml"))
                    (.write z (.getBytes (apply str (repeat 1000 "A")))) (.closeEntry z))
                  (.toByteArray out))]
        (is (= :entry-too-large
               (:error (try (office/read-parts big)
                            (catch clojure.lang.ExceptionInfo e (ex-data e))))))))))

;; =============================================================================
;; XML: hardened parse + emit
;; =============================================================================

(deftest parse-xml-yields-prefixed-plain-data
  (let [tree (office/parse-xml (get (read-fixture-parts "notes.odt") "content.xml"))]
    (is (= :office:document-content (:tag tree)))
    (is (map? (:attrs tree)))
    (is (vector? (:content tree)))))

(deftest parse-xml-blocks-xxe
  (testing "a DOCTYPE with an external entity does not resolve — DTDs disallowed"
    (let [xxe (.getBytes (str "<?xml version=\"1.0\"?>"
                              "<!DOCTYPE r [<!ENTITY x SYSTEM \"file:///etc/passwd\">]>"
                              "<r>&x;</r>") "UTF-8")]
      (is (thrown? Exception (office/parse-xml xxe))))))

(deftest emit-then-parse-is-stable
  (let [bs (get (read-fixture-parts "notes.odt") "content.xml")
        tree (office/parse-xml bs)
        round (office/parse-xml (office/emit-xml tree))]
    (is (= tree round) "parse∘emit∘parse == parse")))

;; =============================================================================
;; open / text / detect — through the sandbox, the agent's path
;; =============================================================================

(deftest open-returns-a-handle-and-summary-not-bytes
  (doseq [[file fmt main] [["/report.docx" :docx "word/document.xml"]
                           ["/deck.pptx"   :pptx "ppt/slides/slide1.xml"]
                           ["/notes.odt"   :odt  "content.xml"]]]
    (let [ctx (sandbox)
          r (ev ctx (str "(office/open \"" file "\")"))]
      (testing (str file " opens as " fmt)
        (is (string? (:handle r)) (pr-str r))
        (is (= fmt (:format r)))
        (is (= main (:main-part r)))
        (is (some #{main} (:parts r)))
        (is (str/includes? (:text-preview r) "Revenue grew"))
        (is (nil? (:bytes r)) "the raw container never reaches the agent")))))

(deftest xlsx-is-detected-and-redirected-to-sheet
  ;; a parts map with xl/workbook.xml is enough to exercise the detect branch;
  ;; open then refuses it with :use-sheet (the live-model path is sheet/*).
  (let [parts (array-map "[Content_Types].xml" (.getBytes "<a/>")
                         "xl/workbook.xml" (.getBytes "<workbook/>"))]
    (is (= :xlsx (#'office/detect-format parts)))))

(deftest text-extracts-the-body
  (doseq [file ["/report.docx" "/deck.pptx" "/notes.odt"]]
    (let [ctx (sandbox)
          _ (ev ctx (str "(def h (office/open \"" file "\"))"))
          r (ev ctx "(office/text h)")]
      (is (str/includes? (:text r) "Revenue grew 18%") (str file " → " (pr-str r))))))

;; =============================================================================
;; replace-text — edit visible text, save, verify fidelity
;; =============================================================================

(deftest replace-text-edits-only-the-body-part-and-preserves-the-rest
  (doseq [[file out edited] [["/report.docx" "/report-v2.docx" "word/document.xml"]
                             ["/deck.pptx"   "/deck-v2.pptx"    "ppt/slides/slide1.xml"]
                             ["/notes.odt"   "/notes-v2.odt"    "content.xml"]]]
    (let [ctx (sandbox)
          orig (read-fixture-parts (subs file 1))
          _ (ev ctx (str "(def h (office/open \"" file "\"))"))
          rep (ev ctx "(office/replace-text h \"18%\" \"23%\")")
          sav (ev ctx (str "(office/save-as! h \"" out "\")"))
          result (read-fixture-parts (subs out 1))]
      (testing (str file " → " out)
        (is (= 1 (:replaced rep)) (pr-str rep))
        (is (= [edited] (:edited-parts sav)) (pr-str sav))
        (is (str/includes? (String. ^bytes (get result edited) "UTF-8") "23%"))
        (is (not (str/includes? (String. ^bytes (get result edited) "UTF-8") "18%")))
        (testing "every part the agent did not touch is BYTE-IDENTICAL"
          (doseq [[nm bs] orig :when (not= nm edited)]
            (is (java.util.Arrays/equals ^bytes bs ^bytes (get result nm))
                (str nm " changed but should not have"))))))))

;; =============================================================================
;; structural edit — office/xml + clojure.zip in the sandbox, then set-xml!
;; =============================================================================

(deftest structural-edit-via-zip-then-set-xml-saves-a-valid-part
  (let [ctx (sandbox)
        _ (ev ctx "(def h (office/open \"/report.docx\"))")
        ;; uppercase every w:t text node using clojure.zip, the way an agent would
        code (str "(require '[clojure.zip :as z])"
                  "(let [t (office/xml h \"word/document.xml\")]"
                  "  (loop [loc (z/zipper map? :content"
                  "                       (fn [n cs] (assoc n :content (vec cs))) t)]"
                  "    (if (z/end? loc)"
                  "      (office/set-xml! h \"word/document.xml\" (z/root loc))"
                  "      (recur (z/next"
                  "               (if (and (map? (z/node loc)) (= :w:t (:tag (z/node loc))))"
                  "                 (z/edit loc update :content"
                  "                         (fn [cs] (mapv #(if (string? %) (clojure.string/upper-case %) %) cs)))"
                  "                 loc))))))")
        staged (ev ctx code)
        _ (ev ctx "(office/save-as! h \"/report-upper.docx\")")
        result (read-fixture-parts "report-upper.docx")
        doc (String. ^bytes (get result "word/document.xml") "UTF-8")]
    (is (:staged? staged) (pr-str staged))
    (is (str/includes? doc "REVENUE GREW"))
    (is (not (str/includes? doc "Revenue grew")))
    (testing "re-emitted part is still well-formed and reopenable by POI"
      (with-open [d (XWPFDocument. (java.io.ByteArrayInputStream.
                                     (office/write-parts result)))]
        (is (str/includes? (str/join " " (map #(.getText %) (.getParagraphs d)))
                           "REVENUE GREW"))))))

;; =============================================================================
;; Errors are DATA
;; =============================================================================

(deftest failures-come-back-as-data
  (let [ctx (sandbox)]
    (testing "unknown handle"
      (is (= :unknown-handle (:error (ev ctx "(office/parts \"office-nope\")")))))
    (testing "unknown file"
      (is (= :not-found (:error (ev ctx "(office/open \"/missing.docx\")")))))
    (ev ctx "(def h (office/open \"/report.docx\"))")
    (testing "unknown part"
      (is (= :unknown-part (:error (ev ctx "(office/xml h \"word/nope.xml\")")))))
    (testing "a malformed edit tree is refused, not written"
      (is (#{:bad-tree :malformed-tree}
            (:error (ev ctx "(office/set-xml! h \"word/document.xml\" {:tag :w:t :content [42]})")))))
    (testing "save-as! keeps the format extension"
      (is (= :bad-extension (:error (ev ctx "(office/save-as! h \"/report.pptx\")")))))))
