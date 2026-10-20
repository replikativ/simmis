(ns is.simm.documents.explicit-projection-test
  "Real immutable Geschichte document projections. Legacy wrapper tests isolate
   only selected-workspace resolution; they do not claim room provisioning."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.documents.office :as documents]
            [is.simm.media.office :as office])
  (:import [java.util Arrays Base64]))

(defn- bytes-of [text] (.getBytes ^String text "UTF-8"))
(defn- encode [bytes] (.encodeToString (Base64/getEncoder) bytes))

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :commit-graph? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn {:name "Explicit projections"})
        (f conn)
        (finally (d/release conn) (d/delete-database cfg))))))

(defn- commit! [conn path bytes]
  (repo/write! conn path bytes)
  (repo/stage-all! conn)
  (:geschichte.commit/id (repo/commit! conn {:message (str "Fixture " path)})))

(deftest all-projections-use-only-the-supplied-frozen-snapshot
  (with-repository
    (fn [conn]
      (let [first-commit (commit! conn "wiki/note.md" (bytes-of "# Original"))
            frozen @conn selected (files/entry frozen "wiki/note.md")
            request {:folders ["" "wiki"] :paths ["wiki/note.md" "later.md"]}]
        (commit! conn "wiki/note.md" (bytes-of "# Later"))
        (commit! conn "later.md" (bytes-of "later"))
        (let [current @conn before-id (d/commit-id current)]
          (with-redefs [files/with-workspace (fn [& _] (throw (AssertionError. "Ambient projection lookup")))
                        d/transact (fn [& _] (throw (AssertionError. "Projection attempted a write")))]
            (is (= ["note.md"] (mapv :name (:entries (files/list-folder-in frozen "wiki")))))
            (is (= ["wiki/note.md"] (mapv :path (:entries (files/search-paths-in frozen "note")))))
            (is (empty? (:entries (files/search-paths-in frozen "later"))))
            (let [metadata (files/metadata-in frozen request)]
              (is (= selected (get-in metadata [:entries "wiki/note.md"])))
              (is (nil? (get-in metadata [:entries "later.md"])))
              (is (= [(get-in frozen [:config :store :id]) (get-in frozen [:config :branch] :db)]
                     (get-in metadata [:workspace :workspace-id])))
              (is (not= (:revision metadata) (:revision (files/metadata-in current request)))))
            (is (= [first-commit] (mapv :commit (:versions (files/history-in frozen "wiki/note.md")))))
            (is (= "# Original" (:text (files/read-history-in frozen "wiki/note.md" first-commit))))
            (is (= (encode (bytes-of "# Original")) (:base64 (files/read-binary-in frozen "wiki/note.md" selected))))
            (is (thrown? Exception (files/read-binary-in current "wiki/note.md" selected))))
          (is (= before-id (d/commit-id @conn))))))))

(deftest legacy-read-wrappers-delegate-with-selected-connection-snapshot
  (with-repository
    (fn [conn]
      (let [commit (commit! conn "file.txt" (bytes-of "selected"))
            db @conn selected (files/entry db "file.txt")
            room ::selected-room request {:folders [""] :paths ["file.txt"]}
            calls (atom 0)]
        (with-redefs [files/with-workspace (fn [actual f]
                                             (is (= room actual))
                                             (swap! calls inc)
                                             (f conn))]
          (is (= (files/list-folder-in db "") (files/list-folder room "")))
          (is (= (files/metadata-in db request) (files/metadata room request)))
          (is (= (files/search-paths-in db "file") (files/search-paths room "file")))
          (is (= (files/history-in db "file.txt") (files/history room "file.txt")))
          (is (= (files/read-history-in db "file.txt" commit) (files/read-history room "file.txt" commit)))
          (is (= (files/read-binary-in db "file.txt" selected) (files/read-binary room "file.txt" selected)))
          (is (= 6 @calls)))))))

(deftest office-preparation-retains-original-zip-and-all-container-guards
  (with-repository
    (fn [conn]
      (let [xml "<?xml version=\"1.0\"?><document><!-- keep -->Original</document>"
            opaque (bytes-of "opaque signatures")
            original (office/write-parts {"word/document.xml" (bytes-of xml) "custom/data.bin" opaque})
            _ (commit! conn "report.docx" original)
            frozen @conn selected (files/entry frozen "report.docx")
            request {:path "report.docx" :expected selected :part "word/document.xml" :text xml}]
        (commit! conn "report.docx" (office/write-parts {"word/document.xml" (bytes-of "<later/>")}))
        (with-redefs [files/with-workspace (fn [& _] (throw (AssertionError. "Office resolved an ambient target")))
                      files/save-file! (fn [& _] (throw (AssertionError. "Preparation wrote a file")))]
          (is (= xml (:text (documents/inspect-in frozen "report.docx" selected nil))))
          (let [prepared (documents/prepare-save-in frozen request)]
            (is (= #{:path :expected :base64} (set (keys prepared))))
            (is (= (encode original) (:base64 prepared))))
          (let [prepared (documents/prepare-save-in frozen (assoc request :text "<changed/>"))
                parts (office/read-parts (.decode (Base64/getDecoder) ^String (:base64 prepared)))]
            (is (Arrays/equals opaque ^bytes (get parts "custom/data.bin")))
            (is (= "<changed/>" (String. ^bytes (get parts "word/document.xml") "UTF-8"))))
          (doseq [bad [(assoc request :extra true) (assoc request :part "custom/data.bin")
                       (assoc request :text "<!DOCTYPE x><x/>")
                       (assoc request :text (str "<x>" (apply str (repeat documents/max-xml-bytes "x")) "</x>"))]]
            (is (thrown? Exception (documents/prepare-save-in frozen bad))))
          (is (thrown? Exception (documents/inspect-in @conn "report.docx" selected nil))))
        (with-redefs [files/with-workspace (fn [_ f] (f (atom frozen)))
                      files/save-file! (fn [room prepared options] [room prepared options])]
          (is (= (documents/inspect-in frozen "report.docx" selected nil)
                 (documents/inspect! ::room "report.docx" selected nil)))
          (is (= [::room (documents/prepare-save-in frozen request) {:fixture true}]
                 (documents/save-part! ::room request {:fixture true}))))))))
