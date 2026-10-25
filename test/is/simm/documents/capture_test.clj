(ns is.simm.documents.capture-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.capture :as capture]
            [is.simm.documents.markdown :as markdown]))

(def session {:id #uuid "91359c37-d5a7-4771-a66c-7c3114a1b9a2"
              :owner #uuid "39bb0b7b-938d-46d7-8a96-5e2f1061c5a5" :started-at 1792891423030})
(def segment {:index 0 :offset-ms 0 :duration-ms 300000 :extension "webm"
              :content #uuid "3399fbba-cf92-51bb-9bd3-1090fa8fcd06" :size 10000000})
(def frame {:id #uuid "09306dc4-fbec-5e42-b89d-8c5d54f1b9ed" :at 1792891425457
            :extension "jpg" :content #uuid "0b0b79e4-9ad3-4f7d-906b-998f86b80777" :size 367095
            :text "## A heading\n```clojure\n(+ 1 2)\n```\nno final newline" :model "codex-subscription-luna-6"})

(deftest notes-use-chronological-files-relative-assets-and-exact-provenance
  (let [later (assoc frame :id (random-uuid) :at (+ (:at frame) 1000) :text "Later observation")
        note (capture/observation-note session segment [later frame])
        parsed (markdown/parse (:text note))
        metadata (get-in parsed [:frontmatter :properties])]
    (is (str/starts-with? (:path note) (str "captures/2026-10-25/01-23-43--" (:id session) "/observations/01-23-43--000000.md")))
    (is (< (.indexOf ^String (:text note) "## 2026-10-25T01:23:45.457Z")
           (.indexOf ^String (:text note) "## 2026-10-25T01:23:46.457Z")))
    (is (str/includes? (:text note) "![Screenshot at 2026-10-25T01:23:45.457Z](../images/"))
    (is (str/includes? (:text note) "[Original recording](../recordings/"))
    (is (str/includes? (:text note) (str "````text\n" (:text frame) "\n````")))
    (is (= "screen-observation" (get metadata "source-kind")))
    (is (= (str (:content segment)) (get-in metadata ["recording" "content"])))
    (is (= 3 (count (:assets note))))
    (is (empty? (:diagnostics parsed)))
    (is (= note (capture/observation-note session segment [frame later])))))

(deftest unresolved-or-out-of-interval-frames-must-not-be-silently-published
  (doseq [bad [(dissoc frame :text) (assoc frame :failed "failure")
               (assoc frame :at (dec (:started-at session)))
               (assoc frame :at (+ (:started-at session) (:duration-ms segment)))
               (assoc frame :content "not a UUID") (assoc frame :extension "svg")]]
    (is (thrown? clojure.lang.ExceptionInfo (capture/observation-note session segment [bad]))))
  (is (thrown? clojure.lang.ExceptionInfo (capture/observation-note session segment [frame frame])))
  (is (thrown? clojure.lang.ExceptionInfo (capture/observation-note session (assoc segment :offset-ms Long/MAX_VALUE) []))))

(deftest failed-transcriptions-retain-the-original-image-and-visible-failure
  (let [failed (-> frame (dissoc :text) (assoc :failed "provider unavailable"))
        note (capture/observation-note session segment [failed])]
    (is (str/includes? (:text note) "Transcription failed: provider unavailable"))
    (is (str/includes? (:text note) "![Screenshot at"))
    (is (= (:content frame) (:content (second (:assets note)))))))

(deftest archived-observations-do-not-claim-a-shared-capture-clock
  (let [note (capture/observation-note session (assoc segment :offset-ms 990) [frame])
        properties (get-in (markdown/parse (:text note)) [:frontmatter :properties])]
    (is (= "server-received" (get properties "timestamp-basis")))
    (is (= "estimated" (get properties "recording-alignment")))
    (is (str/includes? (:text note) "Estimated recording position: 1437 ms"))
    (is (str/includes? (:path note) "/observations/01-23-44--000000.md"))))
