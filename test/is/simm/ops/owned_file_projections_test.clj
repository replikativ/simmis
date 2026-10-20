(ns is.simm.ops.owned-file-projections-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.substrate.geschichte :as workspace]
            [is.simm.documents.files :as files]
            [is.simm.documents.office :as office]
            [is.simm.media.office :as zip]
            [is.simm.ops.owned-files :as actions]
            [is.simm.ops.owned-workspace-test :as fixture])
  (:import [java.util Base64]))

(deftest every-read-projects-the-owned-snapshot-in-both-orders
  (doseq [owned-first? [true false]]
    (@#'fixture/with-fixture
     owned-first?
     (fn [{:keys [conn w room attached]}]
       (let [args [conn (:owner w) (:room w) room]
             other @attached]
         (with-redefs [workspace/current-workspace
                       (fn [] (throw (AssertionError. "Owned projection used ambient selection")))]
           (let [opened (apply actions/read! (conj args {:path "wiki/file.md"}))
                 stamp (:target-stamp opened)
                 metadata (apply actions/metadata! (conj args {:folders ["wiki"] :paths ["wiki/file.md"]}))
                 folder (apply actions/folder! (conj args {:folder "wiki"}))
                 search (apply actions/search! (conj args {:query "file"}))
                 history (apply actions/history! (conj args {:path "wiki/file.md" :expected-target-stamp stamp}))
                 version (apply actions/history-file!
                                (conj args {:path "wiki/file.md" :commit (:commit (first (:versions history)))
                                            :expected-target-stamp stamp}))
                 binary (apply actions/download!
                               (conj args {:path "wiki/file.md" :expected (:entry opened) :expected-target-stamp stamp}))]
             (is (= "# Owned\n" (:text opened) (:text version)))
             (is (every? #(= stamp (:target-stamp %)) [metadata folder search history version binary]))
             (is (= (:entry opened) (get-in metadata [:entries "wiki/file.md"])))
             (is (= "# Owned\n" (String. (.decode (Base64/getDecoder) ^String (:base64 binary)) "UTF-8")))
             (is (= ["wiki/file.md"] (mapv :path (:entries search))))))
         (is (= other @attached)))))))

(deftest discovery-finds-replacement-but-old-buffer-stamps-cannot-read-it
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room attached-descriptor]}]
     (let [args [conn (:owner w) (:room w) room]
           request {:folders ["wiki"] :paths ["wiki/file.md"]}
           before (apply actions/metadata! (conj args request))
           stamp (:target-stamp before)]
       (d/transact conn [{:room/id (:room w)
                          :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
       (let [after (apply actions/metadata! (conj args request))]
         (is (not= stamp (:target-stamp after)))
         (is (not= (:revision before) (:revision after)))
         (is (= "# Attachment\n" (:text (apply actions/read! (conj args {:path "wiki/file.md"}))))))
       (with-redefs [files/history-in (fn [& _] (throw (AssertionError. "Stale stamp reached history")))
                     office/inspect-in (fn [& _] (throw (AssertionError. "Stale stamp reached ZIP decoding")))
                     office/prepare-save-in (fn [& _] (throw (AssertionError. "Stale stamp reached XML parsing")))]
         (doseq [[f request] [[actions/history! {:path "wiki/file.md"}]
                              [actions/office! {:path "report.docx" :expected nil :part nil}]
                              [actions/office-save! {:path "report.docx" :expected nil :part "word/document.xml" :text "<x/>"}]]]
           (is (thrown? clojure.lang.ExceptionInfo
                        (apply f (conj args (assoc request :expected-target-stamp stamp)))))))))))

(deftest office-prepare-cannot-switch-the-original-save-binding
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn w room owned attached attached-descriptor]}]
     (let [args [conn (:owner w) (:room w) room]
           metadata (apply actions/metadata! (conj args {:folders [] :paths []}))
           stamp (:target-stamp metadata)
           xml "<document><!-- keep -->Hello</document>"
           container (zip/write-parts {"word/document.xml" (.getBytes xml "UTF-8")})
           saved (apply actions/save! (conj args {:path "report.docx" :expected nil
                                                  :base64 (.encodeToString (Base64/getEncoder) container)
                                                  :expected-target-stamp stamp}))
           edit {:path "report.docx" :expected (:entry saved) :part "word/document.xml"
                 :text "<document>Updated</document>" :expected-target-stamp stamp}
           opened (apply actions/office! (conj args (dissoc edit :text)))
           before @owned other @attached prepare office/prepare-save-in]
       (is (= xml (:text opened)))
       (is (= stamp (:target-stamp opened)))
       (let [noop (apply actions/office-save! (conj args (assoc edit :text xml)))]
         (is (nil? (:commit noop)))
         (is (= stamp (:target-stamp noop))))
       (with-redefs [office/prepare-save-in
                     (fn [db request]
                       (let [prepared (prepare db request)]
                         (d/transact conn [{:room/id (:room w)
                                            :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                         prepared))]
         (is (thrown? clojure.lang.ExceptionInfo (apply actions/office-save! (conj args edit)))))
       (is (= before @owned))
       (is (= other @attached))))))
