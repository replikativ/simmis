(ns is.simm.ops.owned-file-projections-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.substrate.geschichte :as workspace]
            [is.simm.documents.files :as files]
            [is.simm.documents.office :as office]
            [is.simm.documents.docx-test :as docx-fixtures]
            [is.simm.documents.rich :as rich]
            [is.simm.media.office :as zip]
            [is.simm.ops.owned-files :as actions]
            [is.simm.ops.owned-workspace-test :as fixture])
  (:import [java.util Base64]))

(defn- with-fixture [owned-first? f]
  (let [configs (atom [])]
    (try
      (@#'fixture/with-fixture
       owned-first? {:system-schema :core}
       (fn [{:keys [owned attached] :as data}]
         (reset! configs (mapv #(-> % deref :config) [owned attached]))
         (f data)))
      (finally
        ;; The shared fixture has physically closed its Room and released
        ;; these connections. Delete only these exact named-memory test stores.
        (doseq [cfg @configs]
          (when-not (and (= :memory (get-in cfg [:store :backend]))
                         (uuid? (get-in cfg [:store :id])))
            (throw (ex-info "Refuse non-fixture cleanup" {})))
          (d/delete-database cfg))))))

(deftest every-read-projects-the-owned-snapshot-in-both-orders
  (doseq [owned-first? [true false]]
    (with-fixture
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
  (with-fixture
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
                      office/prepare-save-in (fn [& _] (throw (AssertionError. "Stale stamp reached XML parsing")))
                      office/prepare-correction-in (fn [& _] (throw (AssertionError. "Stale stamp reached text reconstruction")))]
          (doseq [[f request] [[actions/history! {:path "wiki/file.md"}]
                               [actions/office! {:path "report.docx" :expected nil :part nil}]
                               [actions/office-save! {:path "report.docx" :expected nil :part "word/document.xml" :text "<x/>"}]
                               [actions/office-correct! {:path "report.docx" :expected nil :token (random-uuid) :replacement "text"}]]]
            (is (thrown? clojure.lang.ExceptionInfo
                         (apply f (conj args (assoc request :expected-target-stamp stamp)))))))))))

(deftest office-prepare-cannot-switch-the-original-save-binding
  (with-fixture
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

(deftest correction-prepare-cannot-switch-the-original-publication-binding
  (with-fixture
    false
    (fn [{:keys [conn w room owned attached attached-descriptor]}]
      (let [args [conn (:owner w) (:room w) room]
            stamp (:target-stamp (apply actions/metadata! (conj args {:folders [] :paths []})))
            container (zip/write-parts (docx-fixtures/package-parts "<w:p><w:r><w:t>Original</w:t></w:r></w:p>"))
            saved (apply actions/save! (conj args {:path "report.docx" :expected nil
                                                   :base64 (.encodeToString (Base64/getEncoder) container)
                                                   :expected-target-stamp stamp}))
            opened (apply actions/office! (conj args {:path "report.docx" :expected (:entry saved) :part nil :expected-target-stamp stamp}))
            token (first (vals (get-in opened [:reading :corrections :tokens])))
            edit {:path "report.docx" :expected (:entry saved) :token token :replacement "Correction" :expected-target-stamp stamp}
            before @owned other @attached prepare office/prepare-correction-in]
        (is (uuid? token))
        (with-redefs [office/prepare-correction-in
                      (fn [db request]
                        (let [prepared (prepare db request)]
                          (d/transact conn [{:room/id (:room w)
                                             :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                          prepared))]
          (is (thrown? clojure.lang.ExceptionInfo (apply actions/office-correct! (conj args edit)))))
        (is (= before @owned))
        (is (= other @attached))))))

(deftest scoped-discovery-and-rich-actions-share-the-opened-target
  (with-fixture
    true
    (fn [{:keys [conn w room owned attached attached-descriptor]}]
      (let [args [conn (:owner w) (:room w) room]
            opened (apply actions/read! (conj args {:path "wiki/file.md"}))
            stamp (:target-stamp opened)
            scope {:expected-target-stamp stamp}
            text (:text opened)
            analysis (apply actions/analyze! (conj args (assoc scope :path "wiki/file.md" :text text)))
            slot (first (:slots analysis))
            edit (merge scope {:path "wiki/file.md" :text text :revision (:revision analysis)}
                        (select-keys slot [:from :to :model]))
            edited (apply actions/rich! (conj args edit))
            request (merge scope {:path "wiki/file.md" :expected (:entry opened) :source text
                                  :revision (:revision analysis) :edits [] :text text})
            other @attached]
        (is (= (:analysis opened) (dissoc analysis :target-stamp)))
        (is (= stamp (:target-stamp edited)))
        (is (= text (:text edited)))
        (doseq [[f request] [[actions/read! (assoc scope :path "wiki/file.md")]
                             [actions/folder! (assoc scope :folder "wiki")]
                             [actions/search! (assoc scope :query "file")]]]
          (is (= stamp (:target-stamp (apply f (conj args request))))))
        (is (true? (:saved? (apply actions/rich-save! (conj args request)))))
        (let [before @owned prepare rich/apply-edits]
          (with-redefs [rich/apply-edits
                        (fn [& xs]
                          (let [result (apply prepare xs)]
                            (d/transact conn [{:room/id (:room w)
                                               :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                            result))]
            (is (thrown? clojure.lang.ExceptionInfo (apply actions/rich-save! (conj args request)))))
          (is (= before @owned)))
        (with-redefs [files/read-file-in (fn [& _] (throw (AssertionError. "Stale scoped read reached body")))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (apply actions/read! (conj args (assoc scope :path "wiki/file.md"))))))
        (is (= other @attached))))))

(deftest owned-move-returns-original-stamp-and-rechecks-default-at-the-gate
  (with-fixture
    false
    (fn [{:keys [conn w room owned attached attached-descriptor]}]
      (let [args [conn (:owner w) (:room w) room]
            opened (apply actions/read! (conj args {:path "wiki/file.md"}))
            request {:path "wiki/file.md" :target "wiki/moved.md" :expected (:entry opened)
                     :expected-target-stamp (:target-stamp opened)}
            moved (apply actions/move! (conj args request))
            before @owned other @attached move files/move-file-to!]
        (is (true? (:moved? moved)))
        (is (= (:target-stamp opened) (:target-stamp moved)))
        (is (nil? (files/entry @owned "wiki/file.md")))
        (is (= (:entry opened) (files/entry @owned "wiki/moved.md")))
        (with-redefs [files/move-file-to!
                      (fn [& xs]
                        (d/transact conn [{:room/id (:room w)
                                           :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                        (apply move xs))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (apply actions/move! (conj args (assoc request :path "wiki/moved.md" :target "wiki/again.md"))))))
        (is (= before @owned))
        (is (= other @attached))))))
