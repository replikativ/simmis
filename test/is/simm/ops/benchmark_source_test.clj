(ns is.simm.ops.benchmark-source-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.blob :as blob]
            [dvergr.catalog.room :as catalog]
            [is.simm.ops.benchmark-source :as source]
            [is.simm.ops.workflows :as workflows])
  (:import [java.nio.charset StandardCharsets]))

(def files
  {"workflow.edn" "{:title \"Retained source\" :task \"Read the evidence\" :capture [\"/out\"]}"
   "checker.clj" "(throw (ex-info \"Checker must never execute in this test\" {}))"
   "gold.edn" "{:answer \"HOST gold canary\"}"
   "calibration.edn" "{:canary \"HOST calibration\"}"
   "fixtures/docs/shared.md" "shared evidence"})

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest owned-manifest-corresponds-to-full-utf8-bytes-and-bounded-metadata
  (let [files (into (assoc files "fixtures/docs/shared.md" "未来 🦋")
                    (map #(vector (str "fixtures/docs/extra-" % ".md") "extra") (range 9)))
        retained (source/retain "echo" files)
        ;; Managed EDN reads do not preserve sorted-map implementation type.
        retained (update retained :files #(into {} %))
        manifest (mapv (fn [[path text]]
                         (let [bytes (.getBytes ^String text StandardCharsets/UTF_8)]
                           {:path path :content (blob/blob-id bytes) :size (alength bytes) :mode 33188}))
                       (sort-by key files))]
    (is (= manifest (source/validate-manifest! retained manifest)))
    (doseq [changed [(vec (reverse manifest)) (pop manifest)
                     (conj manifest (first manifest))
                     (assoc-in manifest [0 :content] (random-uuid))
                     (update-in manifest [0 :size] inc)
                     (assoc-in manifest [0 :mode] 16384)
                     (assoc-in manifest [0 :path] "wrong/path.md")
                     (assoc-in manifest [0 :extra] true)]]
      (is (= :source-manifest (reason #(source/validate-manifest! retained changed)))))))

(deftest source-is-exact-portable-and-reconstructible-without-authoring-io
  (let [retained (source/retain "echo" files)
        bundle (source/reconstruct retained)]
    (is (= files (:files retained)))
    (is (= (:id (catalog/bundle "echo" files)) (:id bundle) (:bundle retained)))
    (is (= (:checker bundle) (get files "checker.clj")))
    (is (= {"/docs/shared.md" "shared evidence"}
           (source/case-files retained {:kind :default})))
    (is (= (source/source-id retained)
           (source/source-id (source/retain "echo" (into (array-map) (reverse (seq files)))))))
    (is (not= (source/source-id retained)
              (source/source-id (source/retain "echo" (assoc files "gold.edn" "{:answer :changed}")))))
    (is (= :source-identity
           (reason #(source/reconstruct (assoc-in retained [:files "gold.edn"] "{:answer :changed}")))))
    (is (= :source-shape (reason #(source/reconstruct (assoc retained :authority true)))))))

(deftest candidate-files-are-catalog-fixtures-not-the-host-source-bundle
  (let [files (assoc files
                     "fixture-pack.edn" (pr-str {:kind :dvergr/fixture-pack :version 1
                                                :files {"/docs/packed.md" "packed shared"}})
                     "cases/default/fixtures/docs/shared.md" "selected override"
                     "cases/default/fixtures/wiki/Company.md" "selected wiki"
                     "cases/default/gold.edn" "{:answer :selected-host-gold}"
                     "cases/other/fixtures/docs/other.md" "other case canary"
                     "cases/other/gold.edn" "{:answer :other-host-gold}")
        retained (source/retain "echo" files)
        selected (source/case-files retained {:kind :named :name "default"})]
    (is (= {"/docs/shared.md" "selected override"
            "/docs/packed.md" "packed shared"
            "/wiki/Company.md" "selected wiki"} selected))
    (is (not-any? #(re-find #"HOST|other case|selected-host" %) (vals selected)))
    (is (= :unknown-case (reason #(source/case-files retained {:kind :default}))))
    (doseq [bad [{:kind :named :name "missing"} {:kind :named :name nil}
                 {:kind :named :name "default" :extra true} nil "default"]]
      (is (= :unknown-case (reason #(source/case-files retained bad)))))))

(deftest source-bounds-and-canonical-paths-are-checked-before-catalog-decoding
  (doseq [bad [(assoc files "fixtures/../escape.md" "bad")
               (assoc files "/absolute.md" "bad")
               (assoc files "fixtures//alias.md" "bad")
               (assoc files "fixtures/.git/config" "bad")
               (assoc files "fixtures/invalid.md" nil)
               (assoc files "fixtures/invalid.md" (String. (char-array [(char 0xD800)])))
               (assoc files "fixtures/large.md" (apply str (repeat workflows/max-file-bytes "é")))
               (into files (map #(vector (str "extra/" %) "x") (range 100)))]]
    (with-redefs [catalog/bundle (fn [& _] (throw (AssertionError. "No decoding before bounded source validation")))]
      (is (thrown? clojure.lang.ExceptionInfo (source/retain "echo" bad)))))
  (is (= :source-name (reason #(source/retain "../invalid" files)))))
