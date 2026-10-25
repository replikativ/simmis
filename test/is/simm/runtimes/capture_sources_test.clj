(ns is.simm.runtimes.capture-sources-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.blob :as blob]
            [dvergr.drive.blobs :as blobs]
            [is.simm.runtimes.capture-sources :as sources]
            [is.simm.runtimes.media-cache :as cache])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(defn- with-file! [f]
  (let [file (.toFile (Files/createTempFile "capture-source-" ".webm" (make-array FileAttribute 0)))]
    (try (spit file "recording") (f file) (finally (.delete file)))))

(deftest staged-source-is-private-verified-and-detached-from-legacy-path
  (with-file!
    (fn [file]
      (let [resource (cache/create)
            bytes (.getBytes "recording" "UTF-8")
            source (sources/inspect-file! {:file file :size 9 :legacy-blob (blobs/sha256-hex bytes)})]
        (try
          (is (= (blob/blob-id bytes) (:content source)))
          (is (= "recording"
                 (sources/with-staged-file! resource source
                   (fn [staged]
                     (is (not= file staged))
                     (spit file "changed!!")
                     (slurp staged)))))
          (finally (cache/close! resource)))))))

(deftest source-change-before-staging-is-refused
  (doseq [changed ["truncated" "overlong source" "bad"]]
    (with-file!
      (fn [file]
        (let [resource (cache/create)
              source (sources/inspect-file! {:file file :size 9 :legacy-blob (blobs/sha256-hex (.getBytes "recording" "UTF-8"))})
              consumed (atom false)]
          (try
            (spit file changed)
            (is (thrown? clojure.lang.ExceptionInfo
                         (sources/with-staged-file! resource source (fn [_] (reset! consumed true)))))
            (is (false? @consumed))
            (is (empty? (:entries @(:state resource))))
            (finally (cache/close! resource))))))))

(deftest image-hydration-obeys-the-remaining-aggregate-limit
  (let [resource (cache/create) calls (atom []) bytes (byte-array [(unchecked-byte 255) (unchecked-byte 216) (unchecked-byte 255) 0 0])]
    (try
      (with-redefs [blobs/get-bytes-bounded (fn [id limit] (swap! calls conj [id limit]) bytes)]
        (is (= {:content (blob/blob-id bytes) :size 5 :extension "jpg"} (sources/stage-image! resource "legacy" 10)))
        (is (= [["legacy" 10]] @calls)))
      (with-redefs [blobs/get-bytes-bounded (fn [& _] nil)]
        (is (thrown? clojure.lang.ExceptionInfo (sources/stage-image! resource "missing" 10))))
      (finally (cache/close! resource)))))
