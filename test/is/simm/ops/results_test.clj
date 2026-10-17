(ns is.simm.ops.results-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.results :as results]
            [is.simm.ops.worker-files :as worker-files]
            [jsonista.core :as json])
  (:import [java.io ByteArrayOutputStream]
           [java.nio.charset StandardCharsets]
           [java.util Base64]))

(def attempt-id (str (random-uuid)))
(def args {:room "example" :id attempt-id})

(defn- source [files]
  {:id attempt-id :content-id "certified-attempt" :run-id (str (random-uuid))
   :evidence {:files files}})

(deftest binary-evidence-remains-lossless-and-preserves-regular-file-mode
  (let [bytes (byte-array (map unchecked-byte [0 255 128 13 10 42]))
        file {:base64 (.encodeToString (Base64/getEncoder) bytes)
              :bytes (alength bytes) :sha256 (worker-files/sha256 bytes) :mode 0x81ED}]
    (with-redefs [ops/invoke (fn [& _] (source {"/out/program.bin" file}))]
      (let [manifest (results/attempt-files nil :member args)
            chunk (results/attempt-file nil :member (assoc args :path "/out/program.bin"))]
        (is (= 6 (get-in manifest [:files 0 :size])))
        (is (= 0x81ED (get-in manifest [:files 0 :mode])))
        (is (= 0x81ED (:mode chunk)))
        (is (= "application/octet-stream" (:media-type chunk)))
        (is (= (vec bytes) (vec (.decode (Base64/getDecoder) ^String (:base64 chunk)))))
        (is (= (get-in manifest [:files 0 :content-id]) (:content-id chunk)))))
    (doseq [bad [(assoc file :sha256 (apply str (repeat 64 "0")))
                 (assoc file :bytes 99) (assoc file :base64 "bad!")
                 (assoc file :mode 0xA1FF) (assoc file :host-path "/secret") nil]]
      (with-redefs [ops/invoke (fn [& _] (source {"/out/program.bin" bad}))]
        (is (thrown? clojure.lang.ExceptionInfo (results/attempt-files nil :member args)))
        (is (thrown? clojure.lang.ExceptionInfo
                     (results/attempt-file nil :member (assoc args :path "/out/program.bin"))))))))

(deftest captured-file-chunks-are-bounded-and-lossless
  (let [text (apply str (repeat 20000 "汉🦋a"))
        attempt (source {"/out/result.md" text})
        chunk-size 16382
        calls (atom [])]
    (with-redefs [ops/invoke (fn [_ principal op input]
                               (swap! calls conj [principal op input]) attempt)]
      (let [output (ByteArrayOutputStream.)]
        (loop [offset 0]
          (let [chunk (results/attempt-file nil :member
                                            (assoc args :path "/out/result.md" :offset offset :length chunk-size))
                bytes (.decode (Base64/getDecoder) ^String (:base64 chunk))]
            (is (<= (:length chunk) chunk-size))
            (is (= attempt-id (get-in chunk [:attempt :id])))
            (.write output bytes 0 (alength bytes))
            (when-let [next (:next-offset chunk)] (recur next))))
        (is (= text (String. (.toByteArray output) StandardCharsets/UTF_8))))
      (is (every? #(= [:member :attempt/detail args] %) @calls)))))

(deftest captured-file-manifest-is-paged-and-names-content
  (let [files (into {} (for [i (range 103)] [(format "/out/%03d.txt" i) (str i)]))]
    (with-redefs [ops/invoke (fn [& _] (source files))]
      (let [first-page (results/attempt-files nil :member args)
            last-page (results/attempt-files nil :member (assoc args :offset 100))]
        (is (= [103 100 100] ((juxt :total :next-offset (comp count :files)) first-page)))
        (is (= 3 (count (:files last-page))))
        (is (nil? (:next-offset last-page)))
        (is (every? :content-id (:files first-page)))
        (is (not-any? #(contains? % :text) (:files first-page)))))))

(deftest invalid-or-missing-artifacts-do-not-fall-back-to-the-workspace
  (with-redefs [ops/invoke (fn [& _] (source {"/out/empty.txt" ""}))]
    (is (= 0 (:length (results/attempt-file nil :member (assoc args :path "/out/empty.txt")))))
    (doseq [input [(assoc args :path "/out/missing.txt")
                   (assoc args :path "../server-secret")
                   (assoc args :path "/out/empty.txt" :offset 1)
                   (assoc args :path "/out/empty.txt" :length 0)
                   (assoc args :path "/out/empty.txt" :length 16385)
                   (assoc args :path "/out/empty.txt" :sender "another-party")
                   (assoc args :path "/out/empty.txt" :id "invalid")]]
      (is (thrown? clojure.lang.ExceptionInfo (results/attempt-file nil :member input)))))
  (with-redefs [ops/invoke (fn [& _] nil)]
    (is (thrown? clojure.lang.ExceptionInfo (results/attempt-files nil :member args)))))

(deftest long-path-manifests-remain-machine-readable-and-page-without-loss
  (let [prefix (str "/out/" (apply str (repeat 2000 "x")))
        files (into {} (for [i (range 100)] [(str prefix i ".txt") "output"]))]
    (with-redefs [ops/invoke (fn [& _] (source files))]
      (loop [offset 0 paths []]
        (let [page (results/attempt-files nil :member (assoc args :offset offset))
              paths (into paths (map :path (:files page)))]
          (is (< (count (json/write-value-as-string page)) 30000))
          (is (seq (:files page)))
          (if-let [next (:next-offset page)]
            (do (is (> next offset)) (recur next paths))
            (do (is (= 100 (count paths)))
                (is (= (set (keys files)) (set paths))))))))))
