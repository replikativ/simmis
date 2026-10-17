(ns is.simm.ops.worker-frames-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.ops.worker-frames :as frames])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream InputStream OutputStream IOException]
           [java.nio.charset StandardCharsets]))

(defn bytes-of [s] (.getBytes ^String s StandardCharsets/UTF_8))
(defn input-channel [s]
  (frames/channel (ByteArrayInputStream. (bytes-of s)) (ByteArrayOutputStream.)))
(defn refused? [f]
  (= {:type :is.simm.ops.worker-frames/refused}
     (try (f) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))))

(deftest multiple-frames-preserve-filenames-and-escaped-content
  (let [out (ByteArrayOutputStream.)
        ch (frames/channel (ByteArrayInputStream. (byte-array 0)) out)
        a {"kind" "execute" "files" {"ns/a.md" {"text" "雪🦋\n\r\t\""}
                                     ":literal.md" {} "a:b.md" {} "🦋.md" {}}}
        b {"kind" "inference" "sequence" 2}]
    (frames/write! ch a 4096)
    (frames/write! ch b 4096)
    (let [reader (frames/channel (ByteArrayInputStream. (.toByteArray out)) (ByteArrayOutputStream.))]
      (is (= a (frames/read! reader 4096)))
      (is (= b (frames/read! reader 4096)))
      (is (nil? (frames/read! reader 4096)))
      (is (refused? #(frames/read! reader 4096))))))

(deftest malformed-input-permanently-poisons-session
  (doseq [payload ["{\"a\":1,\"a\":2}\n" "{\"x\":{\"a\":1,\"a\":2}}\n"
                   "{} {}\n" "[]\n" "null\n" "true\n" "\n" "{\n" "{}"
                   "{\"x\":NaN}\n" "{\"x\":Infinity}\n" "{\"x\":1e999}\n"
                   "{\"x\":9223372036854775808}\n" "{\"x\":\"raw\nnewline\"}\n"
                   "{\"x\":\"\\ud800\"}\n" "{\"\\udc00\":1}\n"
                   "{\"a\":1,\"\\u0061\":2}\n"
                   (str (apply str (repeat 40 "{\"a\":")) "{}" (apply str (repeat 40 "}")) "\n")]]
    (let [ch (input-channel (str payload "{}\n"))]
      (is (refused? #(frames/read! ch 4096)) (pr-str payload))
      (is (refused? #(frames/read! ch 4096)))))
  (let [payload (byte-array [(byte 123) (byte 34) (byte 120) (byte 34) (byte 58)
                             (byte 34) (unchecked-byte 0xc3) (byte 34) (byte 125) (byte 10)])
        ch (frames/channel (ByteArrayInputStream. payload) (ByteArrayOutputStream.))]
    (is (refused? #(frames/read! ch 4096)))))

(deftest exact-wire-size-and-read-allocation-bounds
  (let [ch (input-channel "{}\n{}\n")]
    (is (= {} (frames/read! ch 2)))
    (is (= {} (frames/read! ch 2))))
  (let [read-count (atom 0)
        in (proxy [InputStream] []
             (read
               ([] (swap! read-count inc) 32)
               ([b off length] (swap! read-count inc) (aset-byte b off (byte 32)) 1)))
        ch (frames/channel in (ByteArrayOutputStream.))]
    (is (refused? #(frames/read! ch 10)))
    (is (= 11 @read-count) "Stop after the first byte exceeding the limit")))

(deftest output-refusal-writes-nothing-and-poisons-session
  (doseq [value [{:keyword "not a JSON key"} {"x" ##NaN} {"x" ##Inf}
                 {"x" (Object.)} {"x" '(1 2)} {"x" 9223372036854775808N}
                 [] {"x" (String. (char-array [(char 0xd800)]))}
                 {"x" (apply str (repeat 200 "a"))}
                 ;; Individually small shared values still have bounded total
                 ;; encoded size, even when the graph is tiny in memory.
                 {"x" (vec (repeat 50 "abc"))}]]
    (let [out (ByteArrayOutputStream.)
          ch (frames/channel (ByteArrayInputStream. (byte-array 0)) out)]
      (is (refused? #(frames/write! ch value 100)))
      (is (zero? (.size out)))
      (is (refused? #(frames/write! ch {} 100)))))
  (let [out (ByteArrayOutputStream.)
        ch (frames/channel (ByteArrayInputStream. (byte-array 0)) out)]
    (frames/write! ch {} 2)
    (is (= "{}\n" (.toString out "UTF-8")))))

(deftest token-limit-is-enforced-before-databinding
  (with-redefs [frames/policy (assoc frames/policy :max-tokens 4)]
    (is (refused? #(frames/read! (input-channel "{\"x\":[0,0,0,0]}\n") 100))))
  (with-redefs [frames/policy (assoc frames/policy :max-nodes 2)]
    (let [out (ByteArrayOutputStream.)
          ch (frames/channel (ByteArrayInputStream. (byte-array 0)) out)]
      (is (refused? #(frames/write! ch {"x" [0]} 100)))
      (is (zero? (.size out))))))

(deftest underlying-io-failure-cannot-be-retried-on-same-channel
  (let [writes (atom 0)
        out (proxy [OutputStream] []
              (write [b] (when (> (swap! writes inc) 1) (throw (IOException. "private detail")))))
        ch (frames/channel (ByteArrayInputStream. (byte-array 0)) out)]
    (is (refused? #(frames/write! ch {"x" 1} 100)))
    (let [before @writes]
      (is (pos? before) "A real pipe failure can leave a partial frame")
      (is (refused? #(frames/write! ch {} 100)))
      (is (= before @writes)))))

(deftest parser-errors-do-not-disclose-content
  (try
    (frames/read! (input-channel "{private-secret}\n") 100)
    (is false)
    (catch clojure.lang.ExceptionInfo e
      (is (= "Invalid local worker process frame" (.getMessage e)))
      (is (nil? (.getCause e))))))
