(ns is.simm.runtimes.media-cache-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.blob :as blob]
            [is.simm.runtimes.media-cache :as cache])
  (:import [java.nio ByteBuffer]
           [java.nio.file Files]))

(defn- bytes-of [text] (.getBytes ^String text "UTF-8"))
(defn- buffer-text [^ByteBuffer buffer]
  (let [out (byte-array (.remaining buffer))] (.get buffer out) (String. out "UTF-8")))
(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(deftest verified-immutable-mappings-have-independent-response-positions
  (let [resource (cache/create {:max-file-bytes 100 :max-bytes 200 :max-entries 2})
        value (bytes-of "0123456789") id (blob/blob-id value) copies (atom 0)
        copy! #(do (swap! copies inc) (.write % value))]
    (try
      (let [first (cache/materialize! resource id 10 copy!)
            second (cache/materialize! resource id 10 copy!)]
        (is (= 1 @copies))
        (is (.isReadOnly ^ByteBuffer (:buffer first)))
        (is (= "234" (buffer-text (cache/slice first 2 3))))
        (is (= "0123456789" (buffer-text (:buffer second))))
        (is (= "0123456789" (buffer-text (:buffer first))))
        (is (= :invalid-range (reason #(cache/slice first 9 2))))
        (is (= :invalid-range (reason #(cache/slice first -1 1))))
        (let [queued (cache/slice first 4 2)
              paths (:paths @(:state resource))]
          (is (seq paths))
          (cache/close! resource)
          (is (every? #(not (Files/exists % (make-array java.nio.file.LinkOption 0))) paths))
          (is (= "45" (buffer-text queued)) "Unix unlink does not invalidate queued read-only buffers")))
      (is (= :closed (reason #(cache/materialize! resource id 10 copy!))))
      (finally (cache/close! resource)))))

(deftest failed-content-never-publishes-a-cache-entry-or-leaves-staging
  (let [resource (cache/create {:max-file-bytes 100 :max-bytes 200 :max-entries 2})
        value (bytes-of "expected") id (blob/blob-id value)]
    (try
      (is (= :content-mismatch (reason #(cache/materialize! resource id 8
                                                            (fn [out] (.write out (bytes-of "unwanted")))))))
      (is (empty? (:entries @(:state resource))))
      (is (empty? (:paths @(:state resource))))
      (is (= :size-mismatch (reason #(cache/materialize! resource id 8 (fn [out] (.write out (byte-array 9)))))))
      (is (empty? (:paths @(:state resource))))
      (is (= :size-mismatch (reason #(cache/materialize! resource id 8 (fn [out] (.write out (byte-array 7)))))))
      (is (empty? (:paths @(:state resource))))
      (let [failure (ex-info "Producer failed" {})]
        (is (identical? failure (try (cache/materialize! resource id 8 (fn [_] (throw failure)))
                                     (catch Exception error error)))))
      (is (zero? (:bytes @(:state resource))))
      (finally (cache/close! resource)))))

(deftest cache-limits-are-enforced-before-new-materialization
  (let [resource (cache/create {:max-file-bytes 10 :max-bytes 12 :max-entries 2})
        a (bytes-of "1234567") b (bytes-of "abcdefg") calls (atom 0)
        copy! (fn [bytes] (fn [out] (swap! calls inc) (.write out bytes)))]
    (try
      (cache/materialize! resource (blob/blob-id a) 7 (copy! a))
      (is (= :capacity (reason #(cache/materialize! resource (blob/blob-id b) 7 (copy! b)))))
      (is (= :size-mismatch (reason #(cache/materialize! resource (blob/blob-id a) 6 (copy! a)))))
      (is (= :invalid-content (reason #(cache/materialize! resource (blob/blob-id b) 11 (copy! b)))))
      (is (= 1 @calls))
      (finally (cache/close! resource)))))

(deftest concurrent-readers-share-one-physical-materialization
  (let [resource (cache/create {:max-file-bytes 100 :max-bytes 200 :max-entries 2})
        value (bytes-of "immutable") id (blob/blob-id value)
        entered (promise) release (promise) second-started (promise) copies (atom 0)
        copy! (fn [out] (swap! copies inc) (deliver entered true) @release (.write out value))
        first-result (future (cache/materialize! resource id (alength value) copy!))]
    (try
      (is (= true (deref entered 5000 ::timeout)))
      (let [second-result (future (deliver second-started true)
                                  (cache/materialize! resource id (alength value) copy!))]
        (is (= true (deref second-started 5000 ::timeout)))
        (deliver release true)
        (is (= "immutable" (buffer-text (:buffer (deref first-result 5000 {})))))
        (is (= "immutable" (buffer-text (:buffer (deref second-result 5000 {})))))
        (is (= 1 @copies)))
      (finally (deliver release true) @first-result (cache/close! resource)))))

(deftest failed-staging-cleanup-holds-the-cache-until-explicit-close
  (let [resource (cache/create {:max-file-bytes 100 :max-bytes 200 :max-entries 2})
        value (bytes-of "expected") id (blob/blob-id value) copies (atom 0)]
    (try
      (with-redefs [cache/remove-path! (fn [& _] (throw (java.io.IOException. "unlink failed")))]
        (let [failure (try (cache/materialize! resource id 8
                                               (fn [out] (swap! copies inc) (.write out (bytes-of "unwanted"))))
                           nil (catch Exception error error))]
          (is (= :content-mismatch (:reason (ex-data failure))))
          (is (= 1 (count (.getSuppressed failure))))
          (is (:closed? @(:state resource)))
          (is (seq (:paths @(:state resource)))))
        (is (= :closed (reason #(cache/materialize! resource id 8
                                                    (fn [out] (swap! copies inc) (.write out value))))))
        (is (= 1 @copies)))
      (is (true? (cache/close! resource)))
      (is (empty? (:paths @(:state resource))))
      (finally (cache/close! resource)))))

(deftest failed-close-retains-a-retryable-closed-resource
  (let [resource (cache/create {:max-file-bytes 100 :max-bytes 200 :max-entries 2})
        value (bytes-of "expected") id (blob/blob-id value)]
    (try
      (cache/materialize! resource id 8 #(.write % value))
      (with-redefs [cache/remove-path! (fn [& _] (throw (java.io.IOException. "unlink failed")))]
        (is (thrown? java.io.IOException (cache/close! resource))))
      (is (:closed? @(:state resource)))
      (is (seq (:paths @(:state resource))))
      (is (= :closed (reason #(cache/materialize! resource id 8 (fn [out] (.write out value))))))
      (is (true? (cache/close! resource)))
      (is (empty? (:paths @(:state resource))))
      (finally (cache/close! resource)))))

(deftest close-joins-an-active-native-materializer
  (let [resource (cache/create {:max-file-bytes 100 :max-bytes 200 :max-entries 2})
        value (bytes-of "expected") id (blob/blob-id value)
        entered (promise) release (promise) close-started (promise)
        produced (future (cache/materialize! resource id 8
                                             (fn [out] (deliver entered true) @release (.write out value))))]
    (try
      (is (= true (deref entered 5000 ::timeout)))
      (let [closed (future (deliver close-started true) (cache/close! resource))]
        (is (= true (deref close-started 5000 ::timeout)))
        (deliver release true)
        (is (= "expected" (buffer-text (:buffer (deref produced 5000 {})))))
        (is (= true (deref closed 5000 ::timeout)))
        (is (:closed? @(:state resource)))
        (is (empty? (:paths @(:state resource)))))
      (finally (deliver release true) @produced (cache/close! resource)))))

(deftest verified-file-borrow-is-outside-cache-lock-and-has-owned-cleanup
  (let [resource (cache/create {:max-file-bytes 100 :max-bytes 200 :max-entries 2})
        value (bytes-of "expected") id (blob/blob-id value) borrowed (atom nil)]
    (try
      (is (= :consumed
             (cache/with-materialized-file! resource id 8 #(.write % value)
               (fn [file]
                 (reset! borrowed file)
                 (is (false? (Thread/holdsLock (:guard resource))))
                 (is (= "expected" (slurp file)))
                 :consumed))))
      (is (.exists ^java.io.File @borrowed))
      (cache/close! resource)
      (is (false? (.exists ^java.io.File @borrowed)))
      (is (= :closed (reason #(cache/with-materialized-file! resource id 8
                                (fn [_] (throw (AssertionError. "must not copy"))) identity))))
      (finally (cache/close! resource)))))
