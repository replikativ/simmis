(ns is.simm.ops.worker-endpoints-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.discourse :as discourse]
            [dvergr.model.providers :as providers]
            [is.simm.ops.worker-broker :as broker]
            [is.simm.ops.worker-executor :as executor]
            [is.simm.ops.worker-executor-test :as fixture]
            [is.simm.ops.worker-frames :as frames]
            [is.simm.ops.worker-guest :as guest]
            [is.simm.ops.worker-inference-test :as model]
            [is.simm.ops.worker-wire :as wire])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream PipedInputStream PipedOutputStream]))

(def limit (:max-frame-bytes frames/policy))
(defn bytes-of [values]
  (let [out (ByteArrayOutputStream.)
        ch (frames/channel (ByteArrayInputStream. (byte-array 0)) out)]
    (doseq [v values] (frames/write! ch v limit))
    (.toByteArray out)))
(defn reader [bytes] (frames/channel (ByteArrayInputStream. bytes) (ByteArrayOutputStream.)))
(defn envelope [request] {"version" 1 "kind" "inference" "request" (wire/encode request)})
(defn opening [input] {"version" 1 "kind" "execute" "frozen" (wire/encode input)})
(defn duplex []
  (let [to-endpoint (PipedInputStream. 65536)
        from-endpoint (PipedInputStream. 65536)
        parent-out (PipedOutputStream. to-endpoint)
        endpoint-out (PipedOutputStream. from-endpoint)]
    {:parent (frames/channel from-endpoint parent-out)
     :endpoint (frames/channel to-endpoint endpoint-out)
     :parent-out parent-out :streams [to-endpoint from-endpoint parent-out endpoint-out]}))

(deftest explicit-wire-normalization-preserves-arbitrary-data-keys
  (let [input (fixture/frozen {"ns/🦋.md" fixture/binary ":literal.md" fixture/binary})]
    (is (= input (wire/frozen! (wire/encode input)))))
  (let [r (assoc-in model/request [:options :tools]
                    [{:name "clojure_eval" :parameters {"properties" {"ns/key" {"type" "string"}
                                                                      ":literal" {"type" "string"}}}}])]
    (is (= r (wire/request! (wire/encode r)))))
  (is (thrown? clojure.lang.ExceptionInfo (wire/encode {:x 1 "x" 2})))
  (is (thrown? clojure.lang.ExceptionInfo (wire/frozen! (assoc (wire/encode (fixture/frozen)) "input-id" "bad"))))
  (is (thrown? clojure.lang.ExceptionInfo
               (wire/reply! {"version" 1 "sequence" 1 "response"
                             {"content" "" "tool-calls" [{"id" "1" "name" "shell" "input" {"command" "no"}}]}}))))

(deftest invalid-guest-input-cannot-allocate-or-replace-host-provider
  (doseq [bad [(assoc (opening (fixture/frozen)) "extra" 1)
               (assoc-in (opening (fixture/frozen)) ["frozen" "input-id"] (str (random-uuid)))
               (assoc-in (opening (fixture/frozen)) ["frozen" "files" "docs/input.bin" "sha256"] (apply str (repeat 64 "0")))]]
    (let [out (ByteArrayOutputStream.) allocated (atom false)]
      (with-redefs [providers/providers (atom {})
                    discourse/make-room (fn [& _] (reset! allocated true) (throw (Exception.)))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (guest/serve! (frames/channel (ByteArrayInputStream. (bytes-of [bad])) out))))
        (is (empty? @providers/providers)))
      (is (false? @allocated))
      (is (zero? (.size out)))))
  (let [existing {:host-provider ::credentialed}
        out (ByteArrayOutputStream.)]
    (with-redefs [providers/providers (atom existing)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (guest/serve! (frames/channel (ByteArrayInputStream. (bytes-of [(opening (fixture/frozen))])) out))))
      (is (= existing @providers/providers))
      (is (zero? (.size out))))))

(deftest broker-refuses-replay-and-config-escalation-without-extra-inference
  (doseq [bad [(envelope model/request)
               {"version" 1 "kind" "configure" "config" {}}
               {"version" 1 "kind" "inference" "request" (assoc (wire/encode model/request) "url" "https://outside.invalid")}
               {"version" 1 "kind" "close" "request" {}}]]
    (let [calls (atom 0) out (ByteArrayOutputStream.)
          input (bytes-of [(envelope model/request) bad])
          endpoint (frames/channel (ByteArrayInputStream. input) out)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (broker/serve! endpoint model/config
                                  (fn [_ _] (swap! calls inc) model/answer))))
      (is @(:closed? endpoint))
      (is (thrown? clojure.lang.ExceptionInfo
                   (broker/serve! endpoint model/config (fn [& _] (swap! calls inc) model/answer))))
      (is (= 1 @calls))
      (let [ch (reader (.toByteArray out))]
        (is (= "broker-ready" (get (frames/read! ch limit) "kind")))
        (is (= "inference-result" (get (frames/read! ch limit) "kind")))
        (is (nil? (frames/read! ch limit)))))))

(deftest trailing-execute-and-close-frames-are-not-ignored
  (let [out (ByteArrayOutputStream.)
        ch (frames/channel (ByteArrayInputStream. (bytes-of [(opening (fixture/frozen))
                                                             (opening (fixture/frozen))])) out)]
    (with-redefs [providers/providers (atom {})
                  executor/execute! (constantly {:files {} :summary "fixture" :exit-code 0})]
      (is (thrown? clojure.lang.ExceptionInfo (guest/serve! ch)))
      (is @(:closed? ch))))
  (let [out (ByteArrayOutputStream.)
        close {"version" 1 "kind" "close"}
        ch (frames/channel (ByteArrayInputStream. (bytes-of [close close])) out)
        calls (atom 0)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (broker/serve! ch model/config (fn [& _] (swap! calls inc) model/answer))))
    (is (zero? @calls))
    (is @(:closed? ch))))

(deftest actual-dvergr-tool-loop-crosses-both-byte-stream-endpoints
  (let [g (duplex) b (duplex) calls (atom [])
        input (fixture/frozen)
        tasks (atom [])]
    (try
      (with-redefs [providers/providers (atom {})
                    providers/init-defaults! (fn [& _] (throw (Exception. "Never load host logins")))]
        (let [broker-task (future
                            (broker/serve! (:endpoint b) model/config
                                           (fn [messages opts]
                                             (swap! calls conj [messages opts])
                                             (if (= 1 (count @calls))
                                               (assoc model/answer :content "" :stop-reason :tool-use
                                                      :tool-calls [{:id "copy" :name "clojure_eval"
                                                                    :input {:code "(do (babashka.fs/create-dirs \"/out\") (babashka.fs/copy \"/docs/input.bin\" \"/out/🦋.bin\") (babashka.fs/delete \"/docs/remove.txt\"))"}}])
                                               (assoc model/answer :content "Binary deliverable ready.")))))
              guest-task (future (guest/serve! (:endpoint g)))]
          (reset! tasks [guest-task broker-task])
          (is (= "broker-ready" (get (frames/read! (:parent b) limit) "kind")))
          (frames/write! (:parent g) (opening input) limit)
          (let [ready (frames/read! (:parent g) limit)]
            (is (= "guest-ready" (get ready "kind")))
            (is (= (str (:input-id input)) (get ready "input-id"))))
          (dotimes [_ 2]
            (let [request (frames/read! (:parent g) limit)]
              (is (= "inference" (get request "kind")))
              (frames/write! (:parent b) request limit)
              (frames/write! (:parent g) (frames/read! (:parent b) limit) limit)))
          (let [result (frames/read! (:parent g) limit)]
            (is (= "result" (get result "kind")))
            (is (= (str (:input-id input)) (get result "input-id")))
            (is (= (wire/encode {:files {"out/🦋.bin" (assoc fixture/binary :mode 0x81A4) "docs/remove.txt" nil}
                                 :summary "Binary deliverable ready." :exit-code 0}) (get result "output"))))
          (.close ^PipedOutputStream (:parent-out g))
          (frames/write! (:parent b) {"version" 1 "kind" "close"} limit)
          (.close ^PipedOutputStream (:parent-out b))
          (is (nil? (deref guest-task 10000 ::timeout)))
          (is (nil? (deref broker-task 10000 ::timeout)))
          (is (= 2 (count @calls)))
          (is (some #(and (= "function_call_output" (:type %)) (= "copy" (:call_id %)))
                    (first (second @calls))))))
      (finally
        (doseq [stream (concat (:streams g) (:streams b))] (.close ^java.io.Closeable stream))
        (doseq [task @tasks] (future-cancel task))))))
