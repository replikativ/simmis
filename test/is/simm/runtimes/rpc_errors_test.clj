(ns is.simm.runtimes.rpc-errors-test
  "A failed remote call tells the client a short message, an allowlisted type
   and a correlation id — no stack frames, no ex-data — and the server log has
   the full exception under the same id (`is.simm.runtimes.rpc-errors`).

   distributed-scope sends `(pr-str e)` of the failure; before simmis wrapped
   the gate and the handlers, that was the JVM's `#error {…}` with every frame,
   the class names and all of ex-data."
  (:require [clojure.core.async :as a]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]
            [is.simm.authority.world :as world]
            [is.simm.authority.rpc-wire :as rpc-wire]
            [is.simm.distributed-scope :as ds]
            [is.simm.runtimes.rpc-errors :as rpc-errors]
            [is.simm.uis.web.desktop.remote :as remote]
            [is.simm.uis.web.desktop.sandbox-remote]
            [kabel.http-kit :as http-kit]
            [kabel.middleware.transit :refer [transit]]
            [kabel.peer :as peer]
            [superv.async :as sv]
            [taoensso.telemere :as tel])
  (:import (java.net ServerSocket)))

(defn- frame-free?
  "Nothing in `s` that comes from the server's code or its JVM: no stack frame
   lines, no trace, no class or namespace names, no file names."
  [s]
  (not (re-find #"(?m)^\s*at |:trace|:via|\.clj|\.java|java\.|clojure\.lang|is\.simm|distributed_scope|\$fn" s)))

(defn- client-map [s] (edn/read-string s))

(defn- signal [signals id]
  (first (filter #(= id (:id %)) signals)))

;; =============================================================================
;; Through distributed-scope's inbound dispatch, as the server wires it
;; =============================================================================

(deftest a-refused-call-gets-a-short-error-and-a-correlation-id
  (world/with-world [_ w]
    (let [{resp :value signals :signals}
          (tel/with-signals
            (world/inbound! (world/principal w :member) "eval-in-room!"
                            {:room-uuid-str (str (:room w)) :code "(+ 1 1)"}))
          e (:error resp)
          m (client-map e)]
      (is (world/refused? resp))
      (is (frame-free? e) e)
      (is (= #{:error :type :correlation-id} (set (keys m))))
      (is (= "not authorized" (:error m)))
      (is (= :not-authorized (:type m)))
      (is (parse-uuid (:correlation-id m)))
      (testing "the log ties the id to the refusal"
        (let [s (signal signals ::rpc-errors/remote-call-refused)]
          (is (= (:correlation-id m) (get-in s [:data :correlation-id])))
          (is (= :not-authorized (get-in s [:data :type])))
          (is (= (str (:member w)) (get-in s [:data :sub]))))
        (is (some? (signal signals :is.simm.model.access/rpc-denied))
            "and access's own entry still says which policy refused it"))
      (testing "an anonymous connection is told to authenticate (the client reauths on it)"
        (let [m (client-map (:error (world/inbound! nil "eval-in-room!"
                                                    {:room-uuid-str (str (:room w)) :code "1"})))]
          (is (= {:error "authentication required" :type :authentication-required}
                 (dissoc m :correlation-id))))))))

(defn- dispatch!
  "Deliver `fn-name` through distributed-scope's inbound dispatch behind the
   server's gate around `allow?`, and return the reply."
  [fn-name arg-map allow?]
  (rpc-errors/install!)
  (rpc-wire/invoke! {:sub (str (random-uuid))} fn-name arg-map
                    (rpc-errors/gate (fn [& _] allow?))))

(defn- register! [fn-name f] (ds/register-remote-fn! fn-name f))

(deftest shared-registry-supports-values-and-sanitizes-gate-errors
  (testing "ordinary values and nil are not mistaken for channels"
    (is (= 42 (a/<!! ((rpc-errors/wrap-handler 'test/value (constantly 42)) {}))))
    (is (nil? (a/<!! ((rpc-errors/wrap-handler 'test/nil (constantly nil)) {})))))
  (testing "unexpected policy failures carry no private data to the client"
    (let [gate (rpc-errors/gate (fn [& _]
                                  (throw (ex-info "Policy unavailable" {:secret "private-token"}))))
          error (try (gate {:sub "alice"} 'test/value {}) nil (catch Throwable e e))
          wire (pr-str error)]
      (is (rpc-errors/client-error? error))
      (is (not (re-find #"private-token|:secret|:trace" wire)))
      (is (parse-uuid (:correlation-id (ex-data error)))))))

(deftest client-normalizes-both-wire-error-formats
  (let [safe {:error "not authorized" :type :not-authorized
              :correlation-id (str (random-uuid))}]
    (is (= safe (remote/error-info (ex-info "legacy" {:error (pr-str safe)}))))
    (is (= safe (remote/error-info (ex-info "modern" {:type (:type safe)
                                                      :data (pr-str (dissoc safe :type))}))))
    (is (= {:error "bad request"}
           (remote/error-info (ex-info "bounded" {:error {:error "bad request" :secret "x"
                                                          :type :unreviewed :correlation-id 5}}))))
    (is (nil? (remote/error-info (ex-info "oversize" {:data (str "{" (apply str (repeat 5000 "x")))}))))))

(deftest memory-target-change-has-one-bounded-public-classification
  (let [id (random-uuid)
        error (ex-info "The Memory repository changed; refresh before editing."
                       {:type :memory/target-changed :reason :target-changed
                        :target {:store "private-store"} :connection "private-handle"})
        safe (rpc-errors/client-info error id)]
    (is (= {:error (ex-message error) :type :memory/target-changed :correlation-id (str id)} safe))
    (is (= safe (remote/error-info (ex-info "legacy" {:error (pr-str safe)}))))
    (is (= safe (remote/error-info (ex-info "modern" {:type (:type safe)
                                                      :data (pr-str (dissoc safe :type))}))))
    (is (not (re-find #"private-store|private-handle|:reason|:target " (pr-str safe))))))

(deftest a-handler-error-gets-only-its-message
  (let [secret-ex (ex-info "Room not found\nwhile resolving /home/x/.dvergr/systems"
                           {:type :internal/lookup :secret "s3cr3t" :path "/home/x/.dvergr"})]
    (register! 'rpc-errors-test/throws-info (fn [_] (throw secret-ex)))
    (register! 'rpc-errors-test/throws-npe
               (fn [_] (throw (NullPointerException. "Cannot invoke \"Object.toString()\" because \"room\" is null"))))
    (register! 'rpc-errors-test/delivers-validation
               (fn [_] (a/go (ex-info "Title must not be empty" {:type :validation-error :field :title}))))
    (register! 'rpc-errors-test/ok (fn [{:keys [x]}] (a/go (inc x))))
    (try
      (testing "an ex-info: its first line, no type that is not allowlisted, no ex-data"
        (let [{resp :value signals :signals}
              (tel/with-signals (dispatch! 'rpc-errors-test/throws-info {} true))
              e (:error resp)
              m (client-map e)]
          (is (frame-free? e) e)
          (is (not (re-find #"s3cr3t|\.dvergr|:path|:secret|internal" e)) e)
          (is (= #{:error :correlation-id} (set (keys m))))
          (is (= "Room not found" (:error m)))
          (testing "the log has the whole exception under the same id"
            (let [s (signal signals ::rpc-errors/remote-call-failed)]
              (is (= (:correlation-id m) (get-in s [:data :correlation-id])))
              (is (identical? secret-ex (:error s)))
              (is (pos? (count (.getStackTrace ^Throwable (:error s)))))))))
      (testing "anything else: \"internal error\", whatever its message says"
        (let [m (client-map (:error (dispatch! 'rpc-errors-test/throws-npe {} true)))]
          (is (= "internal error" (:error m)))
          (is (parse-uuid (:correlation-id m)))))
      (testing "an error delivered on the channel, with an allowlisted type"
        (is (= {:error "Title must not be empty" :type :validation-error}
               (dissoc (client-map (:error (dispatch! 'rpc-errors-test/delivers-validation {} true)))
                       :correlation-id))))
      (testing "a result is untouched"
        (is (= {:result 42} (select-keys (dispatch! 'rpc-errors-test/ok {:x 41} true) [:result :error]))))
      (testing "the gate refuses before the handler runs"
        (let [m (client-map (:error (dispatch! 'rpc-errors-test/throws-info {} false)))]
          (is (= "not authorized" (:error m)))))
      (testing "a handler registered later (a reloaded namespace) is wrapped too"
        (register! 'rpc-errors-test/throws-info (fn [_] (throw (ex-info "again" {:secret "s3cr3t"}))))
        (let [e (:error (dispatch! 'rpc-errors-test/throws-info {} true))]
          (is (= "again" (:error (client-map e))))
          (is (not (re-find #"s3cr3t" e)))))
      (finally
        (doseq [f '[rpc-errors-test/throws-info rpc-errors-test/throws-npe
                    rpc-errors-test/delivers-validation rpc-errors-test/ok]]
          (ds/unregister-remote-fn! f))))))

;; =============================================================================
;; Over a websocket, to the client's error path
;; =============================================================================

(defn- free-port []
  (with-open [^ServerSocket ss (ServerSocket. 0)]
    (.getLocalPort ss)))

(defn- abort! [supervisor]
  (doseq [abort-ch (:aborts supervisor)]
    (a/put! abort-ch :abort)
    (a/close! abort-ch)))

(deftest a-failure-reaches-the-clients-error-path
  ;; A server peer wired like simmis's (remote-middleware, invoke-on-peer behind
  ;; the gate, handlers wrapped) and a client peer, in one JVM over a real
  ;; websocket. `invoke-remote` delivers a failure as the value of its channel,
  ;; which is what the client's error callbacks (`remote/invoke!`,
  ;; `remote/spin!`) branch on, and `remote/error-text` is what they show.
  (let [port (free-port)
        url (str "ws://localhost:" port)
        server-id (random-uuid)
        client-id (random-uuid)
        sup (sv/simple-supervisor)
        handler (http-kit/create-http-kit-handler! sup url server-id)
        server (peer/server-peer sup handler server-id ds/remote-middleware transit)
        client (peer/client-peer sup client-id ds/remote-middleware transit)
        call (fn [fn-name arg-map]
               (a/<!! (ds/invoke-remote server-id fn-name arg-map {:force-remote? true})))]
    (rpc-errors/install!)
    (register! 'rpc-errors-test/wire-throws
               (fn [_] (throw (ex-info "Room not found" {:secret "s3cr3t"}))))
    (register! 'rpc-errors-test/wire-ok (fn [_] (a/go :fine)))
    (try
      (ds/invoke-on-peer server {:supervisor sup
                                 :authorize-fn (rpc-errors/gate
                                                (fn [_ fn-name _]
                                                  (not= fn-name 'rpc-errors-test/wire-refused)))})
      (sv/<?? sup (peer/start server))
      (ds/invoke-on-peer client {:supervisor sup})
      (sv/<?? sup (peer/connect sup client url))
      (testing "a result"
        (is (= :fine (call 'rpc-errors-test/wire-ok {}))))
      (testing "a handler error: the client's error path, with the message"
        (let [err (call 'rpc-errors-test/wire-throws {})
              e (pr-str (ex-data err))]
          (is (instance? Throwable err))
          (is (frame-free? e) e)
          (is (not (re-find #"s3cr3t" e)))
          (is (= "Room not found" (:error (remote/error-info err))))
          (is (re-matches #"Room not found \(ref [0-9a-f]{8}\)" (remote/error-text err)))))
      (testing "a refusal (no principal on this connection: authenticate)"
        (let [err (call 'rpc-errors-test/wire-refused {})]
          (is (instance? Throwable err))
          (is (frame-free? (pr-str (ex-data err))))
          (is (= :authentication-required (:type (remote/error-info err))))
          (is (re-matches #"authentication required \(ref [0-9a-f]{8}\)" (remote/error-text err)))))
      (finally
        (ds/unregister-remote-fn! 'rpc-errors-test/wire-throws)
        (ds/unregister-remote-fn! 'rpc-errors-test/wire-ok)
        (sv/<?? sv/S (peer/stop server))
        (abort! sup)))))
