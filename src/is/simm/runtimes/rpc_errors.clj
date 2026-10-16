(ns is.simm.runtimes.rpc-errors
  "What a client learns when one of its remote calls fails.

   distributed-scope (`is.simm.distributed-scope`, `dispatch-invocation!`)
   answers a failed invocation with `{:error (pr-str e)}`. For a JVM exception
   that is the whole `#error {…}` rendering: every stack frame, the cause
   chain, the class and file names, and all of `ex-data`. A refused call sent
   that to the browser, and so did any handler that threw.

   The library builds that string itself, so simmis decides what the string
   SAYS: every exception that can reach that `pr-str` is replaced first by a
   client error (`client-error`), whose printed form is only

     {:error \"<short message>\" :type <allowlisted ex-data :type> :correlation-id \"<uuid>\"}

   and the original exception goes to the server log (Telemere) under the same
   correlation id, so a user's report can be matched to the trace.

   Two places can throw into that `pr-str`, and both are simmis's to wrap:

   - the gate. `gate` wraps `access/authorize-remote`: a refusal throws a
     client error of type `:not-authorized` (or `:authentication-required`
     for a connection with no principal) instead of returning false, so the
     library's own refusal exception, with its trace, is never built.
   - the handlers. `install!` wraps every function in distributed-scope's
     registry (`wrap-handler`) and keeps wrapping the ones registered later
     (a namespace reloaded in development re-registers its remotes).

   What travels:
   - `:error`: the message of an `ExceptionInfo` (the exceptions simmis and its
     libraries throw on purpose), first line only, at most `max-message`
     characters; for any other throwable (a NullPointerException, a library's
     IllegalStateException) just \"internal error\", since its message is
     written for whoever reads the code, not for the client.
   - `:type`: the `ex-data` `:type`, only when it is in `client-types`.
   - `:correlation-id`: always.
   Nothing else of `ex-data`, no cause, no frames.

   The client reads it back with `is.simm.uis.web.desktop.remote/error-info`."
  (:require [clojure.core.async :as a]
            [clojure.string :as str]
            [is.simm.distributed-scope :as ds]
            [taoensso.telemere :as log])
  (:import [clojure.lang ExceptionInfo]))

(def client-types
  "The `ex-data` `:type`s a client may see. A type is a contract with the client
   (it branches on it); one that is not listed here stays on the server."
  #{:not-authorized
    :authentication-required
    :validation-error
    :dispatch/invalid-mentions
    :dvergr.principal/denied})

(def max-message
  "The longest `:error` message sent to a client."
  200)

;; The error a client sees: a Throwable carrying the safe map as its ex-data,
;; whose own class `print-method` below dispatches on, so its `pr-str` (the one
;; distributed-scope sends) is that map and nothing else.
;;
;; A Throwable, NOT an Exception, on purpose. distributed-scope takes a
;; handler's channel with superv.async's `<?`, and `throw-if-exception` wraps
;; every `Exception` it takes in a fresh `ex-info` (to keep a trace across
;; go blocks) — whose printed form is a frame of superv.async, the class names
;; and our map underneath. A Throwable that is not an Exception passes through
;; `<?` as a value, which is what `dispatch-invocation!` then sends. No stack
;; trace is recorded (`writableStackTrace` false): there is nothing in it for
;; anyone.
(def ^:private client-error-class
  (get-proxy-class Throwable clojure.lang.IExceptionInfo))

(defn- client-error
  "A throwable whose printed form is `info`, and whose ex-data is `info`."
  [info]
  (let [e (construct-proxy client-error-class ^String (:error info) nil false false)]
    (init-proxy e {"getData" (fn [_] info)})
    e))

(defn client-error?
  "Is `x` an error already reduced to what the client may see?"
  [x]
  (instance? client-error-class x))

(defmethod print-method client-error-class [e ^java.io.Writer w]
  (print-method (ex-data e) w))

(defn- short-message [^Throwable e]
  (if (instance? ExceptionInfo e)
    (let [m (or (some-> (ex-message e) str/split-lines first str/trim not-empty)
                "error")]
      (if (> (count m) max-message) (str (subs m 0 max-message) "…") m))
    "internal error"))

(defn client-info
  "The map a client may see for `e`, with `correlation-id`."
  [^Throwable e correlation-id]
  (let [t (:type (ex-data e))]
    (cond-> {:error (short-message e) :correlation-id (str correlation-id)}
      (contains? client-types t) (assoc :type t))))

(defn sanitize
  "Log `e` in full under a fresh correlation id and return the client error
   that replaces it on the wire. An error that is already one passes through."
  [^Throwable e fn-name]
  (if (client-error? e)
    e
    (let [cid (random-uuid)
          info (client-info e cid)
          refusal? (contains? #{:not-authorized :authentication-required} (:type info))]
      (log/log! {:level (if refusal? :warn :error)
                 :id ::remote-call-failed
                 :msg "Remote call failed; the client got only the correlation id"
                 :error e
                 :data {:correlation-id (str cid)
                        :fn (str fn-name)
                        :client info}})
      (client-error info))))

(defn gate
  "The `:authorize-fn` for distributed-scope's `invoke-on-peer`, around
   `authorize` (`access/authorize-remote`): true when it allows, and when it
   refuses, a thrown client error of type `:not-authorized`, or
   `:authentication-required` when the connection carries no principal (the
   two types distributed-scope itself uses, so the client's reauth branch
   keeps working)."
  [authorize]
  (fn [principal fn-name arg-map]
    (or (authorize principal fn-name arg-map)
        (let [cid (random-uuid)
              t (if principal :not-authorized :authentication-required)
              info {:error (if principal "not authorized" "authentication required")
                    :type t
                    :correlation-id (str cid)}]
          ;; the reason (which policy row, which resource) is in authorize's
          ;; own ::rpc-denied entry; this one ties it to what the client got
          (log/log! {:level :warn :id ::remote-call-refused
                     :data {:correlation-id (str cid)
                            :fn (str fn-name)
                            :sub (:sub principal)
                            :type t}})
          (throw (client-error info))))))

(defn wrap-handler
  "Wrap a registry function (`(f arg-map)` → channel) so that a failure, thrown
   or delivered on the channel, arrives as a client error."
  [fn-name f]
  (with-meta
    (fn [arg-map]
      (a/go
        (let [r (try (a/<! (f arg-map))
                     (catch Throwable e e))]
          (if (instance? Throwable r) (sanitize r fn-name) r))))
    {::wrapped true}))

(defn- wrap-registry [registry]
  (reduce-kv (fn [m fn-name f]
               (if (::wrapped (meta f)) m (assoc m fn-name (wrap-handler fn-name f))))
             registry registry))

(defn install!
  "Wrap every handler in distributed-scope's registry, now and whenever one is
   registered later. Idempotent."
  []
  (swap! ds/remote-fn-registry wrap-registry)
  (add-watch ds/remote-fn-registry ::sanitize
             (fn [_ registry _ new]
               ;; terminates: the second pass finds every entry wrapped
               (when (some #(not (::wrapped (meta %))) (vals new))
                 (swap! registry wrap-registry))))
  :installed)
