(ns is.simm.ops.worker-process
  "Minimal stdio bootstrap. Start only from a trusted packaged runtime without
   dev/user.clj. This entrypoint itself provides NO physical containment."
  (:import [java.io BufferedReader ByteArrayInputStream PrintWriter StringReader]))

(defn -main [& args]
  (let [in System/in
        out System/out
        err System/err]
    ;; Do this BEFORE loading Dvergr or any logger. This isolates accidental
    ;; output only: native tool children can still forge guest protocol frames.
    (System/setIn (ByteArrayInputStream. (byte-array 0)))
    (System/setOut err)
    ;; Raw threads do not inherit dynamic bindings. This process is dedicated
    ;; to the endpoint, so root stream bindings must also stop using the pipe.
    (alter-var-root #'*in* (constantly (BufferedReader. (StringReader. ""))))
    (alter-var-root #'*out* (constantly (PrintWriter. err true)))
    (alter-var-root #'*err* (constantly (PrintWriter. err true)))
    (binding [*in* (BufferedReader. (StringReader. "")) *out* (PrintWriter. err true) *err* (PrintWriter. err true)]
      (try
        (when-not (and (= 1 (count args)) (contains? #{"check" "guest" "broker"} (first args)))
          (throw (Exception.)))
        (require 'is.simm.ops.worker-frames)
        (let [ch ((requiring-resolve 'is.simm.ops.worker-frames/channel) in out)]
          (case (first args)
            "check"
            (do
              ;; Load the actual endpoint closure without constructing a
              ;; login-backed provider, room, scheduler or agent. A packaged
              ;; runtime must prove this before it is offered for enrollment.
              (require 'is.simm.ops.worker-guest 'is.simm.ops.worker-broker
                       'is.simm.ops.worker-subscription 'hasch.core)
              (let [identify (requiring-resolve 'hasch.core/uuid)
                    write! (requiring-resolve 'is.simm.ops.worker-frames/write!)
                    executor @(requiring-resolve 'is.simm.ops.worker-executor/policy)
                    inference @(requiring-resolve 'is.simm.ops.worker-inference/policy)]
                (write! ch {"version" 1 "kind" "runtime-ready"
                            "executor-policy-id" (str (identify executor))
                            "inference-policy-id" (str (identify inference))} 4096)))
            "guest" ((requiring-resolve 'is.simm.ops.worker-guest/serve!) ch)
            "broker"
            (let [_ (require 'is.simm.ops.worker-wire)
                  ;; This ONE opening frame comes from the trusted parent
                  ;; before it connects guest traffic. Guest input must never
                  ;; be allowed to supply this locally pinned configuration.
                  opening ((requiring-resolve 'is.simm.ops.worker-wire/object!)
                           ((requiring-resolve 'is.simm.ops.worker-frames/read!) ch 4096)
                           [:version :kind :config] [:version :kind :config])]
              (when-not (and (= 1 (:version opening)) (= "configure" (:kind opening)))
                (throw (Exception.)))
              (let [config ((requiring-resolve 'is.simm.ops.worker-wire/config!) (:config opening))]
                (require 'is.simm.ops.worker-subscription)
                (let [provider! (requiring-resolve 'is.simm.ops.worker-subscription/provider)
                      inference! (requiring-resolve 'is.simm.ops.worker-subscription/inference)
                      serve! (requiring-resolve 'is.simm.ops.worker-broker/serve!)]
                  (serve! ch config (inference! config (provider!))))))))
        (shutdown-agents)
        (catch Throwable failure
          ;; No raw provider exception, parser content or credential in stderr.
          (.write ^PrintWriter *err* "Local worker process failed\n")
          (when (= ["check"] (vec args))
            ;; Check mode has no task input or provider. Class names alone
            ;; distinguish unsupported JVMs/missing runtime dependencies; no
            ;; exception messages, input, paths or credentials are emitted.
            (doseq [cause (take 8 (take-while some? (iterate ex-cause failure)))]
              (.write ^PrintWriter *err* (str (.getName (class cause)) "\n"))))
          (.flush ^PrintWriter *err*)
          (System/exit 1))))))
