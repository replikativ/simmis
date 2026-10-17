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
        (when-not (and (= 1 (count args)) (contains? #{"guest" "broker"} (first args)))
          (throw (Exception.)))
        (require 'is.simm.ops.worker-frames)
        (let [ch ((requiring-resolve 'is.simm.ops.worker-frames/channel) in out)]
          (case (first args)
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
        (catch Throwable _
          ;; No raw provider exception, parser content or credential in stderr.
          (.write ^PrintWriter *err* "Local worker process failed\n")
          (.flush ^PrintWriter *err*)
          (System/exit 1))))))
