(ns is.simm.ops.worker-process-io-fixture
  "Fresh-process bootstrap instrumentation only; never include in worker jar."
  (:require [is.simm.ops.worker-process :as process]
            [is.simm.ops.worker-frames :as frames]))

(defn -main [& _]
  (let [resolve-original requiring-resolve]
    (with-redefs [clojure.core/requiring-resolve
                  (fn [sym]
                    (if (= 'is.simm.ops.worker-guest/serve! sym)
                      (fn [ch]
                        ;; Exercise both dynamic bindings and roots in a raw
                        ;; thread. Neither is allowed to use the captured pipe.
                        (.write System/out (.getBytes "java-output\n" "UTF-8"))
                        (.flush System/out)
                        (.write *out* "clojure-output\n")
                        (.flush *out*)
                        (let [root-read (promise)
                              thread (Thread. ^Runnable
                                      (fn []
                                        (.write *out* "thread-output\n")
                                        (.flush *out*)
                                        (deliver root-read (read-line))))]
                          (.start thread)
                          (.join thread)
                          (frames/write! ch {"dynamic-eof" (nil? (read-line))
                                             "root-eof" (nil? @root-read)
                                             "java-eof" (= -1 (.read System/in))
                                             "original" (frames/read! ch 4096)} 4096)))
                      (resolve-original sym)))]
      (process/-main "guest"))))
