(ns is.simm.ops.knowledge-batch-resolution
  "Fixed HOST retained proof shared by file decisions and observed adoption.
   Callers establish their own authority or original publisher ACK obligation.
   No target lookup, native receipt observation, transaction or dispatch."
  (:require [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-processing :as processing]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn retained!
  "Authorize before calling. A batch read retains the original actual-thread
   App operation through native IO. Observed publication cleanup may finish
   after that owner was fenced; a fresh decision must separately check ready."
  [conn value]
  (when (:batch value)
    (let [ticket lifetime/*ticket*]
      (when-not (and (not (simple/on-drain-thread?))
                     (identical? (Thread/currentThread) (:thread ticket))
                     (lifetime/retained? ticket))
        (throw (ex-info "Knowledge resolution has no original App owner" {:type ::held})))
      (let [row (requests/get-request conn (:request value))]
        (when-not (= value (dispositions/disposition row))
          (throw (ex-info "Knowledge resolution filing changed" {:type ::held})))
        {:expected (processing/expected (proof/request! conn row)) :value value}))))
