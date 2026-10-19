(ns is.simm.ops.workflow-policy
  "Provider-independent saved-workflow admission policy shared by collectors
   and the ordinary execution path. This data is part of execution identity.")

(def policy {:version 1 :max-params 32 :max-param-bytes 65536
             :max-task-bytes 16000 :max-input-files 100 :max-input-bytes 2097152
             :source :pinned-bundle :checker :not-executed :parent :room-fork})
