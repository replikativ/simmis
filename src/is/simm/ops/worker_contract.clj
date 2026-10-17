(ns is.simm.ops.worker-contract
  "Portable worker configuration and file-policy data. Loading the broker's
   wire contract must not initialize a room, database or workspace.")

(def policy {:version 2 :max-files 100 :max-file-bytes 1048576 :max-total-bytes 2097152
             :max-input-json-bytes 4194304 :chunk-bytes 16384 :lease-ttl-ms 30000})

(def config-schema
  [:map {:closed true}
   [:adapter [:enum "codex" "claude-code"]]
   [:version [:string {:min 1 :max 200}]]
   [:model [:string {:min 1 :max 200}]]
   [:policy [:= "local-trusted-v1"]]])
