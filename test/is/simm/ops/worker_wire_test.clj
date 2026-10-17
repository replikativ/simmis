(ns is.simm.ops.worker-wire-test
  (:require [clojure.test :refer [deftest is]]
            [hasch.core :as hasch]
            [is.simm.ops.worker-contract :as contract]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-wire :as wire]
            [jsonista.core :as json]))

(def config {:adapter "codex" :version "fixture-1" :model "fixture-model"
             :policy "local-trusted-v1"})
(def limits {:timeout-ms 60000 :settlement "discard"})

(defn- transport [value]
  (json/read-value (json/write-value-as-bytes (wire/encode value))))

(deftest portable-policy-and-configuration-have-one-canonical-definition
  (is (identical? contract/policy files/policy))
  (is (identical? contract/config-schema files/config-schema))
  (is (= {:version 2 :max-files 100 :max-file-bytes 1048576 :max-total-bytes 2097152
          :max-input-json-bytes 4194304 :chunk-bytes 16384 :lease-ttl-ms 30000}
         contract/policy))
  (is (= config (wire/config! (transport config)))))

(deftest guest-wire-still-validates-the-exact-canonical-frozen-input
  (let [frozen (files/freeze-files "task" config {"docs/资料.md" (files/text-file "selected λ" 0x81A4)} limits)]
    (is (= frozen (wire/frozen! (transport frozen))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (wire/frozen! (transport (assoc frozen :input-id (random-uuid))))))
    (doseq [bad [(assoc-in frozen [:files "docs/资料.md" :sha256] (apply str (repeat 64 "0")))
                 (assoc frozen :files {".git/config" (files/text-file "forbidden" 0x81A4)})
                 (assoc frozen :files {"../escape" (files/text-file "forbidden" 0x81A4)})
                 (assoc-in frozen [:config :url] "https://outside.invalid")]]
      ;; A valid content identity must not bypass path/configuration/digest
      ;; policy. Exercise those checks independently of identity rejection.
      (let [bad (assoc bad :input-id (hasch/uuid [:simmis/worker-input-v1 (dissoc bad :input-id)]))]
        (is (thrown? clojure.lang.ExceptionInfo (wire/frozen! (transport bad))))))))
