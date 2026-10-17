(ns is.simm.ops.worker-files-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.rooms.files :as room-files]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.workflows-test :as fixture]
            [jsonista.core :as json])
  (:import [java.util Base64]))

(def config {:adapter "codex" :version "fixture-1" :model "fixture-model" :policy "local-trusted-v1"})
(def limits {:timeout-ms 60000 :settlement "review"})
(defn encoded [text]
  (let [bytes (.getBytes ^String text "UTF-8")]
    {:bytes (alength bytes) :sha256 (files/sha256 bytes) :mode 0x81A4
     :base64 (.encodeToString (Base64/getEncoder) bytes)}))

(deftest transport-filenames-preserve-namespaces-and-refuse-duplicate-normalization
  (let [output {:files {:out/result.md (encoded "answer")} :summary "done" :exit-code 0}]
    (is (= {"out/result.md" (encoded "answer")} (:files (files/transport-output output))))
    (let [path "out/资料/输入.md"
          normalized (files/transport-output (assoc output :files {(keyword path) (encoded "Unicode")}))]
      (is (= #{path} (set (keys (:files normalized)))))
      (is (= normalized (files/validate-output! {:files {}} normalized))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (files/transport-output (assoc-in output [:files "out/result.md"] (encoded "other")))))))

(deftest explicit-inputs-are-frozen-and-chunks-never-read-current-workspace
  (@#'fixture/with-workspace
   (fn [room]
     (workspace/seed! room {"/docs/readme.md" "before λ" "/private.txt" "must not be sent"})
     (let [frozen (files/freeze room "Edit the selected file" config ["docs/readme.md"] limits)
           chunk (files/input-chunk frozen 0 16384)
           json (.decode (Base64/getDecoder) ^String (:base64 chunk))
           decoded (json/read-value json json/keyword-keys-object-mapper)]
       (is (= #{"docs/readme.md"} (set (keys (:files frozen)))))
       (is (= (str (:input-id frozen)) (:input-id decoded)))
       (is (= (:bytes chunk) (alength json)))
       (is (= (:sha256 chunk) (files/sha256 json)))
       (room-files/write! room {"/docs/readme.md" {:text "after"}} {})
       (is (= chunk (files/input-chunk frozen 0 16384)))
       (is (not= (:input-id frozen) (:input-id (files/freeze room "Edit the selected file" config ["docs/readme.md"] limits))))))))

(deftest selected-paths-configuration-and-missing-inputs-are-refused
  (@#'fixture/with-workspace
   (fn [room]
     (workspace/seed! room {"/docs/readme.md" "input"})
     (doseq [paths [["/"] ["../outside"] ["/docs/readme.md"] ["docs//readme.md"]
                    [".git/config"] ["docs/readme.md" "docs/readme.md"] ["missing"]]]
       (is (thrown? clojure.lang.ExceptionInfo (files/freeze room "task" config paths limits))))
     (is (thrown? clojure.lang.ExceptionInfo (files/freeze room "task" (assoc config :api-key "no") [] limits)))
     (is (thrown? clojure.lang.ExceptionInfo (files/freeze room "task" config [] (assoc limits :eval true)))))))

(deftest output-is-bounded-and-confined-to-selected-files-and-deliverables
  (let [frozen {:files {"docs/readme.md" (encoded "before")}}
        output {:files {"docs/readme.md" (encoded "after") "out/report.md" (encoded "result")}
                :summary "done" :exit-code 0}]
    (is (= output (files/validate-output! frozen output)))
    (is (= {:files {"docs/readme.md" nil} :summary "deleted selected input" :exit-code 0}
           (files/validate-output! frozen {:files {"docs/readme.md" nil} :summary "deleted selected input" :exit-code 0})))
    (doseq [bad [(assoc output :eval true)
                 (assoc output :files {"private.txt" (encoded "outside input")})
                 (assoc output :files {"out/../private.txt" (encoded "outside")})
                 (assoc output :files {"out/unknown.md" nil})
                 (assoc-in output [:files "out/report.md" :mode] 0xA1FF)
                 (assoc-in output [:files "out/report.md" :bytes] 99)
                 (assoc-in output [:files "out/report.md" :sha256] (apply str (repeat 64 "0")))
                 (assoc-in output [:files "out/report.md" :base64] "bad!")
                 (assoc output :files {"out/a" (encoded "a") "out/a/b" (encoded "b")})]]
      (is (thrown? clojure.lang.ExceptionInfo (files/validate-output! frozen bad))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (files/validate-output! {:files {"out" (encoded "file")}}
                                         {:files {"out/result" (encoded "result")} :summary "bad shape" :exit-code 0})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (files/validate-output! frozen (assoc output :files
                                                       (into {} (for [i (range 3)] [(str "out/" i) (encoded (apply str (repeat 1048576 "x")))]))))))))
