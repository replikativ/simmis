(ns is.simm.documents.starter-bundle-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [is.simm.documents.starter-bundle :as bundle])
  (:import [java.io ByteArrayInputStream]))

(deftest packaged-artifact-is-pinned-inert-and-complete
  (let [packaged (bundle/read-packaged!)]
    (is (= bundle/packaged-digest (:digest packaged)))
    (is (= 32 (count (:files packaged))))
    (is (every? #(contains? (:files packaged) %)
                ["LICENSE" "AGENTS.md" "README.md" "user.clj" "dvergr/intake/core.clj"
                 "dvergr/intake/schema.clj" "dvergr/mail/inbox.clj"]))
    (is (not-any? #(re-find #"(^test/|\.class$|\.git)" %) (keys (:files packaged))))
    (is (every? string? (map :text (vals (:files packaged)))))))

(deftest data-integrity-path-mode-and-encoding-fail-closed
  (let [valid (bundle/assemble {"user.clj" "(ns user)\n"})]
    (doseq [invalid [(assoc valid :extra true)
                     (assoc valid :commit "main")
                     (assoc valid :digest "wrong")
                     (assoc-in valid [:files "user.clj" :text] "changed")
                     (assoc-in valid [:files "user.clj" :mode] 33261)
                     (assoc-in valid [:files "user.clj" :size] -1)]]
      (is (thrown? clojure.lang.ExceptionInfo (bundle/verify! invalid))))
    (doseq [path ["../user.clj" "/user.clj" "test/datahike/api.clj" "dvergr/../user.clj"
                  "dvergr/.hidden.clj" "dvergr\\unsafe.clj" "user__init.class"]]
      (is (thrown? clojure.lang.ExceptionInfo (bundle/assemble {path "not executable"}))))
    (is (thrown? java.nio.charset.CharacterCodingException
                 (bundle/assemble {"user.clj" (String. (char-array [(char 0xD800)]))})))))

(deftest adaptation-preserves-code-and-license-and-scopes-markdown-ids-to-the-repository
  (let [packaged (bundle/read-packaged!) repository (random-uuid)
        first (bundle/adapt packaged repository)
        repeated (bundle/adapt packaged repository)
        other (bundle/adapt packaged (random-uuid))]
    (is (= first repeated))
    (is (not= (get-in first [:files "README.md"]) (get-in other [:files "README.md"])))
    (doseq [path ["user.clj" "LICENSE" "dvergr/intake/core.clj" "dvergr/mail/inbox.clj"]]
      (is (= (get-in packaged [:files path :text]) (get-in first [:files path])))
      (is (false? (get-in first [:provenance :files path :modified?]))))
    (is (true? (get-in first [:provenance :files "AGENTS.md" :modified?])))
    (is (.contains (get-in first [:files "AGENTS.md"]) "not auto-loaded or executed by this host"))
    (is (thrown? clojure.lang.ExceptionInfo
                 (bundle/adapt (bundle/assemble {"README.md" "---\nold: metadata\n---\n# Existing\n"}) repository)))))

(deftest serialized-loader-refuses-unreviewed-tagged-and-trailing-data
  (let [packaged (bundle/read-packaged!)]
    (doseq [text [(str (pr-str packaged) "\nnil") "#unexpected/tag 42"]]
      (with-redefs [io/resource (constantly ::fixture)
                    io/input-stream (fn [_] (ByteArrayInputStream. (bundle/utf8-bytes text)))]
        (is (thrown? clojure.lang.ExceptionInfo (bundle/read-packaged!)))))
    (with-redefs [bundle/packaged-digest "unreviewed"]
      (is (thrown? clojure.lang.ExceptionInfo (bundle/read-packaged!))))))
