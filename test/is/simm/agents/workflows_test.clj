(ns is.simm.agents.workflows-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.agents.workflows :as workflows]))

(deftest supplied-file-checks-are-inert-and-do-not-certify-saved-source
  (let [files {"workflow.edn" "{:title \"Task\" :task \"Write /out/result.md\"}"
               "checker.clj" "(throw (ex-info \"This must not run\" {}))"
               "fixtures/input.md" "evidence"}
        result (workflows/inspect-files files)]
    (is (:ok result))
    (is (:benchmark-ready? result))
    (is (false? (:checker-executed? result)))
    (is (false? (:saved-source-verified? result)))
    (is (= :supplied-files (:observation result))))
  (doseq [files [{"../outside.md" "bad"} {"workflow.edn" 42} {}]]
    (is (thrown? Exception (workflows/inspect-files files)))))

(deftest action-links-only-name-the-bound-room-and-human-setup
  (let [room "b985a235-9699-4389-b3f3-b05d1b5e2c36"
        result (workflows/action-links room "competitor-analysis")]
    (is (= (str "/room/" room "/workflows/competitor-analysis/run") (:run result)))
    (is (= :canonical-saved-source (:scope result)))
    (is (not (contains? result :request-id))))
  (is (thrown? Exception (workflows/action-links "room" "../other"))))
