(ns is.simm.agents.workflows-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [dvergr.sandbox.ns.dev :as dev]
            [is.simm.agents.workflows :as workflows]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.rooms :as rooms]
            [is.simm.ops.checkers :as checkers]
            [dvergr.catalog.room :as catalog]
            [dvergr.sandbox :as sandbox]
            [sci.core :as sci]))

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

(deftest a-validator-sentence-reaches-the-author-instead-of-a-generic-refusal
  ;; A Luna author got only "invalid definition, fixture or evaluation
  ;; metadata" for a split naming cases it had not written yet, and stopped.
  ;; Fixed validator sentences are diagnostics; other exception text is not.
  (let [files (assoc (:files (workflows/starter "evidence-check"))
                     "benchmark.edn" "{:version 1 :splits {:development [\"supported\" \"refuted\" \"unknown\" \"not-written-yet\"] :held-out [\"held-out\"]}}\n")
        result (workflows/inspect-files files)
        message "Benchmark splits must partition all named cases without duplicates"]
    (is (true? (:ok result)) "An incomplete evaluation pack does not invalidate the program")
    (is (false? (:benchmark-ready? result)))
    (is (= [message] (:benchmark-problems result)))
    (is (not (contains? result :validator-problems))))
  (let [files (assoc (:files (workflows/starter "evidence-check")) "benchmark.edn" "{:version 1 :splits #unknown/tag []}")]
    (is (some #(str/includes? % "benchmark") (:benchmark-problems (workflows/inspect-files files))))))

(deftest action-links-only-name-the-bound-room-and-human-setup
  (let [room "b985a235-9699-4389-b3f3-b05d1b5e2c36"
        result (workflows/action-links room "competitor-analysis")]
    (is (= (str "/room/" room "/workflows/competitor-analysis/run") (:run result)))
    (is (= :canonical-saved-source (:scope result)))
    (is (not (contains? result :request-id))))
  (is (thrown? Exception (workflows/action-links "room" "../other"))))

(deftest evidence-starter-is-an-ordinary-program-with-a-real-calibrated-pack
  (let [files (:files (workflows/starter "evidence-check"))
        result (workflows/inspect-files files)
        bundle (catalog/bundle "evidence-check" files)
        calibration (checkers/calibrate bundle)]
    (is (:ok result))
    (is (:benchmark-ready? result))
    (is (false? (:checker-executed? result)))
    (is (:ok? calibration))
    (is (= #{"supported" "refuted" "unknown" "held-out"} (set (keys (:cases bundle)))))
    (doseq [[_ {:keys [gold]}] (:cases bundle)]
      (let [answer (pr-str gold)
            verdict (checkers/run-checker (:checker bundle) {:files {"/out/answer.edn" answer} :gold gold})]
        (is (every? true? (vals (:checks verdict))))
        (is (= 1.0 (:reward verdict)))))
    (doseq [bad [nil "42" "{:decision :supported :evidence \"not a vector\"}" "{:decision :unlisted :evidence []}"
                "{:decision :supported :evidence [\"The delivery is scheduled for Monday.\"]} nil"
                "{:decision :supported :evidence [\"The delivery is scheduled for Monday.\"]} {:another :form}"
                "{:decision :supported :evidence [\"The delivery is scheduled for Monday.\"]} garbage"
                "" "{:decision"]]
      (let [verdict (checkers/run-checker (:checker bundle)
                                        {:files {"/out/answer.edn" bad}
                                         :gold (get-in bundle [:cases "supported" :gold])})]
        (is (false? (get-in verdict [:checks :output-contract?])))
        (is (zero? (:reward verdict)))))
    (let [gold (get-in bundle [:cases "supported" :gold])
          verdict (checkers/run-checker (:checker bundle)
                                       {:files {"/out/answer.edn" (str (pr-str gold) "\n ; trailing comment\n")}
                                        :gold gold})]
      (is (= 1.0 (:reward verdict))))
    ;; Removing the optional evaluation pack leaves the same executable task.
    (is (empty? (catalog/check-files (select-keys files ["workflow.edn" "diagram.mmd"]) :program)))))

(deftest installed-workflow-api-is-discoverable-inside-sci
  (let [ctx (sandbox/create-base-ctx)]
    (with-redefs [rooms/get-room (constantly {:room/slug "documentation-only"})]
      (@#'room-agents/add-workflow-ns! ctx #uuid "b985a235-9699-4389-b3f3-b05d1b5e2c36"))
    (dev/add-clojure-repl-ns! ctx)
    (doseq [[sym [arglists _ schema]] workflows/docs]
      (let [metadata (sci/eval-string* ctx (str "(meta #'workflow/" sym ")"))]
        (is (= arglists (:arglists metadata)))
        (is (seq (:doc metadata)))
        (is (= schema (:malli/schema metadata)))))
    (let [docs (sci/eval-string* ctx "(clojure.repl/doc workflow/starter)")]
      (is (str/includes? docs "[name]"))
      (is (str/includes? docs "evidence-check"))
      (is (not (str/includes? docs "arity unknown"))))
    (is (= "evidence-check" (sci/eval-string* ctx "(:name (workflow/starter \"evidence-check\"))")))
    (is (= '([schedule-id mermaid])
           (sci/eval-string* ctx "(:arglists (meta #'workflow/set-shape!))")))))
