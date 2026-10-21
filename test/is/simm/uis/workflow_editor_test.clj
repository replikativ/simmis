(ns is.simm.uis.workflow-editor-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.workflow-editor :as editor]))

(def source {:name "echo" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d" :files {"workflow.edn" "old"}
             :definition {:params {:goal "old default"}}
             :inspection {:ok true :bundle "bundle" :cases []}})

(deftest recurring-deployments-retain-the-inspected-execution-intent
  (let [draft (-> (editor/draft source) (assoc-in [:form :model] "explicit-model")
                  (assoc-in [:form :schedule-minutes] "60")
                  (assoc-in [:form :schedule-tz] "Europe/Berlin"))
        request (editor/scheduled-submission draft "activation")]
    (is (= "activation" (:request-id request)))
    (is (= "workflow" (:kind request)))
    (is (= {:interval-ms 3600000 :tz "Europe/Berlin"} (:cadence request)))
    (is (= (dissoc (editor/submission draft "ignored") :request-id) (:execution request)))
    (is (not (contains? (:execution request) :request-id)))
    (is (= (:revision source) (get-in request [:execution :expected-owned-source])))
    (doseq [bad [(assoc-in draft [:form :backend] "local")
                 (assoc-in draft [:form :backend] "cli")
                 (assoc-in draft [:form :schedule-minutes] "0")
                 (assoc-in draft [:form :schedule-minutes] "1.5")
                 (assoc-in draft [:form :schedule-tz] "")
                 (assoc draft :files {})]]
      (is (thrown? clojure.lang.ExceptionInfo (editor/scheduled-submission bad "bad"))))))

(deftest cli-text-submission-and-allowance-never-claim-http-or-money-bounds
  (let [draft (-> (editor/draft source)
                  (update :form editor/set-form-field :backend "cli")
                  (assoc-in [:form :model] "claude-code-haiku"))
        request (editor/submission draft "request")]
    (is (= "benchmark" (get-in draft [:form :mode])))
    (is (= {:request-id "allowance" :max-cli-runs 2} (editor/allowance (:form draft) "allowance")))
    (is (= {:max-cli-runs 2 :max-model-steps 4 :timeout-ms 120000 :settlement "discard"} (:limits request)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (assoc-in draft [:form :mode] "run") "bad")))))

(deftest benchmark-split-selection-does-not-implicitly-include-held-out-cases
  (let [source (assoc source :inspection {:ok true :bundle "bundle" :cases ["a" "b"]
                                          :evaluation {:splits {:development ["a"] :held-out ["b"]}}})
        draft (-> (editor/draft source) (assoc-in [:form :mode] "benchmark")
                  (assoc-in [:form :model] "explicit-model"))]
    (is (= ["a"] (:cases (editor/submission draft "dev"))))
    (is (= "development" (:split (editor/submission draft "dev"))))
    (is (= ["b"] (:cases (editor/submission (assoc-in draft [:form :split] "held-out") "held"))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (assoc-in draft [:form :case] "b") "wrong")))
    (is (= "" (:case (editor/set-form-field (assoc (:form draft) :case "a") :split "held-out"))))))

(deftest backend-default-timeouts-never-replace-explicit-or-legacy-values
  (let [hosted editor/defaults
        local (editor/set-form-field hosted :backend "local")]
    (is (= "120000" (:timeout-ms hosted)))
    (is (= "300000" (:timeout-ms local)))
    (is (true? (:timeout-default? local)))
    (is (= "120000" (:timeout-ms (editor/set-form-field local :backend "hosted"))))
    (doseq [value ["120000" "5000" ""]]
      (let [edited (editor/set-form-field hosted :timeout-ms value)
            switched (editor/set-form-field edited :backend "local")]
        (is (false? (:timeout-default? edited)))
        (is (= value (:timeout-ms switched)))
        (is (= value (:timeout-ms (editor/set-form-field switched :backend "hosted"))))))
    (is (= "120000" (:timeout-ms (editor/set-form-field
                                  (dissoc hosted :timeout-default?) :backend "local"))))
    (is (= local (editor/set-form-field local :backend "local")))))

(deftest edits-are-an-exact-source-diff
  (let [draft (-> (editor/draft source) (assoc :files {"notes.md" "new"}))]
    (is (= {"workflow.edn" nil "notes.md" "new"} (editor/edits draft)))
    (is (editor/dirty? draft))))

(deftest reload-keeps-immutable-admission-and-funding-records
  (let [old (assoc (editor/draft source) :submission {:payload {:request-id "one"}}
                   :fund-request {:request-id "fund"})
        reloaded (editor/reloaded old (assoc source :revision "new"))]
    (is (= (:submission old) (:submission reloaded)))
    (is (= (:fund-request old) (:fund-request reloaded)))
    (is (= "new" (get-in reloaded [:source :revision])))))

(deftest saving-retains-newer-local-edits-and-only-explicit-parameter-overrides
  (let [draft (editor/draft source)
        reply {:workflow {:ok true :revision "new" :bundle "new-bundle"}
               :definition {:params {:goal "new default"}}}
        result (editor/saved (assoc draft :files {"workflow.edn" "typed while saving"})
                             {"workflow.edn" "sent"} reply)]
    (is (= "typed while saving" (get-in result [:files "workflow.edn"])))
    (is (= "sent" (get-in result [:source :files "workflow.edn"])))
    (is (= "new default" (get-in result [:form :params "goal"])))
    (is (= "explicit override"
           (get-in (editor/saved (assoc-in draft [:form :params "goal"] "explicit override")
                                 (:files draft) reply) [:form :params "goal"])))))

(deftest request-fences-are-token-and-selection-aware
  (let [s {:selected "b" :requests {[:save "a"] "save" [:source "a"] "read"}}]
    (is (editor/current-response? s [:save "a"] "save" nil))
    (is (not (editor/current-response? s [:source "a"] "read" "a")))
    (is (not (editor/current-response? s [:save "a"] "older" nil)))))

(deftest a-racing-source-read-never-upgrades-the-saved-revision
  (let [result (editor/saved (editor/draft source) (:files source)
                             {:source-changed? true :workflow {:ok true :revision "saved" :bundle "bundle"}})]
    (is (= "saved" (get-in result [:source :revision])))
    (is (false? (get-in result [:source :inspection :ok])))
    (is (not (editor/dirty? result)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (assoc-in result [:form :model] "explicit-model") "request")))))

(deftest launches-have-explicit-finite-limits-and-benchmark-defaults
  (let [draft (assoc-in (editor/draft source) [:form :model] "explicit-model")
        request (editor/submission draft "request")]
    (is (= "request" (:request-id request)))
    (is (= "bundle" (:expected-bundle request)))
    (is (= "review" (get-in request [:limits :settlement])))
    (is (thrown? clojure.lang.ExceptionInfo (editor/submission (assoc-in draft [:form :timeout-ms] "Infinity") "bad")))
    (is (thrown? clojure.lang.ExceptionInfo (editor/submission (assoc draft :files {}) "dirty")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (-> draft (assoc-in [:form :mode] "benchmark")
                                        (assoc-in [:form :params "goal"] "override")) "wrong")))
    (let [benchmark (editor/submission (assoc-in draft [:form :mode] "benchmark") "bench")]
      (is (= ["explicit-model"] (:models benchmark)))
      (is (= "discard" (get-in benchmark [:limits :settlement])))
      (is (not (contains? benchmark :params))))))

(deftest local-launches-have-only-enrollment-and-admission-bounds
  (let [worker "99999999-9999-4999-8999-999999999999"
        draft (-> (editor/draft source) (assoc-in [:form :backend] "local")
                  (assoc-in [:form :worker] worker))
        request (editor/submission draft "local")]
    (is (= worker (:worker request)))
    (is (= #{:request-id :name :expected-bundle :expected-owned-source :limits :params :worker} (set (keys request))))
    (is (= {:timeout-ms 120000 :settlement "review"} (:limits request)))
    (is (= {:request-id "fund" :max-worker-admissions 1} (editor/allowance (:form draft) "fund")))
    (doseq [bad [(assoc-in draft [:form :worker] "")
                 (assoc-in draft [:form :timeout-ms] "600001")
                 (assoc-in draft [:form :backend] "unknown")]]
      (is (thrown? clojure.lang.ExceptionInfo (editor/submission bad "bad"))))
    (let [benchmark (editor/submission (assoc-in draft [:form :mode] "benchmark") "bench")]
      (is (= #{:request-id :name :expected-bundle :expected-owned-source :limits :worker :repetitions} (set (keys benchmark))))
      (is (= "discard" (get-in benchmark [:limits :settlement]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/allowance (assoc (:form draft) :worker-admissions "33") "too-many")))))

(deftest all-execution-backends-retain-the-inspected-owned-source
  (doseq [backend ["hosted" "local" "cli"]]
    (let [draft (-> (editor/draft source)
                    (update :form editor/set-form-field :backend backend)
                    (assoc-in [:form :model] "explicit-model")
                    (assoc-in [:form :worker] "99999999-9999-4999-8999-999999999999"))]
      (is (= (:revision source) (:expected-owned-source (editor/submission draft "request"))))
      (doseq [revision [nil "source" ""]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (editor/submission (assoc-in draft [:source :revision] revision) "bad")))))))
