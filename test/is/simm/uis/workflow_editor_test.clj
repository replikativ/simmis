(ns is.simm.uis.workflow-editor-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.workflow-editor :as editor]))

(def source {:name "echo" :revision "source" :files {"workflow.edn" "old"}
             :definition {:params {:goal "old default"}}
             :inspection {:ok true :bundle "bundle" :cases []}})

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
    (is (= #{:request-id :name :expected-bundle :limits :params :worker} (set (keys request))))
    (is (= {:timeout-ms 120000 :settlement "review"} (:limits request)))
    (is (= {:request-id "fund" :max-worker-admissions 1} (editor/allowance (:form draft) "fund")))
    (doseq [bad [(assoc-in draft [:form :worker] "")
                 (assoc-in draft [:form :timeout-ms] "600001")
                 (assoc-in draft [:form :backend] "unknown")]]
      (is (thrown? clojure.lang.ExceptionInfo (editor/submission bad "bad"))))
    (let [benchmark (editor/submission (assoc-in draft [:form :mode] "benchmark") "bench")]
      (is (= #{:request-id :name :expected-bundle :limits :worker :repetitions} (set (keys benchmark))))
      (is (= "discard" (get-in benchmark [:limits :settlement]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/allowance (assoc (:form draft) :worker-admissions "33") "too-many")))))
