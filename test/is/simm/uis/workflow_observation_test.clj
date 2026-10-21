(ns is.simm.uis.workflow-observation-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.workflow-editor :as editor]))

(def before {:name "example" :revision "source-before"
             :definition {:params {:subject "old default" :unchanged "old"}}
             :files {"workflow.edn" "before" "notes.md" "notes"}})
(def after {:name "example" :revision "source-after"
            :definition {:params {:subject "new default" :unchanged "new" :added "added"}}
            :files {"workflow.edn" "after" "notes.md" "new notes"}})

(deftest clean-observations-preserve-form-path-and-original-request-ledgers
  (let [old (-> (editor/draft before)
                (assoc :path "notes.md" :submission {:payload {:request-id "original"}}
                       :schedule-submission {:payload {:request-id "original-schedule"}}
                       :fund-request {:request-id "original-funds"})
                (assoc-in [:form :model] "explicit-model")
                (assoc-in [:form :params "subject"] "my literal override"))
        result (editor/observed (editor/unverified old) after)]
    (is (= (:files after) (:files result)))
    (is (= after (:source result)))
    (is (= "notes.md" (:path result)))
    (is (= "explicit-model" (get-in result [:form :model])))
    (is (= {"subject" "my literal override" "unchanged" "new" "added" "added"}
           (get-in result [:form :params])))
    (is (= (select-keys old [:submission :schedule-submission :fund-request])
           (select-keys result [:submission :schedule-submission :fund-request])))
    (is (editor/source-current? result))))

(deftest dirty-source-never-silently-advances-its-base-or-discards-local-text
  (let [old (-> (editor/draft before)
                (assoc-in [:files "notes.md"] "my unsaved notes")
                (assoc :path "notes.md" :selection {:anchor 4 :head 9}
                       :submission {:payload {:request-id "original"}}))
        result (editor/observed old after)]
    (is (= old (dissoc result :source-stale? :incoming-source)))
    (is (= after (:incoming-source result)))
    (is (= before (:source result)))
    (is (not (editor/source-current? result)))
    (is (thrown? clojure.lang.ExceptionInfo (editor/submission result "new-request")))
    (is (= old (dissoc (editor/observed result before) :source-stale? :incoming-source)))))

(deftest unverified-source-cannot-start-fresh-work-even-if-its-buffer-is-clean
  (let [old (editor/draft before)]
    (is (not (editor/source-current? (editor/unverified old))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (editor/unverified old) "new-request")))
    (is (= old (editor/observed (editor/unverified old) before)))))

(deftest deleted-selected-files-select-a-surviving-file-and-removed-params-do-not-leak
  (let [old (assoc (editor/draft before) :path "notes.md")
        incoming (assoc after :files {"workflow.edn" "after"} :definition {:params {:added "new"}})
        result (editor/observed old incoming)]
    (is (= "workflow.edn" (:path result)))
    (is (= {"added" "new"} (get-in result [:form :params])))
    (is (= (editor/draft before) (editor/observed nil before)))))

(deftest sealed-save-acknowledgement-never-rolls-back-a-newer-observed-base
  (let [old (editor/draft after)
        reply {:workflow {:revision "ack"} :definition {}}]
    (is (= old (editor/acknowledged-save old "source-before" {"workflow.edn" "sent"} reply)))
    (is (= "ack" (get-in (editor/acknowledged-save (editor/draft before) "source-before"
                                                   {"workflow.edn" "sent"} reply) [:source :revision])))
    ;; A no-op ACK may have the original revision; do not build a duplicate
    ;; evaluated set literal or treat it as a second source transition.
    (is (= "source-before"
           (get-in (editor/acknowledged-save (editor/draft before) "source-before" (:files before)
                                             {:workflow {:revision "source-before"} :definition {}})
                   [:source :revision])))))
