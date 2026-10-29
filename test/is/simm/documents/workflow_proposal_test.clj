(ns is.simm.documents.workflow-proposal-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.workflow-proposal :as proposal]
            [is.simm.ops.workflows :as workflows]))

(deftest saved-intent-is-one-closed-inert-bounded-form
  (is (nil? (proposal/read-intent nil)))
  (is (= {:version 1 :action :run} (proposal/read-intent "; comment\n{:version 1 :action :run}")))
  (is (= {:version 1 :action :benchmark :models ["luna" "sol"] :repetitions 2}
         (proposal/read-intent "{:version 1 :action :benchmark :models [\"luna\" \"sol\"] :repetitions 2}")))
  (doseq [text ["{:version 1 :action :run} {}"
               "{:version 1 :action :run} :is.simm.documents.workflow-proposal/eof {}" "#=(System/exit 0)"
               "{:version 1 :action :run :room \"other\"}"
               "{:version 1 :action :schedule}" "{:version 2 :action :run}"
               "{:version 1 :action :run :limits {:grant 100}}"
               "{:version 1 :action :run :limits {:timeout-ms ##Inf}}"
               "{:version 1 :action :run :models [\"sol\"]}"
               "{:version 1 :action :benchmark :models [\"sol\" \"sol\"]}"
               "{:version 1 :action :benchmark :model \"sol\" :models [\"luna\"]}"
               (apply str (repeat 4097 " "))]]
    (is (thrown? Exception (proposal/read-intent text))))
  (let [valid {"workflow.edn" "{:title \"Test\" :task \"Write /out/result.md.\" :capture [\"/out\"]}"
               "proposal.edn" "{:version 1 :action :schedule :cadence {:every \"week\" :on \"monday\" :at \"09:00\" :tz \"UTC\"}}"}
        source (workflows/details-of "test" {:files valid :manifest []})]
    (is (= :ready (get-in source [:setup :status])))
    (is (= :schedule (get-in source [:setup :intent :action])))
    (is (true? (get-in source [:inspection :ok])))
    (is (= :invalid (get-in (workflows/details-of "test" {:files (assoc valid "proposal.edn" "{bad") :manifest []}) [:setup :status])))))
