(ns is.simm.uis.activity-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.activity :as activity]))

(deftest semantic-activity-label-is-compact
  (testing "lifecycle facts keep verb and status visible"
    (is (= "exhaust · blocked"
           (activity/label {:activity/kind :budget
                            :activity/verb :exhaust
                            :activity/status :blocked}))))
  (testing "tool facts keep their exact tool name"
    (is (= "invoke · clojure_eval"
           (activity/label {:activity/kind :tool
                            :activity/verb :invoke
                            :activity/tool-name "clojure_eval"})))))
