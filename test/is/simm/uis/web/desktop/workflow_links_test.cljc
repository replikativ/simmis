(ns is.simm.uis.web.desktop.workflow-links-test
  (:require #?(:clj [clojure.test :refer [deftest is]] :cljs [cljs.test :refer-macros [deftest is]])
            [is.simm.documents.workflow-links :as links]
            [is.simm.uis.web.desktop.routes :as routes]))

(def room "b985a235-9699-4389-b3f3-b05d1b5e2c36")

(deftest workflow-navigation-roundtrips-with-room-and-action
  (doseq [action [:run :schedule :benchmark]]
    (let [ref {:kind :workflow :room room :workflow "competitor-analysis" :action action}]
      (is (= ref (routes/route->ref (routes/ref->route ref))))))
  (doseq [path [(str "/room/" room "/workflows/demo/delete")
                (str "/room/" room "/workflows/demo/run?enabled=true")
                "/room/other/workflows/demo/run"]]
    (is (nil? (routes/route->ref path)))))

(deftest chat-file-links-have-an-explicit-room-and-preserve-external-destinations
  (is (= (str "/room/" room "/files?path=workflows%2Fdemo%2Fworkflow.edn")
         (links/file-route room "workflows/demo/workflow.edn")))
  (is (= (links/file-route room "wiki/My%20note.md")
         (links/file-route room "/wiki/My%20note.md")))
  (doseq [href ["https://example.com/x.md" "//example.com/x.md" "javascript:alert(1)"
                "../other.md" "%2e%2e/other.md" "wiki/%2Fbad.md" "#footnote" "/auth/logout"
                (str "/room/" room "/files?path=x.md")]]
    (is (nil? (links/file-route room href)) href))
  (is (nil? (links/file-route "unknown" "wiki/test.md"))))

(deftest workflow-cards-discover-links-without-treating-prose-as-an-execution
  (is (= ["demo"] (links/mentioned-workflows "[source](workflows/demo/workflow.edn) [more](/workflows/demo/instructions.md)")))
  (is (empty? (links/mentioned-workflows "Please schedule demo every Monday"))))
