(ns is.simm.uis.download-link-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.uis.web.desktop.download-link]))

(deftest forked-devices-never-share-download-object-urls
  (let [one (.createElement js/document "simmis-download")
        two (.createElement js/document "simmis-download")]
    (try
      (doseq [el [one two]]
        (.setAttribute el "data-name" "test.txt")
        (.setAttribute el "data-bytes" (js/btoa "contents"))
        (.appendChild (.-body js/document) el))
      (is (not= (.-simmisDownloadUrl one) (.-simmisDownloadUrl two)))
      (is (= "test.txt" (.-download (.querySelector one "a"))))
      (.remove one)
      (is (nil? (.-simmisDownloadUrl one)))
      (is (some? (.-simmisDownloadUrl two)))
      (is (zero? (.-childElementCount one)))
      (finally (.remove one) (.remove two)))))
