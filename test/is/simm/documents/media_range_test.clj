(ns is.simm.documents.media-range-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.media-range :as range]))

(deftest exact-open-ended-suffix-and-clamped-ranges
  (doseq [[header offset length] [["bytes=0-0" 0 1] ["bytes=2-5" 2 4]
                                   ["bytes=7-" 7 3] ["bytes=-3" 7 3]
                                   ["bytes=-999999999999999999999999" 0 10]
                                   ["bytes=2-999999999999999999999999" 2 8]]]
    (is (= {:status 206 :offset offset :length length
            :content-range (str "bytes " offset "-" (+ offset length -1) "/10")}
           (range/plan 10 :get header nil "\"content\"")))))

(deftest invalid-and-unsatisfiable-ranges-never-plan-a-body
  (doseq [header ["bytes=-0" "bytes=-" "bytes=10-" "bytes=6-2" "bytes=one-two"
                  "bytes=999999999999999999999999-" (str "bytes=" (apply str (repeat 300 "9")) "-")]]
    (is (= {:status 416 :offset 0 :length 0 :content-range "bytes */10"}
           (range/plan 10 :get header nil "\"content\""))))
  (is (= 416 (:status (range/plan 0 :get "bytes=0-" nil "\"empty\"")))))

(deftest head-multipart-and-if-range-are-explicit
  (let [full {:status 200 :offset 0 :length 10} tag "\"content\""]
    (is (= full (range/plan 10 :get nil nil tag)))
    (is (= full (range/plan 10 :head "bytes=6-2" nil tag)))
    (is (= full (range/plan 10 :get "bytes=0-1,5-6" nil tag)))
    (is (= full (range/plan 10 :get "items=0-1" nil tag)))
    (is (= full (range/plan 10 :get "bytes=0-1" "\"changed\"" tag)))
    (is (= full (range/plan 10 :get "bytes=0-1" "W/\"content\"" tag)))
    (is (= 206 (:status (range/plan 10 :get "bytes=0-1" tag tag))))))
