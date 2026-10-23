(ns is.simm.documents.chart-spec-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.chart-spec :as spec]
            [jsonista.core :as json]))

(defn- encode [kind series]
  (json/write-value-as-string {"version" 1 "kind" kind "series" series}))

(defn- row [points] {"label" "Sales" "points" points})
(def valid (encode "line" [(row [[0 1] [2 -3]])]))

(defn- refused? [input]
  (try (spec/decode input) false
       (catch clojure.lang.ExceptionInfo e (= ::spec/refused (:type (ex-data e))))))

(deftest closed-data-model-and-preserved-point-order
  (is (= {:profile spec/profile :kind :line
          :series [{:label "Sales" :points [[0.0 1.0] [2.0 -3.0]]}]}
         (spec/decode valid)))
  (is (= (spec/decode valid) (spec/decode (.getBytes valid java.nio.charset.StandardCharsets/UTF_8))))
  (is (= [[2.0 4.0] [1.0 5.0]]
         (get-in (spec/decode (encode "point" [(row [[2 4] [1 5]])])) [:series 0 :points])))
  (is (= [["April" 1.0] ["March" -2.0]]
         (get-in (spec/decode (encode "bar" [(row [["April" 1] ["March" -2]])])) [:series 0 :points]))))

(deftest strict-json-does-not-evaluate-or-fetch
  (doseq [input ["null" "[]" "{}" (str valid " {}") (str valid " false")
                 "{\"version\":1,\"version\":1,\"kind\":\"line\",\"series\":[]}"
                 "{\"version\":1,\"kind\":\"line\",\"series\":[{\"label\":\"a\",\"label\":\"b\",\"points\":[[0,1]]}]}"
                 "#=(System/exit 0)" "http://localhost/private" "{unquoted: 1}"
                 "{\"version\":1,/*comment*/\"kind\":\"line\",\"series\":[]}"
                 (java.io.File. "/tmp/not-a-chart") (java.net.URI. "https://example.com") nil]]
    (is (refused? input) (str (type input) " " input)))
  (doseq [[key value] [["url" "https://example.com"] ["transform" []] ["expression" "(+ 1 2)"]
                       ["html" "<script>bad()</script>"] ["width" 10000] ["version" 1.0]
                       ["kind" "vega"] ["title" ""] ["title" "a\nb"]]]
    (is (refused? (json/write-value-as-string (assoc (json/read-value valid) key value))))))

(deftest bounded-input-and-parser-allocation
  (is (refused? (apply str (repeat (inc spec/max-bytes) " "))))
  (is (refused? (str valid (apply str (repeat 22000 "😀")))))
  (is (refused? (str (apply str (repeat 10 "[")) "0" (apply str (repeat 10 "]")))))
  (is (refused? (str "{\"" (apply str (repeat 100 "a")) "\":0}")))
  (is (refused? (str "{\"title\":\"" (apply str (repeat 1000 "a")) "\"}")))
  (is (refused? (str "{\"version\":" (apply str (repeat 100 "1")) "}")))
  (is (refused? (byte-array [(unchecked-byte 0xff)])))
  (doseq [encoding ["UTF-16" "UTF-16LE" "UTF-16BE" "UTF-32" "UTF-32LE" "UTF-32BE"]]
    (is (refused? (.getBytes valid ^String encoding)) encoding))
  (is (refused? (str valid (char 0xd800))))
  (is (refused? (encode "line" [(row [[0 (apply str (repeat 129 "x"))]])]))))

(deftest bounded-numeric-coordinates
  (doseq [n ["1e999999999" "1e-999999999" "NaN" "Infinity" "1000000000001"
             "1000000000000.000000001"]]
    (is (refused? (str "{\"version\":1,\"kind\":\"line\",\"series\":[{\"label\":\"a\",\"points\":[[0," n "]]}]}")) n))
  (is (= [[-1.0e12 1.0e12]]
         (get-in (spec/decode (encode "point" [(row [[-1.0e12 1.0e12]])])) [:series 0 :points])))
  (doseq [points [[[nil 1]] [[true 1]] [[0 "1"]] [[0]] [[0 1 2]] []]]
    (is (refused? (encode "line" [(row points)])))))

(deftest series-categories-and-total-point-budget
  (is (refused? (encode "line" [])))
  (is (refused? (encode "line" (vec (repeat 9 (row [[0 1]]))))))
  (is (refused? (encode "line" [(row [[0 1]]) (row [[1 2]])])))
  (is (refused? (encode "bar" [(row [["A" 1] ["A" 2]])])))
  (is (refused? (encode "bar" [(row [[1 2]])])))
  (is (refused? (encode "line" [(assoc (row [[0 1]]) "url" "https://example.com")])) )
  (let [points (vec (repeat 1001 [0 1]))]
    (is (refused? (encode "line" [(row points) {"label" "Costs" "points" points}]))))
  (is (= spec/max-points
         (count (get-in (spec/decode (encode "line" [(row (vec (repeat spec/max-points [0 1])))]))
                        [:series 0 :points])))))
