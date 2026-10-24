(ns is.simm.documents.cell-result-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.cell-result :as result]
            [is.simm.documents.chart-spec :as chart]))

(defrecord ResultRecord [version kind columns rows])

(defn- envelope [data] {:simmis/presentation data})
(defn- table [columns rows] (envelope {:version 1 :kind :table :columns columns :rows rows}))
(defn- reason [value]
  (try (result/presentation value) nil
       (catch clojure.lang.ExceptionInfo e
         (is (= ::result/refused (:type (ex-data e))))
         (:reason (ex-data e)))))

(deftest ordinary-values-do-not-opt-in
  (doseq [value [nil 1 "hello" {:kind :table :rows []} {:version 1 :kind :chart} [1 2]]]
    (is (nil? (result/presentation value))))
  (let [touched (atom false) value (lazy-seq (reset! touched true) (list 1))]
    (is (nil? (result/presentation value)))
    (is (false? @touched))))

(deftest closed-versioned-opt-in
  (doseq [value [{:simmis/presentation nil}
                 {:simmis/presentation {:version 2 :kind :table}}
                 {:simmis/presentation {:version 1.0 :kind :table}}
                 {:simmis/presentation {:version 1 :kind :html}}
                 (assoc (table ["a"] []) :extra 1)
                 (envelope {:version 1 :kind :table :columns ["a"] :rows [] :extra true})
                 (envelope {:version 1 :kind :chart :data {} :extra true})]]
    (is (some? (reason value)))))

(deftest exact-table-scalar-conversion
  (is (= {:profile result/profile :kind :table :columns ["Exact" "Value"]
          :rows [["123456789012345678901234567890" "0.100000000000000000001"]
                 ["true" "false"] ["" "<img src=x onerror=alert(1)>"]
                 ["1.25" "😀\nnotes"]]}
         (result/presentation
          (table ["Exact" "Value"] [[123456789012345678901234567890N 0.100000000000000000001M]
                                     [true false] [nil "<img src=x onerror=alert(1)>"]
                                     [1.25 "😀\nnotes"]]))))
  (is (= "9223372036854775807"
         (get-in (result/presentation (table ["n"] [[Long/MAX_VALUE]])) [:rows 0 0])))
  (is (= 50 (count (:rows (result/presentation (table (vec (repeat 20 "c"))
                                                      (vec (repeat 50 (vec (repeat 20 1)))))))))))

(deftest table-shape-and-size-bounds
  (doseq [value [(table [] []) (table (vec (repeat 21 "a")) [])
                 (table ["a"] (vec (repeat 51 [1]))) (table ["a"] [[1 2]])
                 (table '("a") []) (table ["a"] '([1])) (table ["a"] ['(1)])
                 (table [""] []) (table [" "] []) (table [(apply str (repeat 129 "a"))] [])
                 (table ["a"] [[(apply str (repeat 513 "a"))]])
                 (table (vec (repeat 20 "a")) (vec (repeat 50 (vec (repeat 20 (apply str (repeat 40 "x")))))))]]
    (is (some? (reason value))))
  (is (= [] (:rows (result/presentation (table ["Empty"] [])))))
  (is (= :oversized-table
         (reason (table (vec (repeat 20 "a"))
                        (vec (repeat 50 (vec (repeat 20 (apply str (repeat 20 "é")))))))))))

(deftest no-lazy-realization-or-arbitrary-object-printing
  (let [touched (atom false) lazy-row (lazy-seq (reset! touched true) (list 1))
        object (proxy [Object] [] (toString [] (reset! touched true) "bad"))
        number (proxy [Number] []
                 (doubleValue [] (reset! touched true) 1.0)
                 (floatValue [] (float 1)) (intValue [] 1) (longValue [] 1)
                 (toString [] (reset! touched true) "bad"))]
    (doseq [value [(table ["a"] lazy-row) (table ["a"] [lazy-row])
                   (table ["a"] [[object]]) (table ["a"] [[number]])
                   (table ["a"] [[{}]]) (table ["a"] [[[]]])
                   (table ["a"] [[1/3]])]]
      (is (some? (reason value))))
    (is (false? @touched)))
  (is (some? (reason (envelope (->ResultRecord 1 :table ["a"] []))))))

(deftest invalid-number-and-text-refusals
  (doseq [cell [##Inf ##-Inf ##NaN (bigint (.pow (biginteger 10) 2000))
                (str (char 0xd800)) (str (char 0xdc00)) (str \u0000) (str \u007f)]]
    (is (some? (reason (table ["a"] [[cell]])))))
  (is (= "line\r\n\tend" (get-in (result/presentation (table ["a"] [["line\r\n\tend"]])) [:rows 0 0]))))

(deftest charts-use-the-existing-canonical-projection
  (let [data {"version" 1 "kind" "line" "series" [{"label" "Forecast" "points" [[0 1] [1 2]]}]}]
    (is (= {:profile result/profile :kind :chart :data (chart/validate-data data)}
           (result/presentation (envelope {:version 1 :kind :chart :data data})))))
  (is (= :invalid-chart (reason (envelope {:version 1 :kind :chart :data {}}))))
  (is (= :invalid-chart (reason (envelope {:version 1 :kind :chart :data {:version 1 :kind :line :series []}})))))

(deftest inflated-decimal-is-bounded-before-precision-and-printing
  (let [huge (.shiftLeft java.math.BigInteger/ONE 10000)
        inflated (java.math.BigDecimal. huge 3011)]
    (is (= :invalid-number (reason (table ["n"] [[inflated]])))))
  (let [digits (apply str (repeat 512 "9"))]
    (is (= digits (get-in (result/presentation (table ["n"] [[(bigdec digits)]])) [:rows 0 0])))))
