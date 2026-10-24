(ns is.simm.uis.cell-result-view-test
  (:require [cljs.test :refer-macros [deftest is]]
            [is.simm.uis.web.desktop.cell-result-view :as result]
            [is.simm.uis.web.desktop.chart-view :as charts]))

(deftest native-tables-retain-exact-text-without-interpreting-html
  (let [container (.createElement js/document "div")
        integer "123456789012345678901234567890"
        literal "<img src=x onerror=alert(1)>"
        presentation {:kind :table :columns ["Exact" "<script>header</script>"]
                      :rows [[integer literal] ["" "😀\nnotes"]]}]
    (result/paint! container presentation)
    (is (= 1 (.-length (.querySelectorAll container "table"))))
    (is (= [integer literal "" "😀\nnotes"]
           (mapv #(.-textContent %) (array-seq (.querySelectorAll container "td")))))
    (is (= ["Exact" "<script>header</script>"]
           (mapv #(.-textContent %) (array-seq (.querySelectorAll container "th")))))
    (is (= "col" (.getAttribute (.querySelector container "th") "scope")))
    (is (nil? (.querySelector container "img,script")))
    ;; Repainting replaces the old result; clearing cannot leave stale content.
    (result/paint! container {:kind :table :columns ["Next"] :rows [["replacement"]]})
    (is (= 1 (.-length (.querySelectorAll container "td"))))
    (is (not (.includes (.-textContent container) integer)))
    (result/paint! container nil)
    (is (zero? (.-childElementCount container)))))

(deftest chart-results-normalize-json-values-and-use-the-existing-adapter
  (let [container (.createElement js/document "div") calls (atom [])
        series [{:label "Forecast" :points [[0 1] [1 2]]}]]
    (with-redefs [charts/paint! (fn [target data] (swap! calls conj [target data]))]
      (doseq [[kind expected] [["line" :line] ["bar" :bar] ["point" :point] [:line :line]]]
        (result/paint! container {:kind "chart" :data {:kind kind :series series :title "Literal <b>title</b>"}})
        (is (identical? container (first (last @calls))))
        (is (= {:kind expected :series series :title "Literal <b>title</b>"}
               (second (last @calls)))))
      (result/paint! container {:kind :chart :data {:kind :point :series series}})
      (is (= :point (get-in (last @calls) [1 :kind])))
      (result/paint! container nil)
      (is (= 5 (count @calls)) "Clearing does not start another chart projection"))))
