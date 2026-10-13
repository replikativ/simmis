(ns is.simm.ops.book-report-test
  "A team's Book: statements over a real kontor book, and the journal's
   attribution of entries to agent runs.

   The statements are arrangements of kontor's trial balance, so the assertions
   that matter are the cross-checks a reader relies on without seeing them:
   the balance sheet balances only because the unclosed result is shown as
   equity; the income statement over a period is the MOVEMENT in that period,
   not the balance at its end; and an entry is attributed to a run only when
   exactly one booking call was running when it was sealed."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [kontor.book :as book]
            [kontor.core :as kcore]
            [is.simm.model.accounting :as accounting]
            [is.simm.ops.book-report :as br]))

(defn- day [s]
  (java.util.Date. (.toEpochMilli (.toInstant (.atStartOfDay (java.time.LocalDate/parse s)
                                                             java.time.ZoneOffset/UTC)))))

(defn- fresh-book []
  (let [conn (kcore/create-test-db {:store {:backend :memory :id (random-uuid)}
                                    :schema-flexibility :write
                                    :keep-history? true})]
    ;; the chart every room book starts with (`accounting/starter-book`)
    (d/transact conn accounting/starter-book)
    conn))

(defn- post! [conn date dr cr amount narration]
  (book/entry! conn {:debit-account [:kontor.account/path dr]
                     :credit-account [:kontor.account/path cr]
                     :amount (bigdec amount)
                     :commodity [:kontor.commodity/symbol "USD"]
                     :journal-type :general
                     :effective-date (day date)
                     :narration narration}))

(defn- seeded []
  (doto (fresh-book)
    (post! "2026-01-02" "Assets:Bank" "Equity:Opening" 10000 "capital")
    (post! "2026-02-10" "Expenses:General" "Assets:Bank" 1200 "february costs")
    (post! "2026-02-20" "Assets:Receivable" "Income:Sales" 5000 "february invoice")
    (post! "2026-03-15" "Assets:Bank" "Assets:Receivable" 5000 "invoice paid")
    (post! "2026-03-20" "Expenses:General" "Liabilities:Payable" 300 "march bill")))

(defn- rows [conn through]
  (#'br/balance-rows conn (#'br/end-of-day (day through)) nil))

(defn- by-path [lines] (into {} (map (juxt :path :amount)) lines))

(deftest balance-sheet-balances-with-the-unclosed-result
  (let [[sheet] (br/balance-sheet (rows (seeded) "2026-03-31"))]
    (is (= "USD" (:commodity sheet)))
    (is (:balanced? sheet))
    (testing "each section in its natural sign"
      (is (= {"Assets:Bank" 13800M} (by-path (get-in sheet [:assets :lines]))))
      (is (= {"Liabilities:Payable" 300M} (by-path (get-in sheet [:liabilities :lines])))))
    (testing "the result not yet closed is what makes it balance"
      (is (= {"Equity:Opening" 10000M "Result not yet closed" 3500M}
             (by-path (get-in sheet [:equity :lines]))))
      (is (zero? (.compareTo 13800M (:liabilities-and-equity sheet)))))))

(deftest balance-sheet-respects-the-reporting-date
  (testing "an entry effective ON the reporting date is included"
    (let [[sheet] (br/balance-sheet (rows (seeded) "2026-03-20"))]
      (is (= {"Liabilities:Payable" 300M} (by-path (get-in sheet [:liabilities :lines]))))))
  (testing "and the day before, it is not"
    (let [[sheet] (br/balance-sheet (rows (seeded) "2026-03-19"))]
      (is (empty? (get-in sheet [:liabilities :lines])))
      (is (:balanced? sheet)))))

(deftest income-statement-is-the-movement-in-the-period
  (let [conn (seeded)
        closing (rows conn "2026-03-31")]
    (testing "since the first entry"
      (let [[pl] (br/income-statement [] closing)]
        (is (= {"Income:Sales" 5000M} (by-path (get-in pl [:income :lines]))))
        (is (= {"Expenses:General" 1500M} (by-path (get-in pl [:expenses :lines]))))
        (is (zero? (.compareTo 3500M (:net pl))))))
    (testing "March alone: February's invoice and costs are not in it"
      (let [opening (#'br/balance-rows conn (#'br/just-before (day "2026-03-01")) nil)
            [pl] (br/income-statement opening closing)]
        (is (empty? (get-in pl [:income :lines])))
        (is (= {"Expenses:General" 300M} (by-path (get-in pl [:expenses :lines]))))
        (is (zero? (.compareTo -300M (:net pl))))))))

(deftest trial-balance-agrees-with-kontor
  (let [[tb] (br/trial-balance (rows (seeded) "2026-03-31"))]
    (is (:balanced? tb))
    (is (zero? (.compareTo 15300M (:debits tb))))
    (is (= {"Assets:Bank" [13800M nil] "Equity:Opening" [nil 10000M]}
           (into {} (comp (filter #(#{"Assets:Bank" "Equity:Opening"} (:path %)))
                          (map (juxt :path (juxt :debit :credit))))
                 (:lines tb))))))

(deftest journal-lists-the-period-newest-first
  (let [{:keys [entries truncated?]} (#'br/journal @(seeded) (day "2026-02-01") (day "2026-02-28"))]
    (is (not truncated?))
    (is (= ["february invoice" "february costs"] (mapv :narration entries)))
    (is (= [{:path "Assets:Receivable" :commodity "USD" :amount 5000M}
            {:path "Income:Sales" :commodity "USD" :amount -5000M}]
           (:postings (first entries)))
        "debits first")
    (is (every? #(= :posted (:state %)) entries))))

(deftest an-entry-names-a-run-only-when-one-call-contains-it
  (let [at (fn [ms] (java.util.Date. (long ms)))
        call (fn [run start end] {:run-id run :start (at start) :end (at end)})
        a (random-uuid) b (random-uuid)]
    (is (= a (br/posted-by [(call a 100 200)] (at 150))))
    (is (= a (br/posted-by [(call a 100 200)] (at 200))) "the end is inclusive")
    (is (nil? (br/posted-by [(call a 100 200)] (at 250))) "posted outside every call")
    (is (nil? (br/posted-by [(call a 100 200) (call b 120 300)] (at 150)))
        "two concurrent booking calls: name neither")
    (is (nil? (br/posted-by [(call a 100 200)] nil)))))

(deftest booking-calls-are-the-evals-that-called-kontor
  (let [conn (d/connect (doto {:store {:backend :memory :id (random-uuid)}
                               :schema-flexibility :read}
                          d/create-database))
        run (random-uuid)]
    (d/transact conn [{:tool-call/name "clojure_eval" :tool-call/run-id run
                       :tool-call/started-at (java.util.Date. 100)
                       :tool-call/ended-at (java.util.Date. 200)
                       :tool-call/input "{:code \"(kontor/entry! {})\"}"}
                      {:tool-call/name "clojure_eval" :tool-call/run-id (random-uuid)
                       :tool-call/started-at (java.util.Date. 100)
                       :tool-call/ended-at (java.util.Date. 200)
                       :tool-call/input "{:code \"(wiki/pages)\"}"}])
    (is (= [run] (mapv :run-id (#'br/booking-calls @conn))))))
