(ns is.simm.uis.money-test
  "How a ledger amount reads.

   The Accounting view is the one place in the app that claims to be
   trustworthy about money, and it was rendering the server's raw BigDecimal
   string — `885300 SEK` where the figure is 885 300.00 SEK. A column of
   ungrouped digits is not wrong so much as unreadable, and an unreadable
   number in a financial summary invites the reader to misread it by an order
   of magnitude.

   These assertions pin the two judgment calls (space grouping, leading minus)
   and the one property that matters more than either: the formatter never
   touches the VALUE. It is string→string, so no amount ever takes a trip
   through a float on its way to a balance sheet."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.money :as money]))

(def ^:private nbsp "\u00a0")

(deftest metering-commodities-read-as-dollars-and-counts
  (testing "microUSD moves the decimal point, exactly"
    (is (= ["5.050001" "USD"] (money/display-amount "5050001.00" "microUSD")))
    (is (= ["8.475" "USD"] (money/display-amount "8475000.00" "microUSD")))
    (is (= ["-17.91" "USD"] (money/display-amount "-17910000.00" "microUSD")))
    (is (= ["0.00" "USD"] (money/display-amount "0" "microUSD")))
    (is (= ["0.000001" "USD"] (money/display-amount "1" "microUSD")))
    (is (= [(str "1" nbsp "234.567891") "USD"] (money/display-amount "1234567891" "microUSD"))))
  (testing "dispatch counts are whole numbers when whole"
    (is (= ["298" "model calls"] (money/display-amount "298.00" "model-dispatch")))
    (is (= ["-406" "model calls"] (money/display-amount "-406.00" "model-dispatch")))
    (is (= ["2.50" "model calls"] (money/display-amount "2.5" "model-dispatch"))))
  (testing "other commodities are untouched"
    (is (= [(str "885" nbsp "300.00") "SEK"] (money/display-amount "885300" "SEK")))
    (is (= ["n/a" "microUSD"] (money/display-amount "n/a" "microUSD")))))

(deftest wallet-accounts-get-reading-labels
  (is (= "Wallet (available)" (money/account-label "Resources:Wallet:ce7e9fd5-bdcd-3b90-94fa-d328bf9cf2f8")))
  (is (= "Consumed" (money/account-label "Resources:Consumed")))
  (is (= "Granted (source)" (money/account-label "Resources:Source")))
  (is (nil? (money/account-label "Assets:Bank"))))

(deftest thousands-are-grouped-and-decimals-are-shown
  (testing "the four figures the Kestrel book actually renders"
    (is (= (str "536" nbsp "511.00") (money/format-amount "536511")))
    (is (= (str "-480" nbsp "000.00") (money/format-amount "-480000")))
    (is (= (str "4" nbsp "800.00") (money/format-amount "4800")))
    (is (= (str "-61" nbsp "311.00") (money/format-amount "-61311"))))
  (testing "groups are three digits, counted from the right"
    (is (= "999.00" (money/format-amount "999")))
    (is (= (str "1" nbsp "000.00") (money/format-amount "1000")))
    (is (= (str "1" nbsp "234" nbsp "567.89") (money/format-amount "1234567.89")))))

(deftest decimals-are-padded-never-truncated
  (is (= "0.00" (money/format-amount "0")))
  (is (= (str "2" nbsp "400.00") (money/format-amount "2400.00")))
  (is (= (str "61" nbsp "311.50") (money/format-amount "61311.5")))
  (testing "more precision than the commodity declares is still money"
    (is (= "1.2345" (money/format-amount "1.2345")))))

(deftest negatives-take-a-minus-not-parentheses
  (testing "these rows are a trial balance: negative means CREDIT, not loss"
    (is (= "-480" (subs (money/format-amount "-480000") 0 4)))
    (is (not (re-find #"[()]" (money/format-amount "-480000"))))))

(deftest a-value-it-cannot-read-is-shown-rather-than-mangled
  ;; What the view displayed before `account-rows` resolved the Money record.
  ;; A formatter that swallows this hides a regression in the one view whose
  ;; job is to be trustworthy about money.
  (is (= "kontor.money.Money@dce06ac3"
         (money/format-amount "kontor.money.Money@dce06ac3")))
  (is (= "" (money/format-amount ""))))
