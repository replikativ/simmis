(ns is.simm.uis.book-period-test
  "Reporting periods for the Book. An income statement over the wrong dates
   is wrong in a way nobody reading it can see, so the calendar arithmetic is
   checked against java.time rather than trusted."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.book-period :as period]))

(defn- ms [s] (.toEpochMilli (java.time.Instant/parse s)))

(deftest days-agree-with-java-time
  (doseq [t (range -10000000000000 10000000000000 7777777777)]
    (is (= (subs (str (java.time.Instant/ofEpochMilli t)) 0 10) (period/format-day t))))
  (is (= (ms "2024-02-29T00:00:00Z") (period/date-ms 2024 2 29))))

(deftest presets
  (let [now (ms "2026-10-13T17:00:00Z")
        days #(update-vals % period/format-day)]
    (is (= {:from "2026-10-01" :through "2026-10-13"} (days (period/period :month now))))
    (is (= {:from "2026-10-01" :through "2026-10-13"} (days (period/period :quarter now))))
    (is (= {:from "2026-01-01" :through "2026-10-13"} (days (period/period :ytd now))))
    (is (= {:from "2025-01-01" :through "2025-12-31"} (days (period/period :last-year now))))
    (is (= {:from nil :through "2026-10-13"} (days (period/period :all now))))
    (testing "the quarter of a mid-quarter month starts at the quarter"
      (is (= "2026-04-01" (period/format-day (:from (period/period :quarter (ms "2026-05-20T00:00:00Z")))))))
    (testing "bounds do not move with the clock within a day — the view
              compares them to decide whether its data is current"
      (is (= (period/period :ytd now) (period/period :ytd (+ now 3600000)))))))
