(ns is.simm.uis.benchmark-report-state-test
  (:require [is.simm.uis.web.desktop.benchmark-report-state :as state]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(def room #uuid "2f2a2b8a-114a-41a6-bb35-29c60e381d0e")
(def scorecard #uuid "de417f35-b619-4c44-a637-b9660430b864")
(def prepared {:source {:room room :store #uuid "c40eac3a-76ec-4387-a4c3-5a7933d841a1" :scorecard scorecard}
               :plan #uuid "be4695d3-584c-42a5-8c89-df7d390cb627"
               :target-stamp #uuid "a0569638-4c93-4b0f-8a18-2c6027c01c50"})

(def receipt {:source (:source prepared) :plan (:plan prepared) :target-stamp (:target-stamp prepared)
              :report {:path (str "reports/benchmarks/" scorecard ".md") :status :existing}
              :index {:path "reports/index.md" :status :existing} :complete? true})

(deftest prepared-source-and-receipt-correspond-to-the-original-request
  (let [intent (state/intent (str room) (str scorecard) prepared)]
    (is (= (:source prepared) (:source intent)))
    (is (state/receipt-current? (str room) intent receipt))
    (doseq [bad [(assoc prepared :plan "not-an-id")
                 (assoc-in prepared [:source :store] "not-an-id")
                 (assoc-in prepared [:source :room] (random-uuid))
                 (assoc-in prepared [:source :scorecard] (random-uuid))]]
      (is (nil? (state/intent (str room) (str scorecard) bad))))
    (doseq [bad [(assoc receipt :plan (random-uuid))
                 (assoc receipt :target-stamp (random-uuid))
                 (assoc-in receipt [:source :store] (random-uuid))
                 (assoc-in receipt [:report :path] "other.md")
                 (assoc-in receipt [:index :path] "other.md")
                 (assoc receipt :complete? false)]]
      (is (not (state/receipt-current? (str room) intent bad))))
    (is (state/receipt-current? (str room) intent
                                (-> receipt (assoc :complete? false) (assoc-in [:index :status] :preserved))))))

(deftest reconnect-retires-only-busy-requests-and-retains-original-recovery-data
  (let [intent (state/intent (str room) (str scorecard) prepared)
        initial {(str scorecard) {:loading? true :token :old :epoch :old :pending intent}}
        retired (state/retire-stale initial :new)]
    (is (nil? (state/retire-stale nil :new)))
    (is (= initial (state/retire-stale initial :old)))
    (is (false? (get-in retired [(str scorecard) :loading?])))
    (is (nil? (get-in retired [(str scorecard) :token])))
    (is (= intent (get-in retired [(str scorecard) :pending])))
    (is (= retired (state/retire-stale retired :new)))))
