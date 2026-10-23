(ns is.simm.uis.memory-tab-address-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.routes :as routes]
            [is.simm.uis.web.desktop.memory-references :as references]))

(deftest tab-target-notification-sees-the-new-address-and-stays-out-of-forks
  (let [live (context/fork-context runtime/runtime)
        listeners @#'sig/focus-listeners saved @listeners events (atom [])]
    (try
      (with-redefs [runtime/runtime live]
        (binding [ec/*execution-context* live]
          (reset! sig/layout-columns [{:id "column" :active-tab "tab" :tabs [{:id "tab" :type :memory
                                                                           :data {:room-id "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa"
                                                                                  :path "wiki/Old.md"}}]}])
          (reset! sig/active-column-id "column")
          (reset! listeners {})
          (sig/on-focus-change! ::address
                                (fn [reason]
                                  (swap! events conj [reason (routes/tab->route (sig/focused-tab @sig/layout-columns @sig/active-column-id))])))
          (sig/retarget-tab! "column" "tab" #(references/selected-tab % "wiki/New.md" false) :navigate)
          (is (= [[:navigate "/room/aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa/files?path=wiki%2FNew.md"]] @events))
          (binding [ec/*execution-context* (assoc live ::execution-frame true)]
            (sig/retarget-tab! "column" "tab" #(references/selected-tab % "wiki/Frame.md" false) :passive))
          (is (= [:passive "/room/aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa/files?path=wiki%2FFrame.md"] (last @events)))
          (let [fork (context/fork-context live)]
            (try
              (binding [ec/*execution-context* fork]
                (sig/retarget-tab! "column" "tab" #(references/selected-tab % "wiki/Simulation.md" false) :navigate)
                (is (= "wiki/Simulation.md" (get-in (first @sig/layout-columns) [:tabs 0 :data :path]))))
              (is (= 2 (count @events)))
              (is (= "wiki/Frame.md" (get-in (first @sig/layout-columns) [:tabs 0 :data :path])))
              (finally (context/close-context! fork))))))
      (finally (reset! listeners saved) (context/close-context! live)))))
