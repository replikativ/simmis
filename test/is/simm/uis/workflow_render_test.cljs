(ns is.simm.uis.workflow-render-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.dom.browser :as browser]
            [is.simm.uis.web.desktop.workflow-editor :as editor]
            [is.simm.uis.web.desktop.views.workflows :as workflows]))

(deftest benchmark-deployment-preview-renders-exact-model-and-trial-ceilings
  (let [ctx (context/create-execution-context)
        container (.createElement js/document "div")
        discharge (atom nil)
        opened (atom 0)
        source {:name "echo" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d"
                :files {"workflow.edn" "{}"} :definition {:params {}}
                :inspection {:ok true :bundle "bundle" :cases ["a" "b"]}}
        draft (-> (editor/draft source)
                  (assoc :path nil :funded? true)
                  (assoc-in [:form :mode] "benchmark")
                  (assoc-in [:form :model] "rendered-benchmark-model")
                  (assoc-in [:form :repetitions] "2")
                  (assoc-in [:schedule-submission :reply] {:id "created-id" :enabled? true}))]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge
                (browser/mount! container
                                (workflows/view "Lab"
                                                {:selected "echo" :list {:names ["echo"]}
                                                 :drafts {"echo" draft} :permissions {:merge? true :fund? true}}
                                                {:change (fn [& _]) :open-schedules #(swap! opened inc)})))
        (let [text (.-textContent container)]
          (is (str/includes? text "rendered-benchmark-model · every 1440 minutes"))
          (is (str/includes? text "shared across benchmark trials"))
          (is (str/includes? text "Per benchmark trial: at most 4 model steps and 120000 ms."))
          (is (str/includes? text "Created schedule: created-id. Current status"))
          (is (str/includes? text "This receipt is not its current balance"))
          (is (not (str/includes? text "Schedule: created-id · Active"))))
        (.click (first (filter #(= "Open Schedules" (.-textContent %))
                               (array-seq (.querySelectorAll container "button")))))
        (is (= 1 @opened)))
      (finally
        (when @discharge (browser/unmount! @discharge))
        (.remove container)
        (context/stop-context! ctx)))))
