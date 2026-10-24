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

(deftest readable-wiki-schedule-status-does-not-require-owner-setup-or-launch-permission
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        source {:name "maintain-wiki" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d"
                :files {"workflow.edn" "{}"} :definition {:params {}}
                :inspection {:ok true :bundle "bundle" :cases []}}
        draft (assoc (editor/draft source) :path nil)
        state {:selected "maintain-wiki" :drafts {"maintain-wiki" draft}
               :wiki-setup {:status :unavailable} :permissions {:merge? false}
               :activation-status :ready :activations []}
        discharge (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container (workflows/view "Lab" state {:room-id "room"})))
        (is (str/includes? (.-textContent container) "Not scheduled in this room."))
        (is (not (str/includes? (.-textContent container) "Launch saved revision"))))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))

(deftest blank-model-disables-new-run-and-schedule
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        source {:name "echo" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d"
                :files {"workflow.edn" "{}"} :definition {:params {}}
                :inspection {:ok true :bundle "bundle" :cases []}}
        draft (-> (editor/draft source) (assoc :path nil) (assoc-in [:form :model] ""))
        discharge (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container
                           (workflows/view "Lab" {:selected "echo" :drafts {"echo" draft}
                                                   :permissions {:merge? true}}
                                                 {:room-id "room" :change (fn [& _])})))
        (let [buttons (array-seq (.querySelectorAll container "button"))
              named (fn [label] (some #(when (= label (.-textContent %)) %) buttons))]
          (is (some? (named "Confirm and submit")))
          (is (.-disabled (named "Confirm and submit")))
          (is (some? (named "Confirm and activate schedule…")))
          (is (.-disabled (named "Confirm and activate schedule…")))))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))

(deftest readiness-picker-preserves-exact-selection-and-shows-shared-observation-separately
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        source {:name "echo" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d"
                :files {"workflow.edn" "{}"} :definition {:params {}}
                :inspection {:ok true :bundle "bundle" :cases []}}
        draft (-> (editor/draft source) (assoc :path nil) (assoc-in [:form :model] "custom-exact-selection"))
        state {:selected "echo" :drafts {"echo" draft} :permissions {:merge? true}
               :readiness-status :ready
               :readiness {:models [{:id "registered-model" :label "Registered model" :provider-label "Provider"
                                      :credential-label "Login present; connection not tested"}]
                           :allowance {:status :observed :microdollars "9007199254740993" :model-dispatches "8"}}}
        discharge (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container (workflows/view "Lab" state {:room-id "room" :change (fn [& _])})))
        (is (str/includes? (.-textContent container) "9007199254740993 microdollars and 8 model requests"))
        (is (str/includes? (.-textContent container) "Login present; connection not tested"))
        (is (str/includes? (.-textContent container) "Spend ceiling"))
        (let [selected (.querySelector container "option[value='custom-exact-selection']")]
          (is (some? selected))
          (is (.-disabled selected))
          (is (= "custom-exact-selection" (.. selected -parentElement -value)))))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))

(deftest reserved-checker-selector-does-not-block-the-wiki-launch-form
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        source {:name "maintain-wiki" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d"
                :files {"workflow.edn" "{}"} :definition {:params {:simmis/checker-profile :wiki-structure-v1}}
                :inspection {:ok true :bundle "bundle" :cases []}}
        draft (-> (editor/draft source) (assoc :path nil) (assoc-in [:form :model] "registered-model"))
        discharge (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container
                           (workflows/view "Lab" {:selected "maintain-wiki" :drafts {"maintain-wiki" draft}
                                                   :permissions {:merge? true}}
                                                 {:room-id "room" :change (fn [& _])})))
        (is (not (str/includes? (.-textContent container) "Parameter · checker-profile")))
        (is (str/includes? (.-textContent container) "Next activation"))
        (let [button (some #(when (= "Confirm and activate schedule…" (.-textContent %)) %)
                           (array-seq (.querySelectorAll container "button")))]
          (is (some? button))
          (is (false? (.-disabled button)))))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))
