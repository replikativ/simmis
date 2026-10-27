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
                  (assoc :path nil :funded? true :schedule-open? true)
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
          (is (str/includes? text "rendered-benchmark-model · Every 1440 minutes"))
          (is (str/includes? text "shared across benchmark trials"))
          (is (str/includes? text "Per benchmark trial: at most 4 model steps and 120000 ms."))
          (is (str/includes? text "Saved schedule: created-id. Acknowledged as enabled. Current status"))
          (is (str/includes? text "This receipt is not its current balance"))
          (is (not (str/includes? text "Schedule: created-id · Active"))))
        (.click (first (filter #(= "Open Workflows" (.-textContent %))
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
               :activation-status :ready
               :activations [{:id "activation" :room-id "room" :name "maintain-wiki" :enabled? true
                              :occurrences [{:id "occurrence" :intended-at 1 :status :completed
                                             :batch {:status :observed :id "batch" :phase :review}}]}]}
        discharge (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container (workflows/view "Lab" state {:room-id "room"})))
        (is (str/includes? (.-textContent container) "Input batch awaiting review"))
        (is (not (str/includes? (.-textContent container) "No wiki changes")))
        (is (not (str/includes? (.-textContent container) "Launch saved revision"))))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))

(deftest blank-model-disables-new-run-and-schedule
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        source {:name "echo" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d"
                :files {"workflow.edn" "{}"} :definition {:params {}}
                :inspection {:ok true :bundle "bundle" :cases []}}
        draft (-> (editor/draft source) (assoc :path nil :schedule-open? true) (assoc-in [:form :model] ""))
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
          (is (some? (named "Run once…")))
          (is (.-disabled (named "Run once…")))
          (is (some? (named "Confirm and activate schedule…")))
          (is (.-disabled (named "Confirm and activate schedule…")))
          (is (some? (named "Save paused schedule")))
          (is (.-disabled (named "Save paused schedule")))))
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
        draft (-> (editor/draft source) (assoc :path nil :schedule-open? true) (assoc-in [:form :model] "registered-model"))
        discharge (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container
                           (workflows/view "Lab" {:selected "maintain-wiki" :drafts {"maintain-wiki" draft}
                                                   :permissions {:merge? true}}
                                                 {:room-id "room" :change (fn [& _])})))
        (is (not (str/includes? (.-textContent container) "Parameter · checker-profile")))
        (is (str/includes? (.-textContent container) "Next schedule"))
        (let [button (some #(when (= "Confirm and activate schedule…" (.-textContent %)) %)
                           (array-seq (.querySelectorAll container "button")))]
          (is (some? button))
          (is (false? (.-disabled button)))))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))

(deftest overview-keeps-source-and-execution-controls-in-closed-context-owned-disclosures
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        source {:name "maintain-wiki" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d"
                :files {"workflow.edn" "{}"} :definition {:title "Keep knowledge current" :params {}}
                :inspection {:ok true :bundle "bundle" :cases []}}
        draft (assoc (editor/draft source) :path nil)
        changes (atom []) discharge (atom nil)]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container
                           (workflows/view "Lab" {:selected "maintain-wiki" :drafts {"maintain-wiki" draft}
                                                   :permissions {:merge? true}}
                                                 {:room-id "room" :change (fn [& args] (swap! changes conj args))})))
        (is (= "Keep knowledge current" (.-textContent (.querySelector container ".workflow-detail h3"))))
        (let [[source execution] (array-seq (.querySelectorAll container ".workflow-detail details.workflow-disclosure"))]
          (is (false? (.-open source)))
          (is (false? (.-open execution)))
          (is (nil? (.querySelector container ".workflow-scheduling"))
              "Schedule configuration appears only after choosing Schedule")
          (is (every? #(false? (.checkVisibility %))
                      (array-seq (.querySelectorAll execution "input,select,button")))
              "Closed execution controls do not occupy the overview")
          (is (= "Instructions and code" (.-textContent (.querySelector source "summary")))))
        (is (empty? @changes) "Rendering does not change a draft or start an operation")
        (let [details (.querySelector container ".workflow-detail details.workflow-disclosure")]
          (.remove container)
          (set! (.-open details) true)
          (.dispatchEvent details (js/Event. "toggle"))
          (is (empty? @changes) "Queued native toggles after detach cannot recreate or mutate the draft")))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))

(deftest pristine-starter-upgrade-is-explicit-and-does-not-run-work-during-render
  (let [ctx (context/create-execution-context) container (.createElement js/document "div")
        discharge (atom nil) calls (atom [])
        state {:wiki-setup {:status :upgrade-available :revision "069c7012-c566-4e46-8aab-e7f9a25abc3a"
                           :installed-version 3 :shipped-version 4}}
        actions {:room-id "room" :upgrade-wiki-setup #(swap! calls conj :upgrade)}]
    (.appendChild (.-body js/document) container)
    (try
      (binding [ec/*execution-context* ctx]
        (reset! discharge (browser/mount! container (workflows/view "Lab" state actions)))
        (is (empty? @calls))
        (is (str/includes? (.-textContent container) "Existing schedules are not upgraded"))
        (let [button (some #(when (= "Upgrade starter v3 → v4" (.-textContent %)) %)
                           (array-seq (.querySelectorAll container "button")))]
          (is (some? button))
          (.click button)
          (is (= [:upgrade] @calls))))
      (finally (when @discharge (browser/unmount! @discharge))
               (.remove container) (context/stop-context! ctx)))))

(deftest next-input-check-is-a-compact-observation-without-execution-controls
  (let [source {:name "wiki" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d"
                :files {"workflow.edn" "{}"} :definition {:params {}}
                :inspection {:ok true :bundle "34e2519c-9acb-4a13-9ff1-36ff01ac600d" :cases []}}
        state {:selected "wiki" :drafts {"wiki" (editor/draft source)}}
        request (editor/stream-request state)
        reply {:name "wiki" :source-revision (:revision source) :bundle (get-in source [:inspection :bundle])
               :status :observed :lifecycle :active :pending {:count 1 :complete? true :limit 20000}
               :freshness {:status :current} :batch {:status :none}}
        held {:status :held :code :context-exceeds-budget
              :context {:files 33 :bytes 100 :file-limit 32 :byte-limit 524288}
              :head {:bytes 100 :file-limit 524288 :batch-byte-limit 1048576}
              :reserved {:files 0 :bytes 0} :limits {:files 100 :bytes 2097152}}]
    (doseq [[check text details?]
            [[held "Context exceeds its file or byte limit." true]
             [{:status :not-checked} "Not checked against this observation." false]
             [{:status :unavailable} "Input check unavailable." false]]]
      (let [ctx (context/create-execution-context) container (.createElement js/document "div")
            discharge (atom nil) tree (atom nil) calls (atom [])
            render #(workflows/view "Lab" % {:room-id "room" :change (fn [& args] (swap! calls conj args))})
            observed (editor/observe-stream state request (assoc reply :input-check check))]
        (.appendChild (.-body js/document) container)
        (try
          (binding [ec/*execution-context* ctx]
            (reset! tree (render observed))
            (reset! discharge (browser/mount! container @tree))
            (is (str/includes? (.-textContent container) text))
            (let [detail (some #(when (= "Input counts and limits" (.-textContent %)) (.-parentElement %))
                               (array-seq (.querySelectorAll container "summary")))]
              (is (= details? (some? detail)))
              (when detail
                (is (false? (.-open detail)))
                (is (nil? (.querySelector detail "button,input,select")))))
            (is (empty? @calls))
            (when details?
              (let [detail (some #(when (= "Input counts and limits" (.-textContent %)) (.-parentElement %))
                                 (array-seq (.querySelectorAll container "summary")))]
                (set! (.-open detail) true)
                (.dispatchEvent detail (js/Event. "toggle"))
                (is (= [["wiki" assoc :input-counts-open? true]] @calls))
                (let [changed (editor/observe-stream observed request
                                (assoc reply :freshness {:status :changed} :input-check {:status :not-checked}))
                      next-tree (render changed)]
                  (browser/refresh! @discharge @tree next-tree)
                  (reset! tree next-tree)
                  (is (str/includes? (.-textContent container) "Not checked against this observation."))
                  (is (not (str/includes? (.-textContent container) "Context exceeds its file or byte limit.")))
                  (is (false? (.-isConnected detail))))
                (set! (.-open detail) false)
                (.dispatchEvent detail (js/Event. "toggle"))
                (is (= 1 (count @calls)) "Detached native toggles cannot mutate the owning draft"))))
          (finally (when @discharge (browser/unmount! @discharge))
                   (.remove container) (context/stop-context! ctx)))))))

(deftest benchmark-model-facts-are-hidden-until-readiness-is-current
  (doseq [status [:checking :unavailable]]
    (let [ctx (context/create-execution-context)
          container (.createElement js/document "div")
          source {:name "echo" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d"
                  :files {"workflow.edn" "{}"} :definition {:params {}}
                  :inspection {:ok true :bundle "bundle" :cases []}}
          draft (-> (editor/draft source) (assoc-in [:form :mode] "benchmark"))
          discharge (atom nil)]
      (.appendChild (.-body js/document) container)
      (try
        (binding [ec/*execution-context* ctx]
          (reset! discharge (browser/mount! container
            (workflows/view "Lab" {:selected "echo" :drafts {"echo" draft}
                                   :readiness-status status
                                   :readiness {:models [{:id "old" :label "STALE-MODEL" :credential-label "STALE-CREDENTIAL"}]}
                                   :permissions {:merge? true :operator? true}}
                                  {:room-id "room" :change (fn [& _])})))
          (is (not (str/includes? (.-textContent container) "STALE-MODEL")))
          (is (not (str/includes? (.-textContent container) "STALE-CREDENTIAL")))
          (is (str/includes? (.-textContent container) "Verifying model options")))
        (finally (when @discharge (browser/unmount! @discharge))
                 (.remove container) (context/stop-context! ctx))))))
