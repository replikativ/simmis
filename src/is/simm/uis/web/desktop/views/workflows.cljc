(ns is.simm.uis.web.desktop.views.workflows
  "Room-native source editing and explicit bounded admission."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.workflow-editor :as editor]
            [clojure.string :as str]))

(defn- field [label value change]
  (el/label {:key label :class "workflow-field"}
            (el/span {} label)
            (el/input {:value (or value "") :on-input (fn [e] (change #?(:cljs (.. e -target -value) :clj "")))})))

(defn view [room-name state actions]
  (let [selected (:selected state) draft (get-in state [:drafts selected])
        source (:source draft) form (:form draft)
        permissions (:permissions state)
        loading (reduce (fn [out slot] (assoc out slot (boolean (get-in state [:loading [slot selected]]))))
                        {:list (boolean (get-in state [:loading :list]))} [:source :save :submit :fund])
        write? (:write? permissions) merge? (:merge? permissions)
        benchmark? (= "benchmark" (:mode form))
        display-cases (if benchmark? (editor/benchmark-cases (:inspection source) form)
                          (get-in source [:inspection :cases]))
        local? (editor/local? form)
        cli? (editor/cli? form)
        funded-here? (and (:funded? draft) (= (:fund-backend draft "hosted") (:backend form "hosted")))
        change #(apply (:change actions) selected %&)
        set-form (fn [key value] (change update :form editor/set-form-field key value))
        submission (:submission draft)
        uncertain? (and submission (nil? (:reply submission)))]
    (el/div {:class "workflow-panel"}
            (el/div {:class "workflow-toolbar"}
                    (el/h2 {} (str room-name " · Workflows"))
                    (el/button {:on-click (fn [_] ((:refresh actions))) :disabled (:list loading)} "Refresh"))
     (el/p {:class "workflow-note"} "A workflow is a reusable task saved in this room. Each Run executes it separately and produces results or a proposal to review. Ordinary chat does not require a workflow.")
     (el/p {:class "workflow-note"} "This is the source editor: instructions and inputs are versioned room files. The optional checker is for benchmarking; inspecting source never executes it.")
            (for [[slot error] (:errors state)]
              (el/div {:key (pr-str slot) :class "proposal-error" :role "alert"}
                      (str (when (vector? slot) (str (second slot) ": ")) error)))
            (el/div {:class "workflow-layout"}
                    (el/div {:class "workflow-list"}
                            (when-not (:list state)
                              (el/button {:on-click (fn [_] ((:refresh actions))) :disabled (:list loading)}
                                         (if (:list loading) "Loading…" "Load workflows")))
                            (for [name (sort (set (concat (get-in state [:list :names]) (keys (:drafts state)))))]
                              (el/button {:key name :class (when (= name selected) "workflow-selected")
                                          :on-click (fn [_] ((:select actions) name))} name))
                            (when (get-in state [:list :next-offset])
                              (el/button {:on-click (fn [_] ((:more actions))) :disabled (:list loading)} "More workflows"))
                            (when write?
                              (el/div {:class "workflow-new"}
                                      (field "New workflow name" (:new-name state) (:new-name actions))
                                      (el/button {:on-click (fn [_] ((:create actions))) :disabled (:source loading)} "Create workflow"))))
                    (el/div {:class "workflow-detail"}
                            (cond
                              (:source loading) (el/p {} "Loading source…")
                              (nil? source) (el/p {} "Load the room's workflows, then select one or create a draft.")
                              :else
                              (el/div {}
                                      (el/h3 {} selected)
                                      (el/div {:class "workflow-toolbar"}
                                              (el/span {} (if (editor/dirty? draft) "Unsaved changes" "Saved source"))
                                              (el/button {:disabled (or (not write?) (:save loading) (not (editor/dirty? draft)))
                                                          :on-click (fn [_] ((:save actions) selected))}
                                                         (if (:save loading) "Saving…" "Save and inspect"))
                                              (el/button {:disabled (or (:source loading) (:save loading))
                                                          :on-click (fn [_]
                                                                      #?(:cljs (when (or (not (editor/dirty? draft))
                                                                                         (js/confirm "Discard this local draft and reload the saved source?"))
                                                                                 ((:reload actions) selected)) :clj nil))}
                                                         "Reload source"))
                                      (el/div {:class "workflow-source"}
                                              (el/div {:class "workflow-files"}
                                                      (for [path (sort (keys (:files draft)))]
                                                        (el/button {:key path :class (when (= path (:path draft)) "workflow-selected")
                                                                    :on-click (fn [_] (change assoc :path path))} path))
                                                      (when write?
                                                        (el/button {:on-click (fn [_]
                                                                                #?(:cljs (when-let [path (js/prompt "Relative filename within this workflow")]
                                                                                           (when-not (or (str/blank? path) (contains? (:files draft) path))
                                                                                             (change #(-> % (assoc-in [:files path] "") (assoc :path path)))))
                                                                                   :clj nil))} "Add file")))
                                              (el/div {:class "workflow-text"}
                                                      (when-let [path (:path draft)]
                                                        (el/div {}
                                                                (el/div {:class "workflow-toolbar"}
                                                                        (el/span {} path)
                                                                        (when write?
                                                                          (el/button {:on-click (fn [_]
                                                                                                  #?(:cljs (when (js/confirm (str "Remove " path " from the draft? Save commits the deletion."))
                                                                                                             (change #(-> % (update :files dissoc path) (assoc :path nil)))) :clj nil))}
                                                                                     "Remove file")))
                                                                (el/textarea {:aria-label "Workflow file source" :spellcheck false :readonly (not write?)
                                                                              :value (get-in draft [:files path] "")
                                                                              :on-input (fn [e] (change assoc-in [:files path] #?(:cljs (.. e -target -value) :clj "")))})))))
                                      (el/div {:class "workflow-inspection"}
                                              (el/strong {} (if (get-in source [:inspection :ok]) "Structure valid — not checker-tested" "Draft needs repair"))
                                              (when (editor/dirty? draft) (el/p {} "Inspection below is of the saved source. Save before launching."))
                                              (for [problem (get-in source [:inspection :problems])]
                                                (el/p {:key problem} (str problem)))
                                              (el/p {} (str "Source revision: " (:revision source)))
                                              (when-let [bundle (get-in source [:inspection :bundle])]
                                                (el/p {} (str "Execution bundle: " bundle))))
                                      (when merge?
                                        (el/div {:class "workflow-launch"}
                                                (el/h3 {} "Launch saved revision")
                                                (el/label {:class "workflow-field"}
                                                          (el/span {} "Execution backend")
                                                          (el/select {:value (get form :backend "hosted")
                                                                      :on-change (fn [e] (set-form :backend #?(:cljs (.. e -target -value) :clj "hosted")))}
                                                                     (el/option {:value "hosted"} "This Simmis server")
                                                                     (when (:operator? permissions)
                                                                       (el/option {:value "cli"} "This server · Claude Code text benchmark"))
                                                                     (el/option {:value "local"} "My enrolled local SCI worker")))
                                                (when-not local?
                                                  (el/p {:class "workflow-note"}
                                                        "Runs on this Simmis instance, including when it runs on your computer. Model inference uses its configured provider."))
                                                (if local?
                                                  (el/div {}
                                                          (el/button {:disabled (get-in state [:loading :workers])
                                                                      :on-click (fn [_] ((:workers actions)))} "Refresh my workers")
                                                          (el/label {:class "workflow-field"}
                                                                    (el/span {} "Enrolled worker")
                                                                    (el/select {:value (:worker form)
                                                                                :on-change (fn [e] (set-form :worker #?(:cljs (.. e -target -value) :clj "")))}
                                                                               (el/option {:value ""} "Choose an enrolled worker")
                                                                               (for [{:keys [worker config]} (get-in state [:workers :workers])]
                                                                                 (el/option {:key (str worker) :value (str worker)}
                                                                                            (str (:model config) " · " worker)))))
                                                          (when (get-in state [:workers :next-after])
                                                            (el/button {:disabled (get-in state [:loading :workers])
                                                                        :on-click (fn [_] ((:more-workers actions)))} "More workers"))
                                                          (el/p {:class "workflow-note"} "Enrollment is not liveness. Start the private local driver separately; no login is uploaded. Allowance uses admissions, not measured dollars or tokens. Wall time includes cold JVM/interpreter startup; new local forms default to 300000 ms."))
                                                  (field (if cli? "Model id (Claude Code text)" "Model id (explicit native provider)") (:model form) #(set-form :model %)))
                                                (el/label {:class "workflow-field"}
                                                          (el/span {} "Mode")
                                                          (el/select {:value (:mode form) :on-change (fn [e] (set-form :mode #?(:cljs (.. e -target -value) :clj "run")))}
                                                                     (when-not cli? (el/option {:value "run"} "Ordinary Run"))
                                                                     (when (:operator? permissions) (el/option {:value "benchmark"} "Operator benchmark"))))
                                                (when (and benchmark? (get-in source [:inspection :evaluation]))
                                                  (el/label {:class "workflow-field"}
                                                            (el/span {} "Evaluation split")
                                                            (el/select {:value (get form :split "development")
                                                                        :on-change (fn [e] (set-form :split #?(:cljs (.. e -target -value) :clj "development")))}
                                                                       (for [split ["development" "validation" "held-out"]
                                                                             :when (get-in source [:inspection :evaluation :splits (keyword split)])]
                                                                         (el/option {:key split :value split} split)))))
                                                (when (seq display-cases)
                                                  (el/label {:class "workflow-field"}
                                                            (el/span {} "Case")
                                                            (el/select {:value (:case form) :on-change (fn [e] (set-form :case #?(:cljs (.. e -target -value) :clj "")))}
                                                                       (el/option {:value ""} (if benchmark? "All selected-split cases (up to4)" "Choose a case"))
                                                                       (for [case display-cases]
                                                                         (el/option {:key case :value case} case)))))
                                                (for [[name value] (:params form)]
                                                  (field (str "Parameter · " name) (str value) #(change assoc-in [:form :params name] %)))
                                                (when benchmark?
                                                  (el/div {}
                                                          (el/p {:class "workflow-note"} "Operator-only: executes the saved checker, uses saved parameter defaults, and discards candidate worlds. Save parameter changes in workflow.edn before benchmarking.")
                                                          (field "Repetitions (1–2; maximum8 cells total)" (:repetitions form) #(set-form :repetitions %))))
                                                (for [[key label] (cond
                                                                   cli? [[:cli-runs "Allowance · single-use CLI Runs (1–8)"]
                                                                         [:max-model-steps "Model steps per cell (1–32)"]
                                                                         [:timeout-ms "Wall time per cell · milliseconds (1000–600000)"]]
                                                                   local?
                                                                   [[:worker-admissions "Allowance · local admissions (1–32)"]
                                                                    [:timeout-ms "Wall time · milliseconds (1000–600000)"]]
                                                                   :else [[:budget-microdollars "Budget · microdollars (1–5000000)"]
                                                                   [:max-dispatches "Model HTTP dispatches (1–32)"]
                                                                   [:max-model-steps "Model steps (1–32)"]
                                                                    [:timeout-ms "Wall time · milliseconds (1000–600000)"]])]
                                                  (field label (get form key) #(set-form key %)))
                                                (when-not benchmark?
                                                  (el/label {:class "workflow-field"}
                                                            (el/span {} "Settlement")
                                                            (el/select {:value (:settlement form)
                                                                        :on-change (fn [e] (set-form :settlement #?(:cljs (.. e -target -value) :clj "review")))}
                                                                       (el/option {:value "review"} "Retain for review — no automatic merge")
                                                                       (el/option {:value "discard"} "Capture output and discard work"))))
                                                (el/p {:class "workflow-note"}
                                                      (cond cli? "Experimental, operator-enabled backend. Dvergr owns the SCI tool loop; Claude Code provides text inference only. One funded admission per cell; finite steps/time, no compaction, resume or automatic retry. Not an internal HTTP-request or dollar cap. Server crashes leave requests pending for manual reconciliation."
                                                        local?
                                                        "Local Runs receive pinned workflow inputs only; benchmarks receive clean fixtures without checker/gold. One admission per cell. Run the local driver per queued cell and check Job completion; an empty queue is not completion. Nothing auto-merges or auto-funds."
                                                        :else "Ordinary Runs inherit room files. Frozen source and input changes appear in the proposed diff. Completion does not prove correctness. Allowance is never provisioned automatically."))
                                                (el/div {:class "workflow-toolbar"}
                                                        (el/button {:disabled (or (:submit loading) uncertain? (editor/dirty? draft) (not (get-in source [:inspection :ok])))
                                                                    :on-click (fn [_] ((:submit actions) selected false))} "Confirm and submit")
                                                        (when submission
                                                          (el/button {:disabled (:submit loading) :on-click (fn [_] ((:submit actions) selected true))}
                                                                     "Retry identical request"))
                                                        (when (:fund? permissions)
                                                          (el/button {:disabled (:fund loading) :on-click (fn [_] ((:fund actions) selected))}
                                                                     (if funded-here? "Provision more allowance…" "Provision allowance…"))))
                                                (when uncertain?
                                                  (el/div {:class "workflow-note"}
                                                          (el/p {} "The submission receipt is unknown. Retry uses the original immutable payload, even if this form changed.")
                                                          (el/button {:on-click (fn [_]
                                                                                  #?(:cljs (when (js/confirm "Start a different request? The previous request may still be running; this does not cancel it and could create additional work.")
                                                                                             (change dissoc :submission)) :clj nil))}
                                                                     "Start a different request…")))
                                                (when submission
                                                  (el/p {} (str "Request: " (get-in submission [:payload :request-id]))))
                                                (when-let [reply (:reply submission)]
                                                  (el/div {}
                                                          (el/p {} (str "Admission: " (:state reply)))
                                                          (when-let [job (:job reply)]
                                                            (el/button {:on-click (fn [_] ((:open-run actions) job))} "Open Run and results"))))
                                                (when funded-here? (el/p {} "Execution allowance provisioned for this backend. This is not a payment or evidence of correctness.")))))))))))
