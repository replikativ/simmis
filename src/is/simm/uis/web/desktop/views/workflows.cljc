(ns is.simm.uis.web.desktop.views.workflows
  "Room-native source editing and explicit bounded admission."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.workflow-editor :as editor]
            #?(:cljs [is.simm.uis.web.desktop.workflow-actions :as actions])
            #?(:cljs [is.simm.uis.web.desktop.memory-sync :as sync])
            #?(:cljs [is.simm.uis.web.desktop.signals :as sig])
            #?(:cljs [org.replikativ.spindel.effects.track :refer [track]])
            #?(:cljs [org.replikativ.spindel.incremental.interval :as iv])
            #?(:cljs [is.simm.uis.web.desktop.source-editor :as source-editor])
            #?(:cljs [is.simm.uis.web.desktop.views.mermaid :as mmd])
            [is.simm.uis.web.desktop.disclosure :as disclosure]
            [clojure.string :as str])
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]])))

(defn- admission-message [reply]
  (case (:status reply)
    :empty "No new knowledge to process."
    :held (str (case (:batch-state reply)
                 :capturing "An input capture is unresolved."
                 :ready "A prepared input batch is held."
                 :submitted "An input batch has already been admitted; inspect its Run status."
                 :review "An input batch is waiting for review."
                 "This workflow has an active input batch.")
               (when (uuid? (:batch reply)) (str " Batch " (subs (str (:batch reply)) 0 8) ".")))
    (str "Run: " (let [state (or (:state reply) (:status reply) :submitted)]
                    (if (keyword? state) (name state) state)))))

(defn- field [label value change]
  (el/label {:key label :class "workflow-field"}
            (el/span {} label)
            (el/input {:value (or value "") :on-input (fn [e] (change #?(:cljs (.. e -target -value) :clj "")))})))

(defn- native-model-field [state form set-form]
  (let [rows (when (= :ready (:readiness-status state)) (get-in state [:readiness :models]))
        selected (:model form "") known? (some #(= selected (:id %)) rows)]
    (el/label {:class "workflow-field"}
              (el/span {} "Model · exact version")
              (el/select {:value selected :on-change (fn [event] (set-form :model #?(:cljs (.. event -target -value) :clj selected)))}
                         (el/option {:value ""} "Choose a model…")
                         (when (and (seq selected) (not known?))
                           (el/option {:value selected :disabled true} (str selected " · not in the current options")))
                         (for [{:keys [id label provider-label credential-label]} rows]
                           (el/option {:key id :value id} (str label " · " provider-label " · " credential-label))))
              (when (not= :ready (:readiness-status state))
                (el/small {} "Model options are being verified or unavailable. Your selection is preserved."))
              (when (get-in state [:readiness :models-truncated?])
                (el/small {} "Only the first 256 registered model options are shown.")))))

(defn- benchmark-model-fields [state form set-form]
  (let [models (let [selected (editor/benchmark-models form)] (if (seq selected) selected [""]))
        rows (when (= :ready (:readiness-status state)) (get-in state [:readiness :models]))]
    (el/fieldset {:class "workflow-model-candidates"}
      (el/legend {} "Model candidates · choose up to three")
      (for [[index selected] (map-indexed vector models)]
        (el/div {:key index :class "workflow-toolbar"}
          (el/label {:class "workflow-field"}
            (el/span {} (str "Candidate " (inc index)))
            (el/select {:value selected :aria-label (str "Candidate " (inc index) " model")
                        :on-change (fn [event]
                                     (set-form :models (assoc (vec models) index #?(:cljs (.. event -target -value) :clj selected))))}
              (el/option {:value ""} "Choose a model…")
              (when (and (seq selected) (not (some #(= selected (:id %)) rows)))
                (el/option {:value selected :disabled true} (str selected " · saved choice; not currently verified")))
              (for [{:keys [id label provider-label credential-label]} rows]
                (el/option {:key id :value id
                            :disabled (some #(and (not= index (first %)) (= id (second %))) (map-indexed vector models))}
                  (str label " · " provider-label " · " credential-label)))))
          (when (> (count models) 1)
            (el/button {:type "button" :aria-label (str "Remove candidate " (inc index))
                        :on-click (fn [_] (set-form :models (vec (concat (subvec (vec models) 0 index)
                                                                      (subvec (vec models) (inc index))))))}
              "Remove"))))
      (when (< (count models) 3)
        (el/button {:type "button" :disabled (some str/blank? models)
                    :on-click (fn [_] (set-form :models (conj (vec models) "")))} "Add model candidate"))
      (when (not= :ready (:readiness-status state))
        (el/p {:role "status"} "Verifying model options. Previous credential observations are not current."))
      (el/p {:class "workflow-note"}
        "This benchmark compares models on the same saved workflow and cases. Comparing different workflow revisions is not available here yet."))))

(defn- shared-allowance [state]
  (let [allowance (get-in state [:readiness :allowance])]
    (el/div {:class "workflow-note" :role "status"}
            (el/strong {} "Shared room allowance · ")
            (cond
              (= :checking (:readiness-status state)) (el/span {} "Checking current allowance…")
              (and (= :ready (:readiness-status state)) (= :observed (:status allowance)))
              (el/span {} (str (:microdollars allowance) " microdollars and " (:model-dispatches allowance) " model requests available at the last observation."))
              (and (= :ready (:readiness-status state)) (= :unavailable (:status allowance)))
              (el/span {} "Unavailable until the room and its wallet are active. This check does not add allowance.")
              :else (el/span {} "Current allowance could not be verified. This check does not add allowance."))
            (el/p {} "Shared by room work; separate from each Run’s limits below. Admission rechecks availability. A credential is not an allowance or a verified provider connection."))))

(defn- activation-status [state actions workflow]
  (let [rows (editor/workflow-activations state (:room-id actions) workflow)
        draft (get-in state [:drafts workflow])
        revision (when (and draft (editor/source-current? draft)) (get-in draft [:source :revision]))]
    (el/div {:class "workflow-activation-status" :aria-label (str workflow " schedules")}
            (cond
              (= :unavailable (:activation-status state))
              (el/p {:role "status"} "Schedule status unavailable. No settings were changed.")
              (nil? rows) (el/p {:role "status"} "Checking schedules…")
              (empty? rows) (el/p {} "Not scheduled in this room. Choose a model and finite limits below to configure a schedule.")
              :else
              (el/div {}
                      (for [row rows]
                        (let [latest (first (sort-by :intended-at > (:occurrences row)))]
                          (el/div {:key (:id row)}
                                  (el/p {} (str (editor/activation-label row)
                                                (when (= :benchmark (:kind row)) " · benchmark")
                                                (when (and revision (:source-revision row)
                                                           (not= revision (:source-revision row))) " · earlier source revision")))
                                  (when-let [label (editor/batch-label (:batch latest))]
                                    (el/p {:class "workflow-note"} label))
                                  (when-let [job (:job latest)]
                                    (el/button {:on-click (fn [_] ((:open-run actions) job))} "Latest Run"))
                                  (when-let [proposal (get-in latest [:filing :proposal])]
                                    (el/button {:on-click (fn [_] ((:open-proposal actions) proposal))} "Review proposal")))))
                      (el/button {:on-click (fn [_] ((:open-schedules actions)))} "Manage in Workflows"))))))

(defn- input-check-view [check state actions]
  (when check
    (el/div {}
          (el/strong {} "Your next batch input check · ")
          (case (:status check)
            :fits "Input fits current limits. Admission checks still apply."
            :empty "No pending inputs to check."
            :not-checked "Not checked against this observation."
            :unavailable "Input check unavailable."
            :held (case (:code check)
                    :context-exceeds-budget "Context exceeds its file or byte limit."
                    :no-trigger-capacity "Workflow and context inputs leave no room for a source file."
                    :head-item-exceeds-budget "The first pending file exceeds its file or batch byte limit."
                    :execution-envelope-exceeds-budget "The first file, context, manifest and workflow inputs exceed the total byte limit.")
            "Input check unavailable.")
          (when (#{:fits :held} (:status check))
            (el/details {:open (boolean (get-in state [:drafts (:selected state) :input-counts-open?]))
                         :on-toggle (fn [event]
                                      (disclosure/remember-open! event
                                        #((:change actions) (:selected state) assoc :input-counts-open? %)))}
                        (el/summary {} "Input counts and limits")
                        (el/p {} (str "Context: " (get-in check [:context :files]) "/" (get-in check [:context :file-limit])
                                     " files, " (get-in check [:context :bytes]) "/" (get-in check [:context :byte-limit]) " bytes. "
                                     "First pending file: " (get-in check [:head :bytes]) " bytes; per-file limit "
                                     (get-in check [:head :file-limit]) ", batch limit " (get-in check [:head :batch-byte-limit]) ". "
                                     "Workflow inputs reserve " (get-in check [:reserved :files]) " files and "
                                     (get-in check [:reserved :bytes]) " bytes. Total limits: "
                                     (get-in check [:limits :files]) " files, " (get-in check [:limits :bytes]) " bytes.")))))))

(defn- stream-status [state actions]
  (let [request (editor/stream-request state)
        observed (:workflow-stream state)
        reply (when (= request (:request observed)) (:reply observed))]
    (when (and request (not= :not-enabled (:status reply)))
      (el/div {:class "workflow-note" :role "status"}
              (el/strong {} "Your saved workflow’s stream · ")
              (cond
                (= :checking (:workflow-stream-status state)) (el/span {} "Checking recorded inputs…")
                (= :unavailable (:workflow-stream-status state)) (el/span {} "Status unavailable. No inputs were collected.")
                (= :not-discovered (:status reply)) (el/span {} "Not discovered yet. Reading this status does not collect evidence.")
                (= :observed (:status reply))
                (el/span {}
                         (str (get-in reply [:pending :count]) " pending source versions in the recorded journal")
                         (when (not= :active (:lifecycle reply)) (str " · " (name (:lifecycle reply)))))
                :else (el/span {} "Awaiting a saved-source observation."))
              (when (and (= :ready (:workflow-stream-status state)) (= :observed (:status reply)))
                (el/p {} (case (get-in reply [:freshness :status])
                           :current "Discovery matches the observed Memory snapshot."
                           :changed "Memory has changed since discovery; changes may include unrelated files."
                           "Cannot verify whether discovery is current.")
                        " Counts cover the pending journal, excluding claimed items; they do not establish that another batch can run. "
                        (if (= :none (get-in reply [:batch :status]))
                          "No active batch recorded."
                          (editor/batch-label (:batch reply)))))
              (when (and (= :ready (:workflow-stream-status state)) (= :observed (:status reply)))
                (input-check-view (:input-check reply) state actions))))))

(defn view [room-name state actions]
  (let [selected (:selected state) draft (get-in state [:drafts selected])
        source (:source draft) form (:form draft)
        permissions (:permissions state)
        loading (reduce (fn [out slot] (assoc out slot (boolean (get-in state [:loading [slot selected]]))))
                        {:list (boolean (get-in state [:loading :list]))} [:source :save :submit :fund :schedule :output])
        write? (:write? permissions) merge? (:merge? permissions)
        benchmark? (= "benchmark" (:mode form))
        display-cases (if benchmark? (editor/benchmark-cases (:inspection source) form)
                          (get-in source [:inspection :cases]))
        local? (editor/local? form)
        model-selected? (or local? (if benchmark? (and (seq (editor/benchmark-models form))
                                                                            (every? #(not (str/blank? %)) (editor/benchmark-models form)))
                                                     (not (str/blank? (:model form)))))
        setup-card-visible? (contains? #{:missing :installed :customized :pending :upgrade-available :changed} (get-in state [:wiki-setup :status]))
        cli? (editor/cli? form)
        funded-here? (and (:funded? draft) (= (:fund-backend draft "hosted") (:backend form "hosted")))
        change #(apply (:change actions) selected %&)
        set-form (fn [key value] (change update :form editor/set-form-field key value))
        submission (:submission draft)
        wiki-output (editor/collected-output draft)
        uncertain? (and submission (nil? (:reply submission)))]
    (el/div {:class "workflow-panel"}
            (el/div {:class "workflow-toolbar"}
                    (el/h2 {} (str room-name " · Workflows"))
                    (when (seq (:errors state))
                      (el/button {:on-click (fn [_] ((:refresh actions))) :disabled (:list loading)} "Retry updates")))
            (el/p {:class "workflow-note"} "A workflow is a reusable task saved in this room. Each Run executes it separately and produces results or a proposal to review. Ordinary chat does not require a workflow.")
            (el/p {:class "workflow-note"} "Ask an agent to create or improve a workflow. Its instructions and code remain ordinary versioned room files; you can inspect or edit them below.")
            (let [{:keys [status observed-status]} (:wiki-setup state)]
              (when (and setup-card-visible? (or (nil? (:canonical-workflow actions)) (= "maintain-wiki" (:canonical-workflow actions))))
                (el/div {:class "workflow-note"}
                        (el/strong {} "Maintain knowledge · ")
                        (case status
                          :missing (el/button {:on-click (fn [_] ((:install-wiki-setup actions)))} "Set up wiki maintenance")
                          :installed (el/span {} "Starter workflow installed. ")
                          :customized (el/span {} "Customized workflow preserved. ")
                          :upgrade-available
                          (el/span {}
                                   (el/button {:on-click (fn [_] ((:upgrade-wiki-setup actions)))}
                                              (str "Upgrade starter v" (get-in state [:wiki-setup :installed-version])
                                                   " → v" (get-in state [:wiki-setup :shipped-version])))
                                   " Only the unchanged bundled starter is upgraded. Existing schedules are not upgraded; review and redeploy them separately. ")
                          :changed (el/span {} "Saved starter changed. Inspect it again before upgrading. ")
                          :pending (el/span {} "Setup outcome is uncertain. Checking saved source does not repeat installation. "))
                        (when (or (#{:installed :customized :upgrade-available} status) (#{:installed :customized :upgrade-available} observed-status))
                          (el/button {:on-click (fn [_] ((:select actions) "maintain-wiki"))} "Open workflow"))
                        (when (#{:pending :changed} status)
                          (el/button {:on-click (fn [_] ((:inspect-wiki-setup actions)))
                                      :disabled (get-in state [:loading :wiki-setup-inspect])} "Check saved setup"))
                        (activation-status state actions "maintain-wiki")
                        (el/p {} "The starter uses this room’s inbox, transcripts and captures as evidence and wiki files as context, then proposes changes for review. Chat is not automatically filed. Inspect customized source before use. Setup does not choose a model, add allowance or enable a schedule."))))
            (for [[slot error] (:errors state)]
              (el/div {:key (pr-str slot) :class "proposal-error" :role "alert"}
                      (str (when (vector? slot) (str (second slot) ": ")) error)))
            (el/div {:class "workflow-layout"}
                    (el/div {:class "workflow-list"}
                            (when-not (:list state) (el/p {} "Loading workflows…"))
                            (for [name (sort (set (concat (get-in state [:list :names]) (keys (:drafts state)))))
                                  :when (or (nil? (:canonical-workflow actions)) (= name (:canonical-workflow actions)))]
                              (el/button {:key name :class (when (= name selected) "workflow-selected")
                                          :on-click (fn [_] ((:select actions) name))} name))
                            (when (get-in state [:list :next-offset])
                              (el/button {:on-click (fn [_] ((:more actions))) :disabled (:list loading)} "More workflows"))
                            (when (and write? (nil? (:canonical-workflow actions)))
                              (el/details {:class "workflow-new workflow-disclosure" :open (boolean (:new-authoring-open? state))
                                                   :on-toggle #(disclosure/remember-open! % (:new-authoring-open actions))}
                                      (el/summary {} "Create from code")
                                      (field "New workflow name" (:new-name state) (:new-name actions))
                                      (el/button {:on-click (fn [_] ((:create actions))) :disabled (:source loading)} "Create workflow"))))
                    (el/div {:class "workflow-detail"}
                            (cond
                              (:source loading) (el/p {} "Loading source…")
                              (nil? source) (el/p {} "Select a saved workflow. Ask an agent in the room to create one, or open the manual authoring tools.")
                              :else
                              (el/div {}
                                      (el/h3 {} (or (get-in source [:definition :title]) selected))
                                      (el/p {:class "workflow-note"} "Reusable work prepared in chat. Run it, schedule it, or inspect its results here.")
                                      (let [problems (if-not (get-in source [:inspection :ok])
                                                       (get-in source [:inspection :problems])
                                                       (when (and benchmark? (false? (get-in source [:inspection :benchmark-ready?])))
                                                         (get-in source [:inspection :benchmark-problems])))]
                                        (when (seq problems)
                                          (el/div {:class "workflow-readiness-summary" :role "status"}
                                            (el/strong {} (if (get-in source [:inspection :ok]) "Benchmark setup needed" "Workflow setup needed"))
                                            (for [problem problems] (el/p {:key problem} problem))
                                            (el/p {} "Ask the room’s agent to repair this setup, then return here."))))
                                      (when merge?
                                        (el/div {:class "workflow-toolbar" :aria-label "Workflow actions"}
                                          (el/button {:on-click (fn [_] (change editor/choose-action :run))} "Run once")
                                          (el/button {:on-click (fn [_] (change editor/choose-action :schedule))} "Schedule")
                                          (when (:operator? permissions)
                                            (el/button {:on-click (fn [_] (change editor/choose-action :benchmark))} "Benchmark"))))
                                      (when (editor/dirty? draft) (el/p {:role "status"} "Unsaved code draft · executions use the saved version."))
                                      (when-not (editor/source-current? draft)
                                        (el/p {:role "status"} "Saved source is unverified or changed. Your draft is preserved; reconcile it before starting work."))
                                      (when-let [diagram (get-in source [:files "diagram.mmd"])]
                                        (el/div {:class "workflow-overview-diagram"}
                                                (el/p {:class "workflow-note"} "Workflow overview · saved version")
                                                (if (and (string? diagram) (<= (count diagram) 32000))
                                                  #?(:cljs (mmd/diagram diagram) :clj (el/pre {} diagram))
                                                  (el/p {} "Inspect the diagram in Instructions and code."))))
                                      (when (:open-room actions)
                                        (el/button {:on-click (fn [_] ((:open-room actions)))} "Discuss in room"))
                                      (when-not (and (= "maintain-wiki" selected) setup-card-visible?)
                                        (activation-status state actions selected))
                                      (stream-status state actions)
                                      (el/details {:class "workflow-disclosure" :open (boolean (:source-open? draft))
                                                   :on-toggle (fn [event]
                                                                (disclosure/remember-open! event #(change assoc :source-open? %)))}
                                                  (el/summary {} "Instructions and code"
                                                              (when (editor/dirty? draft) (el/span {:class "workflow-draft-label"} " · Unsaved draft")))
                                      (el/div {:class "workflow-toolbar"}
                                              (el/span {} (if (editor/dirty? draft) "Unsaved changes" "Saved source"))
                                              (when-not (editor/source-current? draft)
                                                (el/span {:role "status"}
                                                         (if (:incoming-source draft)
                                                           "Saved source changed. Your draft is preserved; reconcile before launching."
                                                           "Verifying saved source…")))
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
                                                                #?(:cljs
                                                                   (source-editor/view
                                                                    {:id [(:owner actions) selected path] :path path :label "Workflow file source"
                                                                     :value (get-in draft [:files path] "")
                                                                     :selection (get-in draft [:editor-selections path])
                                                                     :read-only? (not write?)
                                                                     :on-change (fn [text selection]
                                                                                  (change #(-> %
                                                                                               (assoc-in [:files path] text)
                                                                                               (assoc-in [:editor-selections path] selection))))})
                                                                   :clj (el/pre {} (get-in draft [:files path] "")))))))
                                      (el/div {:class "workflow-inspection"}
                                              (el/strong {} (if (get-in source [:inspection :ok]) "Program structure valid · not benchmark-tested" "Draft needs repair"))
                                              (when (editor/dirty? draft) (el/p {} "Inspection below is of the saved source. Save before launching."))
                                              (for [problem (get-in source [:inspection :problems])]
                                                (el/p {:key problem} (str problem)))
                                              (when (and benchmark? (false? (get-in source [:inspection :benchmark-ready?])))
                                                (el/div {:role "status"}
                                                  (el/strong {} "Benchmark setup needed")
                                                  (for [problem (get-in source [:inspection :benchmark-problems])]
                                                    (el/p {:key problem} problem))))
                                              (el/p {} (str "Source revision: " (:revision source)))
                                              (when-let [bundle (get-in source [:inspection :bundle])]
                                                (el/p {} (str "Execution bundle: " bundle))))
                                      )
                                      (when merge?
                                        (el/div {:class "workflow-launch"}
                                                (el/details {:class "workflow-disclosure" :open (boolean (:execution-open? draft))
                                                             :on-toggle (fn [event]
                                                                          (disclosure/remember-open! event #(change assoc :execution-open? %)))}
                                                            (el/summary {} (if benchmark? "Benchmark setup" "Run and schedule setup"))
                                                (el/p {:class "workflow-note"} "Configure an explicit execution of the saved code. Opening this section never starts work.")
                                                (when (and benchmark? (not (:operator? permissions)))
                                                  (el/p {:role "status"} "Benchmark execution currently requires installation operator access and room merge permission."))
                                                (when (contains? (:files source) "execution-origin.edn")
                                                  (el/p {:class "workflow-note"}
                                                        "This workflow reads a retained repository checkpoint on This Simmis server. Capture output and discard work retains wiki proposals for review. CLI and local-worker execution are unsupported."))
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
                                                  (if cli?
                                                    (field "Model id (Claude Code text)" (:model form) #(set-form :model %))
                                                    (el/div {} (if benchmark? (benchmark-model-fields state form set-form)
                                                                               (native-model-field state form set-form))
                                                               (shared-allowance state))))
                                                (el/label {:class "workflow-field"}
                                                          (el/span {} "Mode")
                                                          (el/select {:value (:mode form) :on-change (fn [e] (set-form :mode #?(:cljs (.. e -target -value) :clj "run")))}
                                                                     (when-not cli? (el/option {:value "run"} "Ordinary Run"))
                                                                     (when (:operator? permissions) (el/option {:value "benchmark"} "Benchmark"))))
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
                                                          (field "Independent trials per case (1–2; at most 8 total)" (:repetitions form) #(set-form :repetitions %))
                                                          (el/p {:class "workflow-note"} "Benchmark repetitions compare independent executions. They are not an agent's reasoning iterations.")))
                                                (el/h4 {} "Resources")
                                                (for [[key label] (cond
                                                                    cli? [[:cli-runs "Allowance · single-use CLI Runs (1–8)"]]
                                                                    local? [[:worker-admissions "Allowance · local admissions (1–32)"]]
                                                                    :else [[:budget-microdollars "Spend ceiling · microdollars (1–5000000)"]])]
                                                  (field label (get form key) #(set-form key %)))
                                                (when (and (not cli?) (not local?))
                                                  (el/p {:class "workflow-note"} "1,000,000 microdollars = $1. This caps requested spend; it is not a charge. Provisioning allowance is a separate, explicit action."))
                                                (el/div {:class "workflow-execution-limits"}
                                                        (el/button {:type "button" :aria-expanded (str (boolean (:show-execution-limits? draft)))
                                                                    :on-click (fn [_] (change update :show-execution-limits? not))}
                                                                   (str (if (:show-execution-limits? draft) "▾ " "▸ ") "Advanced execution ceilings"))
                                                        (when (:show-execution-limits? draft)
                                                          (el/div {}
                                                                  (el/p {:class "workflow-note"} "Upper bounds, not a prescribed number of agents or reasoning iterations. The harness may finish earlier; these limits do not allocate a fixed agent team.")
                                                                  (for [[key label] (cond
                                                                                      cli? [[:max-model-steps "Maximum model steps per trial (1–32)"]
                                                                                            [:timeout-ms "Wall-time ceiling per trial · milliseconds (1000–600000)"]]
                                                                                      local? [[:timeout-ms "Wall-time ceiling · milliseconds (1000–600000)"]]
                                                                                      :else [[:max-dispatches "Model-request allowance (1–32)"]
                                                                                             [:max-model-steps "Maximum model steps (1–32)"]
                                                                                             [:timeout-ms "Wall-time ceiling · milliseconds (1000–600000)"]])]
                                                                    (field label (get form key) #(set-form key %))))))
                                                (when-not benchmark?
                                                  (el/label {:class "workflow-field"}
                                                            (el/span {} "Results")
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
                                                        (el/button {:disabled (or (not model-selected?) (:submit loading) uncertain? (editor/dirty? draft) (not (editor/source-current? draft)) (not (get-in source [:inspection :ok]))
                                                                                 (and benchmark? (or (not (:operator? permissions))
                                                                                                    (false? (get-in source [:inspection :benchmark-ready?])))))
                                                                    :on-click (fn [_] ((:submit actions) selected false))} (if benchmark? "Run benchmark…" "Run once…"))
                                                        )
                                                (when submission
                                                          (el/button {:disabled (:submit loading) :on-click (fn [_] ((:submit actions) selected true))}
                                                                     "Retry identical request"))
                                                        (when (:fund? permissions)
                                                          (el/button {:disabled (:fund loading) :on-click (fn [_] ((:fund actions) selected))}
                                                                     (if funded-here? "Provision more allowance…" "Provision allowance…")))
                                                (when (and (not local?) (not cli?) (:schedule-open? draft))
                                                  (el/div {:class "workflow-scheduling"}
                                                          (el/h4 {} "Schedule")
                                                          (el/label {:class "workflow-field"}
                                                            (el/span {} "Repeat")
                                                            (el/select {:value (:schedule-cadence form "interval")
                                                                        :on-change (fn [event] (set-form :schedule-cadence #?(:cljs (.. event -target -value) :clj "interval")))}
                                                              (el/option {:value "day"} "Daily")
                                                              (el/option {:value "week"} "Weekly")
                                                              (el/option {:value "month"} "Monthly")
                                                              (el/option {:value "interval"} "Fixed interval")))
                                                          (if (= "interval" (:schedule-cadence form "interval"))
                                                            (field "Interval · minutes (1–525600)" (:schedule-minutes form) #(set-form :schedule-minutes %))
                                                            (el/div {}
                                                              (field "Time · 24-hour HH:MM" (:schedule-at form) #(set-form :schedule-at %))
                                                              (when (= "week" (:schedule-cadence form))
                                                                (el/label {:class "workflow-field"}
                                                                  (el/span {} "Day")
                                                                  (el/select {:value (:schedule-on form)
                                                                              :on-change (fn [event] (set-form :schedule-on #?(:cljs (.. event -target -value) :clj "monday")))}
                                                                    (for [day ["monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday"]]
                                                                      (el/option {:key day :value day} (str/capitalize day))))))
                                                              (when (= "month" (:schedule-cadence form))
                                                                (field "Day of month · 1–28" (:schedule-day form) #(set-form :schedule-day %)))))
                                                          (field "Timezone (e.g. Europe/Berlin)" (:schedule-tz form) #(set-form :schedule-tz %))
                                                          (when-let [{:keys [pending? payload]} (editor/deployment-preview draft)]
                                                            (let [execution (:execution payload)
                                                                  description (editor/deployment-description payload)]
                                                              (el/div {:class "workflow-note" :role "status"}
                                                                      (el/strong {} (if pending?
                                                                                     (if (false? (:enabled? payload)) "Pending paused schedule · " "Pending activation · ")
                                                                                     "Next schedule · "))
                                                                      (el/span {} (str (:model description) " · " (editor/cadence-label (:cadence payload))))
                                                                      (el/p {} (str "Saved revision " (:expected-owned-source execution)))
                                                                      (el/p {} (:resources description))
                                                                      (el/p {} (str (:execution-ceilings description) " " (:results description))))))
                                                          (el/p {:class "workflow-note"} "Pins saved source and the execution settings above. Uses the shared room allowance; no automatic funding or merging. Overlapping work is skipped. Manage activations and inspect occurrences in Workflows.")
                                                          (el/button {:disabled (or (nil? (editor/deployment-preview draft)) (:schedule loading) (editor/dirty? draft) (not (editor/source-current? draft))
                                                                                    (not (get-in source [:inspection :ok]))
                                                                                    (and (:schedule-submission draft)
                                                                                         (nil? (get-in draft [:schedule-submission :reply]))))
                                                                      :on-click (fn [_] ((:schedule actions) selected false false))}
                                                                     "Save paused schedule")
                                                          (el/button {:disabled (or (nil? (editor/deployment-preview draft)) (:schedule loading) (editor/dirty? draft) (not (editor/source-current? draft))
                                                                                    (not (get-in source [:inspection :ok]))
                                                                                    (and (:schedule-submission draft)
                                                                                         (nil? (get-in draft [:schedule-submission :reply]))))
                                                                      :on-click (fn [_] ((:schedule actions) selected false))}
                                                                     "Confirm and activate schedule…")
                                                          (when (:schedule-submission draft)
                                                            (el/button {:disabled (:schedule loading)
                                                                        :on-click (fn [_] ((:schedule actions) selected true))}
                                                                       "Retry identical schedule request"))
                                                          (when-let [reply (get-in draft [:schedule-submission :reply])]
                                                            (el/div {}
                                                                    (el/p {} (str "Saved schedule: " (:id reply)
                                                                                  ". Acknowledged as " (case (:enabled? reply) true "enabled" false "paused" "state not reported")
                                                                                  ". Current status and occurrences are in Workflows."))
                                                                    (el/button {:on-click (fn [_] ((:open-schedules actions)))} "Open Workflows"))))))
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
                                                          (el/p {:role "status"} (admission-message reply))
                                                          (when-let [job (:job reply)]
                                                            (el/button {:on-click (fn [_] ((:open-run actions) job))} "Open Run and results"))
                                                          (when (and (= "benchmark" (:mode submission)) (:open-leaderboard actions))
                                                            (el/button {:on-click (fn [_] ((:open-leaderboard actions)))} "Open benchmark leaderboard"))))
                                                (when (and merge? (editor/collectable-output-request draft))
                                                  (el/div {:class "workflow-output-review"}
                                                          (el/p {:class "workflow-note"}
                                                                "After the Run completes, collect its mechanically checked wiki output for human review. This never reruns inference or accepts files.")
                                                          (el/button {:disabled (:output loading)
                                                                      :on-click (fn [_] ((:collect-output actions) selected))}
                                                                     (if (:output loading) "Collecting wiki output…" "Review wiki output"))
                                                          (case (:status wiki-output)
                                                            :proposed (el/button {:on-click (fn [_] ((:open-proposal actions) (:proposal wiki-output)))} "Open wiki proposal")
                                                            :no-op (el/p {:role "status"} "No wiki changes were proposed.")
                                                            :unavailable (el/p {:role "status"}
                                                                               "Output is not available for review. Check that the Run completed and passed its checks, and that you still have permission. Retrying collection does not rerun the workflow.")
                                                            nil)))
                                                (when funded-here? (el/p {} "Allowance was provisioned for this backend. This receipt is not its current balance, a payment or evidence of correctness.")))))))))))

#?(:cljs
   (defn render-workflows [room-data]
     (spin
        ;; All tracks precede interval consumption; resumes cannot skip a
        ;; dependency or reconsume a preceding incremental delta.
      (let [states-i (track sig/workflow-editor-state)
            socket-i (track sig/memory-connection)
            stream-i (track sync/state)
            roster-i (track sig/user-rooms)
            states (iv/get-new states-i)
            _socket (iv/get-new socket-i)
            _stream (iv/get-new stream-i)
            _roster (iv/get-new roster-i)
            controller (actions/controller room-data)
            state (get states (actions/state-key (:room-id room-data) (or (:workflow-state-owner room-data) (:tab-id room-data))))]
        ((:observe controller))
        (if (:access-denied? state)
          (el/p {:role "status"} "This room is no longer available to this account.")
          (view (editor/room-label room-data _roster)
                (cond-> state (sync/problem _stream) (assoc-in [:errors :stream] (sync/problem _stream)))
                controller))))))
