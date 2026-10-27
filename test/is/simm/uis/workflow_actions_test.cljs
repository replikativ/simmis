(ns is.simm.uis.workflow-actions-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.render :as render]
            [is.simm.runtimes.web :as web]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.workflow-actions :as actions]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.memory-remote :as memory-remote]
            [is.simm.uis.web.desktop.workflow-editor :as editor]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [is.simm.uis.web.desktop.views.workflows :as view])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(defn source [name revision]
  {:name name :revision revision :files {"workflow.edn" "{}"}
   :definition {:params {:goal "old"}}
   :inspection {:ok true :bundle "bundle" :cases []}})

(def initial-revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d")

(deftest knowledge-card-renders-blocked-schedules-without-configuring-anything
  (async done
    (let [ctx (context/fork-context runtime/runtime) container (.createElement js/document "div")
          handle (atom nil) invoked (atom [])
          state {:wiki-setup {:status :customized} :activation-status :ready
                 :activations [{:id "legacy" :room-id "room" :name "maintain-wiki" :enabled? true
                                :resume-blocked :legacy-recreate}
                               {:id "held" :room-id "room" :name "maintain-wiki" :enabled? false
                                :resume-blocked :reconciliation-required}]}
          observer (js/MutationObserver.
                    (fn [_ observer]
                      (when (.includes (.-textContent container) "Needs recreation")
                        (.disconnect observer)
                        (try
                          (is (.includes (.-textContent container) "admission requires reconciliation"))
                          (is (.includes (.-textContent container) "The starter uses"))
                          (is (.includes (.-textContent container) "Inspect customized source"))
                          (is (not-any? #(= "Active" (.-textContent %))
                                        (array-seq (.querySelectorAll container ".workflow-activation-status p"))))
                          (is (empty? @invoked))
                          (finally
                            (when-let [stop (:stop! @handle)] (stop))
                            (.remove container) (context/close-context! ctx) (done))))))]
      (set! (.. container -style -display) "none")
      (.appendChild (.-body js/document) container)
      (.observe observer container #js {:childList true :subtree true})
      (binding [ec/*execution-context* ctx]
        (reset! handle (render/render-spin! container
                                           (spin (view/view "Room" state {:room-id "room"
                                                                         :open-schedules #(swap! invoked conj :schedule)
                                                                         :select #(swap! invoked conj %)}))
                                           (browser/make-dom-discharge js/document)))))))

(defn deferred [requests]
  (fn [& args]
    (fn [ok error] (swap! requests conj {:ok ok :error error :args args}))))

(defn with-state [f]
  (let [ctx (context/fork-context runtime/runtime)
        session {:id "workflow-ui-fixture"}]
    (try
      (with-redefs [runtime/runtime ctx
                    web/current-session (constantly session)
                    web/current-session? #(= session %)
                    connection/snapshot (constantly {:socket :fixture})
                    remote/inspect-wiki-maintenance-setup!
                    (fn [& _] (fn [ok _] (ok {:status :unavailable})))
                    remote/load-workflow-schedules!
                    (fn [& _] (fn [ok _] (ok [])))
                    remote/load-workflow-readiness!
                    (fn [& _] (fn [ok _] (ok {:models [] :models-truncated? false
                                              :allowance {:status :unavailable :reason :room-not-active}})))
                    actions/confirm! (constantly true)]
        (binding [ec/*execution-context* ctx]
          (reset! sig/workflow-editor-state {})
          (reset! sig/user-rooms nil)
          (reset! sync/state nil)
          (let [k (actions/state-key "room")]
            (swap! sig/workflow-editor-state assoc k
                   {:selected "a" :drafts {"a" (editor/draft (source "a" initial-revision))
                                           "b" (editor/draft (source "b" initial-revision))}})
            (f (actions/controller {:room-id "room"}) k))))
      (finally (context/close-context! ctx)))))

(deftest stale-controller-cannot-dispatch-through-a-replacement-account
  (with-state
    (fn [ui k]
      (let [requests (atom []) before @sig/workflow-editor-state]
        (with-redefs [web/current-session? (constantly false)
                      remote/load-workflow-source! (deferred requests)
                      remote/save-workflow-source! (deferred requests)]
          ((:reload ui) "a")
          ((:save ui) "a")
          (is (empty? @requests))
          (is (= before @sig/workflow-editor-state)))))))

(deftest schedule-observation-is-inert-reactive-and-cannot-restore-revoked-access
  (with-state
    (fn [ui k]
      (let [schedules (atom []) sources (atom []) lists (atom []) watches (atom []) writes (atom [])
            row {:id "activation" :room-id "room" :name "maintain-wiki" :enabled? false}]
        (with-redefs [remote/load-workflow-schedules! (deferred schedules)
                      remote/load-workflow-source! (deferred sources)
                      remote/load-workflow-list! (deferred lists)
                      memory-remote/prepare-memory-stream! (deferred watches)
                      remote/create-workflow-schedule! (deferred writes)
                      remote/provision-workflow-allowance! (deferred writes)
                      remote/install-wiki-maintenance! (deferred writes)]
          ((:change ui) "a" assoc-in [:files "workflow.edn"] "unsaved")
          ((:observe ui))
          (is (= 1 (count @schedules)))
          ((:ok (first @schedules)) [row])
          (is (= [row] (editor/workflow-activations (get @sig/workflow-editor-state k) "room" "maintain-wiki")))
          (reset! sync/state {:control-dirty 1})
          ((:observe ui))
          (is (= 2 (count @schedules)))
          (is (nil? (editor/workflow-activations (get @sig/workflow-editor-state k) "room" "maintain-wiki")))
          ;; A duplicate old reply cannot finish the replacement observation.
          ((:ok (first @schedules)) [(assoc row :enabled? true)])
          (is (= :checking (get-in @sig/workflow-editor-state [k :activation-status])))
          ((:ok (second @schedules)) [(assoc row :reason :budget-exhausted)])
          (is (= :budget-exhausted (get-in @sig/workflow-editor-state [k :activations 0 :reason])))
          (reset! sync/state {:control-dirty 2})
          ((:observe ui))
          (is (= 3 (count @schedules)))
          (reset! sig/user-rooms {:rooms []})
          ((:observe ui))
          ((:ok (last @schedules)) [row])
          (is (nil? (get-in @sig/workflow-editor-state [k :activations])))
          (is (true? (get-in @sig/workflow-editor-state [k :access-denied?])))
          (is (= "unsaved" (get-in @sig/workflow-editor-state [k :drafts "a" :files "workflow.edn"])))
          (is (empty? @writes)))))))

(deftest immediate-schedule-reply-does-not-get-stuck-checking-after-stream-registration
  (with-state
    (fn [ui k]
      (with-redefs [remote/load-workflow-list! (fn [& _] (fn [ok _] (ok {:names []})))
                    remote/load-workflow-source! (fn [& _] (fn [ok _] (ok (source "a" initial-revision))))
                    memory-remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {:watching? true})))
                    sync/ensure! (fn [& _])]
        ((:observe ui))
        ((:observe ui))
        (is (= :ready (get-in @sig/workflow-editor-state [k :activation-status])))
        (is (= :ready (get-in @sig/workflow-editor-state [k :readiness-status])))
        (is (= [] (editor/workflow-activations (get @sig/workflow-editor-state k) "room" "maintain-wiki")))))))

(deftest restored-identical-roster-fetches-schedules-again
  (doseq [immediate? [true false]]
    (with-state
      (fn [ui k]
        (let [roster {:rooms [{:room/id "room"}]} requests (atom [])
              row {:id "activation" :room-id "room" :name "maintain-wiki" :enabled? false}
              schedules (deferred requests) other (atom [])]
          (with-redefs [remote/load-workflow-list! (deferred other)
                        remote/load-workflow-source! (deferred other)
                        memory-remote/prepare-memory-stream! (deferred other)
                        remote/load-workflow-schedules!
                        (fn [& args] (let [request (apply schedules args)]
                                       (fn [ok error]
                                         (request ok error)
                                         (when immediate? (ok [row])))))]
            (reset! sig/user-rooms roster)
            ((:observe ui))
            (when-not immediate? ((:ok (first @requests)) [row]))
            (is (= [row] (editor/workflow-activations (get @sig/workflow-editor-state k) "room" "maintain-wiki")))
            (reset! sig/user-rooms {:rooms []})
            ((:observe ui))
            (is (nil? (get-in @sig/workflow-editor-state [k :activations])))
            (reset! sig/user-rooms roster)
            ((:observe ui))
            (is (= 2 (count @requests)))
            (when-not immediate? ((:ok (second @requests)) [row]))
            (is (= [row] (editor/workflow-activations (get @sig/workflow-editor-state k) "room" "maintain-wiki")))))))))

(deftest schedule-navigation-retains-controller-context-and-rejects-replacement-session
  (with-state
    (fn [ui _]
      (let [opened (atom []) expected ec/*execution-context*]
        ;; Match open-tab!'s two fixed arguments + optional options ABI:
        ;; compiled CLJS calls its variadic entry directly.
        (with-redefs [sig/open-tab! (fn [kind data & [options]]
                                     (swap! opened conj {:ctx ec/*execution-context*
                                                         :kind kind :data data :options options}))]
          (binding [ec/*execution-context* nil] ((:open-schedules ui)))
          (is (= 1 (count @opened)))
          (is (identical? expected (:ctx (first @opened))))
          (is (= :schedules (:kind (first @opened))))
          (with-redefs [web/current-session? (constantly false)] ((:open-schedules ui)))
          (is (= 1 (count @opened))))))))

(deftest own-control-hints-do-not-drop-original-command-receipts
  (doseq [[operation receipt-path reply]
          [[:submit [:submission :reply] {:state :submitted :job "job"}]
           [:schedule [:schedule-submission :reply] {:id "activation" :enabled? true}]
           [:fund [:fund-reply] {:state :granted}]]]
    (with-state
      (fn [ui k]
        (let [requests (atom [])]
          (with-redefs [remote/submit-workflow-run! (deferred requests)
                        remote/create-workflow-schedule! (deferred requests)
                        remote/provision-workflow-allowance! (deferred requests)]
            ((:change ui) "a" assoc-in [:form :model] "fixture/model")
            (case operation
              :submit ((:submit ui) "a" false)
              :schedule ((:schedule ui) "a" false)
              :fund ((:fund ui) "a"))
            (is (= 1 (count @requests)))
            (reset! sync/state {:control-dirty 1})
            ((:ok (first @requests)) (assoc reply :permissions {:merge? true}))
            (let [state (get @sig/workflow-editor-state k) draft (get-in state [:drafts "a"])]
              (is (= reply (get-in draft receipt-path)))
              (is (false? (get-in state [:loading [operation "a"]])))
              (is (nil? (:permissions state)))
              (is (not (editor/source-current? draft)))
              (when (= :fund operation) (is (true? (:funded? draft)))))))))))

(deftest wiki-filing-hint-before-ack-keeps-only-the-current-request-receipt
  (doseq [race [:own-hint :changed-roster :replacement-request]]
    (with-state
      (fn [ui k]
        (let [requests (atom []) id (str (random-uuid))
              reply {:status :proposed :request id :proposal (str (random-uuid))}]
          (with-redefs [remote/collect-workflow-wiki-output! (deferred requests)]
            ((:change ui) "a" assoc :submission
             {:mode "run" :backend "hosted" :knowledge? true
              :reply {:id id :kind :workflow :job (random-uuid)}})
            ((:collect-output ui) "a")
            (is (= 1 (count @requests)))
            (reset! sync/state {:control-dirty 1})
            (case race
              :changed-roster (reset! sig/user-rooms {:rooms []})
              :replacement-request ((:change ui) "a" assoc-in [:submission :reply :id] (str (random-uuid)))
              nil)
            ((:ok (first @requests)) (assoc reply :permissions {:merge? true}))
            (let [state (get @sig/workflow-editor-state k) draft (get-in state [:drafts "a"])]
              (is (= (when (= :own-hint race) reply) (editor/collected-output draft)) (pr-str state))
              (is (false? (get-in state [:loading [:output "a"]])))
              (is (nil? (:permissions state)))
              (is (not (editor/source-current? draft))))))))))

(deftest wiki-setup-receipt-preserves-dirty-editor-and-never-replays-uncertainty
  (doseq [outcome [:ack :lost-ack]]
    (with-state
      (fn [ui k]
        (let [requests (atom [])]
          (with-redefs [remote/install-wiki-maintenance! (deferred requests)]
            ((:change ui) "a" assoc-in [:files "workflow.edn"] "unsaved")
            (swap! sig/workflow-editor-state assoc-in [k :wiki-setup] {:status :missing})
            ((:install-wiki-setup ui))
            (is (= 1 (count @requests)))
            (reset! sync/state {:control-dirty 1})
            (case outcome
              :ack ((:ok (first @requests)) {:status :installed :name "maintain-wiki"
                                            :activation :unchanged :observation :acknowledged})
              :lost-ack ((:error (first @requests)) :disconnected))
            (let [state (get @sig/workflow-editor-state k)]
              (is (= "a" (:selected state)))
              (is (= "unsaved" (get-in state [:drafts "a" :files "workflow.edn"])))
              (is (= (if (= :ack outcome) :installed :pending) (get-in state [:wiki-setup :status]))))
            ((:install-wiki-setup ui))
            (is (= 1 (count @requests)))))))))

(deftest explicit-source-reply-cannot-restore-revoked-room-access
  (with-state
    (fn [ui k]
      (let [requests (atom [])]
        (with-redefs [remote/load-workflow-source! (deferred requests)]
          ((:reload ui) "a")
          (reset! sig/user-rooms {:rooms []})
          ((:ok (first @requests)) (assoc (source "a" "forbidden") :permissions {:write? true}))
          (is (not= "forbidden" (get-in @sig/workflow-editor-state [k :drafts "a" :source :revision])))
          (is (nil? (get-in @sig/workflow-editor-state [k :permissions])))
          (is (false? (get-in @sig/workflow-editor-state [k :loading [:source "a"]])))
          (is (false? (editor/source-current? (get-in @sig/workflow-editor-state [k :drafts "a"])))))))))

(deftest passive-observation-preserves-dirty-source-and-original-admission
  (with-state
    (fn [ui k]
      (let [sources (atom []) lists (atom []) watches (atom [])]
        (with-redefs [remote/load-workflow-source! (deferred sources)
                      remote/load-workflow-list! (deferred lists)
                      memory-remote/prepare-memory-stream! (deferred watches)]
          ((:change ui) "a" assoc :submission {:payload {:request-id :original}})
          ((:change ui) "a" assoc-in [:files "workflow.edn"] "my unsaved code")
          ((:observe ui))
          (is (false? (editor/source-current? (get-in @sig/workflow-editor-state [k :drafts "a"]))))
          (reset! sync/state {:dirty {"room" 1}})
          ((:observe ui))
          ((:ok (first @sources)) (source "a" "obsolete"))
          ((:observe ui))
          (is (= 2 (count @sources)))
          ((:ok (second @sources)) (source "a" "incoming"))
          (let [draft (get-in @sig/workflow-editor-state [k :drafts "a"])]
            (is (= "my unsaved code" (get-in draft [:files "workflow.edn"])))
            (is (= initial-revision (get-in draft [:source :revision])))
            (is (= "incoming" (get-in draft [:incoming-source :revision])))
            (is (= {:payload {:request-id :original}} (:submission draft)))))))))

(deftest registration-completion-forces-catchup-even-on-an-already-ready-topic
  (with-state
    (fn [ui k]
      (let [sources (atom []) lists (atom []) watches (atom [])]
        (with-redefs [remote/load-workflow-source! (deferred sources)
                      remote/load-workflow-list! (deferred lists)
                      memory-remote/prepare-memory-stream! (deferred watches)
                      sync/ensure! (fn [& _])]
          (reset! sync/state {:status :ready :dirty {} :control-dirty 0})
          ((:observe ui))
          ;; A commit before this room's registration may have no hint. The
          ;; successful registration itself must force a subsequent read.
          ((:ok (first @watches)) {:watching? true})
          ((:ok (first @sources)) (source "a" "before-registration"))
          (is (not= "before-registration" (get-in @sig/workflow-editor-state [k :drafts "a" :source :revision])))
          ((:observe ui))
          (is (= 2 (count @sources)))
          ((:ok (second @sources)) (source "a" "caught-up"))
          (is (= "caught-up" (get-in @sig/workflow-editor-state [k :drafts "a" :source :revision]))))))))

(deftest reconnect-unverifies-source-before-stream-registration-acknowledges
  (with-state
    (fn [ui k]
      (let [sources (atom []) lists (atom []) watches (atom [])]
        (with-redefs [remote/load-workflow-source! (deferred sources)
                      remote/load-workflow-list! (deferred lists)
                      memory-remote/prepare-memory-stream! (deferred watches)]
          ((:observe ui))
          ((:ok (first @sources)) (source "a" initial-revision))
          (is (editor/source-current? (get-in @sig/workflow-editor-state [k :drafts "a"])))
          ((:change ui) "a" assoc :submission {:payload {:request-id :original}})
          (swap! sig/workflow-editor-state assoc-in [k :loading [:submit "a"]] true)
          (with-redefs [connection/snapshot (constantly {:socket :replacement})]
            ((:observe ui))
            (is (false? (editor/source-current? (get-in @sig/workflow-editor-state [k :drafts "a"]))))
            (is (nil? (get-in @sig/workflow-editor-state [k :loading [:submit "a"]])))
            (is (= {:payload {:request-id :original}}
                   (get-in @sig/workflow-editor-state [k :drafts "a" :submission])))
            (is (= 2 (count @watches)))))))))

(deftest own-save-hint-before-acknowledgement-advances-only-the-sent-base
  (doseq [typed-after? [false true]]
    (with-state
      (fn [ui k]
        (let [sources (atom []) lists (atom []) watches (atom []) saves (atom [])]
          (with-redefs [remote/load-workflow-source! (deferred sources)
                        remote/load-workflow-list! (deferred lists)
                        memory-remote/prepare-memory-stream! (deferred watches)
                        remote/save-workflow-source! (deferred saves)]
            ((:observe ui))
            ((:ok (first @sources)) (source "a" initial-revision))
            ((:change ui) "a" assoc :submission {:payload {:request-id :original}})
            ((:change ui) "a" assoc-in [:files "workflow.edn"] "sent")
            ((:save ui) "a")
            (reset! sync/state {:dirty {"room" 1}})
            ((:observe ui))
            (when typed-after? ((:change ui) "a" assoc-in [:files "workflow.edn"] "typed after save"))
            ((:ok (first @saves)) {:workflow {:ok true :revision "saved" :bundle "bundle"}
                                   :definition {:params {:goal "old"}} :permissions {:write? true}})
            (let [draft (get-in @sig/workflow-editor-state [k :drafts "a"])]
              (is (= "saved" (get-in draft [:source :revision])))
              (is (= "sent" (get-in draft [:source :files "workflow.edn"])))
              (is (= (if typed-after? "typed after save" "sent") (get-in draft [:files "workflow.edn"])))
              (is (not (editor/source-current? draft))))
            ((:observe ui))
            ((:ok (last @sources)) (assoc (source "a" "saved") :files {"workflow.edn" "sent"}))
            (let [draft (get-in @sig/workflow-editor-state [k :drafts "a"])]
              (is (editor/source-current? draft))
              (is (nil? (:incoming-source draft)))
              (is (= typed-after? (editor/dirty? draft)))
              (is (= {:payload {:request-id :original}} (:submission draft))))))))))

(deftest benchmark-confirmation-and-retry-retain-the-selected-evaluation-split
  (with-state
    (fn [ui k]
      (let [requests (atom []) confirmations (atom [])]
        (with-redefs [remote/submit-workflow-benchmark! (deferred requests)
                      actions/confirm! (fn [text] (swap! confirmations conj text) true)]
          ((:change ui) "a" assoc-in [:source :inspection :cases] ["dev" "held"])
          ((:change ui) "a" assoc-in [:source :inspection :evaluation]
                        {:splits {:development ["dev"] :held-out ["held"]}})
          ((:change ui) "a" assoc-in [:form :model] "fixture/model")
          ((:change ui) "a" assoc-in [:form :mode] "benchmark")
          ((:change ui) "a" assoc-in [:form :split] "held-out")
          ((:submit ui) "a" false)
          (let [payload (last (:args (first @requests)))]
            (is (= "held-out" (:split payload)))
            (is (= ["held"] (:cases payload)))
            (is (.includes (first @confirmations) "Evaluation split: held-out"))
            ((:error (first @requests)) :connection-lost)
            ((:change ui) "a" assoc-in [:form :split] "development")
            ((:submit ui) "a" true)
            (is (= payload (last (:args (second @requests)))))
            (is (= 1 (count @confirmations)))))))))

(deftest cli-benchmarks-use-their-own-admission-and-immutable-retry
  (with-state
    (fn [ui _]
      (let [requests (atom []) confirmations (atom [])]
        (with-redefs [remote/submit-cli-workflow-benchmark! (deferred requests)
                      actions/confirm! (fn [text] (swap! confirmations conj text) true)]
          ((:change ui) "a" update :form editor/set-form-field :backend "cli")
          ((:change ui) "a" assoc-in [:form :model] "claude-code-haiku")
          ((:submit ui) "a" false)
          (let [payload (last (:args (first @requests)))]
            (is (= 2 (get-in payload [:limits :max-cli-runs])))
            (is (nil? (get-in payload [:limits :max-dispatches])))
            (is (.includes (first @confirmations) "Not an HTTP or dollar cap"))
            ((:error (first @requests)) :connection-lost)
            ((:change ui) "a" assoc-in [:form :backend] "hosted")
            ((:submit ui) "a" true)
            (is (= payload (last (:args (second @requests)))))
            (is (= 1 (count @confirmations)))))))))

(deftest source-reload-completes-its-own-cache-after-selection-changes
  (with-state
    (fn [ui k]
      (let [requests (atom [])]
        (with-redefs [remote/load-workflow-source! (deferred requests)]
          ((:reload ui) "a")
          ((:select ui) "b")
          (binding [ec/*execution-context* nil]
            ((:ok (first @requests)) (source "a" "r2")))
          (is (= "b" (get-in @sig/workflow-editor-state [k :selected])))
          (is (= "r2" (get-in @sig/workflow-editor-state [k :drafts "a" :source :revision])))
          (is (false? (get-in @sig/workflow-editor-state [k :loading [:source "a"]])))
          ((:select ui) "a")
          (is (= 1 (count @requests))))))))

(deftest save-is-single-flight-and-retains-new-edits-and-other-selection
  (with-state
    (fn [ui k]
      (let [saves (atom []) loads (atom [])]
        (with-redefs [remote/save-workflow-source! (deferred saves)
                      remote/load-workflow-source! (deferred loads)]
          ((:change ui) "a" assoc-in [:files "workflow.edn"] "sent")
          ((:save ui) "a")
          ((:save ui) "a")
          ((:reload ui) "a")
          (is (= 1 (count @saves)))
          (is (empty? @loads))
          ((:change ui) "a" assoc-in [:files "workflow.edn"] "typed while saving")
          ((:select ui) "b")
          (binding [ec/*execution-context* nil]
            ((:ok (first @saves)) {:workflow {:ok true :revision "r2" :bundle "next"}
                                   :definition {:params {:goal "new"}}}))
          (let [draft (get-in @sig/workflow-editor-state [k :drafts "a"])]
            (is (= "b" (get-in @sig/workflow-editor-state [k :selected])))
            (is (= "sent" (get-in draft [:source :files "workflow.edn"])))
            (is (= "typed while saving" (get-in draft [:files "workflow.edn"])))
            (is (editor/dirty? draft))
            (is (= "new" (get-in draft [:form :params "goal"])))))))))

(deftest unresolved-submission-survives-reload-and-retries-immutable-payload
  (with-state
    (fn [ui k]
      (let [runs (atom []) loads (atom [])]
        (with-redefs [remote/submit-workflow-run! (deferred runs)
                      remote/load-workflow-source! (deferred loads)]
          ((:change ui) "a" assoc-in [:form :model] "fixture/model")
          ((:submit ui) "a" false)
          ((:submit ui) "a" false)
          (is (= 1 (count @runs)))
          ((:error (first @runs)) :connection-lost)
          (let [payload (last (:args (first @runs)))]
            ((:reload ui) "a")
            ((:ok (first @loads)) (source "a" "r2"))
            ((:change ui) "a" assoc-in [:form :model] "different/model")
            ((:submit ui) "a" false)
            (is (= 1 (count @runs)))
            ((:submit ui) "a" true)
            (is (= payload (last (:args (second @runs)))))
            ((:ok (second @runs)) {:state "queued" :job "job"})
            ((:submit ui) "a" true)
            ((:error (nth @runs 2)) :connection-lost)
            (is (= {:state "queued" :job "job"}
                   (get-in @sig/workflow-editor-state [k :drafts "a" :submission :reply])))))))))

(deftest allowance-retry-retains-identity-through-reload
  (with-state
    (fn [ui k]
      (let [funds (atom []) loads (atom [])]
        (with-redefs [remote/provision-workflow-allowance! (deferred funds)
                      remote/load-workflow-source! (deferred loads)]
          ((:fund ui) "a")
          ((:fund ui) "a")
          (is (= 1 (count @funds)))
          ((:error (first @funds)) :connection-lost)
          ((:reload ui) "a")
          ((:ok (first @loads)) (source "a" "r2"))
          ((:change ui) "a" assoc-in [:form :max-dispatches] "8")
          ((:fund ui) "a")
          (is (= (:args (first @funds)) (:args (second @funds))))
          ((:ok (second @funds)) {:state "funded"})
          (is (true? (get-in @sig/workflow-editor-state [k :drafts "a" :funded?])))
          ((:fund ui) "a")
          (is (not= (last (:args (first @funds))) (last (:args (nth @funds 2))))))))))

(deftest callbacks-from-an-old-session-cannot-mutate-state
  (with-state
    (fn [ui k]
      (let [requests (atom [])]
        (with-redefs [remote/load-workflow-source! (deferred requests)]
          ((:reload ui) "a")
          (let [before @sig/workflow-editor-state]
            (with-redefs [web/current-session? (constantly false)]
              (binding [ec/*execution-context* nil]
                ((:ok (first @requests)) (source "a" "r2"))))
            (is (= before @sig/workflow-editor-state))
            (is (= initial-revision (get-in @sig/workflow-editor-state [k :drafts "a" :source :revision])))))))))

(deftest local-retries-retain-backend-for-both-submission-and-funding
  (with-state
    (fn [ui k]
      (let [runs (atom []) funds (atom []) hosted (atom [])]
        (with-redefs [remote/submit-local-workflow-run! (deferred runs)
                      remote/submit-workflow-run! (deferred hosted)
                      remote/provision-local-workflow-allowance! (deferred funds)
                      remote/provision-workflow-allowance! (deferred hosted)]
          ((:change ui) "a" assoc-in [:form :backend] "local")
          ((:change ui) "a" assoc-in [:form :worker] "99999999-9999-4999-8999-999999999999")
          ((:submit ui) "a" false)
          ((:fund ui) "a")
          (is (= 1 (count @runs)))
          (is (= 1 (count @funds)))
          ((:error (first @runs)) :connection-lost)
          ((:error (first @funds)) :connection-lost)
          ((:change ui) "a" assoc-in [:form :backend] "hosted")
          ((:change ui) "a" assoc-in [:form :model] "different")
          ((:submit ui) "a" true)
          ((:fund ui) "a")
          (is (= (:args (first @runs)) (:args (second @runs))))
          (is (= (:args (first @funds)) (:args (second @funds))))
          (is (empty? @hosted))
          (is (= "local" (get-in @sig/workflow-editor-state [k :drafts "a" :submission :backend])))
          (is (= "local" (get-in @sig/workflow-editor-state [k :drafts "a" :fund-backend]))))))))

(deftest hosted-retries-never-switch-to-local-and-local-benchmarks-route-separately
  (with-state
    (fn [ui k]
      (let [hosted (atom []) local (atom [])]
        (with-redefs [remote/submit-workflow-run! (deferred hosted)
                      remote/submit-local-workflow-benchmark! (deferred local)]
          ((:change ui) "a" assoc-in [:form :model] "fixture/model")
          ((:submit ui) "a" false)
          ((:error (first @hosted)) :connection-lost)
          ((:change ui) "a" assoc-in [:form :backend] "local")
          ((:change ui) "a" assoc-in [:form :mode] "benchmark")
          ((:submit ui) "a" true)
          (is (= (:args (first @hosted)) (:args (second @hosted))))
          (is (empty? @local))
          ((:change ui) "b" assoc-in [:form :backend] "local")
          ((:change ui) "b" assoc-in [:form :mode] "benchmark")
          ((:change ui) "b" assoc-in [:form :worker] "99999999-9999-4999-8999-999999999999")
          ((:submit ui) "b" false)
          (is (= 1 (count @local)))
          (is (= "discard" (get-in (last (:args (first @local))) [:limits :settlement])))
          (is (= "local" (get-in @sig/workflow-editor-state [k :drafts "b" :submission :backend]))))))))

(deftest worker-discovery-replies-cannot-cross-login-sessions
  (with-state
    (fn [ui _]
      (let [requests (atom [])]
        (with-redefs [remote/load-local-workflow-workers! (deferred requests)]
          ((:workers ui))
          (is (= 1 (count @requests)))
          (let [before @sig/workflow-editor-state]
            (with-redefs [web/current-session? (constantly false)]
              (binding [ec/*execution-context* nil]
                ((:ok (first @requests)) {:workers [{:worker "old-session"}]})))
            (is (= before @sig/workflow-editor-state))))))))

(deftest local-timeout-default-is-confirmed-and-retry-keeps-the-admitted-limit
  (with-state
    (fn [ui k]
      (let [runs (atom []) benchmarks (atom []) confirmations (atom [])]
        (with-redefs [remote/submit-local-workflow-run! (deferred runs)
                      remote/submit-local-workflow-benchmark! (deferred benchmarks)
                      actions/confirm! (fn [message] (swap! confirmations conj message) true)]
          ((:change ui) "a" update :form editor/set-form-field :backend "local")
          ((:change ui) "a" update :form editor/set-form-field
                        :worker "99999999-9999-4999-8999-999999999999")
          ((:submit ui) "a" false)
          (is (= 300000 (get-in (last (:args (first @runs))) [:limits :timeout-ms])))
          (is (re-find #"Wall time: 300000 ms \(includes cold startup\)" (first @confirmations)))
          ((:error (first @runs)) :connection-lost)
          ((:change ui) "a" update :form editor/set-form-field :timeout-ms "1000")
          ((:change ui) "a" update :form editor/set-form-field :backend "hosted")
          ((:submit ui) "a" true)
          (is (= 1 (count @confirmations)))
          (is (= (:args (first @runs)) (:args (second @runs))))
          (is (= "1000" (get-in @sig/workflow-editor-state [k :drafts "a" :form :timeout-ms])))
          ((:change ui) "b" update :form editor/set-form-field :backend "local")
          ((:change ui) "b" assoc-in [:form :mode] "benchmark")
          ((:change ui) "b" assoc-in [:form :worker] "99999999-9999-4999-8999-999999999999")
          ((:submit ui) "b" false)
          (is (= 1 (count @benchmarks)))
          (is (re-find #"Wall time per cell: 300000 ms \(includes cold startup\)"
                       (second @confirmations))))))))

(deftest result-navigation-keeps-the-owning-room-replica-and-column
  (with-state
    (fn [_ _]
      (let [opened (atom [])
            data {:room-id "room" :room-name "A" :db-scope "room-scope" :col-id "column-a"}
            ui (actions/controller data)]
        ;; Match the real function's two fixed arguments plus variadic options:
        ;; CLJS may statically call its variadic entry point, bypassing the
        ;; generic JS argument-packing wrapper.
        (with-redefs [sig/open-tab! (fn [kind data & [options]]
                                      (swap! opened conj {:args [kind data options]
                                                          :context ec/*execution-context*}))]
          (binding [ec/*execution-context* nil] ((:open-run ui) "job"))
          (is (= [:run-inspector (assoc data :run-id "job")
                  {:title "Workflow Run" :new-tab? true :col-id "column-a"}]
                 (vec (:args (first @opened)))))
          (is (some? (:context (first @opened))))
          (with-redefs [web/current-session? (constantly false)] ((:open-run ui) "other-job"))
          (is (= 1 (count @opened))))))))

(deftest readiness-observations-ignore-revoked-and-superseded-wallet-replies
  (with-state
    (fn [ui k]
      (let [requests (atom []) other (atom [])
            roster {:rooms [{:room/id "room"}]}
            result {:models [] :allowance {:status :observed :microdollars "9007199254740993" :model-dispatches "8"}}]
        (with-redefs [remote/load-workflow-readiness! (deferred requests)
                      remote/load-workflow-list! (deferred other)
                      remote/load-workflow-source! (deferred other)
                      memory-remote/prepare-memory-stream! (deferred other)]
          (reset! sig/user-rooms roster)
          ((:observe ui))
          (is (= 1 (count @requests)))
          ((:ok (first @requests)) result)
          (is (= :ready (get-in @sig/workflow-editor-state [k :readiness-status])))
          (reset! sig/user-rooms {:rooms []})
          ((:observe ui))
          ((:ok (first @requests)) result)
          (is (nil? (get-in @sig/workflow-editor-state [k :readiness])))
          (reset! sig/user-rooms roster)
          ((:observe ui))
          (is (= 2 (count @requests)))
          ((:ok (second @requests)) result)
          (swap! sync/state assoc :dirty {"room" 1})
          ((:observe ui))
          (is (= 3 (count @requests)))
          (is (= :checking (get-in @sig/workflow-editor-state [k :readiness-status])))
          ((:ok (second @requests)) result)
          (is (= :checking (get-in @sig/workflow-editor-state [k :readiness-status])))
          ((:ok (nth @requests 2)) (assoc-in result [:allowance :model-dispatches] "7"))
          (is (= "7" (get-in @sig/workflow-editor-state [k :readiness :allowance :model-dispatches]))))))))

(deftest explicit-allowance-ack-causes-one-current-readiness-observation
  (doseq [inflight? [false true]]
    (with-state
      (fn [ui k]
        (let [reads (atom []) grants (atom []) other (atom [])
              old {:models [] :allowance {:status :observed :microdollars "0" :model-dispatches "0"}}
              funded (assoc old :allowance {:status :observed :microdollars "1000000" :model-dispatches "8"})]
          (with-redefs [remote/load-workflow-readiness! (deferred reads)
                        remote/load-workflow-list! (deferred other)
                        remote/load-workflow-source! (deferred other)
                        memory-remote/prepare-memory-stream! (deferred other)
                        remote/provision-workflow-allowance! (deferred grants)]
            ((:observe ui))
            (is (= 1 (count @reads)))
            (when-not inflight? ((:ok (first @reads)) old))
            ((:fund ui) "a")
            (is (= 1 (count @grants)))
            ((:ok (first @grants)) {:id "fixture-grant"})
            ((:observe ui))
            (when inflight?
              ;; Keep one physical fetch. Its acknowledgement releases the
              ;; slot, but its pre-grant amount cannot complete the new epoch.
              (is (= 1 (count @reads)))
              ((:ok (first @reads)) old)
              (is (nil? (get-in @sig/workflow-editor-state [k :readiness])))
              ((:observe ui)))
            (is (= 2 (count @reads)))
            ((:observe ui))
            (is (= 2 (count @reads)))
            ((:ok (second @reads)) funded)
            (is (= funded (get-in @sig/workflow-editor-state [k :readiness])))
            (is (= :ready (get-in @sig/workflow-editor-state [k :readiness-status])))
            (is (= 1 (count @grants)))))))))

(deftest failed-readiness-can-use-existing-retry-updates-without-funding
  (with-state
    (fn [ui k]
      (let [reads (atom []) other (atom [])]
        (with-redefs [remote/load-workflow-readiness! (deferred reads)
                      remote/load-workflow-list! (deferred other)
                      remote/load-workflow-source! (deferred other)
                      memory-remote/prepare-memory-stream! (deferred other)
                      sync/retry! (fn [& _])]
          ((:observe ui))
          ((:error (first @reads)) (js/Error. "Unavailable"))
          (is (= :unavailable (get-in @sig/workflow-editor-state [k :readiness-status])))
          (is (string? (get-in @sig/workflow-editor-state [k :errors :readiness])))
          ((:refresh ui))
          ((:observe ui))
          (is (= 2 (count @reads)))
          ((:ok (second @reads)) {:models [] :allowance {:status :unavailable}})
          (is (= :ready (get-in @sig/workflow-editor-state [k :readiness-status])))
          (is (nil? (get-in @sig/workflow-editor-state [k :errors :readiness]))))))))

(deftest disclosure-preferences-follow-the-ui-world-and-preserve-drafts-and-pending-intent
  (with-state
    (fn [_ key]
      (let [pending {:payload {:request-id "original" :model "original-model"}}
            original (assoc (get-in @sig/workflow-editor-state [key :drafts "a"])
                            :files {"workflow.edn" "unsaved code"} :submission pending)
            _ (swap! sig/workflow-editor-state assoc-in [key :drafts "a"] original)
            child (context/fork-context ec/*execution-context*)]
        (try
          (binding [ec/*execution-context* child]
            (let [ui (actions/controller {:room-id "room"})]
              ((:change ui) "a" assoc :source-open? true :execution-open? true)
              (let [draft (get-in @sig/workflow-editor-state [key :drafts "a"])]
                (is (true? (:source-open? draft)))
                (is (true? (:execution-open? draft)))
                (is (= (:files original) (:files draft)))
                (is (= pending (:submission draft))))))
          (is (= original (get-in @sig/workflow-editor-state [key :drafts "a"])))
          (finally (context/close-context! child)))))))

(deftest paused-schedule-creation-and-uncertain-retry-retain-the-original-choice
  (with-state
    (fn [ui k]
      (let [requests (atom []) confirmations (atom [])]
        (with-redefs [remote/create-workflow-schedule! (deferred requests)
                      actions/confirm! (fn [text] (swap! confirmations conj text) true)]
          ((:change ui) "a" assoc-in [:form :model] "fixture/model")
          ((:schedule ui) "a" false false)
          (is (= 1 (count @requests)))
          (let [payload (last (:args (first @requests)))]
            (is (false? (:enabled? payload)))
            (is (empty? @confirmations) "saving a paused configuration requires no activation dialog")
            ((:error (first @requests)) :connection-lost)
            ((:change ui) "a" assoc-in [:form :model] "edited-model")
            ((:schedule ui) "a" true true)
            (is (= 2 (count @requests)))
            (is (= payload (last (:args (second @requests)))))
            ((:ok (second @requests)) {:id "activation" :enabled? false})
            (is (false? (get-in @sig/workflow-editor-state [k :drafts "a" :schedule-submission :reply :enabled?])))
            (is (empty? @confirmations))))))))

(deftest pristine-upgrade-keeps-inspected-intent-and-unsaved-workflow-buffers
  (with-state
    (fn [ui k]
      (let [inspections (atom []) upgrades (atom []) revision (str (random-uuid))
            offer {:status :upgrade-available :name "maintain-wiki" :activation :unchanged :observation :current-source
                   :revision revision :installed-version 3 :shipped-version 4}]
        (with-redefs [remote/inspect-wiki-maintenance-setup! (deferred inspections)
                      remote/upgrade-wiki-maintenance! (deferred upgrades)]
          ((:change ui) "a" assoc-in [:files "workflow.edn"] "Unsaved customization")
          ((:inspect-wiki-setup ui))
          ((:ok (first @inspections)) offer)
          ((:upgrade-wiki-setup ui))
          (is (= 1 (count @upgrades)))
          (let [payload (last (:args (first @upgrades)))]
            (is (= {:expected-revision revision :from-version 3} payload))
            ((:inspect-wiki-setup ui))
            ((:ok (second @inspections)) (assoc offer :status :installed :installed-version 4))
            (is (= :pending (get-in @sig/workflow-editor-state [k :wiki-setup :status])))
            (is (= payload (get-in @sig/workflow-editor-state [k :wiki-setup :intent])))
            ((:upgrade-wiki-setup ui))
            (is (= 1 (count @upgrades)) "observed installed source never authorizes an uncertain replay")
            (is (= "Unsaved customization" (get-in @sig/workflow-editor-state [k :drafts "a" :files "workflow.edn"])))
            (reset! sync/state {:control-dirty 1})
            ((:ok (first @upgrades)) (assoc offer :status :installed :installed-version 4 :observation :acknowledged))
            (is (= :installed (get-in @sig/workflow-editor-state [k :wiki-setup :status])))
            (is (nil? (get-in @sig/workflow-editor-state [k :permissions])))
            (is (= "Unsaved customization" (get-in @sig/workflow-editor-state [k :drafts "a" :files "workflow.edn"])))))))))


(deftest selected-stream-observations-are-coalesced-and-late-replies-cannot-restore-old-source
  (with-state
    (fn [ui k]
      (let [requests (atom []) sources (atom []) other (atom [])
            bundle "34e2519c-9acb-4a13-9ff1-36ff01ac600d"
            saved #(assoc-in (source % initial-revision) [:inspection :bundle] bundle)
            reply #(hash-map :name % :source-revision initial-revision :bundle bundle :status :not-discovered)]
        (with-redefs [remote/load-workflow-stream-status! (deferred requests)
                      remote/load-workflow-source! (deferred sources)
                      remote/load-workflow-list! (deferred other)
                      memory-remote/prepare-memory-stream! (deferred other)]
          ((:observe ui))
          ((:ok (first @sources)) (saved "a"))
          ((:observe ui))
          (is (= 1 (count @requests)))
          (dotimes [_ 3] ((:observe ui)))
          (is (= 1 (count @requests)))
          ((:change ui) "a" assoc-in [:files "workflow.edn"] "Unsaved code")
          ((:observe ui))
          (is (= 1 (count @requests)))
          ((:select ui) "b" false)
          ((:observe ui))
          ((:ok (first @requests)) (reply "a"))
          (is (nil? (get-in @sig/workflow-editor-state [k :workflow-stream])))
          ((:ok (last @sources)) (saved "b"))
          ((:observe ui))
          (is (= 2 (count @requests)))
          ((:ok (second @requests)) (reply "b"))
          (is (= "b" (get-in @sig/workflow-editor-state [k :workflow-stream :reply :name])))
          (is (= "Unsaved code" (get-in @sig/workflow-editor-state [k :drafts "a" :files "workflow.edn"])))
          (reset! sig/user-rooms {:rooms []})
          ((:observe ui))
          ((:ok (second @requests)) (reply "b"))
          (is (nil? (get-in @sig/workflow-editor-state [k :workflow-stream])))
          (is (= 2 (count @requests))))))))


(deftest returning-to-the-same-saved-source-retires-old-stream-observations
  (with-state
    (fn [ui k]
      (let [reads (atom []) sources (atom []) other (atom [])
            bundle "34e2519c-9acb-4a13-9ff1-36ff01ac600d"
            saved (assoc-in (source "a" initial-revision) [:inspection :bundle] bundle)
            reply {:name "a" :source-revision initial-revision :bundle bundle :status :not-discovered}]
        (with-redefs [remote/load-workflow-stream-status! (deferred reads)
                      remote/load-workflow-source! (deferred sources)
                      remote/load-workflow-list! (deferred other)
                      memory-remote/prepare-memory-stream! (deferred other)]
          ((:observe ui))
          ((:ok (last @sources)) saved)
          ((:observe ui))
          (is (= 1 (count @reads)))
          ;; First round-trip retires an in-flight frame. The second retires a
          ;; completed frame: neither may suppress the same request on return.
          (doseq [expected [2 3]]
            ((:select ui) "scratch" false)
            ((:observe ui))
            (is (nil? (get-in @sig/workflow-editor-state [k :observations :workflow-stream])))
            ((:select ui) "a" false)
            ((:observe ui))
            (is (editor/source-current? (get-in @sig/workflow-editor-state [k :drafts "a"])))
            (is (= expected (count @reads)))
            ((:ok (nth @reads (- expected 2))) reply)
            (is (nil? (get-in @sig/workflow-editor-state [k :workflow-stream])))
            ((:ok (last @reads)) reply)
            (is (= :ready (get-in @sig/workflow-editor-state [k :workflow-stream-status])))
            (dotimes [_ 3] ((:observe ui)))
            (is (= expected (count @reads)))))))))

(deftest same-room-navigation-is-owned-by-each-tab-and-consumed-once
  (with-state
    (fn [_ _]
      (let [sources (atom []) lists (atom []) watches (atom [])
            left (actions/controller {:room-id "room" :tab-id "left" :workflow-name "alpha" :workflow-action :benchmark})
            right (actions/controller {:room-id "room" :tab-id "right" :workflow-name "beta" :workflow-action :schedule})
            lk (actions/state-key "room" "left") rk (actions/state-key "room" "right")]
        (with-redefs [remote/load-workflow-source! (deferred sources)
                      remote/load-workflow-list! (deferred lists)
                      memory-remote/prepare-memory-stream! (deferred watches)]
          (dotimes [_ 4] ((:observe left)) ((:observe right)))
          (is (= "alpha" (get-in @sig/workflow-editor-state [lk :selected])))
          (is (= "beta" (get-in @sig/workflow-editor-state [rk :selected])))
          (is (= 2 (count @sources)))
          ((:ok (first @sources)) (source "alpha" initial-revision))
          ((:ok (second @sources)) (source "beta" initial-revision))
          ((:observe left)) ((:observe right))
          (is (= "benchmark" (get-in @sig/workflow-editor-state [lk :drafts "alpha" :form :mode])))
          (is (true? (get-in @sig/workflow-editor-state [rk :drafts "beta" :schedule-open?])))
          ((:select left) "gamma")
          ((:observe left))
          (is (= "gamma" (get-in @sig/workflow-editor-state [lk :selected])))
          (is (= "beta" (get-in @sig/workflow-editor-state [rk :selected]))))))))

(deftest a-late-route-source-never-reselects-after-user-navigation
  (with-state
    (fn [_ _]
      (let [sources (atom []) lists (atom []) watches (atom [])
            ui (actions/controller {:room-id "room" :tab-id "route" :workflow-name "alpha" :workflow-action :benchmark})
            k (actions/state-key "room" "route")]
        (with-redefs [remote/load-workflow-source! (deferred sources)
                      remote/load-workflow-list! (deferred lists)
                      memory-remote/prepare-memory-stream! (deferred watches)]
          ((:observe ui))
          ((:select ui) "beta")
          ((:ok (first @sources)) (source "alpha" initial-revision))
          ((:observe ui))
          (is (= "beta" (get-in @sig/workflow-editor-state [k :selected])))
          (is (nil? (get-in @sig/workflow-editor-state [k :pending-navigation]))))))))

(deftest calendar-confirmation-and-uncertain-retry-use-the-original-intent
  (with-state
    (fn [ui k]
      (let [requests (atom []) confirmations (atom [])]
        (with-redefs [actions/confirm! (fn [message] (swap! confirmations conj message) true)
                      remote/create-workflow-schedule! (deferred requests)]
          ((:change ui) "a" update :form merge {:model "model" :schedule-cadence "week"
                                                :schedule-on "monday" :schedule-at "09:00"
                                                :schedule-tz "America/Vancouver"})
          ((:schedule ui) "a" false true)
          (is (= 1 (count @confirmations)))
          (is (.includes (first @confirmations) "Weekly on monday at 09:00 · America/Vancouver"))
          (let [original (get-in @sig/workflow-editor-state [k :drafts "a" :schedule-submission :payload])]
            (is (= {:every "week" :on "monday" :at "09:00" :tz "America/Vancouver"} (:cadence original)))
            ((:error (first @requests)) (js/Error. "lost acknowledgement"))
            ((:change ui) "a" assoc-in [:form :schedule-at] "15:00")
            ((:schedule ui) "a" true)
            (is (= original (last (:args (last @requests)))))
            (is (= 1 (count @confirmations)))))))))

(def setup-ready
  {:default-model {:status :selected :id "luna" :label "Luna" :source :user-preference}
   :models [{:id "luna" :label "Luna" :credential-status :present}]
   :allowance {:status :observed :microdollars "100000" :model-dispatches "4"}})

(defn ready-setup! [k]
  (swap! sig/workflow-editor-state update k assoc
         :readiness-status :ready :readiness setup-ready :permissions {:write? true :merge? true}))

(deftest inline-confirmation-is-once-only-even-with-synchronous-receipt
  (with-state
    (fn [ui k]
      (ready-setup! k)
      (let [calls (atom []) grants (atom []) confirms (atom 0)]
        (with-redefs [remote/submit-workflow-run! (fn [& args] (swap! calls conj args) (fn [ok _] (ok {:id "receipt" :job "job"})))
                      remote/provision-workflow-allowance! (deferred grants)
                      actions/confirm! (fn [& _] (swap! confirms inc) false)]
          ((:prepare-setup ui) "a" nil)
          (let [offered (get-in @sig/workflow-editor-state [k :drafts "a" :setup-offer])]
            (is (empty? @calls))
            (is ((:offer-current? ui) "a" offered))
            ((:accept-setup ui) "a" offered)
            ((:accept-setup ui) "a" offered)
            (is (= 1 (count @calls)))
            (is (= "luna" (:model (nth (first @calls) 2))))
            (is (= "receipt" (get-in @sig/workflow-editor-state [k :drafts "a" :submission :reply :id])))
            (is (empty? @grants))
            (is (zero? @confirms))))))))

(deftest stale-source-default-permissions-and-form-refuse-inline-confirmation
  (doseq [change [(fn [k] (swap! sig/workflow-editor-state assoc-in [k :drafts "a" :source-stale?] true))
                  (fn [k] (swap! sig/workflow-editor-state assoc-in [k :readiness :default-model :id] "other"))
                  (fn [k] (swap! sig/workflow-editor-state assoc-in [k :permissions :merge?] false))
                  (fn [k] (swap! sig/workflow-editor-state assoc-in [k :drafts "a" :form :max-dispatches] "2"))
                  (fn [_] (reset! sig/user-rooms {:rooms []}))]]
    (with-state
      (fn [ui k]
        (ready-setup! k)
        (let [calls (atom [])]
          (with-redefs [remote/submit-workflow-run! (deferred calls)]
            ((:prepare-setup ui) "a" nil)
            (let [offered (get-in @sig/workflow-editor-state [k :drafts "a" :setup-offer])]
              (change k)
              ((:accept-setup ui) "a" offered)
              (is (empty? @calls))
              (is (nil? (get-in @sig/workflow-editor-state [k :drafts "a" :submission]))))))))))

(deftest schedule-offer-and-advanced-use-one-frozen-form-and-journal
  (with-state
    (fn [ui k]
      (ready-setup! k)
      (swap! sig/workflow-editor-state assoc-in [k :drafts "a" :source :setup]
             {:status :ready :intent {:version 1 :action :schedule
              :cadence {:every "week" :on "monday" :at "09:00" :tz "America/Vancouver"}}})
      (let [calls (atom []) tabs (atom [])]
        (with-redefs [remote/create-workflow-schedule! (deferred calls)
                      sig/open-tab! (fn [& args] (swap! tabs conj args))]
          ((:prepare-setup ui) "a" nil)
          (let [offered (get-in @sig/workflow-editor-state [k :drafts "a" :setup-offer])]
            ((:open-advanced ui) "a")
            (is (= (:form offered) (get-in @sig/workflow-editor-state [k :drafts "a" :form])))
            (is (= "week" (get-in @sig/workflow-editor-state [k :drafts "a" :form :schedule-cadence])))
            ((:accept-setup ui) "a" offered)
            (is (= 1 (count @calls)))
            (let [sent (nth (:args (first @calls)) 2)]
              ((:error (first @calls)) (js/Error. "ACK lost"))
              (is (false? (get-in @sig/workflow-editor-state [k :loading [:schedule "a"]])))
              ;; Lost ACK retains the original payload even after Advanced edits.
              ((:change ui) "a" assoc-in [:form :schedule-at] "11:00")
              ((:schedule ui) "a" true)
              (is (= 2 (count @calls)))
              (is (= sent (nth (:args (last @calls)) 2)))
              (is (= "09:00" (get-in sent [:cadence :at]))))))))))

(deftest copied-ready-offer-cannot-confirm-through-the-live-socket
  (with-state
    (fn [ui k]
      (ready-setup! k)
      ((:prepare-setup ui) "a" nil)
      (let [copy (context/fork-context ec/*execution-context*) calls (atom [])]
        (try
          (binding [ec/*execution-context* copy]
            (let [copied-ui (actions/controller {:room-id "room"})]
              ((:prepare-setup copied-ui) "a" nil)
              (let [offered (get-in @sig/workflow-editor-state [k :drafts "a" :setup-offer])]
                (with-redefs [remote/submit-workflow-run! (deferred calls)]
                  (is (not ((:offer-current? copied-ui) "a" offered)))
                  ((:accept-setup copied-ui) "a" offered)
                  (is (empty? @calls))))))
          (finally (context/close-context! copy)))))))

(deftest early-advanced-does-not-manufacture-a-partial-draft
  (with-state
    (fn [ui k]
      (swap! sig/workflow-editor-state update-in [k :drafts] dissoc "a")
      (let [requests (atom [])]
        (with-redefs [sig/open-tab! (fn [& _]) remote/load-workflow-source! (deferred requests)]
          ((:open-advanced ui) "a")
          (is (nil? (get-in @sig/workflow-editor-state [k :drafts "a"])))
          ((:reload ui) "a")
          ((:ok (first @requests)) (source "a" initial-revision))
          (is (= "100000" (get-in @sig/workflow-editor-state [k :drafts "a" :form :budget-microdollars])))
          (is (= "4" (get-in @sig/workflow-editor-state [k :drafts "a" :form :max-dispatches]))))))))

(deftest canonical-advanced-selection-cannot-retarget-another-chat-workflow
  (with-state
    (fn [_ _]
      (let [data {:room-id "room" :tab-id "actual-tab" :workflow-state-owner [:chat-workflow "a"]}
            k (actions/state-key "room" [:chat-workflow "a"])
            ui (actions/controller data) calls (atom [])]
        (swap! sig/workflow-editor-state assoc k {:selected "a" :drafts {"a" (editor/draft (source "a" initial-revision))}})
        (with-redefs [remote/load-workflow-source! (deferred calls)]
          ((:select ui) "b")
          (is (= "a" (get-in @sig/workflow-editor-state [k :selected])))
          (is (empty? @calls))
          (is (= "a" (:canonical-workflow ui))))))))

(deftest malformed-inline-confirmation-neither-dispatches-nor-opens-a-modal
  (with-state
    (fn [ui k]
      (ready-setup! k)
      (let [calls (atom []) modals (atom [])]
        (with-redefs [remote/submit-workflow-run! (deferred calls)
                      remote/create-workflow-schedule! (deferred calls)
                      actions/confirm! (fn [& args] (swap! modals conj args) true)]
          (doseq [offer [nil {} {:action :run}]] ((:accept-setup ui) "a" offer))
          (is (empty? @calls))
          (is (empty? @modals)))))))
