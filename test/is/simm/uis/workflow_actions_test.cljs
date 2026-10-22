(ns is.simm.uis.workflow-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.runtimes.web :as web]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.workflow-actions :as actions]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.memory-remote :as memory-remote]
            [is.simm.uis.web.desktop.workflow-editor :as editor]
            [is.simm.uis.web.desktop.workflows-remote :as remote]))

(defn source [name revision]
  {:name name :revision revision :files {"workflow.edn" "{}"}
   :definition {:params {:goal "old"}}
   :inspection {:ok true :bundle "bundle" :cases []}})

(def initial-revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d")

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
