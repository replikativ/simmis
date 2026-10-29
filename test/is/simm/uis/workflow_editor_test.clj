(ns is.simm.uis.workflow-editor-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.workflow-editor :as editor]
            [is.simm.ops.workflows :as workflows]
            [is.simm.starter.wiki-maintenance :as starter]))

(def source {:name "echo" :revision "c1d56a1b-63f9-414c-a6ad-feb35d5e709d" :files {"workflow.edn" "old"}
             :definition {:params {:goal "old default"}}
             :inspection {:ok true :bundle "bundle" :cases []}})

(deftest new-checkpoint-form-selects-compatible-settlement-without-changing-existing-intent
  (let [checkpoint-source (assoc source :files (starter/source))
        ordinary (editor/draft source)
        checkpoint (editor/draft checkpoint-source)]
    (is (= "review" (get-in ordinary [:form :settlement])))
    (is (= "discard" (get-in checkpoint [:form :settlement])))
    (is (= "hosted" (get-in checkpoint [:form :backend])))
    (is (= (:files checkpoint-source) (:files checkpoint)))
    (is (false? (editor/dirty? checkpoint)))
    (is (= ordinary (editor/draft source)))))

(deftest schedule-observations-distinguish-unknown-empty-paused-and-held
  (let [paused {:id "one" :room-id "room" :name "maintain-wiki" :enabled? false}
        held (assoc paused :id "two" :reason :budget-exhausted)
        state {:activation-status :ready
               :activations [held paused (assoc paused :room-id "other")
                             (assoc paused :name "other")]}]
    (is (= [paused held] (editor/workflow-activations state "room" "maintain-wiki")))
    (is (= [] (editor/workflow-activations state "room" "absent")))
    (is (nil? (editor/workflow-activations (assoc state :activation-status :checking) "room" "maintain-wiki")))
    (is (nil? (editor/workflow-activations (assoc state :activation-status :unavailable) "room" "maintain-wiki")))
    (is (= "Paused" (editor/activation-label paused)))
    (is (= "Held · budget-exhausted" (editor/activation-label held)))
    (is (= "Needs recreation · earlier schedule format"
           (editor/activation-label (assoc paused :enabled? true :resume-blocked :legacy-recreate))))
    (is (= "Held · admission requires reconciliation"
           (editor/activation-label (assoc paused :resume-blocked :reconciliation-required))))
    (is (= "Active" (editor/activation-label (assoc paused :enabled? true))))))

(deftest wiki-setup-is-isolated-from-editor-intent-and-uncertainty-is-sticky
  (let [state {:selected "other" :drafts {"other" (assoc (editor/draft source) :files {"a" "unsaved"})}}
        observation {:status :missing :name "maintain-wiki" :activation :unchanged :observation :current-source}
        observed (editor/observe-wiki-setup state observation)
        pending (editor/begin-wiki-setup observed)
        checked (editor/observe-wiki-setup pending observation)
        reply (assoc observation :status :installed :observation :acknowledged :permissions {:write? true})
        acknowledged (editor/acknowledge-wiki-setup checked reply)]
    (is (= :missing (get-in observed [:wiki-setup :status])))
    (is (= :pending (get-in checked [:wiki-setup :status])))
    (is (= :missing (get-in checked [:wiki-setup :observed-status])))
    (is (= :installed (get-in acknowledged [:wiki-setup :status])))
    (is (= state (dissoc acknowledged :wiki-setup)))
    (is (= pending (editor/acknowledge-wiki-setup pending {:status :unavailable})))
    (is (= :unavailable (get-in (editor/observe-wiki-setup state reply) [:wiki-setup :status])))))

(deftest wiki-output-belongs-to-the-original-submission-not-the-edited-form
  (let [id (str (random-uuid)) proposal (str (random-uuid))
        draft {:submission {:mode "run" :knowledge? true
                            :reply {:id id :kind :workflow :job (random-uuid)}}
               :form {:mode "benchmark"}}
        reply {:status :proposed :request id :proposal proposal :native-handle :secret}]
    (is (= id (editor/collectable-output-request draft)))
    (is (= (dissoc reply :native-handle) (:wiki-output (editor/apply-output-result draft id reply))))
    (is (= (dissoc reply :native-handle) (editor/collected-output (editor/apply-output-result draft id reply))))
    (is (nil? (editor/collected-output (assoc-in (editor/apply-output-result draft id reply)
                                                 [:submission :reply :id] (str (random-uuid))))))
    (is (= draft (editor/apply-output-result draft (str (random-uuid)) reply)))
    (is (= :unavailable (get-in (editor/apply-output-result draft id (assoc reply :request "other"))
                                [:wiki-output :status])))
    (doseq [ineligible [(assoc-in draft [:submission :mode] "benchmark")
                        (assoc-in draft [:submission :backend] "local")
                        (assoc-in draft [:submission :knowledge?] false)
                        (update-in draft [:submission :reply] dissoc :job)
                        (assoc-in draft [:submission :reply :kind] :benchmark)]]
      (is (nil? (editor/collectable-output-request ineligible))))))

(deftest own-filing-invalidation-keeps-only-the-exact-result-receipt
  (let [id (str (random-uuid))
        draft {:source-stale? true :source source :files (:files source)
               :submission {:mode "run" :knowledge? true
                            :reply {:id id :kind :workflow :job (random-uuid)}}}
        state {:drafts {"memory" draft "other" {:files {"a" "unsaved"}}}}
        reply {:status :proposed :request id :proposal (str (random-uuid))
               :permissions {:merge? true} :source :untrusted}
        acknowledged (editor/acknowledge-output state "memory" id reply)]
    (is (= (select-keys reply [:status :request :proposal])
           (get-in acknowledged [:drafts "memory" :wiki-output])))
    (is (true? (get-in acknowledged [:drafts "memory" :source-stale?])))
    (is (not (contains? acknowledged :permissions)))
    (is (= source (get-in acknowledged [:drafts "memory" :source])))
    (is (= (get-in state [:drafts "other"]) (get-in acknowledged [:drafts "other"])))
    (is (= state (editor/acknowledge-output state "memory" (str (random-uuid)) reply)))
    (is (= {} (editor/acknowledge-output {} "memory" id reply)))))

(deftest recurring-deployments-retain-the-inspected-execution-intent
  (let [draft (-> (editor/draft source) (assoc-in [:form :model] "explicit-model")
                  (assoc-in [:form :schedule-minutes] "60")
                  (assoc-in [:form :schedule-tz] "Europe/Berlin"))
        request (editor/scheduled-submission draft "activation")]
    (is (= "activation" (:request-id request)))
    (is (true? (:enabled? request)))
    (let [paused (editor/scheduled-submission draft "paused" false)
          edited (-> draft (assoc :schedule-submission {:payload paused})
                     (assoc-in [:form :model] "new-model"))]
      (is (false? (:enabled? paused)))
      (is (= paused (:payload (editor/deployment-preview edited)))))
    (doseq [bad [nil "false" 0]]
      (is (thrown? clojure.lang.ExceptionInfo (editor/scheduled-submission draft "invalid" bad))))
    (is (= "workflow" (:kind request)))
    (is (= {:interval-ms 3600000 :tz "Europe/Berlin"} (:cadence request)))
    (is (= (dissoc (editor/submission draft "ignored") :request-id) (:execution request)))
    (is (not (contains? (:execution request) :request-id)))
    (is (= (:revision source) (get-in request [:execution :expected-owned-source])))
    (doseq [bad [(assoc-in draft [:form :backend] "local")
                 (assoc-in draft [:form :backend] "cli")
                 (assoc-in draft [:form :schedule-minutes] "0")
                 (assoc-in draft [:form :schedule-minutes] "1.5")
                 (assoc-in draft [:form :schedule-tz] "")
                 (assoc draft :files {})]]
      (is (thrown? clojure.lang.ExceptionInfo (editor/scheduled-submission bad "bad"))))))

(deftest deployment-preview-separates-edited-settings-from-unresolved-intent
  (let [draft (assoc-in (editor/draft source) [:form :model] "original-model")
        payload (editor/scheduled-submission draft "original-request")
        pending (-> draft (assoc :schedule-submission {:payload payload})
                    (assoc-in [:form :model] "edited-model"))
        resolved (assoc-in pending [:schedule-submission :reply] {:id "activation" :enabled? true})]
    (is (= {:pending? true :payload payload} (editor/deployment-preview pending)))
    (is (= "edited-model" (get-in (editor/deployment-preview resolved) [:payload :execution :model])))
    (is (false? (:pending? (editor/deployment-preview resolved))))
    (is (not (contains? (:payload (editor/deployment-preview resolved)) :request-id)))
    (is (nil? (editor/deployment-preview (assoc-in draft [:form :model] ""))))
    (is (nil? (editor/deployment-preview (assoc draft :files {}))))
    (is (nil? (editor/deployment-preview (assoc draft :source-stale? true))))
    (is (nil? (editor/deployment-preview (assoc-in draft [:form :backend] "local"))))))

(deftest benchmark-deployment-descriptions-distinguish-shared-and-per-trial-ceilings
  (let [draft (-> (editor/draft (assoc-in source [:inspection :cases] ["a" "b"]))
                  (assoc-in [:form :mode] "benchmark")
                  (assoc-in [:form :model] "benchmark-model")
                  (assoc-in [:form :repetitions] "2"))
        payload (editor/scheduled-submission draft "benchmark-request")
        pending (-> draft (assoc :schedule-submission {:payload payload})
                    (assoc-in [:form :model] "changed-model"))
        description (editor/deployment-description (:payload (editor/deployment-preview pending)))
        ordinary (editor/deployment-description
                   (editor/scheduled-submission (assoc-in (editor/draft source) [:form :model] "ordinary-model") nil))]
    (is (= ["a" "b"] (get-in payload [:execution :cases])))
    (is (= 2 (get-in payload [:execution :repetitions])))
    (is (= "benchmark-model" (:model description)))
    (is (= "one, two" (:model (editor/deployment-description (assoc-in payload [:execution :models] ["one" "two"])))))
    (is (re-find #"Per occurrence:.*shared across benchmark trials" (:resources description)))
    (is (re-find #"^Per benchmark trial:" (:execution-ceilings description)))
    (is (re-find #"^Checker-backed" (:results description)))
    (is (= "ordinary-model" (:model ordinary)))
    (is (re-find #"^Per occurrence:" (:execution-ceilings ordinary)))
    (is (not (re-find #"benchmark" (:resources ordinary))))))

(deftest cli-text-submission-and-allowance-never-claim-http-or-money-bounds
  (let [draft (-> (editor/draft source)
                  (update :form editor/set-form-field :backend "cli")
                  (assoc-in [:form :model] "claude-code-haiku"))
        request (editor/submission draft "request")]
    (is (= "benchmark" (get-in draft [:form :mode])))
    (is (= {:request-id "allowance" :max-cli-runs 2} (editor/allowance (:form draft) "allowance")))
    (is (= {:max-cli-runs 2 :max-model-steps 4 :timeout-ms 120000 :settlement "discard"} (:limits request)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (assoc-in draft [:form :mode] "run") "bad")))))

(deftest benchmark-split-selection-does-not-implicitly-include-held-out-cases
  (let [source (assoc source :inspection {:ok true :bundle "bundle" :cases ["a" "b"]
                                          :evaluation {:splits {:development ["a"] :held-out ["b"]}}})
        draft (-> (editor/draft source) (assoc-in [:form :mode] "benchmark")
                  (assoc-in [:form :model] "explicit-model"))]
    (is (= ["a"] (:cases (editor/submission draft "dev"))))
    (is (= "development" (:split (editor/submission draft "dev"))))
    (is (= ["b"] (:cases (editor/submission (assoc-in draft [:form :split] "held-out") "held"))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (assoc-in draft [:form :case] "b") "wrong")))
    (is (= "" (:case (editor/set-form-field (assoc (:form draft) :case "a") :split "held-out"))))))

(deftest backend-default-timeouts-never-replace-explicit-or-legacy-values
  (let [hosted editor/defaults
        local (editor/set-form-field hosted :backend "local")]
    (is (= "120000" (:timeout-ms hosted)))
    (is (= "300000" (:timeout-ms local)))
    (is (true? (:timeout-default? local)))
    (is (= "120000" (:timeout-ms (editor/set-form-field local :backend "hosted"))))
    (doseq [value ["120000" "5000" ""]]
      (let [edited (editor/set-form-field hosted :timeout-ms value)
            switched (editor/set-form-field edited :backend "local")]
        (is (false? (:timeout-default? edited)))
        (is (= value (:timeout-ms switched)))
        (is (= value (:timeout-ms (editor/set-form-field switched :backend "hosted"))))))
    (is (= "120000" (:timeout-ms (editor/set-form-field
                                  (dissoc hosted :timeout-default?) :backend "local"))))
    (is (= local (editor/set-form-field local :backend "local")))))

(deftest edits-are-an-exact-source-diff
  (let [draft (-> (editor/draft source) (assoc :files {"notes.md" "new"}))]
    (is (= {"workflow.edn" nil "notes.md" "new"} (editor/edits draft)))
    (is (editor/dirty? draft))))

(deftest reload-keeps-immutable-admission-and-funding-records
  (let [old (assoc (editor/draft source) :submission {:payload {:request-id "one"}}
                   :fund-request {:request-id "fund"})
        reloaded (editor/reloaded old (assoc source :revision "new"))]
    (is (= (:submission old) (:submission reloaded)))
    (is (= (:fund-request old) (:fund-request reloaded)))
    (is (= "new" (get-in reloaded [:source :revision])))))

(deftest saving-retains-newer-local-edits-and-only-explicit-parameter-overrides
  (let [draft (editor/draft source)
        reply {:workflow {:ok true :revision "new" :bundle "new-bundle"}
               :definition {:params {:goal "new default"}}}
        result (editor/saved (assoc draft :files {"workflow.edn" "typed while saving"})
                             {"workflow.edn" "sent"} reply)]
    (is (= "typed while saving" (get-in result [:files "workflow.edn"])))
    (is (= "sent" (get-in result [:source :files "workflow.edn"])))
    (is (= "new default" (get-in result [:form :params "goal"])))
    (is (= "explicit override"
           (get-in (editor/saved (assoc-in draft [:form :params "goal"] "explicit override")
                                 (:files draft) reply) [:form :params "goal"])))))

(deftest request-fences-are-token-and-selection-aware
  (let [s {:selected "b" :requests {[:save "a"] "save" [:source "a"] "read"}}]
    (is (editor/current-response? s [:save "a"] "save" nil))
    (is (not (editor/current-response? s [:source "a"] "read" "a")))
    (is (not (editor/current-response? s [:save "a"] "older" nil)))))

(deftest a-racing-source-read-never-upgrades-the-saved-revision
  (let [result (editor/saved (editor/draft source) (:files source)
                             {:source-changed? true :workflow {:ok true :revision "saved" :bundle "bundle"}})]
    (is (= "saved" (get-in result [:source :revision])))
    (is (false? (get-in result [:source :inspection :ok])))
    (is (not (editor/dirty? result)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (assoc-in result [:form :model] "explicit-model") "request")))))

(deftest launches-have-explicit-finite-limits-and-benchmark-defaults
  (let [draft (assoc-in (editor/draft source) [:form :model] "explicit-model")
        request (editor/submission draft "request")]
    (is (= "request" (:request-id request)))
    (is (= "bundle" (:expected-bundle request)))
    (is (= "review" (get-in request [:limits :settlement])))
    (is (thrown? clojure.lang.ExceptionInfo (editor/submission (assoc-in draft [:form :timeout-ms] "Infinity") "bad")))
    (is (thrown? clojure.lang.ExceptionInfo (editor/submission (assoc draft :files {}) "dirty")))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (-> draft (assoc-in [:form :mode] "benchmark")
                                        (assoc-in [:form :params "goal"] "override")) "wrong")))
    (let [benchmark (editor/submission (assoc-in draft [:form :mode] "benchmark") "bench")]
      (is (= ["explicit-model"] (:models benchmark)))
      (is (= "discard" (get-in benchmark [:limits :settlement])))
      (is (not (contains? benchmark :params))))))

(deftest local-launches-have-only-enrollment-and-admission-bounds
  (let [worker "99999999-9999-4999-8999-999999999999"
        draft (-> (editor/draft source) (assoc-in [:form :backend] "local")
                  (assoc-in [:form :worker] worker))
        request (editor/submission draft "local")]
    (is (= worker (:worker request)))
    (is (= #{:request-id :name :expected-bundle :expected-owned-source :limits :params :worker} (set (keys request))))
    (is (= {:timeout-ms 120000 :settlement "review"} (:limits request)))
    (is (= {:request-id "fund" :max-worker-admissions 1} (editor/allowance (:form draft) "fund")))
    (doseq [bad [(assoc-in draft [:form :worker] "")
                 (assoc-in draft [:form :timeout-ms] "600001")
                 (assoc-in draft [:form :backend] "unknown")]]
      (is (thrown? clojure.lang.ExceptionInfo (editor/submission bad "bad"))))
    (let [benchmark (editor/submission (assoc-in draft [:form :mode] "benchmark") "bench")]
      (is (= #{:request-id :name :expected-bundle :expected-owned-source :limits :worker :repetitions} (set (keys benchmark))))
      (is (= "discard" (get-in benchmark [:limits :settlement]))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/allowance (assoc (:form draft) :worker-admissions "33") "too-many")))))

(deftest all-execution-backends-retain-the-inspected-owned-source
  (doseq [backend ["hosted" "local" "cli"]]
    (let [draft (-> (editor/draft source)
                    (update :form editor/set-form-field :backend backend)
                    (assoc-in [:form :model] "explicit-model")
                    (assoc-in [:form :worker] "99999999-9999-4999-8999-999999999999"))]
      (is (= (:revision source) (:expected-owned-source (editor/submission draft "request"))))
      (doseq [revision [nil "source" ""]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (editor/submission (assoc-in draft [:source :revision] revision) "bad")))))))

(deftest internal-checker-selector-is-not-a-user-parameter-or-a-name-collision
  (let [source {:definition {:params {:simmis/checker-profile :wiki-structure-v1
                                     :checker-profile "ordinary user parameter" :goal "Keep me"}}}]
    (is (= {"checker-profile" "ordinary user parameter" "goal" "Keep me"}
           (editor/parameters source)))
    (is (= :wiki-structure-v1 (get-in source [:definition :params :simmis/checker-profile])))))

(deftest shipped-wiki-supports-ordinary-and-benchmark-deployment-without-editing-internal-selector
  (let [source (workflows/details-of starter/name {:files (starter/source 2)})
        draft (assoc-in (editor/draft source) [:form :model] "codex-subscription-luna-6")
        run (editor/submission draft (random-uuid))
        scheduled (editor/deployment-preview draft)
        benchmark (assoc-in draft [:form :mode] "benchmark")]
    (is (= :wiki-structure-v1 (get-in source [:definition :params :simmis/checker-profile])))
    (is (= {} (get-in draft [:form :params])))
    (is (= {} (:params run)))
    (is (= {} (get-in scheduled [:payload :execution :params])))
    (is (= "workflow" (get-in scheduled [:payload :kind])))
    (is (= "benchmark" (get-in (editor/deployment-preview benchmark) [:payload :kind])))
    (is (= ["codex-subscription-luna-6"] (:models (editor/submission benchmark (random-uuid)))))))

(deftest upgrade-observations-retain-exact-intent-and-do-not-replace-editor-drafts
  (let [revision (str (random-uuid))
        state {:selected "maintain-wiki" :drafts {"maintain-wiki" {:files {"workflow.edn" "Unsaved customization"}}}}
        reply {:name "maintain-wiki" :activation :unchanged :observation :current-source
               :status :upgrade-available :revision revision :installed-version 3 :shipped-version 4}
        observed (editor/observe-wiki-setup state reply)
        intent {:expected-revision revision :from-version 3}
        pending (editor/begin-wiki-setup observed intent)
        checked (editor/observe-wiki-setup pending (assoc reply :status :installed :installed-version 4))
        acknowledged (editor/acknowledge-wiki-setup checked (assoc reply :status :installed :installed-version 4 :observation :acknowledged))]
    (is (= :upgrade-available (get-in observed [:wiki-setup :status])))
    (is (= revision (get-in observed [:wiki-setup :revision])))
    (is (= :pending (get-in checked [:wiki-setup :status])))
    (is (= :installed (get-in checked [:wiki-setup :observed-status])))
    (is (= intent (get-in checked [:wiki-setup :intent])))
    (is (= (:drafts state) (:drafts checked) (:drafts acknowledged)))
    (is (= :installed (get-in acknowledged [:wiki-setup :status])))
    (is (= 4 (get-in acknowledged [:wiki-setup :installed-version])))
    (is (= :changed (get-in (editor/acknowledge-wiki-setup pending (assoc reply :status :changed :observation :acknowledged)) [:wiki-setup :status])))
    (doseq [bad [(dissoc reply :revision) (assoc reply :installed-version 4) (assoc reply :revision "not-a-revision")]]
      (is (= :unavailable (get-in (editor/observe-wiki-setup state bad) [:wiki-setup :status]))))))


(deftest batch-status-is-an-observation-not-a-job-outcome
  (is (nil? (editor/batch-label nil)))
  (is (= "Input batch status unavailable" (editor/batch-label {:status :unavailable})))
  (is (= "Input batch awaiting review" (editor/batch-label {:status :observed :phase :review})))
  (is (= "Input batch closed" (editor/batch-label {:status :observed :phase :closed})))
  (is (= "Input batch status unavailable" (editor/batch-label {:status :observed :phase :unexpected}))))


(deftest personal-stream-observations-belong-to-saved-source-and-preserve-drafts
  (let [bundle "34e2519c-9acb-4a13-9ff1-36ff01ac600d"
        saved (assoc-in source [:inspection :bundle] bundle)
        state {:selected "echo" :drafts {"echo" (-> (editor/draft saved)
                                                   (assoc-in [:files "workflow.edn"] "Unsaved code")
                                                   (assoc :submission {:intent :pending}))}}
        request (editor/stream-request state)
        reply {:name "echo" :source-revision (:revision saved) :bundle bundle :status :not-discovered}
        result (editor/observe-stream state request reply)]
    (is (= {:name "echo" :expected-source (:revision saved) :expected-bundle bundle} request))
    (is (= (:drafts state) (:drafts result)))
    (is (= :ready (:workflow-stream-status result)))
    (is (= reply (get-in result [:workflow-stream :reply])))
    (is (= state (editor/observe-stream state (assoc request :name "other") reply)))
    (is (nil? (editor/stream-request (assoc-in state [:drafts "echo" :source-stale?] true))))
    (is (thrown? clojure.lang.ExceptionInfo (editor/observe-stream state request (assoc reply :source-revision "foreign")))))
    (is (nil? (editor/stream-request {:selected "new"}))))

(deftest input-check-observations-are-closed-and-never-sticky-across-freshness
  (let [bundle "34e2519c-9acb-4a13-9ff1-36ff01ac600d"
        saved (assoc-in source [:inspection :bundle] bundle)
        state {:selected "echo" :drafts {"echo" (assoc (editor/draft saved) :submission {:intent :pending})}}
        request (editor/stream-request state)
        reply {:name "echo" :source-revision (:revision saved) :bundle bundle :status :observed :lifecycle :active
               :pending {:count 1 :complete? true :limit 20000} :freshness {:status :current} :batch {:status :none}}
        held {:status :held :code :context-exceeds-budget
              :context {:files 33 :bytes 100 :file-limit 32 :byte-limit 524288}
              :head {:bytes 100 :file-limit 524288 :batch-byte-limit 1048576}
              :reserved {:files 0 :bytes 0} :limits {:files 100 :bytes 2097152}}
        observed (editor/observe-stream state request (assoc reply :input-check held))
        changed (editor/observe-stream observed request (assoc reply :freshness {:status :changed}
                                                                   :input-check {:status :not-checked}))]
    (is (= (:drafts state) (:drafts changed)))
    (is (= {:status :not-checked} (get-in changed [:workflow-stream :reply :input-check])))
    (doseq [bad [(assoc held :source (random-uuid))
                 (assoc held :code :untrusted-exception)
                 (assoc-in held [:context :path] "private.md")
                 (assoc-in held [:head :bytes] -1)]]
      (is (thrown? clojure.lang.ExceptionInfo (editor/observe-stream state request (assoc reply :input-check bad)))))
    (is (= changed (editor/observe-stream changed (assoc request :expected-source "old") (assoc reply :input-check held))))))

(deftest benchmark-candidates-share-the-same-cases-and-eight-cell-ceiling
  (let [draft (-> (editor/draft source)
                  (assoc-in [:form :mode] "benchmark")
                  (assoc-in [:form :models] ["model-a" "model-b"]))
        payload (editor/submission draft "request")]
    (is (= ["model-a" "model-b"] (:models payload)))
    (is (= "bundle" (:expected-bundle payload)))
    (is (= "discard" (get-in payload [:limits :settlement])))
    (doseq [models [[] ["a" "a"] ["a" "b" "c" "d"]]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (editor/submission (assoc-in draft [:form :models] models) "bad"))))
    (let [cases (-> draft (assoc-in [:source :inspection :cases] ["one" "two" "three"])
                    (assoc-in [:form :models] ["a" "b" "c"]))]
      (is (thrown? clojure.lang.ExceptionInfo (editor/submission cases "nine-cells"))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (editor/submission (assoc-in draft [:source :inspection :benchmark-ready?] false) "not-ready")))))

(deftest action-navigation-opens-setup-and-preserves-frozen-requests
  (let [pending {:payload {:request-id "original" :models ["a"]}}
        draft (assoc (editor/draft source) :submission pending :schedule-submission pending)]
    (doseq [action [:run :schedule :benchmark]]
      (let [opened (editor/choose-action draft action)]
        (is (:execution-open? opened))
        (is (= (= action :schedule) (:schedule-open? opened)))
        (is (= pending (:submission opened)))
        (is (= pending (:schedule-submission opened)))
        (is (= (:files draft) (:files opened)))))))

(deftest calendar-schedules-use-the-existing-explicit-backend-contract
  (let [draft (-> (editor/draft source) (assoc-in [:form :model] "explicit-model")
                  (assoc-in [:form :schedule-cadence] "week")
                  (assoc-in [:form :schedule-on] "monday")
                  (assoc-in [:form :schedule-at] "09:00")
                  (assoc-in [:form :schedule-tz] "America/Vancouver"))
        payload (editor/scheduled-submission draft "weekly")]
    (is (= {:every "week" :on "monday" :at "09:00" :tz "America/Vancouver"} (:cadence payload)))
    (is (= "Weekly on monday at 09:00 · America/Vancouver" (editor/cadence-label (:cadence payload))))
    (doseq [at ["25:00" "09:60" "9:00"]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (editor/scheduled-submission (assoc-in draft [:form :schedule-at] at) "invalid"))))
    (let [pending (assoc draft :schedule-submission {:payload payload})
          edited (assoc-in pending [:form :schedule-at] "15:00")]
      (is (= payload (:payload (editor/deployment-preview edited)))))))

(deftest workflow-room-label-follows-late-roster-and-room-renames
  (let [data {:room-id "target" :room-name "Room"}]
    (is (= "Room" (editor/room-label data nil)))
    (is (= "Research workspace" (editor/room-label data {:rooms [{:room/id "other" :room/name "Other"}
                                                                 {:room/id "target" :room/name "Research workspace"}]})))
    (is (= "Renamed workspace" (editor/room-label data [{:room/id "target" :room/name "Renamed workspace"}]))))
  (is (= "Explicit label" (editor/room-label {:room-id "target" :room-name "Explicit label"} {:rooms []}))))

(def setup-readiness
  {:default-model {:status :selected :id "luna" :label "Luna"}
   :models [{:id "luna" :label "Luna" :credential-status :present}
            {:id "sol" :label "Sol" :credential-status :present}
            {:id "missing" :credential-status :undetected}]})

(deftest automatic-choice-is-inert-and-preserves-explicit-overrides
  (let [draft (editor/draft source)]
    (is (= "luna" (get-in (editor/automatic-model draft setup-readiness) [:form :model])))
    (is (= "sol" (get-in (editor/automatic-model (assoc-in draft [:form :model] "sol") setup-readiness) [:form :model])))
    (is (nil? (:submission (editor/automatic-model draft setup-readiness))))))

(deftest setup-offers-pin-source-model-and-explicit-cadence-without-dispatch
  (let [draft (editor/draft source)
        run (editor/setup-offer draft setup-readiness nil)
        schedule (editor/setup-offer
                   (assoc-in draft [:source :setup] {:status :ready :intent {:version 1 :action :schedule
                     :cadence {:every "week" :on "monday" :at "09:00" :tz "America/Vancouver"}}}) setup-readiness nil)]
    (is (= :run (:action run)))
    (is (= "luna" (get-in run [:payload :model])))
    (is (= (:revision source) (get-in run [:payload :expected-owned-source])))
    (is (nil? (get-in run [:payload :request-id])))
    (is (= :schedule (:action schedule)))
    (is (= "America/Vancouver" (get-in schedule [:payload :cadence :tz])))
    (is (true? (get-in schedule [:payload :enabled?])))
    (is (nil? (:submission draft)))
    (is (thrown? Exception (editor/setup-offer draft setup-readiness :schedule)))
    (is (thrown? Exception (editor/setup-offer (assoc draft :source-stale? true) setup-readiness :run)))
    (is (thrown? Exception (editor/setup-offer (assoc-in draft [:source :setup] {:status :invalid}) setup-readiness nil)))
    (is (thrown? Exception (editor/setup-offer draft (assoc setup-readiness :default-model {:status :unavailable}) nil)))
    (is (thrown? Exception (editor/setup-offer (assoc-in draft [:source :setup :intent] {:model "missing"}) setup-readiness nil)))
    (is (= "sol" (get-in (editor/setup-offer (assoc-in draft [:source :setup :intent] {:model "sol"}) setup-readiness nil) [:payload :model])))
    (is (thrown? Exception (editor/setup-offer (assoc-in draft [:source :setup :intent] {:limits {:max-dispatches 1000}}) setup-readiness nil)))
    (is (thrown? Exception (editor/setup-offer (assoc-in draft [:source :setup :intent] {:action :schedule :cadence {:every "week" :on "monday" :at "09:00" :tz "UTC" :interval-ms 60000}}) setup-readiness nil)))))

(deftest benchmark-setup-offers-use-preferred-or-explicit-models-and-frozen-cases
  (let [draft (-> (editor/draft source)
                  (assoc-in [:source :inspection :cases] ["supported"])
                  (assoc-in [:source :inspection :benchmark-ready?] true)
                  (assoc-in [:source :setup :intent] {:version 1 :action :benchmark}))
        automatic (editor/setup-offer draft setup-readiness nil)
        comparison (editor/setup-offer
                    (assoc-in draft [:source :setup :intent]
                              {:version 1 :action :benchmark :models ["luna" "sol"] :repetitions 2})
                    setup-readiness nil)]
    (is (= :benchmark (:action automatic)))
    (is (= ["luna"] (get-in automatic [:payload :models])))
    (is (= ["luna" "sol"] (get-in comparison [:payload :models])))
    (is (= ["supported"] (get-in comparison [:payload :cases])))
    (is (= 2 (get-in comparison [:payload :repetitions])))
    (is (= "discard" (get-in comparison [:payload :limits :settlement])))
    (is (nil? (:submission draft)))
    (is (thrown? Exception (editor/setup-offer (assoc-in draft [:source :inspection :benchmark-ready?] false)
                                              setup-readiness nil)))))

(deftest checkpoint-offer-preserves-discard-and-advanced-edits-can-be-reviewed
  (let [draft (editor/draft (assoc source :files (starter/source)))
        offer (editor/setup-offer draft setup-readiness nil)
        configured (assoc draft :form (:form offer) :setup-form-revision (:revision source))
        edited (assoc-in configured [:form :max-dispatches] "2")]
    (is (= "discard" (get-in offer [:payload :limits :settlement])))
    (is (= 2 (get-in (editor/setup-offer edited setup-readiness nil) [:payload :limits :max-dispatches])))))

(deftest advanced-initializes-proposed-settings-before-model-is-configured
  (let [draft (assoc-in (editor/draft source) [:source :setup]
                       {:status :ready :intent {:version 1 :action :schedule
                        :cadence {:every "week" :on "monday" :at "09:00" :tz "UTC"}
                        :limits {:max-dispatches 8}}})
        configuration (editor/setup-configuration draft {} nil)
        edited (-> draft (assoc :form (:form configuration) :setup-form-revision (:revision source))
                   (assoc-in [:form :model] "sol") (assoc-in [:form :max-dispatches] "2"))
        offer (editor/setup-offer edited setup-readiness nil)]
    (is (= "week" (get-in configuration [:form :schedule-cadence])))
    (is (= "8" (get-in configuration [:form :max-dispatches])))
    (is (= "sol" (get-in offer [:payload :execution :model])))
    (is (= 2 (get-in offer [:payload :execution :limits :max-dispatches])))))
