(ns is.simm.ops.shipped-wiki-v6-daily-test
  "One bounded real-native v6 daily loop. Only provider transport is scripted;
   this proves execution wiring and exact retained evidence, not model quality."
  (:require [clojure.test :refer [deftest is]]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as native-catalog]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.ops :as dops]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.system.rooms :as native-rooms]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.space :as space]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-submitted-recovery :as submitted-model]
            [is.simm.model.knowledge-batch-recovery :as unlinked-model]
            [is.simm.model.room-evidence-collections :as collections]
            [is.simm.model.workflow-schedules :as schedules-model]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.work-request-checkpoints :as request-roots]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.conversation-capture :as intake]
            [is.simm.ops.file-output-controller :as controller]
            [is.simm.ops.knowledge-output-service :as output]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-failed-settlement :as failed-settlement]
            [is.simm.ops.owned-knowledge-submitted-recovery :as submitted-recovery]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.knowledge-batch-proof :as batch-proof]
            [is.simm.ops.owned-schedule-fixture :as fixture]
            [is.simm.ops.owned-schedule-occurrences-test :as occurrence-fixture]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workspace-test :as workspace-fixture]
            [is.simm.ops.shipped-wiki-schedule-test :as scripted]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs-test :as saved]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.application-ops-test :as app-fixture]
            [is.simm.starter.wiki-maintenance :as starter]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.adapters.geschichte :as gy]))

(def company-id #uuid "bec7d5a5-343a-4b26-a9ad-d4624e295d9d")
(def block-id #uuid "03a8780a-f749-40ae-8c47-dd645bf59751")
(def sam-id #uuid "0de9e634-b96d-49b1-b8e5-c5919f5b83e2")
(def original-company
  (str "---\nsimmis-id: " company-id "\nrevenue: 0\nactive: false\ncustom:\n  owner-note: preserve me\n---\n"
       "<!-- simmis:block " block-id " -->\n\n# Harbor Labs\n\nHarbor Labs is exploring invoice tools.\n\n<!-- /simmis:block -->\n"))
(def original-capture
  "---\nsimmis-id: 49c3fd94-abaf-4ac6-ae83-8e58504b4163\nobservation-kind: sampled-model-description\noriginal-image-inspected: false\n---\n# Sampled company observation\n\nA sampled description mentions Cedar Works in a browser tab. It establishes no client relationship. The original screenshot is not inspected and recording position is estimated.\n\n[Recorded image](screen.png)\n")
(def decisive-message "Sam approved the interview protocol for Harbor Labs. Customer demand remains unvalidated; no customer has committed to buy.")

(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))

(defn- with-canonical-fixture [f]
  ;; Match the existing native adoption probe: provision pristine unpublished
  ;; test repositories before registration, never relabel an authored legacy DB.
  (let [opened (atom []) seed! @#'workspace-fixture/seed!]
    (with-redefs-fn
      {#'native-catalog/open-memory-workspace!
       (fn [room]
         (let [cfg (assoc workspace/repository-options :attribute-refs? false
                          :store {:backend :memory :id (random-uuid)})]
           (d/create-database cfg)
           (let [conn (d/connect cfg)]
             (swap! opened conj conn)
             (space/provision! conn)
             (binding [ec/*execution-context* (:ctx room)]
               (ygg/register! (gy/create conn {:system-name (str "daily-v6-" (random-uuid))})))
             conn)))
       #'workspace-fixture/seed!
       (fn [conn text] (seed! conn (str "---\nsimmis-id: " (random-uuid) "\n---\n" text)))}
      #(fixture/with-fixture true
         (fn [data]
           (try (f data) (finally (doseq [conn @opened] (governance/ungovern! conn)))))))))

(defn- occurrence [conn activation]
  (schedules-model/occurrence conn (:schedule.activation/last-occurrence
                                    (schedules-model/activation conn (:schedule.activation/id activation)))))

(defn- scripted-pages
  "The actual candidate reads its immutable manifest and source copies, checks
   the retained wiki, and emits exact v2 output and original-coverage report."
  []
  (str "(let [manifest (read-string (slurp \"/in/knowledge/manifest.edn\")) "
       "triggers (filter #(= :trigger (:role %)) (:files manifest)) "
       "human (first (filter #(.startsWith (:path %) \"inbox/conversations/room-activity/\") triggers)) "
       "capture (first (filter #(.startsWith (:path %) \"captures/\") triggers)) "
       "old (slurp \"/wiki/Company.md\")] "
       "(when-not (and (= 2 (:version manifest)) (= 2 (count triggers)) human capture "
       "(.contains (slurp (:input human)) " (pr-str decisive-message) ") "
       "(.contains (slurp (:input capture)) \"original-image-inspected: false\") "
       "(= old " (pr-str original-company) ")) "
       "(throw (ex-info \"Original selection or retained context mismatch\" {}))) "
       "(babashka.fs/create-dirs \"/out/knowledge\") "
       "(spit \"/out/knowledge/Sam.md\" "
       "(str " (pr-str (str "---\nsimmis-id: " sam-id "\n---\n# Sam\n\n" decisive-message "\n\n"))
       "\"[Original message](\" (:input human) \")\\n\")) "
       "(spit \"/out/knowledge/Company.md\" "
       "(str old \"\\n## Decision and observations\\n\\n[Sam](Sam.md) approved the interview protocol. Customer demand remains unvalidated.\\n\\n\" "
       "\"A sampled description mentions Cedar Works in a browser tab; no client relationship is established. The original screenshot was not inspected and recording position is estimated.\\n\\n\" "
       "\"[Original message](\" (:input human) \") [Capture description](\" (:input capture) \")\\n\")) "
       "(spit \"/out/coverage.edn\" (pr-str {:version 1 :trigger-id (:trigger-id manifest) "
       ":items (mapv (fn [entry] {:input (:input entry) :content (:content entry) :status :incorporated "
       ":targets (if (= (:path entry) (:path human)) [\"wiki/Company.md\" \"wiki/Sam.md\"] [\"wiki/Company.md\"]) "
       ":note \"Read the exact original; preserve attribution and visual uncertainty.\"}) triggers)})) "
       "{:read-originals (mapv :input triggers) :emitted [\"wiki/Company.md\" \"wiki/Sam.md\"]})"))

(defn- filing-diagnostic [conn principal room-id room request]
  (try
    (let [prepared (filing/prepare conn principal room-id room request)]
      {:stage :prepared :kind (get-in prepared [:disposition :kind])})
    (catch Throwable error
      ;; Keep this fixture failure actionable without exposing retained bodies,
      ;; provider messages, native handles or arbitrary helper ex-data.
      {:stage :prepare-refused
       :causes (loop [cause error remaining 5 result []]
                 (if (and cause (pos? remaining))
                   (let [data (ex-data cause)]
                     (recur (.getCause ^Throwable cause) (dec remaining)
                            (conj result (cond-> {:class (.getName (class cause))}
                                           (keyword? (:type data)) (assoc :type (:type data))
                                           (keyword? (:reason data)) (assoc :reason (:reason data))))))
                   result))})))

(defn- next-original-and-failure-hold!
  [{:keys [conn w host owned room activation due principal handle execution observed]}]
  (let [later #(java.util.Date. (+ (.getTime ^java.util.Date due) (* % 60000)))
        calls (atom 0)
        current-company (:text (files/read-file-in @owned "wiki/Company.md"))
        original-count (count (requests/requests conn))
        post! (fn [text]
                (binding [ec/*execution-context* (:ctx room)]
                  (let [m (discourse/message (keyword "party" (str (:owner w))) :_room-log text nil {:role :user})]
                    (is (= :inserted (store/-store-message! (:store room) (:id room) m))) m)))
        await! (fn [o message]
                 (is (= :linked (:schedule.occurrence/state o)))
                 (let [r (requests/get-request conn (:schedule.occurrence/request o))
                       job (:work.request/job r) done (get @observed job)]
                   (when-not (and (uuid? job) (instance? clojure.lang.IDeref done)
                                  (true? (deref done 120000 false)))
                     (throw (ex-info "Incremental v6 Job did not physically return" {:job job})))
                   (@#'occurrence-fixture/join-fixture-jobs! room observed)
                   (let [retained (requests/payload conn r)
                         manifest (edn/read-string (get-in retained [:input :inputs "/in/knowledge/manifest.edn"]))]
                     (is (= 1 (count (:files manifest))) "Only the newly arrived original is selected")
                     (is (str/includes? (:path (first (:files manifest))) (str (:id message)))))
                   {:request r :job (jobs/status job)}))]
    ;; Fixture funding only. The production dispatcher never mints resources.
    (resource/mint! room {:id (random-uuid) :resources (update-vals (work/resources (:limits execution)) #(* 2M %))})
    (let [new-text "A meeting reminder only; no change to the earlier interview decision or customer status."
          message (post! new-text)
          previous (:schedule.occurrence/id (occurrence conn activation))
          step (atom 0)
          program (str "(let [m (read-string (slurp \"/in/knowledge/manifest.edn\")) "
                       "entries (filter #(= :trigger (:role %)) (:files m))] "
                       "(assert (= 1 (count entries))) "
                       "(assert (.contains (slurp (:input (first entries))) " (pr-str new-text) ")) "
                       "(assert (= (slurp \"/wiki/Company.md\") " (pr-str current-company) ")) "
                       "(spit \"/out/coverage.edn\" (pr-str {:version 1 :trigger-id (:trigger-id m) "
                       ":items (mapv (fn [e] {:input (:input e) :content (:content e) :status :no-change :targets [] "
                       ":note \"Reminder only; no new supported knowledge.\"}) entries)})))")]
      (with-redefs [gateway/*request-fn* (fn [& _]
                                           (swap! calls inc)
                                           (if (= 1 (swap! step inc))
                                             (@#'scripted/tool-response program "v6-incremental-no-change")
                                             (@#'scripted/final-response "No new supported factual change.")))]
        (schedules/tick-owned! host (later 2))
        (let [o (occurrence conn activation) _ (is (not= previous (:schedule.occurrence/id o)))
              {:keys [request job]} (await! o message)]
          (is (= :completed (:status job)))
          (schedules/tick-owned! host (later 2))
          (is (= :no-op (:kind (dispositions/disposition (requests/get-request conn (:work.request/id request)))))))
        (let [before @calls wallet (resource/balance room)]
          (schedules/tick-owned! host (later 3))
          (is (= before @calls))
          (is (= wallet (resource/balance room)))
          (is (= :skipped (:schedule.occurrence/state (occurrence conn activation)))))
        (is (= (inc original-count) (count (requests/requests conn))))))
    (let [message (post! "Please incorporate the attached acquisition research when it becomes available.")]
      (with-redefs [gateway/*request-fn* (fn [& _] (swap! calls inc) (throw (ex-info "Scripted provider unavailable" {})))]
        (schedules/tick-owned! host (later 4))
        (let [failed (occurrence conn activation)
              {:keys [request job]} (await! failed message)
              outcome (filing/candidate-outcome conn principal (:room w) room (:work.request/id request))]
          (is (= :completed (:status job)) "The Job returned; the certified candidate failed")
          (is (= :failed (:outcome outcome)))
          (is (= (+ 2 original-count) (count (requests/requests conn))))
          (schedules/tick-owned! host (later 4))
          (let [summary (first (schedules/list-owned! host principal handle))
                row (schedules-model/activation conn (:schedule.activation/id activation))
                recorded (schedules-model/occurrence conn (:schedule.occurrence/id failed))
                notices (count (discourse/messages room)) before @calls wallet (resource/balance room)]
            (is (false? (:enabled? summary)))
            (is (= :execution-failed (:reason summary)))
            (is (= :batch-recovery-required (:resume-blocked summary)))
            (is (nil? (:schedule.activation/active-occurrence row)))
            (is (= (:candidate outcome) (:schedule.occurrence/failed-candidate recorded)))
            (is (= (:attempt-content outcome) (:schedule.occurrence/failed-attempt-content recorded)))
            (let [id (hasch/uuid [:simmis/schedule-completion-v1 (:schedule.occurrence/id failed)])
                  messages (filterv #(= id (:id %)) (discourse/messages room))]
              (is (= 1 (count messages)) "One truthful canonical failure notice")
              (is (str/includes? (:content (first messages)) "failed")))
            (is (nil? (dispositions/disposition (requests/get-request conn (:work.request/id request)))))
            (is (nil? (schedules-model/processing-proof conn (:schedule.occurrence/id failed))))
            (is (= :is.simm.ops.workflow-schedules/batch-recovery-required
                   (try (schedules/set-enabled-owned! host principal handle (:schedule.activation/id activation) true)
                        nil (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
            ;; The writer repeats this hold; bypassing host advice cannot enable it.
            (is (= :batch-recovery-required
                   (try (schedules-model/resume-managed! conn row)
                        nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
            (schedules/recover-owned! host)
            (schedules/tick-owned! host (later 5))
            (is (= row (schedules-model/activation conn (:schedule.activation/id activation))))
            (is (= before @calls) "Held failure never silently reruns")
            (is (= wallet (resource/balance room)))
            (is (= notices (count (discourse/messages room))) "Reconciliation never duplicates notices")
            (is (= (+ 2 original-count) (count (requests/requests conn))))
            (is (= current-company (:text (files/read-file-in @owned "wiki/Company.md"))))
            (let [bid (:work.request/knowledge-batch request)
                  batch (batches/batch @conn bid)
                  recover-input {:request (:work.request/id request) :batch bid
                                 :expected-claim (:knowledge.batch/claim batch)
                                 :expected-frozen (:knowledge.batch/frozen-id batch)
                                 :request-id (random-uuid) :reason "Retry current evidence in a fresh update"}
                  original-request (requests/get-request conn (:work.request/id request))
                  recover! #(dops/invoke host principal :simmis/workflow-failure-recover
                                         (assoc (update-vals recover-input (fn [v] (if (uuid? v) (str v) v))) :room handle))
                  recovered (recover!)]
              (is (= :recovered (:status recovered)))
              (is (false? (:dispatched? recovered)))
              (is (false? (:schedule-resumed? recovered)))
              (with-redefs [failed-settlement/verify! (fn [& _] (throw (AssertionError. "Historical replay entered native recovery")))]
                (is (= recovered (recover!))))
              (is (= row (schedules-model/activation conn (:schedule.activation/id activation))))
              (is (nil? (schedules-model/resume-blocked-reason @conn row)))
              (is (= recorded (schedules-model/occurrence conn (:schedule.occurrence/id failed))))
              (is (= original-request (requests/get-request conn (:work.request/id request))))
              (is (= before @calls))
              (is (= wallet (resource/balance room)))
              (is (= notices (count (discourse/messages room))))
              (is (submitted-model/recovered? @conn bid (:work.request/id request)))
              ;; Resume reauthorizes the original delegator and remains explicit.
              (schedules/set-enabled-owned! host principal handle (:schedule.activation/id activation) true)
              (is (true? (:schedule.activation/enabled? (schedules-model/activation conn (:schedule.activation/id activation)))))
              (schedules/set-enabled-owned! host principal handle (:schedule.activation/id activation) false))))))))

(deftest v6-automatic-room-originals-reach-reviewed-native-adoption-once
  (with-canonical-fixture
    (fn [{:keys [conn w host owned attached room]}]
      (d/transact conn (concat schedules-model/schema collections/schema adoptions/schema unlinked-model/schema submitted-model/schema))
      (d/transact conn [{:party/id (:owner w) :party/role :admin}])
      (let [source (starter/source 6) principal (ops/party-principal (:owner w)) handle (str (:room w))
            calls (atom 0) observed (atom {}) historical (atom nil) start jobs/start!
            balance-before (resource/balance room)
            provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
        (doseq [[path text] source] (write! owned (str "workflows/" starter/name "/" path) text))
        (write! owned "wiki/Company.md" original-company)
        (write! owned "captures/company.md" original-capture)
        (let [message (binding [ec/*execution-context* (:ctx room)]
                        (let [m (discourse/message (keyword "party" (str (:owner w))) :_room-log decisive-message nil {:role :user})]
                          (is (= :inserted (store/-store-message! (:store room) (:id room) m)))
                          m))
              execution (-> (saved/input source) (dissoc :request-id)
                            (assoc :name starter/name :params {} :expected-bundle (:id (catalog/bundle starter/name source)))
                            (assoc-in [:limits :settlement] "discard")
                            (assoc-in [:limits :max-dispatches] 2)
                            (assoc-in [:limits :max-model-steps] 2)
                            (assoc-in [:limits :timeout-ms] 60000)
                            (assoc :expected-owned-source
                                   (parse-uuid (get-in (sources/details! host principal handle {:name starter/name}) [:data :revision]))))
              activation (schedules/create-owned! host principal handle
                                                   {:request-id (random-uuid) :kind "workflow"
                                                    :cadence {:interval-ms 60000 :tz "UTC"} :execution execution})
              due (:schedule.activation/next-fire activation)
              next-due (java.util.Date. (+ 60000 (.getTime ^java.util.Date due)))
              attached-before (@#'occurrence-fixture/state attached)
              wallet balance-before]
          (is (zero? @calls))
          (is (= wallet (resource/balance room)) "Installation/activation does not fund work")
          ;; Fresh occurrences check full admission readiness before discovery,
          ;; even when discovery will find no work. Fund two bounded occurrences
          ;; so the second pass tests unchanged input rather than exhausted funds.
          ;; The production path never mints; the second pass must spend nothing.
          (resource/mint! room {:id (random-uuid)
                                :resources (update-vals (work/resources (:limits execution)) #(* 2M %))})
          (with-redefs [gateway/*request-fn*
                        (fn [& _]
                          (let [n (swap! calls inc)
                                o (occurrence conn activation)
                                rid (requests/request-id (:owner w) (:room w) (:schedule.occurrence/id o))
                                row (requests/get-request conn rid)
                                retained (requests/payload conn row)
                                link (get-in retained [:input :knowledge/collection])
                                receipt (collections/receipt @conn rid)]
                            (is (= :available (:room.collection/state receipt))
                                "No dispatch precedes original collection readiness")
                            (is (= rid (:collection link)))
                            (is (= (:room.collection/intent-id receipt) (:intent link)))
                            (is (= :write-acknowledged (:proof link)))
                            (if (= n 1)
                              (@#'scripted/tool-response (scripted-pages) "v6-read-originals-write-coverage")
                              (@#'scripted/final-response "Prepared coherent source-backed notes; screenshot uninspected."))))
                        providers/providers (atom {:codex-subscription provider})
                        native-rooms/room-ctx-for #(when (= (:room w) %) (:ctx room))
                        jobs/start! (fn [actual-room options actual-work & callbacks]
                                      (when-not (identical? actual-room room) (throw (AssertionError. "Unexpected Room")))
                                      (apply start actual-room options
                                             (fn [id]
                                               (swap! observed assoc id (get-in @(var-get #'jobs/live) [id :done]))
                                               (actual-work id)) callbacks))]
            (try
              (schedules/tick-owned! host due)
              (let [o (occurrence conn activation) rid (:schedule.occurrence/request o)
                    request (requests/get-request conn rid) job (:work.request/job request)
                    done (get @observed job)]
                (when-not (and (uuid? job) (instance? clojure.lang.IDeref done) (true? (deref done 120000 false)))
                  (throw (ex-info "The bounded v6 Job did not physically return" {:occurrence o :job job :calls @calls})))
                (@#'occurrence-fixture/join-fixture-jobs! room observed)
                (is (= :completed (:status (jobs/status job))))
                (is (= source (get-in (requests/payload conn request) [:input :source])))
                (let [retained (requests/payload conn request)
                      manifest (edn/read-string (get-in retained [:input :inputs "/in/knowledge/manifest.edn"]))
                      human (first (filter #(str/starts-with? (:path %) "inbox/conversations/room-activity/") (:files manifest)))]
                  (let [planned (runs/plan-source {:files source} {:name starter/name :expected-bundle (:id (catalog/bundle starter/name source))})
                        expected-program (hasch/uuid [:simmis/knowledge-program-v1
                                                      (select-keys (:workflow planned) [:name :bundle :case :params])
                                                      (:task planned) (:inputs planned)])]
                    (is (= expected-program (get-in (batch-proof/request! conn request retained) [:selection :stream :program]))
                        "HOST citation context does not change the semantic processing stream"))
                  (is (= {:version 2 :room (:room w) :request rid} (get-in retained [:input :knowledge/source-citations])))
                  (is (= 2 (:version manifest)))
                  (is (= 2 (count (:files manifest))))
                  (is (= #{:trigger} (set (map :role (:files manifest)))))
                  (is (str/includes? (get-in retained [:input :inputs (:input human)]) decisive-message))
                  (is (= original-capture (get-in retained [:input :inputs "/in/knowledge/sources/captures/company.md"])))
                  (is (not-any? #(str/starts-with? (:path %) "digests/") (:files manifest)))
                  (is (str/includes? (:path human) (str (:id message))))
                  (is (= original-company (:text (files/read-file-in @owned "wiki/Company.md"))))
                  ;; The Run observed the admission checkpoint. Change the live
                  ;; existing target before filing: BEFORE must still be original.
                  (write! owned "wiki/Company.md" (str original-company "\nLater human edit; do not overwrite.\n"))
                  (let [prepared (filing/prepare conn principal (:room w) room rid)
                        plan (:plan prepared) manifest (:manifest plan)
                        captured (outputs/validate!
                                  {:version (:version manifest) :scope (:scope manifest) :id (:output manifest)
                                   :files (into (sorted-map) (map (fn [[path pair]] [path (:after pair)])) (:bodies plan))})]
                    (is (not (contains? (get-in retained [:input :inputs]) "/in/knowledge/sources/wiki/Company.md")))
                    (is (= original-company (markdown/decode (outputs/bytes! (get-in plan [:bodies "wiki/Company.md" :before]))))
                        "Historical BEFORE comes from the Request checkpoint, never today's changed target")
                    (reset! historical {:input (:input retained) :basis (get-in retained [:input :knowledge/publication-basis])
                                        :output captured :request rid}))
                  (@#'app-fixture/isolated
                   (fn []
                     (application/start!)
                     (let [filed (output/collect! (:owner w) (:room w) rid)
                           diagnostic (when-not (= :proposed (:status filed))
                                        (filing-diagnostic conn principal (:room w) room rid))]
                       (is (= :proposed (:status filed)) (pr-str diagnostic))
                       (when-not (and (= :proposed (:status filed)) (string? (:proposal filed)))
                         (throw (ex-info "v6 native proposal filing did not return a proposal"
                                         {:status (:status filed) :diagnostic diagnostic})))
                       (let [proposal (parse-uuid (:proposal filed))]
                         (when-not proposal
                           (throw (ex-info "v6 native filing returned an invalid proposal identity"
                                           {:status (:status filed)})))
                         (application/with-operation!
                          (fn [_host]
                       (is (= 2 (:version (dispositions/disposition (requests/get-request conn rid)))))
                       (is (str/includes? (:text (files/read-file-in @owned "wiki/Company.md")) "Later human edit"))
                       (let [failure (try (controller/preview-reviewed! conn (:owner w) (:room w) room proposal)
                                          nil (catch clojure.lang.ExceptionInfo error (select-keys (ex-data error) [:type :reason])))]
                         (is (= {:type :is.simm.documents.view/refused :reason :stale-or-unselected-change} failure)
                             "Changed live target refuses review before any adoption claim"))
                       (is (empty? (d/datoms @conn :aevt :file.adoption/id)))
                       (write! owned "wiki/Company.md" original-company)
                       (let [review (controller/preview-reviewed! conn (:owner w) (:room w) room proposal)]
                         (is (= :reviewed (:status review)))
                         (write! owned "wiki/Company.md" (str original-company "\nLater human edit; do not overwrite.\n"))
                         (let [failure (try (controller/adopt-reviewed! conn (:owner w) (:room w) room proposal (:revision review))
                                            nil (catch clojure.lang.ExceptionInfo error (select-keys (ex-data error) [:type :reason])))]
                           (is (= {:type :is.simm.documents.view/refused :reason :stale-or-unselected-change} failure)
                               "A real earlier review cannot authorize overwriting changed current bytes"))
                         (is (empty? (d/datoms @conn :aevt :file.adoption/id)))
                         (write! owned "wiki/Company.md" original-company)
                         (let [fresh (controller/preview-reviewed! conn (:owner w) (:room w) room proposal)]
                           (is (= :reviewed (:status fresh)))
                           (is (= :accepted (:status (controller/adopt-reviewed! conn (:owner w) (:room w) room proposal (:revision fresh)))))))))))))
                  (let [company (:text (files/read-file-in @owned "wiki/Company.md"))
                        sam (:text (files/read-file-in @owned "wiki/Sam.md"))]
                    (is (str/starts-with? company original-company) "Exact identity, metadata and existing block survive")
                    (is (= (str company-id) (get-in (markdown/parse company) [:frontmatter :properties "simmis-id"])))
                    (is (str/includes? company "[Sam](Sam.md)"))
                    (is (str/includes? company "no client relationship is established"))
                    (is (str/includes? company "original screenshot was not inspected"))
                    (is (= (str sam-id) (get-in (markdown/parse sam) [:frontmatter :properties "simmis-id"])))
                    (is (str/includes? sam decisive-message) "A decisive original is not gated by digest inclusion")
                    (is (= original-capture (:text (files/read-file-in @owned "captures/company.md"))))
                    (is (= (get-in retained [:input :inputs (:input human)])
                           (:text (files/read-file-in @owned (:path human)))))))
                ;; The app's own canonical proposal notice must not hold original
                ;; collection as an unsupported system actor on the next pass.
                (let [next-intake (intake/preview-room! host principal handle {})]
                  (is (= :ready (:state next-intake))
                      (pr-str {:held (select-keys (:held next-intake) [:reason :message-id])
                               :source (when-let [id (get-in next-intake [:held :message-id])]
                                         (->> (:rows (intake/room-source
                                                      (is.simm.ops.owned-reads/resolve-host-read! host principal handle)))
                                              (some #(when (= id (:id %))
                                                       (select-keys % [:id :author :actor-kind :metadata])))))}))
                  (is (empty? (:artifacts next-intake)))
                  (is (= 1 (:retained-count next-intake))))
                ;; Reconcile accepted processing, then a fresh occurrence observes
                ;; unchanged originals; adopted pages cannot become new triggers.
                (schedules/tick-owned! host due)
                (let [before-calls @calls
                      before-balance (resource/balance room)]
                  (schedules/tick-owned! host next-due)
                  (is (= before-calls @calls))
                  (is (= before-balance (resource/balance room)) "An unchanged occurrence spends and reserves nothing")
                  (is (= :skipped (:schedule.occurrence/state (occurrence conn activation))))
                  (is (#{:no-inputs :unchanged} (:schedule.occurrence/reason (occurrence conn activation)))))
                (is (= attached-before (@#'occurrence-fixture/state attached)))
                (is (= 2 @calls))
                (is (= 1 (count @observed)))
                (@#'app-fixture/isolated
                 (fn []
                   (application/start!)
                   (next-original-and-failure-hold!
                    {:conn conn :w w :host host :owned owned :room room :activation activation
                     :due due :principal principal :handle handle :execution execution :observed observed})))
                (let [{:keys [input basis output request]} @historical
                      root (:work.checkpoint/id (request-roots/intent @conn request))]
                  (when-not (uuid? root) (throw (ex-info "Fixture lacks its real Request retention root" {})))
                  (write! owned "wiki/Company.md" original-company)
                  (is (= original-company (:text (files/read-file-in @owned "wiki/Company.md"))))
                  (is (contains? (roots/roots (:store @owned) {:sync? true}) root)
                      "The actual Request physical root exists before this disposable-fixture release")
                  (is (= original-company
                         (get (#'filing/retained-before-inputs conn (:room w) room request input basis output)
                              "/in/knowledge/sources/wiki/Company.md")))
                  (roots/release! @owned root {:sync? true})
                  (is (not (contains? (roots/roots (:store @owned) {:sync? true}) root)))
                  (let [failure (try (#'filing/retained-before-inputs conn (:room w) room request input basis output)
                                     nil (catch clojure.lang.ExceptionInfo error (select-keys (ex-data error) [:type :reason])))]
                    (is (= {:type :is.simm.ops.checkpoint-roots/held :reason :pin-unavailable} failure)
                        "Even matching current bytes cannot replace a missing historical Request root"))
                  (is (not (contains? (roots/roots (:store @owned) {:sync? true}) root))
                      "Historical reads never recreate the released root")))
              (finally (@#'occurrence-fixture/join-fixture-jobs! room observed)))))))))
