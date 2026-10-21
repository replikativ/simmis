(ns is.simm.ops.work-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as dh]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.run :as run]
            [dvergr.chat.schema :as chat-schema]
            [dvergr.discourse :as discourse]
            [dvergr.effects :as effects]
            [dvergr.jobs :as jobs]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as store]
            [dvergr.room.store.datahike :as datahike-store]
            [dvergr.rooms.forks :as forks]
            [dvergr.system.db :as system-db]
            [jsonista.core :as json]
            [is.simm.authority.world :as world]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work :as work]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- with-room [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :keep-history? true :schema-flexibility :write}
        id (keyword (str "bounded-work-" (random-uuid)))
        chat (random-uuid)]
    (dh/create-database cfg)
    (let [conn (dh/connect cfg)]
      (chat-schema/ensure-full-schema! conn)
      (dh/transact conn [(merge (chat-schema/create-chat-entity {:id chat :title (name id)})
                                {:room/slug (store/room-id->slug id) :room/type :internal})])
      (resource/install-connection! conn id chat)
      (let [room (discourse/make-room {:id id :store (datahike-store/make conn)})]
        (try
          (binding [ec/*execution-context* (:ctx room)]
            (registry/register! room)
            (resource/install-unit! room {:symbol resource/model-dispatches
                                          :name "Model dispatches" :precision 0})
            (f room))
          (finally
            (binding [ec/*execution-context* (:ctx room)] (registry/unregister! id))
            (governance/ungovern! conn)
            (discourse/close-room! room)))))))

(defn- fixture []
  (let [limits {:budget-microdollars 10000 :max-dispatches 2 :max-model-steps 2
                :timeout-ms 1000 :settlement "discard"}
        env (environment/make-environment
             {:id :work-test/echo :task "deterministic fixture"
              :verifier {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"}
              :limits {:timeout-ms 2000 :cancel-timeout-ms 1000}
              :world {:isolation :ctx :settlement :discard :effects work/effects
                      :resources (work/resources limits)}})
        team (roster/make-agent (roster/make-roster {:id :work-test/team})
                                {:id :candidate :program {:kind :echo}})]
    [{:request-id (random-uuid) :task "deterministic fixture" :model "fixture" :limits limits}
     {:team team :agent :candidate :environment env :evaluator work/completion
      :basis {:team team :environment env}}]))

(deftest one-submission-one-job-with-conserved-parent-and-child-wallets
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (let [[input prepared] (fixture)
              principal (ops/party-principal (:owner w))
              host-ctx ec/*execution-context*
              grant (work/resources (:limits input))]
          (resource/mint! room {:id (random-uuid) :resources grant})
          (let [r (work/submit-prepared! conn host-ctx principal (:room w) room input prepared)
                job (:job r)
                settled (jobs/status job 25000)]
            (is (= :completed (:status settled)))
            (is (:settled? settled))
            (is (= grant (resource/balance room)) "all unused parent and child resources returned")
            (is (every? zero? (vals (resource/run-balance room job))))
            (is (false? (:return-pending? (work/status conn host-ctx principal (:room w) (:id r)))))
            (is (not (contains? r :claim)))
            (is (not (contains? r :payload-ref)))
            (is (= job (:job (work/submit-prepared! conn host-ctx principal (:room w) room input prepared))))
            (is (= 1 (count (jobs/jobs room))))
            (let [changed (assoc-in prepared [:basis :changed] true)]
              (is (thrown? clojure.lang.ExceptionInfo
                           (work/submit-prepared! conn host-ctx principal (:room w) room input changed))))))))))

(deftest admission-failure-is-durable-and-cannot-be-replayed-as-new-work
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (let [[input prepared] (fixture)
              principal (ops/party-principal (:owner w))
              host-ctx ec/*execution-context*]
          (is (thrown? clojure.lang.ExceptionInfo
                       (work/submit-prepared! conn host-ctx principal (:room w) room input prepared)))
          (let [retry (work/submit-prepared! conn host-ctx principal (:room w) room input prepared)
                status (jobs/status (:job retry))]
            (is (= :failed (:status status)))
            (is (= 1 (count (jobs/jobs room))))
            (is (false? (:return-pending? retry)))))))))

(deftest generic-work-requires-explicit-room-authority-and-never-leaks-a-claim
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (let [[input prepared] (fixture)]
          (doseq [p [(:member w) (:non-member w) (:admin w) nil]]
            (is (thrown? clojure.lang.ExceptionInfo
                         (work/submit-prepared! conn ec/*execution-context* (ops/party-principal p)
                                                (:room w) room input prepared))))
          (is (empty? (requests/requests conn))))))))

(deftest native-preparation-pins-a-valid-final-environment
  (let [[input _] (fixture)
        prepared (work/prepare (assoc input :model "claude-opus-4-7"))]
    (is (= (:environment prepared) (environment/validate-environment (:environment prepared))))
    (is (= (:environment prepared) (get-in prepared [:basis :environment])))
    (doseq [bad [(assoc-in input [:limits :timeout-ms] 0)
                 (assoc-in input [:limits :max-dispatches] 100000)
                 (assoc-in input [:limits :settlement] "merge")
                 (assoc input :extra true)]]
      (is (thrown? clojure.lang.ExceptionInfo (work/prepare bad))))))

(deftest native-subscriptions-use-normal-interpreter-worlds-with-dispatch-limits
  (let [[input _] (fixture)
        prepared (work/prepare (assoc input :model "codex-subscription-sol"))
        agent (get-in prepared [:team :roster/agents (:agent prepared)])]
    (is (= :codex-subscription (get-in agent [:agent/model-policy :provider])))
    (is (= :llm (get-in agent [:agent/program :kind])))
    (is (= :ctx (get-in prepared [:environment :environment/world :isolation])))
    (is (= (work/resources (:limits input)) (get-in prepared [:environment :environment/world :resources])))
    (is (= 2 (get-in prepared [:environment :environment/limits :max-model-steps])))
    (doseq [model ["claude-code-sonnet" "codex-subscription-cli"]]
      (is (thrown? clojure.lang.ExceptionInfo (work/prepare (assoc input :model model)))))))

(deftest native-subscription-work-captures-results-and-keeps-review-merge-explicit
  (doseq [settlement ["discard" "review"]]
    (world/with-world [conn w]
      (with-room
        (fn [room]
          (workspace/ensure-workspace! room)
          ;; The predicate-only authority fixture omits persistent locators.
          ;; Actual Dvergr merge checks need the same room slug and canonical
          ;; registry scopes that real Simmis provisioning records.
          (dh/transact conn [{:room/id (:room w) :room/slug (store/room-id->slug (:id room))}])
          (dh/transact conn (mapv (fn [system]
                                    {:system/id (:system/id system)
                                     :system/scope (str "/fixture/systems/" (:system/id system))})
                                  (filter #(contains? #{:kb :repo} (:system/type %)) (system-db/all-systems))))
          (let [[base _] (fixture)
                input (-> base (assoc :model "codex-subscription-sol")
                          (assoc-in [:limits :timeout-ms] 30000)
                          (assoc-in [:limits :settlement] settlement))
                principal (ops/party-principal (:owner w))
                host-ctx ec/*execution-context*
                calls (atom 0)
                retained (volatile! nil)
                provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})
                response (fn [n]
                           (let [events (concat
                                         (when (= n 1)
                                           [{:type "response.output_item.done"
                                             :item {:type "function_call" :call_id "write-result" :name "clojure_eval"
                                                    :arguments (json/write-value-as-string
                                                                {:code "(do (babashka.fs/create-dirs \"/out\") (spit \"/out/result.md\" \"interpreter output\"))"})}}])
                                         (when (= n 2) [{:type "response.output_text.delta" :delta "Prepared the result."}])
                                         [{:type "response.completed" :response {:id (str "fixture-" n) :model "gpt-5.6-sol"
                                                                                 :usage {:input_tokens 100 :output_tokens 20}}}])]
                             {:status 200 :body (java.io.ByteArrayInputStream.
                                                 (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8"))}))]
            (try
              (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits input))})
              ;; Room continuations need not inherit the submitter's dynamic
              ;; bindings. Intercept actual gateway egress on every worker,
              ;; not just the REPL/test thread. Never use real credentials.
              (with-redefs [gateway/*request-fn* (fn [_] (response (swap! calls inc)))]
                (with-redefs [providers/providers (atom {:codex-subscription provider})]
                  (let [request (work/submit! conn host-ctx principal (:room w) room input)
                        status (jobs/status (:job request) 40000)
                        attempts (store/-list-attempts (:store room) (store/conversation-id room) {})
                        attempt (first attempts)
                        child (first (remove #(= (:job request) (:run/id %))
                                             (run/runs room {:root-run-id (:job request) :limit 10})))]
                    (vreset! retained (registry/lookup (:run/world child)))
                    (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
                    (is (:settled? status))
                    (is (= 2 @calls))
                    (is (= 1 (count attempts)))
                    (is (= {"/out/result.md" "interpreter output"} (get-in attempt [:attempt/evidence :files])))
                    (is (empty? (workspace/read-tree room "/out")) "Execution never writes trunk directly")
                    (is (= {resource/microdollars 10000M} (resource/balance room)))
                    (is (= (:job request) (:job (work/submit! conn host-ctx principal (:room w) room input))))
                    (is (= 2 @calls) "Request replay never runs the model again")
                    (if (= settlement "review")
                      (do
                        (is (some? @retained))
                        (is (= :review (:run/settlement-status child)))
                        (let [review (forks/review @retained)
                              merged (forks/merge! @retained :expect-state (:state review))]
                          (is (:ok? merged) (pr-str merged))
                          (is (= {"/out/result.md" "interpreter output"} (workspace/read-tree room "/out")))
                          (is (= :merged (:run/settlement-status (run/run room (:run/id child)))))))
                      (is (= :discarded (:run/settlement-status child)))))))
              (finally
                (when (and @retained (nil? (discourse/settlement @retained)))
                  (forks/discard! @retained))))))))))

(deftest allowance-provisioning-is-explicit-authorized-and-idempotent
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (let [host-ctx ec/*execution-context*
              owner (ops/party-principal (:owner w))
              input {:request-id (random-uuid) :budget-microdollars 10000 :max-dispatches 2}]
          (doseq [party [(:member w) (:non-member w) (:admin w)]]
            (is (thrown? clojure.lang.ExceptionInfo
                         (work/provision! conn host-ctx (ops/party-principal party)
                                          (:room w) room input))))
          (let [result (work/provision! conn host-ctx owner (:room w) room input)]
            (is (= :provisioned (:state result)))
            (is (= result (work/provision! conn host-ctx owner (:room w) room input)))
            (is (= (work/resources input) (resource/balance room)))
            (is (thrown? clojure.lang.ExceptionInfo
                         (work/provision! conn host-ctx owner (:room w) room
                                          (update input :budget-microdollars inc))))
            (work/recover! conn host-ctx (constantly room))
            (is (= :provisioned (:state (work/status conn host-ctx owner (:room w) (:id result)))))))))))

(deftest cancellation-does-not-refund-until-child-physically-returns-its-wallet
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (let [[input _] (fixture)
              entered (promise)
              release (java.util.concurrent.CountDownLatch. 1)
              protocol (evaluation/make-protocol
                        {:id :work-test/blocked :version 1
                         :run (fn [_]
                                (deliver entered true)
                                ;; Model a foreign operation still unwinding
                                ;; after cancellation; only its owner releases it.
                                (loop []
                                  (when-not (try (.await release) true
                                                 (catch InterruptedException _ false))
                                    (recur)))
                                :done)})
              env (environment/make-environment
                   {:id :work-test/cancel :task "blocked"
                    :verifier {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"}
                    :limits {:timeout-ms 60000 :cancel-timeout-ms 1000}
                    :world {:isolation :ctx :settlement :discard
                            :protocol (evaluation/protocol-ref protocol)
                            :resources (work/resources (:limits input))}})
              team (roster/make-agent (roster/make-roster {:id :work-test/team})
                                      {:id :candidate :program {:kind :echo}})
              prepared {:team team :agent :candidate :environment env
                        :evaluator work/completion :protocol protocol
                        :basis {:team team :environment env}}
              owner (ops/party-principal (:owner w))
              host-ctx ec/*execution-context*
              grant (work/resources (:limits input))]
          (try
            (resource/mint! room {:id (random-uuid) :resources grant})
            (let [result (work/submit-prepared! conn host-ctx owner (:room w) room input prepared)
                  job (:job result)]
              (is (= true (deref entered 10000 ::timeout)))
              (run/cancel-run! job)
              (is (thrown? clojure.lang.ExceptionInfo
                           (evaluation/await-cleanups-for! room job 1)))
              (is (:return-pending? (work/status conn host-ctx owner (:room w) (:id result))))
              (is (every? zero? (vals (resource/balance room))))
              (.countDown release)
              (is (:settled? (jobs/status job 25000)))
              (is (= grant (resource/balance room)))
              (is (false? (:return-pending? (work/status conn host-ctx owner (:room w) (:id result))))))
            (finally (.countDown release))))))))

(deftest trusted-adapter-failure-cleans-up-once-and-does-not-replay
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (let [[input prepared] (fixture)
              events (atom [])
              owner (ops/party-principal (:owner w))
              host-ctx ec/*execution-context*
              grant (work/resources (:limits input))
              prepared (assoc prepared
                              :admit! (fn [_] (swap! events conj :admitted))
                              :start! (fn [_] (throw (ex-info "construction failed" {})))
                              :settled! (fn [_] (swap! events conj [:settled (resource/balance room)])))]
          (resource/mint! room {:id (random-uuid) :resources grant})
          (is (thrown? clojure.lang.ExceptionInfo
                       (work/submit-prepared! conn host-ctx owner (:room w) room input prepared)))
          (is (= [:admitted [:settled grant]] @events))
          (is (= :failed (:status (jobs/status (:job (work/submit-prepared! conn host-ctx owner
                                                                            (:room w) room input prepared))))))
          (is (= [:admitted [:settled grant]] @events)))))))

(deftest unstarted-adapter-provisioning-is-abandoned-without-a-job
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (let [[input prepared] (fixture)
              abandoned (atom 0)
              prepared (assoc prepared :job-context! (fn [_] (throw (ex-info "provision failed" {})))
                              :abandon! (fn [_] (swap! abandoned inc)))]
          (is (thrown? clojure.lang.ExceptionInfo
                       (work/submit-prepared! conn ec/*execution-context* (ops/party-principal (:owner w))
                                              (:room w) room input prepared)))
          (is (= 1 @abandoned))
          (is (empty? (jobs/jobs room))))))))

(deftest scoring-worker-retains-adapter-capacity-until-physical-completion
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (let [[input prepared] (fixture)
              entered (promise)
              release (java.util.concurrent.CountDownLatch. 1)
              retired (atom 0)
              evaluator (evaluation/make-evaluator
                         {:id :simmis/work-completion :version 1 :basis "simmis-work-completion-v1"
                          :observe (fn [_] {})
                          :verify (fn [_ _]
                                    (deliver entered true)
                                    (loop []
                                      (when-not (try (.await release) true
                                                     (catch InterruptedException _ false))
                                        (recur)))
                                    {:checks {:done? true} :reward 1.0})})
              prepared (assoc prepared :evaluator evaluator :settled! (fn [_] (swap! retired inc)))
              owner (ops/party-principal (:owner w))
              host-ctx ec/*execution-context*
              grant (work/resources (:limits input))]
          (try
            (resource/mint! room {:id (random-uuid) :resources grant})
            (let [row (work/submit-prepared! conn host-ctx owner (:room w) room input prepared)
                  job (:job row)]
              (is (= true (deref entered 10000 ::timeout)))
              (run/cancel-run! job)
              (is (thrown? clojure.lang.ExceptionInfo (evaluation/await-cleanups-for! room job 1)))
              (is (zero? @retired))
              (is (:return-pending? (work/status conn host-ctx owner (:room w) (:id row))))
              (.countDown release)
              (is (:settled? (jobs/status job 25000)))
              (is (= 1 @retired))
              (is (= grant (resource/balance room))))
            (finally (.countDown release))))))))

(deftest boot-recovery-reclaims-dead-descendant-wallets-but-not-held-exposure
  (doseq [kind [:task :coordinator]]
    (world/with-world [conn w]
      (with-room
        (fn [room]
          (let [[input prepared] (fixture)
                host-ctx ec/*execution-context*
                grant (work/resources (:limits input))
                row (:request (requests/claim! conn {:party (:owner w) :room (:room w)
                                                     :client-id (:request-id input) :kind kind
                                                     :intent (:basis prepared)
                                                     :limits (update (:limits input) :settlement keyword)}))
                job (:run/id (run/start! room :test (random-uuid) nil {:kind :workflow}))
                child (:run/id (run/start! room :test (random-uuid) nil {:parent job}))]
            (resource/mint! room {:id (random-uuid) :resources grant})
            (requests/record-job! conn (:work.request/id row) (:work.request/claim row) job)
            (resource/allocate-run! room job nil grant)
            (resource/allocate-run! room child job grant)
          ;; A held external exposure has left the Run wallet. Simulate a
          ;; committed dispatch debit; recovery must never mint that back.
            (resource/consume! room child {:id (random-uuid)
                                           :resources {resource/model-dispatches 1M}})
            (let [reservation (binding [resource/*model-scope* (resource/model-scope room child)
                                        resource/*spend-wallet* {:payer {:room room :wallet child}}
                                        resource/*dispatch-bound* 2500M
                                        resource/*dispatches* (atom [])]
                                (resource/reserve-dispatch!))]
              (run/finish! child :failed {:reason :orphaned})
              (run/finish! job :failed {:reason :orphaned})
              (work/recover! conn host-ctx (constantly room))
              (is (= {resource/microdollars 7500M resource/model-dispatches 1M}
                     (resource/balance room)))
              (is (= {resource/microdollars 2500M}
                     (resource/balance room [:kontor.resource-account/id (:wallet reservation)])))
              (is (false? (:return-pending? (work/status conn host-ctx (ops/party-principal (:owner w))
                                                         (:room w) (:work.request/id row)))))
              (work/recover! conn host-ctx (constantly room))
              (is (= {resource/microdollars 7500M resource/model-dispatches 1M}
                     (resource/balance room))))))))))

(deftest speculative-work-denies-irreversible-and-undeclared-effects
  (let [{:keys [specs release]} (effects/environment-handlers! work/effects)
        boundary (constantly {:handlers (effects/handlers specs)})
        reached (atom 0)]
    (try
      (doseq [effect [{:effect :room/post :resource {:room "r"}}
                      {:effect :room/join :resource {:room "r" :who "x"}}
                      {:effect :room/merge :resource {:room "r" :fork "f"}}
                      {:effect :run/start :resource {}}
                      {:effect :deps/add :resource {:libs ["untrusted"]}}
                      {:effect :process/run :resource {:cmd "touch /outside"}}
                      {:effect :channel/call :resource {:tool "telegram"}}
                      {:effect :tool/call :resource {:tool "unknown"}}
                      {:effect :actor/write :resource {:op :update}}
                      {:effect :http/request :resource {:url "https://example.org" :method "GET"}}
                      {:effect :http/request :resource {:url "https://example.org" :method "POST"}}]]
        (let [error (try (effects/perform! boundary effect #(swap! reached inc)) nil
                         (catch clojure.lang.ExceptionInfo e e))]
          (is (= :effect/denied (:type (ex-data error))) (pr-str effect))))
      (is (zero? @reached))
      (is (= :read (effects/perform! boundary {:effect :fs/read :resource {:path "/out/result"}}
                                     (constantly :read))))
      (is (= :model (effects/perform! boundary {:effect :model/call :resource {:model "fixture"}}
                                      (constantly :model)))
          "provider admission is separate from sandbox HTTP, not denied by empty allow-hosts")
      (finally (release)))))

(deftest concurrent-client-retries-cannot-allocate-or-execute-twice
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (let [[input prepared] (fixture)
              host-ctx ec/*execution-context*
              owner (ops/party-principal (:owner w))
              start (promise)
              grant (work/resources (:limits input))]
          (resource/mint! room {:id (random-uuid) :resources grant})
          (let [workers (mapv (fn [_]
                                (future @start
                                        (work/submit-prepared! conn host-ctx owner (:room w)
                                                               room input prepared))) (range 4))]
            (deliver start true)
            (let [results (mapv #(deref % 10000 ::timeout) workers)
                  job (some :job results)]
              (is (not-any? #{::timeout} results))
              (is (= 1 (count (set (map :id results)))))
              ;; A concurrent observer may catch admission before its pointer
              ;; commits; it must never launch a replacement for that window.
              (is (uuid? job))
              (is (:settled? (jobs/status job 25000)))
              (is (= 1 (count (jobs/jobs room))))
              (is (= grant (resource/balance room))))))))))

(deftest pointerless-boot-admission-is-interrupted-not-restarted
  (doseq [kind [:task :coordinator]]
    (world/with-world [conn w]
      (with-room
        (fn [room]
          (let [[input prepared] (fixture)
                host-ctx ec/*execution-context*
                owner (ops/party-principal (:owner w))]
            (requests/claim! conn {:party (:owner w) :room (:room w)
                                   :client-id (:request-id input) :kind kind
                                   :intent (:basis prepared)
                                   :limits (update (:limits input) :settlement keyword)})
            (work/recover! conn host-ctx (constantly room))
            (let [retry (work/submit-prepared! conn host-ctx owner (:room w) room input
                                               (assoc prepared :request-kind kind))]
              (is (= :interrupted (:state retry)))
              (is (nil? (:job retry)))
              (is (empty? (jobs/jobs room))))))))))
