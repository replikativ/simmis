(ns is.simm.ops.benchmark-checkpoint-native-test
  "Full owned native admission on real file-backed profiled Rooms. Only model
   HTTP is stubbed. Failed native teardown retains its test host for recovery."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.experiment :as experiment]
            [dvergr.agent.run :as run]
            [dvergr.jobs :as jobs]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as store]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.substrate.paths :as paths]
            [dvergr.system.db :as native-control]
            [dvergr.system.rooms :as native]
            [is.simm.authority.world :as world]
            [is.simm.model.benchmark-resolutions :as journal]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-case-manifest :as manifests]
            [is.simm.ops.benchmark-fixture :as fixture]
            [is.simm.ops.benchmark-fixture-test :as fixture-test]
            [is.simm.ops.benchmark-fixture-snapshot :as snapshot]
            [is.simm.ops.benchmark-resolution :as resolution]
            [is.simm.ops.benchmark-resolution-test :as source-fixture]
            [is.simm.ops.owned-benchmark-admission :as admission]
            [is.simm.ops.owned-benchmark-admission-test :as answers]
            [is.simm.ops.owned-mcp :as mcp]
            [is.simm.ops.work :as work]
            [is.simm.runtimes.context :as server]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [jsonista.core :as json]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]))

(defonce ^:private held-host (atom nil))

(defn- with-native-host* [f]
  (when @held-host (throw (ex-info "Previous test host needs native reconciliation" {})))
  (let [conn (world/fresh-system-conn) w (world/seed! conn)
        root (context/create-execution-context)
        directory (.toFile (java.nio.file.Files/createTempDirectory
                            "simmis-native-checkpoint-" (make-array java.nio.file.attribute.FileAttribute 0)))
        contexts (atom {}) errors (atom {}) configs (atom {})
        owner {:conn conn :root root :directory directory :contexts contexts :errors errors :configs configs}
        config! (fn [scope file?]
                  (get (swap! configs
                              #(if (contains? % scope) %
                                   (assoc % scope
                                          (cond-> {:store (cond-> {:backend (if file? :file :memory) :id (random-uuid)}
                                                            file? (assoc :path (str (io/file scope "datahike"))))
                                                   :schema-flexibility :write :keep-history? false :commit-graph? true}
                                            file? (assoc :attribute-refs? false :crypto-hash? true))))) scope))]
    (with-redefs-fn
      {#'system-db/get-conn (constantly conn) #'native-control/get-conn (constantly conn)
       #'server/server-context root #'paths/systems-dir (constantly (str directory))
       #'native/room-ctxs contexts #'native/room-hydration-errors errors
       #'native/msgs-cfg #(config! % false) #'native/kb-cfg #(config! % false)
       #'workspace/repository-config #(config! % true)}
      #(binding [ec/*execution-context* root]
         (let [outcome (try {:ok (f (assoc owner :w w))} (catch Throwable error {:error error}))
               cleanup (try
                         ;; Never let shutdown force-release an unresolved test
                         ;; owner or hide a half-retired context after a failure.
                         (when (or (seq (registry/list-rooms)) (seq @contexts) (seq @errors))
                           (throw (ex-info "Native test host is not quiescent" {})))
                         (doseq [cfg (vals @configs) :when (d/database-exists? cfg)]
                           (let [opened (d/connect cfg)]
                             (governance/ungovern! opened)
                             (d/release opened))
                           (d/delete-database cfg))
                         (governance/ungovern! conn)
                         (let [cfg (:config @conn)] (d/release conn) (d/delete-database cfg))
                         (context/close-context! root)
                         (doseq [file (reverse (file-seq directory))] (io/delete-file file true))
                         nil
                         (catch Throwable error error))]
           (when cleanup
             (reset! held-host owner)
             (when-let [primary (:error outcome)] (.addSuppressed cleanup primary))
             (throw cleanup))
           (when-let [error (:error outcome)] (throw error))
           (:ok outcome))))))

(defn- with-native-host [f]
  (let [original @#'gateway/*request-fn*
        deny (fn [& _] (throw (AssertionError. "No live model HTTP in a native test host")))]
    ;; Scoped scripted responses restore this guard, including during cleanup.
    ;; If native ownership remains unresolved, keep the verifier quarantined
    ;; behind the guard until explicit recovery or verifier process exit.
    (alter-var-root #'gateway/*request-fn* (constantly deny))
    (try (with-native-host* f)
         (finally
           (when-not @held-host
             (alter-var-root #'gateway/*request-fn* (constantly original)))))))

(defn- source-files []
  (assoc (@#'source-fixture/source)
         "checker.clj" "(ns checker) (defn check [{:keys [files gold]}] (let [ok (= (:expected gold) (get files \"/out/result.md\"))] {:checks {:exact-output? ok} :reward (if ok 1 0)}))"
         "gold.edn" "{:secret \"HOST-ONLY-GOLD-CANARY\"}"
         "cases/a/gold.edn" "{:expected \"ALPHA\"}"
         "cases/b/gold.edn" "{:expected \"BETA\"}"
         "cases/a/fixtures/docs/input.md" "ALPHA"
         "cases/b/fixtures/docs/input.md" "BETA"
         "cases/a/fixtures/a-only.txt" "ONLY-ALPHA"
         "cases/b/fixtures/b-only.txt" "ONLY-BETA"))

(defn- response [n]
  (let [code "(let [text (slurp \"/docs/input.md\") a? (= text \"ALPHA\")] (assert (babashka.fs/exists? (if a? \"/a-only.txt\" \"/b-only.txt\"))) (assert (not (babashka.fs/exists? (if a? \"/b-only.txt\" \"/a-only.txt\")))) (assert (not (babashka.fs/exists? \"/workflows/echo/gold.edn\"))) (babashka.fs/create-dirs \"/out\") (spit \"/out/result.md\" text))"
        events [(if (odd? n)
                  {:type "response.output_item.done"
                   :item {:type "function_call" :call_id (str "native-" n) :name "clojure_eval"
                          :arguments (json/write-value-as-string {:code code})}}
                  {:type "response.output_text.delta" :delta "Done"})
                {:type "response.completed"
                 :response {:id (str "native-checkpoint-" n) :model "gpt-5.6-sol"
                            :usage {:input_tokens 10 :output_tokens 10}}}]]
    {:status 200 :body (java.io.ByteArrayInputStream.
                        (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8"))}))

(defn- with-benchmark [f]
  (with-native-host
    (fn [{:keys [conn w root] :as env}]
      (d/transact conn [{:party/id (:owner w) :party/role :admin}])
      (@#'fixture-test/with-template
       (fn []
         (let [nonce (random-uuid) source (fixture/create! root nonce)
               source-id (:persistent-id source) room (:room source)
               app (doto (lifetime/create) lifetime/open!)]
           (try
             (d/transact conn [{:room/id source-id :room/owner [:party/id (:owner w)]}])
             (let [files (source-files)
                   source-conn (:conn (native/default-workspace source-id))
                   _ (@#'answers/seed! source-conn files)
                   before (manifests/inventory @source-conn)
                   host (mcp/host conn root app) principal (@#'answers/principal w)
                   input (-> (@#'answers/input :benchmark host (assoc w :room source-id) files)
                             (assoc-in [:limits :timeout-ms] 30000))
                   grant (work/resources (:limits input))]
               (resource/install-unit! room {:symbol resource/model-dispatches :name "Model dispatches" :precision 0})
               (resource/mint! room {:id (random-uuid) :resources grant})
               (f (assoc env :source-id source-id :room room :app app :source-conn source-conn
                         :before before :host host :principal principal :input input :grant grant)))
             (finally
               (lifetime/fence! app) (lifetime/join! app 3000)
               (fixture/retire! root nonce source)))))))))

(deftest full-owned-checkpoint-job-certifies-results-and-retires-native-runtime
  (with-benchmark
    (fn [{:keys [conn root source-id room source-conn before host principal input]}]
      (let [calls (atom 0) transcripts (atom [])
            provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
        (with-redefs [providers/providers (atom {:codex-subscription provider})
                      gateway/*request-fn* (fn [http]
                                             (swap! transcripts conj (:body http))
                                             (response (swap! calls inc)))]
          (let [submission (admission/submit-native! host principal (str source-id) input)
                status (jobs/status (:job submission) 45000)
                row (requests/get-request conn (:id submission))
                attempts (store/-list-attempts (:store room) (store/conversation-id room) {})
                scorecards (store/-list-scorecards (:store room) (store/conversation-id room) {})
                fixture-id (:work.request/fixture-room row)]
            (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
            (is (:settled? status))
            (is (= :ready (:benchmark.resolution/state (journal/resolution @conn (:id submission)))))
            (is (uuid? fixture-id))
            (is (some? (:room/archived-at (d/entity @conn [:room/id fixture-id]))))
            (is (nil? (native/room-ctx-for fixture-id)))
            (is (nil? (registry/lookup (keyword (fixture/slug (:id submission))))))
            (is (= 2 (count attempts)))
            (is (every? experiment/verdict? attempts))
            (is (= #{"ALPHA" "BETA"} (set (map #(get-in % [:attempt/evidence :files "/out/result.md"]) attempts))))
            (is (= 1 (count scorecards)))
            (is (= 2 (get-in (first scorecards) [:scorecard/summary 0 :passed-count])))
            (is (= 4 @calls))
            (is (not-any? #(re-find #"HOST-ONLY-GOLD-CANARY" %) @transcripts))
            (is (= before (manifests/inventory @source-conn)))
            (is (false? (:return-pending? (work/status conn root principal source-id (:id submission)))))
            (let [retry (admission/submit-native! host principal (str source-id) input)]
              (is (= (select-keys submission [:id :job]) (select-keys retry [:id :job])))
              (is (= (work/status conn root principal source-id (:id submission)) retry))
              (is (= 4 @calls)))))))))

(deftest cancelled-or-revoked-admission-never-dispatches-and-releases-its-runtime
  (doseq [scenario [:cancel-after-c0 :revoke-before-ready]]
    (with-benchmark
      (fn [{:keys [conn w root source-id room source-conn before host principal input grant]}]
        (let [capture snapshot/capture! ready resolution/publish-ready!
              captures (atom 0) interrupted (atom 0) calls (atom 0)
              request-id (requests/request-id (:owner w) source-id (:request-id input))]
          (with-redefs [snapshot/capture!
                        (fn [host-context id receipt]
                          (let [result (capture host-context id receipt)]
                            (when (and (= :cancel-after-c0 scenario) (= 1 (swap! captures inc)))
                              (swap! interrupted inc)
                              (is (true? (run/cancel-run! (:work.request/job (requests/get-request conn id))))))
                            result))
                        resolution/publish-ready!
                        (fn [& args]
                          (when (= :revoke-before-ready scenario)
                            (swap! interrupted inc)
                            (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]]))
                          (apply ready args))
                        gateway/*request-fn* (fn [& _] (swap! calls inc) (throw (AssertionError. "Admission refused before any model")))]
            (is (thrown? clojure.lang.ExceptionInfo
                         (admission/submit-native! host principal (str source-id) input))))
          (is (= 1 @interrupted))
          (let [row (requests/get-request conn request-id)
                status (jobs/status (:work.request/job row) 10000)
                fixture-id (:work.request/fixture-room row)]
            (is (#{:cancelled :failed} (:status status)))
            (is (:settled? status))
            (is (not= :ready (:benchmark.resolution/state (journal/resolution @conn request-id))))
            (is (empty? (store/-list-attempts (:store room) (store/conversation-id room) {})))
            (is (empty? (store/-list-scorecards (:store room) (store/conversation-id room) {})))
            (is (zero? @calls))
            (is (= grant (resource/balance room)))
            (is (some? (:room/archived-at (d/entity @conn [:room/id fixture-id]))))
            (is (nil? (native/room-ctx-for fixture-id)))
            (is (nil? (registry/lookup (keyword (fixture/slug request-id)))))
            (is (= before (manifests/inventory @source-conn)))))))))
