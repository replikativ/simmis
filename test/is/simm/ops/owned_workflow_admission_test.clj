(ns is.simm.ops.owned-workflow-admission-test
  "Inactive source/admission composition. The trusted claim-only producer below
   writes REAL canonical intent, but starts NO Job/model and makes no Run proof."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.jobs :as jobs]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.run :as run]
            [dvergr.model.api.codex-subscription :as codex]
            [dvergr.model.gateway :as gateway]
            [dvergr.model.providers :as providers]
            [dvergr.resource :as resource]
            [dvergr.room.store :as store]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.ops.knowledge-inputs :as knowledge]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workflow-admission :as owned]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflow-runs-test :as saved]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.context :as context]
            [jsonista.core :as json]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- seed! [conn files]
  (doseq [[path text] files]
    (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8"))))

(defn- principal [w] (ops/party-principal (:owner w)))
(defn- opened-input [host w files]
  (assoc (saved/input files)
         :expected-owned-source (parse-uuid (get-in (sources/details! host (principal w) (str (:room w)) {:name "echo"})
                                                    [:data :revision]))))

(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))
(defn- state [conn] {:commit (d/commit-id @conn) :config (:config @conn) :tree (query/worktree @conn)})

(defn- claim-only
  "Synthetic trusted test producer: actual artifact+canonical claim+status,
   no Job start, wallet, provider request or physical execution."
  [seen]
  (fn [conn ctx p room-id room input prepared]
    (reset! seen {:prepared prepared :input input :room room :room-id room-id})
    (let [party (work/authorize! conn p room-id)
          {:keys [request]} (requests/claim! conn {:party party :room room-id :client-id (:request-id input)
                                                   :kind (:request-kind prepared) :intent (:basis prepared)
                                                   :limits (update (:limits input) :settlement keyword)})]
      (work/status conn ctx p room-id (:work.request/id request)))))

(deftest one-owned-db-pins-code-live-knowledge-and-publication-before-canonical-intent
  (doseq [order [true false]]
    (@#'fixture/with-fixture
     order
     (fn [{:keys [conn w host owned attached]}]
       (seed! owned saved/knowledge-source)
       (seed! attached (assoc saved/knowledge-source "helpers/data.txt" "OTHER code"))
       (repo/write! owned "inbox/evidence.md" (.getBytes "Owned evidence" "UTF-8"))
       (repo/write! owned "wiki/context.md" (.getBytes "Owned context" "UTF-8"))
       (repo/write! attached "inbox/evidence.md" (.getBytes "OTHER evidence" "UTF-8"))
       (repo/write! attached "wiki/context.md" (.getBytes "OTHER context" "UTF-8"))
       (let [input (opened-input host w saved/knowledge-source)
             snapshot workflows/snapshot-in resolve reads/resolve-host-read!
             prepare work/prepare gate work/authorize-in!
             seen (atom nil) binding (atom nil) calls (atom {:resolve 0 :snapshot 0 :prepare 0})
             gate-dbs (atom []) other (state attached)]
         (with-redefs [reads/resolve-host-read!
                       (fn [& args] (swap! calls update :resolve inc)
                         (let [value (apply resolve args)] (reset! binding value) value))
                       workflows/snapshot-in
                       (fn [db name]
                         (swap! calls update :snapshot inc)
                         (let [captured (snapshot db name)]
                           ;; The later knowledge resolution MUST still use db.
                           (repo/write! owned "workflows/echo/helpers/data.txt" (.getBytes "Later code" "UTF-8"))
                           (repo/write! owned "wiki/context.md" (.getBytes "Later context" "UTF-8"))
                           captured))
                       work/authorize-in! (fn [db p id] (swap! gate-dbs conj db) (gate db p id))
                       work/prepare (fn [& args] (swap! calls update :prepare inc) (apply prepare args))
                       admission/default-target (fn [& _] (throw (AssertionError. "No ambient target resolution")))
                       workflows/snapshot (fn [& _] (throw (AssertionError. "No selected source acquisition")))
                       work/submit-prepared! (claim-only seen)]
           (let [result (owned/submit! host (principal w) (str (:room w)) input)
                 basis (get-in @seen [:prepared :basis])
                 retained (get-in (requests/payload conn (requests/get-request conn (:id result))) [:input])]
             (is (uuid? (:id result)))
             (is (= :admitting (:state result)))
             (is (nil? (:job result)) "Claim-only fixture is NOT evidence of an executing Job")
             (is (= {:resolve 1 :snapshot 1 :prepare 1} @calls))
             (is (= :host (get-in @seen [:prepared :submission-channel])))
             (is (= saved/knowledge-source (:source basis)))
             (is (= "Owned context" (get-in basis [:inputs "/in/knowledge/sources/wiki/context.md"])))
             (is (= "Owned evidence" (get-in basis [:inputs "/in/knowledge/sources/inbox/evidence.md"])))
             (is (= (:target @binding) (get-in basis [:knowledge/publication-basis :target])))
             (is (= input (:submission basis) (:submission retained)))
             (is (= (:expected-owned-source input) (get-in retained [:source-origin :revision])))
             (is (= (:target-stamp @binding) (get-in retained [:source-origin :target-stamp])))
             (is (= (workflows/manifest-in (:db @binding) "echo") (get-in retained [:source-origin :manifest])))
             (is (some #(identical? (:control-db @binding) %) @gate-dbs))
             (is (not (contains? (:source-origin retained) :db)))
             (is (not (contains? (:source-origin retained) :conn)))))
         (is (= other (state attached))))))))

(deftest exact-durable-retry-needs-no-default-hydration-source-or-provider
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host owned hydrations]}]
     (seed! owned saved/source)
     (let [input (opened-input host w saved/source) seen (atom nil)
           first (with-redefs [work/submit-prepared! (claim-only seen)]
                   (owned/submit! host (principal w) (str (:room w)) input))
           before (requests/get-request conn (:id first))
           calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Exact retry touched fresh work")))]
       (repo/write! owned "workflows/echo/helpers/data.txt" (.getBytes "New source" "UTF-8"))
       (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository]])
       (reset! hydrations [])
       (with-redefs [reads/resolve-host-read! forbidden workflows/manifest-in forbidden
                     workflows/snapshot-in forbidden work/prepare forbidden runs/prepare forbidden
                     runs/resolve-admission-in forbidden work/submit-prepared! forbidden]
         (is (= first (owned/submit! host (principal w) (str (:room w)) input)))
         (is (= ::owned/conflict
                (:type (ex-data (failure #(owned/submit! host (principal w) (str (:room w))
                                                         (assoc input :expected-owned-source (random-uuid))))))))
         (is (= ::owned/invalid
                (:type (ex-data (failure #(owned/submit! host (principal w) (str (:room w))
                                                         (dissoc input :expected-owned-source)))))))
         (is (zero? @calls))
         (is (empty? @hydrations)))
       (is (= (:work.request/intent-id before) (:work.request/intent-id (requests/get-request conn (:id first)))))
       (is (= input (get-in (requests/payload conn before) [:input :submission])))))))

(deftest invalid-and-denied-inputs-refuse-before-hydration-or-source
  (@#'fixture/with-fixture
   false
   (fn [{:keys [w host hydrations]}]
     (let [input (assoc (saved/input) :expected-owned-source (random-uuid))]
       (doseq [bad [(dissoc input :expected-owned-source) (assoc input :expected-owned-source "1-1-1-1-1")
                    (assoc input :room (str (:room w))) (assoc input :name "echo\n")]]
         (is (= ::owned/invalid (:type (ex-data (failure #(owned/submit! host (principal w) (str (:room w)) bad)))))))
       (doseq [p [(ops/party-principal (:member w)) (ops/party-principal (:non-member w))
                  (assoc (principal w) :principal/kind :worker)]]
         (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit! host p (str (:room w)) input)))))))
       (is (empty? @hydrations))))))

(deftest stale-owned-source-is-normal-conflict-before-bodies-or-preparation
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host owned attached attached-descriptor]}]
     (seed! owned saved/source) (seed! attached saved/source)
     (let [input (opened-input host w saved/source) calls (atom 0)
           forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Stale admission read or prepared")))]
       (d/transact conn [{:room/id (:room w) :room/default-repository
                          [:system/id (:registry-system attached-descriptor)]}])
       (with-redefs [content/read-by-id forbidden workflows/snapshot-in forbidden work/prepare forbidden
                     runs/resolve-admission-in forbidden work/submit-prepared! forbidden]
         (is (= ::owned/conflict (:type (ex-data (failure #(owned/submit! host (principal w) (str (:room w)) input))))))
         (is (zero? @calls)))))))

(deftest original-readable-slug-cannot-be-retargeted-between-preflight-and-hydration
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn w host owned room]}]
     (seed! owned saved/source)
     (let [input (opened-input host w saved/source)
           slug (:room/slug (d/entity @conn [:room/id (:room w)]))
           resolve reads/resolve-host-read! calls (atom 0)
           forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Retargeted request read source")))]
       (d/transact conn [{:room/id (:other-room w) :room/slug (str "other-" (:other-room w))
                          :room/owner [:party/id (:owner w)]}])
       (with-redefs [reads/resolve-host-read!
                     (fn [& args]
                       ;; Both canonical rows are readable by this human. The
                       ;; changed ORIGINAL slug may not change the request ID.
                       (d/transact conn [{:room/id (:room w) :room/slug (str "old-" (:room w))}
                                         {:room/id (:other-room w) :room/slug slug}])
                       (let [db @conn current (reads/authorize-read! db (principal w) slug)]
                         (is (= (:other-room w) (:room-id current)))
                         ;; Isolate the domain's posthydrate tuple fence; a real
                         ;; cold resolver has its own registered-Room guard.
                         {:room-id (:other-room w) :party (:owner w) :room room :control-db db}))
                     workflows/manifest-in forbidden work/prepare forbidden work/submit-prepared! forbidden]
         (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit! host (principal w) slug input))))))
         (is (zero? @calls)))))))

(deftest fresh-owner-and-target-gates-surround-provider-preparation
  (doseq [phase [:before-prepare :before-claim]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [conn w host owned attached-descriptor]}]
       (seed! owned saved/source)
       (let [input (opened-input host w saved/source)
             snapshot workflows/snapshot-in prepare runs/prepare
             calls (atom {:prepare 0 :submit 0})
             change! #(d/transact conn [{:room/id (:room w) :room/default-repository
                                         [:system/id (:registry-system attached-descriptor)]}])]
         (with-redefs [workflows/snapshot-in (fn [& args] (let [value (apply snapshot args)]
                                                            (when (= :before-prepare phase) (change!)) value))
                       runs/prepare (fn [& args] (swap! calls update :prepare inc)
                                      (let [value (apply prepare args)]
                                        (when (= :before-claim phase) (change!)) value))
                       work/submit-prepared! (fn [& _] (swap! calls update :submit inc) (throw (AssertionError. "Stale target claimed")))]
           (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit! host (principal w) (str (:room w)) input))))))
           (is (= {:prepare (if (= :before-prepare phase) 0 1) :submit 0} @calls))))))))

(deftest native-and-helper-conflict-tags-never-become-normal-conflicts
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host owned]}]
     (seed! owned saved/source)
     (let [input (opened-input host w saved/source)]
       (doseq [stage [:retry :identity :hash :prepare]]
         (let [calls (atom 0) forged (fn [& _] (swap! calls inc)
                                       (throw (ex-info "SECRET/admission" {:type ::owned/conflict :status 409})))
               error (with-redefs [runs/retry-request (case stage :retry forged :identity (constantly {:status :absent}) runs/retry-request)
                                   requests/request-id (if (= :identity stage) forged requests/request-id)
                                   sources/source-id (if (= :hash stage) forged sources/source-id)
                                   runs/prepare (if (= :prepare stage) forged runs/prepare)]
                       (failure #(owned/submit! host (principal w) (str (:room w)) input)))]
           (is (= 1 @calls))
           (is (= ::owned/unavailable (:type (ex-data error))))
           (is (not (str/includes? (pr-str [(ex-message error) (ex-data error)]) "SECRET")))))))))

(deftest no-declaration-performs-no-live-knowledge-or-publication-scan
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host owned]}]
     (seed! owned saved/source)
     (let [input (opened-input host w saved/source) seen (atom nil) calls (atom 0)
           forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Undeclared live selection")))]
       (with-redefs [knowledge/selection forbidden knowledge/capture forbidden knowledge/publication-basis forbidden
                     work/submit-prepared! (claim-only seen)]
         (let [result (owned/submit! host (principal w) (str (:room w)) input)]
           (is (uuid? (:id result)))
           (is (zero? @calls))
           (is (nil? (get-in @seen [:prepared :basis :knowledge/publication-basis])))))))))

(deftest frozen-fixture-inputs-are-verified-without-live-knowledge-or-publication
  (@#'fixture/with-fixture
   false
   (fn [{:keys [w host owned]}]
     (seed! owned saved/knowledge-source)
     (repo/write! owned "inbox/fixture.md" (.getBytes "Frozen evidence" "UTF-8"))
     (let [capture (knowledge/capture @owned (knowledge/declaration saved/knowledge-source))
           input (assoc (opened-input host w saved/knowledge-source) :inputs (:inputs capture))
           seen (atom nil) calls (atom 0)
           forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Frozen fixtures reread live inputs")))]
       (repo/write! owned "inbox/fixture.md" (.getBytes "Later evidence" "UTF-8"))
       (with-redefs [knowledge/selection forbidden knowledge/capture forbidden knowledge/publication-basis forbidden
                     work/submit-prepared! (claim-only seen)]
         (let [result (owned/submit! host (principal w) (str (:room w)) input)]
           (is (uuid? (:id result)))
           (is (zero? @calls))
           (is (= "Frozen evidence" (get-in @seen [:prepared :basis :inputs "/in/knowledge/sources/inbox/fixture.md"])))
           (is (nil? (get-in @seen [:prepared :basis :knowledge/publication-basis])))))))))

(deftest original-control-and-context-expiry-refuse-at-the-current-gates
  (doseq [[kind at expected-prepares] [[:control 2 0] [:context 3 1]]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [w host owned]}]
       (seed! owned saved/source)
       (let [input (opened-input host w saved/source) preflight reads/preflight-host-read!
             prepare runs/prepare n (atom 0) calls (atom {:prepare 0 :submit 0})]
         (with-redefs [reads/preflight-host-read!
                       (fn [& args]
                         (if (= at (swap! n inc))
                           (case kind
                             :control (with-redefs [system-db/get-conn (constantly (Object.))] (apply preflight args))
                             :context (with-redefs [context/server-context (Object.)] (apply preflight args)))
                           (apply preflight args)))
                       runs/prepare (fn [& args] (swap! calls update :prepare inc) (apply prepare args))
                       work/submit-prepared! (fn [& _] (swap! calls update :submit inc)
                                               (throw (AssertionError. "Expired owner claimed work")))]
           (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit! host (principal w) (str (:room w)) input))))))
           (is (= at @n))
           (is (= {:prepare expected-prepares :submit 0} @calls))))))))

(deftest merge-revocation-after-capture-refuses-before-provider-preparation
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host owned]}]
     (seed! owned saved/source)
     (let [input (opened-input host w saved/source) resolve runs/resolve-admission-in
           calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Revoked merge prepared or claimed")))]
       (with-redefs [runs/resolve-admission-in
                     (fn [& args]
                       (let [value (apply resolve args)]
                         (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]
                                            :room/parties [[:party/id (:owner w)]]}])
                         value))
                     runs/prepare forbidden work/submit-prepared! forbidden]
         (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit! host (principal w) (str (:room w)) input))))))
         (is (zero? @calls)))
       (is (uuid? (:party (reads/authorize-read! @conn (principal w) (str (:room w))))))
       (is (thrown? clojure.lang.ExceptionInfo (work/authorize-in! @conn (principal w) (:room w))))))))

(deftest exact-retry-causal-interruption-is-preserved-before-reporting
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host owned]}]
     (seed! owned saved/source)
     (let [input (opened-input host w saved/source) calls (atom 0)]
       (with-redefs [work/submit-prepared! (claim-only (atom nil))]
         (owned/submit! host (principal w) (str (:room w)) input))
       (try
         (let [error (with-redefs [runs/retry-request
                                   (fn [& _] (swap! calls inc)
                                     (throw (ex-info "SECRET/retry cancellation" {} (InterruptedException. "Cancelled"))))]
                       (failure #(owned/submit! host (principal w) (str (:room w)) input)))
               interrupted? (.isInterrupted (Thread/currentThread))]
           (is interrupted?)
           (is (= 1 @calls))
           (is (= ::owned/unavailable (:type (ex-data error))))
           (is (not (str/includes? (pr-str [(ex-message error) (ex-data error)]) "SECRET"))))
         (finally (Thread/interrupted)))))))

(deftest malformed-normal-retry-outcomes-and-portable-mismatch-are-unavailable
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host owned]}]
     (seed! owned saved/source)
     (let [input (opened-input host w saved/source) calls (atom 0)
           forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Invalid metadata continued fresh admission")))]
       (doseq [result [nil [] :retry {:status :retry} {:status :retry :id (random-uuid)}
                       {:status :absent :id (random-uuid)}]]
         (with-redefs [runs/retry-request (constantly result) reads/resolve-host-read! forbidden runs/prepare forbidden]
           (is (= ::owned/unavailable (:type (ex-data (failure #(owned/submit! host (principal w) (str (:room w)) input))))))))
       (with-redefs [runs/prepare forbidden work/submit-prepared! forbidden]
         ;; Legacy plan-source's thrown portable conflict stays quarantined;
         ;; only NORMAL owned UUID and journal comparisons claim a409 here.
         (is (= ::owned/unavailable
                (:type (ex-data (failure #(owned/submit! host (principal w) (str (:room w))
                                                         (assoc input :expected-bundle (random-uuid)))))))))
       (is (zero? @calls))))))

(deftest interleaved-fresh-canonical-claims-never-upgrade-a-different-live-basis
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host owned]}]
     (seed! owned saved/knowledge-source)
     (repo/write! owned "inbox/evidence.md" (.getBytes "Trigger" "UTF-8"))
     (repo/write! owned "wiki/context.md" (.getBytes "Outer context" "UTF-8"))
     (let [input (opened-input host w saved/knowledge-source)
           enter? (atom true) winner (atom nil) outer (atom nil) kernel-calls (atom 0)
           producer (claim-only (atom nil))
           error (with-redefs [work/submit-prepared!
                               (fn [& args]
                                 (swap! kernel-calls inc)
                                 (when (compare-and-set! enter? true false)
                                   (reset! outer (:basis (last args)))
                                   ;; Deterministic interleave of two FRESH
                                   ;; submissions, not a new runner/thread.
                                   (repo/write! owned "wiki/context.md" (.getBytes "Winning context" "UTF-8"))
                                   (reset! winner (owned/submit! host (principal w) (str (:room w)) input)))
                                 (apply producer args))]
                   (failure #(owned/submit! host (principal w) (str (:room w)) input)))
           row (requests/get-request conn (:id @winner))
           retained (get-in (requests/payload conn row) [:input])]
       (is (= ::owned/unavailable (:type (ex-data error))) "Outer losing claim cannot adopt the different basis")
       (is (= 2 @kernel-calls))
       (is (uuid? (:id @winner)))
       (is (= 1 (count (requests/requests conn))))
       (is (= input (:submission retained)))
       (is (= "Outer context" (get-in @outer [:inputs "/in/knowledge/sources/wiki/context.md"])))
       (is (= "Winning context" (get-in retained [:inputs "/in/knowledge/sources/wiki/context.md"])))
       (is (not= (:knowledge/publication-basis @outer) (:knowledge/publication-basis retained)))
       (is (nil? (:work.request/job row)) "LayerA contains no physical Job")
       (let [before (:work.request/intent-id row) calls (atom 0)
             forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Observed winning claim recaptured")))]
         (with-redefs [reads/resolve-host-read! forbidden work/prepare forbidden work/submit-prepared! forbidden]
           (is (= @winner (owned/submit! host (principal w) (str (:room w)) input)))
           (is (zero? @calls)))
         (is (= before (:work.request/intent-id (requests/get-request conn (:id @winner))))))))))

(defn- fixed-response [n]
  (let [events (concat
                (when (= n 1)
                  [{:type "response.output_item.done"
                    :item {:type "function_call" :call_id "owned-source-proof" :name "clojure_eval"
                           :arguments (json/write-value-as-string
                                       {:code "(do (babashka.fs/create-dirs \"/out\") (spit \"/out/owned-proof.md\" (str (slurp \"/workflows/echo/helpers/data.txt\") \"|\" (slurp \"/in/knowledge/sources/wiki/context.md\"))))"})}}])
                (when (> n 1) [{:type "response.output_text.delta" :delta "Owned source and knowledge captured."}])
                [{:type "response.completed" :response {:id (str "owned-fixture-" n) :model "gpt-5.6-sol"
                                                        :usage {:input_tokens 10 :output_tokens 10}}}])]
    {:status 200 :body (java.io.ByteArrayInputStream.
                        (.getBytes (apply str (map #(str "data: " (json/write-value-as-string %) "\n\n") events)) "UTF-8"))}))

(defn- pending-group [room id]
  ;; READ-ONLY existing native cleanup handles, scoped to this exact incarnation
  ;; and group. Never remove entries, mutate outcomes or invent a replacement.
  (filter (fn [[_ entry]] (= id (:group entry)))
          (get @(var-get #'evaluation/pending-tasks) (@#'evaluation/cleanup-scope room))))

(defn- join-original! [handle interrupted?]
  (when handle
    (loop []
      (when-not (try @handle true
                     (catch InterruptedException _ (vreset! interrupted? true) false))
        (recur)))))

(defn- join-fixture-job! [room id done]
  ;; `done` is the ORIGINAL private native Job promise, never a new latch.
  ;; Its ACK ends runner+settled-callback extent, not proof that a callback
  ;; successfully joined detached evaluation cleanup (Job logs those failures).
  (let [interrupted? (volatile! false) first-error (volatile! nil)]
    (try
      (try
        (when-not (:settled? (jobs/status id)) (jobs/cancel! id))
        (catch InterruptedException _ (vreset! interrupted? true))
        (catch Throwable error (vreset! first-error error)))
      (join-original! done interrupted?)
      ;; Keep provider roots held through the REAL native descendant boundary.
      ;; Its known timeout leaves exact tasks registered, so it is not ACK.
      (loop []
        (let [joined?
              (try (evaluation/await-cleanups-for! room id 30000) true
                   (catch InterruptedException _ (vreset! interrupted? true) false)
                   (catch clojure.lang.ExceptionInfo error
                     (if (and (= :dvergr.agent.evaluation/cleanup-incomplete (:type (ex-data error)))
                              (some #(= :dvergr.agent.evaluation/cleanup-timeout (:type %)) (:failures (ex-data error)))
                              (seq (pending-group room id)))
                       (do (when-not @first-error (vreset! first-error error)) false)
                       (throw error))))]
          (when-not joined? (recur))))
      (when @first-error (throw @first-error))
      (finally
        ;; Even an unexpected public status/cancel/group-reader error cannot
        ;; unwind provider roots while OWNED native worker gates remain live.
        ;; A completed failure stays a failure, never synthetic cleanup success.
        (try
          (loop [observed #{}]
            (let [entries (remove (comp observed key) (pending-group room id))]
              (when (seq entries)
                ;; Actual pinned registry stores ONLY :gate/:group. Gate
                ;; delivery follows task-fn/provider/native work return.
                (doseq [[_ entry] entries]
                  (join-original! (:gate entry) interrupted?))
                (recur (into observed (map key entries))))))
          (finally (when @interrupted? (.interrupt (Thread/currentThread)))))))))

(deftest actual-existing-job-kernel-executes-owned-source-with-fixed-gateway-and-no-network
  (doseq [order [true false]]
    (@#'fixture/with-fixture
     order
     (fn [{:keys [conn w host room owned attached]}]
       (seed! owned saved/knowledge-source)
       (seed! attached (assoc saved/knowledge-source "helpers/data.txt" "OTHER code"))
       (repo/write! owned "inbox/evidence.md" (.getBytes "Owned trigger" "UTF-8"))
       (repo/write! owned "wiki/context.md" (.getBytes "Owned context" "UTF-8"))
       (repo/write! attached "inbox/evidence.md" (.getBytes "OTHER trigger" "UTF-8"))
       (repo/write! attached "wiki/context.md" (.getBytes "OTHER context" "UTF-8"))
       (let [input (assoc-in (opened-input host w saved/knowledge-source) [:limits :settlement] "discard")
             p (cond-> (principal w) (not order) (assoc :principal/mcp? true))
             before (state owned) other (state attached) selected (workspace/selected-system-id)
             calls (atom 0) job (atom nil) done (atom nil) start jobs/start!
             provider (codex/create {:credentials (gateway/static-credentials :fixture {} #{"https://chatgpt.com"})})]
         ;; Isolated fixture allowance ONLY: no account, provider admission,
         ;; external API or subscription call. Kernel/Job/SCI remain real.
         (resource/mint! room {:id (random-uuid) :resources (work/resources (:limits input))})
         (with-redefs [gateway/*request-fn* (fn [& _] (fixed-response (swap! calls inc)))
                       providers/providers (atom {:codex-subscription provider})
                       jobs/start!
                       (fn [actual-room options actual-work & callbacks]
                         ;; OBSERVE the real native job BEFORE original work can
                         ;; start or the outer admission response can be lost.
                         ;; Preserve every option/callback/return; no fake runner.
                         (apply start actual-room options
                                (fn [id]
                                  (when (and (identical? room actual-room) (compare-and-set! job nil id))
                                    (reset! done (get-in @(var-get #'jobs/live) [id :done])))
                                  (actual-work id))
                                callbacks))]
           (try
             (let [admitted (owned/submit! host p (str (:room w)) input)
                   status (jobs/status @job 40000)
                   attempts (store/-list-attempts (:store room) (store/conversation-id room) {})
                   children (binding [ec/*execution-context* (:ctx room)]
                              (remove #(= @job (:run/id %)) (run/runs room {:root-run-id @job :limit 10})))]
               (is (uuid? @job))
               (is (= @job (:job admitted)))
               (is (some? @done) "Original Job completion handle captured before original work")
               (is (= :completed (:status status)) (pr-str (select-keys status [:status :error])))
               (is (:settled? status))
               (is (false? (:return-pending? (work/status conn (:execution-ctx host) (principal w) (:room w) (:id admitted)))))
               (is (= 2 @calls))
               (is (= 1 (count attempts)))
               (is (= (if (:principal/mcp? p) :mcp :host)
                      (get-in (first attempts) [:attempt/receipt :attempt/metrics :submission-channel])))
               (is (= 1 (count children)))
               (is (= :discarded (:run/settlement-status (first children))))
               (is (= "pinned helper|Owned context" (get-in (first attempts) [:attempt/evidence :files "/out/owned-proof.md"])))
               (is (= :simmis/knowledge-workspace-completion
                      (get-in (first attempts) [:attempt/environment :environment/verifier :verifier/id])))
               (is (= input (get-in (requests/payload conn (requests/get-request conn (:id admitted))) [:input :submission])))
               (let [reads (atom 0) forbidden (fn [& _] (swap! reads inc) (throw (AssertionError. "Real Job retry touched fresh work")))]
                 (with-redefs [workflows/manifest-in forbidden workflows/snapshot-in forbidden
                               reads/resolve-host-read! forbidden work/prepare forbidden runs/prepare forbidden]
                   (is (= @job (:job (owned/submit! host p (str (:room w)) input))))
                   (is (zero? @reads))))
               (is (= 2 @calls))
               (is (= selected (workspace/selected-system-id)))
               (is (= before (state owned)))
               (is (= other (state attached))))
             (finally
               (when @job (join-fixture-job! room @job @done))))))))))
