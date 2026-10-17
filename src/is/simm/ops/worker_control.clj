(ns is.simm.ops.worker-control
  "Explicit HUMAN control of local SCI workers; separate from worker leases.
   New admission is opt-in. Disabling it never removes revocation or the
   existing worker transport's heartbeat/stop/reconciliation capabilities."
  (:require [clojure.string :as str]
            [dvergr.mcp.server :as server]
            [dvergr.model.registry :as registry]
            [dvergr.ops :as ops]
            [dvergr.substrate.config :as config]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.worker-benchmarks :as benchmarks]
            [is.simm.ops.worker-service :as service]
            [is.simm.ops.worker-work :as work]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.worker-maintenance :as maintenance]
            [malli.core :as m]))

(defn admission-open?
  "Only NEW human enrollment/funding/submission; prior queued assignments may
   still be claimed. Revoke the enrollment to cancel its delegated execution."
  []
  (if-let [value (System/getenv "SIMMIS_LOCAL_WORKER_ADMISSION")]
    (= "true" (str/lower-case (str/trim value)))
    (true? (get-in (config/config) [:mcp :local-worker-admission?]))))

(def ^:private uuid-string [:string {:min 36 :max 36}])
(def ^:private room-entry [:room [:string {:min 1 :max 200}]])
(def sci-config-schema
  [:map {:closed true}
   [:adapter [:= "codex"]] [:version [:string {:min 1 :max 200}]]
   [:model [:string {:min 1 :max 200}]] [:policy [:= "local-trusted-v1"]]])
(def enroll-schema [:map {:closed true} room-entry [:worker uuid-string] [:config sci-config-schema]])
(def revoke-schema [:map {:closed true} room-entry [:worker uuid-string]])
(def allowance-schema [:map {:closed true} room-entry [:request-id uuid-string]
                       [:max-worker-admissions [:int {:min 1 :max 32}]]])
(def task-schema [:map {:closed true} room-entry [:request-id uuid-string] [:worker uuid-string]
                  [:task [:string {:min 1 :max 16000}]]
                  [:paths [:vector {:max 100} [:string {:min 1 :max 512}]]]
                  [:limits [:map {:closed true} [:timeout-ms [:int {:min 1000 :max 600000}]]
                            [:settlement [:enum "review" "discard"]]]]])

(def workflow-schema
  (workflow-runs/transport-schema-for
   (into [:map {:closed true} room-entry [:request-id uuid-string] [:worker uuid-string]]
         (map (fn [entry] (if (= :expected-bundle (first entry))
                           [:expected-bundle uuid-string] entry)))
         (concat (drop 2 workflow-runs/source-schema) [[:limits (nth (last task-schema) 1)]]))))

(def benchmark-schema
  (into [:map {:closed true} room-entry]
        (map (fn [[key & _ :as entry]]
               (cond
                 (#{:request-id :worker :expected-bundle} key) [key uuid-string]
                 (= :limits key) [:limits [:map {:closed true}
                                           [:timeout-ms [:int {:min 1000 :max 600000}]]
                                           [:settlement [:= "discard"]]]]
                 :else entry)))
        (drop 2 benchmarks/schema)))

(defn- require-admission! []
  (when-not (and (admission-open?) (maintenance/claims-ready?))
    (ops/op-error! "New local worker admission is disabled or recovery is unavailable" ::disabled 503)))

(defn- uuid! [value]
  (or (parse-uuid value) (ops/op-error! "Invalid local worker identity" ::invalid 400)))

(defn- model! [configuration]
  ;; Registry metadata only: the server does NOT need the local login and must
  ;; not authenticate a provider merely to validate an enrollment.
  (when-not (= :codex-subscription (:provider (registry/get-model (:model configuration))))
    (ops/op-error! "Local SCI enrollment requires a registered native Codex model" ::invalid 400)))

(defn- secret! []
  (let [secret (get-in (auth/get-auth-config) [:jwt :secret])]
    (when-not (and (string? secret) (seq secret))
      (ops/op-error! "Local worker issuer is unavailable" ::disabled 503))
    secret))

(defn- invoke [host schema args new? operation]
  (when-not (m/validate schema args)
    (ops/op-error! "Invalid local worker control arguments" ::invalid 400))
  (api/guarded host
               #(let [[conn room-id room] (api/room! host (:room args))]
                  (when new? (require-admission!))
                  (operation conn room-id room))))

(def specifications
  (api/room-specifications
   {:simmis/local-worker-enroll
    {:kind :write :schema enroll-schema :authz {:action :write}
     :doc "HUMAN control only: explicitly enroll one native Codex SCI worker in this room. Requires current human room read/merge and enabled new admission. Stable worker UUID/config makes exact retries return the same bounded worker credential; a changed owner/room/config conflicts. No local subscription login is sent to the server. Use only a private operator helper, not an ordinary model tool session."
     :impl (fn [host args]
             (invoke host enroll-schema args true
                     (fn [conn id _]
                       (model! (:config args))
                       (service/enroll! conn ops/*principal* id (secret!) (uuid! (:worker args)) (:config args)))))}
    :simmis/local-worker-revoke
    {:kind :write :schema revoke-schema :authz {:action :write}
     :doc "HUMAN control only: revoke this room's delegated worker. Works while new admission is disabled. Existing leases retain heartbeat/stop acknowledgement only; revocation never fabricates physical shutdown."
     :impl (fn [host args]
             (invoke host revoke-schema args false
                     (fn [conn id _] (service/revoke! conn ops/*principal* id (uuid! (:worker args))))))}
    :simmis/local-worker-allowance
    {:kind :write :schema allowance-schema :authz {:action :write}
     :doc "HUMAN control only: explicitly provision 1..32 local-worker-admission units for this room. These units are NOT measured dollars or tokens. Never funds automatically; exact request-id retries are idempotent. Requires current room read/merge and enabled new admission."
     :impl (fn [host args]
             (invoke host allowance-schema args true
                     (fn [conn id room]
                       (work/provision! conn (:execution-ctx host) ops/*principal* id room
                                        (-> (dissoc args :room) (update :request-id uuid!))))))}
    :simmis/local-worker-task
    {:kind :write :schema task-schema :authz {:action :write}
     :doc "HUMAN control only: submit one finite selected-file task to your enrolled room worker using the existing Job/Run/Attempt flow. Consumes an explicitly funded admission, not verified provider usage. Frozen files only, review/discard settlement; worker cannot certify or merge. Exact request-id retries observe one Job. Requires current room read/merge and enabled new admission."
     :impl (fn [host args]
             (invoke host task-schema args true
                     (fn [conn id room]
                       (work/submit! conn (:execution-ctx host) ops/*principal* id room
                                     (-> (dissoc args :room) (update :request-id uuid!) (update :worker uuid!))))))}
    :simmis/local-worker-workflow
    {:kind :write :schema workflow-schema :authz {:action :write}
     :doc "HUMAN control only: run one inspected saved workflow revision on your enrolled local SCI worker. Pins literal parameters, source and fixture/input bytes. Uses one explicitly funded admission and existing Job/Run/Attempt flow; authored checkers are NOT executed. Exact retries observe the original Job. Review/discard only; no provider login is sent to the server."
     :impl (fn [host args]
             (invoke host workflow-schema args true
                     (fn [conn id room]
                       (work/submit-workflow!
                        conn (:execution-ctx host) ops/*principal* id room
                        (workflow-runs/transport-for
                         work/workflow-schema
                         (-> (dissoc args :room) (update :request-id uuid!)
                             (update :worker uuid!) (update :expected-bundle uuid!)))))))}
    :simmis/local-worker-benchmark
    {:kind :write :schema benchmark-schema :authz {:action :write}
     :doc "HUMAN control only: benchmark an inspected workflow revision on your enrolled local SCI worker. Requires installation-operator AND current room read/merge authority. At most four cases and two repetitions, serial and discard-only; one explicitly funded admission per cell, no automatic retries or funding. Only clean fixtures reach the guest; checkers and calibration run on the server. Usage remains unreported, not zero. Exact retries observe one Job."
     :impl (fn [host args]
             (invoke host benchmark-schema args true
                     (fn [conn id room]
                       (benchmarks/submit!
                        conn (:execution-ctx host) ops/*principal* id room
                        (-> (dissoc args :room) (update :request-id uuid!)
                            (update :worker uuid!) (update :expected-bundle uuid!))))))}}))

(defn install! []
  (doseq [[op spec] specifications] (ops/register-op! op spec))
  (server/sync-op-tools!))

(defn uninstall! []
  (doseq [op (keys specifications)] (ops/unregister-op! op))
  (server/sync-op-tools!))
