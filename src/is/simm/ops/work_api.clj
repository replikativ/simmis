(ns is.simm.ops.work-api
  "Small external bindings over canonical chat and bounded work services."
  (:require [datahike.api :as d]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as ops]
            [dvergr.room.store :as store]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.chat :as chat]
            [is.simm.ops.coordinator :as coordinator]
            [is.simm.ops.dvergr :as simmis-ops]
            [is.simm.ops.git-snapshot :as git-snapshot]
            [is.simm.ops.owned-benchmark-admission :as owned-benchmarks]
            [is.simm.ops.owned-reads :as owned-reads]
            [is.simm.ops.owned-workflow-admission :as owned-admission]
            [is.simm.ops.owned-workflow-authoring :as owned-authoring]
            [is.simm.ops.owned-workflows :as owned-workflows]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflow-schedules :as workflow-schedules]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private uuid-string [:string {:min 36 :max 36}])
(def ^:private room-entry [:room [:string {:min 1 :max 200}]])

(defn- external-schema [schema]
  (into [:map {:closed true} room-entry]
        (map (fn [entry]
               (update entry (dec (count entry)) #(if (= :uuid %) uuid-string %))))
        (drop 2 schema)))

(def task-schema (external-schema work/task-schema))
(def benchmark-schema (external-schema benchmarks/schema))
(def cli-benchmark-schema (external-schema benchmarks/cli-schema))
(def cli-allowance-schema (external-schema benchmarks/cli-allowance-schema))
(def allowance-schema (external-schema work/allowance-schema))
(def status-schema [:map {:closed true} room-entry [:id uuid-string]])
(def post-schema
  [:map {:closed true} room-entry [:id uuid-string]
   [:content [:string {:min 1 :max 16000}]]
   [:in-reply-to {:optional true} uuid-string]])

(def workflow-write-schema (external-schema workflows/transport-write-schema))
(def workflow-run-schema (external-schema workflow-runs/transport-schema))
(defn with-owned-source-wire
  "Transport-only optional field for host-agnostic tool descriptions. Owned
   handlers REQUIRE it; legacy handlers reject it before room hydration."
  [schema]
  (conj schema [:expected-owned-source {:optional true} uuid-string]))
(def workflow-run-route-schema (with-owned-source-wire workflow-run-schema))
(def benchmark-route-schema (with-owned-source-wire benchmark-schema))
(def cli-benchmark-route-schema (with-owned-source-wire cli-benchmark-schema))
(defn- scheduled-execution-schema [schema]
  ;; Reuse canonical closed schemas; only transport UUIDs differ, and a
  ;; scheduled occurrence, not its caller, owns each execution request-id.
  (into [:map {:closed true}]
        (comp (remove #(= :request-id (first %)))
              (map (fn [entry]
                     (update entry (dec (count entry)) #(if (= :uuid %) uuid-string %)))))
        (drop 2 schema)))

(defn- schedule-create-schema [owned-wire?]
  (into [:multi {:dispatch :kind}]
        (map (fn [[kind execution]]
               [kind [:map {:closed true} room-entry [:request-id uuid-string]
                      [:kind [:= kind]] [:cadence workflow-schedules/cadence-schema]
                      [:execution (cond-> (scheduled-execution-schema execution)
                                    owned-wire? with-owned-source-wire)]]]))
        [["workflow" workflow-runs/transport-schema] ["benchmark" benchmarks/schema]]))
(def workflow-schedule-create-schema (schedule-create-schema false))
(def workflow-schedule-route-schema (schedule-create-schema true))
(def workflow-schedule-list-schema [:map {:closed true} room-entry])
(def workflow-schedule-enabled-schema
  [:map {:closed true} room-entry [:id uuid-string] [:enabled? :boolean]])

(def workflow-check-schema [:map {:closed true} room-entry [:name workflows/name-schema]])
(def workflow-source-schema
  [:map {:closed true} room-entry [:name workflows/name-schema]
   [:file-offset {:optional true} [:int {:min 0 :max 100}]]
   [:path {:optional true} [:string {:min 1 :max 512}]]
   [:offset {:optional true} [:int {:min 0 :max workflows/max-file-bytes}]]
   [:expected-revision {:optional true} uuid-string]])
(def workflow-list-schema
  [:map {:closed true} room-entry
   [:offset {:optional true} [:int {:min 0 :max workflows/max-list-entries}]]
   [:expected-revision {:optional true} uuid-string]])
(def bundle-schema [:map {:closed true} room-entry])
(def bundle-chunk-schema
  [:map {:closed true} room-entry [:expected-snapshot uuid-string]
   [:offset [:int {:min 0 :max (:max-bundle-bytes git-snapshot/policy)}]]
   [:length {:optional true} [:int {:min 1 :max (:chunk-bytes git-snapshot/policy)}]]])

(defn- uuid! [s]
  (or (parse-uuid s) (ops/op-error! "Invalid UUID" ::invalid 400)))

(defn- checked! [schema args]
  (when-not (m/validate schema args)
    (ops/op-error! "Invalid work arguments" ::invalid 400)))

(defn workflow-schedule-transport
  "Normalize only declared transport fields, preserving exact nested intent."
  [args]
  (checked! workflow-schedule-create-schema args)
  (let [execution (update (:execution args) :expected-bundle uuid!)
        execution (if (= "workflow" (:kind args))
                    (dissoc (workflow-runs/transport (assoc execution :request-id (uuid! (:request-id args))))
                            :request-id)
                    execution)]
    (assoc (dissoc args :room) :request-id (uuid! (:request-id args)) :execution execution)))

(defn- schedule-receipt [row]
  ;; Raw activation rows contain a GC-tracked frozen target reference. It is
  ;; neither an external capability nor part of the public acknowledgement.
  (when-not (and (every? #(uuid? (get row %))
                         [:schedule.activation/id :schedule.activation/room :schedule.activation/revision :schedule.activation/by])
                 (boolean? (:schedule.activation/enabled? row)))
    (throw (ex-info "Invalid schedule acknowledgement" {})))
  (cond-> {:id (str (:schedule.activation/id row))
           :room-id (str (:schedule.activation/room row))
           :revision (str (:schedule.activation/revision row))
           :by (str (:schedule.activation/by row))
           :enabled? (:schedule.activation/enabled? row)}
    (:schedule.activation/next-fire row)
    (assoc :next-fire (.getTime ^java.util.Date (:schedule.activation/next-fire row)))
    (:schedule.activation/reason row) (assoc :reason (:schedule.activation/reason row))))

(defn room-handle
  "Translate a canonical Simmis UUID through its current system-db row.
   UUID-shaped handles never fall back to a slug with the same spelling."
  [arg]
  (simmis-ops/room-handle arg))

(defn room-resource [{:keys [room]}] [:room (room-handle room)])

(defn room-specifications [specifications]
  (update-vals specifications #(assoc-in % [:authz :resource] room-resource)))

(defn room!
  "Resolve a canonical Simmis room and its system-db identity for host ops.
   Caller must enforce the operation's principal and current room authority."
  [host arg]
  (let [room (ops/resolve-room host (room-handle arg))
        conn (system-db/get-conn)
        slug (some-> (:id room) store/room-id->slug)
        id (when slug
             (d/q '[:find ?id . :in $ ?slug
                    :where [?r :room/slug ?slug] [?r :room/id ?id]] @conn slug))]
    (when-not (and room id (or (not (string? arg)) (nil? (parse-uuid arg))
                               (= id (parse-uuid arg))))
      (ops/op-error! "No Simmis room" ::missing 404))
    [conn id room]))

(defn guarded
  "Run host operation code in its host context and map bounded service errors."
  [host f]
  (try (binding [ec/*execution-context* (:execution-ctx host)] (f))
       (catch clojure.lang.ExceptionInfo e
         (if-let [status (get {:is.simm.ops.work/invalid 400 :is.simm.ops.chat/invalid 400
                               :is.simm.ops.work/denied 403 :is.simm.ops.chat/denied 403
                               :is.simm.ops.work/missing 404 :is.simm.ops.chat/unavailable 503
                               :is.simm.model.work-requests/conflict 409
                               :is.simm.ops.workflows/invalid 400
                               :is.simm.ops.workflows/unavailable 503
                               :is.simm.ops.workflows/conflict 409
                               :is.simm.ops.workflow-runs/invalid 400
                               :is.simm.ops.knowledge-inputs/refused 400
                               :is.simm.documents.view/refused 400
                               :is.simm.documents.files/invalid 400
                               :is.simm.ops.workflow-runs/conflict 409
                               :is.simm.ops.workflow-schedules/invalid 400
                               :is.simm.ops.workflow-schedules/denied 403
                               :is.simm.ops.workflow-schedules/missing 404
                               :is.simm.ops.workflow-schedules/conflict 409
                               :is.simm.model.workflow-schedules/missing 404
                               :is.simm.model.workflow-schedules/conflict 409
                               :is.simm.model.workflow-schedules/capacity 409
                               :is.simm.model.workflow-schedules/corrupt 503
                               :is.simm.ops.benchmarks/invalid 400
                               :is.simm.ops.checkers/invalid 400
                               :is.simm.ops.git-snapshot/invalid 400
                               :is.simm.ops.git-snapshot/conflict 409
                               :is.simm.ops.git-snapshot/unavailable 503
                               :is.simm.ops.benchmarks/denied 403
                               :is.simm.ops.benchmarks/conflict 409
                               :is.simm.ops.benchmarks/busy 409
                               :is.simm.ops.benchmarks/unavailable 503
                               :is.simm.ops.cli-owner/disabled 503
                               :is.simm.ops.cli-owner/unavailable 503
                               :is.simm.ops.worker-control/disabled 503
                               :is.simm.ops.worker-control/invalid 400
                               :is.simm.ops.worker-service/invalid 400
                               :is.simm.ops.worker-service/denied 403
                               :is.simm.ops.worker-service/missing 404
                               :is.simm.ops.worker-work/invalid 400
                               :is.simm.ops.worker-benchmarks/invalid 400
                               :is.simm.ops.worker-work/denied 403
                               :is.simm.ops.worker-work/conflict 409
                               :is.simm.ops.worker-files/invalid 400
                               :is.simm.ops.worker-files/unavailable 503
                               :is.simm.model.workers/invalid 400
                               :is.simm.model.workers/denied 403
                               :is.simm.model.workers/missing 404
                               :is.simm.model.workers/conflict 409
                               :is.simm.model.workers/busy 409
                               :is.simm.model.workers/corrupt 503
                               :kontor.resource/insufficient 409}
                              (:type (ex-data e)))]
           (ops/op-error! (ex-message e) (:type (ex-data e)) status)
           (throw e)))))

(defn- owned-export-value! [f]
  ;; This boundary deliberately does NOT interpret Throwable tags/status/text:
  ;; a native content reader can throw forged op/conflict metadata as well.
  (try (f)
       (catch InterruptedException _
         (.interrupt (Thread/currentThread))
         (ops/op-error! "Owned workspace export unavailable" ::owned-export-unavailable 503))
       (catch Throwable _
         (ops/op-error! "Owned workspace export unavailable" ::owned-export-unavailable 503))))

(defn- owned-export! [host args chunk?]
  (let [expected (when chunk? (uuid! (:expected-snapshot args)))]
    (binding [ec/*execution-context* (:execution-ctx host)]
      (let [{:keys [db room-id target-stamp]}
            (owned-export-value! #(owned-reads/resolve-host-read! host ops/*principal* (:room args)))]
        (when chunk?
        ;; A NORMAL fixed metadata result, not arbitrary exception ex-data,
        ;; supplies the only trusted revision comparison. No body on conflict.
          (let [current (owned-export-value! #(git-snapshot/snapshot-id-in db room-id target-stamp))]
            (when-not (= expected current)
              (ops/op-error! "Workspace changed; restart the snapshot download" ::owned-export-conflict 409))))
        (owned-export-value!
         #(if chunk?
            (git-snapshot/chunk-in db room-id target-stamp expected (:offset args)
                                   (get args :length (:chunk-bytes git-snapshot/policy)))
            (git-snapshot/describe-in db room-id target-stamp)))))))

(defn- preserve-interruption! [error]
  (loop [cause error seen #{}]
    (cond (nil? cause) nil
          (instance? InterruptedException cause) (.interrupt (Thread/currentThread))
          (contains? seen cause) nil
          :else (recur (.getCause ^Throwable cause) (conj seen cause)))))

(defn owned-helper-value!
  "HOST-only quarantine for normalization/projection, not service-outcome
   classification. No exception tag can manufacture an adapter outcome."
  [f]
  (try (f)
       (catch Throwable error
         (preserve-interruption! error)
         (ops/op-error! "Owned workflow operation unavailable" ::owned-transport-unavailable 503))))

(defn- owned-uuid! [text]
  (or (owned-helper-value! #(parse-uuid text))
      (ops/op-error! "Invalid owned workflow identity" ::owned-transport-invalid 400)))

(defn owned-transport!
  "HOST DTO normalization only. Required guards come from the supplied fixed
   internal schema, never inferred from ambient source. All thrown helper
   failures are quarantined; only NORMAL validation outcomes become 400."
  [schema args]
  (let [{:keys [input result]}
        (owned-helper-value!
         #(let [input (reduce (fn [input field]
                                (if (contains? input field)
                                  (update input field (fn [value] (when (string? value) (parse-uuid value)))) input))
                              (dissoc args :room) [:request-id :worker :expected-bundle :expected-owned-source])]
            {:input input :result (workflow-runs/transport-result schema input)}))]
    (cond
      (and (map? result) (= #{:status :reason} (set (keys result)))
           (= :invalid (:status result)) (#{:shape :duplicate-key} (:reason result)))
      (ops/op-error! "Invalid owned workflow arguments" ::owned-transport-invalid 400)

      (and (map? result) (= #{:status :input} (set (keys result))) (= :ok (:status result)))
      (let [normalized (:input result)]
        (when-not (and (map? normalized)
                       (= (dissoc input :params :inputs) (dissoc normalized :params :inputs)))
          (ops/op-error! "Owned workflow operation unavailable" ::owned-transport-unavailable 503))
        (when-not (owned-helper-value! #(m/validate schema normalized))
          (ops/op-error! "Owned workflow operation unavailable" ::owned-transport-unavailable 503))
        normalized)

      :else (ops/op-error! "Owned workflow operation unavailable" ::owned-transport-unavailable 503))))

(defn owned-service-result!
  "HOST-only adapter mapping over fixed owned service calls. Normalization and
   projection MUST remain outside this boundary; native diagnostics stay private."
  [call]
  (try (call)
       (catch Throwable error
         (preserve-interruption! error)
         (let [[status kind message]
               (case (:type (ex-data error))
                 (:is.simm.ops.owned-workflow-admission/invalid
                  :is.simm.ops.owned-benchmark-admission/invalid
                  :is.simm.ops.owned-worker-admission/invalid
                  :is.simm.ops.owned-workflow-authoring/invalid
                  :is.simm.ops.owned-workflows/invalid
                  :is.simm.ops.workflow-schedules/invalid)
                 [400 ::owned-operation-invalid "Invalid owned workflow arguments"]
                 (:is.simm.ops.owned-workflow-admission/conflict
                  :is.simm.ops.owned-benchmark-admission/conflict
                  :is.simm.ops.owned-worker-admission/conflict
                  :is.simm.ops.owned-workflow-authoring/conflict
                  :is.simm.ops.owned-workflows/conflict
                  :is.simm.ops.workflow-schedules/conflict
                  :is.simm.ops.workflow-schedules/legacy-recreate)
                 [409 ::owned-operation-conflict "Work or source changed; inspect metadata before retrying"]
                 :is.simm.ops.owned-workflow-authoring/pending
                 [503 ::owned-save-pending "Workflow save outcome is pending; do not retry automatically"]
                 [503 ::owned-operation-unavailable "Owned workflow operation unavailable"])]
           (ops/op-error! message kind status)))))

(defn- owned-write-transport! [args]
  (let [input (dissoc args :room)
        _ (when-not (and (string? (:expected-revision input))
                         (owned-helper-value! #(parse-uuid (:expected-revision input))))
            (ops/op-error! "An inspected owned source revision is required" ::owned-transport-invalid 400))
        result (owned-helper-value! #(workflows/transport-write-result input))]
    (cond
      (and (map? result) (= #{:status :reason} (set (keys result))) (= :invalid (:status result))
           (#{:shape :duplicate-key :revision} (:reason result)))
      (ops/op-error! "Invalid owned workflow edits" ::owned-transport-invalid 400)

      (and (map? result) (= #{:status :input} (set (keys result))) (= :ok (:status result))
           (map? (:input result)))
      (let [normalized (:input result)]
        (when-not (and (= (dissoc input :files) (dissoc normalized :files))
                       (owned-helper-value!
                        #(m/validate workflows/write-schema (update normalized :expected-revision parse-uuid))))
          (ops/op-error! "Owned workflow operation unavailable" ::owned-transport-unavailable 503))
        normalized)

      :else (ops/op-error! "Owned workflow operation unavailable" ::owned-transport-unavailable 503))))

(defn owned-workflow-save!
  "Fixed HOST save shared by authenticated adapters. Returns the ORIGINAL
   sealed ACK; each adapter projects only its own public result. No reread."
  [host principal handle edits]
  (when (and (map? edits) (contains? edits :room))
    (ops/op-error! "Invalid owned workflow edits" ::owned-transport-invalid 400))
  (let [args (if (map? edits) (assoc edits :room handle) edits)]
    (checked! workflow-write-schema args)
    (let [input (owned-write-transport! args)
          saved (owned-service-result! #(owned-authoring/write! host principal handle input))]
      (owned-helper-value!
       #(do
          (when-not (and (map? (:write saved)) (contains? (:write saved) :commit)
                         (let [commit (get-in saved [:write :commit])]
                           (or (nil? commit) (and (string? commit) (parse-uuid commit))))
                         (string? (get-in saved [:source :revision]))
                         (parse-uuid (get-in saved [:source :revision]))
                         (= (get-in saved [:source :revision]) (get-in saved [:source :inspection :revision]))
                         (map? (get-in saved [:source :inspection])))
            (throw (ex-info "Invalid owned save acknowledgement" {})))
          saved)))))

(defn- owned-schedule-transport! [args]
  (let [schema (if (= "workflow" (:kind args)) workflow-runs/schema benchmarks/schema)
        execution (owned-transport! (conj schema [:expected-owned-source :uuid])
                                    (assoc (:execution args) :request-id (:request-id args)))]
    (assoc (dissoc args :room) :request-id (:request-id execution)
           :execution (dissoc execution :request-id))))

(defn owned-workflow-read-error!
  "HOST-only constant transport mapping of the fixed owned service's outcomes.
   Never relay native messages/status metadata. No file authority or callback."
  [error]
  (preserve-interruption! error)
  (let [[status kind message]
        (case (:type (ex-data error))
          :is.simm.ops.owned-workflows/invalid
          [400 ::owned-workflow-invalid "Invalid owned workflow read"]
          :is.simm.ops.owned-workflows/conflict
          [409 ::owned-workflow-conflict "Workflow source changed; reload metadata"]
          [503 ::owned-workflow-unavailable "Owned workflow read unavailable"])]
    (ops/op-error! message kind status)))

(defn- owned-workflow-read! [host args source?]
  (try
    (:data ((if source? owned-workflows/source! owned-workflows/list!)
            host ops/*principal* (:room args) (dissoc args :room)))
    (catch Throwable error (owned-workflow-read-error! error))))

(defn owned-status!
  "Inactive HOST metadata-only request status. No room hydration, source
   guard synthesis, model preparation, wallet access or fresh admission."
  [host principal handle id]
  (when-not (uuid? id)
    (ops/op-error! "Invalid request identity" ::invalid 400))
  (try
    (let [original (owned-reads/preflight-host-read! host principal handle)
          {:keys [control-conn execution-ctx]} (::owned-reads/owner host)]
      (binding [ec/*execution-context* execution-ctx]
        (let [result (work/status control-conn execution-ctx principal (:room-id original) id)
              current (owned-reads/preflight-host-read! host principal handle)]
          (when-not (= (select-keys original [:party :room-id]) (select-keys current [:party :room-id]))
            (throw (ex-info "Request metadata unavailable" {})))
          (work/authorize-in! (:control-db current) principal (:room-id current))
          result)))
    (catch Throwable error
      ;; Status has no normal source-conflict outcome. Helper exception tags
      ;; cannot manufacture a 400/409 or relay provider/native diagnostics.
      (preserve-interruption! error)
      (ops/op-error! "Owned request status unavailable" ::owned-status-unavailable 503))))

(def specifications
  (room-specifications
   {:simmis/workflow-schedule-list
    {:kind :read :schema workflow-schedule-list-schema :authz {:action :read}
     :doc "List this room's pinned workflow/benchmark schedules and recent occurrence links to canonical Jobs. Requires current room read authority. Does not execute, fund or resume anything."
     :impl (fn [host args]
             (checked! workflow-schedule-list-schema args)
             (if (owned-reads/owned-host? host)
               (owned-service-result! #(workflow-schedules/list-owned! host ops/*principal* (:room args)))
               (guarded host #(let [[conn id _] (room! host (:room args))]
                                (workflow-schedules/list-room conn (:execution-ctx host) ops/*principal* id)))))}
    :simmis/workflow-schedule-create
    {:kind :write :schema workflow-schedule-route-schema :authz {:action :merge}
     :doc "Explicitly activate a pinned saved workflow or operator-only benchmark on an interval/calendar cadence with an explicit timezone. Stable request-id retries observe the admitted revision. Each occurrence uses its finite execution limits and existing shared room allowance; never auto-funds. Missed slots coalesce; overlapping work is skipped. Execution has no caller request-id, code, credentials or arbitrary hooks."
     :impl (fn [host args]
             (checked! workflow-schedule-route-schema args)
             (if (owned-reads/owned-host? host)
               (let [input (owned-schedule-transport! args)
                     row (owned-service-result! #(workflow-schedules/create-owned! host ops/*principal* (:room args) input))]
                 (owned-helper-value! #(schedule-receipt row)))
               (do
                 (checked! workflow-schedule-create-schema args)
                 (guarded host #(let [[conn id room] (room! host (:room args))]
                                  (schedule-receipt
                                   (workflow-schedules/create! conn (:execution-ctx host) ops/*principal* id room
                                                               (workflow-schedule-transport args))))))))}
    :simmis/workflow-schedule-set-enabled
    {:kind :write :schema workflow-schedule-enabled-schema :authz {:action :merge}
     :doc "Explicitly pause/resume one pinned schedule in this room. Requires current execution authority (and operator status for benchmarks); resume rechecks the original delegator and pinned host policy. Disabling does not cancel already admitted work or fund new allowance."
     :impl (fn [host args]
             (checked! workflow-schedule-enabled-schema args)
             (if (owned-reads/owned-host? host)
               (let [id (owned-uuid! (:id args))
                     row (owned-service-result!
                          #(workflow-schedules/set-enabled-owned! host ops/*principal* (:room args)
                                                                  id (:enabled? args)))]
                 (owned-helper-value! #(schedule-receipt row)))
               (guarded host #(let [[conn id _] (room! host (:room args))]
                                (schedule-receipt
                                 (workflow-schedules/set-enabled! conn (:execution-ctx host) ops/*principal* id
                                                                  (uuid! (:id args)) (:enabled? args)))))))}
    :simmis/workflow-list
    {:kind :read :schema workflow-list-schema :authz {:action :read}
     :doc "List25 room-local workflow names, including invalid drafts, without loading file contents. Scans at most20000 workflow path entries; refuses larger indices. expected-revision pins the sorted name set across pages. Never executes checker source."
     :impl (fn [host args]
             (checked! workflow-list-schema args)
             (if (owned-reads/owned-host? host)
               (owned-workflow-read! host args false)
               (guarded host #(let [[_ _ room] (room! host (:room args))]
                                (workflows/list-page room (cond-> (dissoc args :room)
                                                            (:expected-revision args)
                                                            (update :expected-revision uuid!)))))))}
    :simmis/workflow-source
    {:kind :read :schema workflow-source-schema :authz {:action :read}
     :doc "Read source metadata in10-file pages, or <=8192 bytes of one file as base64 with path/offset/expected-revision. Source snapshots are bounded100 regular UTF8 files,512KiB/file,2MiB total. Chunks REQUIRE the inspected source revision; any source change refuses. Invalid drafts remain readable; no checker/calibration execution. Use revision for guarded writes and validated bundle id for execution."
     :impl (fn [host args]
             (checked! workflow-source-schema args)
             (if (owned-reads/owned-host? host)
               (owned-workflow-read! host args true)
               (guarded host #(let [[_ _ room] (room! host (:room args))]
                                (workflows/source-page room (dissoc args :room))))))}
    :simmis/workspace-bundle
    {:kind :read :schema bundle-schema :authz {:action :read}
     :doc "Describe a cloneable synthetic Git bundle of CURRENT workspace files (NOT original history or hosted Git). Bounded to512 regular files,1MiB/file,8MiB total; sensitive paths/symlinks refuse the whole export. Does not commit or write anything. Use snapshot and chunk tool to download; changes between chunks require a restart."
     :impl (fn [host args]
             (checked! bundle-schema args)
             (if (owned-reads/owned-host? host)
               (owned-export! host args false)
               (guarded host #(let [[_ _ room] (room! host (:room args))] (git-snapshot/describe room)))))}
    :simmis/workspace-bundle-chunk
    {:kind :read :schema bundle-chunk-schema :authz {:action :read}
     :doc "Read <=16384 bytes of the exact current synthetic workspace Git bundle as base64. expected-snapshot pins the inspected manifest; workspace changes return conflict, never mixed bytes. Verify offsets, size and full SHA256 before cloning the completed bundle. No retained exports, ancestry or server paths."
     :impl (fn [host args]
             (checked! bundle-chunk-schema args)
             (if (owned-reads/owned-host? host)
               (owned-export! host args true)
               (guarded host #(let [[_ _ room] (room! host (:room args))]
                                (git-snapshot/chunk room (uuid! (:expected-snapshot args)) (:offset args)
                                                    (get args :length (:chunk-bytes git-snapshot/policy)))))))}
    :simmis/cli-benchmark
    {:kind :write :schema cli-benchmark-route-schema :authz {:action :merge}
     :doc "Operator-only offline benchmark using Claude Code TEXT inference under Dvergr's SCI loop. Explicit single-use CLI Run admissions, finite steps/deadline, no compaction, resume, delegation or automatic retry. Not HTTP-dispatch accounting. Same frozen fixtures/checker/Attempts/Scorecards; discard only."
     :impl (fn [host args]
             (checked! cli-benchmark-route-schema args)
             (if (owned-reads/owned-host? host)
               (let [input (owned-transport! (conj benchmarks/cli-schema [:expected-owned-source :uuid]) args)]
                 (owned-service-result! #(owned-benchmarks/submit-cli! host ops/*principal* (:room args) input)))
               (do (checked! cli-benchmark-schema args)
                   (guarded host #(let [[conn id room] (room! host (:room args))]
                                    (benchmarks/submit-cli! conn (:execution-ctx host) ops/*principal* id room
                                                            (-> (dissoc args :room) (update :request-id uuid!)
                                                                (update :expected-bundle uuid!))))))))}
    :simmis/cli-allowance
    {:kind :write :schema cli-allowance-schema :authz {:action :merge}
     :doc "Explicit finite operator allowance for single-use Claude CLI Runs. One admission per benchmark cell, not one internal HTTP request; no dollar-spend-cap claim. Stable request-id; no automatic funding."
     :impl (fn [host args]
             (checked! cli-allowance-schema args)
             (guarded host #(let [[conn id room] (room! host (:room args))]
                              (benchmarks/provision-cli! conn (:execution-ctx host) ops/*principal* id room
                                                         (-> (dissoc args :room) (update :request-id uuid!))))))}
    :simmis/benchmark
    {:kind :write :schema benchmark-route-schema :authz {:action :merge}
     :doc "Admit an exact inspected workflow bundle as an OFFLINE benchmark. Requires installation operator AND current room merge authority: checker code is trusted-operator SCI, cooperatively bounded, NOT hard OS isolation. Maximum eight cells, one at a time, native providers only, explicit aggregate money/dispatch allowance, /out capture, discard settlement. Optional calibration.edn runs reference/damaged answers before each candidate without exposing them to it. Results use the room's existing Attempts, Scorecards and experiment reports. request-id retries observe the same job; expected-bundle pins the reviewed revision. No judge, live web, arbitrary eval or automatic checker promotion."
     :impl (fn [host args]
             (checked! benchmark-route-schema args)
             (if (owned-reads/owned-host? host)
               (let [input (owned-transport! (conj benchmarks/schema [:expected-owned-source :uuid]) args)]
                 (owned-service-result! #(owned-benchmarks/submit-native! host ops/*principal* (:room args) input)))
               (do (checked! benchmark-schema args)
                   (guarded host #(let [[conn id room] (room! host (:room args))]
                                    (benchmarks/submit! conn (:execution-ctx host) ops/*principal* id room
                                                        (-> (dissoc args :room)
                                                            (update :request-id uuid!)
                                                            (update :expected-bundle uuid!))))))))}
    :simmis/workflow-run
    {:kind :write :schema workflow-run-route-schema :authz {:action :merge}
     :doc "Run ONE exact inspected saved workflow in an inherited room fork, with explicit case, literal declared string params, optional bounded canonical file inputs and finite limits. Same read/merge authority and file-worker policy as simmis_task. Pinned bundle source and selected inputs are materialized ONLY inside the fork and appear in its proposed diff. Never executes checker/calibration or claims correctness; uses ordinary completion capture under /out. review/discard only, no automatic merge. Identical request-id retries observe the old Job even after source edits; changed intent conflicts. Not benchmark isolation: other inherited room files remain visible."
     :impl (fn [host args]
             (checked! workflow-run-route-schema args)
             (if (owned-reads/owned-host? host)
               (let [input (owned-transport! (conj workflow-runs/schema [:expected-owned-source :uuid]) args)]
                 (owned-service-result! #(owned-admission/submit! host ops/*principal* (:room args) input)))
               (do (checked! workflow-run-schema args)
                   (guarded host #(let [[conn id room] (room! host (:room args))]
                                    (workflow-runs/submit! conn (:execution-ctx host) ops/*principal* id room
                                                           (workflow-runs/transport
                                                            (-> (dissoc args :room)
                                                                (update :request-id uuid!)
                                                                (update :expected-bundle uuid!)))))))))}
    :simmis/workflow-write
    {:kind :write :schema workflow-write-schema :authz {:action :write}
     :doc "Atomically edit text files ONLY under workflows/<name>/ as one repository commit. Paths are relative to that bundle; null deletes. At most100 files,512KiB/file,2MiB/batch. Optional expected-revision from workflow-check prevents stale saves under the workspace write lock; omitted means unconditional write. Cannot edit the room-root AGENTS.md or paths outside this bundle. Returns commit and shape inspection of the current bundle; never executes checker.clj."
     :impl (fn [host args]
             (checked! workflow-write-schema args)
             (if (owned-reads/owned-host? host)
               (let [saved (owned-workflow-save! host ops/*principal* (:room args) (dissoc args :room))]
                 (owned-helper-value!
                  #(workflows/public-write {:write (:write saved) :workflow (get-in saved [:source :inspection])})))
               (guarded host #(let [[_ _ room] (room! host (:room args))]
                                (workflows/public-write
                                 (workflows/write! room ops/*principal* (workflows/transport-write (dissoc args :room))))))))}
    :simmis/workflow-check
    {:kind :read :schema workflow-check-schema :authz {:action :read}
     :doc "Inspect one pinned current workflow bundle (max100 files/2MiB). Returns source revision even for invalid drafts, plus validated bundle id or structural problems. Source revision guards workflow-write; bundle id pins workflow-run or benchmark. This NEVER executes checker.clj or calibration; no answer/eval argument is accepted."
     :impl (fn [host args]
             (checked! workflow-check-schema args)
             (if (owned-reads/owned-host? host)
               (let [details (owned-service-result! #(owned-workflows/details! host ops/*principal* (:room args) (dissoc args :room)))]
                 (owned-helper-value! #(let [inspection (get-in details [:data :inspection])]
                                         (when-not (map? inspection) (throw (ex-info "Invalid owned source metadata" {})))
                                         (workflows/public-inspection inspection))))
               (guarded host #(let [[_ _ room] (room! host (:room args))]
                                (workflows/public-inspection (workflows/inspect room (:name args)))))))}
    :simmis/post
    {:kind :write :schema post-schema :authz {:action :write}
     :doc "Post as your authenticated Simmis account through canonical room dispatch. May wake assigned agents on the ROOM's ongoing budgets. This is conversation, NOT a bounded isolated task. Stable message id deduplicates retries."
     :impl (fn [host args]
             (checked! post-schema args)
             (guarded host #(let [[_ id _] (room! host (:room args))]
                              (chat/post! ops/*principal*
                                          (cond-> {:room id :id (uuid! (:id args)) :content (:content args)}
                                            (:in-reply-to args) (assoc :in-reply-to (uuid! (:in-reply-to args))))
                                          :mcp))))}
    :simmis/coordinator
    {:kind :write :schema task-schema :authz {:action :merge}
     :doc "Opt-in bounded recursive coordinator in an inherited room fork. Same room merge authority and existing native money/dispatch allowance as simmis_task; never auto-funds or merges trunk. Can hire only file-worker/coordinator profiles with bounded tools/providers, and run up to eight completion-only experiment cells with parallelism two. Children split the conserved parent grant. Scoped immutable file results are available inside SCI. Completion is not task correctness. request-id retries observe identical intent."
     :impl (fn [host args]
             (checked! task-schema args)
             (guarded host #(let [[conn id room] (room! host (:room args))]
                              (coordinator/submit! conn (:execution-ctx host) ops/*principal* id room
                                                   (-> (dissoc args :room) (update :request-id uuid!))))))}
    :simmis/task
    {:kind :write :schema task-schema :authz {:action :merge}
     :doc "Submit ONE generic code/file candidate in an isolated fork with explicit limits. Requires room membership and merge authority, and existing work allowance. review/discard only; never auto-merges. Deliverables under /out are captured. Completion is NOT a correctness verifier. Reuse request-id only for identical intent; retries observe the same job, never re-execute."
     :impl (fn [host args]
             (checked! task-schema args)
             (guarded host #(let [[conn id room] (room! host (:room args))]
                              (work/submit! conn (:execution-ctx host) ops/*principal* id room
                                            (-> (dissoc args :room) (update :request-id uuid!))))))}
    :simmis/work-allowance
    {:kind :write :schema allowance-schema :authz {:action :merge}
     :doc "Explicitly provision a finite room execution allowance (incremental microUSD and native model dispatches, including native subscriptions). Zero-dollar models still consume dispatch allowance; notional list price is reported separately. Owner-level; NOT payment, billing balance or a sandbox mint capability. Stable request-id prevents repeat provisioning; changing its vector conflicts."
     :impl (fn [host args]
             (checked! allowance-schema args)
             (guarded host #(let [[conn id room] (room! host (:room args))]
                              (work/provision! conn (:execution-ctx host) ops/*principal* id room
                                               (-> (dissoc args :room) (update :request-id uuid!))))))}
    :simmis/work-status
    {:kind :read :schema status-schema :authz {:action :merge}
     :doc "Read admission/allowance metadata for one request in a room. The job pointer leads to canonical job_status/run_detail/attempt_list; this is not a second execution status. Requires current generic-work authority."
     :impl (fn [host args]
             (checked! status-schema args)
             (if (owned-reads/owned-host? host)
               (owned-status! host ops/*principal* (:room args) (owned-uuid! (:id args)))
               (guarded host #(let [[conn id _] (room! host (:room args))]
                                (work/status conn (:execution-ctx host) ops/*principal* id (uuid! (:id args)))))))}}))

(defn install! []
  (doseq [[op spec] specifications] (ops/register-op! op spec))
  (server/sync-op-tools!)
  nil)

(defn uninstall! []
  (doseq [op (keys specifications)] (ops/unregister-op! op))
  (server/sync-op-tools!)
  nil)
