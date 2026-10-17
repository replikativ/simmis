(ns is.simm.ops.work-api
  "Small external bindings over canonical chat and bounded work services."
  (:require [datahike.api :as d]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as ops]
            [dvergr.room.store :as store]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.chat :as chat]
            [is.simm.ops.git-snapshot :as git-snapshot]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as workflow-runs]
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
(def allowance-schema (external-schema work/allowance-schema))
(def status-schema [:map {:closed true} room-entry [:id uuid-string]])
(def post-schema
  [:map {:closed true} room-entry [:id uuid-string]
   [:content [:string {:min 1 :max 16000}]]
   [:in-reply-to {:optional true} uuid-string]])

(def workflow-write-schema (external-schema workflows/transport-write-schema))
(def workflow-run-schema (external-schema workflow-runs/transport-schema))
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

(defn room-handle
  "Translate a canonical Simmis UUID through its current system-db row.
   UUID-shaped handles never fall back to a slug with the same spelling."
  [arg]
  (if-let [id (when (string? arg) (parse-uuid arg))]
    (when-let [conn (system-db/get-conn)]
      (d/q '[:find ?slug . :in $ ?id
             :where [?r :room/id ?id] [?r :room/slug ?slug]] @conn id))
    arg))

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
                               :is.simm.ops.workflow-runs/conflict 409
                               :is.simm.ops.benchmarks/invalid 400
                               :is.simm.ops.checkers/invalid 400
                               :is.simm.ops.git-snapshot/invalid 400
                               :is.simm.ops.git-snapshot/conflict 409
                               :is.simm.ops.git-snapshot/unavailable 503
                               :is.simm.ops.benchmarks/denied 403
                               :is.simm.ops.benchmarks/conflict 409
                               :is.simm.ops.benchmarks/busy 409
                               :is.simm.ops.benchmarks/unavailable 503
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

(def specifications
  (room-specifications
   {:simmis/workflow-list
    {:kind :read :schema workflow-list-schema :authz {:action :read}
     :doc "List25 room-local workflow names, including invalid drafts, without loading file contents. Scans at most20000 workflow path entries; refuses larger indices. expected-revision pins the sorted name set across pages. Never executes checker source."
     :impl (fn [host args]
             (checked! workflow-list-schema args)
             (guarded host #(let [[_ _ room] (room! host (:room args))]
                              (workflows/list-page room (cond-> (dissoc args :room)
                                                          (:expected-revision args)
                                                          (update :expected-revision uuid!))))))}
    :simmis/workflow-source
    {:kind :read :schema workflow-source-schema :authz {:action :read}
     :doc "Read source metadata in10-file pages, or <=8192 bytes of one file as base64 with path/offset/expected-revision. Source snapshots are bounded100 regular UTF8 files,512KiB/file,2MiB total. Chunks REQUIRE the inspected source revision; any source change refuses. Invalid drafts remain readable; no checker/calibration execution. Use revision for guarded writes and validated bundle id for execution."
     :impl (fn [host args]
             (checked! workflow-source-schema args)
             (guarded host #(let [[_ _ room] (room! host (:room args))]
                              (workflows/source-page room (dissoc args :room)))))}
    :simmis/workspace-bundle
    {:kind :read :schema bundle-schema :authz {:action :read}
     :doc "Describe a cloneable synthetic Git bundle of CURRENT workspace files (NOT original history or hosted Git). Bounded to512 regular files,1MiB/file,8MiB total; sensitive paths/symlinks refuse the whole export. Does not commit or write anything. Use snapshot and chunk tool to download; changes between chunks require a restart."
     :impl (fn [host args]
             (checked! bundle-schema args)
             (guarded host #(let [[_ _ room] (room! host (:room args))] (git-snapshot/describe room))))}
    :simmis/workspace-bundle-chunk
    {:kind :read :schema bundle-chunk-schema :authz {:action :read}
     :doc "Read <=16384 bytes of the exact current synthetic workspace Git bundle as base64. expected-snapshot pins the inspected manifest; workspace changes return conflict, never mixed bytes. Verify offsets, size and full SHA256 before cloning the completed bundle. No retained exports, ancestry or server paths."
     :impl (fn [host args]
             (checked! bundle-chunk-schema args)
             (guarded host #(let [[_ _ room] (room! host (:room args))]
                              (git-snapshot/chunk room (uuid! (:expected-snapshot args)) (:offset args)
                                                  (get args :length (:chunk-bytes git-snapshot/policy))))))}
    :simmis/benchmark
    {:kind :write :schema benchmark-schema :authz {:action :merge}
     :doc "Admit an exact inspected workflow bundle as an OFFLINE benchmark. Requires installation operator AND current room merge authority: checker code is trusted-operator SCI, cooperatively bounded, NOT hard OS isolation. Maximum eight cells, one at a time, native providers only, explicit aggregate money/dispatch allowance, /out capture, discard settlement. Optional calibration.edn runs reference/damaged answers before each candidate without exposing them to it. Results use the room's existing Attempts, Scorecards and experiment reports. request-id retries observe the same job; expected-bundle pins the reviewed revision. No judge, live web, arbitrary eval or automatic checker promotion."
     :impl (fn [host args]
             (checked! benchmark-schema args)
             (guarded host #(let [[conn id room] (room! host (:room args))]
                              (benchmarks/submit! conn (:execution-ctx host) ops/*principal* id room
                                                  (-> (dissoc args :room)
                                                      (update :request-id uuid!)
                                                      (update :expected-bundle uuid!))))))}
    :simmis/workflow-run
    {:kind :write :schema workflow-run-schema :authz {:action :merge}
     :doc "Run ONE exact inspected saved workflow in an inherited room fork, with explicit case, literal declared string params, optional bounded canonical file inputs and finite limits. Same read/merge authority and file-worker policy as simmis_task. Pinned bundle source and selected inputs are materialized ONLY inside the fork and appear in its proposed diff. Never executes checker/calibration or claims correctness; uses ordinary completion capture under /out. review/discard only, no automatic merge. Identical request-id retries observe the old Job even after source edits; changed intent conflicts. Not benchmark isolation: other inherited room files remain visible."
     :impl (fn [host args]
             (checked! workflow-run-schema args)
             (guarded host #(let [[conn id room] (room! host (:room args))]
                              (workflow-runs/submit! conn (:execution-ctx host) ops/*principal* id room
                                                     (workflow-runs/transport
                                                      (-> (dissoc args :room)
                                                          (update :request-id uuid!)
                                                          (update :expected-bundle uuid!)))))))}
    :simmis/workflow-write
    {:kind :write :schema workflow-write-schema :authz {:action :write}
     :doc "Atomically edit text files ONLY under workflows/<name>/ as one repository commit. Paths are relative to that bundle; null deletes. At most100 files,512KiB/file,2MiB/batch. Optional expected-revision from workflow-check prevents stale saves under the workspace write lock; omitted means unconditional write. Cannot edit the room-root AGENTS.md or paths outside this bundle. Returns commit and shape inspection of the current bundle; never executes checker.clj."
     :impl (fn [host args]
             (checked! workflow-write-schema args)
             (guarded host #(let [[_ _ room] (room! host (:room args))]
                              (workflows/public-write
                               (workflows/write! room ops/*principal* (workflows/transport-write (dissoc args :room)))))))}
    :simmis/workflow-check
    {:kind :read :schema workflow-check-schema :authz {:action :read}
     :doc "Inspect one pinned current workflow bundle (max100 files/2MiB). Returns source revision even for invalid drafts, plus validated bundle id or structural problems. Source revision guards workflow-write; bundle id pins workflow-run or benchmark. This NEVER executes checker.clj or calibration; no answer/eval argument is accepted."
     :impl (fn [host args]
             (checked! workflow-check-schema args)
             (guarded host #(let [[_ _ room] (room! host (:room args))]
                              (workflows/public-inspection (workflows/inspect room (:name args))))))}
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
             (guarded host #(let [[conn id _] (room! host (:room args))]
                              (work/status conn (:execution-ctx host) ops/*principal* id (uuid! (:id args))))))}}))

(defn install! []
  (doseq [[op spec] specifications] (ops/register-op! op spec))
  (server/sync-op-tools!)
  nil)

(defn uninstall! []
  (doseq [op (keys specifications)] (ops/unregister-op! op))
  (server/sync-op-tools!)
  nil)
