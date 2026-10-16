(ns is.simm.ops.work-api
  "Small external bindings over canonical chat and bounded work services."
  (:require [datahike.api :as d]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as ops]
            [dvergr.room.store :as store]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.chat :as chat]
            [is.simm.ops.work :as work]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private uuid-string [:string {:min 36 :max 36}])
(def ^:private room-entry [:room [:string {:min 1 :max 200}]])

(defn- external-schema [schema]
  (into [:map {:closed true} room-entry]
        (map (fn [[k type]] [k (if (= :uuid type) uuid-string type)]))
        (drop 2 schema)))

(def task-schema (external-schema work/task-schema))
(def allowance-schema (external-schema work/allowance-schema))
(def status-schema [:map {:closed true} room-entry [:id uuid-string]])
(def post-schema
  [:map {:closed true} room-entry [:id uuid-string]
   [:content [:string {:min 1 :max 16000}]]
   [:in-reply-to {:optional true} uuid-string]])

(defn- uuid! [s]
  (or (parse-uuid s) (ops/op-error! "Invalid UUID" ::invalid 400)))

(defn- checked! [schema args]
  (when-not (m/validate schema args)
    (ops/op-error! "Invalid work arguments" ::invalid 400)))

(defn- room! [host arg]
  (let [room (ops/resolve-room host arg)
        conn (system-db/get-conn)
        slug (some-> (:id room) store/room-id->slug)
        id (when slug
             (d/q '[:find ?id . :in $ ?slug
                    :where [?r :room/slug ?slug] [?r :room/id ?id]] @conn slug))]
    (when-not (and room id) (ops/op-error! "No Simmis room" ::missing 404))
    [conn id room]))

(defn- guarded [host f]
  (try (binding [ec/*execution-context* (:execution-ctx host)] (f))
       (catch clojure.lang.ExceptionInfo e
         (if-let [status (get {:is.simm.ops.work/invalid 400 :is.simm.ops.chat/invalid 400
                               :is.simm.ops.work/denied 403 :is.simm.ops.chat/denied 403
                               :is.simm.ops.work/missing 404 :is.simm.ops.chat/unavailable 503
                               :is.simm.model.work-requests/conflict 409
                               :kontor.resource/insufficient 409}
                              (:type (ex-data e)))]
           (ops/op-error! (ex-message e) (:type (ex-data e)) status)
           (throw e)))))

(def specifications
  {:simmis/post
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
    :doc "Explicitly provision a finite room execution allowance (microUSD and native model dispatches). Owner-level; NOT payment, billing balance or a sandbox mint capability. Stable request-id prevents repeat provisioning; changing its vector conflicts."
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
                             (work/status conn (:execution-ctx host) ops/*principal* id (uuid! (:id args))))))}})

(defn install! []
  (doseq [[op spec] specifications] (ops/register-op! op spec))
  (server/sync-op-tools!)
  nil)

(defn uninstall! []
  (doseq [op (keys specifications)] (ops/unregister-op! op))
  (server/sync-op-tools!)
  nil)
