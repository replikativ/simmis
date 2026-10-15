(ns is.simm.authority.world
  "The shared fixture of the authority matrix: one real system DB (in memory,
   with dvergr's and simmis's schema exactly as `system-db/init!` installs
   them), seeded with the five principals and the resources they contend over,
   and `inbound!`, which delivers an RPC through distributed-scope's own
   network-inbound dispatch — the gate `authorize-remote`, the `*principal*`
   binding, the registered handler — rather than a re-implementation of it.

   No stubs of `can?`: every decision in the matrix is the real predicate over
   real grants. What IS redefined is where the model namespaces find their
   connections (`system-db/get-conn`, and dvergr's `get-conn`, which would
   otherwise open `.dvergr/system-db` on disk).

   THE WORLD

     owner       owns room `:room` (and is a member), owns KBs `:kb-granted`
                 Handbook (attached to `:room` :read-write) and `:kb-private`
                 Archive (attached nowhere; first by name, so it is what the
                 creator-KB fallback picks)
     member      member of `:room`; owns nothing
     non-member  owns `:other-room` and `:kb-stranger`; no relation to `:room`
     admin       `:party/role :admin`; member of nothing
     anon        no principal at all (nil)

   Two agent parties: `:team-agent` (owned by the owner, in `:room`) and
   `:stranger-agent` (owned by the non-member, in no room). The admin, too, is
   in no room — which matters to `remove-agent-from-room!`.

   Two drives, attached nowhere: `:drive-owner` (the owner's) and
   `:drive-stranger` (the non-member's).

   Plus the team room's own repo system (`:repo-sys`, granted :owner to `:room`
   as dvergr's provisioning records it), a proposal on `:room` with two open
   forks of the Handbook (`\"member-fork\"` by the member, `\"agent-fork\"` by the
   team agent), a mail account of the owner, and
   `:room-nogrant`, a room the owner created with the member in it and NO KB
   grant (the creator-KB fallback's precondition)."
  (:require [clojure.core.async :as a]
            [datahike.api :as d]
            [dvergr.chat.schema :as chat-schema]
            [dvergr.system.db :as dsdb]
            [is.simm.distributed-scope :as ds]
            [is.simm.model.access :as access]
            [is.simm.model.parties :as parties]
            [is.simm.model.system-db :as system-db]
            [superv.async :as sv]))

(def principal-kinds
  "Every principal the matrix asks about, in report order."
  [:owner :member :non-member :admin :anon])

(defn fresh-system-conn
  "An in-memory system DB carrying the schema `system-db/init!` installs:
   dvergr's system/actor/assignment/pricing/task schema, its chat schema, and
   simmis's extension schema on the same entities."
  []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn (vec (concat dsdb/schema dsdb/actor-schema dsdb/assignment-schema
                                    dsdb/pricing-schema dsdb/task-schema)))
      (chat-schema/ensure-full-schema! conn)
      (d/transact conn system-db/schema)
      conn)))

(defn- party-row [id display & {:as more}]
  (merge {:party/id id
          :party/type :human
          :party/display-name display
          :actor/id (parties/party-id->actor-id id)
          :actor/kind :human
          :actor/name display
          :actor/status :online
          :actor/created-at (java.util.Date.)}
         more))

(defn seed!
  "Seed `conn` with the world described in the ns doc. Returns the id map."
  [conn]
  (let [w {:owner (random-uuid) :member (random-uuid)
           :non-member (random-uuid) :admin (random-uuid)
           :team-agent (random-uuid) :stranger-agent (random-uuid)
           :room (random-uuid) :room-scope (random-uuid)
           :room-nogrant (random-uuid) :room-nogrant-scope (random-uuid)
           :other-room (random-uuid) :other-room-scope (random-uuid)
           :kb-granted (random-uuid) :kb-granted-scope (random-uuid) :kb-granted-sys (random-uuid)
           :kb-private (random-uuid) :kb-private-scope (random-uuid) :kb-private-sys (random-uuid)
           :kb-stranger (random-uuid) :kb-stranger-scope (random-uuid) :kb-stranger-sys (random-uuid)
           :repo-sys (random-uuid)
           :proposal (random-uuid)
           :mail (random-uuid) :mail-sys (random-uuid)
           :drive-owner (random-uuid) :drive-owner-sys (random-uuid)
           :drive-stranger (random-uuid) :drive-stranger-sys (random-uuid)}
        now (java.util.Date.)
        pref (fn [k] [:party/id (get w k)])]
    (d/transact conn [(party-row (:owner w) "Owner")
                      (party-row (:member w) "Member")
                      (party-row (:non-member w) "Stranger")
                      (party-row (:admin w) "Admin" :party/role :admin)])
    ;; agents: one the owner runs in the team room, one the stranger owns
    (d/transact conn [(party-row (:team-agent w) "Team Agent" :party/type :agent
                                 :actor/kind :agent :party/owner (pref :owner))
                      (party-row (:stranger-agent w) "Stranger's Agent" :party/type :agent
                                 :actor/kind :agent :party/owner (pref :non-member))])
    (d/transact
     conn
     [;; the contended room: owner owns it, member is in it
      {:room/id (:room w) :room/name "Team" :room/type :group
       :room/owner (pref :owner) :room/created-by (:owner w)
       :room/parties [(pref :owner) (pref :member) (pref :team-agent)]
       :room/content-db-scope (:room-scope w)}
      ;; a room the owner created that holds NO KB grant
      {:room/id (:room-nogrant w) :room/name "Scratch" :room/type :group
       :room/owner (pref :owner) :room/created-by (:owner w)
       :room/parties [(pref :owner) (pref :member)]
       :room/content-db-scope (:room-nogrant-scope w)}
      ;; the stranger's own room
      {:room/id (:other-room w) :room/name "Elsewhere" :room/type :group
       :room/owner (pref :non-member) :room/created-by (:non-member w)
       :room/parties [(pref :non-member)]
       :room/content-db-scope (:other-room-scope w)}
      ;; KBs, each registered as a dvergr system
      {:db/id "sys-granted" :system/id (:kb-granted-sys w) :system/type :kb :system/name "Handbook"}
      {:db/id "sys-private" :system/id (:kb-private-sys w) :system/type :kb :system/name "Archive"}
      {:db/id "sys-stranger" :system/id (:kb-stranger-sys w) :system/type :kb :system/name "Stranger Private"}
      {:kb/id (:kb-granted w) :kb/name "Handbook" :kb/owner (:owner w)
       :kb/db-scope (:kb-granted-scope w) :kb/system-id (:kb-granted-sys w) :kb/created now}
      {:kb/id (:kb-private w) :kb/name "Archive" :kb/owner (:owner w)
       :kb/db-scope (:kb-private-scope w) :kb/system-id (:kb-private-sys w) :kb/created now}
      {:kb/id (:kb-stranger w) :kb/name "Stranger Private" :kb/owner (:non-member w)
       :kb/db-scope (:kb-stranger-scope w) :kb/system-id (:kb-stranger-sys w) :kb/created now}
      ;; the only grant: Handbook → Team, read-write
      {:grant/id (random-uuid) :grant/room [:room/id (:room w)] :grant/system "sys-granted"
       :grant/permission :read-write :grant/created now}
      ;; the team room's own repo, attached the way dvergr's provisioning does
      {:db/id "sys-repo" :system/id (:repo-sys w) :system/type :repo :system/name "team-repo"}
      {:grant/id (random-uuid) :grant/room [:room/id (:room w)] :grant/system "sys-repo"
       :grant/permission :owner :grant/created now}
      ;; a proposal on the team room
      {:proposal/id (:proposal w) :proposal/room (:room w) :proposal/status :open
       :proposal/title "Change the handbook"
       ;; two open forks of the Handbook: one the member wrote, one the
       ;; owner's agent wrote
       :proposal/forks [{:proposal.fork/scope (:kb-granted-scope w)
                         :proposal.fork/branch "member-fork"
                         :proposal.fork/author (:member w)}
                        {:proposal.fork/scope (:kb-granted-scope w)
                         :proposal.fork/branch "agent-fork"
                         :proposal.fork/author (:team-agent w)}]}
      ;; drives, each registered as a dvergr :fs system, attached nowhere
      {:db/id "sys-drive-owner" :system/id (:drive-owner-sys w) :system/type :fs :system/name "Owner Drive"}
      {:db/id "sys-drive-stranger" :system/id (:drive-stranger-sys w) :system/type :fs :system/name "Stranger Drive"}
      {:drive/id (:drive-owner w) :drive/name "Owner Drive" :drive/owner (:owner w)
       :drive/system-id (:drive-owner-sys w) :drive/created now}
      {:drive/id (:drive-stranger w) :drive/name "Stranger Drive" :drive/owner (:non-member w)
       :drive/system-id (:drive-stranger-sys w) :drive/created now}
      ;; the owner's mail account
      {:mail-account/id (:mail w) :mail-account/owner (:owner w)
       :mail-account/system-id (:mail-sys w)}])
    w))

(defn principal
  "The JWT principal kabel-auth attaches for `kind` in world `w`: `{:sub
   \"<party uuid>\"}`, nil for `:anon`."
  [w kind]
  (when-not (= :anon kind)
    {:sub (str (get w kind))}))

(defn with-world*
  "Call `(f conn w)` with a fresh seeded system DB installed as both simmis's
   and dvergr's system connection."
  [f]
  (let [conn (fresh-system-conn)
        w (seed! conn)]
    (try
      (with-redefs [system-db/get-conn (constantly conn)
                    dsdb/get-conn (constantly conn)]
        (f conn w))
      (finally (d/release conn)))))

(defmacro with-world
  "`(with-world [conn w] body…)` — see `with-world*`."
  [[conn w] & body]
  `(with-world* (fn [~conn ~w] ~@body)))

;; =============================================================================
;; The network-inbound path
;; =============================================================================

(defn registered-fn
  "The registry key of the remote whose normalized name is `fn-name`, e.g.
   \"cancel-room-run!\" → `…chat-remote/spin-remote-cancel-room-run!-0`."
  [fn-name]
  (let [hits (filter #(= fn-name (access/normalize-remote-name %))
                     (keys @ds/remote-fn-registry))]
    (when (not= 1 (count hits))
      (throw (ex-info "expected exactly one registered remote"
                      {:fn-name fn-name :hits (vec hits)})))
    (first hits)))

(defn inbound!
  "Deliver `fn-name` (a normalized name such as \"cancel-room-run!\", or a
   symbol sent verbatim, as a client names a fn the registry may not hold)
   with `arg-map` as a network-inbound invocation from
   `principal`, through distributed-scope's `dispatch-invocation!` with simmis's
   `authorize-remote` as the gate — the path a websocket message takes once
   kabel-auth has attached `:kabel/principal`. Returns
   `{:result …}` or `{:error \"…\" :refused? bool}`; `:refused?` is true when the
   GATE refused (the handler never ran)."
  [principal fn-name arg-map]
  (let [peer-id (random-uuid)
        router-id (random-uuid)
        out (a/chan 1)
        owners @#'ds/invocation-owners]
    (swap! owners assoc peer-id {:supervisor sv/S
                                 :authorize-fn access/authorize-remote
                                 :router-id router-id})
    (try
      (@#'ds/dispatch-invocation! peer-id router-id
                                  {:fn-name (if (symbol? fn-name) fn-name (registered-fn fn-name))
                                   :arg-map arg-map
                                   :scope peer-id
                                   :request-id (random-uuid)
                                   :request-scope peer-id
                                   :kabel/principal principal
                                   ::ds/reply-out out})
      (let [[resp port] (a/alts!! [out (a/timeout 30000)])]
        (when-not (= port out)
          (throw (ex-info "inbound invocation produced no reply" {:fn fn-name})))
        (if-let [e (:error resp)]
          {:error e
           :refused? (boolean (re-find #":type :(not-authorized|authentication-required)" e))}
          {:result (:result resp)}))
      (finally (swap! owners dissoc peer-id)))))

(defn refused? [resp] (true? (:refused? resp)))
