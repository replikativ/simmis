(ns is.simm.model.access-test
  "Authorization decisions over the shared system DB. Seeds a minimal
   party/KB/room/grant graph and asserts can-access-kb?/can-access-room? for
   the direct (owner, shared-with, membership) and transitive (member of a room
   holding a grant on the KB's system) paths, plus deny-by-default."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.model.access :as access]))

(def ^:private schema
  [{:db/ident :party/id :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :system/id :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :system/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
   {:db/ident :kb/owner :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}
   {:db/ident :kb/shared-with :db/valueType :db.type/uuid :db/cardinality :db.cardinality/many}
   {:db/ident :kb/db-scope :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :kb/system-id :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one}
   {:db/ident :kb/id :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :room/id :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :party/role :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
   {:db/ident :room/owner :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :room/parties :db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   {:db/ident :room/content-db-scope :db/valueType :db.type/uuid :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :grant/room :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :grant/system :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
   {:db/ident :grant/permission :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}])

(def owner (random-uuid))     ; owns the KB
(def sharee (random-uuid))    ; in :kb/shared-with
(def member (random-uuid))    ; member of a room with a :read grant on the KB
(def writer (random-uuid))    ; member of a room with a :read-write grant on the KB
(def merger (random-uuid))    ; member of a room with a :merge grant on the KB
(def stranger (random-uuid))  ; no relation to anything
(def room-mate (random-uuid)) ; member of the standalone room
(def admin-party (random-uuid))
(def kb-scope (random-uuid))
(def kb-id (random-uuid))
(def kb-sys (random-uuid))
(def room-scope (random-uuid))
(def room-id (random-uuid))
(def granted-room-scope (random-uuid))
(def rw-room-scope (random-uuid))
(def merge-room-scope (random-uuid))

(defn- test-db []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}
        _ (d/delete-database cfg)
        _ (d/create-database cfg)
        conn (d/connect cfg)]
    (d/transact conn schema)
    (d/transact
     conn
     [{:db/id "owner" :party/id owner}
      {:db/id "sharee" :party/id sharee}
      {:db/id "member" :party/id member}
      {:db/id "writer" :party/id writer}
      {:db/id "merger" :party/id merger}
      {:db/id "stranger" :party/id stranger}
      {:db/id "room-mate" :party/id room-mate}
      ;; an admin party
      {:party/id admin-party :party/role :admin}
      ;; a KB owned by `owner`, shared with `sharee`, registered as system kb-sys
      {:kb/id kb-id :kb/db-scope kb-scope :kb/owner owner :kb/shared-with #{sharee} :kb/system-id kb-sys}
      {:db/id "kb-sys" :system/id kb-sys}
      ;; a standalone room owned by `owner`, `room-mate` is a member
      {:room/id room-id :room/content-db-scope room-scope :room/owner "owner" :room/parties ["room-mate"]}
      ;; a room `member` belongs to, holding a READ grant on the KB's system
      {:db/id "grroom" :room/content-db-scope granted-room-scope :room/parties ["member"]}
      {:grant/room "grroom" :grant/system "kb-sys" :grant/permission :read}
      ;; a room `writer` belongs to, holding a READ-WRITE grant on the KB's system
      {:db/id "rwroom" :room/content-db-scope rw-room-scope :room/parties ["writer"]}
      {:grant/room "rwroom" :grant/system "kb-sys" :grant/permission :read-write}
      ;; a room `merger` belongs to, holding a MERGE grant on the KB's system
      {:db/id "mroom" :room/content-db-scope merge-room-scope :room/parties ["merger"]}
      {:grant/room "mroom" :grant/system "kb-sys" :grant/permission :merge}])
    conn))

(def ^:private can-kb? @#'access/party-can-access-kb?)
(def ^:private can-room? @#'access/party-can-access-room?)
(def ^:private kb-eid @#'access/kb-eid-by-scope)
(def ^:private room-eid @#'access/room-eid-by-scope)
(def ^:private kb-eid-by-id @#'access/kb-eid-by-id)
(def ^:private room-eid-by-id @#'access/room-eid-by-id)
(def ^:private admin? @#'access/admin?)

(deftest source-repository-access-rechecks-membership-grants-and-system-type
  (let [conn (test-db) scope (random-uuid)]
    (try
      (d/transact conn [{:db/id "repo" :system/id scope :system/type :repo}
                       {:db/id "grant" :grant/room [:room/id room-id]
                        :grant/system "repo" :grant/permission :read}])
      (doseq [permission [:read :read-write :owner]]
        (let [grant (d/q '[:find ?g . :in $ ?scope :where [?s :system/id ?scope] [?g :grant/system ?s]] @conn scope)]
          (d/transact conn [{:db/id grant :grant/permission permission}]))
        (is (access/sandbox-room-repository-allows? @conn room-mate room-id scope)))
      (is (not (access/sandbox-room-repository-allows? @conn stranger room-id scope)))
      (is (not (access/sandbox-room-repository-allows? @conn room-mate room-id (random-uuid))))
      (d/transact conn [{:system/id scope :system/type :kb}])
      (is (not (access/sandbox-room-repository-allows? @conn room-mate room-id scope)))
      (d/transact conn [{:system/id scope :system/type :repo}])
      (d/transact conn [[:db/retract [:room/id room-id] :room/parties [:party/id room-mate]]])
      (is (not (access/sandbox-room-repository-allows? @conn room-mate room-id scope)))
      (d/transact conn [{:room/id room-id :room/parties [[:party/id room-mate]]}])
      (let [grant (d/q '[:find ?g . :in $ ?scope :where [?s :system/id ?scope] [?g :grant/system ?s]] @conn scope)]
        (d/transact conn [{:db/id grant :grant/permission :read-only}])
        (is (not (access/sandbox-room-repository-allows? @conn room-mate room-id scope)))
        (d/transact conn [[:db/retractEntity grant]])
        (is (not (access/sandbox-room-repository-allows? @conn room-mate room-id scope))))
      (finally (d/release conn)))))

(deftest kb-access-direct-and-transitive
  (let [db @(test-db)
        kb (kb-eid db kb-scope)]
    (testing "direct: KB owner reads and writes"
      (is (true? (can-kb? db owner kb :read)))
      (is (true? (can-kb? db owner kb :write))))
    (testing "direct: shared-with reads and writes"
      (is (true? (can-kb? db sharee kb :read)))
      (is (true? (can-kb? db sharee kb :write))))
    (testing "transitive: member of a room granted on the KB's system reads"
      (is (true? (can-kb? db member kb :read))))
    (testing "deny: unrelated party" (is (false? (can-kb? db stranger kb :read))))
    (testing "deny: a room member with no grant on THIS kb"
      (is (false? (can-kb? db room-mate kb :read))))))

(deftest kb-write-refinement-by-grant-permission
  (let [db @(test-db)
        kb (kb-eid db kb-scope)]
    (testing "a :read grant permits read but NOT write"
      (is (true?  (can-kb? db member kb :read)))
      (is (false? (can-kb? db member kb :write))))
    (testing "a :read-write grant permits both"
      (is (true? (can-kb? db writer kb :read)))
      (is (true? (can-kb? db writer kb :write))))
    (testing "owner/shared write regardless of any grant"
      (is (true? (can-kb? db owner kb :write)))
      (is (true? (can-kb? db sharee kb :write))))
    (testing "an unrelated party neither reads nor writes"
      (is (false? (can-kb? db stranger kb :read)))
      (is (false? (can-kb? db stranger kb :write))))))

(deftest room-access-owner-and-member
  (let [db @(test-db)
        room (room-eid db room-scope)]
    (testing "room owner reads and writes"
      (is (true?  (can-room? db owner room :read)))
      (is (true?  (can-room? db owner room :write))))
    (testing "room member reads and writes — a room IS its conversation"
      (is (true?  (can-room? db room-mate room :read)))
      (is (true?  (can-room? db room-mate room :write))))
    (testing "deny: non-member"
      (is (false? (can-room? db stranger room :read)))
      (is (false? (can-room? db stranger room :write))))))

(deftest room-merge-and-grant-stop-at-the-owner
  ;; The B1 rule: membership is generous for :read/:write and confers NOTHING
  ;; for the irreversible half. Before this, `party-can-access-room?` took no
  ;; action at all, so every one of the 21 `:action W` room policies — and any
  ;; future :merge/:grant — resolved to "is a member".
  (let [db @(test-db)
        room (room-eid db room-scope)]
    (testing "owner may merge and grant"
      (is (true? (can-room? db owner room :merge)))
      (is (true? (can-room? db owner room :grant))))
    (testing "a MEMBER may write but may NOT merge or grant"
      (is (true?  (can-room? db room-mate room :write)))
      (is (false? (can-room? db room-mate room :merge)))
      (is (false? (can-room? db room-mate room :grant))))
    (testing "a stranger gets nothing"
      (is (false? (can-room? db stranger room :merge)))
      (is (false? (can-room? db stranger room :grant))))))

(deftest merge-is-not-implied-by-write
  ;; The whole point of the added `:merge` permission value: writing a fork is
  ;; the generous half of the review model, landing it onto trunk is not.
  (let [db @(test-db)
        kb (kb-eid db kb-scope)]
    (testing ":read-write grants write but NOT merge"
      (is (true?  (can-kb? db writer kb :write)))
      (is (false? (can-kb? db writer kb :merge))))
    (testing ":merge grants both write and merge (it is strictly stronger)"
      (is (true? (can-kb? db merger kb :read)))
      (is (true? (can-kb? db merger kb :write)))
      (is (true? (can-kb? db merger kb :merge))))
    (testing ":read grants neither"
      (is (false? (can-kb? db member kb :write)))
      (is (false? (can-kb? db member kb :merge))))
    (testing "the KB owner merges directly, no grant needed"
      (is (true? (can-kb? db owner kb :merge))))
    (testing "a direct share is :read-write (access/shared-with-permission): no merge, no grant"
      (is (false? (can-kb? db sharee kb :merge)))
      (is (false? (can-kb? db sharee kb :grant))))
    (testing "a stranger merges nothing"
      (is (false? (can-kb? db stranger kb :merge))))))

(deftest unknown-actions-are-denied-not-waved-through
  ;; permission-satisfies? used to fall through to `true` for anything that was
  ;; not :write. With a four-verb vocabulary that would make a typo'd action as
  ;; permissive as :read on every grant-reachable resource.
  (let [db @(test-db)
        kb (kb-eid db kb-scope)]
    (is (false? (can-kb? db writer kb :delete)))
    (is (false? (can-kb? db writer kb :wrtie)))
    (is (false? (can-kb? db merger kb :grant)))
    (testing "the owner path is direct and so is unaffected by the action"
      (is (true? (can-kb? db owner kb :delete))))))

(deftest kb-answers-the-same-by-db-scope-and-by-system-id
  ;; A KB has BOTH a :kb/db-scope and a :kb/system-id, and can? resolves them in
  ;; different branches — kb-eid-by-scope for the first, party-can-access-system?
  ;; for the second. party-can-access-system? is grant-only (it was written for a
  ;; room's own repo/messages store, which have no owner field), so before it
  ;; delegated, the SAME KB refused its OWN OWNER when named by system id.
  ;; Measured on live data 2026-07-30. It failed closed, so it was never a hole;
  ;; B1 made it visible, because until :merge existed the :read-write grant
  ;; satisfied :write and the two paths agreed by accident.
  (let [db @(test-db)]
    (doseq [action [:read :write :merge]]
      (testing (str "owner agrees across identifiers for " action)
        (is (= (access/can? db owner action kb-scope)
               (access/can? db owner action kb-sys))
            (str "owner disagreed on " action)))
      (testing (str "a :read-only grantee agrees across identifiers for " action)
        (is (= (access/can? db member action kb-scope)
               (access/can? db member action kb-sys))))
      (testing (str "a :read-write grantee agrees across identifiers for " action)
        (is (= (access/can? db writer action kb-scope)
               (access/can? db writer action kb-sys))))
      (testing (str "a stranger agrees across identifiers for " action)
        (is (= (access/can? db stranger action kb-scope)
               (access/can? db stranger action kb-sys)))))
    (testing "and the agreed answer is the KB rule, not the grant-only rule"
      (is (true?  (access/can? db owner :merge kb-sys)))
      (is (false? (access/can? db writer :merge kb-sys)))
      (is (true?  (access/can? db writer :write kb-sys))))))

(deftest can?-end-to-end-over-an-explicit-db
  ;; B0: `can?` takes the system-DB VALUE, so the whole decision — subject
  ;; normalization, resource resolution and graph traversal — is testable
  ;; without installing a global connection. This is the arity every new call
  ;; site should use.
  (let [db @(test-db)
        principal {:sub (str writer)}]
    (testing "a bare KB scope resolves and honours the grant"
      (is (true?  (access/can? db principal :read  kb-scope)))
      (is (true?  (access/can? db principal :write kb-scope)))
      (is (false? (access/can? db principal :merge kb-scope))))
    (testing "the same subject as a uuid and as a uuid-string agree"
      (is (true? (access/can? db writer       :write kb-scope)))
      (is (true? (access/can? db (str writer) :write kb-scope))))
    (testing "{:kb id} and {:room id} control-plane shapes"
      (is (true?  (access/can? db owner    :merge {:kb kb-id})))
      (is (false? (access/can? db writer   :merge {:kb kb-id})))
      (is (true?  (access/can? db room-mate :write {:room room-id})))
      (is (false? (access/can? db room-mate :merge {:room room-id})))
      (is (true?  (access/can? db owner    :merge {:room room-id}))))
    (testing ":admin resource needs the admin role"
      (is (true?  (access/can? db admin-party :read :admin)))
      (is (false? (access/can? db owner       :read :admin))))
    (testing "any authenticated party clears :authenticated; anonymous does not"
      (is (true?  (access/can? db stranger :read :authenticated)))
      (is (false? (access/can? db nil      :read :authenticated))))
    (testing "a nil db denies everything, however good the subject"
      (is (false? (access/can? nil owner :read kb-scope))))))

(deftest can?-resolves-scope-and-denies-anonymous
  ;; The 3-arity reads system-db/get-conn (a global), so with no server running
  ;; it returns false — which is the correct deny-by-default.
  (testing "anonymous subject is always denied"
    (is (false? (access/can? nil :read kb-scope)))
    (is (false? (access/can? {:sub nil} :read kb-scope))))
  (testing "a subject with no resolvable scope is denied"
    (is (false? (access/can? {:sub (str owner)} :read nil))))
  (testing "subject normalization accepts uuid, uuid-string, and principal map"
    (is (= owner (access/subject->party-uuid owner)))
    (is (= owner (access/subject->party-uuid (str owner))))
    (is (= owner (access/subject->party-uuid {:sub (str owner)})))
    (is (nil? (access/subject->party-uuid {:sub nil})))
    (is (nil? (access/subject->party-uuid nil)))))

(deftest control-plane-resource-resolution
  ;; the by-id lookups + role check that back can?'s {:kb}/{:room}/:admin shapes
  (let [db @(test-db)]
    (testing "kb-eid-by-id resolves and feeds the same access check as by-scope"
      (let [e (kb-eid-by-id db kb-id)]
        (is (some? e))
        (is (true?  (can-kb? db owner e :read)))
        (is (true?  (can-kb? db member e :read)))    ;; transitive read
        (is (false? (can-kb? db member e :write)))   ;; :read grant → no write
        (is (false? (can-kb? db stranger e :read)))))
    (testing "room-eid-by-id resolves and feeds the room access check"
      (let [e (room-eid-by-id db room-id)]
        (is (some? e))
        (is (true?  (can-room? db room-mate e :read)))
        (is (false? (can-room? db stranger e :read)))))
    (testing "admin? is true only for the :admin-role party"
      (is (true?  (admin? db admin-party)))
      (is (false? (admin? db owner)))
      (is (false? (admin? db stranger))))
    (testing "unknown ids resolve to nil (=> can? denies)"
      (is (nil? (kb-eid-by-id db (random-uuid))))
      (is (nil? (room-eid-by-id db (random-uuid)))))))

;; =============================================================================
;; Read-side twin of the merge gate (ops.proposals/with-merge-authority)
;; =============================================================================

(deftest merge-authority-annotation-matches-the-gate
  ;; The card must not offer an Accept that `authorize-forks!` will refuse, and
  ;; must not hide one it would allow. Same db, same verb, same answer — this
  ;; asserts the affordance cannot drift from the gate.
  (let [db @(test-db)
        annotate (requiring-resolve 'is.simm.ops.proposals/with-merge-authority)
        ;; one fork per scope: a KB `owner` owns, and a KB reached only by a
        ;; :read-write room grant (writable, NOT landable since B1)
        proposal {:proposal/id (random-uuid)
                  :proposal/forks [{:proposal.fork/scope kb-scope
                                    :proposal.fork/branch "on-owned-kb"}
                                   {:proposal.fork/scope rw-room-scope
                                    :proposal.fork/branch "on-granted-room"}]}
        by-branch (fn [party]
                    (into {} (map (juxt :proposal.fork/branch :proposal.fork/may-merge?))
                          (:proposal/forks (first (annotate db party [proposal])))))]
    (testing "the KB owner may land the patch on their own KB"
      (is (true? (get (by-branch owner) "on-owned-kb"))))
    (testing "a :read-write grantee may NOT land onto the KB they can write"
      (is (false? (get (by-branch writer) "on-owned-kb"))))
    (testing "a stranger may land nothing"
      (is (false? (get (by-branch stranger) "on-owned-kb")))
      (is (false? (get (by-branch stranger) "on-granted-room"))))
    (testing "anonymous is denied without consulting the graph"
      (is (false? (get (by-branch nil) "on-owned-kb"))))
    (testing "the annotation agrees with can? fork by fork — no drift"
      (doseq [party [owner writer member stranger]
              fk (:proposal/forks proposal)]
        (is (= (access/can? db party :merge (:proposal.fork/scope fk))
               (get (by-branch party) (:proposal.fork/branch fk)))
            (str "drift for " party " on " (:proposal.fork/branch fk)))))))
