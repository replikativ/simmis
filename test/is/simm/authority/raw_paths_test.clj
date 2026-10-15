(ns is.simm.authority.raw-paths-test
  "The raw paths of plan §2.7 — capabilities that bypass the op model — each
   EXECUTED, not read: the sandbox's creator-KB fallback, the client-supplied
   author of `eval-in-room!`, and datahike.kabel's raw `dispatch` into a
   room's message store. Every attack is paired with the legitimate use of the
   same path."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.kabel.handlers :as dh-handlers]
            [kabel.remote :as kremote]
            [sci.core :as sci]
            [is.simm.agents.room-agents :as ra]
            [is.simm.authority.known-hole :refer [known-hole]]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.model.store :as store]
            [is.simm.runtimes.branching :as branching]
            [is.simm.runtimes.context :as ctx]
            [is.simm.uis.web.desktop.chat-remote]
            [is.simm.uis.web.desktop.sandbox-remote]))

;; =============================================================================
;; §2.7 sandbox kb/conn — the creator-KB fallback
;; =============================================================================

(defn- fresh-kb!
  "A registered, branchable KB store under `scope`, as campaign-test builds one."
  [scope]
  (let [cfg {:store {:backend :memory :id scope} :keep-history? true
             :crypto-hash? true :commit-graph? true :schema-flexibility :write}]
    (d/create-database cfg)
    (doto (d/connect cfg) (store/install!) (branching/register-system! scope))))

(defn- member-sandbox
  "The member's SCI context in `room`, with `kb/*` installed over exactly the
   KB conns the agent boot path hands that room (`get-room-kb-conns`)."
  [w room]
  (let [sc (sci/init {})]
    (#'ra/add-kb-write-ns! sc (#'ra/get-room-kb-conns (get w room))
                           #(#'ra/active-overlay (get w room) (:member w)) (:member w))
    sc))

(defn- page-on-trunk? [scope title]
  (some? (d/q '[:find ?e . :in $ ?t :where [?e :S.Page/title ?t]]
              @(branching/get-kb-conn scope) title)))

(deftest the-sandbox-reaches-only-granted-kbs
  (ctx/with-server-context
    (world/with-world [_ w]
      (fresh-kb! (:kb-granted-scope w))
      (fresh-kb! (:kb-private-scope w))
      (with-redefs [kbs/connect-kb-database #(branching/get-kb-conn %)]
        (testing "legitimate: in the team room the sandbox writes the KB the room is granted"
          (let [sc (member-sandbox w :room)]
            (is (= ["Handbook"] (mapv :name (#'ra/get-room-kb-conns (:room w)))))
            (sci/eval-string* sc "(kb/ensure-page! \"Handbook\" \"Onboarding\")")
            (is (page-on-trunk? (:kb-granted-scope w) "Onboarding"))
            (is (thrown? Exception (sci/eval-string* sc "(kb/conn \"Archive\")"))
                "the owner's ungranted KB is not addressable from the team room")))
        (testing "the member has no write authority over the owner's private KB"
          (is (false? (access/can? (world/principal w :member) :write (:kb-private-scope w)))))
        (known-hole
         "§2.7 kb/conn creator-KB fallback"
         "in a room with no KB grant, the sandbox falls back to the room CREATOR's first KB: a member writes the owner's private KB on trunk"
         (let [sc (member-sandbox w :room-nogrant)]
           (sci/eval-string* sc "(kb/ensure-page! \"Archive\" \"written from Scratch\")")
           (and (some? (sci/eval-string* sc "(kb/conn \"Archive\")"))
                (page-on-trunk? (:kb-private-scope w) "written from Scratch"))))))))

;; =============================================================================
;; §2.7 eval-in-room! — the author is whoever the client says
;; =============================================================================

(defn- room-store-conn
  "A stand-in for the room's store, with the entities eval entries point at."
  [w]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :read
             :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn [{:db/ident :entity/uuid :db/valueType :db.type/uuid
                         :db/unique :db.unique/identity :db/cardinality :db.cardinality/one}
                        {:db/ident :S.EvalEntry/room :db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/one}
                        {:db/ident :S.EvalEntry/agent :db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/one}])
      (d/transact conn (mapv (fn [k] {:entity/uuid (get w k)}) [:room :owner :member]))
      conn)))

(defn- eval-authors
  "code → the author uuid recorded for it in the room store."
  [conn]
  (into {} (d/q '[:find ?code ?a :where [?e :S.EvalEntry/code ?code]
                  [?e :S.EvalEntry/agent ?ae] [?ae :entity/uuid ?a]] @conn)))

(deftest eval-in-room-records-the-caller-as-author
  (world/with-world [_ w]
    (let [conn (room-store-conn w)
          sc (sci/init {})
          call! (fn [kind code user-id]
                  (world/inbound! (world/principal w kind) "eval-in-room!"
                                  {:room-uuid-str (str (:room w)) :code code
                                   :user-uuid-str (str user-id)}))]
      (with-redefs [ra/room-context-state (fn [_] {:sci-ctx sc})
                    room-dbs/connect-room-database (fn [_] conn)]
        (testing "legitimate: the owner's eval is recorded as the owner's"
          (is (= {:success true :result "=> 2"} (:result (call! :owner "(+ 1 1)" (:owner w)))))
          (is (= (:owner w) (get (eval-authors conn) "(+ 1 1)"))))
        (testing "a non-member is refused at the gate and records nothing"
          (is (world/refused? (call! :non-member "(+ 2 2)" (:non-member w))))
          (is (nil? (get (eval-authors conn) "(+ 2 2)"))))
        (known-hole
         "§2.7 eval-in-room! client-supplied user id"
         "eval-in-room! records the user-uuid-str the CLIENT sends as the author: the member's code lands attributed to the owner"
         (call! :member "(str :forged)" (:owner w))
         (= (:owner w) (get (eval-authors conn) "(str :forged)")))))))

;; =============================================================================
;; §2.7 datahike.kabel dispatch into a room's message store
;; =============================================================================

(defn- forged-message [w]
  {:entity/uuid (random-uuid) :block/content "I resign." :S.Message/author (:owner w)})

(defn- messages-by [conn author]
  (set (d/q '[:find [?c ...] :in $ ?a
              :where [?m :S.Message/author ?a] [?m :block/content ?c]] @conn author)))

(defn- schemaless-conn []
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :read
             :keep-history? false}]
    (d/create-database cfg)
    (d/connect cfg)))

(deftest a-member-cannot-forge-a-message-in-the-room-store
  ;; THE SHAPE TODAY. `rpc-policy` admits a member to `dispatch` on the room's
  ;; store (the control-plane matrix carries that as a known hole). But the
  ;; handler datahike registers lives in `kabel.remote/functions`, and the
  ;; simmis server composes distributed-scope's middleware, never
  ;; `kabel.remote/serve`, so the network-inbound path finds no `dispatch` and
  ;; the forged datom never lands. If this starts FAILING, the handler became
  ;; reachable and the policy row is a live forgery hole — fix the row
  ;; (§2.7: refuse dispatch on message stores) before anything else.
  (world/with-world [_ w]
    (let [room-store (schemaless-conn)]
      (dh-handlers/register-global-handlers! (atom {:id (random-uuid)}))
      (dh-handlers/register-connection-for-store! (:room-scope w) :db room-store nil)
      (try
        (testing "the attack: a raw transact of a message authored by the owner"
          (let [resp (world/inbound! (world/principal w :member) 'datahike.kabel/dispatch
                                     {:store-id (:room-scope w)
                                      :arg-map {:op 'transact! :args [{:tx-data [(forged-message w)]}]}})]
            (is (some? (:error resp)) "no reachable handler")
            (is (empty? (messages-by room-store (:owner w))) "no forged message landed")))
        (testing "what the handler WOULD do if served: it checks nothing itself"
          ;; called directly, the way kabel.remote/serve would call it after a
          ;; permissive gate — this is why the policy row matters
          (let [f (get @kremote/functions 'datahike.kabel/dispatch)
                rep (clojure.core.async/<!! (f {:store-id (:room-scope w)
                                                :arg-map {:op 'transact!
                                                          :args [{:tx-data [(forged-message w)]}]}}))]
            (is (map? rep))
            (is (= #{"I resign."} (messages-by room-store (:owner w))))))
        (finally
          (dh-handlers/unregister-connection-for-store! (:room-scope w) :db))))))

(deftest a-member-posts-as-themselves
  ;; The legitimate counterpart of forging: posting through the op that
  ;; derives the author from the JWT, whatever sender id the client passes.
  (world/with-world [_ w]
    (let [posted (atom [])]
      (with-redefs [room-dbs/connect-room-database (fn [_] (schemaless-conn))
                    is.simm.uis.web.desktop.chat-remote/ensure-room-chatroom! (fn [& _])
                    is.simm.uis.web.desktop.chat-remote/ensure-party-projection! (fn [& _])
                    ra/post-user-message! (fn [room-uuid msg sender-id & _]
                                            (swap! posted conj [room-uuid msg sender-id]))]
        (let [send! (fn [kind sender-id text]
                      (world/inbound! (world/principal w kind) "dispatch-message!"
                                      {:room-id-str (str (:room w)) :sender-id-str (str sender-id)
                                       :content text :msg-uuid-str (str (random-uuid))
                                       :in-reply-to-str nil}))]
          (send! :member (:member w) "hello")
          (send! :member (:owner w) "posing as the owner")
          (is (world/refused? (send! :non-member (:non-member w) "let me in")))
          (is (= [[(:room w) "hello" (:member w)]
                  [(:room w) "posing as the owner" (:member w)]]
                 @posted)
              "both posts land, both as the member; the outsider's does not"))))))
