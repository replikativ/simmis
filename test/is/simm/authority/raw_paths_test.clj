(ns is.simm.authority.raw-paths-test
  "The raw paths of plan §2.7 — capabilities that bypass the op model — each
   EXECUTED, not read: the sandbox's KB handles (once with a creator-KB
   fallback), the author and the gate of `eval-in-room!`, and datahike.kabel's
   raw `dispatch` into a room's message store. All three are closed; every
   attack is paired with the legitimate use of the same path."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.kabel.handlers :as dh-handlers]
            [kabel.remote :as kremote]
            [sci.core :as sci]
            [is.simm.agents.room-agents :as ra]
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

(defn- sandbox-of
  "`kind`'s SCI context in `room`, with `kb/*` installed over exactly the KB
   conns the agent boot path hands that party in that room
   (`get-room-kb-conns`) — `kind` is the acting party, as the agent is for its
   own sandbox."
  [w room kind]
  (let [sc (sci/init {})
        party (get w kind)]
    (#'ra/add-kb-write-ns! sc (#'ra/get-room-kb-conns (get w room) party)
                           #(#'ra/active-overlay (get w room) party) party)
    sc))

(defn- page-on-trunk? [scope title]
  (some? (d/q '[:find ?e . :in $ ?t :where [?e :S.Page/title ?t]]
              @(branching/get-kb-conn scope) title)))

(defn- not-authorized?
  "Did `thunk` throw a `:not-authorized` refusal? SCI wraps what sandbox code
   throws, so the refusal is looked for along the cause chain."
  [thunk]
  (try (thunk) false
       (catch Exception e
         (boolean (some #(= :not-authorized (:type (ex-data %)))
                        (take-while some? (iterate ex-cause e)))))))

(defn- grant! [conn w room sys-key permission]
  (d/transact conn [{:grant/id (random-uuid) :grant/room [:room/id (get w room)]
                     :grant/system [:system/id (get w sys-key)]
                     :grant/permission permission :grant/created (java.util.Date.)}]))

(deftest saved-wiki-reads-recheck-room-membership-and-kb-grants
  (doseq [revoke [:membership :grant]]
    (ctx/with-server-context
      (world/with-world [conn w]
        (let [kb (fresh-kb! (:kb-granted-scope w))]
          (d/transact kb [{:S.Page/title "Read sentinel" :entity/uuid (random-uuid)}])
          (when (= :grant revoke)
            (let [other (fresh-kb! (:kb-private-scope w))]
              (d/transact other [{:S.Page/title "Still authorized" :entity/uuid (random-uuid)}])
              (grant! conn w :room :kb-private-sys :read)))
          (with-redefs [kbs/connect-kb-database #(branching/get-kb-conn %)]
            (let [sc (sci/init {})]
              (#'ra/add-wiki-ns! sc (#'ra/get-room-kb-conns (:room w) (:member w)) (:room w) (:member w))
              (sci/eval-string* sc "(def saved-pages wiki/pages)")
              (let [pages (sci/eval-string* sc "(saved-pages)")]
                (is (= (if (= :grant revoke) #{"Archive" "Handbook"} #{"Handbook"}) (set (map :kb pages))))
                (is (some #{"Read sentinel"} (:pages (first (filter #(= "Handbook" (:kb %)) pages))))))
              (is (= {:kb "Handbook" :title "Read sentinel" :blocks []}
                     (sci/eval-string* sc "(wiki/read-page \"Read sentinel\")")))
              (case revoke
                :membership
                (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                :grant
                (let [grant (d/q '[:find ?g . :in $ ?room ?sys
                                   :where [?r :room/id ?room] [?s :system/id ?sys]
                                   [?g :grant/room ?r] [?g :grant/system ?s]]
                                 @conn (:room w) (:kb-granted-sys w))]
                  (is (some? grant))
                  (d/transact conn [[:db/retractEntity grant]])))
              (when (= :grant revoke)
                ;; An incomplete cached selection refuses as a whole, rather
                ;; than presenting a silently filtered/partial wiki as complete.
                (is (access/sandbox-kb-allows? @conn (:member w) (:room w) (:kb-private w) :read))
                (is (not-authorized? #(sci/eval-string* sc "(wiki/read-page \"Still authorized\")"))))
              (doseq [source ["(saved-pages)" "(wiki/pages)" "(wiki/read-page \"Read sentinel\")"
                              "(wiki/search \"Read sentinel\")" "(wiki/backlinks \"Read sentinel\")"
                              "(wiki/neighborhood \"Read sentinel\")"]]
                (is (not-authorized? #(sci/eval-string* sc source)) (str revoke ": " source))))))))))

(deftest the-sandbox-reaches-only-granted-kbs
  (ctx/with-server-context
    (world/with-world [conn w]
      (fresh-kb! (:kb-granted-scope w))
      (fresh-kb! (:kb-private-scope w))
      (with-redefs [kbs/connect-kb-database #(branching/get-kb-conn %)]
        (testing "legitimate: in the team room the sandbox writes the KB the room is granted"
          (let [sc (sandbox-of w :room :member)]
            (is (= ["Handbook"] (mapv :name (#'ra/get-room-kb-conns (:room w) (:member w)))))
            (sci/eval-string* sc "(kb/ensure-page! \"Handbook\" \"Onboarding\")")
            (is (page-on-trunk? (:kb-granted-scope w) "Onboarding"))
            (is (thrown? Exception (sci/eval-string* sc "(kb/conn \"Archive\")"))
                "the owner's ungranted KB is not addressable from the team room")))
        (testing "legitimate: the room's agent writes it too — the agent is the acting party"
          (let [sc (sandbox-of w :room :team-agent)]
            (sci/eval-string* sc "(kb/ensure-page! \"Handbook\" \"Agent notes\")")
            (is (page-on-trunk? (:kb-granted-scope w) "Agent notes"))))
        (testing "legitimate: the owner keeps the raw conn in the room they own (plan §2.9.3)"
          (let [sc (sandbox-of w :room :owner)
                c (sci/eval-string* sc "(kb/conn \"Handbook\")")]
            (d/transact c [{:S.Page/title "Raw" :entity/uuid (random-uuid)}])
            (is (page-on-trunk? (:kb-granted-scope w) "Raw"))))
        (testing "the member has no write authority over the owner's private KB"
          (is (false? (access/can? (world/principal w :member) :write (:kb-private-scope w)))))
        (testing "§2.7 (closed): a room with no KB grant reaches no KB — no creator fallback"
          (is (= [] (#'ra/get-room-kb-conns (:room-nogrant w) (:member w))))
          (is (= [] (#'ra/get-room-kb-conns (:room-nogrant w) (:owner w)))
              "not even for the creator: the sandbox is the room's, not the creator's")
          (let [sc (sandbox-of w :room-nogrant :member)]
            (is (thrown? Exception (sci/eval-string* sc "(kb/ensure-page! \"Archive\" \"written from Scratch\")")))
            (is (thrown? Exception (sci/eval-string* sc "(kb/conn \"Archive\")")))
            (is (not (page-on-trunk? (:kb-private-scope w) "written from Scratch")))))
        (testing "a :read grant reads, and hands out no write conn"
          (grant! conn w :room :kb-private-sys :read)
          (let [kbc (#'ra/get-room-kb-conns (:room w) (:member w))
                sc (sandbox-of w :room :member)]
            (is (= {"Handbook" true "Archive" false} (into {} (map (juxt :name :writable?)) kbc)))
            (is (some? (sci/eval-string* sc "(kb/db \"Archive\")")) "reading it works")
            (is (not-authorized? #(sci/eval-string* sc "(kb/conn \"Archive\")")))
            (is (not-authorized? #(sci/eval-string* sc "(kb/ensure-page! \"Archive\" \"via read grant\")")))
            (is (not (page-on-trunk? (:kb-private-scope w) "via read grant")))))
        (testing "a handle is authorized on use: removing the member revokes it mid-session"
          (let [sc (sandbox-of w :room :member)]
            (is (some? (sci/eval-string* sc "(kb/conn \"Handbook\")")))
            (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
            (is (not-authorized? #(sci/eval-string* sc "(kb/conn \"Handbook\")")))
            (is (not-authorized? #(sci/eval-string* sc "(kb/ensure-page! \"Handbook\" \"after leaving\")")))
            (is (not (page-on-trunk? (:kb-granted-scope w) "after leaving")))))))))

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

(deftest eval-in-room-is-the-owners-and-records-the-caller
  (world/with-world [_ w]
    (let [conn (room-store-conn w)
          sc (sci/init {})
          ;; `:user-uuid-str` is what the client used to send as the author;
          ;; the RPC no longer has such an argument, and a client still
          ;; sending it changes nothing
          call! (fn [kind code user-id]
                  (world/inbound! (world/principal w kind) "eval-in-room!"
                                  {:room-uuid-str (str (:room w)) :code code
                                   :user-uuid-str (str user-id)}))]
      (with-redefs [ra/room-context-state (fn [_] {:sci-ctx sc})
                    room-dbs/connect-room-database (fn [_] conn)]
        (testing "legitimate: the owner's eval is recorded as the owner's"
          (is (= {:success true :result "=> 2"} (:result (call! :owner "(+ 1 1)" (:owner w)))))
          (is (= (:owner w) (get (eval-authors conn) "(+ 1 1)"))))
        (testing "§2.7 (closed): the author is the principal, not an id the client names"
          (is (= {:success true :result "=> 3"} (:result (call! :owner "(+ 1 2)" (:member w)))))
          (is (= (:owner w) (get (eval-authors conn) "(+ 1 2)"))))
        (testing "§2.7 (closed): evaluating is the owner's — a member is refused and records nothing"
          (is (world/refused? (call! :member "(str :forged)" (:owner w))))
          (is (nil? (get (eval-authors conn) "(str :forged)"))))
        (testing "a non-member is refused at the gate and records nothing"
          (is (world/refused? (call! :non-member "(+ 2 2)" (:non-member w))))
          (is (nil? (get (eval-authors conn) "(+ 2 2)"))))
        (testing "the UI asks the same question to decide whether to show the eval controls"
          (let [ask (fn [kind] (:result (world/inbound! (world/principal w kind) "can-eval-in-room!"
                                                        {:room-uuid-str (str (:room w))})))]
            (is (true? (ask :owner)))
            (is (false? (ask :member)))
            (is (false? (ask :non-member)))
            (is (false? (ask :admin)))))))))

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
  ;; Transport handlers share the served registry. Their exact symbols are
  ;; explicitly denied before application policy lookup, for every principal.
  (world/with-world [_ w]
    (let [room-store (schemaless-conn)]
      (dh-handlers/register-global-handlers! (atom {:id (random-uuid)}))
      (dh-handlers/register-connection-for-store! (:room-scope w) :db room-store nil)
      (try
        (testing "the attack: a raw transact of a message authored by the owner"
          (let [resp (world/inbound! (world/principal w :member) 'datahike.kabel/dispatch
                                     {:store-id (:room-scope w)
                                      :arg-map {:op 'transact! :args [{:tx-data [(forged-message w)]}]}})]
            (is (world/refused? resp) "the reachable handler is denied before execution")
            (is (empty? (messages-by room-store (:owner w))) "no forged message landed")))
        (testing "§2.7 (closed): the gate refuses it for every principal, the owner included"
          (doseq [kind world/principal-kinds
                  f '[datahike.kabel/dispatch datahike.kabel/create-database
                      datahike.kabel/delete-database]]
            (is (false? (boolean (access/authorize-remote
                                  (world/principal w kind) f
                                  {:store-id (:room-scope w)
                                   :arg-map {:op 'transact! :args [{:tx-data [(forged-message w)]}]}})))
                (str kind " " f))))
        (testing "what the handler WOULD do if served: it checks nothing itself"
          ;; called directly, the way kabel.remote/serve would call it after a
          ;; permissive gate — this is why there must be no policy row
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
                    is.simm.ops.chat/ensure-room-chatroom! (fn [& _])
                    is.simm.ops.chat/ensure-party-projection! (fn [& _])
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
