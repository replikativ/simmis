(ns is.simm.authority.agent-install-attr-test
  "An agent's `kb/install-attr!`, EXECUTED in its sandbox the way the agent
   boot path builds it (`add-kb-write-ns!` over `get-room-kb-conns`), each
   attack paired with the legitimate use of the same path.

   The rule is a widget's (`widget_writes_test`, `block-remote/widget-install-attr`):
   declaring an attribute is `:merge` on the KB — schema is append-only and
   global to the KB, so a `:read-write` grant writes data and does not fix new
   attributes — and the agent names the CODOMAIN object, the value type being
   derived from it (`attributes/declaration`). It used to take a
   `:db/valueType` from the agent's code, at `:write`.

   THE WORLD (`authority.world`): Handbook is granted `:read-write` to the team
   room, which the owner's agent is in; Archive is the owner's, granted `:merge`
   to the room in the tests that need it."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [sci.core :as sci]
            [is.simm.agents.room-agents :as ra]
            [is.simm.authority.world :as world]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.store :as store]
            [is.simm.runtimes.branching :as branching]
            [is.simm.runtimes.context :as ctx]))

(defn- fresh-kb!
  "A registered, branchable KB store under `scope` (raw-paths-test's)."
  [scope]
  (let [cfg {:store {:backend :memory :id scope} :keep-history? true
             :crypto-hash? true :commit-graph? true :schema-flexibility :write}]
    (d/create-database cfg)
    (doto (d/connect cfg) (store/install!) (branching/register-system! scope))))

(defn- sandbox-of [w kind]
  (let [sc (sci/init {})
        party (get w kind)]
    (#'ra/add-kb-write-ns! sc (#'ra/get-room-kb-conns (:room w) party)
                           #(#'ra/active-overlay (:room w) party) party)
    sc))

(defn- refused?
  "Did `thunk` throw with `type` (`:not-authorized`, `:validation-error`)
   somewhere along the cause chain? SCI wraps what sandbox code throws."
  [type thunk]
  (try (thunk) false
       (catch Exception e
         (boolean (some #(= type (:type (ex-data %)))
                        (take-while some? (iterate ex-cause e)))))))

(defn- value-type [scope ident]
  (d/q '[:find ?vt . :in $ ?i :where [?a :db/ident ?i] [?a :db/valueType ?vt]]
       @(branching/get-kb-conn scope) ident))

(defn- head [scope] (:max-tx @(branching/get-kb-conn scope)))

(defn- grant! [conn w sys-key permission]
  (d/transact conn [{:grant/id (random-uuid) :grant/room [:room/id (:room w)]
                     :grant/system [:system/id (get w sys-key)]
                     :grant/permission permission :grant/created (java.util.Date.)}]))

(defn- install [sc kb ident codomain]
  #(sci/eval-string* sc (str "(kb/install-attr! \"" kb "\" " ident " " codomain " :one)")))

(deftest a-read-write-grant-declares-no-schema
  (ctx/with-server-context
    (world/with-world [_ w]
      (fresh-kb! (:kb-granted-scope w))
      (with-redefs [kbs/connect-kb-database #(branching/get-kb-conn %)]
        (let [sc (sandbox-of w :team-agent)
              scope (:kb-granted-scope w)
              before (head scope)]
          (testing "attack: the room's agent declares on Handbook, granted :read-write"
            (is (refused? :not-authorized (install sc "Handbook" ":todo/text" "\"S/String\"")))
            (is (refused? :not-authorized (install sc "Handbook" ":todo/text" ":db.type/string"))
                "the old signature, a value type of the agent's choosing, neither")
            (is (nil? (value-type scope :todo/text)))
            (is (= before (head scope)) "nothing landed"))
          (testing "legitimate: the same grant writes data under an attribute that exists"
            (sci/eval-string* sc "(kb/ensure-page! \"Handbook\" \"Agent notes\")")
            (is (> (head scope) before))))))))

(deftest a-merge-grant-declares-by-codomain
  (ctx/with-server-context
    (world/with-world [conn w]
      (fresh-kb! (:kb-private-scope w))
      (grant! conn w :kb-private-sys :merge)
      (with-redefs [kbs/connect-kb-database #(branching/get-kb-conn %)]
        (let [sc (sandbox-of w :team-agent)
              scope (:kb-private-scope w)]
          (testing "legitimate: the type is derived from the codomain object"
            (doseq [[ident codomain vt] [[:todo/text "S/String" :db.type/string]
                                         [:todo/count "S/Number" :db.type/long]
                                         [:todo/weight "S/Float" :db.type/double]
                                         [:todo/done? "S/Boolean" :db.type/boolean]
                                         [:todo/due "S/Date" :db.type/instant]
                                         [:todo/page "S/Page" :db.type/ref]]]
              (is (= ident (sci/eval-string* sc (str "(kb/install-attr! \"Archive\" " ident
                                                     " \"" codomain "\")"))))
              (is (= vt (value-type scope ident)) (str ident))))
          (testing "the agent then writes data under it"
            (let [e (random-uuid)]
              (sci/eval-string* sc (str "(kb/ensure-page! \"Archive\" \"Todo\")"))
              (d/transact (branching/get-kb-conn scope) [{:entity/uuid e}])
              (is (= e (sci/eval-string* sc (str "(kb/set-property! \"Archive\" #uuid \"" e
                                                 "\" :todo/text \"write it\")"))))))
          (let [before (head scope)]
            (testing "attack: a value type of the agent's choosing is refused"
              (is (refused? :validation-error (install sc "Archive" ":todo/forced" ":db.type/string")))
              (is (refused? :validation-error (install sc "Archive" ":todo/forced" ":db.type/ref"))))
            (testing "attack: an object the store does not have, and the store's own namespaces"
              (is (refused? :validation-error (install sc "Archive" ":todo/forced" "\"S/Nonsense\"")))
              (is (refused? :validation-error (install sc "Archive" ":db/forced" "\"S/String\"")))
              (is (refused? :validation-error (install sc "Archive" ":block/forced" "\"S/String\"")))
              (is (refused? :validation-error (install sc "Archive" ":morphism/forced" "\"S/String\""))))
            (is (nil? (value-type scope :todo/forced)))
            (testing "re-declaring an attribute is a no-op: its type never changes"
              (is (= :todo/text (sci/eval-string* sc "(kb/install-attr! \"Archive\" :todo/text \"S/Number\")")))
              (is (= :db.type/string (value-type scope :todo/text))))
            (is (= before (head scope)) "none of it landed")))))))

(deftest a-read-grant-declares-nothing-either
  (ctx/with-server-context
    (world/with-world [conn w]
      (fresh-kb! (:kb-private-scope w))
      (grant! conn w :kb-private-sys :read)
      (with-redefs [kbs/connect-kb-database #(branching/get-kb-conn %)]
        (let [sc (sandbox-of w :team-agent)
              before (head (:kb-private-scope w))]
          (is (refused? :not-authorized (install sc "Archive" ":todo/text" "\"S/String\"")))
          (is (= before (head (:kb-private-scope w)))))))))
