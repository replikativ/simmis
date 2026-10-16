(ns is.simm.authority.widget-writes-test
  "A wiki widget's writes, executed through the network-inbound gate
   (`world/inbound!` — `authorize-remote`, the principal binding, the registered
   handler), each attack paired with the legitimate use of the same path.

   Widget code runs in the VIEWER's browser and used to `d/transact!` the KB
   replica: a write that never left the browser (KabelWriter over an unserved
   `kabel.remote`), and that would have skipped `can?` had it arrived. It is
   now two RPCs (`block_remote.clj`, \"Widget writes\"):

     widget-transact      data — `:write` on the KB, never a room store
     widget-install-attr  schema — `:merge` on the KB (its owner); the value
                          type is derived from the codomain object, never sent

   THE WORLD (`authority.world`): the owner owns Handbook (granted :read-write
   to the team room) and Archive (granted nowhere); the member is in the team
   room. Each KB here is a real in-memory store with simmis's schema and seed."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.authority.world :as world]
            [is.simm.model.schema :as schema]
            [is.simm.model.seed :as seed]
            [is.simm.uis.web.desktop.block-remote :as br]))

(defn- fresh-kb []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             ;; real KB stores keep history (store/install!), which is also what
             ;; stores a transaction's own datoms (`:tx/author`)
             :schema-flexibility :write :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn schema/full-schema)
      (seed/ensure-seed-data! conn)
      ;; an attribute the widget already uses, as an earlier install left it
      (d/transact conn [{:db/ident :counter/value :db/valueType :db.type/long
                         :db/cardinality :db.cardinality/one}])
      conn)))

(def ^:private counter #uuid "cccccccc-0000-4000-8000-000000000001")

(defn- with-kb-stores
  "Run `(f conns)` with the world's two KB scopes resolving to fresh stores, as
   `block_remote`'s `resolve-conn` resolves them."
  [w f]
  (let [conns {(:kb-granted-scope w) (fresh-kb)
               (:kb-private-scope w) (fresh-kb)}]
    (doseq [c (vals conns)] (d/transact c [{:entity/uuid counter :counter/value 0}]))
    (with-redefs [br/resolve-conn (fn [scope & _] (get conns scope))]
      (f conns))))

(defn- grant-read! [sys w]
  (d/transact sys [{:grant/id (random-uuid) :grant/room [:room/id (:room w)]
                    :grant/system [:system/id (:kb-private-sys w)]
                    :grant/permission :read :grant/created (java.util.Date.)}]))

(defn- counter-value [conn] (:counter/value (d/pull @conn [:counter/value] [:entity/uuid counter])))
(defn- ident? [conn ident] (some? (d/q '[:find ?e . :in $ ?i :where [?e :db/ident ?i]] @conn ident)))
(defn- value-type [conn ident]
  (d/q '[:find ?vt . :in $ ?i :where [?a :db/ident ?i] [?a :db/valueType ?vt]] @conn ident))

(defn- transact! [w kind scope tx-data]
  (world/inbound! (world/principal w kind) "widget-transact" {:db-scope scope :tx-data tx-data}))

(defn- install! [w kind scope ident codomain]
  (world/inbound! (world/principal w kind) "widget-install-attr"
                  {:db-scope scope :ident ident :codomain codomain :cardinality :one}))

(def ^:private inc-counter [{:entity/uuid counter :counter/value 1}])

;; =============================================================================
;; Data: :write on the KB
;; =============================================================================

(deftest a-read-grant-member-cannot-transact
  (world/with-world [sys w]
    (grant-read! sys w)
    (with-kb-stores w
      (fn [conns]
        (let [archive (get conns (:kb-private-scope w))
              before (:max-tx @archive)]
          (testing "attack: the member reaches Archive through a :read grant only"
            (is (world/refused? (transact! w :member (:kb-private-scope w) inc-counter)))
            (is (= before (:max-tx @archive)) "nothing landed")
            (is (= 0 (counter-value archive))))
          (testing "nor may someone with no relation to the KB at all"
            (is (world/refused? (transact! w :non-member (:kb-private-scope w) inc-counter)))
            (is (world/refused? (transact! w :anon (:kb-private-scope w) inc-counter)))
            (is (= before (:max-tx @archive))))
          (testing "legitimate: the KB's owner writes it, and gets the head"
            (let [r (transact! w :owner (:kb-private-scope w) inc-counter)]
              (is (nil? (:error r)) (str (:error r)))
              (is (= 1 (counter-value archive)))
              (is (= (:max-tx @archive) (get-in r [:result :max-tx])))))
          (testing "the write says who made it"
            (is (= (str (:owner w))
                   (d/q '[:find ?a . :in $ ?t :where [?t :tx/author ?a]]
                        @archive (:max-tx @archive))))))))))

(deftest a-write-grant-member-transacts-data-and-nothing-else
  (world/with-world [_ w]
    (with-kb-stores w
      (fn [conns]
        (let [handbook (get conns (:kb-granted-scope w))
              scope (:kb-granted-scope w)]
          (testing "legitimate: the :read-write grant writes data"
            (let [r (transact! w :member scope inc-counter)]
              (is (nil? (:error r)) (str (:error r)))
              (is (= 1 (counter-value handbook)))))
          (let [before (:max-tx @handbook)]
            (doseq [[label tx] {"an attribute declaration as data"
                                [{:db/ident :todo/sneaky :db/valueType :db.type/string
                                  :db/cardinality :db.cardinality/one}]
                                "an attribute declaration hidden on a data entity"
                                [{:entity/uuid (random-uuid) :db/ident :todo/sneaky2}]
                                "changing an existing attribute's schema"
                                [[:db/add :counter/value :db/doc "mine now"]]
                                "retracting an attribute"
                                [[:db/retractEntity [:db/ident :counter/value]]]
                                "forging the transaction's author"
                                [[:db/add "datomic.tx" :tx/author "someone else"]]
                                "a transaction function"
                                [[:db.fn/call 'clojure.core/identity]]}]
              (testing (str "attack: " label)
                (is (:error (transact! w :member scope tx)))
                (is (= before (:max-tx @handbook)) "nothing landed")))
            (is (not (ident? handbook :todo/sneaky)))
            (is (not (ident? handbook :todo/sneaky2)))
            (is (= :db.type/long (value-type handbook :counter/value)))))))))

;; =============================================================================
;; Schema: :merge on the KB, the type derived from the codomain
;; =============================================================================

(deftest a-write-grant-member-cannot-install-schema
  (world/with-world [_ w]
    (with-kb-stores w
      (fn [conns]
        (let [handbook (get conns (:kb-granted-scope w))
              scope (:kb-granted-scope w)
              before (:max-tx @handbook)]
          (testing "attack: the :read-write grantee declares an attribute"
            (is (world/refused? (install! w :member scope :todo/text "S/String")))
            (is (not (ident? handbook :todo/text)))
            (is (= before (:max-tx @handbook)) "nothing landed"))
          (testing "nor does the :admin role, which manages authority, not content"
            (is (world/refused? (install! w :admin scope :todo/text "S/String")))
            (is (not (ident? handbook :todo/text))))
          (testing "legitimate: the owner declares it"
            (let [r (install! w :owner scope :todo/text "S/String")]
              (is (nil? (:error r)) (str (:error r)))
              (is (= :installed (get-in r [:result :status])))
              (is (= :db.type/string (value-type handbook :todo/text)))))
          (testing "and the member may then write data under it"
            (let [r (transact! w :member scope [{:entity/uuid (random-uuid) :todo/text "milk"}])]
              (is (nil? (:error r)) (str (:error r)))
              (is (= ["milk"] (d/q '[:find [?t ...] :where [_ :todo/text ?t]] @handbook))))))))))

(deftest the-owner-names-a-codomain-and-the-server-derives-the-type
  (world/with-world [_ w]
    (with-kb-stores w
      (fn [conns]
        (let [handbook (get conns (:kb-granted-scope w))
              scope (:kb-granted-scope w)]
          (testing "the value type comes from the codomain object (schema/codomain->db-type)"
            (doseq [[ident object vt] [[:todo/count "S/Number" :db.type/long]
                                       [:todo/weight "S/Float" :db.type/double]
                                       [:todo/done? "S/Boolean" :db.type/boolean]
                                       [:todo/due "S/Date" :db.type/instant]
                                       [:todo/page "S/Page" :db.type/ref]]]
              (let [r (install! w :owner scope ident object)]
                (is (nil? (:error r)) (str ident " " (:error r)))
                (is (= vt (value-type handbook ident)) (str ident)))))
          (testing "and a morphism describes each, under the name the attribute derives from"
            (is (= "S/Number"
                   (d/q '[:find ?dn . :where [?m :entity/name "todo/count"]
                          [?m :morphism/dst ?d] [?d :entity/name ?dn]]
                        @handbook))))
          (testing "a client-chosen :db/valueType is refused, from the owner too"
            (let [before (:max-tx @handbook)]
              (is (:error (install! w :owner scope :todo/raw :db.type/string)))
              (is (:error (install! w :owner scope :todo/raw2 "S/NoSuchObject")))
              (is (:error (install! w :owner scope :db/sneaky "S/String")))
              (is (:error (install! w :owner scope :morphism/sneaky "S/String")))
              (is (= before (:max-tx @handbook)) "nothing landed")))
          (testing "installing an existing attribute is a no-op, not a type change"
            (let [before (:max-tx @handbook)
                  r (install! w :owner scope :todo/count "S/String")]
              (is (= :exists (get-in r [:result :status])))
              (is (= :db.type/long (value-type handbook :todo/count)))
              (is (= before (:max-tx @handbook))))))))))

;; =============================================================================
;; Never a room store
;; =============================================================================

(deftest a-widget-write-never-reaches-a-room-store
  (world/with-world [_ w]
    (with-kb-stores w
      (fn [_]
        ;; The room's members may write the room (chat), but a RAW transaction
        ;; into its store would forge messages. `{:kb-store …}` answers only for
        ;; a KB, so the gate refuses before any handler resolves a conn.
        (doseq [kind [:owner :member]]
          (testing (name kind)
            (is (world/refused? (transact! w kind (:room-scope w) inc-counter)))
            (is (world/refused? (install! w kind (:room-scope w) :todo/text "S/String")))))))))
