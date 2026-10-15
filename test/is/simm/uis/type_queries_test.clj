(ns is.simm.uis.type-queries-test
  "The client's type-system queries (`datahike-query/page-types-q` & co.) run
   against a real installed store.

   They pull in `:find` and bind collections over joined variables — the two
   forms datahike's CLJS planner answered nil for until datahike #806/#807.
   Until then the client asked for `[?a ?b]` tuples and issued one `d/pull` per
   row. Each test here also runs that old shape and requires the same answer,
   so the switch cannot have changed what the type panels see."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.model.store :as store]
            [is.simm.uis.web.desktop.datahike-query :as dq]))

(def ^:private page-uuid #uuid "00000000-0000-0000-0000-0000000a11ce")

(defn- installed-db []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (store/install! conn)
      (d/transact conn [{:entity/uuid page-uuid
                         :entity/name "Alice"
                         :instance/of-role [[:entity/name "S/Page"]
                                            [:entity/name "S/Person"]]
                         :S.Page/title "Alice"}])
      @conn)))

(defn- per-row
  "The retired shape: tuple find, then one pull per second element."
  [db tuple-q pattern & args]
  (->> (apply d/q tuple-q db args)
       (map second)
       (map #(d/pull db pattern %))
       set))

(def ^:private type-pattern [:entity/uuid :entity/name :object/primitive? :object/icon])

(deftest type-queries-against-an-installed-store
  (let [db (installed-db)]
    (testing "page-types-q: the roles a page instantiates"
      (let [rows (d/q dq/page-types-q db page-uuid)]
        (is (= #{"S/Page" "S/Person"} (set (map :entity/name rows))))
        (is (= (set rows)
               (per-row db '[:find ?page ?role :in $ ?u
                             :where [?page :entity/uuid ?u]
                             [?page :instance/of-role ?role]
                             [?role :object/of-category _]]
                        type-pattern page-uuid)))))

    (testing "all-types-q: every object of category S, primitives flagged"
      (let [rows (d/q dq/all-types-q db)
            by-name (into {} (map (juxt :entity/name identity)) rows)]
        (is (contains? by-name "S/Page"))
        (is (true? (:object/primitive? (by-name "S/String"))))
        (is (= (set rows)
               (per-row db '[:find ?cat ?type
                             :where [?cat :entity/name "S"]
                             [?type :object/of-category ?cat]]
                        type-pattern)))))

    (testing "type-properties-q: storage-attr and the nested dst pull survive"
      (let [rows (d/q dq/type-properties-q db "S/Page")
            created (first (filter #(= "S/Page/created-at" (:entity/name %)) rows))]
        (is (= :entity/created-at (:morphism/storage-attr created)))
        (is (= "S/Date" (get-in created [:morphism/dst :entity/name])))
        (is (= (set rows)
               (per-row db '[:find ?type ?morph :in $ ?n
                             :where [?type :entity/name ?n]
                             [?morph :morphism/src ?type]]
                        [:entity/uuid :entity/name :morphism/property-type
                         :morphism/storage-attr :morphism/cardinality
                         :morphism/optional?
                         {:morphism/dst [:entity/uuid :entity/name]}]
                        "S/Page")))))

    (let [instances-tuple-q '[:find ?type ?inst :in $ ?n
                              :where [?type :entity/name ?n]
                              [?inst :instance/of-role ?type]]]
      (testing "type-instances-q: list rows of a type's instances"
        (let [rows (d/q dq/type-instances-q db "S/Person")]
          (is (= [{:entity/uuid page-uuid :entity/name "Alice" :S.Page/title "Alice"}]
                 rows))
          (is (= (set rows)
                 (per-row db instances-tuple-q
                          [:entity/uuid :entity/name :S.Page/title] "S/Person")))))

      (testing "type-instance-entities-q: whole instances, values included"
        (let [rows (d/q dq/type-instance-entities-q db "S/Page")]
          (is (some #(= "Alice" (:S.Page/title %)) rows))
          (is (= (set rows)
                 (per-row db instances-tuple-q '[*] "S/Page"))))))

    (testing "nothing to find answers empty, not nil rows"
      (is (= [] (d/q dq/page-types-q db (random-uuid))))
      (is (= [] (d/q dq/type-properties-q db "S/Nope")))
      (is (= [] (d/q dq/type-instances-q db "S/Nope"))))))
