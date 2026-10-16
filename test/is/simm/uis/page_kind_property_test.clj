(ns is.simm.uis.page-kind-property-test
  "A page's Kind (`:S.Page/kind`, a KEYWORD attribute) can be saved from the
   property box, and only to a kind a person may set.

   The box edited every property as text, so it sent \"wiki/atom\" and the
   transaction was refused — the Kind could not be saved at all. The editor
   now produces a keyword (`page-kind/parse-keyword`, a select for the kind),
   and `-save-property-handler` checks the value against the attribute's type
   and the kind rules before it transacts."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :refer [<!!]]
            [datahike.api :as d]
            [is.simm.model.page-kind :as page-kind]
            [is.simm.model.schema :as schema]
            [is.simm.model.seed :as seed]
            [is.simm.uis.web.desktop.block-remote :as br]))

(def ^:private page-uuid #uuid "dddddddd-0000-0000-0000-0000000000b1")

(defn- fresh-store []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn schema/full-schema)
      (seed/ensure-seed-data! conn)
      ;; the property as katzen projects it: `kind : Entity → Keyword`, with
      ;; the UI hint :text that the old editor followed
      (when-not (d/q '[:find ?m . :where [?m :entity/name "S/Page/kind"]] @conn)
        (d/transact conn [{:entity/uuid (random-uuid)
                           :entity/name "S/Page/kind"
                           :morphism/src [:entity/name "S/Page"]
                           :morphism/dst [:entity/name "S/String"]
                           :morphism/attr-type :Keyword
                           :morphism/property-type :text}]))
      (d/transact conn [{:entity/uuid page-uuid
                         :instance/of-role [:entity/name "S/Page"]
                         :S.Page/title "Kinds"}])
      conn)))

(defn- save! [conn value]
  (with-redefs [br/resolve-conn (fn [& _] conn)]
    (let [r (<!! (br/-save-property-handler {:page-uuid page-uuid
                                             :morphism-name "S/Page/kind"
                                             :value value}))]
      (if (instance? Throwable r) (throw r) r))))

(defn- kind [conn]
  (d/q '[:find ?k . :in $ ?p :where [?e :entity/uuid ?p] [?e :S.Page/kind ?k]]
       @conn page-uuid))

(deftest the-kind-round-trips-as-a-keyword
  (let [conn (fresh-store)]
    (is (= :db.type/keyword (get-in (d/schema @conn) [:S.Page/kind :db/valueType])))
    (testing "what the editor sends for a typed or selected kind is a keyword, and it lands"
      (is (= :wiki/atom (page-kind/parse-keyword "wiki/atom")))
      (is (= {:success true} (save! conn (page-kind/parse-keyword ":wiki/atom"))))
      (is (= :wiki/atom (kind conn))))
    (testing "changed to another kind a person may set"
      (is (= {:success true} (save! conn :wiki/overview)))
      (is (= :wiki/overview (kind conn))))
    (testing "cleared"
      (is (= {:success true} (save! conn nil)))
      (is (nil? (kind conn))))))

(deftest a-string-is-refused-with-the-reason
  (let [conn (fresh-store)
        r (save! conn "wiki/atom")]
    (is (false? (:success r)))
    (is (= :wrong-value-type (:error r)))
    (is (re-find #"keyword" (:message r)) "says what the attribute holds")
    (is (nil? (kind conn)) "nothing landed")))

(deftest system-kinds-are-the-systems
  (testing "a person cannot give a page a system kind"
    (let [conn (fresh-store)]
      (doseq [k page-kind/system-kinds]
        (let [r (save! conn k)]
          (is (= :system-page-kind (:error r)) (str k))
          (is (nil? (kind conn)))))))
  (testing "nor a kind outside the list"
    (let [conn (fresh-store)]
      (is (= :system-page-kind (:error (save! conn :something/else))))))
  (testing "nor change or clear a kind the system set"
    (let [conn (fresh-store)]
      (d/transact conn [{:entity/uuid page-uuid :S.Page/kind :bootstrap}])
      (is (= :system-page-kind (:error (save! conn :wiki/atom))))
      (is (= :system-page-kind (:error (save! conn nil))))
      (is (= :bootstrap (kind conn))))))

(deftest typed-keywords
  (is (nil? (page-kind/parse-keyword "  ")))
  (is (= :atom (page-kind/parse-keyword "atom")))
  (is (= :wiki/index (page-kind/parse-keyword " :wiki/index ")))
  (is (page-kind/invalid? (page-kind/parse-keyword "two words")))
  (is (page-kind/invalid? (page-kind/parse-keyword "a/b/c"))))
