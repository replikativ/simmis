(ns is.simm.documents.canonical-view-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-yaml.core :as yaml]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]
            [is.simm.model.crud :as crud]
            [is.simm.model.seed :as seed]
            [kontor.governance :as governance]))

(def options {:roots [""] :schema-revision :canonical-fixture-v1 :entity-profile space/profile})
(def page-a #uuid "aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa")
(def page-b #uuid "bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb")
(def block-id #uuid "cccccccc-cccc-4ccc-8ccc-cccccccccccc")

(defn- source [raw body]
  (str "---\n" (yaml/generate-string raw) "---\n" body))

(defn- write! [conn path text]
  (repo/write! conn path (markdown/source-bytes text)))

(deftest canonical-scalars-use-the-existing-query-model-without-mutating-the-base
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (space/provision! conn)
        (let [company (crud/create-type! conn {:name "ProjectionCompany"})
              research (crud/create-type! conn {:name "ProjectionResearch"})
              revenue (crud/create-morphism! conn {:name "S/ProjectionCompany/revenue"
                                                   :src "S/ProjectionCompany" :dst "S/Number" :optional? false})
              active (crud/create-morphism! conn {:name "S/ProjectionResearch/active"
                                                  :src "S/ProjectionResearch" :dst "S/Boolean"})
              person-property (crud/create-morphism! conn {:name "S/ProjectionCompany/person"
                                                           :src "S/ProjectionCompany" :dst "S/Person"})
              slug-property (crud/create-morphism! conn {:name "S/ProjectionCompany/slug"
                                                         :src "S/ProjectionCompany" :dst "S/String" :unique :db.unique/identity})
              number-property (crud/create-morphism! conn {:name "S/ProjectionCompany/tax-number"
                                                           :src "S/ProjectionCompany" :dst "S/Number" :unique :db.unique/value})
              uuid-property (crud/create-morphism! conn {:name "S/ProjectionCompany/external-id"
                                                         :src "S/ProjectionCompany" :dst "S/UUID"})
              person-type (:entity/uuid (d/pull @conn [:entity/uuid] [:entity/name "S/Person"]))
              person-id (random-uuid)
              raw {"simmis-id" (str page-a)
                   "simmis" {"version" 1 "types" [(str company) (str research)]
                             "properties" {"turnover" {"id" (str revenue) "type" (str company)}}
                             "blocks" {(str block-id)
                                       {"types" [(str research)]
                                        "properties" {"active" {"id" (str active) "type" (str research)}}
                                        "values" {"active" false}}}}
                   "turnover" 0 "tags" ["business"] "unknown" {"keep" [1 2]}}
              body (str "# Company\n\n<!-- simmis:block " block-id " -->\n\nResearch\n\n<!-- /simmis:block -->\n")
              original (source raw body)]
          (d/transact conn [{:entity/uuid person-id :instance/of-role [[:entity/uuid person-type]]}])
          (write! conn "companies/a.md" original)
          (write! conn "research/a.md" (source (-> raw (assoc "simmis-id" (str page-b) "turnover" 10)
                                                     (update "simmis" dissoc "blocks")) "# Another company\n"))
          (let [base @conn tx (:max-tx base)
                prepared (view/prepare base options)]
            (is (= :ready (:status prepared)) (pr-str (:diagnostics prepared)))
            (when-let [projected (:db prepared)]
              (testing "same UUID/type/property vocabulary, path titles, typed semantic blocks"
                (is (= #{[page-a 0] [page-b 10]}
                       (d/q '[:find ?id ?revenue :where [?e :entity/uuid ?id]
                              [?e :S.ProjectionCompany/revenue ?revenue]] projected)))
                (is (= #{["companies/a.md"] ["research/a.md"]}
                       (d/q '[:find ?title :where [?e :S.Page/title ?title]
                              [?e :document/path ?title]] projected)))
                (is (= false (:S.ProjectionResearch/active
                              (d/pull projected [:S.ProjectionResearch/active] [:entity/uuid block-id]))))
                (is (= #{(:entity/uuid seed/object-Page) company research}
                       (set (map :entity/uuid (:instance/of-role
                                              (d/pull projected [{:instance/of-role [:entity/uuid]}]
                                                      [:entity/uuid page-a]))))))
                (is (= #{(:entity/uuid seed/object-Block) research}
                       (set (map :entity/uuid (:instance/of-role
                                              (d/pull projected [{:instance/of-role [:entity/uuid]}]
                                                      [:entity/uuid block-id]))))))
                (is (= 2 (count (:typed-scopes prepared))) "identical page scopes compile only once")
                (is (empty? (d/q '[:find ?id :where [?e :entity/uuid ?id] [?e :document.node/kind]] projected)))
                (is (empty? (d/q '[:find ?id :where [?e :entity/uuid ?id]
                                  [?e :document.block/identity-status :provisional]] projected)))
                (is (= original (get-in prepared [:documents 0 :source]))))
              (testing "immutable only; a derived value cannot be reused as canonical input"
                (is (= tx (:max-tx @conn)))
                (is (empty? (d/datoms base :avet :entity/uuid page-a)))
                (is (view/current? base options prepared))
                (is (view/current? projected options prepared))
                (is (= :derived-db-is-not-canonical-source
                       (try (view/prepare projected options)
                            (catch clojure.lang.ExceptionInfo error (:reason (ex-data error))))))))
            (testing "schema changes invalidate freshness even with unchanged bytes"
              (let [changed (d/db-with base [{:entity/uuid revenue :morphism/optional? true}])]
                (is (not (view/current? changed options prepared))))
              (let [removed (d/db-with base [[:db/retractEntity [:entity/uuid revenue]]])]
                (is (not (view/current? removed options prepared)))))
            (testing "replacement preview shares validation without publishing or pretending bytes are stored"
              (let [selected (get-in prepared [:selection :files "companies/a.md"])
                    candidate (source (assoc raw "turnover" 42) body)
                    request {:path "companies/a.md" :expected selected :source candidate}
                    before (:max-tx @conn)
                    result (view/validate-replacement base options request)]
                (is (= :replacement-preview (:kind result)))
                (is (= :valid (:status result)) (pr-str (:diagnostics result)))
                (is (= (markdown/revision candidate) (:revision result)))
                (is (= selected (:expected result)))
                (is (every? #(not (contains? result %)) [:db :selection :documents :document/source-content]))
                (is (not (view/current? base options result)))
                (is (not (view/current? base options (assoc result :selection (:selection prepared)))))
                (is (nil? (:db/id (d/entity base [:geschichte.content/id (blob/blob-id (markdown/source-bytes candidate))]))))
                (is (= before (:max-tx @conn)))
                (is (= selected (get (repo/worktree conn) "companies/a.md")))
                (let [reads (atom []) read-content content/read-by-id]
                  (with-redefs [content/read-by-id (fn [conn id & args]
                                                    (swap! reads conj id)
                                                    (apply read-content conn id args))]
                    (is (= :valid (:status (view/validate-replacement base options request)))))
                  (is (= #{(get-in prepared [:selection :files "research/a.md" :content])} (set @reads))))
                (is (= :valid (:status (view/validate-replacement base options (assoc request :source original)))))
                (is (= :invalid (:status (view/validate-replacement base options
                                           (assoc request :source (source (dissoc raw "turnover") body))))))
                (is (= :invalid (:status (view/validate-replacement base options
                                           (assoc request :source (source (assoc raw "simmis-id" (str page-b)) body))))))
                (let [changed (d/db-with base [{:entity/uuid revenue :morphism/validation "never execute"}])]
                  (is (= :invalid (:status (view/validate-replacement changed options request)))))
                (doseq [invalid [nil false {} (assoc request :extra true)
                                  (assoc request :path "absent.md")
                                  (assoc request :expected (assoc selected :size (inc (:size selected))))]]
                  (is (thrown? clojure.lang.ExceptionInfo (view/validate-replacement base options invalid))))
                (is (thrown? clojure.lang.ExceptionInfo
                             (view/validate-replacement base (assoc options :roots ["research"]) request)))
                (is (thrown? clojure.lang.ExceptionInfo
                             (view/validate-replacement base (dissoc options :entity-profile) request)))
                (let [size (reduce + 0 (map :size (vals (get-in prepared [:selection :files]))))]
                  (with-redefs-fn {(ns-resolve 'is.simm.documents.view 'max-total-bytes) (inc size)}
                    #(is (thrown? clojure.lang.ExceptionInfo
                                  (view/validate-replacement base options (assoc request :source (str original "\nExtra candidate bytes\n")))))))))
            (testing "generated path satisfies explicit required title, never arbitrary YAML"
              (let [required (d/db-with base [{:entity/uuid (:entity/uuid seed/morphism-Page-title)
                                              :morphism/optional? false}])]
                (is (= :ready (:status (view/prepare required options)))))
              (let [unsupported (d/db-with base [{:entity/uuid (:entity/uuid seed/morphism-Page-title)
                                                 :morphism/validation "never evaluate"}])]
                (is (= :invalid (:status (view/prepare unsupported options)))))
              (let [moved (d/db-with base [{:entity/uuid (:entity/uuid seed/morphism-Page-title)
                                           :morphism/src [:entity/uuid company]}])]
                (is (= :invalid (:status (view/prepare moved options)))))
              (let [alias-raw (assoc-in raw ["simmis" "properties" "title"]
                                        {"id" (str (:entity/uuid seed/morphism-Page-title))
                                         "type" (str (:entity/uuid seed/object-Page))})]
                (write! conn "companies/a.md" (source alias-raw body))
                (let [result (view/prepare @conn options)]
                  (is (= :invalid (:status result)))
                  (is (nil? (:db result))))
                (write! conn "companies/a.md" original)))
            (testing "existing canonical identities and domain facts are not upserted or deleted"
              (let [overlap (d/db-with base [{:entity/uuid page-a :entity/name "Authored domain row"}])
                    result (view/prepare overlap options)]
                (is (= :invalid (:status result)))
                (is (nil? (:db result)))
                (is (not (view/current? overlap options prepared)))
                (is (= "Authored domain row" (:entity/name (d/pull overlap [:entity/name] [:entity/uuid page-a])))))
              (let [collision (d/db-with base [{:entity/uuid (random-uuid) :S.Page/title "companies/a.md"}])]
                (is (= :invalid (:status (view/prepare collision options))))
                (is (not (view/current? collision options prepared)))))
            (testing "partial projection rows also refuse, while unrelated authored facts do not"
              (let [partial (d/db-with base [{:document/path "orphan.md"}])]
                (is (= :derived-db-is-not-canonical-source
                       (try (view/prepare partial options)
                            (catch clojure.lang.ExceptionInfo error (:reason (ex-data error))))))
                (is (not (view/current? partial options prepared))))
              (let [unrelated (d/db-with base [{:entity/uuid (random-uuid) :entity/name "Unrelated domain row"}])]
                (is (view/current? unrelated options prepared))))
            (testing "a required property cannot disappear by deleting its alias"
              (write! conn "companies/a.md" (source (update-in raw ["simmis" "properties"] dissoc "turnover") body))
              (let [result (view/prepare @conn options)]
                (is (= :invalid (:status result)))
                (is (nil? (:db result))))
              (write! conn "companies/a.md" original))
            (testing "source edits invalidate, while old immutable values stay readable"
              (write! conn "companies/a.md" (source (assoc raw "turnover" 7) body))
              (is (not (view/current? @conn options prepared)))
              (is (= 7 (:S.ProjectionCompany/revenue
                         (d/pull (:db (view/prepare @conn options)) [:S.ProjectionCompany/revenue]
                                 [:entity/uuid page-a])))))
            (testing "rename retains explicit identity; deletion does not mutate an old view"
              (write! conn "renamed/a.md" original)
              (repo/remove! conn "companies/a.md")
              (let [renamed (view/prepare @conn options)]
                (is (= :ready (:status renamed)))
                (is (= "renamed/a.md" (:S.Page/title (d/pull (:db renamed) [:S.Page/title] [:entity/uuid page-a]))))
                (is (empty? (d/datoms (:db renamed) :avet :S.Page/title "companies/a.md"))))
              (repo/remove! conn "renamed/a.md")
              (let [deleted (view/prepare @conn options)]
                (is (= :ready (:status deleted)))
                (is (empty? (d/datoms (:db deleted) :avet :entity/uuid page-a))))
              (is (= 0 (:S.ProjectionCompany/revenue
                         (d/pull (:db prepared) [:S.ProjectionCompany/revenue] [:entity/uuid page-a])))))
            (testing "real Person targets, UUID scalars and unique fields share final preflight"
              (let [person-raw (-> raw
                                   (update-in ["simmis" "properties"] merge
                                              {"person" {"id" (str person-property) "type" (str company)}
                                               "slug" {"id" (str slug-property) "type" (str company)}
                                               "tax-number" {"id" (str number-property) "type" (str company)}
                                               "external-id" {"id" (str uuid-property) "type" (str company)}})
                                   (assoc "person" {"id" (str person-id) "path" "research/a.md"}
                                          "slug" "unique-company" "tax-number" 0 "external-id" (str page-b)))]
                (write! conn "companies/a.md" (source person-raw body))
                (let [person-base @conn
                      result (view/prepare person-base options)
                      projected (:db result)]
                  (is (= :ready (:status result)) (pr-str (:diagnostics result)))
                  (when projected
                    (is (= person-id (get-in (d/pull projected [{:S.ProjectionCompany/person [:entity/uuid]}]
                                                    [:entity/uuid page-a])
                                             [:S.ProjectionCompany/person :entity/uuid])))
                    (is (= page-b (:S.ProjectionCompany/external-id
                                  (d/pull projected [:S.ProjectionCompany/external-id] [:entity/uuid page-a]))))
                    (is (view/current? projected options result)))
                  (let [removed (d/db-with person-base [[:db/retract [:entity/uuid person-id] :instance/of-role [:entity/uuid person-type]]])
                        target-type-changed (d/db-with person-base [[:db/retract [:entity/uuid person-type]
                                                                   :instance/of-role [:entity/name "S/EntityType"]]])]
                    (is (not (view/current? removed options result)))
                    (is (not (view/current? target-type-changed options result)))
                    (is (= :invalid (:status (view/prepare removed options)))))
                  (doseq [[attr value] [[:S.ProjectionCompany/slug "unique-company"] [:S.ProjectionCompany/tax-number 0]]]
                    (let [collision (d/db-with person-base [{:entity/uuid (random-uuid) attr value}])]
                      (is (not (view/current? collision options result)))
                      (is (= :invalid (:status (view/prepare collision options))))))
                  (let [unrelated (d/db-with person-base [{:entity/uuid (random-uuid) :entity/name "Unrelated after typed view"}])]
                    (is (view/current? unrelated options result))))
                (write! conn "research/a.md" (source (-> person-raw (assoc "simmis-id" (str page-b))
                                                         (update "simmis" dissoc "blocks")) "# Duplicate unique fields\n"))
                (let [invalid (view/current-view conn options)]
                  (is (= :invalid (:status invalid)) "stable constraint errors are not perpetual pending")
                  (is (nil? (:db invalid))))
                (let [db @conn
                      preview (view/validate-replacement
                               db options {:path "companies/a.md"
                                           :expected (get (repo/worktree conn) "companies/a.md")
                                           :source (source (assoc person-raw "turnover" 99) body)})]
                  (is (= :invalid (:status preview)) "the entire selection, not just the edited file, is checked")
                  (is (some #(= :invalid-selection-constraints (:kind %)) (:diagnostics preview)))
                  (is (nil? (:db preview))))
                (write! conn "research/a.md" (source (-> raw (assoc "simmis-id" (str page-b) "turnover" 10)
                                                          (update "simmis" dissoc "blocks")) "# Another company\n"))))
            (testing "relations use explicit UUIDs, not path hints or forward lookup refs"
              (let [founder (crud/create-morphism! conn {:name "S/ProjectionCompany/founder"
                                                        :src "S/ProjectionCompany" :dst "S/ProjectionResearch"})
                    relation (-> raw
                                 (assoc-in ["simmis" "properties" "founder"]
                                           {"id" (str founder) "type" (str company)})
                                 (assoc "founder" {"id" (str page-b) "path" "research/a.md"}))]
                (write! conn "companies/a.md" (source relation body))
                (let [result (view/prepare @conn options)]
                  (is (= :ready (:status result)) (pr-str (:diagnostics result)))
                  (is (= page-b (get-in (d/pull (:db result) [{:S.ProjectionCompany/founder [:entity/uuid]}]
                                               [:entity/uuid page-a])
                                       [:S.ProjectionCompany/founder :entity/uuid])))
                  (is (view/current? (:db result) options result)))))))
        (finally (governance/ungovern! conn) (d/release conn) (d/delete-database cfg))))))
