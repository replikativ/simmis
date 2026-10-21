(ns is.simm.documents.canonical-view-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-yaml.core :as yaml]
            [datahike.api :as d]
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
            (testing "a relation declaration is inert until the separate relation gate exists"
              (let [founder (crud/create-morphism! conn {:name "S/ProjectionCompany/founder"
                                                        :src "S/ProjectionCompany" :dst "S/ProjectionResearch"})
                    relation (-> raw
                                 (assoc-in ["simmis" "properties" "founder"]
                                           {"id" (str founder) "type" (str company)})
                                 (assoc "founder" {"id" (str page-b) "path" "research/a.md"}))]
                (write! conn "companies/a.md" (source relation body))
                (let [result (view/prepare @conn options)]
                  (is (= :invalid (:status result)))
                  (is (nil? (:db result)))
                  (is (some #(= :unresolved-property-reference (:kind %)) (:diagnostics result))))))))
        (finally (governance/ungovern! conn) (d/release conn) (d/delete-database cfg))))))
