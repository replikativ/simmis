(ns is.simm.documents.canonical-view-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-yaml.core :as yaml]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.property-model :as property-model]
            [is.simm.uis.entity-view :as entity-view]
            [org.replikativ.spindel.dom.ssr :as ssr]
            [clojure.string :as str]
            [is.simm.ops.owned-files :as owned-files]
            [is.simm.documents.space :as space]
            [is.simm.fixtures.accounting :as accounting-fixture]
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

(defn- edit-request [model alias value]
  (assoc (select-keys model [:path :target :expected :revision :scope-revision])
         :alias alias :property (:id (first (filter #(= alias (:alias %)) (:fields model))))
         :value value))

(deftest canonical-scalars-use-the-existing-query-model-without-mutating-the-base
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (accounting-fixture/with-schema-only #(space/provision! conn))
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
            (testing "safe property model and scalar commands rederive actual source definitions"
              (let [page (property-model/inspect-in base "companies/a.md" {:kind :page})
                    field (first (filter #(= "turnover" (:alias %)) (:fields page)))
                    title (first (filter #(= (:entity/uuid seed/morphism-Page-title) (:id %)) (:fields page)))
                    block (property-model/inspect-in base "companies/a.md" {:kind :block :id block-id})
                    active-field (first (filter #(= "active" (:alias %)) (:fields block)))
                    request (edit-request page "turnover" 42)
                    prepared-edit (property-model/prepare-edit-in base options request)
                    block-edit (property-model/prepare-edit-in base options (edit-request block "active" true))]
                (is (= {:kind :page :id page-a} (:entity page)))
                (is (= {:kind :block :id block-id} (:entity block)))
                (let [html (ssr/render-to-string (entity-view/view (assoc page :fields [field]) {:title "Company"}))]
                  (is (str/includes? html (str "data-entity-id=\"" page-a "\"")))
                  (is (str/includes? html ">0</span>"))
                  (is (not (re-find #"<(?:input|button|select)(?:\s|>)" html))))
                (is (= 0 (:value field)))
                (is (:required? field))
                (is (:editable? field))
                (is (= :db.type/long (:value-type field)))
                (is (= "companies/a.md" (:value title)))
                (is (= :generated-property (:reason title)))
                (is (false? (:editable? title)))
                (is (false? (:value active-field)))
                (is (:editable? active-field))
                (is (empty? (:diagnostics page)))
                (is (every? #(not (contains? page %)) [:db :compiled :selection :source]))
                (is (every? #(not-any? (set (keys %)) [:morphism :computation :validation :schema :compiled]) (:fields page)))
                (is (= #{:path :expected :text} (set (keys prepared-edit))))
                (is (= 42 (get-in (markdown/parse (:text prepared-edit)) [:frontmatter :properties "turnover"])))
                (is (= true (get-in (markdown/parse (:text block-edit))
                                    [:frontmatter :properties "simmis" "blocks" (str block-id) "values" "active"])))
                (is (= original (markdown/decode (repo/read-entry conn (get (repo/worktree conn) "companies/a.md")))))
                (doseq [unsupported [(d/db-with base [{:entity/uuid (:entity/uuid seed/object-Page)
                                                       :entity/name "S/RenamedPage"}])
                                     (d/db-with base [{:geschichte.work/path "companies/a.md"
                                                       :geschichte.work/mode (long 33261)}])]]
                  (is (thrown? clojure.lang.ExceptionInfo
                               (property-model/inspect-in unsupported "companies/a.md" {:kind :page}))))
                (doseq [bad [(assoc request :extra true) (assoc request :property (random-uuid))
                             (assoc request :alias "not-declared") (assoc request :revision (random-uuid))
                             (assoc request :scope-revision (random-uuid)) (assoc request :expected nil)
                             (assoc request :target {:kind :block :id (random-uuid)})
                             (assoc request :target {:kind :page :id page-a})
                             (assoc request :value nil) (assoc request :value false)]]
                  (is (thrown? clojure.lang.ExceptionInfo (property-model/prepare-edit-in base options bad))))
                (let [changed (d/db-with base [{:entity/uuid revenue :morphism/validation "never execute this descriptor"}])
                      new-model (property-model/inspect-in changed "companies/a.md" {:kind :page})
                      new-field (first (filter #(= "turnover" (:alias %)) (:fields new-model)))]
                  (is (not= (:scope-revision page) (:scope-revision new-model)))
                  (is (= :property-needs-validator (:reason new-field)))
                  (is (false? (:editable? new-field)))
                  (is (thrown? clojure.lang.ExceptionInfo (property-model/prepare-edit-in changed options request)))
                  (is (thrown? clojure.lang.ExceptionInfo
                               (property-model/prepare-edit-in changed options (edit-request new-model "turnover" 2)))))))
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
                  (let [model (property-model/inspect-in person-base "companies/a.md" {:kind :page})
                        relation (first (filter #(= "person" (:alias %)) (:fields model)))
                        optional-edit (property-model/prepare-edit-in person-base options (edit-request model "slug" nil))
                        uuid-value (str (random-uuid))
                        uuid-edit (property-model/prepare-edit-in person-base options (edit-request model "external-id" uuid-value))]
                    (is (nil? (:reason relation)))
                    (is (true? (:editable? relation)))
                    (is (= :ready (get-in model [:reference-options person-type :status])))
                    (is (some #(= (str person-id) (:id %)) (get-in model [:reference-options person-type :items])))
                    (is (not-any? #(= (str page-b) (:id %)) (get-in model [:reference-options person-type :items])))
                    (is (= {:text (str person-id)} (:input relation)))
                    (is (= {:representation :mapping :target (str person-id) :path-hint "research/a.md"
                            :codomain person-type :clearable? false} (:relation relation)))
                    (is (= (source person-raw body)
                           (:text (property-model/prepare-edit-in person-base options (edit-request model "person" (str person-id))))))
                    (is (thrown? clojure.lang.ExceptionInfo
                                 (property-model/prepare-edit-in person-base options (edit-request model "person" nil))))
                    (is (thrown? clojure.lang.ExceptionInfo
                                 (property-model/prepare-edit-in person-base options (edit-request model "person" (str page-b)))))
                    (let [next-person (random-uuid)
                          alternate (d/db-with person-base [{:entity/uuid next-person
                                                            :instance/of-role [[:entity/uuid person-type]]}])
                          input-request (-> (edit-request model "person" nil) (dissoc :value)
                                            (assoc :input {:text (str next-person)}))
                          prepared (property-model/prepare-input-in alternate options input-request)
                          after (get-in (markdown/parse (:text prepared)) [:frontmatter :properties "person"])
                          deleted (d/db-with alternate [[:db/retractEntity [:entity/uuid next-person]]])
                          retyped (d/db-with alternate [[:db/retract [:entity/uuid next-person]
                                                        :instance/of-role [:entity/uuid person-type]]])]
                      (is (= {"id" (str next-person) "path" "research/a.md"} after))
                      (is (= person-base @conn) "Preparing a relation edit never writes the source repository")
                      (is (thrown? clojure.lang.ExceptionInfo
                                   (property-model/prepare-input-in deleted options input-request)))
                      (is (thrown? clojure.lang.ExceptionInfo
                                   (property-model/prepare-input-in retyped options input-request))))
                    (is (nil? (get-in (markdown/parse (:text optional-edit)) [:frontmatter :properties "slug"])))
                    (is (= uuid-value (get-in (markdown/parse (:text uuid-edit)) [:frontmatter :properties "external-id"]))))
                  (testing "scalar relation clearing still validates optionality in the fresh source/schema"
                    (write! conn "companies/a.md" (source (assoc person-raw "person" (str person-id)) body))
                    (try
                      (let [scalar-db @conn
                            scalar-model (property-model/inspect-in scalar-db "companies/a.md" {:kind :page})
                            scalar-field (first (filter #(= "person" (:alias %)) (:fields scalar-model)))
                            cleared (property-model/prepare-edit-in scalar-db options (edit-request scalar-model "person" nil))
                            required-db (d/db-with scalar-db [{:entity/uuid person-property :morphism/optional? false}])
                            required-model (property-model/inspect-in required-db "companies/a.md" {:kind :page})
                            required-field (first (filter #(= "person" (:alias %)) (:fields required-model)))]
                        (is (true? (get-in scalar-field [:relation :clearable?])))
                        (is (nil? (get-in (markdown/parse (:text cleared)) [:frontmatter :properties "person"])))
                        (is (false? (get-in required-field [:relation :clearable?])))
                        (is (thrown? clojure.lang.ExceptionInfo
                                     (property-model/prepare-edit-in required-db options (edit-request required-model "person" nil))))
                        (is (= scalar-db @conn) "Neither successful nor refused preparation publishes source"))
                      (finally (write! conn "companies/a.md" (source person-raw body)))))
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

(deftest malformed-typed-metadata-does-not-replace-an-otherwise-readable-file-with-an-error
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? false :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (accounting-fixture/with-schema-only #(space/provision! conn))
        (let [text (source {"simmis-id" (str page-a) "simmis" "invalid declaration"} "# Retained source\n")]
          (write! conn "wiki/a.md" text)
          (let [before @conn
                result (@#'owned-files/property-projection before "wiki/a.md" {:kind :page})]
            (is (= :unavailable (:status result)))
            (is (= :invalid-declarations (:reason result)))
            (is (= text (:text (is.simm.documents.files/read-file-in before "wiki/a.md"))))
            (is (= before @conn))))
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest ordinary-markdown-coexists-with-semantic-pages-without-adoption
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}
        plain (str "---\ntitle: Upstream readme\n---\n# Imported documentation\n\n"
                   "Ordinary Markdown stays portable.\n")]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (accounting-fixture/with-schema-only #(space/provision! conn))
        (write! conn "README.md" plain)
        (let [before @conn only-plain (view/prepare before options)]
          (is (= :ready (:status only-plain)))
          (is (= 1 (count (:documents only-plain))))
          (is (empty? (d/q '[:find ?id :where [?e :document/path] [?e :entity/uuid ?id]] (:db only-plain))))
          (is (view/current? @conn options only-plain))
          (is (view/current? (:db only-plain) options only-plain))
          (is (view/same-source? before @conn)))
        (write! conn "wiki/Company.md" (source {"simmis-id" (str page-a)} "# Company\n"))
        (let [before @conn prepared (view/prepare before options)
              doc (first (filter #(= "README.md" (:path %)) (:documents prepared)))
              projected (:db prepared)]
          (is (= :ready (:status prepared)) (pr-str (:diagnostics prepared)))
          (is (= :provisional (:identity-status doc)))
          (is (= #{"README.md" "wiki/Company.md"}
                 (set (d/q '[:find [?path ...] :where [_ :document/path ?path]] projected))))
          (is (= #{page-a} (set (d/q '[:find [?id ...] :where [?e :document/path] [?e :entity/uuid ?id]] projected))))
          (is (nil? (:entity/uuid (d/pull projected [:entity/uuid] [:document/id (:id doc)]))))
          (is (= #{"wiki/Company.md"} (set (d/q '[:find [?title ...] :where [?e :document/path] [?e :S.Page/title ?title]] projected))))
          (is (view/current? @conn options prepared))
          (is (view/current? projected options prepared))
          (is (view/same-source? before @conn) "Reading/projection never adopts or rewrites a file")
          (is (= plain (String. ^bytes (repo/read conn "README.md") "UTF-8")))
          (is (= 4 (get-in prepared [:selection :projection-version])))
          (is (= 2 (get-in prepared [:selection :canonical-codec :version])))
          (testing "unrelated authored entity sharing a provisional UUID is not a canonical collision"
            (d/transact conn [{:entity/uuid (:id doc)}])
            (is (view/current? @conn options prepared))
            (is (= :ready (:status (view/prepare @conn options)))))
          (testing "ordinary edits are valid without semantic adoption"
            (let [entry (get-in prepared [:selection :files "README.md"])]
              (is (= :valid (:status (view/validate-replacement @conn options
                                                                {:path "README.md" :expected entry :source (str plain "\nMore documentation.\n")}))))))
          (testing "malformed reserved metadata does not silently become a plain file"
            (doseq [raw [{"simmis-id" "not-a-uuid"}
                         {"simmis" {"version" 99}}
                         {"simmis" {"version" 1}}
                         {"simmis" {"version" 1 "blocks" {(str block-id) {"values" {"unknown" 123}}}}}
                         {"simmis" {"version" 1 "types" [(str (:entity/uuid seed/object-Page))]}}]]
              (write! conn "README.md" (source raw (str "# Broken declarations\n\n<!-- simmis:block " block-id " -->\n\nA block.\n\n<!-- /simmis:block -->\n")))
              (is (= :invalid (:status (view/prepare @conn options))))))
          (testing "explicit adoption participates in the existing canonical constraints"
            (write! conn "README.md" (source {"simmis-id" (str page-b)} "# Adopted documentation\n"))
            (let [adopted (view/prepare @conn options)]
              (is (= :ready (:status adopted)))
              (is (= #{page-a page-b} (set (d/q '[:find [?id ...] :where [?e :document/path] [?e :entity/uuid ?id]] (:db adopted))))))
            (write! conn "README.md" (source {"simmis-id" (str page-a)} "# Duplicate\n"))
            (is (= :invalid (:status (view/prepare @conn options))))))
        (finally
          (governance/ungovern! conn)
          (d/release conn)
          (d/delete-database cfg))))))
