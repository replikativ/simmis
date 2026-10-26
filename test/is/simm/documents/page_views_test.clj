(ns is.simm.documents.page-views-test
  (:require [clojure.test :refer [deftest is]]
            [clj-yaml.core :as yaml]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.page-views :as views]
            [is.simm.documents.page-view-spec :as spec]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as projection]
            [is.simm.model.crud :as crud]))

(def page-a #uuid "20fb42d0-54d8-4d56-8c1b-18401236413f")
(def page-b #uuid "28ee282b-a48d-4cf1-b097-e05ef259a33b")
(def page-c #uuid "44444768-ecc9-43f7-a785-6e08d1380fdb")

(defn- write! [conn path id raw body]
  (repo/write! conn path
               (markdown/source-bytes
                (str "---\n" (yaml/generate-string (assoc raw "simmis-id" (str id))) "---\n" body))))

(defn- with-fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? false :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (space/provision! conn)
        (let [company (crud/create-type! conn {:name "ViewCompany"})
              research (crud/create-type! conn {:name "ViewResearch"})
              revenue (crud/create-morphism! conn {:name "S/ViewCompany/revenue"
                                                  :src "S/ViewCompany" :dst "S/Number"})
              owner (crud/create-morphism! conn {:name "S/ViewCompany/owner"
                                                :src "S/ViewCompany" :dst "S/Person"})
              native-person (random-uuid)
              person-type (:entity/uuid (d/pull @conn [:entity/uuid] [:entity/name "S/Person"]))
              company-data {"simmis" {"version" 1 "types" [(str company)]
                                      "properties" {"revenue" {"id" (str revenue) "type" (str company)}
                                                    "owner" {"id" (str owner) "type" (str company)}}}
                            "revenue" 9007199254740993 "owner" (str native-person)}
              descriptor {:version 1 :id (random-uuid) :presentation :table
                          :source {:kind :pages :roots ["companies"] :types [company]}
                          :columns [{:kind :title} {:property revenue} {:property owner}]}]
          (d/transact conn [{:entity/uuid native-person :entity/name "NOT SOURCE DATA"
                            :instance/of-role [[:entity/uuid person-type]]}])
          (write! conn "companies/Beta.md" page-b company-data "# Beta\n")
          (write! conn "companies/alpha.md" page-a (assoc company-data "revenue" 0) "# Alpha\n")
          (write! conn "companies/research.md" page-c
                  {"simmis" {"version" 1 "types" [(str research)]}} "# Research\n")
          (repo/write! conn "outside/import.md" (markdown/source-bytes "# Provisional outside scope\n"))
          (f {:conn conn :company company :research research :revenue revenue :owner owner
              :native-person native-person :descriptor descriptor :company-data company-data}))
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest scoped-table-exports-only-source-backed-typed-values-with-stable-order
  (with-fixture
    (fn [{:keys [conn descriptor native-person]}]
      (let [source @conn tx (:max-tx source) result (views/prepare source descriptor)]
        (is (= :ready (:status result)) (pr-str result))
        (is (:complete? result))
        (is (= [page-a page-b] (mapv :id (:rows result))))
        (is (= 2 (:total result)))
        (is (= {:kind :number :text "9007199254740993"} (get-in result [:rows 1 :cells 1])))
        (is (= {:kind :reference :id native-person} (get-in result [:rows 0 :cells 2]))
            "UUID is portable source data; native person's name must not leak")
        (is (not (contains? result :db)))
        (is (not (contains? result :typed-scopes)))
        (is (not (clojure.string/includes? (pr-str result) "NOT SOURCE DATA")))
        (is (= tx (:max-tx @conn)) "read-only derived projection")
        (is (= result (views/prepare source descriptor)))
        (is (= [page-b page-a]
               (mapv :id (:rows (views/prepare source (assoc descriptor :order [{:kind :title :direction :desc}]))))))
        (let [limited (views/prepare source (assoc descriptor :limit 1))]
          (is (= [page-a] (mapv :id (:rows limited))))
          (is (= 2 (:total limited)))
          (is (false? (:complete? limited))))
        (let [form (views/prepare source (-> descriptor (assoc :presentation :form)
                                                   (assoc-in [:source :page] page-b)))]
          (is (= [page-b] (mapv :id (:rows form))))
          (is (:complete? form)))
        (is (not= (:basis result)
                  (:basis (views/prepare source (assoc-in descriptor [:columns 1 :label] "Revenue")))))))))

(deftest invalid-source-or-definitions-never-masquerade-as-a-successful-empty-table
  (with-fixture
    (fn [{:keys [conn descriptor]}]
      (let [unknown-property (views/prepare @conn (assoc-in descriptor [:columns 1 :property] (random-uuid)))
            unknown-type (views/prepare @conn (assoc-in descriptor [:source :types] [(random-uuid)]))]
        (is (= :unavailable (:status unknown-property)))
        (is (= :unavailable (:status unknown-type)))
        (is (false? (:complete? unknown-type))))
      (repo/write! conn "companies/import.md" (markdown/source-bytes "# Missing durable identity\n"))
      (let [result (views/prepare @conn descriptor)]
        (is (= :unavailable (:status result)))
        (is (= :invalid-source (:reason result)))
        (is (false? (:complete? result)))
        (is (nil? (:rows result)))
        (is (seq (:diagnostics result)))))))

(deftest descriptor-validation-precedes-source-and-storage-errors-stay-errors
  (with-redefs [projection/prepare (fn [& _] (throw (AssertionError. "No source read permitted")))]
    (is (thrown? clojure.lang.ExceptionInfo (views/prepare nil {:version 2}))))
  (with-fixture
    (fn [{:keys [conn descriptor]}]
      (with-redefs [projection/prepare (fn [& _] (throw (ex-info "bounded" {:type ::projection/refused :reason :too-many-files})))]
        (let [result (views/prepare @conn descriptor)]
          (is (= :unavailable (:status result)))
          (is (= :too-many-files (:reason result)))
          (is (false? (:complete? result)))))
      (with-redefs [projection/prepare (fn [& _] (throw (IllegalStateException. "Opaque store failure")))]
        (is (thrown? IllegalStateException (views/prepare @conn descriptor)))))))

(deftest form-selection-and-type-intersection-are-explicit
  (with-fixture
    (fn [{:keys [conn descriptor company research company-data]}]
      (doseq [id [(random-uuid) page-c]]
        (let [result (views/prepare @conn (-> descriptor (assoc :presentation :form)
                                                        (assoc-in [:source :page] id)))]
          (is (= :unavailable (:status result)))
          (is (= :page-not-in-selection (:reason result)))
          (is (false? (:complete? result)))))
      (write! conn "companies/Beta.md" page-b
              (assoc-in company-data ["simmis" "types"] [(str company) (str research)]) "# Both types\n")
      (let [result (views/prepare @conn (assoc-in descriptor [:source :types] [company research]))]
        (is (= :ready (:status result)))
        (is (= [page-b] (mapv :id (:rows result))))))))

(deftest owner-managed-and-generated-columns-are-never-native-attribute-reads
  (with-fixture
    (fn [{:keys [conn descriptor]}]
      (doseq [options [{:name "S/ViewResearch/reflected" :storage-attr :entity/name}
                       {:name "S/ViewResearch/computed" :computation "(throw (Exception. \"must-not-run\"))"}]]
        (let [id (crud/create-morphism! conn (merge {:src "S/ViewResearch" :dst "S/String" :optional? true} options))
              result (views/prepare @conn (assoc descriptor :columns [{:property id}]))]
          (is (= :unavailable (:status result)))
          (is (nil? (:rows result)))))
      (let [id (:entity/uuid (d/pull @conn [:entity/uuid] [:entity/name "S/Page/title"]))
            result (views/prepare @conn (assoc descriptor :columns [{:property id}]))]
        (is (= :unavailable (:status result)))
        (is (= :unsupported-column-property (:reason result)))))))

(deftest portable-reference-order-does-not-depend-on-native-entity-identifiers
  (with-fixture
    (fn [{:keys [conn descriptor company company-data]}]
      (let [ids [#uuid "00000000-0000-4000-8000-000000000001" #uuid "00000000-0000-4000-8000-000000000002"]
            person-type (:entity/uuid (d/pull @conn [:entity/uuid] [:entity/name "S/Person"]))
            entity (fn [id] {:entity/uuid id :instance/of-role [[:entity/uuid person-type]]})
            property (crud/create-morphism! conn {:name "S/ViewCompany/owners" :src "S/ViewCompany"
                                                  :dst "S/Person" :cardinality :many :optional? true})
            raw (-> company-data
                    (assoc-in ["simmis" "properties" "owners"] {"id" (str property) "type" (str company)})
                    (assoc "owners" (mapv str ids)))
            _ (d/transact conn (mapv entity (reverse ids)))
            _ (write! conn "companies/alpha.md" page-a raw "# Owners\n")
            source @conn
            moved (d/db-with source (into (mapv #(vector :db/retractEntity [:entity/uuid %]) ids)
                                         (map entity ids)))
            query (assoc descriptor :columns [{:property property}])
            first-result (views/prepare source query)
            second-result (views/prepare moved query)]
        (is (not= (:db/id (d/entity source [:entity/uuid (first ids)]))
                  (:db/id (d/entity moved [:entity/uuid (first ids)]))))
        (is (= :ready (:status first-result)))
        (is (= :ready (:status second-result)))
        (is (= ids (mapv :id (get-in first-result [:rows 0 :cells 0 :items]))))
        (is (= (:rows first-result) (:rows second-result)))
        (is (not (clojure.string/includes? (pr-str (:rows first-result)) ":db/id")))))))

(deftest display-bounds-preserve-unicode-and-never-clip-numeric-identities
  (with-fixture
    (fn [{:keys [conn descriptor company company-data]}]
      (let [property (crud/create-morphism! conn {:name "S/ViewCompany/note" :src "S/ViewCompany"
                                                  :dst "S/String" :optional? true})
            raw (-> company-data
                    (assoc-in ["simmis" "properties" "note"] {"id" (str property) "type" (str company)})
                    (assoc "note" "abc😀z" "revenue" 0))]
        (write! conn "companies/alpha.md" page-a raw "# Unicode\n")
        (with-redefs [views/max-cell-characters 4]
          (let [result (views/prepare @conn (assoc descriptor :columns [{:property property}]))]
            (is (= "abc" (get-in result [:rows 0 :cells 0 :text])))
            (is (false? (get-in result [:rows 0 :cells 0 :complete?])))
            (is (false? (get-in result [:display :complete?])))))
        (with-redefs [views/max-display-characters 4]
          (let [result (views/prepare @conn (assoc descriptor :columns [(second (:columns descriptor))]))]
            (is (= {:kind :number :text "0"} (get-in result [:rows 0 :cells 0])))
            (is (= {:kind :truncated} (get-in result [:rows 1 :cells 0])))
            (is (<= (get-in result [:display :used-characters]) 4))
            (is (false? (get-in result [:display :complete?])))))
        (let [label (str (apply str (repeat 127 "a")) "😀")
              id (crud/create-morphism! conn {:name (str "S/ViewCompany/" label) :src "S/ViewCompany"
                                              :dst "S/String" :optional? true})
              result (views/prepare @conn (assoc descriptor :columns [{:property id}]))]
          (is (= :ready (:status result)))
          (is (= (apply str (repeat 127 "a")) (get-in result [:columns 0 :label])))
          (is (false? (get-in result [:columns 0 :label-complete?]))))))))

(deftest forged-known-tags-and-diagnostics-cannot-export-arbitrary-values
  (with-fixture
    (fn [{:keys [conn descriptor]}]
      (doseq [reason ["SECRET" {:secret "SECRET"} (apply str (repeat 20000 "SECRET"))]]
        (with-redefs [projection/prepare (fn [& _] (throw (ex-info "SECRET message"
                                                                 {:type ::projection/refused :reason reason})))]
          (let [result (views/prepare @conn descriptor)]
            (is (= :unavailable (:status result)))
            (is (= :invalid-property-definition (:reason result)))
            (is (not (clojure.string/includes? (pr-str result) "SECRET"))))))
      (with-redefs [projection/prepare
                    (fn [& _]
                      {:status :invalid :diagnostics
                       [{:kind {:secret "SECRET"} :path "companies/alpha.md" :reason "SECRET"}
                        {:kind :invalid-canonical-document :path "outside/import.md" :reason {:secret "SECRET"}}
                        {:kind (apply str (repeat 20000 "SECRET"))
                         :path (apply str (repeat 20000 "SECRET")) :reason :unknown-secret}]})]
        (let [result (views/prepare @conn descriptor)]
          (is (= :unavailable (:status result)))
          (is (not (clojure.string/includes? (pr-str result) "SECRET")))
          (is (= ["companies/alpha.md"] (vec (keep :path (:diagnostics result)))))
          (is (every? keyword? (map :kind (:diagnostics result))))
          (is (every? keyword? (map :reason (:diagnostics result)))))))))
