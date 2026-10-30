(ns is.simm.ops.github-review-test
  (:require [clj-yaml.core :as yaml]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.versioning :as versioning]
            [geschichte.git.http :as http]
            [geschichte.git.project :as project]
            [geschichte.repo :as repo]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]
            [is.simm.model.crud :as crud]
            [is.simm.model.github-proposals :as provenance]
            [is.simm.ops.github-candidate-roots :as roots]
            [is.simm.ops.github-merge :as merge]
            [is.simm.ops.github-merge-test :as fixtures]
            [is.simm.ops.github-review :as review]
            [is.simm.ops.knowledge-changes :as changes]
            [kontor.governance :as governance]))

(defn- blueprint [canonical private local-head base incoming candidate]
  (let [source @canonical basis (changes/basis source)
        target (assoc (select-keys basis [:repository :store :backend :branch :ref])
                      :registry-system (random-uuid) :runtime-system "repository")
        connection {:version 1 :room (random-uuid) :target-stamp (random-uuid)
                    :source {:kind :github :repository "owner/repository"} :ref "refs/heads/main" :target target}
        base-commit (repo/commit-by-id private base) incoming-commit (repo/commit-by-id private incoming)
        identity (provenance/incoming-identity connection
                                               {:head base :git-oid (:geschichte.commit/git-oid base-commit) :proof-id (random-uuid)}
                                               {:head incoming :git-oid (:geschichte.commit/git-oid incoming-commit)})]
    {:version 1 :identity identity :key (provenance/incoming-key identity)
     :producer {:sync-id (random-uuid) :claim (random-uuid) :by (random-uuid)}
     :proposal {:id (random-uuid) :scope (:registry-system target)
                :review-ref (str "refs/heads/github-review-" (random-uuid)) :base local-head :head candidate}
     :canonical-basis basis
     :roots {:base (roots/descriptor (d/commit-as-db @private (:geschichte.commit/snapshot base-commit)) base)
             :local (roots/descriptor source local-head)
             :incoming (roots/descriptor (d/commit-as-db @private (:geschichte.commit/snapshot incoming-commit)) incoming)
             :candidate (roots/descriptor @private candidate)}}))

(defn- fixture [unknown? f]
  (#'fixtures/with-repositories
   (fn [upstream canonical]
     (try
       (space/provision! canonical)
       (let [company (crud/create-type! canonical {:name "ReviewCompany"})
             revenue (crud/create-morphism! canonical {:name "S/ReviewCompany/revenue"
                                                       :src "S/ReviewCompany" :dst "S/Number"})
             page (random-uuid)
             text (fn [type amount]
                    (str "---\n" (yaml/generate-string
                                  {"simmis-id" (str page)
                                   "simmis" {"version" 1 "types" [(str type)]
                                             "properties" {"revenue" {"id" (str revenue) "type" (str type)}}}
                                   "revenue" amount}) "---\n# Acme\n"))]
         (repo/init! upstream {:branch "refs/heads/main"})
         (let [base (#'fixtures/commit! upstream {"companies/acme.md" (text company 10)} "Initial typed source")]
           (http/clone! (#'fixtures/sender (project/project upstream base)) canonical "origin"
                        "https://github.com/owner/repository.git" (#'fixtures/options))
           (let [imported (:geschichte.commit/id (repo/head-commit canonical))
                 local-head (#'fixtures/commit! canonical {"README.md" "Saved local work\n"} "Local work")
                 next (#'fixtures/commit! upstream {"companies/acme.md" (text (if unknown? (random-uuid) company) 20)} "Incoming value")
                 options (#'fixtures/options)
                 fetched (http/fetch! (#'fixtures/sender (project/project upstream next)) canonical "origin"
                                      "https://github.com/owner/repository.git" options)
                 branch (keyword (str "typed-review-" (random-uuid)))]
             (versioning/branch! canonical (get-in @canonical [:config :branch]) branch)
             (let [private (d/connect (assoc (:config @canonical) :branch branch))]
               (try
                 (let [{:keys [plan upstream-head]} (merge/plan-fetched! private fetched
                                                                         {:ref "refs/heads/main" :preparation-id (random-uuid)
                                                                          :upstream-base imported :operation (:import-budget options)})]
                   (repo/prepare-merge! private plan)
                   (let [candidate (:geschichte.commit/id (repo/commit! private {:message "Frozen typed candidate"}))]
                     (f {:canonical canonical :private private :company company
                         :body (blueprint canonical private local-head imported upstream-head candidate)})))
                 (finally (d/release private) (versioning/delete-branch! canonical branch)))))))
       (finally (governance/ungovern! canonical))))))

(defn- inspect [{:keys [canonical private body]}]
  (review/inspect @canonical (d/commit-as-db @canonical (get-in body [:roots :local :basis :commit])) @private body))

(deftest review-is-stable-and-binds-the-prospective-trusted-type-context
  (fixture false
           (fn [environment]
             (let [first (inspect environment) second (inspect environment)]
               (is (= :ready (:status first)))
               (is (= (:review-id first) (:review-id second)))
               (is (seq (get-in first [:basis :typed-validation :typed-scopes])))
               (is (= ["companies/acme.md"] (mapv :path (:changes first))))))))

(deftest imported-unknown-types-do-not-install-schema-or-become-reviewable
  (fixture true
           (fn [{:keys [canonical] :as environment}]
             (let [before @canonical result (inspect environment)]
               (is (= :held (:status result)))
               (is (= :document-validation (:reason result)))
               (is (seq (:diagnostics result)))
               (is (= before @canonical))))))

(deftest current-definitions-control-review-instead-of-the-candidates-old-catalog
  (fixture false
           (fn [{:keys [canonical private company] :as environment}]
             (is (= :ready (:status (inspect environment))))
             (d/transact canonical [[:db/retractEntity [:entity/uuid company]]])
             (is (= :ready (:status (view/prepare @private profile/selection-options))))
             (let [result (inspect environment)]
               (is (= :held (:status result)))
               (is (= :document-validation (:reason result)))))))

(deftest a-new-local-version-cannot-be-covered-by-the-old-review
  (fixture false
           (fn [{:keys [canonical] :as environment}]
             (#'fixtures/commit! canonical {"README.md" "Changed after preparation\n"} "New local work")
             (is (= :local-version-changed
                    (try (inspect environment) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))

(deftest unchanged-canonical-content-is-budget-checked-before-validator-reads
  (fixture false
           (fn [{:keys [canonical] :as environment}]
             (let [id (d/q '[:find ?id . :where [?w :geschichte.work/path "README.md"]
                             [?w :geschichte.work/content ?c] [?c :geschichte.content/id ?id]] @canonical)]
        ;; Simulate corrupt native metadata without changing the visible file
        ;; manifest. The unbudgeted decoder would recurse; the budgeted
        ;; preflight must detect the cycle before shared validation runs.
               (d/transact canonical [{:geschichte.content/id id :geschichte.content/kind :line-delta
                                       :geschichte.content/base [:geschichte.content/id id]
                                       :geschichte.content/depth 1}])
               (let [before @canonical]
                 (is (= :geschichte.content/content-cycle
                        (try (inspect environment) nil
                             (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
                 (is (= before @canonical)))))))

(deftest markdown-removal-is-explicitly-held-until-prospective-set-validation
  (fixture false
           (fn [{:keys [private body] :as environment}]
             (repo/remove! private "companies/acme.md")
             (repo/stage-all! private)
             (let [head (:geschichte.commit/id (repo/commit! private {:message "Removed typed document"}))
                   revised (-> body (assoc-in [:proposal :head] head)
                               (assoc-in [:roots :candidate] (roots/descriptor @private head)))
                   result (inspect (assoc environment :body revised))]
               (is (= :held (:status result)))
               (is (= :markdown-removal-requires-review (:reason result)))
               (is (= ["companies/acme.md"] (:unsupported result)))))))
