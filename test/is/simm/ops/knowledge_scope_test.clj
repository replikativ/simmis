(ns is.simm.ops.knowledge-scope-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-scope :as scope]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.knowledge-workspace :as workspace]))

(def root-scope
  {:version 2 :output-root "out/knowledge" :destination-root ""
   :exclude-prefixes [".obsidian" "inbox" "transcripts"]})

(def root-policy
  {:version 2 :trigger-roots ["inbox" "transcripts"] :context-roots [""]
   :exclude-prefixes [".obsidian"] :output-scope root-scope})

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:name "Scoped knowledge publication"}) (f conn)
           (finally (d/release conn))))))

(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))

(defn- plan [basis captured output]
  (plans/plan {:request (random-uuid) :room (random-uuid) :attempt (random-uuid)
               :environment {:fixture :scope} :workflow-bundle (random-uuid)
               :publication-basis basis :retained-inputs (:inputs captured) :output output}))

(deftest root-vault-has-one-role-complete-occupancy-and-exact-publication
  (with-repository
    (fn [conn]
      (doseq [[path text] {"inbox/evidence.md" "Evidence"
                          "Topic.md" "Before\r\n" ".obsidian/private.md" "Excluded"
                          "workflows/helper.md" "Code" "in/stale.md" "Staging"
                          "out/wiki/stale.md" "Old output" "asset.bin" "Asset"}]
        (write! conn path text))
      (let [db @conn basis (inputs/publication-basis db nil root-scope)
            captured (inputs/capture db root-policy)
            roles (into {} (map (juxt :path :role)) (get-in captured [:manifest :files]))]
        (is (= {"Topic.md" :context "inbox/evidence.md" :trigger} roles))
        (is (= captured (inputs/verify-capture root-policy captured)))
        (is (contains? (:files basis) ".obsidian/private.md"))
        (is (contains? (:files basis) "asset.bin"))
        (is (contains? (:files basis) "out/wiki/stale.md"))
        (is (thrown? Exception (inputs/publication-before basis ".obsidian/private.md")))
        (is (thrown? Exception (inputs/publication-before basis "inbox/evidence.md")))
        (write! conn "out/knowledge/Topic.md" "After\r\n")
        (write! conn "out/knowledge/New.md" "New")
        (let [output (outputs/capture-db @conn root-scope) planned (plan basis captured output)
              manifest (:manifest planned)
              row (manifests/entity manifest {})]
          (is (= 2 (:version output) (:version manifest)))
          (is (= #{"Topic.md" "New.md"} (set (keys (:files output)))))
          (is (= root-scope (:scope manifest)))
          (is (= "Before\r\n" (markdown/decode (outputs/bytes! (get-in planned [:bodies "Topic.md" :before])))))
          (is (= {:state :absent} (get-in manifest [:entries "New.md" :before])))
          (is (= manifest (manifests/metadata row)))
          (d/transact conn manifests/schema)
          (storage/publish! conn planned #(d/transact conn [(assoc % :db/id -1)]))
          (is (= manifest (storage/manifest conn (:id manifest))))
          (is (thrown? Exception (plans/plan {:request (random-uuid) :room (random-uuid)
                                             :attempt (random-uuid) :environment {}
                                             :workflow-bundle (random-uuid) :retained-inputs {}
                                             :publication-basis (inputs/publication-basis db)
                                             :output output})))
          (let [receipt (dispositions/make
                         {:request (:request manifest) :intent (random-uuid) :job (random-uuid)
                          :candidate (:attempt manifest) :attempt-content (random-uuid)
                          :basis (:basis manifest) :output (:output manifest) :kind :proposed
                          :manifest (:id manifest) :proposal (:proposal planned)} 2)]
            (is (= 2 (:version (dispositions/validate! receipt))))))))))

(deftest folder-destination-mapping-preserves-blocked-ancestors
  (with-repository
    (fn [conn]
      (let [output-scope (assoc root-scope :destination-root "Notes/topics")]
        (write! conn "Notes" "A file blocks this directory")
        (write! conn "out/knowledge/New.md" "New")
        (let [basis (inputs/publication-basis @conn nil output-scope)
              output (outputs/capture-db @conn output-scope)]
          (is (= #{"Notes/topics/New.md"} (set (keys (:files output)))))
          (is (contains? (:files basis) "Notes"))
          (is (thrown? Exception (inputs/publication-before basis "Notes/topics/New.md")))
          (is (thrown? Exception (scope/target-path! output-scope "Notes/other.md"))))))))

(deftest old-contracts-stay-literal-wiki-and-cannot-be-relabelled
  (with-repository
    (fn [conn]
      (write! conn "out/wiki/Page.md" "Legacy")
      (let [old (outputs/capture-db @conn) basis (inputs/publication-basis @conn)]
        (is (= 1 (:version old) (:version basis)))
        (is (= (:id old) (hasch/uuid [:simmis/knowledge-output-v1 (dissoc old :id)])))
        (is (= (:id basis) (hasch/uuid [:simmis/knowledge-publication-basis-v1 (dissoc basis :id)])))
        (is (thrown? Exception (outputs/target-path! "Page.md")))
        (is (thrown? Exception (outputs/validate! (assoc old :version 2 :scope root-scope))))
        (is (thrown? Exception (outputs/validate! (assoc old :scope root-scope))))))))

(deftest source-output-and-reserved-namespace-gates
  (is (= root-policy (inputs/normalize-policy root-policy)))
  (is (thrown? Exception (inputs/normalize-policy (assoc-in root-policy [:output-scope :exclude-prefixes] [".obsidian"]))))
  (is (thrown? Exception (inputs/normalize-policy (assoc inputs/default-policy :context-roots [""]))))
  (doseq [path ["workflows/new.md" "in/new.md" "out/new.md" "inbox/new.md" ".obsidian/new.md"]]
    (is (thrown? Exception (scope/target-path! root-scope path))))
  (is (thrown? Exception (workspace/validate-inputs! {"/out/knowledge/forged.md" "Forged"}))))
