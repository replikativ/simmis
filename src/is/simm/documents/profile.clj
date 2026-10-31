(ns is.simm.documents.profile
  "Trusted HOST lifecycle for explicitly new canonical knowledge repositories.
   Not a legacy classifier, transport policy or document-open installer."
  (:require [clj-yaml.core :as yaml]
            [datahike.api :as d]
            [dvergr.chat.schema :as chat-schema]
            [geschichte.repo :as repo]
            [geschichte.query :as query]
            [geschichte.schema :as repository-schema]
            [is.simm.documents.chart-profile :as charts]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.repository :as documents]
            [is.simm.documents.space :as space]
            [is.simm.documents.starter-bundle :as starter]
            [is.simm.documents.view :as view]
            [is.simm.model.accounting :as accounting]
            [is.simm.model.crud :as crud]
            [is.simm.model.schema :as model-schema]
            [is.simm.model.store :as store]
            [is.simm.starter.wiki-maintenance :as wiki]
            [is.simm.starter.conversation-digest :as digest]
            [kontor.governance :as governance]
            [kontor.schema :as accounting-schema]))

(def selection-options
  {:roots [""] :schema-revision space/profile :entity-profile space/profile})

(defn- refuse! [reason]
  (throw (ex-info "Canonical knowledge repository preparation refused"
                  {:type ::refused :reason reason})))

(defn- profile! [options]
  (when-not (= space/profile (:profile options)) (refuse! :profile-mismatch)))

(defn- schema-shape [attribute]
  (assoc (select-keys attribute [:db/valueType :db/cardinality :db/unique])
         :db/isComponent (boolean (:db/isComponent attribute))))

(defn- validate-foundation! [db]
  ;; Read actual mandatory shapes, not an accounting sentinel or profile marker.
  ;; Extra user-defined schema is allowed. The Page title uniqueness override
  ;; is deliberately applied last, just as in the pristine store installer.
  (let [actual (d/schema db)
        required (into {} (map (juxt :db/ident identity))
                       (concat repository-schema/schema documents/schema space/schema
                               model-schema/full-schema accounting-schema/all
                               chat-schema/full-schema store/page-title-unique-schema))]
    (doseq [[ident expected] required]
      (when-not (= (schema-shape expected) (schema-shape (get actual ident)))
        (refuse! :incompatible-mandatory-schema)))
    (when-not (and (= 1 (count (take 2 (d/datoms db :aevt :geschichte.repo/id))))
                   (space/provisioned? db))
      (refuse! :invalid-repository-identity))))

(defn prepare-open!
  "Validate mandatory profile/schema and reattach synchronous writer governance.
   No schema installation, source parsing, seed transactions or async workers.
   Malformed authored Markdown can still be read/repaired through raw source;
   its canonical projection remains independently guarded by view/prepare."
  [conn options]
  (profile! options)
  (validate-foundation! @conn)
  (accounting/govern-store! conn))

(defn- write-markdown! [conn path metadata body]
  (repo/write! conn path
               (markdown/source-bytes (str "---\n" (yaml/generate-string metadata) "---\n" body))))

(defn- seed-files! [conn]
  (let [company (crud/create-type! conn {:name "Company"})
        research (crud/create-type! conn {:name "Research"})
        person (:entity/uuid (d/pull @conn [:entity/uuid] [:entity/name "S/Person"]))
        revenue (crud/create-morphism! conn {:name "S/Company/revenue" :src "S/Company" :dst "S/Number" :optional? false})
        founder (crud/create-morphism! conn {:name "S/Company/founder" :src "S/Company" :dst "S/Person"})
        active (crud/create-morphism! conn {:name "S/Research/active" :src "S/Research" :dst "S/Boolean"})
        person-id (random-uuid) block-id (random-uuid)]
    (write-markdown! conn "wiki/Welcome.md" {"simmis-id" (str (random-uuid))}
                     "# Welcome\n\nYour knowledge lives in these Markdown files.\n\nEdit source or use the rich editor and typed properties.\n")
    (write-markdown! conn "wiki/People/Founder.md"
                     {"simmis-id" (str person-id) "simmis" {"version" 1 "types" [(str person)]}}
                     "# Founder\n\nReplace this example with your own people and research.\n")
    (write-markdown! conn "wiki/Company.md"
                     {"simmis-id" (str (random-uuid))
                      "simmis" {"version" 1 "types" [(str company)]
                                "properties" {"revenue" {"id" (str revenue) "type" (str company)}
                                              "founder" {"id" (str founder) "type" (str company)}}
                                "blocks" {(str block-id)
                                          {"types" [(str research)]
                                           "properties" {"active" {"id" (str active) "type" (str research)}}
                                           "values" {"active" false}}}}
                      "revenue" 0 "founder" {"id" (str person-id)}}
                     (str "# Company\n\nAn example company with editable, typed properties.\n\n"
                          "<!-- simmis:block " block-id " -->\n\nResearch to explore next.\n\n<!-- /simmis:block -->\n"))
    (repo/write! conn "code/welcome.clj"
                 (markdown/source-bytes ";; Ordinary editable source, not executed on open.\n(str \"Hello, \" \"Simmis\")\n"))))

(defn initialize-foundation!
  "HOST-only pristine schema/governance foundation for an unpublished source
   import. Writes no files, Git commits or workflow defaults. The importer owns
   validation and publication; authored code is never evaluated here."
  [conn options]
  (profile! options)
  (space/provision! conn)
  (chat-schema/install-schema! conn)
  (d/transact conn (charts/installation-tx @conn))
  (validate-foundation! @conn)
  conn)

(defn initialize!
  "Pristine exclusive connection only. Seed and validate the whole starter wiki
   before its owner can publish a Room. Failure belongs to that bootstrap owner,
   never to an implicit migration or a retry of a partially initialized store."
  ([conn options bundle] (initialize! conn options bundle (wiki/source) (digest/source)))
  ([conn options bundle wiki-source]
   (initialize! conn options bundle wiki-source {}))
  ([conn options bundle wiki-source digest-source]
   (profile! options)
   (starter/verify! bundle)
   (initialize-foundation! conn options)
   ;; Optional feature definitions belong only to explicit NEW-space creation.
   ;; Existing canonical v1 repositories are valid without this feature.
   (let [{:keys [files provenance]} (starter/adapt bundle (:geschichte.repo/id (query/repository @conn)))]
     (doseq [[path text] files] (repo/write! conn path (markdown/source-bytes text)))
     (repo/write! conn "code/starter-provenance.edn" (markdown/source-bytes (str (pr-str provenance) "\n"))))
   (seed-files! conn)
  ;; Exclusive NEW repository only. Reopening never reinstalls defaults or
  ;; overwrites customized/deleted workflow files.
   (doseq [[path text] wiki-source]
     (repo/write! conn (str "workflows/" wiki/name "/" path) (markdown/source-bytes text)))
   (doseq [[path text] digest-source]
     (repo/write! conn (str "workflows/" digest/name "/" path) (markdown/source-bytes text)))
   (validate-foundation! @conn)
   (let [prepared (view/prepare @conn selection-options)]
     (when-not (= :ready (:status prepared)) (refuse! :invalid-starter-projection)))
   (repo/stage-all! conn)
   (repo/commit! conn {:message "Initialize knowledge workspace" :author "Simmis"})
   conn))

(defn policies
  "Closed HOST map over already-owned immutable bundle data. Acquire/verify the
   packaged resource BEFORE Room intent. Never accept bundles from transports;
   hosts supplying a custom bundle own its source-provenance claim."
  [bundle]
  (let [bundle (starter/verify! bundle)
        ;; Acquire fixed bounded resources BEFORE durable Room intent.
        wiki-source (wiki/source)
        digest-source (digest/source)]
    {space/profile {:initialize! (fn [conn options] (initialize! conn options bundle wiki-source digest-source)) :prepare-open! prepare-open!
                  ;; Exclusive unpublished initialization only; no async worker
                  ;; is started above. This synchronous detach is idempotent.
                    :abort-create! (fn [conn _] (governance/ungovern! conn))}}))
