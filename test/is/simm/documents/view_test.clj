(ns is.simm.documents.view-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.agents.memory-read :as memory]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.repository :as documents]
            [is.simm.documents.view :as view]))

(def options {:roots ["wiki"] :schema-revision :test-schema-v1})

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:name "Source selection tests"}) (f conn)
           (finally (d/release conn))))))

(defn- write! [conn path source]
  (repo/write! conn path (markdown/source-bytes source)))

(defn- paths [v]
  (set (d/q '[:find [?path ...] :where [_ :document/path ?path]] (:db v))))

(deftest immutable-projection-does-not-write-or-require-a-second-repository
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "# First\n")
      (write! conn "outside/ignored.md" "not selected")
      (let [tx (:max-tx @conn)
            v (view/current-view conn options)]
        (is (= :ready (:status v)))
        (is (= #{"wiki/a.md"} (paths v)))
        (is (= tx (:max-tx @conn)))
        (is (= :provisional (get-in v [:documents 0 :identity-status])))
        (is (= #{[:Heading] [:Text]} (d/q '[:find ?kind :where [_ :document.node/kind ?kind]] (:db v))))
        (is (view/current? @conn options v))
        (is (view/current? (:db v) options v) "Projection datoms do not self-invalidate")
        (write! conn "outside/ignored.md" "unrelated change")
        (is (view/current? @conn options v))
        (write! conn "wiki/a.md" "## Second\n")
        (is (not (view/current? @conn options v)))
        (is (= "# First\n" (get-in v [:documents 0 :source])))
        (is (= #{[1]} (d/q '[:find ?n :where [_ :document.node/heading-level ?n]] (:db v))))
        (let [next (view/current-view conn options)]
          (is (= "## Second\n" (get-in next [:documents 0 :source])))
          (is (= #{[2]} (d/q '[:find ?n :where [_ :document.node/heading-level ?n]] (:db next))))
          (is (not= (:id v) (:id next))))))))

(deftest checkout-and-raw-path-removal-select-the-real-files
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "original")
      (repo/stage-all! conn)
      (repo/commit! conn {:message "Original"})
      (repo/branch! conn "original")
      (write! conn "wiki/a.md" "changed")
      (write! conn "wiki/b.md" "new")
      (repo/stage-all! conn)
      (repo/commit! conn {:message "Changed"})
      (let [changed (view/current-view conn options)]
        (repo/checkout! conn "original")
        (is (not (view/current? @conn options changed)))
        (let [original (view/current-view conn options)]
          (is (= #{"wiki/a.md"} (paths original)))
          (is (= "original" (get-in original [:documents 0 :source])))
          ;; Raw worktree mutations need no document-specific callback.
          (d/transact conn [[:db/retractEntity [:geschichte.work/path "wiki/a.md"]]])
          (is (not (view/current? @conn options original)))
          (is (empty? (paths (view/current-view conn options)))))))))

(deftest source-change-during-preparation-is-pending-not-stale-current
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "before")
      (let [read-content content/read-by-id
            raced? (atom false)
            result (with-redefs [content/read-by-id
                                 (fn [& args]
                                   (when (compare-and-set! raced? false true)
                                     (write! conn "wiki/a.md" "after"))
                                   (apply read-content args))]
                     (view/current-view conn options))]
        (is @raced?)
        (is (= {:status :pending :reason :source-changed} result))
        (is (= "after" (get-in (view/current-view conn options) [:documents 0 :source])))))))

(deftest portable-identities-are-strict-and-duplicates-never-upsert-together
  (with-repository
    (fn [conn]
      (let [id (random-uuid)
            source (str "---\nsimmis-id: '" id "'\n---\nbody")]
        (write! conn "wiki/a.md" source)
        (let [v (view/current-view conn options)]
          (is (= id (get-in v [:documents 0 :id])))
          (is (= :explicit (get-in v [:documents 0 :identity-status]))))
        (write! conn "wiki/b.md" source)
        (let [v (view/current-view conn options)]
          (is (= :invalid (:status v)))
          (is (nil? (:db v)))
          (is (contains? (set (map :kind (:diagnostics v))) :duplicate-document-id)))
        (write! conn "wiki/b.md" "---\nsimmis-id: not-a-uuid\n---\nbody")
        (let [v (view/current-view conn options)]
          (is (= :invalid (:status v)))
          (is (= :invalid-document-id (-> v :diagnostics first :kind)))
          (is (nil? (:db v))))))))

(deftest codec-schema-and-root-are-part-of-the-selection
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "text")
      (let [v (view/current-view conn options)]
        (is (not (view/current? @conn options (assoc-in v [:selection :codec :version] 2))))
        (is (not (view/current? @conn (assoc options :schema-revision :v2) v)))
        (with-redefs [markdown/syntax-profile (update markdown/syntax-profile :version inc)]
          (is (not (view/current? @conn options v))))
        (doseq [root ["../wiki" "/wiki" "wiki/" "a\\b" ".git"]]
          (is (thrown? clojure.lang.ExceptionInfo (view/selection @conn (assoc options :roots [root])))))
        (is (thrown? clojure.lang.ExceptionInfo (view/selection @conn {:roots ["wiki"]})))))))

(deftest repository-wide-markdown-selection-is-bounded-separately-from-code
  (with-repository
    (fn [conn]
      (doseq [path ["README.md" "wiki/a.md" "nested/deeper/B.MARKDOWN" "private/a.md"
                    "private-extra/visible.md"]]
        (write! conn path "# Knowledge"))
      ;; More code than the Markdown allowance is normal for a seeded room.
      (d/transact conn (mapv (fn [n] {:geschichte.work/path (str "src/" n ".clj")}) (range 210)))
      (let [all {:schema-revision :test-schema-v1}
            limited (assoc all :exclude-prefixes ["private"])
            selected (view/selection @conn limited)]
        (is (= #{"README.md" "wiki/a.md" "nested/deeper/B.MARKDOWN"
                 "private/a.md" "private-extra/visible.md"}
               (set (keys (:files (view/selection @conn all))))))
        (is (= #{"README.md" "wiki/a.md" "nested/deeper/B.MARKDOWN" "private-extra/visible.md"}
               (set (keys (:files selected)))))
        (is (= selected (view/selection @conn (assoc limited :roots ["" ""])))))
      (with-redefs-fn {(ns-resolve 'is.simm.documents.view 'max-scanned-entries) 10}
        #(is (= :too-many-worktree-entries
                (try (view/selection @conn {:schema-revision :test-schema-v1})
                     (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))

(deftest source-view-feeds-read-vocabulary-without-writing
  (with-repository
    (fn [conn]
      (write! conn "README.md" "# Current knowledge\nExact source.")
      (let [opts {:schema-revision :test-schema-v1}
            api (memory/bindings #(view/current-view conn opts))
            before (:max-tx @conn)
            result ((api 'read) "README.md")]
        (is (= :ready (:status result)))
        (is (= "# Current knowledge\nExact source." (get-in result [:document :source])))
        (is (= 20 (get-in ((api 'search) "Exact") [:matches 0 :from])))
        (is (= before (:max-tx @conn)))
        (write! conn "README.md" "---\nsimmis-id: invalid\n---\nInvalid")
        (let [invalid ((api 'documents))]
          (is (= :invalid (:status invalid)))
          (is (seq (:diagnostics invalid)))
          (is (not (contains? invalid :documents))))))))

(deftest malformed-metadata-never-becomes-an-empty-successful-kb
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "---\nvalue: [\n---\nbody")
      (let [v (view/current-view conn options)]
        (is (= :invalid (:status v)))
        (is (seq (:diagnostics v)))
        (is (nil? (:db v)))))))

(deftest colocated-prototype-rows-are-replaced-only-in-the-derived-db
  (with-repository
    (fn [conn]
      (documents/initialize! conn)
      (documents/create! conn {:id (random-uuid) :path "wiki/a.md" :source "# Old"})
      (write! conn "wiki/a.md" "## New")
      (let [v (view/current-view conn options)]
        (is (= #{[2]} (d/q '[:find ?n :where [_ :document.node/heading-level ?n]] (:db v))))
        (is (= #{[1]} (d/q '[:find ?n :where [_ :document.node/heading-level ?n]] @conn)))))))

(deftest explicit-file-identity-survives-a-raw-move
  (with-repository
    (fn [conn]
      (let [id (random-uuid)
            source (str "---\nsimmis-id: '" id "'\n---\n# Moved")]
        (write! conn "wiki/old.md" source)
        (let [before (view/current-view conn options)
              row (d/pull @conn '[*] [:geschichte.work/path "wiki/old.md"])]
          (d/transact conn [[:db/retractEntity (:db/id row)]
                            (-> row (dissoc :db/id) (assoc :geschichte.work/path "wiki/new.md"))])
          (is (not (view/current? @conn options before)))
          (let [after (view/current-view conn options)]
            (is (= #{"wiki/new.md"} (paths after)))
            (is (= id (get-in after [:documents 0 :id])))
            (is (= source (get-in after [:documents 0 :source])))))))))

(deftest resource-and-file-mode-refusals-occur-before-unbounded-content-work
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "# a")
      (let [read-content content/read-by-id
            reads (atom 0)]
        (with-redefs [content/read-by-id (fn [& args] (swap! reads inc) (apply read-content args))]
          (d/transact conn [{:geschichte.work/path "wiki/a.md" :geschichte.work/mode 40960}])
          (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options)))
          (is (zero? @reads))
          (d/transact conn [{:geschichte.work/path "wiki/a.md"
                             :geschichte.work/mode (long repo/default-mode)
                             :geschichte.work/size (inc markdown/max-source-bytes)}])
          (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options)))
          (is (zero? @reads)))))))

(deftest aggregate-node-refusal-stops-before-parsing-later-files
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "# a")
      (write! conn "wiki/b.md" "# b")
      (let [parse markdown/parse
            parses (atom 0)]
        (with-redefs-fn {(ns-resolve 'is.simm.documents.view 'max-total-nodes) 1
                        #'markdown/parse (fn [source] (swap! parses inc) (parse source))}
          #(is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options))))
        (is (= 1 @parses))))))

(deftest authored-facts-remain-queryable-and-references-to-disposable-rows-are-refused
  (with-repository
    (fn [conn]
      (documents/initialize! conn)
      (d/transact conn [{:db/ident :test/property :db/valueType :db.type/string
                         :db/cardinality :db.cardinality/one}
                        {:db/ident :test/ref :db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/one}])
      (let [id (random-uuid)]
        (documents/create! conn {:id id :path "wiki/a.md" :source "# Test"})
        (d/transact conn [{:db/id "authored" :test/property "Keep me"}])
        (let [before (:max-tx @conn)
              v (view/current-view conn options)]
          (is (= #{["Keep me"]} (d/q '[:find ?x :where [_ :test/property ?x]] (:db v))))
          (is (= before (:max-tx @conn))))
        (d/transact conn [{:test/property "Referencing" :test/ref [:document/id id]}])
        (let [before (:max-tx @conn)]
          (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options)))
          (is (= before (:max-tx @conn)))
          (is (= 1 (count (d/q '[:find ?e :where [?e :test/ref _]] @conn)))))))))

(deftest restore-and-hard-reset-reselect-without-an-application-callback
  (with-repository
    (fn [conn]
      (write! conn "wiki/a.md" "original")
      (repo/stage-all! conn)
      (let [commit (repo/commit! conn {:message "Original"})]
        (write! conn "wiki/a.md" "unstaged")
        (let [unstaged (view/current-view conn options)]
          (repo/restore-paths! conn ["wiki/a.md"] {:source :head})
          (is (not (view/current? @conn options unstaged)))
          (is (= "original" (get-in (view/current-view conn options) [:documents 0 :source]))))
        (write! conn "wiki/a.md" "committed change")
        (repo/stage-all! conn)
        (repo/commit! conn {:message "Changed"})
        (let [changed (view/current-view conn options)]
          (repo/reset! conn commit {:mode :hard})
          (is (not (view/current? @conn options changed)))
          (is (= "original" (get-in (view/current-view conn options) [:documents 0 :source]))))))))

(deftest orphan-nodes-and-unexpected-component-ownership-are-not-silently-reused
  (with-repository
    (fn [conn]
      (documents/initialize! conn)
      (d/transact conn [{:document.node/kind :Paragraph :document.node/from 0
                         :document.node/to 0 :document.node/ordinal 0}])
      (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options)))))
  (with-repository
    (fn [conn]
      (documents/initialize! conn)
      (d/transact conn [{:db/ident :document/source-content :db/isComponent true}])
      (is (thrown? clojure.lang.ExceptionInfo (view/current-view conn options))))))

(deftest semantic-group-identities-and-codec-are-part-of-the-selected-view
  (with-repository
    (fn [conn]
      (let [id (random-uuid)
            source (str "<!-- simmis:block " id " -->\n\nFirst.\n\nSecond.\n\n<!-- /simmis:block -->\n")]
        (write! conn "wiki/a.md" source)
        (let [v (view/current-view conn options)]
          (is (= :ready (:status v)))
          (is (= id (get-in v [:documents 0 :semantic :blocks 0 :id])))
          (is (= #{[id :explicit]}
                 (d/q '[:find ?id ?status :where [?e :document.block/id ?id]
                        [?e :document.block/identity-status ?status]] (:db v))))
          (is (= 1 (count (get-in v [:documents 0 :semantic :blocks]))))
          (with-redefs [blocks/profile (assoc blocks/profile :version 2)]
            (is (not (view/current? @conn options v)))))
        (write! conn "wiki/b.md" source)
        (let [v (view/current-view conn options)]
          (is (= :invalid (:status v)))
          (is (nil? (:db v)))
          (is (= #{:duplicate-semantic-id} (set (map :kind (:diagnostics v))))))))))

(deftest typed-properties-use-existing-categorical-schema-and-are-queryable
  (with-repository
    (fn [conn]
      (let [id (random-uuid)]
        (d/transact conn [{:db/ident :entity/uuid :db/valueType :db.type/uuid
                           :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
                          {:db/ident :entity/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                          {:db/ident :morphism/cardinality :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
                          {:db/ident :morphism/storage-attr :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}
                          {:db/ident :S.Project/staff :db/valueType :db.type/long :db/cardinality :db.cardinality/one}
                          {:entity/uuid id :entity/name "S/Project/staff" :morphism/cardinality :one}])
        (write! conn "wiki/a.md" "---\nstaff: 3\ncustom: retain\n---\n# Project")
        (let [opts (assoc options :property-bindings {"staff" id})
              tx (:max-tx @conn)
              v (view/current-view conn opts)]
          (is (= :ready (:status v)))
          (is (= #{["wiki/a.md" 3]}
                 (d/q '[:find ?path ?n :where [?e :document/path ?path] [?e :S.Project/staff ?n]] (:db v))))
          (is (empty? (d/q '[:find ?e :where [?e :S.Project/staff]] @conn)))
          (is (= tx (:max-tx @conn)))
          (is (= {"custom" "retain"} (get-in v [:documents 0 :properties :unknown])))
          ;; Actual morphism ownership changes invalidate the view even while
          ;; files AND the caller's schema revision label stay unchanged.
          (d/transact conn [{:entity/uuid id :morphism/storage-attr :S.Project/staff}])
          (is (not (view/current? @conn opts v)))
          (let [changed (view/current-view conn opts)]
            (is (= :invalid (:status changed)))
            (is (nil? (:db changed)))
            (is (= :owner-managed-property (-> changed :diagnostics first :kind)))))))))

(deftest semantic-block-hierarchy-is-queryable-without-confusing-internal-lists
  (with-repository
    (fn [conn]
      (let [parent (random-uuid) child (random-uuid)
            source (str "<!-- simmis:block " parent " -->\n\n- Internal list\n  - Internal item\n\n"
                        "<!-- simmis:block " child " -->\n\nChild\n\n<!-- /simmis:block -->\n\n"
                        "After\n\n<!-- /simmis:block -->\n")]
        (write! conn "wiki/a.md" source)
        (let [v (view/current-view conn options)]
          (is (= :ready (:status v)))
          (is (= #{[parent child 0]}
                 (d/q '[:find ?parent ?child ?order :where
                        [?p :document.block/id ?parent] [?p :document.block/children ?c]
                        [?c :document.block/id ?child] [?c :document.block/ordinal ?order]] (:db v))))
          (is (= 2 (count (d/q '[:find ?e :where [?e :document.block/id]] (:db v)))))
          (is (= 2 (count (d/q '[:find ?e :where [?e :document.node/kind :BulletList]] (:db v))))
              "Two nested Markdown lists are still internal content, not semantic children")
          (is (= source (get-in v [:documents 0 :source]))))))))
