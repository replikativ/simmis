(ns is.simm.documents.prospective-view-test
  (:require [clojure.test :refer [deftest is testing]]
            [clj-yaml.core :as yaml]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.preservation :as preservation]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]
            [is.simm.model.crud :as crud]
            [kontor.governance :as governance]))

(def options {:roots [""] :schema-revision :prospective-test-v1 :entity-profile space/profile})

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
             :attribute-refs? false :keep-history? false :max-string-length 0
             :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (space/provision! conn)
        (let [type (crud/create-type! conn {:name "ProspectiveNote"})
              rank (crud/create-morphism! conn {:name "S/ProspectiveNote/rank"
                                                :src "S/ProspectiveNote" :dst "S/Number" :optional? false})
              peer (crud/create-morphism! conn {:name "S/ProspectiveNote/peer"
                                                :src "S/ProspectiveNote" :dst "S/ProspectiveNote"})
              slug (crud/create-morphism! conn {:name "S/ProspectiveNote/slug"
                                                :src "S/ProspectiveNote" :dst "S/String" :unique :db.unique/value})]
          (f conn {:type type :rank rank :peer peer :slug slug}))
        (finally (governance/ungovern! conn) (d/release conn))))))

(defn- source [{:keys [type rank peer slug]} id values body]
  (str "---\n"
       (yaml/generate-string
        (merge {"simmis-id" (str id)
                "simmis" {"version" 1 "types" [(str type)]
                          "properties" (into {} (for [[alias property] {"rank" rank "peer" peer "slug" slug}]
                                                  [alias {"id" (str property) "type" (str type)}]))}}
               values))
       "---\n" body))

(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))

(deftest complete-prospective-selection-validates-cross-file-relations-and-untouched-facts
  (with-repository
    (fn [conn defs]
      (let [old-id (random-uuid) a (random-uuid) b (random-uuid)
            old (source defs old-id {"rank" 0 "slug" "existing"} "# Existing\n")
            first (source defs a {"rank" 1 "slug" "first" "peer" {"id" (str b)}} "# First\n")
            second (source defs b {"rank" 2 "slug" "second" "peer" {"id" (str a)}} "# Second\n")]
        (write! conn "wiki/old.md" old)
        (let [db @conn tx (:max-tx db) before (repo/worktree conn)
              changes {"wiki/a.md" {:expected nil :source first}
                       "wiki/b.md" {:expected nil :source second}}
              preview (view/validate-changes db options changes)]
          (is (= :valid (:status preview)) (pr-str (:diagnostics preview)))
          (is (= :changes-preview (:kind preview)))
          (is (= #{"wiki/a.md" "wiki/b.md"} (set (keys (:changes preview)))))
          (is (= #{} (get-in preview [:basis :constraint-dependencies :external-targets])))
          (is (= #{"wiki/old.md"} (set (keys (get-in preview [:basis :selection :files])))))
          (is (every? #(not (contains? preview %)) [:db :documents :source :publication]))
          (is (not (view/current? db options (assoc preview :selection (get-in preview [:basis :selection])))))
          (is (= tx (:max-tx @conn)))
          (is (= before (repo/worktree conn)))
          (is (nil? (:db/id (d/entity @conn [:geschichte.content/id (blob/blob-id (markdown/source-bytes first))]))))
          (testing "untouched documents participate in identity and uniqueness checks"
            (doseq [bad [(source defs old-id {"rank" 3} "Duplicate identity")
                         (source defs a {"rank" 3 "slug" "existing"} "Duplicate unique")
                         (source defs a {} "Missing required rank")]]
              (is (= :invalid (:status (view/validate-changes db options {"wiki/new.md" {:expected nil :source bad}}))))))
          (testing "the exact immutable source and real definitions determine the preview"
            (let [changed (d/db-with db [{:entity/uuid (:rank defs) :morphism/validation "inert; never execute"}])]
              (is (= :invalid (:status (view/validate-changes changed options changes))))
              (is (= :valid (:status (view/validate-changes db options changes))))))
          (testing "selected existing files require exact metadata, never boolean or partial expectations"
            (let [expected (get before "wiki/old.md")
                  replacement (source defs old-id {"rank" 4 "slug" "existing"} "Updated")]
              (is (= :valid (:status (view/validate-changes db options
                                                            {"wiki/old.md" {:expected expected :source replacement}}))))
              (is (= :valid (:status (view/validate-changes db options
                                                            (assoc changes "wiki/old.md" {:expected expected :source replacement})))))
              (doseq [bad [nil false {} (assoc expected :size (inc (:size expected)))]]
                (is (failure #(view/validate-changes db options {"wiki/old.md" {:expected bad :source replacement}}))))
              (let [corrupt (d/db-with db [{:geschichte.content/id (:content expected)
                                            :geschichte.content/size (inc (:size expected))}])
                    single {:path "wiki/old.md" :expected expected :source replacement}]
                (doseq [preview! [#(view/validate-replacement corrupt options single)
                                  #(view/validate-changes corrupt options {"wiki/old.md" (dissoc single :path)})]]
                  (is (= :content-size-mismatch (:reason (ex-data (failure preview!)))))))))
          (testing "aggregate limits apply to proposed creations too"
            (with-redefs-fn {(ns-resolve 'is.simm.documents.view 'max-files) 1}
              #(is (failure (fn [] (view/validate-changes db options changes)))))
            (with-redefs-fn {(ns-resolve 'is.simm.documents.view 'max-total-bytes) 256}
              #(is (failure (fn [] (view/validate-changes db options changes))))))
          (doseq [bad [nil {} [] {"wiki/a.md" {:expected nil :source first :extra true}}
                       {"wiki/a.txt" {:expected nil :source first}}]]
            (is (failure #(view/validate-changes db options bad))))
          (is (failure #(view/validate-changes db (assoc options :roots ["other"]) changes)))
          (is (failure #(view/validate-changes db (assoc options :exclude-prefixes ["wiki"]) changes)))
          (is (failure #(view/validate-changes db (dissoc options :entity-profile) changes))))))))

(deftest creations-use-complete-occupancy-and-replacements-preserve-explicit-identities
  (with-repository
    (fn [conn defs]
      (let [id (random-uuid) block (random-uuid)
            body (str "<!-- simmis:block " block " -->\n\nKeep identity\n\n<!-- /simmis:block -->\n")
            text (source defs id {"rank" 0} body)
            candidate (source defs (random-uuid) {"rank" 1} "New")]
        (write! conn "wiki/original.md" text)
        (write! conn "wiki/file.md" (source defs (random-uuid) {"rank" 1} "Occupied"))
        (write! conn "wiki/implicit.md/child.txt" "nested")
        (d/transact conn [{:geschichte.work-dir/path "wiki/empty.md"}])
        (let [db @conn expected (get (repo/worktree conn) "wiki/original.md")]
          (doseq [path ["wiki/file.md" "wiki/file.md/child.md" "wiki/implicit.md" "wiki/empty.md"]]
            (is (failure #(view/validate-changes db options {path {:expected nil :source candidate}}))))
          (is (failure #(view/validate-changes db options
                                               {"new.md" {:expected nil :source candidate}
                                                "new.md/child.md" {:expected nil :source candidate}})))
          (let [changed (view/validate-changes db options
                                               {"wiki/original.md" {:expected expected :source (source defs (random-uuid) {"rank" 2} body)}})
                removed (view/validate-changes db options
                                               {"wiki/original.md" {:expected expected :source (source defs id {"rank" 2} "Block removed")}})]
            (is (= :invalid (:status changed)))
            (is (some #(= :changed-document-identity (:kind %)) (:diagnostics changed)))
            (is (= :invalid (:status removed)))
            (is (some #(= :removed-explicit-block-identity (:kind %)) (:diagnostics removed)))))))))

(deftest conservative-preservation-does-not-replace-real-canonical-validation
  (with-repository
    (fn [conn defs]
      (let [id (random-uuid) text (source defs id {"rank" 0} "Original")
            before (markdown/parse text)
            raw (get-in before [:frontmatter :properties])]
        (write! conn "wiki/original.md" text)
        (let [db @conn expected (get (repo/worktree conn) "wiki/original.md") tx (:max-tx db)]
          (doseq [candidate [(assoc raw "rank" false)
                             (update-in raw ["simmis" "types"] conj (str (random-uuid)))]]
            (let [after (markdown/parse (str "---\n" (yaml/generate-string candidate) "---\nChanged"))]
              (is (= :valid (:status (preservation/check {:before before :after after}))))
              (is (= :invalid (:status (view/validate-changes db options
                                                              {"wiki/original.md" {:expected expected :source (:source after)}}))))))
          (is (= tx (:max-tx @conn))))))))
