(ns is.simm.ops.file-output-validation-test
  (:require [clojure.test :refer [deftest is]]
            [clj-yaml.core :as yaml]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.profile :as knowledge]
            [is.simm.documents.prospective-view-test :as fixture]
            [is.simm.documents.view :as view]
            [is.simm.ops.file-output-validation :as validation]
            [is.simm.ops.file-output-plan :as plans])
  (:import [java.util Base64]))

(defn- with-repository [f] (@#'fixture/with-repository f))
(defn- text [defs id values body] (@#'fixture/source defs id values body))
(defn- write! [conn path source] (repo/write! conn path (markdown/source-bytes source)))
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))

(defn- descriptor [source]
  (when (some? source)
    (let [bytes (markdown/source-bytes source)]
      {:content (blob/blob-id bytes) :bytes (alength bytes) :mode 33188
       :base64 (.encodeToString (Base64/getEncoder) bytes)})))

(defn- plan [db before after]
  (let [bodies (into (sorted-map) (for [[path source] after]
                                    [path {:before (descriptor (get before path)) :after (descriptor source)}]))
        manifest {:version 1 :request (random-uuid) :room (random-uuid) :attempt (random-uuid)
                  :environment (random-uuid) :workflow (random-uuid) :basis (random-uuid) :output (random-uuid)
                  :target {:repository (:geschichte.repo/id (query/repository db))
                           :store (get-in db [:config :store :id]) :backend (get-in db [:config :store :backend])
                           :branch (get-in db [:config :branch]) :ref (query/current-ref db)
                           :registry-system (random-uuid) :runtime-system "fixed-test-source"}
                  :entries (into (sorted-map)
                                 (for [[path pair] bodies]
                                   [path {:before (if-let [old (:before pair)]
                                                    (assoc (dissoc old :base64) :state :present) {:state :absent})
                                          :after (dissoc (:after pair) :base64)}]))}
        id (hasch/uuid [:simmis/knowledge-file-manifest-v1 manifest])]
    {:manifest (assoc manifest :id id) :proposal (hasch/uuid [:simmis/knowledge-file-proposal-v1 id]) :bodies bodies}))

(deftest mixed-canonical-updates-freeze-exact-source-and-definitions-not-unrelated-files
  (with-repository
    (fn [conn defs]
      (let [id (random-uuid) context-id (random-uuid) new-id (random-uuid)
            before (text defs id {"rank" 0 "unknown" {"keep" [false nil 0]}} "Original")
            after (text defs id {"rank" 1 "unknown" {"keep" [false nil 0]}} "Updated")
            context (text defs context-id {"rank" 2} "Context")
            changed-context (text defs context-id {"rank" 3} "Changed context")
            created (text defs new-id {"rank" 4} "New note")]
        (write! conn "wiki/original.md" before)
        (write! conn "wiki/context.md" context)
        (write! conn "scratch/context.txt" changed-context)
        (let [base @conn tx (:max-tx base)
              proposed (plan base {"wiki/original.md" before}
                             {"wiki/original.md" after "wiki/new.md" created})
              witness (validation/prepare-in base proposed)]
          (is (= tx (:max-tx @conn)))
          (is (= validation/profile (:profile witness)))
          (is (every? #(not (contains? witness %)) [:db :conn :bodies :source :callback]))
          (is (failure #(validation/assert-after! base witness)) "BEFORE cannot masquerade as published AFTER")
          (doseq [[path source] {"wiki/original.md" after "wiki/new.md" created}] (write! conn path source))
          (let [published @conn spare (get (repo/worktree conn) "scratch/context.txt")
                changed (d/db-with published [{:geschichte.work/path "wiki/context.md"
                                               :geschichte.work/content [:geschichte.content/id (:content spare)]
                                               :geschichte.work/size (:size spare)}])
                schema-changed (d/db-with published [{:entity/uuid (:rank defs) :morphism/optional? true}])]
            (is (true? (validation/assert-after! published witness)))
            (is (= :ready (:status (view/prepare changed knowledge/selection-options))))
            (is (failure #(validation/assert-after! changed witness)) "valid changed context is still not the frozen preflight")
            (is (= :ready (:status (view/prepare schema-changed knowledge/selection-options))))
            (is (failure #(validation/assert-after! schema-changed witness)) "valid changed definitions are not the frozen preflight")
            (doseq [bad [nil (dissoc witness :typed-scopes) (assoc witness :typed-scopes [])
                         (assoc witness :constraint-dependencies {}) (assoc witness :after {})
                         (assoc-in witness [:profile :version] 2) (assoc witness :extra true)
                         (assoc-in witness [:selection :roots] ["wiki"])
                         (assoc-in witness [:target :store] (random-uuid))]]
              (is (failure #(validation/assert-after! published bad))))
            (let [mode-changed (d/db-with published [{:geschichte.work/path "wiki/new.md" :geschichte.work/mode (long 33261)}])]
              (is (failure #(validation/assert-after! mode-changed witness))))
            (let [different-branch (assoc-in published [:config :branch] :other)
                  different-ref (d/db-with published [{:db/id (:db/id (query/repository published))
                                                       :geschichte.repo/head "refs/heads/other"}])]
              (doseq [changed [different-branch different-ref]]
                (is (= :target-changed (:reason (ex-data (failure #(validation/assert-after! changed witness))))))))
            (write! conn "scratch/unrelated.clj" ";; no semantic wiki change\n")
            (is (true? (validation/assert-after! @conn witness)))
            (write! conn "wiki/extra.md" (text defs (random-uuid) {"rank" 5} "New unrelated knowledge"))
            (is (= :ready (:status (view/prepare @conn knowledge/selection-options))))
            (is (failure #(validation/assert-after! @conn witness)))))))))

(deftest preserved-metadata-and-historical-before-bodies-are-independent-gates
  (with-repository
    (fn [conn defs]
      (let [id (random-uuid) before (text defs id {"rank" 0 "unknown" false} "Original")
            valid (text defs id {"rank" 1 "unknown" false} "Update")
            missing (text defs id {"rank" 1} "Missing unknown metadata")]
        (write! conn "wiki/original.md" before)
        (let [base @conn good (plan base {"wiki/original.md" before} {"wiki/original.md" valid})]
          (is (some? (validation/prepare-in base good)))
          (with-redefs-fn {(ns-resolve 'is.simm.ops.file-output-validation 'max-preservation-nodes) 1}
            #(is (= :too-much-preservation-work
                    (:reason (ex-data (failure (fn [] (validation/prepare-in base good))))))))
          (is (= :metadata-not-preserved
                 (:reason (ex-data (failure #(validation/prepare-in base
                                                                    (plan base {"wiki/original.md" before} {"wiki/original.md" missing})))))))
          (is (failure #(validation/prepare-in base
                                               (plan base {"wiki/original.md" (str before "wrong baseline")}
                                                     {"wiki/original.md" valid}))))
          (is (failure #(validation/prepare-in base (assoc-in good [:bodies "wiki/original.md" :before :base64] ""))))
          (let [noop (validation/prepare-in base (plan base {"wiki/original.md" before} {"wiki/original.md" before}))]
            (is (true? (validation/assert-after! base noop)))))))))

(deftest external-relation-targets-and-removed-source-entries-cannot-change-under-validation
  (with-repository
    (fn [conn defs]
      (let [id (random-uuid) external (random-uuid)
            before (text defs id {"rank" 0 "peer" {"id" (str external)}} "Original")
            after (text defs id {"rank" 1 "peer" {"id" (str external)}} "Updated")]
        (d/transact conn [{:entity/uuid external :instance/of-role [[:entity/uuid (:type defs)]]}])
        (write! conn "wiki/original.md" before)
        (let [witness (validation/prepare-in @conn (plan @conn {"wiki/original.md" before} {"wiki/original.md" after}))]
          (write! conn "wiki/original.md" after)
          (let [published @conn
                removed (d/db-with published [[:db/retractEntity [:geschichte.work/path "wiki/original.md"]]])
                wrong-role (d/db-with published [[:db/retract [:entity/uuid external] :instance/of-role [:entity/uuid (:type defs)]]])]
            (is (true? (validation/assert-after! published witness)))
            (is (failure #(validation/assert-after! removed witness)))
            (is (failure #(validation/assert-after! wrong-role witness)))))))))

(deftest block-metadata-preservation-is-enforced-after-canonical-validity
  (with-repository
    (fn [conn defs]
      (let [id (random-uuid) block (random-uuid)
            raw (get-in (markdown/parse (text defs id {"rank" 0} "Page")) [:frontmatter :properties])
            raw (assoc-in raw ["simmis" "blocks" (str block)]
                          {"types" [(str (:type defs))]
                           "properties" {"rank" {"id" (str (:rank defs)) "type" (str (:type defs))}}
                           "values" {"rank" 0 "unknown" nil}})
            body (str "<!-- simmis:block " block " -->\n\nKeep block\n\n<!-- /simmis:block -->\n")
            source #(str "---\n" (yaml/generate-string %) "---\n" body)
            before (source raw)
            after (source (update-in raw ["simmis" "blocks" (str block) "values"] dissoc "unknown"))]
        (write! conn "wiki/blocks.md" before)
        (let [proposed (plan @conn {"wiki/blocks.md" before} {"wiki/blocks.md" after})]
          (is (= :metadata-not-preserved
                 (:reason (ex-data (failure #(validation/prepare-in @conn proposed)))))))))))

(deftest scoped-root-publication-keeps-canonical-and-after-guards
  (with-repository
    (fn [conn defs]
      (let [id (random-uuid)
            before (text defs id {"rank" 0 "unknown" false} "Before")
            after (text defs id {"rank" 1 "unknown" false} "After")
            output-scope {:version 2 :output-root "out/knowledge" :destination-root ""
                          :exclude-prefixes [".obsidian" "inbox"]}]
        (write! conn "Topic.md" before)
        (let [base @conn unsealed (plan base {"Topic.md" before} {"Topic.md" after})
              manifest (-> (:manifest unsealed) (dissoc :id) (assoc :version 2 :scope output-scope))
              id (hasch/uuid [(plans/manifest-tag 2) manifest])
              proposed (assoc unsealed :manifest (assoc manifest :id id)
                              :proposal (hasch/uuid [(plans/proposal-tag 2) id]))
              witness (validation/prepare-in base proposed)]
          (is (= output-scope (:scope witness)))
          (is (= 2 (get-in witness [:profile :version])))
          (is (failure #(validation/assert-after! base witness)))
          (write! conn "Topic.md" after)
          (is (true? (validation/assert-after! @conn witness)))
          (is (failure #(validation/assert-after! @conn (dissoc witness :scope))))
          (is (failure #(validation/assert-after! @conn (assoc-in witness [:scope :destination-root] "Elsewhere")))))))))
