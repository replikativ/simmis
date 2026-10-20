(ns is.simm.ops.knowledge-inputs-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as store]
            [dvergr.substrate.geschichte :as geschichte]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.files :as document-files]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-admission :as knowledge-admission]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.workflow-runs :as workflow-runs]
            [is.simm.ops.workflow-runs-test :as workflow-fixture]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- with-repository [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true :branch-history? true
             :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (repo/init! conn {:name "Knowledge workflow input tests"}) (f conn)
           (finally (d/release conn))))))

(defn- write! [conn path text] (repo/write! conn path (markdown/source-bytes text)))

(deftest publication-basis-is-complete-occupancy-not-selected-model-inputs
  (with-repository
    (fn [conn]
      (write! conn "inbox/evidence.md" "Evidence")
      (write! conn "wiki/private/secret.md" "Excluded from model inputs")
      (write! conn "wiki/config.edn" "{:private true}")
      (write! conn "wiki/public.md" "Visible context")
      (d/transact conn [{:geschichte.work-dir/path "wiki/empty.md"}])
      (let [db @conn policy (assoc inputs/default-policy :exclude-prefixes ["wiki/private"])
            selected (inputs/selection db policy)
            basis (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Metadata-only admission")))]
                    (inputs/publication-basis db))]
        (is (= ["inbox/evidence.md" "wiki/public.md"] (mapv :path (get-in selected [:manifest :files]))))
        (is (= #{"wiki/private/secret.md" "wiki/config.edn" "wiki/public.md"}
               (set (keys (:files basis)))))
        (is (= {:present? false} (inputs/publication-before basis "wiki/new.md")))
        (is (= true (:present? (inputs/publication-before basis "wiki/private/secret.md"))))
        (is (thrown? clojure.lang.ExceptionInfo (inputs/publication-before basis "wiki/empty.md")))
        (is (thrown? clojure.lang.ExceptionInfo (inputs/publication-before basis "wiki/config.edn")))
        (is (thrown? clojure.lang.ExceptionInfo (inputs/publication-before basis "out/wiki/new.md")))
        (is (= basis (inputs/publication-basis db)))))))

(deftest outside-inclusive-endpoints-do-not-enter-publication-occupancy-or-its-cap
  (with-repository
    (fn [conn]
      (write! conn "seed.md" "Seed")
      (let [db @conn
            source (document-files/entry db "seed.md")
            row (fn [path] {:geschichte.work/path path :geschichte.work/size (:size source)
                            :geschichte.work/mode (:mode source)
                            :geschichte.work/content [:geschichte.content/id (:content source)]})
            valid (:db-after (d/with db [(row "wiki/a.md") (row "wiki/b.edn")
                                         {:geschichte.work-dir/path "wiki"}
                                         {:geschichte.work-dir/path "wiki/empty"}]))
            siblings (:db-after (d/with valid [{:geschichte.work/path "wiki0"}
                                               {:geschichte.work-dir/path "wiki0"}]))
            overflow (:db-after (d/with siblings [{:geschichte.work-dir/path "wiki/other"}]))]
        (is (= "wiki0" (:v (last (d/index-range siblings {:attrid :geschichte.work/path
                                                          :start "wiki/" :end "wiki0"})))))
        (is (= "wiki0" (:v (last (d/index-range siblings {:attrid :geschichte.work-dir/path
                                                          :start "wiki/" :end "wiki0"})))))
        (with-redefs [document-files/max-index-entries 4
                      content/read-by-id (fn [& _] (throw (AssertionError. "Occupancy is metadata only")))]
          (is (= (inputs/publication-basis valid) (inputs/publication-basis siblings)))
          ;; A raw global take5 followed by filtering the two endpoints could
          ;; hide this fifth REAL row. Bound/filter each prefix first instead.
          (is (= :too-many-publication-entries
                 (:reason (ex-data (try (inputs/publication-basis overflow)
                                        (catch clojure.lang.ExceptionInfo e e)))))))))))

(deftest publication-before-never-refreshes-absence-after-a-human-edit
  (with-repository
    (fn [conn]
      (let [db @conn basis (inputs/publication-basis db)]
        (write! conn "wiki/new.md" "Human created this after admission")
        (is (= {:present? false} (inputs/publication-before basis "wiki/new.md")))
        (is (:present? (inputs/publication-before (inputs/publication-basis @conn) "wiki/new.md")))
        (is (= basis (inputs/publication-basis db)))
        (is (not= (:id basis) (:id (inputs/publication-basis @conn))))))))

(deftest publication-basis-refuses-files-and-symlinks-as-parent-directories
  (with-repository
    (fn [conn]
      (write! conn "wiki/link" "symlink target")
      (d/transact conn [[:db/add [:geschichte.work/path "wiki/link"] :geschichte.work/mode (long 40960)]])
      (let [basis (inputs/publication-basis @conn)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (inputs/publication-before basis "wiki/link/new.md"))))
      (write! conn "wiki" "Root occupied by a file")
      (is (thrown? clojure.lang.ExceptionInfo
                   (inputs/publication-before (inputs/publication-basis @conn) "wiki/new.md"))))))

(deftest publication-basis-binds-the-physical-workspace-and-logical-ref
  (with-repository
    (fn [conn]
      (let [db @conn basis (inputs/publication-basis db)]
        (is (= (get-in db [:config :store :id]) (get-in basis [:target :store])))
        (doseq [[path value] [[[:config :store :id] (random-uuid)]
                              [[:config :store :backend] :other-backend]
                              [[:config :branch] :other-physical-branch]]]
          (is (not= (:id basis) (:id (inputs/publication-basis (assoc-in db path value))))))
        (d/transact conn [[:db/add [:geschichte.repo/id (get-in basis [:target :repository])]
                           :geschichte.repo/head "refs/heads/other"]])
        (is (not= (:id basis) (:id (inputs/publication-basis @conn))))))))

(deftest publication-basis-bounds-derived-directory-amplification-and-serialized-size
  (with-repository
    (fn [conn]
      (write! conn "wiki/a/b/c/d/e/f/g/h.md" "One source file")
      (let [db @conn]
        (is (= :too-many-publication-entries
               (:reason (ex-data
                         (try
                           (with-redefs [document-files/max-index-entries 5]
                             (inputs/publication-basis db))
                           (catch clojure.lang.ExceptionInfo error error))))))
        (is (= :publication-basis-too-large
               (:reason (ex-data
                         (try
                           (with-redefs [inputs/max-publication-basis-bytes 1]
                             (inputs/publication-basis db))
                           (catch clojure.lang.ExceptionInfo error error))))))))))

(deftest frozen-inputs-are-directly-reusable-by-ordinary-workflow-preparation
  (with-repository
    (fn [conn]
      (write! conn "inbox/meeting.md" "# Meeting\nDecision: ship.")
      (write! conn "wiki/company.md" "# Company\nExisting memory.")
      (write! conn "out/earlier.md" "Not incoming evidence")
      (write! conn "workflows/example/README.md" "Task data, not source evidence")
      (let [db @conn before (:max-tx db)
            frozen (inputs/capture db inputs/default-policy)
            entries (:inputs frozen)
            manifest (edn/read-string (get entries "/in/knowledge/manifest.edn"))
            selection (assoc (select-keys (workflow-fixture/input) workflow-runs/source-fields)
                             :inputs entries)
            prepared (workflow-runs/prepare-source (workflow-fixture/snapshot workflow-fixture/source) selection)]
        (is (= before (:max-tx @conn)))
        (is (= frozen (inputs/verify-capture inputs/default-policy frozen)))
        (is (= frozen (inputs/from-inputs inputs/default-policy entries)))
        (is (thrown? clojure.lang.ExceptionInfo
                     (inputs/verify-capture inputs/default-policy
                                            (assoc-in frozen [:inputs "/in/knowledge/sources/inbox/meeting.md"] "Forged"))))
        (is (= #{:trigger :context} (set (map :role (:files manifest)))))
        (is (= 3 (count entries)))
        (is (= "# Meeting\nDecision: ship." (get entries "/in/knowledge/sources/inbox/meeting.md")))
        (is (= entries (select-keys (:inputs prepared) (keys entries))))
        (write! conn "inbox/meeting.md" "Later source edit")
        (is (= frozen (inputs/capture db inputs/default-policy)) "Old immutable snapshot captures the old bytes")
        (is (not= (:trigger-id frozen) (:trigger-id (inputs/capture @conn inputs/default-policy))))
        (is (= "# Meeting\nDecision: ship."
               (get-in prepared [:inputs "/in/knowledge/sources/inbox/meeting.md"])))))))

(deftest declared-workflows-require-explicit-frozen-evidence-before-pure-preparation
  (with-repository
    (fn [conn]
      (write! conn "inbox/a.md" "Frozen evidence")
      (let [captured (inputs/capture @conn inputs/default-policy)
            source (assoc workflow-fixture/source "input-selection.edn"
                          (pr-str {:kind :knowledge :policy inputs/default-policy}))
            bundle (dvergr.catalog.room/bundle "reusable" source)
            request {:name "reusable" :expected-bundle (:id bundle)
                     :params {"subject" "test"}}
            snapshot (workflow-fixture/snapshot source)]
        (is (thrown? clojure.lang.ExceptionInfo (workflow-runs/prepare-source snapshot request)))
        (let [prepared (workflow-runs/prepare-source snapshot request captured)]
          (is (= (:trigger-id captured) (get-in prepared [:workflow :input-selection :trigger-id])))
          (is (= "Frozen evidence" (get-in prepared [:inputs "/in/knowledge/sources/inbox/a.md"]))))
        (let [frozen-request (assoc request :inputs (:inputs captured))
              prepared (workflow-runs/prepare-source snapshot frozen-request)]
          (is (= (:execution-id captured) (get-in prepared [:workflow :input-selection :execution-id])))
          (is (thrown? clojure.lang.ExceptionInfo
                       (workflow-runs/prepare-source snapshot frozen-request captured))))))))

(deftest host-resolution-freezes-once-and-setup-replaces-owned-staging-only
  (let [room (discourse/make-room {:id (keyword (str "knowledge-staging-" (random-uuid))) :store (store/make)})]
    (try
      (workspace/ensure-workspace! room)
      (workspace/seed! room {"/inbox/a.md" "Evidence at admission"
                             "/in/knowledge/sources/stale.md" "Not admitted"
                             "/unrelated.txt" "Keep this"})
      (let [source (assoc workflow-fixture/source "input-selection.edn"
                          (pr-str {:kind :knowledge :policy inputs/default-policy}))
            snapshot (workflow-fixture/snapshot source)
            request {:name "reusable" :expected-bundle (:id (dvergr.catalog.room/bundle "reusable" source))}
            captured (workflow-runs/resolve-source room snapshot request)
            prepared (workflow-runs/prepare-source snapshot request captured)]
        (binding [ec/*execution-context* (:ctx room)]
          (repo/write! (:conn (geschichte/current-workspace)) "inbox/a.md" (.getBytes "Later change" "UTF-8")))
        ((:prepare (:world-setup prepared)) {:room room})
        (is (= "Evidence at admission"
               (:text (document-files/read-file room "in/knowledge/sources/inbox/a.md"))))
        (is (nil? (get-in (document-files/metadata room {:folders [] :paths ["in/knowledge/sources/stale.md"]})
                          [:entries "in/knowledge/sources/stale.md"])))
        (is (= "Keep this" (:text (document-files/read-file room "unrelated.txt")))))
      (finally (discourse/close-room! room)))))

(deftest publication-basis-binds-explicit-default-identity-without-upgrading-legacy-bases
  (with-repository
    (fn [conn]
      (let [db @conn identity {:registry-system (random-uuid) :runtime-system "owned-runtime"}
            legacy (inputs/publication-basis db)
            basis (inputs/publication-basis db identity)]
        (is (= identity (select-keys (:target basis) (keys identity))))
        (is (not= (:id legacy) (:id basis)))
        (is (not (contains? (:target legacy) :registry-system)))
        (is (= legacy (inputs/publication-basis db)))
        (doseq [bad [{:registry-system (:registry-system identity)}
                     {:runtime-system "owned-runtime"}
                     (assoc identity :runtime-system " ")
                     (assoc identity :unexpected true)]]
          (is (thrown? clojure.lang.ExceptionInfo (inputs/publication-basis db bad))))))))

(deftest admission-binds-complete-occupancy-to-the-same-frozen-db
  (let [room (discourse/make-room {:id (keyword (str "publication-admission-" (random-uuid)))
                                   :store (store/make)})]
    (try
      (workspace/ensure-workspace! room)
      (workspace/seed! room {"/inbox/a.md" "Original evidence" "/wiki/a.md" "Original wiki"})
      (let [room-id (random-uuid)
            source (assoc workflow-fixture/source "input-selection.edn"
                          (pr-str {:kind :knowledge :policy inputs/default-policy}))
            snapshot (workflow-fixture/snapshot source)
            request {:name "reusable" :expected-bundle (:id (dvergr.catalog.room/bundle "reusable" source))}
            resolution
            (with-redefs [knowledge-admission/default-target workflow-fixture/fixture-default-target]
              (workflow-runs/with-admission-selection
                room-id room snapshot request
                (fn [_ resolve]
               ;; The live workspace advances after selection but before bodies
               ;; and complete occupancy are captured. Both must use the old DB.
                  (binding [ec/*execution-context* (:ctx room)]
                    (let [conn (:conn (geschichte/current-workspace))]
                      (write! conn "inbox/a.md" "Later evidence")
                      (write! conn "wiki/new.md" "Human work")))
                  (resolve))))
            captured (:captured resolution)
            basis (:publication-basis resolution)
            setup (evaluation/make-world-setup
                   {:id :fixture/binding :prepare (fn [_] (throw (AssertionError. "Admission executed setup")))})
            evaluator (evaluation/make-evaluator
                       {:id :fixture/binding :observe identity
                        :verify (fn [_ _] (throw (AssertionError. "Admission executed verifier")))})
            env (environment/make-environment {:id :fixture/binding :task :binding
                                               :verifier {:id :fixture/binding}})
            prepared {:environment env :world-setup setup :evaluator evaluator
                      :basis {:environment env
                              :workflow {:input-selection {:execution-id (:execution-id captured)}}}}]
        (is (= "Original evidence" (get-in captured [:inputs "/in/knowledge/sources/inbox/a.md"])))
        (is (= {:present? false} (inputs/publication-before basis "wiki/new.md")))
        (is (contains? (:files basis) "wiki/a.md"))
        (is (= basis (get-in (workflow-runs/bind-publication prepared resolution)
                             [:basis :knowledge/publication-basis])))
        (is (thrown? clojure.lang.ExceptionInfo
                     (workflow-runs/bind-publication {:basis {}} resolution)))
        (let [fixtures (assoc request :inputs (:inputs captured))
              unbound (workflow-runs/resolve-admission room-id room snapshot fixtures)]
          (is (= {:captured nil :publication-basis nil} unbound))
          (is (= prepared (workflow-runs/bind-publication prepared unbound)))))
      (finally (discourse/close-room! room)))))

(deftest knowledge-output-and-staging-edits-do-not-retrigger-maintenance
  (with-repository
    (fn [conn]
      (write! conn "transcripts/call.md" "Evidence")
      (write! conn "wiki/a.md" "Prior")
      (let [initial (inputs/capture @conn inputs/default-policy)]
        (is (false? (:empty? initial)))
        (write! conn "wiki/a.md" "Adopted output")
        (let [next (inputs/capture @conn inputs/default-policy)]
          (is (= (:trigger-id initial) (:trigger-id next)))
          (is (not= (:execution-id initial) (:execution-id next))))
        (let [before (inputs/capture @conn inputs/default-policy)]
          (write! conn "out/wiki/a.md" "Proposal output")
          (write! conn "in/knowledge/sources/example.md" "Staging")
          (is (= before (inputs/capture @conn inputs/default-policy)))))))
  (with-repository #(is (:empty? (inputs/capture @% inputs/default-policy)))))

(deftest benchmark-fixture-setup-also-replaces-the-owned-input-namespace
  (with-repository
    (fn [conn]
      (write! conn "inbox/a.md" "Case evidence")
      (let [capture (inputs/capture @conn inputs/default-policy)
            source (merge (dissoc workflow-fixture/source "calibration.edn")
                          {"input-selection.edn" (pr-str {:kind :knowledge :policy inputs/default-policy})}
                          (into {} (map (fn [[path text]] [(str "fixtures" path) text])) (:inputs capture)))
            bundle (dvergr.catalog.room/bundle "knowledge-case" source)
            ;; The next upstream catalog shape omits :files; policy must come
            ;; from the actual pinned source snapshot, not this derived map.
            setup (checkers/world-setup (dissoc bundle :files) (inputs/declaration source))
            room (discourse/make-room {:id (keyword (str "knowledge-case-" (random-uuid))) :store (store/make)})
            fresh (discourse/make-room {:id (keyword (str "knowledge-fresh-case-" (random-uuid))) :store (store/make)})]
        (try
          (workspace/ensure-workspace! room)
          (workspace/seed! room {"/in/knowledge/sources/not-in-case.md" "Stale"
                                 "/unrelated.txt" "Keep"})
          ((:prepare setup) {:room room :environment {}})
          (is (= "Case evidence" (:text (document-files/read-file room "in/knowledge/sources/inbox/a.md"))))
          (is (nil? (get-in (document-files/metadata room {:folders [] :paths ["in/knowledge/sources/not-in-case.md"]})
                            [:entries "in/knowledge/sources/not-in-case.md"])))
          (is (= "Keep" (:text (document-files/read-file room "unrelated.txt"))))
          ((:prepare setup) {:room fresh :environment {}})
          (is (= "Case evidence" (:text (document-files/read-file fresh "in/knowledge/sources/inbox/a.md"))))
          (finally
            (try (discourse/close-room! fresh)
                 (finally (discourse/close-room! room)))))))))

(deftest exact-exclusions-and-normalized-policy-are-deterministic
  (with-repository
    (fn [conn]
      (write! conn "inbox/private/a.md" "Excluded")
      (write! conn "inbox/private-extra/b.md" "Included")
      (let [policy (assoc inputs/default-policy :exclude-prefixes ["inbox/private"])
            captured (inputs/capture @conn policy)]
        (is (= ["inbox/private-extra/b.md"] (mapv :path (get-in captured [:manifest :files]))))
        (is (= captured (inputs/capture @conn (update policy :trigger-roots #(vec (concat (reverse %) %))))))))))

(deftest unchanged-decision-needs-no-source-bodies-or-allowance
  (with-repository
    (fn [conn]
      (write! conn "inbox/a.md" "Evidence")
      (write! conn "wiki/a.md" "Supporting context")
      (let [prior (:trigger-id (inputs/selection @conn inputs/default-policy))]
        (write! conn "wiki/a.md" "Adopted change")
        (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "No body reads for skip decision")))]
          (is (= prior (:trigger-id (inputs/selection @conn inputs/default-policy)))))))))

(deftest unsafe-role-and-staging-policies-refuse
  (doseq [policy [(assoc inputs/default-policy :trigger-roots [""])
                  (assoc inputs/default-policy :trigger-roots ["../private"])
                  (assoc inputs/default-policy :context-roots ["inbox/nested"])
                  (assoc inputs/default-policy :trigger-roots ["out"])
                  (assoc inputs/default-policy :context-roots ["workflows/custom"])
                  (assoc inputs/default-policy :eval "forged")]]
    (is (thrown? clojure.lang.ExceptionInfo (inputs/normalize-policy policy)))))

(deftest selection-sidecar-is-inert-closed-and-part-of-the-ordinary-bundle
  (let [text (pr-str {:kind :knowledge :policy inputs/default-policy})
        source (assoc workflow-fixture/source "input-selection.edn" text)]
    (is (nil? (inputs/declaration workflow-fixture/source)))
    (is (= (inputs/normalize-policy inputs/default-policy) (inputs/declaration source)))
    (is (not= (:expected-bundle (workflow-fixture/input))
              (:id (dvergr.catalog.room/bundle "reusable" source))))
    (doseq [bad [(str text " {}") "#=(System/exit 0)" "#custom/tag {}"
                 "{:kind :knowledge :policy {}}"
                 (pr-str {:kind :knowledge :policy inputs/default-policy :eval true})]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (inputs/declaration {"input-selection.edn" bad}))))))

(deftest admission-resource-limits-refuse-before-reading-source-bodies
  (with-repository
    (fn [conn]
      (dotimes [n 100] (write! conn (str "inbox/" n ".md") "Evidence"))
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Must refuse before body reads")))]
        (is (= :too-many-input-files
               (try (inputs/capture @conn inputs/default-policy)
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))

(deftest forged-size-and-expanded-path-refuse-before-blob-allocation
  (with-repository
    (fn [conn]
      (write! conn "inbox/a.md" "Evidence")
      (d/transact conn [{:geschichte.work/path "inbox/a.md" :geschichte.work/size 1}])
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Must not read mismatched content")))]
        (is (= :content-size-mismatch
               (try (inputs/capture @conn inputs/default-policy)
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))
  (with-repository
    (fn [conn]
      (write! conn (str "inbox/" (apply str (repeat 490 "a")) ".md") "Evidence")
      (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "Must not read overlong input paths")))]
        (is (= :input-path-too-long
               (try (inputs/capture @conn inputs/default-policy)
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))))
