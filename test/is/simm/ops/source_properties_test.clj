(ns is.simm.ops.source-properties-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as catalog]
            [dvergr.rooms.file-tx :as file-tx]
            [dvergr.rooms.files :as native-files]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.space :as space]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]
            [is.simm.model.crud :as crud]
            [is.simm.ops.owned-files :as actions]
            [is.simm.ops.owned-workspace-test :as fixture]
            [is.simm.runtimes.context :as context]
            [is.simm.uis.web.desktop.memory-remote]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.adapters.geschichte :as gy]))

(defn- with-canonical-fixture [owned-first? f]
  (let [open! catalog/open-memory-workspace! calls (atom 0) configs (atom [])]
    (try
      ;; Provision a REAL pristine governed repository before registration;
      ;; never turn an already initialized old fixture into a canonical space.
      (with-redefs [catalog/open-memory-workspace!
                    (fn [room]
                      (if (= (swap! calls inc) (if owned-first? 1 2))
                        (let [cfg (assoc workspace/repository-options :store {:backend :memory :id (random-uuid)})]
                          (swap! configs conj cfg)
                          (d/create-database cfg)
                          (let [conn (d/connect cfg)]
                            (space/provision! conn)
                            (binding [ec/*execution-context* (:ctx room)]
                              (ygg/register! (gy/create conn {:system-name (str "canonical-fixture-" (random-uuid))})))
                            conn))
                        (open! room)))]
        (@#'fixture/with-fixture owned-first?
         (fn [{:keys [owned] :as context}]
           (try (f context) (finally (governance/ungovern! owned))))))
      (finally (doseq [cfg @configs] (d/delete-database cfg))))))

(defn- seed-source! [conn]
  (let [type (crud/create-type! conn {:name "SourcePropertyCompany"})
        property (crud/create-morphism! conn {:name "S/SourcePropertyCompany/revenue"
                                              :src "S/SourcePropertyCompany" :dst "S/Number" :optional? false})
        text (str "---\nsimmis-id: '" (random-uuid) "'\nsimmis:\n  version: 1\n  types: ['" type "']\n"
                  "  properties:\n    revenue:\n      id: '" property "'\n      type: '" type "'\n"
                  "revenue: 0 # retain this comment\n---\n# Company\n")]
    (repo/write! conn "wiki/file.md" (markdown/source-bytes text))
    (repo/stage-all! conn)
    (repo/commit! conn {:message "Typed source fixture"})
    {:text text :property property}))

(defn- model [conn w room]
  (let [metadata (actions/metadata! conn (:member w) (:room w) room {:folders [] :paths ["wiki/file.md"]})]
    (actions/properties! conn (:member w) (:room w) room
                         {:path "wiki/file.md" :target {:kind :page} :expected-target-stamp (:target-stamp metadata)})))

(defn- edit [model property value]
  (assoc (select-keys model [:path :target :expected :revision :scope-revision :target-stamp])
         :expected-target-stamp (:target-stamp model) :alias "revenue" :property property :value value))

(defn- request [model property value]
  (dissoc (edit model property value) :target-stamp))

(defn- failure [f] (try (f) nil (catch Throwable error error)))

(deftest actual-inbound-property-input-preserves-long-precision-and-source
  (with-canonical-fixture true
    (fn [{:keys [conn w room owned attached]}]
      (let [{:keys [text property]} (seed-source! owned)
            other @attached]
        (with-redefs [context/server-context (:ctx room) rooms/live-room (fn [id] (when (= id (:room w)) room))]
          (let [call (fn [kind name request]
                       (world/inbound! (world/principal w kind) name
                                       {:room-id-str (str (:room w)) :request request}))
                opened (:result (call :member "load-memory-properties!"
                                      {:path "wiki/file.md" :target {:kind :page}
                                       :expected-target-stamp (:target-stamp (model conn w room))}))
                command (-> (request opened property nil) (dissoc :value)
                            (assoc :input {:text "9007199254740993"}))]
            (is (= 0 (:value (first (filter #(= property (:id %)) (:fields opened))))))
            (doseq [kind [:non-member :admin :anon]]
              (is (world/refused? (call kind "save-memory-property!" command))))
            (doseq [extra [{:value 1} {:value-type :db.type/string} {:validated-basis {}}]]
              (is (:error (call :member "save-memory-property!" (merge command extra)))))
            (is (:error (call :member "save-memory-property!" (assoc command :input {:text "1.5"}))))
            (is (= text (:text (files/read-file-in @owned "wiki/file.md"))))
            (let [saved (:result (call :member "save-memory-property!" command))]
              (is (:saved? saved))
              (is (= (:target-stamp opened) (:target-stamp saved)))
              (is (= (.replace text "revenue: 0" "revenue: 9007199254740993") (:text saved)))
              (is (= (:text saved) (:text (files/read-file-in @owned "wiki/file.md"))))
              (is (= :explicit (:identity-status (:analysis saved))))
              (let [reopened (:result (call :member "load-memory-properties!"
                                          {:path "wiki/file.md" :target {:kind :page}
                                           :expected-target-stamp (:target-stamp opened)}))
                    input (:input (first (filter #(= property (:id %)) (:fields reopened))))
                    unchanged (-> (request reopened property nil) (dissoc :value) (assoc :input input))]
                (is (= {:text "9007199254740993"} input))
                (is (= (:text saved) (get-in (call :member "save-memory-property!" unchanged) [:result :text])))))
            (is (:error (call :member "save-memory-property!" command)))
            (is (= other @attached))))))))

(deftest property-saves-use-real-owned-target-and-fresh-atomic-validation
  (doseq [owned-first? [true false]]
    (with-canonical-fixture owned-first?
      (fn [{:keys [conn w room owned attached]}]
        (let [{:keys [text property]} (seed-source! owned)
              opened (model conn w room)
              other @attached selected (workspace/selected-system-id)
              writer native-files/write-to!]
          (is (not (access/can? @conn (:member w) :merge {:room (:room w)})))
          (is (true? (:editable? (first (filter #(= property (:id %)) (:fields opened))))))
          (let [error (with-redefs [native-files/write-to!
                                   (fn [& args]
                                     (d/transact owned [{:db/ident :fixture/raced-schema :db/valueType :db.type/string
                                                        :db/cardinality :db.cardinality/one}])
                                     (apply writer args))]
                        (failure #(actions/property-save! conn (:member w) (:room w) room (request opened property 5))))]
            (is (file-tx/native-basis-refusal? error))
            (is (= text (:text (files/read-file-in @owned "wiki/file.md")))))
          ;; The old editor's scope is unchanged by this unrelated schema tx.
          ;; Save captures a fresh basis; it must NOT retain the editor-open CID.
          (let [saved (actions/property-save! conn (:member w) (:room w) room (request opened property 7))]
            (is (:saved? saved))
            (is (= (:target-stamp opened) (:target-stamp saved)))
            (is (= (.replace text "revenue: 0" "revenue: 7") (:text (files/read-file-in @owned "wiki/file.md")))))
          (is (some? (failure #(actions/property-save! conn (:member w) (:room w) room (request opened property 8)))))
          (doseq [extra [{:validated-basis {}} {:bindings {}} {:options {:roots []}}]]
            (is (some? (failure #(actions/property-save! conn (:member w) (:room w) room
                                                        (merge (request (model conn w room) property 9) extra))))))
          (is (= other @attached))
          (is (= selected (workspace/selected-system-id))))))))

(deftest property-write-rechecks-room-authority-at-native-effect-boundary
  (with-canonical-fixture true
    (fn [{:keys [conn w room owned attached]}]
      (let [{:keys [property]} (seed-source! owned)
            opened (model conn w room) before (repo/worktree owned) other @attached
            writer native-files/write-to!
            error (with-redefs [native-files/write-to!
                                (fn [& args]
                                  (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                                  (apply writer args))]
                    (failure #(actions/property-save! conn (:member w) (:room w) room (request opened property 7))))]
        (is (some? error))
        (is (= before (repo/worktree owned)))
        (is (= other @attached))))))
