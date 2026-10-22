(ns is.simm.starter.install-wiki-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as catalog]
            [dvergr.rooms.files :as native]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.documents.space :as space]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-authoring :as authoring]
            [is.simm.ops.owned-workflows :as owned]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.wiki-setup :as setup]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.starter.install-wiki :as install]
            [is.simm.starter.wiki-maintenance :as starter]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.adapters.geschichte :as gy]))

(defn- open-canonical! [room opened]
  ;; Provision a NEW pristine fixture store, rather than mark or migrate an
  ;; existing repository. The shared fixture owns registration and cleanup.
  (let [cfg (assoc workspace/repository-options :attribute-refs? false
                   :store {:backend :memory :id (random-uuid)})]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      ;; Retain the acquisition before either fallible initialization step.
      (swap! opened conj conn)
      (space/provision! conn)
      (binding [ec/*execution-context* (:ctx room)]
        (ygg/register! (gy/create conn {:system-name (str "canonical-install-" (random-uuid))})))
      conn)))

(defn- with-canonical [f]
  (let [opened (atom [])]
    (with-redefs [catalog/open-memory-workspace! #(open-canonical! % opened)]
      (@#'fixture/with-fixture true
                               (fn [data]
                                 (try (f data)
               ;; The shared fixture releases/deletes its stores AFTER this
               ;; callback returns, so remove governors while conns are valid.
                                      (finally (doseq [conn @opened] (governance/ungovern! conn)))))))))

(defn- principal [w] (ops/party-principal (:owner w)))
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo error error)))

(deftest pure-installation-decisions-preserve-every-kind-of-customization
  (let [shipped {"workflow.edn" "{}"}
        snapshot {:files shipped :manifest [{:path "workflow.edn" :mode 33188}]}]
    (is (= :install (install/decision {:files {} :manifest []} shipped)))
    (is (= :already-installed (install/decision snapshot shipped)))
    (doseq [custom [(assoc-in snapshot [:files "workflow.edn"] "edited")
                    (assoc-in snapshot [:manifest 0 :mode] 33261)
                    (assoc snapshot :files {})
                    (-> snapshot (assoc-in [:files "extra.clj"] "keep")
                        (update :manifest conj {:path "extra.clj" :mode 33188}))]]
      (is (= :customized (install/decision custom shipped))))))

(deftest native-install-is-idempotent-does-not-touch-other-files-or-activation
  (with-canonical
    (fn [{:keys [conn w host owned attached]}]
      (let [handle (str (:room w))
            control-before (vec (d/datoms @conn :eavt))
            other-before (query/worktree @attached)
            selected (workspace/selected-system-id)
            answer (install/install! host (principal w) handle)
            after (query/worktree @owned)
            head (query/refs @owned)]
        (is (= :installed (:status answer)))
        (is (= :unchanged (:activation answer)))
        (is (some? (parse-uuid (:revision answer))))
        (is (= (starter/source) (:files (workflows/snapshot-in @owned starter/name))))
        (is (contains? after "wiki/file.md"))
        (is (= other-before (query/worktree @attached)))
        (is (= selected (workspace/selected-system-id)))
        (with-redefs [native/write-to-completion! (fn [& _] (throw (AssertionError. "Exact install must not write")))]
          (is (= :already-installed (:status (install/install! host (principal w) handle)))))
        (is (= after (query/worktree @owned)))
        (is (= head (query/refs @owned)))
        (is (= (:revision answer)
               (str (owned/source-id
                     (reads/resolve-host-install! host (principal w) handle)
                     starter/name (workflows/manifest-in @owned starter/name)))))
        (is (= control-before (vec (d/datoms @conn :eavt))) "No activation, funding or schedule records")))))

(deftest setup-service-inspects-installs-and-preserves-custom-source-under-real-owners
  (with-canonical
    (fn [{:keys [conn w host owned]}]
      (let [owner (lifetime/create) party (:owner w) room (:room w)
            before (vec (d/datoms @conn :eavt))]
        (lifetime/open! owner)
        (try
          ;; Isolate this test's embedding installation while exercising the
          ;; real application admission and exact native Room lease.
          (with-redefs-fn {#'application/installation (atom {:host host :owner owner})}
            (fn []
              (is (= :missing (:status (setup/inspect! party room))))
              (is (= :installed (:status (setup/install! party room))))
              (is (= :installed (:status (setup/inspect! party room))))
              (with-redefs [native/write-to-completion!
                            (fn [& _] (throw (AssertionError. "Exact setup repeat must not write")))]
                (is (= :installed (:status (setup/install! party room)))))
              (repo/write! owned "workflows/maintain-wiki/custom.txt" (.getBytes "Keep this customization" "UTF-8"))
              (let [custom (query/worktree @owned)]
                (is (= :customized (:status (setup/inspect! party room))))
                (with-redefs [native/write-to-completion!
                              (fn [& _] (throw (AssertionError. "Customized setup must not write")))]
                  (is (= :customized (:status (setup/install! party room)))))
                (is (= custom (query/worktree @owned))))
              (is (= before (vec (d/datoms @conn :eavt))))))
          (finally (lifetime/fence! owner) (lifetime/join! owner 30000)))))))

(deftest customized-installed-source-is-preserved-without-a-native-dispatch
  (with-canonical
    (fn [{:keys [w host owned]}]
      (let [handle (str (:room w))]
        (install/install! host (principal w) handle)
        (repo/write! owned "workflows/maintain-wiki/custom.txt" (.getBytes "Human customization" "UTF-8"))
        (let [before (query/worktree @owned)]
          (with-redefs [native/write-to-completion! (fn [& _] (throw (AssertionError. "Custom source must not write")))]
            (is (= :customized (:status (install/install! host (principal w) handle)))))
          (is (= before (query/worktree @owned))))))))

(deftest room-membership-and-legacy-repositories-do-not-authorize-default-installation
  (with-canonical
    (fn [{:keys [w host hydrations]}]
      (let [before (count @hydrations)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (install/install! host (ops/party-principal (:member w)) (str (:room w)))))
        (is (= before (count @hydrations)) "Denied before hydration"))))
  (@#'fixture/with-fixture true
                           (fn [{:keys [w host owned]}]
                             (let [before (query/worktree @owned)]
                               (is (thrown? clojure.lang.ExceptionInfo (install/install! host (principal w) (str (:room w)))))
                               (is (= before (query/worktree @owned)))
                               (is (not (space/provisioned? @owned)))))))

(deftest source-and-ownership-races-are-rechecked-at-the-native-boundary
  (doseq [race [:source :owner :profile :target]]
    (with-canonical
      (fn [{:keys [conn w host owned attached-descriptor]}]
        (let [write authoring/install!
              original (query/worktree @owned)
              error (with-redefs [authoring/install!
                                  (fn [& args]
                                    (case race
                                      :source (repo/write! owned "workflows/maintain-wiki/custom.txt" (.getBytes "Concurrent edit" "UTF-8"))
                                      :owner (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
                                      :profile (d/transact owned [[:db/retract (:db/id (query/repository @owned))
                                                                   :document.repository/profile space/profile]])
                                      :target (d/transact conn [{:room/id (:room w)
                                                                 :room/default-repository [:system/id (:registry-system attached-descriptor)]}]))
                                    (apply write args))]
                      (failure #(install/install! host (principal w) (str (:room w)))))]
          (is error)
          (is (= (if (#{:source :target} race) ::authoring/conflict ::authoring/unavailable) (:type (ex-data error))))
          (is (not (contains? (query/worktree @owned) "workflows/maintain-wiki/workflow.edn")))
          (when (= :owner race) (is (= original (query/worktree @owned))))))))
  (doseq [race [:owner :profile :target]]
    (with-canonical
      (fn [{:keys [conn w host owned attached-descriptor]}]
        (let [validate reads/validate-host-install!
              before (query/worktree @owned)
              error (with-redefs [reads/validate-host-install!
                                  (fn [& args]
                                    (case race
                                      :owner (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
                                      :profile (d/transact owned [[:db/retract (:db/id (query/repository @owned))
                                                                   :document.repository/profile space/profile]])
                                      :target (d/transact conn [{:room/id (:room w)
                                                                 :room/default-repository [:system/id (:registry-system attached-descriptor)]}]))
                                    (apply validate args))]
                      (failure #(install/install! host (principal w) (str (:room w)))))]
          (is (= ::authoring/unavailable (:type (ex-data error))))
          (is (= before (query/worktree @owned))))))))

(deftest uncertain-completion-is-never-automatically-replayed
  (with-canonical
    (fn [{:keys [w host]}]
      (let [calls (atom 0)
            error (with-redefs [native/write-to-completion!
                                (fn [& _] (swap! calls inc) {:completion :pending})]
                    (failure #(install/install! host (principal w) (str (:room w)))))]
        (is (= ::authoring/pending (:type (ex-data error))))
        (is (= 1 @calls))))))
