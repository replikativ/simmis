(ns is.simm.ops.github-sync-workspace-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.versioning :as versioning]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.git.budget :as budget]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.project :as project]
            [geschichte.git.transport :as transport]
            [geschichte.repo :as repo]
            [geschichte.workspace :as workspace]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.ops.github-import :as import]
            [is.simm.ops.github-import-test :as fixtures]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.github-sync-workspace :as private]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- sender [graph]
  (fn [{:keys [method body]}]
    (let [{:keys [oid objects]} @graph]
      {:status 200
       :body (if (= :get method)
               (pkt/encode ["version 2\n" "ls-refs\n" "fetch\n" :flush])
               (if (= "command=ls-refs\n" (pkt/text (first (pkt/decode body))))
                 (pkt/encode [(str oid " HEAD symref-target:refs/heads/main\n")
                              (str oid " refs/heads/main\n") :flush])
                 (pkt/encode ["packfile\n" (object/concat-bytes (byte-array [1]) (pack/encode objects)) :flush])))})))

(defn- options []
  {:import-budget (budget/create {:limits import/limits :timeout-ms 30000})
   :http-body-idle-timeout-ms 1000 :prefixes ["HEAD" "refs/heads/"] :haves []})

(deftest retained-private-ff-is-reviewed-before-guarded-advance
  (#'fixtures/with-stores
   (fn [upstream parent _]
     (repo/init! upstream)
     (repo/write! upstream "README.md" (object/utf8 "Original\n"))
     (repo/stage-all! upstream)
     (let [commit (repo/commit! upstream {:message "C1"})
           graph (atom (project/project upstream (:geschichte.commit/id commit)))
           owner (lifetime/create)
           branch (facts/branch (random-uuid) (random-uuid))
           origin {:repository "owner/repository" :ref "refs/heads/main"
                   :registry-system (random-uuid) :import-head (:geschichte.commit/id commit)}]
       (profile/initialize-foundation! parent {:profile space/profile})
       (lifetime/open! owner)
       (with-redefs [http/send-http (sender graph)]
         (transport/clone! {:conn parent :remote "origin" :url "https://github.com/owner/repository.git"
                            :options (options)})
         (let [before (facts/capture @parent origin)
               _ (repo/write! upstream "nested/report.md" (object/utf8 "New upstream report\n"))
               _ (repo/stage-all! upstream)
               commit (repo/commit! upstream {:message "C2"})
               _ (reset! graph (project/project upstream (:geschichte.commit/id commit)))
               after (lifetime/with-operation!
                       owner
                       #(private/with-connection!
                          parent nil branch (:checkpoint before) true
                          (fn [child]
                            (transport/pull! {:conn child :remote "origin" :url "https://github.com/owner/repository.git"
                                              :options (assoc (options) :policy :ff-only :branch "main")})
                            (facts/validate-desired! before (facts/capture @child origin)))))]
           (is (= before (facts/capture @parent origin)))
           (is (= [{:path "nested/report.md" :kind :added
                    :after (get-in after [:visible :manifest :files "/nested/report.md"])}]
                  (facts/changes before after)))
           (is (contains? (versioning/branches parent) branch))
           (lifetime/with-operation!
             owner
             #(private/with-connection!
                parent nil branch (:checkpoint after) false
                (fn [child]
                  (is (facts/same-visible? after (facts/capture @child origin)))
                  (workspace/advance! child parent {:ref (:ref origin) :expected-checkpoint (:checkpoint before)}))))
           (is (facts/same-visible? after (facts/capture @parent origin)))
           (is (= "New upstream report\n" (String. ^bytes (repo/read parent "nested/report.md") "UTF-8")))
           (versioning/delete-branch! parent branch)
           (is (not (contains? (versioning/branches parent) branch)))))))))

(deftest unknown-native-connect-or-release-keeps-the-exact-cleanup-entry
  (#'fixtures/with-stores
   (fn [_ parent _]
     (profile/initialize-foundation! parent {:profile space/profile})
     (let [owner (doto (lifetime/create) lifetime/open!)
           connect @#'d/connect release @#'d/release fork @#'workspace/fork-at-checkpoint!]
       (doseq [fault [:fork :handoff :connect :release]]
         (let [branch (facts/branch (random-uuid) (random-uuid)) retained (atom nil) releases (atom 0)]
           (with-redefs [preparation/retain-host-cleanup!
                         (fn [lease entries cleanup]
                           (reset! retained {:lease lease :entries entries :cleanup cleanup}))
                         workspace/fork-at-checkpoint!
                         (fn [& args]
                           (let [result (apply fork args)]
                             (when (= fault :fork) (throw (java.io.IOException. "Lost native allocation ACK")))
                             result))
                         d/connect (fn [config]
                                     (if (and (= fault :connect) (= branch (:branch config)))
                                       ;; This fixture knows no connection was
                                       ;; acquired; production cannot assume it.
                                       (throw (java.io.IOException. "Unknown connect outcome"))
                                       (connect config)))
                         d/release (fn [conn]
                                     (if (and (= fault :release) (= branch (get-in @conn [:config :branch])))
                                       (do (swap! releases inc) (release conn)
                                           (throw (java.io.IOException. "Lost release ACK")))
                                       (release conn)))]
             (is (thrown? Exception
                          (lifetime/with-operation!
                            owner #(private/with-connection! parent ::original-lease branch (d/commit-id @parent) true
                                     (fn [] (when (= fault :handoff)
                                              (throw (java.io.IOException. "Unconfirmed control handoff"))))
                                     (constantly nil)))))
             (is (= ::original-lease (:lease @retained)))
             (is (= #{branch} (set (keys (:entries @retained)))))
             (is (= ({:fork :allocating :handoff :forked :connect :acquiring :release :releasing} fault)
                    (:phase @(get-in @retained [:entries branch]))))
             (is (thrown? clojure.lang.ExceptionInfo
                          ((:cleanup @retained) (get-in @retained [:entries branch]))))
             (is (= (if (= fault :release) 1 0) @releases) "Unknown release is never repeated"))
           ;; The fixture positively knows its scripted connect made no native
           ;; acquisition, or that its original allocation/release returned before the
           ;; injected error. No production recovery acknowledgement is forged.
           (versioning/delete-branch! parent branch)))))))

(deftest unknown-root-removal-retains-its-original-physical-slot
  (#'fixtures/with-stores
   (fn [_ parent _]
     (profile/initialize-foundation! parent {:profile space/profile})
     (let [owner (doto (lifetime/create) lifetime/open!) branch (facts/branch (random-uuid) (random-uuid))
           checkpoint (d/commit-id @parent) remove @#'versioning/delete-branch!
           retained (atom nil) removals (atom 0)]
       (lifetime/with-operation! owner #(private/with-connection! parent ::original-lease branch checkpoint true (constantly nil)))
       (with-redefs [preparation/retain-host-cleanup!
                     (fn [lease entries cleanup] (reset! retained {:lease lease :entries entries :cleanup cleanup}))
                     versioning/delete-branch!
                     (fn [conn branch] (swap! removals inc) (remove conn branch {:sync? true})
                       (throw (java.io.IOException. "Lost root removal ACK")))]
         (is (thrown? Exception (private/retire! parent ::original-lease branch checkpoint)))
         (is (= ::original-lease (:lease @retained)))
         (is (= :retiring (:phase @(get-in @retained [:entries branch]))))
         (is (thrown? clojure.lang.ExceptionInfo ((:cleanup @retained) (get-in @retained [:entries branch]))))
         (is (= 1 @removals))
         (is (not (contains? (versioning/branches parent) branch))))))))
