(ns is.simm.ops.github-candidate-roots-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [datahike.versioning :as versioning]
            [dvergr.artifact :as artifact]
            [dvergr.system.rooms :as native]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.model.github-connections :as connections]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.model.github-pins :as pins]
            [is.simm.model.github-proposals :as provenance]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.github-candidate-roots :as candidates]
            [is.simm.ops.github-merge :as merge]
            [is.simm.ops.github-merge-test :as fixtures]
            [is.simm.ops.github-roots :as bases]
            [is.simm.ops.knowledge-changes :as changes]))

(use-fixtures :each
  (fn [f]
    (with-redefs-fn {#'native/storage-gc-homes (atom #{})
                     #'native/storage-gc-admission (atom {:ever-admitted? false :prohibited-by nil})}
      f)))

(defn- fixture [f]
  (#'fixtures/with-edited-import
   {"notes.md" "Incoming addition\n"}
   (fn [{:keys [local local-head fetched opts]}]
     (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}
           room (random-uuid) by (random-uuid) client (random-uuid)
           binding {:id (journal/request-id by room client) :room room :by by :client-id client :claim (random-uuid)}
           source @local basis (changes/basis source)
           connection {:version 1 :room room :target-stamp (random-uuid)
                       :source {:kind :github :repository "owner/repository"} :ref "refs/heads/main"
                       :target (assoc (select-keys basis [:repository :store :backend :branch :ref])
                                      :registry-system (random-uuid) :runtime-system "repository")}
           upstream-base (:upstream-base opts)
           oid (:geschichte.commit/git-oid (repo/commit-by-id local upstream-base))
           branch (keyword (str "candidate-roots-" (random-uuid)))]
       (d/create-database cfg)
       (let [control (d/connect cfg) worker (atom nil)]
         (try
           (d/transact control (concat integrations/schema pins/schema journal/schema))
           (let [store (artifact/datahike-store control)
                 cref (artifact/publish-value! store connection identity)
                 pref (artifact/publish-value! store {:fixture :verified-import} identity)]
             (d/transact control [[:db.fn/call integrations/initialize-tx room cref
                                   (connections/connection-id connection) upstream-base oid pref (random-uuid)]])
             (let [base (bases/retain-base! control source connection by (constantly true) (constantly true))
                   cursor (integrations/current-in @control room)
                   preparation (artifact/publish-value! store {:fixture :incoming} identity)]
               (d/transact control [[:db.fn/call journal/prepare-tx binding preparation (random-uuid) :incoming]])
               (let [creating (journal/sync-in @control (:id binding))]
                 (d/transact control [[:db.fn/call journal/transition-tx creating :allocated (random-uuid) (random-uuid)]]))
               (versioning/branch! local (get-in source [:config :branch]) branch)
               (reset! worker (d/connect (assoc (:config source) :branch branch)))
               (let [{:keys [plan upstream-head upstream-git-oid]} (merge/plan-fetched! @worker fetched opts)]
                 (repo/prepare-merge! @worker plan)
                 (let [head (:geschichte.commit/id (repo/commit! @worker {:message "Frozen merge candidate"}))
                       incoming-commit (repo/commit-by-id @worker upstream-head)
                       incoming (d/commit-as-db @@worker (:geschichte.commit/snapshot incoming-commit))
                       identity (provenance/incoming-identity connection
                                                              {:head upstream-base :git-oid oid
                                                               :proof-id (:github.integration/proof-id cursor)}
                                                              {:head upstream-head :git-oid upstream-git-oid})
                       body {:version 1 :identity identity :key (provenance/incoming-key identity)
                             :producer {:sync-id (:id binding) :claim (:claim binding) :by by}
                             :proposal {:id (random-uuid) :scope (get-in connection [:target :registry-system])
                                        :review-ref (str "refs/heads/github-review-" (random-uuid))
                                        :base local-head :head head}
                             :canonical-basis basis
                             :roots {:base {:id (:root base) :head upstream-base :basis (get-in base [:selection :basis])}
                                     :local (candidates/descriptor source local-head)
                                     :incoming (candidates/descriptor incoming upstream-head)
                                     :candidate (candidates/descriptor @@worker head)}}]
                   (f {:control control :source source :parent local :body body
                       :row (journal/sync-in @control (:id binding))
                       :retire! (fn [] (d/release @worker) (reset! worker nil)
                                  (versioning/delete-branch! local branch))})))))
           (finally
             (when @worker (d/release @worker))
             (when (contains? (versioning/branches local) branch) (versioning/delete-branch! local branch))
             (d/release control) (d/delete-database cfg))))))))

(defn- retain! [{:keys [control source row body]}]
  (candidates/retain! control source row body (constantly true) (constantly true)))

(defn- verify! [{:keys [control source body]}]
  (candidates/verify! control source (journal/sync-in @control (get-in body [:producer :sync-id])) body (constantly true)))

(deftest every-role-remains-verifiable-after-private-branch-removal
  (fixture
   (fn [{:keys [body retire! parent] :as environment}]
     (let [before (repo/worktree parent) retained (retain! environment)]
       (is (= :retaining (get-in retained [:row :github.sync/state])))
       (retire!)
       (is (= body (:provenance (verify! environment))))
       (is (= before (repo/worktree parent)))))))

(deftest a-lost-retention-intent-ack-never-permits-root-creation
  (fixture
   (fn [{:keys [control] :as environment}]
     (let [transact d/transact calls (atom 0)]
       (with-redefs [d/transact (fn [conn tx]
                                  (let [report (transact conn tx)]
                                    (when (and (identical? conn control)
                                               (= :retaining (:github.sync/state (journal/sync-in (:db-after report)
                                                                                                  (get-in environment [:body :producer :sync-id])))))
                                      (throw (ex-info "Lost retention intent ACK" {})))
                                    report))
                     roots/pin! (fn [& _] (swap! calls inc) (throw (AssertionError. "No acknowledged creator")))]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Lost retention intent ACK" (retain! environment))))
       (is (zero? @calls))
       (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "Verification cannot create")))]
         (is (= :pin-unavailable (try (verify! environment) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))))

(deftest partial-native-root-creation-is-held-with-the-original-ids
  (fixture
   (fn [environment]
     (let [pin! roots/pin! calls (atom 0)]
       (with-redefs [roots/pin! (fn [& args] (swap! calls inc) (apply pin! args)
                                  (throw (ex-info "Lost first root ACK" {})))]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Lost first root ACK" (retain! environment))))
       (is (= 1 @calls))
       (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "Do not recreate any root")))]
         (is (= :pin-unavailable (try (verify! environment) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))))

(deftest an-ack-lost-after-all-roots-can-be-verified-without-recreation
  (fixture
   (fn [{:keys [body] :as environment}]
     (let [pin! roots/pin! calls (atom 0)]
       (with-redefs [roots/pin! (fn [& args]
                                  (let [result (apply pin! args)]
                                    (when (= 3 (swap! calls inc)) (throw (ex-info "Lost final root ACK" {}))) result))]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Lost final root ACK" (retain! environment))))
       (is (= 3 @calls))
       (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "Recovery cannot recreate")))]
         (is (= body (:provenance (verify! environment)))))))))
