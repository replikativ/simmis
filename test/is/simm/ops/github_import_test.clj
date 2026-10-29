(ns is.simm.ops.github-import-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [datahike.api :as d]
            [dvergr.chat.schema :as chat]
            [dvergr.system.db :as registry]
            [geschichte.git.budget :as budget]
            [geschichte.git.http :as http]
            [geschichte.git.object :as object]
            [geschichte.git.pack :as pack]
            [geschichte.git.pkt-line :as pkt]
            [geschichte.git.project :as project]
            [geschichte.repo :as repo]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.space :as space]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workspace-intent :as intent]
            [is.simm.ops.github-import :as github]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [kontor.governance :as governance]))

(defn- with-stores [f]
  (let [configs (vec (repeatedly 3 #(hash-map :store {:backend :memory :id (random-uuid)}
                                            :schema-flexibility :write :attribute-refs? false
                                            :keep-history? true :crypto-hash? true :commit-graph? true)))]
    (doseq [cfg configs] (d/create-database cfg))
    (let [connections (mapv d/connect configs)]
      (try (apply f connections)
           (finally
             (doseq [conn connections] (governance/ungovern! conn) (d/release conn))
             (doseq [cfg configs] (d/delete-database cfg)))))))

(defn- pending! [control]
  (let [schema (vals (into {} (map (juxt :db/ident identity))
                          (concat registry/schema chat/actor-schema system-db/schema)))
        creator (random-uuid) id (random-uuid) room (random-uuid) repo-id (random-uuid)
        request (intent/request! {:id (str id) :name "Imported source" :members []
                                  :source {:kind :github :repository "owner/repository"}})]
    (d/transact control schema)
    (d/transact control [{:party/id creator :party/type :human}])
    (d/transact control [{:system/id repo-id :system/type :repo :system/name "source"
                         :system/scope "/virtual/source" :system/repository-profile space/profile}
                        {:room/id room :room/slug (intent/slug creator request) :room/name (:name request)
                         :room/type :project :room/owner [:party/id creator]
                         :room/default-repository [:system/id repo-id]
                         :room/provisioning-state :pending :room/provisioning-version 1}])
    {:creator creator :request request :room room}))

(deftest native-import-keeps-source-bytes-without-seeds-or-evaluation
  (with-stores
    (fn [source target control]
      (repo/init! source)
      (let [files {"README.md" "# Ordinary upstream Markdown\n"
                   "build.clj" "(throw (Exception. \"Imported code must never run\"))\n"}]
        (doseq [[path text] files] (repo/write! source path (object/utf8 text)))
        (repo/stage-all! source)
        (let [commit (repo/commit! source {:message "Synthetic upstream source"})
              graph (project/project source (:geschichte.commit/id commit))
              oid (:oid graph) packed (pack/encode (:objects graph))
              calls (atom [])
              {:keys [creator request room]} (pending! control)
              owner (lifetime/create)
              sender (fn [{:keys [method url body import-budget] :as request}]
                       (swap! calls conj [method url])
                       (is (str/starts-with? url "https://github.com/owner/repository.git/"))
                       ;; Native HTTP monitors can poll on a different thread;
                       ;; a borrowed App ticket must not be checked there.
                       (is (nil? @(future (budget/check! import-budget))))
                       (if (= :get method)
                         {:status 200 :body (pkt/encode ["version 2\n" "ls-refs\n" "fetch\n" :flush])}
                         (if (= "command=ls-refs\n" (pkt/text (first (pkt/decode body))))
                           {:status 200 :body (pkt/encode [(str oid " HEAD symref-target:refs/heads/main\n")
                                                          (str oid " refs/heads/main\n") :flush])}
                           {:status 200 :body (pkt/encode ["packfile\n"
                                                          (object/concat-bytes (byte-array [1]) packed) :flush])})))]
          (lifetime/open! owner)
          (with-redefs [http/send-http sender]
            (lifetime/with-operation!
             owner
             #(github/with-admission!
               (fn []
                 (let [hooks (get (github/policies control creator request lifetime/check-current!) space/profile)]
                   ((:initialize! hooks) target {:profile space/profile}))))))
          (is (= 3 (count @calls)))
          (is (= (set (keys files)) (set (keys (repo/worktree target)))))
          (doseq [[path text] files]
            (is (= text (String. ^bytes (repo/read target path) "UTF-8"))))
          (is (= oid (:geschichte.commit/git-oid
                      (repo/commit-by-id target (:geschichte.commit/id (repo/head-commit target))))))
          (is (:clean? (repo/status target)))
          (is (= {:status :preparing} (intent/observe @control creator request)))
          (is (nil? (:room/parties (d/entity @control [:room/id room]))))
          (is (nil? (:room/content-db-scope (d/entity @control [:room/id room]))))
          (is (identical? target (profile/prepare-open! target {:profile space/profile}))))))))

(deftest initializer-refuses-inherited-ticket-on-a-different-worker-before-io
  (with-stores
    (fn [_ target control]
      (let [{:keys [creator request room]} (pending! control)
            owner (lifetime/create) before @target]
        (lifetime/open! owner)
        (lifetime/with-operation!
         owner
         (fn []
           (let [hooks (get (github/policies control creator request lifetime/check-current!) space/profile)
                 result (future
                          (try ((:initialize! hooks) target {:profile space/profile})
                               (catch Exception e e)))]
             (try
               (is (instance? clojure.lang.ExceptionInfo (deref result 5000 ::timeout)))
               (finally (future-cancel result))))))
        (is (= before @target))
        (is (nil? (:room/github-source (d/entity @control [:room/id room]))))))))

(deftest finite-host-admission-does-not-queue-or-leak-a-permit
  (let [entered (promise) release (promise)
        first (future (github/with-admission! #(do (deliver entered true) @release :finished)))]
    (try
      (is (= true (deref entered 5000 ::timeout)))
      (is (thrown? clojure.lang.ExceptionInfo (github/with-admission! #(throw (AssertionError. "No queued native operation")))))
      (deliver release true)
      (is (= :finished (deref first 5000 ::timeout)))
      (is (= :next (github/with-admission! (constantly :next))))
      (finally (deliver release true) (future-cancel first)))))
