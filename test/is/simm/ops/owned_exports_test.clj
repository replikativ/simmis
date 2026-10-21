(ns is.simm.ops.owned-exports-test
  "Inactive owned export composition through real ops/MCP and native stores.
   No listener/JWT, activation, retained exports or execution claim."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as dops]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.content :as content]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.authority.world :as world]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.git-snapshot :as git]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-mcp-transport-test :as wire]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.work-api :as api]))

(defn- with-api [f]
  (let [previous (dops/operations)]
    (try (api/install!) (f)
         (finally
           (doseq [op (keys api/specifications)]
             (if-let [spec (get previous op)] (dops/register-op! op spec) (dops/unregister-op! op)))
           (server/sync-op-tools!)))))

(defn- principal [w]
  (update (@#'fixture/principal w :member) :principal/tools into
          #{"simmis_workspace-bundle" "simmis_workspace-bundle-chunk"}))

(defn- invoke [host p op args] (dops/invoke host p op args))

(defn- failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e e)))

(defn- durable [conn]
  (let [db @conn] {:commit (d/commit-id db) :config (:config db) :worktree (query/worktree db)}))

(deftest owned-and-legacy-exports-preserve-their-distinct-source-and-identities
  (doseq [owned-first? [true false]]
    (@#'fixture/with-fixture
     owned-first?
     (fn [{:keys [conn w room host owned attached hydrations]}]
       (with-api
         (fn []
           (let [p (principal w) id (str (:room w))
                 slug (:room/slug (d/entity @conn [:room/id (:room w)]))
                 legacy (ops/host (:ctx room))
                 selected (git/describe room) before (durable attached)
                 selected-system (workspace/selected-system-id)
                 original reads/resolve-host-read! captures (atom 0)
                 expected (reads/resolve-host-read! host p id)
                 owned-id (git/snapshot-id-in (:db expected) (:room-id expected) (:target-stamp expected))]
             (reset! hydrations [])
             (doseq [handle [id slug]]
               (let [result (invoke legacy p :simmis/workspace-bundle {:room handle})]
                 (is (= selected result))
                 (is (not= owned-id (:snapshot result)))))
             (with-redefs [reads/resolve-host-read! (fn [& args] (swap! captures inc) (apply original args))
                           api/room! (fn [& _] (throw (AssertionError. "No post-binding Room lookup")))]
               (let [host (assoc-in host [:config :room-files :resolve-read]
                                    (fn [& _] (throw (AssertionError. "No file callback composition"))))]
                 (with-redefs [server/current-daemon (constantly host)]
                   (doseq [version ["2025-11-25" "2026-07-28"] handle [id slug]]
                     (let [send (@#'wire/session version p)
                           descriptor (@#'wire/data (@#'wire/tool send "simmis_workspace-bundle" {:room handle}))
                           chunk (@#'wire/data (@#'wire/tool send "simmis_workspace-bundle-chunk"
                                                             {:room handle :expected-snapshot (:snapshot descriptor)
                                                              :offset 0 :length 16}))]
                       (is (= (str owned-id) (:snapshot descriptor) (:snapshot chunk)))
                       (is (false? (:history descriptor)))
                       (is (= 1 (:files descriptor))))))))
             (is (= 8 @captures (count @hydrations)))
             (is (every? #(= (:room w) %) @hydrations))
             (is (= selected-system (workspace/selected-system-id)))
             (is (= before (durable attached)))
             (is (= "# Owned\n" (String. ^bytes (get-in (git/snapshot-in @owned (:room-id expected)
                                                                         (:target-stamp expected) nil)
                                                        [:files "wiki/file.md" :bytes]) "UTF-8"))))))))))

(deftest owner-presence-never-downgrades-to-selected-export
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host hydrations]}]
     (with-api
       (fn []
         (doseq [owner [nil {} {:control-conn :forged :execution-ctx :forged}
                        (assoc (:is.simm.ops.owned-reads/owner host) :extra true)]]
           (let [e (failure #(invoke (assoc host :is.simm.ops.owned-reads/owner owner)
                                     (principal w) :simmis/workspace-bundle {:room (str (:room w))}))]
             (is (= 503 (dops/error-status e)))
             (is (= "Owned workspace export unavailable" (ex-message e)))))
         (let [e (failure #(invoke host (principal w) :simmis/workspace-bundle-chunk
                                   {:room (str (:room w)) :expected-snapshot (apply str (repeat 36 "z"))
                                    :offset 0 :length 16}))]
           (is (= 400 (dops/error-status e))))
         (is (empty? @hydrations)))))))

(deftest export-authorizer-coheres-original-and-legacy-descriptor-without-hydration
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host hydrations]}]
     (with-api
       (fn []
         (let [p (principal w) id (str (:room w))
               slug (:room/slug (d/entity @conn [:room/id (:room w)]))
               auth (get-in host [:config :auth :authorize])
               request {:op :simmis/workspace-bundle :action :read
                        :resource [:room slug] :args {:room id}}]
           (d/transact conn [{:room/id (:other-room w) :room/slug (str "other-" (:other-room w))
                              :room/parties [[:party/id (:member w)]]}])
           (is (= (:other-room w) (:room-id (reads/authorize-read! @conn p (str (:other-room w))))))
           (is (:allow? (auth p request)))
           (is (:allow? (auth p (dissoc request :args))))
           (doseq [bad [(assoc request :args nil) (assoc request :args {})
                        (assoc request :resource [:room (str (:other-room w))])
                        (assoc request :action :write)
                        (assoc request :args {:room id :conn "forged"})]]
             (is (false? (:allow? (auth p bad)))))
           (is (false? (:allow? (auth (dissoc p :principal/mcp?) request))))
           (is (empty? @hydrations))))))))

(deftest exported-tools-retain-current-room-and-credential-ceilings
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host hydrations]}]
     (with-api
       #(with-redefs [server/current-daemon (constantly host)]
          (let [p (principal w) args {:room (str (:room w))}
                send (@#'wire/session "2026-07-28" p)
                descriptor (@#'wire/data (@#'wire/tool send "simmis_workspace-bundle" args))
                chunk (assoc args :expected-snapshot (:snapshot descriptor) :offset 0 :length 16)
                restricted (@#'wire/session "2026-07-28"
                                            (update p :principal/tools disj "simmis_workspace-bundle"
                                                    "simmis_workspace-bundle-chunk"))
                before (count @hydrations)]
            (doseq [[tool input] [["simmis_workspace-bundle" args] ["simmis_workspace-bundle-chunk" chunk]]]
              (let [reply (@#'wire/tool restricted tool input)]
                (is (or (:error reply) (get-in reply [:result :isError])))))
            (is (= before (count @hydrations)))
            (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
            (doseq [[tool input] [["simmis_workspace-bundle" args] ["simmis_workspace-bundle-chunk" chunk]]]
              (let [reply (@#'wire/tool send tool input)]
                (is (or (:error reply) (get-in reply [:result :isError])))))
            (is (= before (count @hydrations)))))))))

(deftest inactive-owned-export-host-never-delegates-nonhuman-exports
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host hydrations]}]
     (with-api
       (fn []
         (let [args {:room (str (:room w))}
               auth (get-in host [:config :auth :authorize])
               impl (get-in api/specifications [:simmis/workspace-bundle :impl])]
           (doseq [kind [:worker :agent]]
             (let [p (assoc (principal w) :principal/kind kind)]
               (with-redefs [ops/authorize (fn [& _] {:allow? true})]
                 (is (false? (:allow? (auth p {:op :simmis/workspace-bundle :action :read
                                               :resource [:room (:room args)] :args args}))))
                 (is (false? (:allow? (auth p {:op :simmis/workspace-bundle :action :read
                                               :resource [:room (:room args)]}))))
                 (is (:allow? (auth p {:op :room/read :action :read
                                       :resource [:room (:room args)] :args (assoc args :path "wiki/file.md")}))))
               ;; Even a trusted direct implementation call cannot turn the
               ;; host-only owner map into a nonhuman read capability.
               (binding [dops/*principal* p]
                 (let [e (failure #(impl host args))]
                   (is (= 503 (dops/error-status e)))))))
           (is (empty? @hydrations))))))))

(deftest same-bytes-retargeted-default-conflicts-before-body-reading
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn w host attached attached-descriptor]}]
     (with-api
       (fn []
         (let [p (principal w) args {:room (str (:room w))}
               before (invoke host p :simmis/workspace-bundle args)]
           (repo/write! attached "wiki/file.md" (.getBytes "# Owned\n" "UTF-8"))
           (d/transact conn [{:room/id (:room w)
                              :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
           (with-redefs [content/read-by-id (fn [& _] (throw (AssertionError. "No stale chunk body")))]
             (let [e (failure #(invoke host p :simmis/workspace-bundle-chunk
                                       (assoc args :expected-snapshot (str (:snapshot before)) :offset 0 :length 16)))]
               (is (= 409 (dops/error-status e)))
               (is (= "Workspace changed; restart the snapshot download" (ex-message e)))))))))))

(deftest native-errors-cannot-forge-public-conflict-or-leak-owner-details
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host]}]
     (with-api
       (fn []
         (let [p (principal w) args {:room (str (:room w))}
               descriptor (invoke host p :simmis/workspace-bundle args)
               forged #(throw (ex-info "SECRET native/config/path" {:type :is.simm.ops.git-snapshot/conflict
                                                                    :dvergr.ops/status 409 :secret "SECRET"}))]
           (doseq [stage [:binding :metadata :body]]
             (with-redefs [reads/resolve-host-read! (if (= stage :binding) (fn [& _] (forged)) reads/resolve-host-read!)
                           git/snapshot-id-in (if (= stage :metadata) (fn [& _] (forged)) git/snapshot-id-in)
                           content/read-by-id (fn [& _] (forged))]
               (doseq [[op input] [[:simmis/workspace-bundle args]
                                   [:simmis/workspace-bundle-chunk
                                    (assoc args :expected-snapshot (str (:snapshot descriptor)) :offset 0 :length 16)]]]
                 (let [e (failure #(invoke host p op input))]
                   (is (= 503 (dops/error-status e)))
                   (is (= "Owned workspace export unavailable" (ex-message e)))
                   (is (not (str/includes? (pr-str [(ex-message e) (ex-data e)]) "SECRET")))))))
           (with-redefs [server/current-daemon (constantly host)
                         content/read-by-id (fn [& _] (forged))]
             (let [send (@#'wire/session "2026-07-28" p)
                   reply (@#'wire/tool send "simmis_workspace-bundle" args)]
               (is (get-in reply [:result :isError]))
               (is (not (str/includes? (pr-str reply) "SECRET")))))))))))

(deftest control-owner-replacement-after-authorization-cannot-adopt-new-authority
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room host attached-descriptor hydrations]}]
     (let [replacement (world/fresh-system-conn) config (:config @replacement)]
       (try
         (d/transact replacement [{:party/id (:member w) :party/type :human :actor/kind :human}
                                  {:system/id (:registry-system attached-descriptor) :system/type :repo
                                   :system/scope "/fixture/attached"}
                                  {:room/id (:room w)
                                   :room/slug (:room/slug (d/entity @conn [:room/id (:room w)]))
                                   :room/parties [[:party/id (:member w)]]
                                   :room/default-repository [:system/id (:registry-system attached-descriptor)]}
                                  {:grant/id (random-uuid) :grant/room [:room/id (:room w)]
                                   :grant/system [:system/id (:registry-system attached-descriptor)]
                                   :grant/permission :owner}])
         (let [p (principal w) args {:room (str (:room w))} current (atom conn)
               original dops/authorize!]
           ;; Prove replacement B would independently authorize and read the
           ;; OTHER default, so a fresh-owner downgrade would not also refuse.
           (with-redefs [system-db/get-conn (constantly replacement)]
             (let [binding (reads/resolve-read! replacement (:ctx room) p (:room args))]
               (is (= "# Attachment\n"
                      (String. ^bytes (get-in (git/snapshot-in (:db binding) (:room-id binding)
                                                               (:target-stamp binding) nil)
                                              [:files "wiki/file.md" :bytes]) "UTF-8")))))
           (reset! hydrations [])
           (with-api
             #(with-redefs [system-db/get-conn (fn [] @current)
                            dops/authorize! (fn [& a] (let [result (apply original a)]
                                                        (reset! current replacement) result))]
                (let [e (failure (fn [] (invoke host p :simmis/workspace-bundle args)))]
                  (is (= 503 (dops/error-status e)))
                  (is (empty? @hydrations))))))
         (finally (d/release replacement) (d/delete-database config)))))))
