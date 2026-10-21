(ns is.simm.authority.workflows-remote-test
  "Browser workflow RPCs cross both the inbound gate and canonical op authority."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as dops]
            [geschichte.repo :as repo]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp-test :as owned-fixture]
            [is.simm.ops.owned-reads :as owned-reads]
            [is.simm.ops.owned-workflows :as owned-workflows]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.work-test]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as fixture]
            [is.simm.runtimes.context :as context]
            [is.simm.uis.web.desktop.workflows-remote :as remote]))

(defn- with-api [f]
  (let [previous (dops/operations)]
    (try
      (api/install!)
      (f)
      (finally
        (doseq [op (keys api/specifications)]
          (if-let [spec (get previous op)]
            (dops/register-op! op spec)
            (dops/unregister-op! op)))
        (server/sync-op-tools!)))))

(deftest browser-discovery-source-and-saves-use-the-authenticated-room-on-every-call
  (doseq [order [true false]]
    (@#'owned-fixture/with-fixture
     order
     (fn [{:keys [conn w room owned]}]
       (doseq [[path text] fixture/fixture]
         (repo/write! owned (str "workflows/echo/" path) (.getBytes ^String text "UTF-8")))
       (repo/stage-all! owned)
       (repo/commit! owned {:message "Seed canonical workflow baseline"})
       (with-api
         (fn []
           (with-redefs [context/server-context (:ctx room)
                         rooms/live-room #(when (= % (:room w)) room)]
             (let [args {:room-id-str (str (:room w))}
                   inbound (fn [kind op more]
                             (world/inbound! (world/principal w kind) op (merge args more)))
                   source #(inbound % "load-workflow-source!" {:workflow-name "echo"})
                   listing #(inbound % "load-workflow-list!" {:options {}})]
               (doseq [kind [:owner :member]]
                 (is (= ["echo"] (get-in (listing kind) [:result :names])))
                 (let [loaded (:result (source kind))]
                   (is (= fixture/fixture (:files loaded)))
                   (is (false? (get-in loaded [:inspection :checker-executed?])))
                   (is (true? (get-in loaded [:permissions :write?])))
                   (is (= (= kind :owner) (get-in loaded [:permissions :merge?])))))
               ;; A room argument hidden in the options cannot retarget the
               ;; fixed UUID the inbound gate authorized.
               (is (:error (inbound :member "load-workflow-list!"
                                    {:options {:room "different-room"}})))
               (doseq [kind [:non-member :admin :anon]]
                 (is (world/refused? (listing kind)))
                 (is (world/refused? (source kind)))
                 (is (world/refused?
                      (inbound kind "save-workflow-source!"
                               {:edits {:name "echo" :files {"workflow.edn" "forged"}}}))))
               (let [revision (:revision (:result (source :member)))
                     saved (inbound :member "save-workflow-source!"
                                    {:edits {:name "echo"
                                             :expected-revision revision :files {"notes.md" "member edit"}}})]
                 (is (some? (get-in saved [:result :write :commit])))
                 (is (= "member edit" (String. ^bytes (repo/read owned "workflows/echo/notes.md") "UTF-8")))
                 (is (:error (inbound :member "save-workflow-source!"
                                      {:edits {:name "echo" :expected-revision revision
                                               :files {"notes.md" "stale"}}}))))
               (is (:error (inbound :member "load-workflow-list!" {:options {:eval true}})))
               (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
               (is (world/refused? (listing :member)))
               (is (world/refused? (source :member)))
               (is (world/refused?
                    (inbound :member "save-workflow-source!"
                             {:edits {:name "echo" :files {"notes.md" "revoked"}}})))))))))))

(deftest browser-save-does-not-adopt-a-newer-definition-after-its-write-ack
  (doseq [order [true false]]
    (@#'owned-fixture/with-fixture
     order
     (fn [{:keys [w room owned host]}]
       (doseq [[path text] fixture/fixture]
         (repo/write! owned (str "workflows/echo/" path) (.getBytes ^String text "UTF-8")))
       (repo/stage-all! owned)
       (repo/commit! owned {:message "Seed canonical workflow ACK baseline"})
       (with-api
         (fn []
           (let [save api/owned-workflow-save!
                 before (remote/invoke-workflow (:owner w) (str (:room w)) :simmis/workflow-source {:name "echo"})
                 revision (:revision before)
                 raced (atom false)]
             (with-redefs [context/server-context (:ctx room)
                           rooms/live-room #(when (= % (:room w)) room)
                           api/owned-workflow-save! (fn [& args]
                                                      (let [reply (apply save args)]
                                                        (when (compare-and-set! raced false true)
                                                          (repo/write! owned "workflows/echo/workflow.edn"
                                                                       (.getBytes "{:title \"Newer\" :task \"Different task\"}" "UTF-8")))
                                                        reply))]
               (let [reply (world/inbound! (world/principal w :owner) "save-workflow-source!"
                                           {:room-id-str (str (:room w))
                                            :edits {:name "echo" :expected-revision revision
                                                    :files {"notes.md" "my saved edit"}}})
                     result (:result reply)]
                 (is @raced)
                 (is (some? (get-in result [:write :commit])))
                 (is (not (:source-changed? result)))
                 (is (= (:definition before) (:definition result)))
                 (is (string? (get-in result [:workflow :revision])))
                 (is (not= (get-in result [:workflow :revision])
                           (get-in (owned-workflows/details! host (ops/party-principal (:owner w))
                                                             (str (:room w)) {:name "echo"}) [:data :revision])))
                 (is (= "my saved edit" (String. ^bytes (repo/read owned "workflows/echo/notes.md") "UTF-8"))))))))))))

(deftest canonical-workflow-op-guard-is-not-replaced-by-browser-permission-hints
  (doseq [order [true false]]
    (@#'owned-fixture/with-fixture
     order
     (fn [{:keys [w room]}]
       (with-api
         (fn []
           (with-redefs [context/server-context (:ctx room)
                         rooms/live-room #(when (= % (:room w)) room)]
             (let [invoke @#'is.simm.uis.web.desktop.workflows-remote/invoke-workflow
                   reads (atom 0)
                   details owned-workflows/details!]
               ;; Even a direct adapter invocation must pass canonical op
               ;; authority before its browser-only full-source read.
               (with-redefs [owned-workflows/details! (fn [& args] (swap! reads inc) (apply details args))]
                 (is (thrown? clojure.lang.ExceptionInfo
                              (invoke (:non-member w) (str (:room w)) :simmis/workflow-source {:name "echo"})))
                 (is (zero? @reads)))
               (is (thrown? clojure.lang.ExceptionInfo
                            (invoke nil (str (:room w)) :simmis/workflow-list {})))
               (doseq [kind [:member :non-member :admin :anon]
                       [op arg] [["submit-workflow-run!" :request]
                                 ["submit-workflow-benchmark!" :request]
                                 ["provision-workflow-allowance!" :request]]]
                 (is (world/refused?
                      (world/inbound! (world/principal w kind) op
                                      {:room-id-str (str (:room w)) arg {:eval true}}))))))))))))

(defn- owned-source! [conn files]
  (doseq [[path text] files]
    (repo/write! conn (str "workflows/echo/" path) (.getBytes ^String text "UTF-8"))))

(defn- owned-failure [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error error)))

(deftest dormant-owned-browser-reads-use-one-normal-human-capture
  (doseq [owned-first? [true false]]
    (@#'owned-fixture/with-fixture
     owned-first?
     (fn [{:keys [w owned attached hydrations]}]
       (owned-source! owned fixture/fixture)
       (owned-source! attached (assoc fixture/fixture "gold.edn" "{:answer :OTHER}"))
       (let [resolve owned-reads/resolve-host-read! captures (atom [])
             expected (:data (owned-workflows/details!
                              {:execution-ctx context/server-context
                               ::owned-reads/owner (owned-reads/host-owner (system-db/get-conn) context/server-context)}
                              (ops/party-principal (:member w)) (str (:room w)) {:name "echo"}))]
         (reset! hydrations [])
         (with-redefs [owned-reads/resolve-host-read!
                       (fn [host principal handle]
                         (swap! captures conj principal)
                         (is (not (:principal/mcp? principal)))
                         (resolve host principal handle))
                       ops/invoke (fn [& _] (throw (AssertionError. "No MCP or selected op dispatch")))
                       workflows/details (fn [& _] (throw (AssertionError. "No second selected details read")))
                       workflows/source-page (fn [& _] (throw (AssertionError. "No selected page read")))]
           (let [listing (remote/invoke-owned-workflow-read (:member w) (str (:room w)) :simmis/workflow-list {})
                 result (remote/invoke-owned-workflow-read (:member w) (str (:room w)) :simmis/workflow-source {:name "echo"})]
             (is (= ["echo"] (:names listing)))
             (is (= expected (dissoc result :permissions)))
             (is (= fixture/fixture (:files result)))
             (is (true? (get-in result [:permissions :write?])))
             (is (false? (get-in result [:permissions :merge?])))
             (is (not (contains? result :read-context)))))
         (is (= 2 (count @captures)))
         (is (= [(:room w) (:room w)] @hydrations)))))))

(deftest owned-browser-permissions-use-the-exact-captured-control-db
  (@#'owned-fixture/with-fixture
   false
   (fn [{:keys [conn w owned]}]
     (owned-source! owned fixture/fixture)
     (let [details owned-workflows/details! can? access/can? get-conn system-db/get-conn
           after? (atom false) captured (atom nil) permission-dbs (atom [])]
       (with-redefs [owned-workflows/details!
                     (fn [& args]
                       (let [envelope (apply details args)]
                         (reset! captured (get-in envelope [:read-context :control-db]))
                         (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
                         (reset! after? true)
                         envelope))
                     system-db/get-conn (fn [] (when @after? (throw (AssertionError. "No post-capture control read"))) (get-conn))
                     access/can? (fn [db party action resource]
                                   (when @after? (swap! permission-dbs conj db))
                                   (can? db party action resource))]
         (let [result (remote/invoke-owned-workflow-read (:owner w) (str (:room w)) :simmis/workflow-source {:name "echo"})]
           (is (true? (get-in result [:permissions :fund?])))
           (is (true? (get-in result [:permissions :merge?])))
           (is (= 2 (count @permission-dbs)))
           (is (every? #(identical? @captured %) @permission-dbs))))
       (is (not= (:owner w) (d/q '[:find ?p . :in $ ?id :where [?r :room/id ?id] [?r :room/owner ?o] [?o :party/id ?p]] @conn (:room w))))))))

(deftest owned-browser-refuses-retargeting-and-sanitizes-unknown-permission-errors
  (@#'owned-fixture/with-fixture
   true
   (fn [{:keys [w owned hydrations]}]
     (owned-source! owned fixture/fixture)
     (let [invoke #(remote/invoke-owned-workflow-read %1 (str (:room w)) %2 %3)]
       (doseq [[party op args status]
               [[nil :simmis/workflow-list {} 403]
                [(:member w) :simmis/workflow-write {} 400]
                [(:member w) :simmis/workflow-list {:room (str (:room w))} 400]
                [(:member w) :simmis/workflow-source {:name "echo" :room "other"} 400]
                [(:member w) :simmis/workflow-list {:eval true} 400]
                [(:non-member w) :simmis/workflow-list {} 503]]]
         (let [error (owned-failure #(invoke party op args))]
           (is (= status (dops/error-status error)))))
       (is (empty? @hydrations))
       (let [can? access/can?]
         (with-redefs [access/can?
                       (fn [db party action resource]
                         (if (= :write action)
                           (throw (ex-info "SECRET/permission" {:type ::owned-workflows/conflict}))
                           (can? db party action resource)))]
           (let [error (owned-failure #(invoke (:member w) :simmis/workflow-list {}))]
             (is (= 503 (dops/error-status error)))
             (is (not (re-find #"SECRET" (pr-str [(ex-message error) (ex-data error)])))))))))))
