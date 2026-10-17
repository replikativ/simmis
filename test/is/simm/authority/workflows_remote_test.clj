(ns is.simm.authority.workflows-remote-test
  "Browser workflow RPCs cross both the inbound gate and canonical op authority."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as dops]
            [dvergr.room.store :as store]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.authority.world :as world]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.work-test]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.workflows-test :as fixture]
            [is.simm.runtimes.context :as context]
            [is.simm.uis.web.desktop.workflows-remote]))

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
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (workflows/write! room {:principal/id "fixture"} {:name "echo" :files fixture/fixture})
       (d/transact conn [{:room/id (:room w) :room/slug (store/room-id->slug (:id room))}])
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
               (is (= ["echo"]
                      (get-in (inbound :member "load-workflow-list!"
                                       {:options {:room "different-room"}}) [:result :names])))
               (doseq [kind [:non-member :admin :anon]]
                 (is (world/refused? (listing kind)))
                 (is (world/refused? (source kind)))
                 (is (world/refused?
                      (inbound kind "save-workflow-source!"
                               {:edits {:name "echo" :files {"workflow.edn" "forged"}}}))))
               (let [revision (:revision (:result (source :member)))
                     saved (inbound :member "save-workflow-source!"
                                    {:edits {:room "different-room" :name "echo"
                                             :expected-revision revision :files {"notes.md" "member edit"}}})]
                 (is (some? (get-in saved [:result :write :commit])))
                 (is (= "member edit" (get-in (workflows/details room "echo") [:files "notes.md"])))
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
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (workflows/write! room {:principal/id "fixture"} {:name "echo" :files fixture/fixture})
       (d/transact conn [{:room/id (:room w) :room/slug (store/room-id->slug (:id room))}])
       (with-api
         (fn []
           (let [invoke ops/invoke
                 revision (:revision (workflows/details room "echo"))
                 raced (atom false)]
             (with-redefs [context/server-context (:ctx room)
                           rooms/live-room #(when (= % (:room w)) room)
                           ops/invoke (fn [ctx principal operation args]
                                        (let [reply (invoke ctx principal operation args)]
                                          (when (and (= :simmis/workflow-write operation)
                                                     (compare-and-set! raced false true))
                                            (workflows/write! room {:principal/id "other-editor"}
                                                              {:name "echo" :files {"workflow.edn" "{:title \"Newer\" :task \"Different task\"}"}}))
                                          reply))]
               (let [reply (world/inbound! (world/principal w :owner) "save-workflow-source!"
                                           {:room-id-str (str (:room w))
                                            :edits {:name "echo" :expected-revision revision
                                                    :files {"notes.md" "my saved edit"}}})
                     result (:result reply)]
                 (is @raced)
                 (is (some? (get-in result [:write :commit])))
                 (is (true? (:source-changed? result)))
                 (is (not (contains? result :definition)))
                 (is (string? (get-in result [:workflow :revision])))
                 (is (not= (get-in result [:workflow :revision]) (:revision (workflows/details room "echo"))))
                 (is (= "my saved edit" (get-in (workflows/details room "echo") [:files "notes.md"]))))))))))))

(deftest canonical-workflow-op-guard-is-not-replaced-by-browser-permission-hints
  (world/with-world [conn w]
    (@#'is.simm.ops.work-test/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (d/transact conn [{:room/id (:room w) :room/slug (store/room-id->slug (:id room))}])
       (with-api
         (fn []
           (with-redefs [context/server-context (:ctx room)
                         rooms/live-room #(when (= % (:room w)) room)]
             (let [invoke @#'is.simm.uis.web.desktop.workflows-remote/invoke-workflow
                   reads (atom 0)
                   details workflows/details]
               ;; Even a direct adapter invocation must pass canonical op
               ;; authority before its browser-only full-source read.
               (with-redefs [workflows/details (fn [& args] (swap! reads inc) (apply details args))]
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
