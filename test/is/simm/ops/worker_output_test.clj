(ns is.simm.ops.worker-output-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.jobs :as jobs]
            [dvergr.resource :as resource]
            [dvergr.rooms.files :as room-files]
            [dvergr.room.store :as store]
            [is.simm.authority.world :as world]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.work :as work]
            [is.simm.ops.work-test :as work-fixture]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-files-test :as file-fixture]
            [is.simm.ops.worker-output :as output]
            [is.simm.ops.worker-credentials :as credentials]
            [muschel.fs :as fs]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Base64]))

(defn- binary-file []
  (let [bytes (byte-array (map unchecked-byte [0 255 128 42]))]
    {:base64 (.encodeToString (Base64/getEncoder) bytes) :bytes 4
     :sha256 (files/sha256 bytes) :mode 0x81ED}))

(defn- run-import [conn w room behavior]
  (let [[input prepared] (@#'work-fixture/fixture)
        revoke-after-content? (= true behavior)
        host-ctx ec/*execution-context*
        enrollment {:simmis.worker/by (:owner w) :simmis.worker/room (:room w)}
        permitted? #(binding [ec/*execution-context* host-ctx]
                      (credentials/permitted? @conn enrollment))
        frozen (files/freeze room "import fixture" file-fixture/config ["docs/input.md"] file-fixture/limits)
        files {"docs/input.md" (when-not (= :delete-input behavior) (file-fixture/encoded "updated"))
               "out/program.bin" (binary-file)}
        proposed {:files files :summary "binary result" :exit-code 0}
        protocol (evaluation/make-protocol
                  {:id :worker-output-test/import :version 1
                   :run (fn [{world :room id :run/id}]
                          (output/import! world id frozen proposed permitted?))})
        evaluator (evaluation/make-evaluator
                   {:id :worker-output-test/evidence :version 1
                    :capture (fn [{world :world/room}]
                               (if revoke-after-content? {}
                                   {:files (into {} (map (fn [[path file]] [(str "/" path) file]))
                                                 (:files (files/freeze world "capture" file-fixture/config
                                                                       (vec (keep (fn [[path file]] (when file path)) files))
                                                                       file-fixture/limits)))}))
                    :observe (fn [{:keys [default] evidence :execution/evidence}] (merge default evidence))
                    :verify (fn [_ evidence]
                              (let [done? (= :completed (get-in evidence [:trace :runs 0 :run/status]))]
                                {:checks {:completed? done?} :reward (if done? 1.0 0.0)}))})
        env (environment/make-environment
             {:id :worker-output-test/environment :task "import fixture"
              :verifier {:id :worker-output-test/evidence :version 1}
              :limits {:timeout-ms 10000 :cancel-timeout-ms 1000}
              :world {:isolation :ctx :settlement :discard :effects work/effects
                      :protocol (evaluation/protocol-ref protocol) :resources (work/resources (:limits input))}})
        prepared (assoc prepared :environment env :protocol protocol :evaluator evaluator
                        :basis {:environment env :protocol (evaluation/protocol-ref protocol)})
        write room-files/write! chmod fs/chmod grant (work/resources (:limits input))]
    (resource/mint! room {:id (random-uuid) :resources grant})
    (with-redefs [fs/chmod (if (= :noop-mode behavior) (constantly true) chmod)
                  room-files/write! (fn [& args]
                                      (let [result (apply write args)]
                                        (when revoke-after-content?
                                          (binding [ec/*execution-context* host-ctx]
                                            (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                                                              [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])))
                                        result))]
      (let [submitted (work/submit-prepared! conn host-ctx (ops/party-principal (:owner w))
                                             (:room w) room input prepared)
            status (jobs/status (:job submitted) 25000)
            entry (first (store/-list-attempts (:store room) (store/conversation-id room) {}))]
        (is (:settled? status))
        (is (= grant (resource/balance room)))
        {:status status
         :attempt (store/-load-attempt (:store room) (store/conversation-id room) (:attempt/id entry))}))))

(deftest isolated-run-import-preserves-binary-and-executable-modes-before-discard
  (world/with-world [conn w]
    (@#'work-fixture/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (workspace/seed! room {"/docs/input.md" "original"})
       (let [{:keys [attempt]} (run-import conn w room false)]
         (is (= 1.0 (get-in attempt [:attempt/receipt :attempt/reward])))
         (is (= (binary-file) (get-in attempt [:attempt/evidence :files "/out/program.bin"])))
         (is (= (file-fixture/encoded "updated") (get-in attempt [:attempt/evidence :files "/docs/input.md"])))
         (is (= {"/docs/input.md" "original"} (workspace/read-tree room "/"))
             "discarded Run output never touched the parent"))))))

(deftest revocation-after-content-commit-refuses-the-next-effect-and-positive-certification
  (world/with-world [conn w]
    (@#'work-fixture/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (workspace/seed! room {"/docs/input.md" "original"})
       (let [{:keys [attempt]} (run-import conn w room true)]
         (is (zero? (get-in attempt [:attempt/receipt :attempt/reward])))
         (is (= :failed (get-in attempt [:attempt/evidence :trace :runs 0 :run/status])))
         (is (= {"/docs/input.md" "original"} (workspace/read-tree room "/"))))))))

(deftest an-intercepted-mode-write-cannot-be-reported-as-successful-import
  (world/with-world [conn w]
    (@#'work-fixture/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (workspace/seed! room {"/docs/input.md" "original"})
       (let [{:keys [attempt]} (run-import conn w room :noop-mode)]
         (is (zero? (get-in attempt [:attempt/receipt :attempt/reward])))
         (is (= :failed (get-in attempt [:attempt/evidence :trace :runs 0 :run/status])))
         (is (= 0x81A4 (get-in attempt [:attempt/evidence :files "/out/program.bin" :mode])))
         (is (= {"/docs/input.md" "original"} (workspace/read-tree room "/"))))))))

(deftest deleting-a-selected-input-is-recorded-without-deleting-the-parent-file
  (world/with-world [conn w]
    (@#'work-fixture/with-room
     (fn [room]
       (workspace/ensure-workspace! room)
       (workspace/seed! room {"/docs/input.md" "original"})
       (let [{:keys [attempt]} (run-import conn w room :delete-input)]
         (is (= 1.0 (get-in attempt [:attempt/receipt :attempt/reward])))
         (is (= ["/docs/input.md"] (get-in attempt [:attempt/evidence :result :removed])))
         (is (= #{"/out/program.bin"} (set (keys (get-in attempt [:attempt/evidence :files])))))
         (is (= {"/docs/input.md" "original"} (workspace/read-tree room "/"))))))))

(deftest a-parent-room-is-never-an-import-target
  (@#'work-fixture/with-room
   (fn [room]
     (workspace/ensure-workspace! room)
     (is (thrown? clojure.lang.ExceptionInfo
                  (output/import! room (random-uuid) {:files {}}
                                  {:files {"out/program.bin" (binary-file)} :summary "no" :exit-code 0}
                                  (constantly true))))
     (is (= {} (workspace/read-tree room "/"))))))
