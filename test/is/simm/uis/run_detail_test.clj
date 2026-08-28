(ns is.simm.uis.run-detail-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [dvergr.chat.schema :as chat-schema]
            [is.simm.uis.web.desktop.run-detail :as run-detail]))

(defn- call
  ([id name] (call id name {}))
  ([id name extra]
   (merge {:id id
           :name name
           :status :completed
           :approval :auto-approved
           :error? false}
          extra)))

(deftest tool-family-is-a-display-projection
  (is (= :read (run-detail/tool-family "read_file")))
  (is (= :write (run-detail/tool-family "apply_patch")))
  (is (= :execute (run-detail/tool-family "clojure_eval")))
  (is (= :observe (run-detail/tool-family "chrome_screenshot")))
  (is (= :coordinate (run-detail/tool-family "spawn_agent")))
  (is (= :generic (run-detail/tool-family "opaque_tool"))))

(deftest exact-tool-state-has-textual-provenance
  (is (= "Failed" (run-detail/tool-status-label {:status :error :error? true})))
  (is (= "Completed" (run-detail/tool-status-label {:status :completed})))
  (is (= "Auto-approved" (run-detail/approval-label :auto-approved)))
  (is (= "Approval pending" (run-detail/approval-label :pending-approval)))
  (is (= "Rejected" (run-detail/approval-label :rejected))))

(deftest groups-routine-tool-bursts-without-losing-leaves
  (let [calls [(call "1" "read_file")
               (call "2" "read_file")
               (call "3" "read_file")
               (call "4" "shell")]
        grouped (run-detail/group-tool-activity calls)]
    (is (= 1 (count grouped)))
    (is (= :mixed (:variant (first grouped))))
    (is (= "Ran 4 tool calls" (:label (first grouped))))
    (is (= calls (:calls (first grouped))))))

(deftest failures-and-approval-boundaries-stay-visible
  (let [failed (call "3" "read_file" {:status :error :error? true})
        approval (call "5" "write_file" {:approval :pending-approval})
        calls [(call "1" "read_file")
               (call "2" "read_file")
               failed
               (call "4" "write_file")
               approval]
        grouped (run-detail/group-tool-activity calls)]
    (testing "nothing crosses an intervention point"
      (is (= [:summary :call :call :call] (mapv :kind grouped)))
      (is (= ["1" "2"] (mapv :id (:calls (first grouped))))))
    (is (= failed (:call (nth grouped 1))))
    (is (= approval (:call (nth grouped 3))))))

(deftest two-file-edits-collapse-earlier-than-reads
  (let [edits [(call "1" "edit_file") (call "2" "edit_file")]
        reads [(call "3" "read_file") (call "4" "read_file")]]
    (is (= :same-kind (:variant (first (run-detail/group-tool-activity edits)))))
    (is (= :mixed
           (:variant (first (run-detail/group-tool-activity reads)))))))

(defn- forest-ids [nodes]
  (mapcat (fn [{:keys [run children]}]
            (cons (:id run) (forest-ids children)))
          nodes))

(deftest causal-forest-uses-containment-not-global-chronology
  (let [runs [{:id "root-a" :started-at 10}
              {:id "root-b" :started-at 40}
              {:id "child-a-early" :parent-id "root-a" :started-at 20}
              {:id "child-a-late" :parent-id "root-a" :started-at 30}]
        forest (run-detail/causal-forest runs)]
    (is (= ["root-b" "root-a"] (mapv (comp :id :run) forest)))
    (is (= ["child-a-late" "child-a-early"]
           (mapv (comp :id :run) (:children (second forest)))))))

(deftest causal-forest-retains-orphans-and-breaks-cycles
  (let [runs [{:id "orphan" :parent-id "outside" :started-at 4}
              {:id "a" :parent-id "b" :started-at 3}
              {:id "b" :parent-id "a" :started-at 2}
              {:id "self" :parent-id "self" :started-at 1}]
        forest (run-detail/causal-forest runs)]
    (is (= #{"orphan" "a" "b" "self"} (set (forest-ids forest))))
    (is (:parent-missing? (first forest)))
    (is (= 4 (count (forest-ids forest))))))

(deftest queries-a-run-as-a-causal-projection
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write
             :keep-history? false}
        trigger-id (random-uuid)
        parent-id (random-uuid)
        run-id (random-uuid)
        child-id (random-uuid)
        output-id (random-uuid)
        tool-id (random-uuid)
        actor-id (random-uuid)
        actor (keyword "party" (str actor-id))
        started (java.util.Date. 1000)
        finished (java.util.Date. 2000)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn chat-schema/full-schema)
        ;; Room stores carry this small Simmis user projection in addition to
        ;; Dvergr's portable chat schema.
        (d/transact conn [{:db/ident :entity/uuid
                           :db/valueType :db.type/uuid
                           :db/unique :db.unique/identity
                           :db/cardinality :db.cardinality/one}
                          {:db/ident :S.User/display-name
                           :db/valueType :db.type/string
                           :db/cardinality :db.cardinality/one}])
        (d/transact conn [{:entity/uuid actor-id
                           :S.User/display-name "Vár"}
                          {:message/id trigger-id
                           :message/role :user
                           :message/from :party/human
                           :message/content "Investigate the failing build"
                           :message/created-at started}
                          {:run/id parent-id
                           :run/kind :agent-turn
                           :run/actor actor
                           :run/trigger trigger-id
                           :run/status :completed
                           :run/created-at started
                           :run/started-at started
                           :run/updated-at finished
                           :run/ended-at finished}
                          {:run/id run-id
                           :run/kind :agent-turn
                           :run/actor actor
                           :run/trigger trigger-id
                           :run/parent parent-id
                           :run/status :running
                           :run/created-at started
                           :run/started-at started
                           :run/updated-at finished}
                          {:run/id child-id
                           :run/kind :delegation
                           :run/actor actor
                           :run/trigger output-id
                           :run/parent run-id
                           :run/status :queued
                           :run/created-at finished
                           :run/updated-at finished}
                          {:message/id output-id
                           :message/role :assistant
                           :message/from actor
                           :message/content "The failure is isolated."
                           :message/created-at finished
                           :message/run-id run-id}
                          {:tool-call/id tool-id
                           :tool-call/name "read_file"
                           :tool-call/input "{:path \"ci.edn\"}"
                           :tool-call/result "configuration"
                           :tool-call/duration-ms 17
                           :tool-call/error? false
                           :tool-call/status :completed
                           :tool-call/approval :auto-approved
                           :tool-call/run-id run-id
                           :tool-call/started-at started}])
        (let [{:keys [run trigger parent children messages tool-calls]}
              (run-detail/query-run-detail @conn run-id)]
          (is (= (str run-id) (:id run)))
          (is (= "Vár" (:actor-name run)))
          (is (= :running (:status run)))
          (is (= "Investigate the failing build" (:content trigger)))
          (is (= (str parent-id) (:id parent)))
          (is (= [(str child-id)] (mapv :id children)))
          (is (= [(str output-id)] (mapv :id messages)))
          (is (= [{:name "read_file"
                   :result "configuration"
                   :duration-ms 17}]
                 (mapv #(select-keys % [:name :result :duration-ms]) tool-calls))))
        (finally
          (d/release conn)
          (d/delete-database cfg))))))
