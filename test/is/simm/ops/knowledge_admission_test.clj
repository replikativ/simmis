(ns is.simm.ops.knowledge-admission-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.room.store :as store]
            [dvergr.system.db :as registry-db]
            [dvergr.system.rooms :as rooms]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.ops.work-test :as work-test]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]))

(deftest default-target-is-selected-by-logical-room-not-current-attachments
  (let [id (random-uuid) ctx (Object.) room {:ctx ctx}
        conn (Object.) system (random-uuid) calls (atom [])]
    (with-redefs [room-agents/registered-room (fn [rid] (is (= id rid)) room)
                  rooms/room-ctx-for (fn [rid] (is (= id rid)) ctx)
                  rooms/default-workspace (fn [rid]
                                            (swap! calls conj rid)
                                            {:conn conn :registry-system system :runtime-system "owned-runtime"})]
      (let [target (admission/default-target id room)]
        (is (identical? conn (:conn target)))
        (is (= {:registry-system system :runtime-system "owned-runtime"} (:identity target)))
        (is (= [id] @calls))))))

(deftest stale-forked-or-unconfigured-rooms-never-fall-back-to-an-attachment
  (let [id (random-uuid) ctx (Object.) room {:ctx ctx} other {:ctx ctx}]
    (with-redefs [room-agents/registered-room (constantly room)
                  rooms/room-ctx-for (constantly ctx)
                  rooms/default-workspace (fn [& _] (throw (AssertionError. "Invalid room reached workspace")))]
      (is (thrown? clojure.lang.ExceptionInfo (admission/default-target nil room)))
      (is (thrown? clojure.lang.ExceptionInfo (admission/default-target id other)))
      (with-redefs [rooms/room-ctx-for (constantly (Object.))]
        (is (thrown? clojure.lang.ExceptionInfo (admission/default-target id room)))))
    (with-redefs [room-agents/registered-room (constantly room)
                  rooms/room-ctx-for (constantly ctx)
                  rooms/default-workspace (fn [& _] (throw (ex-info "Default not configured" {})))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Default not configured"
                            (admission/default-target id room))))))

(deftest serialized-writer-resolves-the-room-registry-in-its-own-root
  (@#'work-test/with-room
   (fn [room]
     (let [cfg {:store {:backend :memory :id (random-uuid)}
                :schema-flexibility :write :keep-history? false}
           id (random-uuid) connection (Object.)
           identity {:registry-system (random-uuid) :runtime-system "owned-runtime"}
           foreign (context/create-execution-context)]
       (d/create-database cfg)
       (let [conn (d/connect cfg)]
         (try
           (d/transact conn registry-db/schema)
           (d/transact conn [{:room/id id :room/slug (store/room-id->slug (:id room))}])
           (with-redefs [rooms/room-ctx-for (fn [rid] (is (= id rid)) (:ctx room))
                         room-agents/registered-room
                         (fn [& _] (throw (AssertionError. "Ambient control-room lookup")))
                         rooms/default-workspace
                         (fn [db rid]
                           (is (= id rid))
                           (is (= (store/room-id->slug (:id room))
                                  (:room/slug (d/entity db [:room/id id]))))
                           {:conn connection :registry-system (:registry-system identity)
                            :runtime-system (:runtime-system identity)})]
             ;; Real db.fn execution and real Tier-1 Room registry lookup.
             ;; Only the upstream default resolver is isolated (its explicit
             ;; snapshot behavior has separate registry integration coverage).
             (doseq [ambient [nil foreign]]
               (binding [ec/*execution-context* ambient]
                 (d/transact conn
                             [[:db.fn/call
                               (fn [db]
                                 (let [selected (admission/default-target db id room)]
                                   (is (identical? connection (:conn selected)))
                                   (is (= identity (:identity selected))))
                                 [])]]))))
           (finally
             (d/release conn)
             (d/delete-database cfg)
             (context/close-context! foreign))))))))
