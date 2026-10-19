(ns is.simm.ops.knowledge-admission-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.system.rooms :as rooms]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.ops.knowledge-admission :as admission]))

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
