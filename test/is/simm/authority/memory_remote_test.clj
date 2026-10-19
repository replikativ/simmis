(ns is.simm.authority.memory-remote-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [dvergr.rooms.files :as room-files]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.authority.world :as world]
            [is.simm.documents.files :as files]
            [is.simm.runtimes.context :as context]
            [is.simm.uis.web.desktop.memory-remote]))

(defn- with-room [f]
  (let [room (discourse/make-room {:id (keyword (str "memory-authority-" (random-uuid)))
                                   :store (memory/make)})]
    (try (workspace/ensure-workspace! room) (f room)
         (finally (discourse/close-room! room)))))

(deftest memory-rpcs-use-live-room-authority-and-do-not-execute-code
  (world/with-world [conn w]
    (with-room
      (fn [room]
        (files/save-file! room {:path "wiki/hello.md" :expected nil :text "# Hello\n"})
        (files/save-file! room {:path "src/core.clj" :expected nil
                               :text "(throw (ex-info \"Not an eval endpoint\" {}))"})
        (with-redefs [context/server-context (:ctx room)
                      rooms/live-room #(when (= % (:room w)) room)]
          (let [inbound (fn [kind op args]
                          (world/inbound! (world/principal w kind) op
                                          (assoc args :room-id-str (str (:room w)))))]
            (doseq [kind [:owner :member]]
              (is (= ["src" "wiki"]
                     (mapv :name (get-in (inbound kind "load-memory-folder!" {:folder ""}) [:result :entries]))))
              (is (= :source-only
                     (get-in (inbound kind "load-memory-file!" {:path "src/core.clj"}) [:result :execution-policy])))
              (is (= "# Hello\n"
                     (get-in (inbound kind "load-memory-file!" {:path "wiki/hello.md"}) [:result :text]))))
            (doseq [kind [:non-member :admin :anon]]
              (is (world/refused? (inbound kind "load-memory-folder!" {:folder ""})))
              (is (world/refused? (inbound kind "load-memory-file!" {:path "src/core.clj"})))
              (is (world/refused? (inbound kind "save-memory-file!"
                                         {:request {:path "forged.md" :expected nil :text "forged"}}))))
            (is (:error (inbound :member "save-memory-file!"
                                 {:request {:path "wiki/new.md" :expected nil :text "bad" :room "elsewhere"}})))
            (let [write room-files/write!]
              (with-redefs [room-files/write! (fn [& args]
                                               (d/transact conn [[:db/retract [:room/id (:room w)]
                                                                  :room/parties [:party/id (:member w)]]])
                                               (apply write args))]
                (is (:error (inbound :member "save-memory-file!"
                                     {:request {:path "wiki/revoked.md" :expected nil :text "revoked"}})))))
            (is (= ["hello.md"] (mapv :name (:entries (files/list-folder room "wiki")))))
            (is (world/refused? (inbound :member "load-memory-folder!" {:folder ""})))))))))
