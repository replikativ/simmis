(ns is.simm.authority.result-rpc-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as store]
            [dvergr.room.store.memory :as memory]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.authority.world :as world]
            [is.simm.ops.experiments-test]
            [is.simm.runtimes.context :as context]
            [is.simm.uis.web.desktop.results-remote]
            [org.replikativ.spindel.engine.context :as ectx]
            [org.replikativ.spindel.engine.core :as ec]))

(deftest browser-results-use-canonical-room-authority-on-every-read
  (world/with-world [conn w]
    (@#'is.simm.ops.experiments-test/with-experiment-room
     (fn [room]
       (let [other (discourse/make-room {:id :empty-results-room :store (memory/make)
                                         :ctx (ectx/fork-context (:ctx room))})
             attempt (:attempt/id (first (store/-list-attempts (:store room) (store/conversation-id room) {:limit 1})))
             args {:room-id-str (str (:room w)) :attempt-id-str (str attempt) :offset 0}
             read! (fn [kind name args] (world/inbound! (world/principal w kind) name args))]
         (try
           (registry/register! other)
           (d/transact conn [{:room/id (:room w) :room/slug (name (:id room))}
                             {:room/id (:other-room w) :room/slug (name (:id other))
                              :room/parties [:party/id (:member w)]}])
           (with-redefs [context/server-context (:ctx room)
                         rooms/live-room (fn [id] (cond (= id (:room w)) room (= id (:other-room w)) other))]
             (is (= ["/out/result.md"] (mapv :path (get-in (read! :member "load-attempt-files!" args) [:result :files]))))
             (let [chunk (read! :member "load-attempt-file!" (assoc args :path "/out/result.md"))]
               (is (= "captured attempt output"
                      (String. (.decode (java.util.Base64/getDecoder) ^String (get-in chunk [:result :base64])) "UTF-8"))))
             (doseq [kind [:non-member :admin :anon]
                     [op args] [["load-attempt-files!" args]
                                ["load-attempt-file!" (assoc args :path "/out/result.md")]]]
               (is (world/refused? (read! kind op args))))
             ;; The member may read BOTH rooms, but an Attempt names evidence
             ;; only in its own room. Never search another store on a miss.
             (is (:error (read! :member "load-attempt-files!" (assoc args :room-id-str (str (:other-room w))))))
             (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
             (is (world/refused? (read! :member "load-attempt-file!" (assoc args :path "/out/result.md" :offset 1)))))
           (finally (registry/unregister! (:id other)) (discourse/close-room! other))))))))
