(ns is.simm.authority.human-evaluation-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.authority.world :as world]
            [is.simm.documents.evaluation :as evaluation]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.memory-remote :as remote]))

(deftest inbound-human-evaluation-rechecks-authority-and-keeps-original-owner
  (@#'fixture/with-fixture
   true
   (fn [{:keys [host w]}]
     (let [owner (doto (lifetime/create) lifetime/open!)
           request {:syntax :clojure :text "(+ 2 3)" :namespace "user"}]
       (try
         (with-redefs-fn
           {#'application/installation (atom {:host host :owner owner})}
           (fn []
             (doseq [kind [:non-member :anon]]
               (is (world/refused?
                    (world/inbound! (world/principal w kind) "evaluate-memory-repl!"
                                    {:room-id-str (str (:room w)) :request request}))))
             (let [reply (:result (world/inbound! (world/principal w :owner) "evaluate-memory-repl!"
                                                  {:room-id-str (str (:room w)) :request request}))]
               (is (:success reply) (pr-str reply))
               (is (= "5" (:value reply)))
               (is (:worker-finished? reply)))
             (let [queued (remote/prepare-evaluation (:owner w) (str (:room w)) :repl request)
                   replacement (doto (lifetime/create) lifetime/open!) calls (atom 0)]
               (try
                 (lifetime/fence! owner)
                 (with-redefs-fn
                   {#'application/installation (atom {:host host :owner replacement})
                    #'evaluation/evaluate-repl! (fn [& _] (swap! calls inc))}
                   (fn []
                     (is (thrown? Exception (queued)))
                     (is (zero? @calls))))
                 (finally (lifetime/fence! replacement) (lifetime/join! replacement 30000))))))
         (finally (lifetime/fence! owner) (lifetime/join! owner 30000)))))))
