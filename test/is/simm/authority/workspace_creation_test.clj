(ns is.simm.authority.workspace-creation-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.authority.world :as wire]
            [is.simm.model.knowledge-rooms :as rooms]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.workspace-intent-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.context :as server]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.workspace-remote]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.distributed.core :as distributed]
            [org.replikativ.spindel.engine.core :as ec]))

(deftest inbound-identity-and-closed-request-control-both-fixed-handlers
  (let [{:keys [owner other request pending]} (@#'fixture/fixture)
        conn (atom pending) root (context/create-execution-context)
        previous-default (distributed/get-context :default)
        lifetime-owner (doto (lifetime/create) lifetime/open!)
        host {:execution-ctx root ::reads/owner (reads/host-owner conn root)}
        native-calls (atom [])
        wire-request {:id (str (:id request)) :name (:name request) :members []}
        principal #(hash-map :sub (str %))]
    (try
      ;; Production web startup installs this exact root in both places.
      ;; Binding only the caller thread does not bind inbound peer workers.
      (distributed/register-context! :default root)
      (with-redefs-fn {#'system-db/get-conn (constantly conn) #'server/server-context root
                      #'application/installation (atom {:host host :owner lifetime-owner})
                      #'rooms/create-for-intent! (fn [creator supplied current!]
                                                  (current!)
                                                  (swap! native-calls conj [creator supplied]))}
        #(binding [ec/*execution-context* root]
           (is (wire/refused? (wire/inbound! nil "create-workspace!" {:request wire-request})))
           (is (wire/refused? (wire/inbound! nil "observe-workspace-creation!" {:request wire-request})))
           (is (= {:result {:status :preparing}}
                  (wire/inbound! (principal owner) "create-workspace!" {:request wire-request})))
           (is (= [[owner wire-request]] @native-calls))
           (is (= {:result {:status :preparing}}
                  (wire/inbound! (principal owner) "observe-workspace-creation!" {:request wire-request})))
           (is (= 1 (count @native-calls)))
           (is (= {:result {:status :not-observed}}
                  (wire/inbound! (principal other) "observe-workspace-creation!" {:request wire-request})))
           (is (:error (wire/inbound! (principal owner) "create-workspace!"
                                      {:request (assoc wire-request :creator (str other))})))
           (is (= 1 (count @native-calls)))
           (let [agent (random-uuid)]
             (swap! conn d/db-with [{:party/id agent :party/type :agent}])
             (is (= :unavailable
                    (get-in (wire/inbound! (principal agent) "create-workspace!" {:request wire-request})
                            [:result :status])))
             (is (= 1 (count @native-calls))))))
      (finally
        (if previous-default (distributed/register-context! :default previous-default)
            (distributed/unregister-context! :default))
        (lifetime/fence! lifetime-owner) (lifetime/join! lifetime-owner 5000)
        (context/close-context! root)))))
