(ns is.simm.authority.github-remote-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.authority.world :as world]
            [is.simm.ops.github-service :as service]
            [is.simm.ops.github-decisions :as decisions]
            [is.simm.uis.web.desktop.github-remote]))

(deftest inbound-git-actions-capture-the-actual-human-and-retain-room-policy
  (world/with-world [conn w]
    (let [calls (atom []) request {:client-id (str (random-uuid))}
          capture (fn [party room operation supplied]
                    (swap! calls conj [party room operation supplied])
                    (fn [] {:status :not-observed}))]
      (with-redefs [service/prepare capture decisions/prepare capture]
        (doseq [[method operation allowed] [["load-github-memory-sync!" :observe #{:owner :member}]
                                            ["sync-github-memory!" :prepare #{:owner}]
                                            ["sync-github-memory!" :prepare-incoming #{:owner}]
                                            ["github-proposal-operation!" :review #{:owner}]
                                            ["github-proposal-operation!" :accept #{:owner}]
                                            ["github-proposal-operation!" :dismiss #{:owner}]
                                            ["retire-github-memory-sync!" :retire #{:owner}]]]
          (doseq [kind [:owner :member :non-member :admin :anon]]
            (reset! calls [])
            (let [args (cond-> {:room-id-str (str (:room w)) :request request}
                         (#{"sync-github-memory!" "github-proposal-operation!"} method) (assoc :operation operation))
                  reply (world/inbound! (world/principal w kind) method args)]
              (if (allowed kind)
                (do (is (= :not-observed (get-in reply [:result :status])))
                    (is (= [[(get w kind) (:room w) operation request]] @calls)))
                (do (is (world/refused? reply)) (is (empty? @calls)))))))
        (reset! calls [])
        (is (:error (world/inbound! (world/principal w :owner) "sync-github-memory!"
                                    {:room-id-str (str (:room w)) :operation :retire :request request})))
        (is (empty? @calls))))))
