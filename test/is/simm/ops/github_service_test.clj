(ns is.simm.ops.github-service-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-incoming :as incoming]
            [is.simm.ops.github-push :as push]
            [is.simm.ops.github-credentials :as credentials]
            [is.simm.ops.github-service :as service]))

(deftest scheduled-entry-uses-the-retained-host-and-never-recaptures-an-app
  (let [original (Object.) party (random-uuid) room (random-uuid)
        client (random-uuid) stamp (random-uuid) calls (atom [])
        context {:resolved {:party party :room-id room} :conn (atom nil)}]
    (with-redefs [host/capture (fn [& _] (throw (AssertionError. "Captured a replacement App")))
                  host/in! (fn [supplied p r action body]
                             (is (identical? original supplied))
                             (is (= [party room :prepare] [p r action]))
                             (body context))
                  journal/sync-in (fn [& _] nil)
                  incoming/in! (fn [ctx action request]
                                 (swap! calls conj [ctx action request])
                                 {:status :closed})]
      (is (= {:status :closed}
             (service/in! original party room :prepare-incoming
                          {:client-id (str client) :expected-target-stamp (str stamp)})))
      (is (= [[context :prepare {:client-id client :expected-target-stamp stamp}]] @calls)))))

(deftest recovery-selects-authenticated-observation-once-and-never-needs-a-token-for-terminal-cleanup
  (doseq [[state configured? auth?] [[:sending true true] [:sending false false] [:pushed true false]]]
    (let [original (Object.) party (random-uuid) room (random-uuid) client (random-uuid)
          context {:resolved {:party party :room-id room} :conn (atom nil)}
          row {:github.sync/kind :push :github.sync/state state} auth-calls (atom 0)]
      (with-redefs [host/capture (fn [& _] (throw (AssertionError. "Recaptured App")))
                    host/in! (fn [host _ _ _ body] (is (identical? original host)) (body context))
                    journal/sync-in (fn [& _] row)
                    credentials/selected? (fn [_] configured?)
                    credentials/with-transport! (fn [ctx body] (swap! auth-calls inc) (body (assoc ctx :authenticated? true)))
                    push/in! (fn [ctx operation request]
                               (is (= :recover operation))
                               (is (= {:client-id client} request))
                               (is (= auth? (boolean (:authenticated? ctx))))
                               {:status :closed})]
        (is (= {:status :closed} (service/in! original party room :recover {:client-id (str client)})))
        (is (= (if auth? 1 0) @auth-calls))))))
