(ns is.simm.authority.data-plane-matrix-test
  "The data plane's authority matrix: the konserve-sync SUBSCRIBE gate
   (`data-plane-authorized?`) and the PUBLISH gate
   (`data-plane-publish-authorized?`), with the real `can?` over the real
   world rather than the stub `runtimes/data-plane-test` pins the dispatch
   with.

   Subscribing to a store scope replicates the whole store, so each row's
   attack is a principal replicating a store it has no relation to, and its
   counterpart is the related principal replicating it. Publishing is refused
   for everyone; its counterpart is that the very same principal still
   SUBSCRIBES to that topic — the refusal is about direction, not about who."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.authority.world :as world]
            [is.simm.runtimes.web :as web]))

(def ^:private AUTH #{:owner :member :non-member :admin})
(def ^:private MEMBERS #{:owner :member})

(defn- topics
  "topic → the principals that may SUBSCRIBE to it."
  [w]
  {(:room-scope w)        MEMBERS             ; the team room's store (its messages)
   (:repo-sys w)          MEMBERS             ; the team room's own repo
   (:kb-granted-scope w)  MEMBERS             ; KB granted to the team room
   (:kb-granted-sys w)    MEMBERS             ; ... named by its system id
   (:kb-private-scope w)  #{:owner}           ; owner's KB attached nowhere
   (:kb-stranger-scope w) #{:non-member}      ; the stranger's KB
   (:other-room-scope w)  #{:non-member}      ; the stranger's room
   (random-uuid)          #{}                 ; a scope nobody has
   web/simmis-scope       AUTH                ; the app's own furniture
   :user-rooms/dirty      AUTH                ; an allowlisted control topic
   :some/future-topic     #{}                 ; an unlisted control topic
   (keyword "notify" (str (:owner w)))  #{:owner}
   (keyword "notify" (str (:member w))) #{:member}})

(deftest subscribe-matrix
  (world/with-world [_ w]
    (doseq [[topic allowed] (topics w)
            kind world/principal-kinds]
      (testing (str "subscribe " topic " as " kind)
        (is (= (contains? allowed kind)
               (boolean (web/data-plane-authorized? (world/principal w kind) topic)))
            (str kind (if (contains? allowed kind) " refused " " admitted ") topic))))))

(deftest every-principal-subscribes-to-something
  ;; The matrix above could pass with a gate that refuses everything; this is
  ;; the half that says every principal kind but anonymous still syncs.
  (world/with-world [_ w]
    (doseq [kind (remove #{:anon} world/principal-kinds)]
      (is (some (fn [[topic allowed]]
                  (and (contains? allowed kind)
                       (web/data-plane-authorized? (world/principal w kind) topic)))
                (topics w))
          (str kind " can replicate nothing")))))

(deftest publish-is-refused-to-everyone-who-may-subscribe
  (world/with-world [_ w]
    (doseq [[topic allowed] (topics w)
            kind allowed
            :let [p (world/principal w kind)]]
      (testing (str kind " on " topic)
        (is (true? (boolean (web/data-plane-authorized? p topic)))
            "the counterpart: this principal still subscribes")
        (is (false? (boolean (web/data-plane-publish-authorized? p topic)))
            "and may not publish into it")))
    (testing "nor may the principals with no relation to the topic"
      (doseq [[topic allowed] (topics w)
              kind (remove (set allowed) world/principal-kinds)]
        (is (false? (boolean (web/data-plane-publish-authorized?
                              (world/principal w kind) topic))))))))
