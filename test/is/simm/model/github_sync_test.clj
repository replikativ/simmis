(ns is.simm.model.github-sync-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.github-sync :as sync]))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? true}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (d/transact conn sync/schema) (f conn)
           (finally (d/release conn) (d/delete-database cfg))))))

(defn- binding-for
  ([] (binding-for (random-uuid)))
  ([room]
   (let [by (random-uuid) client (random-uuid)]
     {:id (sync/request-id by room client) :by by :room room :client-id client :claim (random-uuid)})))

(defn- prepare! [conn binding value]
  (artifact/publish-value! (artifact/datahike-store conn) value
                           #(d/transact conn [[:db.fn/call sync/prepare-tx binding % (hasch/uuid value)]]))
  (sync/sync-in @conn (:id binding)))

(defn- transition! [conn row state]
  (if (#{:applying :closing} state)
    (d/transact conn [[:db.fn/call sync/transition-tx row state nil nil]])
    (let [value {:state state :intent (:github.sync/id row)}]
      (artifact/publish-value! (artifact/datahike-store conn) value
                               #(d/transact conn [[:db.fn/call sync/transition-tx row state % (hasch/uuid value)]]))))
  (sync/sync-in @conn (:github.sync/id row)))

(deftest exact-replay-keeps-original-claim-and-one-exclusive-room-slot
  (fixture
   (fn [conn]
     (let [binding (binding-for) value {:source "public/repository" :basis (random-uuid)}
           row (prepare! conn binding value)]
       (is (= :creating (:github.sync/state row)))
       (is (= row (sync/active-in @conn (:room binding))))
       (is (= row (prepare! conn (assoc binding :claim (random-uuid)) value)))
       (is (= (:claim binding) (:github.sync/claim (sync/sync-in @conn (:id binding)))))
       (is (thrown? clojure.lang.ExceptionInfo (prepare! conn binding {:source "different"})))
       (is (thrown? clojure.lang.ExceptionInfo (prepare! conn (binding-for (:room binding)) value)))
       (is (= row (sync/sync-in @conn (:id binding))))))))

(deftest stale-observations-and-missing-closure-never-release-the-room
  (fixture
   (fn [conn]
     (let [binding (binding-for) original (prepare! conn binding {:source "repo"})
           allocated (transition! conn original :allocated)
           ready (transition! conn allocated :ready)]
       (is (thrown? clojure.lang.ExceptionInfo
                    (sync/transition-tx @conn original :applying nil nil)))
       (is (thrown? clojure.lang.ExceptionInfo (transition! conn ready :closed)))
       (let [applying (transition! conn ready :applying)
             applied (transition! conn applying :applied)
             closing (transition! conn applied :closing)]
         (is (= closing (sync/active-in @conn (:room binding))))
         (is (thrown? clojure.lang.ExceptionInfo
                      (sync/transition-tx @conn closing :closed nil nil)))
         (let [closed (transition! conn closing :closed)]
           (is (= :closed (:github.sync/state closed)))
           (is (nil? (sync/active-in @conn (:room binding))))
           (is (:github.sync/outcome closed)) (is (:github.sync/closure closed))
           (is (= closed (prepare! conn binding {:source "repo"})))
           (is (= :creating (:github.sync/state (prepare! conn (binding-for (:room binding)) {:source "new"}))))))))))

(deftest uncertain-apply-cannot-return-to-creation-or-automatic-apply
  (fixture
   (fn [conn]
     (let [binding (binding-for)
           held (-> (prepare! conn binding {:source "repo"})
                    (#(transition! conn % :allocated))
                    (#(transition! conn % :ready))
                    (#(transition! conn % :applying))
                    (#(transition! conn % :held)))]
       (is (= :applying (:github.sync/held-from held)))
       (is (thrown? clojure.lang.ExceptionInfo (transition! conn held :ready)))
       (is (thrown? clojure.lang.ExceptionInfo (transition! conn held :applying)))
       (is (thrown? clojure.lang.ExceptionInfo (transition! conn held :creating)))
       (is (= held (sync/active-in @conn (:room binding))))
       (let [observed (transition! conn held :satisfied-observed)]
         (is (= :satisfied-observed (:github.sync/state observed)))
         (is (nil? (:github.sync/held-from observed)))
         (is (= :closed (:github.sync/state
                         (-> observed (#(transition! conn % :closing)) (#(transition! conn % :closed)))))))))))

(deftest identity-and-artifact-shapes-refuse-before-any-journal-mutation
  (fixture
   (fn [conn]
     (let [binding (binding-for) before @conn]
       (doseq [bad [(assoc binding :id (random-uuid)) (assoc binding :claim nil)
                    (assoc binding :arbitrary :callback)]]
         (is (thrown? clojure.lang.ExceptionInfo
                      (sync/prepare-tx @conn bad (random-uuid) (random-uuid)))))
       (is (= before @conn))))))

(deftest competing-preparations-serialize-at-the-room-slot
  (fixture
   (fn [conn]
     (let [room (random-uuid) start (promise)
           workers (mapv (fn [_]
                           (future @start
                                   (try (prepare! conn (binding-for room) {:source "repo"})
                                        (catch clojure.lang.ExceptionInfo error (:reason (ex-data error))))))
                         (range 2))]
       (deliver start true)
       (let [results (mapv #(deref % 10000 :timeout) workers)]
         (is (= 1 (count (filter map? results))))
         (is (= 1 (count (filter #{:room-busy} results))))
         (is (= (first (filter map? results)) (sync/active-in @conn room))))))))

(deftest a-frozen-ready-receipt-cannot-be-replaced
  (fixture
   (fn [conn]
     (let [row (-> (prepare! conn (binding-for) {:source "repo"}) (#(transition! conn % :allocated))
                   (#(transition! conn % :ready)))
           before @conn]
       (is (empty? (sync/transition-tx @conn row :ready (:github.sync/ready row) (:github.sync/ready-id row))))
       (is (thrown? clojure.lang.ExceptionInfo
                    (sync/transition-tx @conn row :ready (random-uuid) (random-uuid))))
       (is (= before @conn))))))

(defn- prepare-incoming! [conn binding]
  (let [body {:kind :incoming :source "owner/repository"}]
    (artifact/publish-value! (artifact/datahike-store conn) body
                             #(d/transact conn [[:db.fn/call sync/prepare-tx binding % (hasch/uuid body) :incoming]]))
    (sync/sync-in @conn (:id binding))))

(deftest incoming-kind-is-immutable-and-shares-the-existing-exclusive-room-slot
  (fixture
   (fn [conn]
     (let [binding (binding-for) row (prepare-incoming! conn binding)]
       (is (= :incoming (sync/kind row)))
       (is (= row (prepare-incoming! conn (assoc binding :claim (random-uuid)))))
       (is (thrown? clojure.lang.ExceptionInfo (prepare! conn binding {:kind :incoming :source "owner/repository"})))
       (is (thrown? clojure.lang.ExceptionInfo (prepare! conn (binding-for (:room binding)) {:source "owner/repository"})))
       (is (= row (sync/active-in @conn (:room binding))))))))

(deftest incoming-preparation-filing-and-native-cleanup-are-distinct-receipts
  (fixture
   (fn [conn]
     (let [binding (binding-for)
           allocated (transition! conn (prepare-incoming! conn binding) :allocated)
           retaining (transition! conn allocated :retaining)
           ready (transition! conn retaining :ready)]
       (is (:github.sync/retention ready))
       (doseq [old-state [:applying :applied :satisfied-observed]]
         (is (= :wrong-kind
                (try (sync/transition-tx @conn ready old-state (random-uuid) (random-uuid)) nil
                     (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
       (let [filing (transition! conn ready :filing)
             filed (transition! conn filing :filed)]
         (is (:github.sync/filing filed))
         (is (:github.sync/outcome filed))
         (is (= filed (sync/active-in @conn (:room binding))) "Filing does not release the physical cleanup obligation")
         (is (thrown? clojure.lang.ExceptionInfo (transition! conn filed :closed)))
         (let [closed (-> filed (#(transition! conn % :closing)) (#(transition! conn % :closed)))]
           (is (= :closed (:github.sync/state closed)))
           (is (nil? (sync/active-in @conn (:room binding))))))))))

(deftest incoming-ready-cannot-skip-the-durable-retention-blueprint
  (fixture
   (fn [conn]
     (let [allocated (transition! conn (prepare-incoming! conn (binding-for)) :allocated)]
       (is (= :retention-required
              (try (sync/transition-tx @conn allocated :ready (random-uuid) (random-uuid))
                   nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
       (is (= allocated (sync/sync-in @conn (:github.sync/id allocated))))
       (let [held (transition! conn allocated :held)]
         (is (= :retention-required
                (try (sync/transition-tx @conn held :ready (random-uuid) (random-uuid))
                     nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
         (is (= held (sync/sync-in @conn (:github.sync/id held)))))))))

(deftest incoming-retention-recovery-must-preserve-the-original-evidence
  (fixture
   (fn [conn]
     (let [retaining (-> (prepare-incoming! conn (binding-for))
                         (#(transition! conn % :allocated)) (#(transition! conn % :retaining)))
           held (transition! conn retaining :held)
           ready (transition! conn held :ready)]
       (is (= :ready (:github.sync/state ready)))
       (is (= (select-keys retaining [:github.sync/retention :github.sync/retention-id])
              (select-keys ready [:github.sync/retention :github.sync/retention-id])))))))

(deftest unknown-filing-outcomes-cannot-reenter-native-publication
  (fixture
   (fn [conn]
     (let [binding (binding-for)
           held (-> (prepare-incoming! conn binding)
                    (#(transition! conn % :allocated)) (#(transition! conn % :retaining))
                    (#(transition! conn % :ready)) (#(transition! conn % :filing))
                    (#(transition! conn % :held)))]
       (is (= :filing (:github.sync/held-from held)))
       (is (thrown? clojure.lang.ExceptionInfo (transition! conn held :ready)))
       (is (thrown? clojure.lang.ExceptionInfo (transition! conn held :filing)))
       (is (= held (sync/active-in @conn (:room binding))))))))

(deftest fast-forward-rows-cannot-enter-incoming-publication-phases
  (fixture
   (fn [conn]
     (let [binding (binding-for)
           allocated (transition! conn (prepare! conn binding {:source "repo"}) :allocated)
           ready (transition! conn allocated :ready)]
       (is (= :fast-forward (sync/kind ready)))
       (doseq [[row state] [[ready :retaining] [ready :filing] [ready :filed]]]
         (is (= :wrong-kind
                (try (sync/transition-tx @conn row state (random-uuid) (random-uuid)) nil
                     (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
       (is (thrown? clojure.lang.ExceptionInfo (prepare-incoming! conn binding)))
       (is (= ready (sync/sync-in @conn (:id binding))))))))
