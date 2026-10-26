(ns is.simm.ops.knowledge-checkpoint-roots-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [datahike.versioning :as versioning]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-pins-test :as fixtures]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.checkpoint-roots :as physical]
            [is.simm.ops.knowledge-checkpoint-roots :as checkpoints]
            [konserve.core :as k]))

(defn- fixture [f]
  (#'fixtures/fixture
   (fn [control value]
     (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
                   :keep-history? false :commit-graph? true :max-string-length 0
                   :max-bytes-length 0 :max-tuple-string-length 0}]
       (d/create-database config)
       (let [source (d/connect config)]
         (try (repo/init! source {:name "Disposable native pin tests"}) (f control (streams/descriptor (assoc (dissoc value :id) :target (pins/target-id (:target (fixtures/origin (changes/basis @source)))))) source)
              (finally (d/release source) (d/delete-database config))))))))

(defn- prepare [control value source]
  (let [basis (changes/basis @source)] (pins/prepare! control value nil basis (fixtures/origin basis))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest native-creation-and-verification-bind-the-full-published-record
  (fixture
   (fn [control value source]
     (let [prepared (prepare control value source) id (get-in prepared [:intent :knowledge.pin/id])
           verified (checkpoints/create! control @source prepared)]
       (is (= id (get-in verified [:intent :knowledge.pin/id])))
       (is (= (changes/basis @source) (:basis verified)))
       (is (= :root-already-present (reason #(checkpoints/create! control @source prepared))))
       (is (= :not-creation-winner (reason #(checkpoints/create! control @source (assoc prepared :new? false)))))
       (d/transact control [[:db.fn/call pins/mark-pinned-tx (:intent prepared)]])
       (is (= :pinned (get-in (checkpoints/verify! control @source id) [:intent :knowledge.pin/state])))))))

(deftest a-lost-native-ack-is-inspected-without-another-native-write
  (fixture
   (fn [control value source]
     (let [prepared (prepare control value source) id (get-in prepared [:intent :knowledge.pin/id])
           native roots/pin! calls (atom 0)]
       (with-redefs [roots/pin! (fn [& args] (swap! calls inc) (apply native args)
                                  (throw (ex-info "Injected native ACK loss" {})))]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Injected native ACK loss"
                               (checkpoints/create! control @source prepared))))
       (is (= 1 @calls))
       (is (= id (get-in (checkpoints/verify! control @source id) [:intent :knowledge.pin/id])))
       (let [retry (prepare control value source)]
         (is (false? (:new? retry)))
         (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "Recovery must not recreate")))]
           (is (= :not-creation-winner (reason #(checkpoints/create! control @source retry))))))))))

(deftest partial-native-outcomes-are-held-instead-of-overwritten
  (fixture
   (fn [control value source]
     (let [prepared (prepare control value source) id (get-in prepared [:intent :knowledge.pin/id])]
       (checkpoints/create! control @source prepared)
       (roots/release! @source id {:sync? true})
       (is (= :pin-unavailable (reason #(checkpoints/verify! control @source id))))
       (is (= :root-already-present (reason #(checkpoints/create! control @source prepared))))))))

(deftest root-owner-and-actual-roots-must-match-not-just-the-commit-label
  (doseq [damage [:owner :record]]
    (fixture
     (fn [control value source]
       (let [prepared (prepare control value source) id (get-in prepared [:intent :knowledge.pin/id])]
         (checkpoints/create! control @source prepared)
         (case damage
           :owner (k/update (:store @source) roots/registry-key #(assoc-in % [id :owner :claim] (random-uuid)) {:sync? true})
           :record (k/update (:store @source) (roots/record-key id) #(assoc % :schema-meta-key (random-uuid)) {:sync? true}))
         (is (= :pin-mismatch (reason #(checkpoints/verify! control @source id)))))))))

(deftest a-stale-control-claim-cannot-create-a-native-root
  (fixture
   (fn [control value source]
     (let [prepared (prepare control value source)
           stale (assoc-in prepared [:intent :knowledge.pin/claim] (random-uuid))]
       (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "No stale root creation")))]
         (is (= :intent-changed (reason #(checkpoints/create! control @source stale)))))))))

(deftest malformed-reserved-keys-are-not-overwritten-even-when-their-values-are-false-or-nil
  (doseq [location [:registry :record] value [false nil]]
    (fixture
     (fn [control descriptor source]
       (let [prepared (prepare control descriptor source) id (get-in prepared [:intent :knowledge.pin/id])]
         (case location
           :registry (k/assoc (:store @source) roots/registry-key {id value} {:sync? true})
           :record (k/assoc (:store @source) (roots/record-key id) value {:sync? true}))
         (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "Never overwrite a reserved root key")))]
           (is (= :root-already-present (reason #(checkpoints/create! control @source prepared))))))))))

(deftest malformed-global-registry-is-held-before-native-creation
  (doseq [malformed [false nil]]
    (fixture
     (fn [control descriptor source]
       (let [prepared (prepare control descriptor source)]
         (k/assoc (:store @source) roots/registry-key malformed {:sync? true})
         (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "Do not replace a corrupt registry")))]
           (is (= :invalid-root-registry (reason #(checkpoints/create! control @source prepared))))))))))

(deftest a-retained-builder-checkpoint-resolves-through-its-original-store-after-branch-retirement
  (fixture
   (fn [_ _ source]
     (let [branch (keyword (str "case-builder-" (random-uuid)))
           root (random-uuid) owner {:fixture :case-builder}
           _ (versioning/branch! source (get-in @source [:config :branch]) branch)
           builder (d/connect (assoc (:config @source) :branch branch))
           basis (try
                   (d/transact builder [{:db/id -1 :db/ident :case-builder/only}])
                   (let [basis (changes/basis @builder)]
                     (physical/create! @builder basis root owner)
                     basis)
                   (finally (d/release builder)))]
       (versioning/delete-branch! source branch)
       ;; Do not weaken the full branch/configuration check to accept a live
       ;; parent. Resolve the actual immutable child DB from the owned store.
       (is (= :physical-source-changed
              (reason #(physical/verify! @source basis root owner))))
       (let [snapshot (d/commit-as-db (:store @source) (:commit basis))
             verified (physical/verify! snapshot basis root owner)]
         (is (= basis (changes/basis snapshot)))
         (is (= basis (:basis verified)))
         (is (d/q '[:find ?e . :where [?e :db/ident :case-builder/only]] (:db verified)))
         (is (nil? (d/q '[:find ?e . :where [?e :db/ident :case-builder/only]] @source))))))))
