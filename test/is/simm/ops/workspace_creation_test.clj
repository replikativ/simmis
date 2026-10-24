(ns is.simm.ops.workspace-creation-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.room.registry :as registry]
            [is.simm.model.knowledge-rooms :as rooms]
            [is.simm.model.knowledge-rooms-integration-test :as fixture]
            [is.simm.model.store :as store]
            [is.simm.model.workspace-intent :as intent]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.workspace-creation :as creation]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- with-app [f]
  (@#'fixture/fixture
   (fn [{:keys [conn root] :as env}]
     (let [party (random-uuid) owner (doto (lifetime/create) lifetime/open!)
           host {:execution-ctx root ::reads/owner (reads/host-owner conn root)}
           request {:id (str (random-uuid)) :name "Intent workspace" :members []}]
       (d/transact conn [{:party/id party :party/type :human}])
       (try
         (with-redefs-fn {#'application/installation (atom {:host host :owner owner})}
           #(f (assoc env :party party :owner owner :host host :request request)))
         (finally (lifetime/fence! owner) (lifetime/join! owner 5000)))))))

(deftest invalid-request-never-captures-an-operation
  (let [calls (atom 0)]
    (with-redefs [application/capture-operation #(swap! calls inc)]
      (doseq [[party op request] [[nil :create {}] [(random-uuid) :other {}]
                                 [(random-uuid) :create {:id (str (random-uuid)) :name "" :members []}]]]
        (is (thrown? clojure.lang.ExceptionInfo (creation/prepare party op request))))
      (is (zero? @calls)))))

(deftest native-creation-and-read-only-observation-retain-one-canonical-room
  (with-app
    (fn [{:keys [conn party request]}]
      (let [created ((creation/prepare party :create request))]
        (is (= :created (:status created)))
        (is (= :project (get-in created [:room :room/type])))
        (is (= 1 (count (registry/list-rooms))))
        (let [snapshot @conn]
          (with-redefs [rooms/create-for-intent! (fn [& _] (throw (AssertionError. "Observation must not provision")))]
            (is (= created ((creation/prepare party :observe request))))
            (is (= snapshot @conn))))
        (is (= :unavailable (:status ((creation/prepare party :create request)))))
        (is (= 1 (count (registry/list-rooms))))
        (is (= created ((creation/prepare party :observe request))))))))

(deftest lost-publication-ack-is-observed-without-replaying-native-work
  (with-app
    (fn [{:keys [conn party request]}]
      (let [transact d/transact
            result (with-redefs [d/transact
                                (fn [target tx & more]
                                  (let [result (apply transact target tx more)]
                                    (when (and (vector? tx) (vector? (first tx))
                                               (identical? @#'rooms/publish-tx (second (first tx))))
                                      (throw (ex-info "Lost final ACK" {})))
                                    result))]
                     ((creation/prepare party :create request)))
            snapshot @conn]
        (is (= :unavailable (:status result)))
        (is (string? (:correlation-id result)))
        (is (= :created (:status ((creation/prepare party :observe request)))))
        (is (= 1 (count (registry/list-rooms))))
        (is (= snapshot @conn))))))

(deftest concurrent-same-intent-creates-at-most-one-native-room
  (with-app
    (fn [{:keys [party request]}]
      (let [start (CountDownLatch. 1)
            body (fn [] (when-not (.await start 10 TimeUnit/SECONDS)
                          (throw (ex-info "Fixture start was not released" {})))
                   ((creation/prepare party :create request)))
            a (future (body)) b (future (body))]
        (.countDown start)
        (try
          (let [results [(deref a 60000 ::pending) (deref b 60000 ::pending)]]
            (is (not-any? #{::pending} results))
            (is (= #{:created :unavailable} (set (map :status results))))
            (is (= 1 (count (registry/list-rooms)))))
          (finally
            ;; Join actual test work before fixture storage cleanup.
            (.countDown start)
            (doseq [worker [a b]]
              (when (= ::pending (deref worker 60000 ::pending))
                (throw (ex-info "Creation worker has not exited" {}))))))))))

(deftest queued-work-cannot-borrow-a-replacement-app-or-promote-a-child
  (with-app
    (fn [{:keys [party request owner host root]}]
      (let [queued (creation/prepare party :create request)
            replacement (doto (lifetime/create) lifetime/open!)]
        (try
          (lifetime/fence! owner)
          (with-redefs-fn {#'application/installation (atom {:host host :owner replacement})}
            #(do
               (is (= :unavailable (:status (queued))))
               (is (empty? (registry/list-rooms)))
               (let [child (context/fork-context root)]
                 (try
                   (binding [ec/*execution-context* child]
                     (is (= :unavailable (:status ((creation/prepare party :create request))))))
                   (is (empty? (registry/list-rooms)))
                   (finally (context/close-context! child))))))
          (finally (lifetime/fence! replacement) (lifetime/join! replacement 5000)))))))

(deftest app-fencing-during-preparation-joins-work-and-withholds-publication
  (with-app
    (fn [{:keys [conn party request owner]}]
      (let [entered (CountDownLatch. 1) release (CountDownLatch. 1)
            ensure! store/ensure! body (creation/prepare party :create request)]
        (with-redefs [store/ensure! (fn [& args]
                                    (let [result (apply ensure! args)]
                                      (.countDown entered)
                                      (when-not (.await release 10 TimeUnit/SECONDS)
                                        (throw (ex-info "Preparation fixture was not released" {})))
                                      result))]
          (let [worker (future (body))]
            (try
              (is (.await entered 30 TimeUnit/SECONDS))
              (lifetime/fence! owner)
              (is (not (realized? worker)))
              (is (thrown? clojure.lang.ExceptionInfo (lifetime/join! owner 1)))
              (.countDown release)
              (is (= :unavailable (:status (deref worker 30000 ::pending))))
              (lifetime/join! owner 5000)
              (is (= {:status :prepared-not-published}
                     (intent/observe @conn party (intent/request! request))))
              (is (empty? (registry/list-rooms)))
              (finally
                (.countDown release)
                (when (= ::pending (deref worker 30000 ::pending))
                  (throw (ex-info "Preparation worker has not exited" {})))))))))))
