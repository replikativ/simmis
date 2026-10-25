(ns is.simm.ops.owned-knowledge-discovery-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [dvergr.artifact :as artifact]
            [dvergr.catalog.room :as catalog]
            [dvergr.runtime.preparation :as preparation]
            [dvergr.system.rooms :as native-rooms]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as owned-workflows]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.operation-lifetime :as lifetime])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def source
  {"workflow.edn" "{:title \"Wiki maintenance\" :task \"Maintain the wiki from evidence.\" :capture [\"/out\"]}"
   "checker.clj" "(throw (ex-info \"Discovery must not execute the checker\" {}))"
   "fixtures/input.md" "Seed fixture"
   "input-selection.edn" "{:kind :knowledge :policy {:version 1 :trigger-roots [\"inbox\"] :context-roots [\"wiki\"] :exclude-prefixes []}}"})

(defn- write! [conn path text]
  (repo/write! conn path (markdown/source-bytes text))
  (repo/stage-all! conn)
  (repo/commit! conn {:message "Discovery fixture"}))

(defn- with-fixture [f]
  (@#'fixture/with-fixture true
   (fn [{:keys [conn owned] :as data}]
     (let [owner (doto (lifetime/create) lifetime/open!)]
       (try
         (d/transact conn (into streams/schema pins/schema))
         (doseq [[path text] source] (write! owned (str "workflows/wiki/" path) text))
         (write! owned "inbox/meeting.md" "The team decided to ship.")
         (with-redefs [work/prepare (fn [& _] (throw (AssertionError. "Discovery must not resolve a model")))]
           (f (-> data (assoc :app-owner owner)
                  (update-in [:host :config :host-lifecycle] assoc :with-operation #(lifetime/with-operation! owner %)))))
         (finally (lifetime/fence! owner) (lifetime/join! owner 5000)))))))

(defn- request [{:keys [host w]}]
  (let [binding (reads/resolve-host-read! host (ops/party-principal (:owner w)) (str (:room w)))]
    {:name "wiki" :expected-source (owned-workflows/source-id binding "wiki" (workflows/manifest-in (:db binding) "wiki"))
     :expected-bundle (:id (catalog/bundle "wiki" (:files (workflows/snapshot-in (:db binding) "wiki")))) :epoch (random-uuid)}))

(defn- refresh [{:keys [host w]} input]
  (discovery/refresh! host (ops/party-principal (:owner w)) (str (:room w)) input))

(defn- recover [{:keys [host w own-descriptor conn]} operation]
  ;; This shared memory fixture substitutes registry->runtime provisioning.
  ;; Preserve that explicit mapping for recovery, never consult its new default.
  (with-redefs [native-rooms/registry-system-for
                (fn [db runtime]
                  (is (= (:runtime-system own-descriptor) runtime))
                  (d/pull db '[*] [:system/id (:registry-system own-descriptor)]))]
    (discovery/recover! host (ops/party-principal (:owner w)) (str (:room w)) {:operation operation})))

(deftest refresh-enqueues-once-and-releases-only-the-predecessor
  (with-fixture
    (fn [{:keys [conn owned attached] :as data}]
      (let [input (request data) other @attached first (refresh data input)
            root (:operation first) stream (:stream first)]
        (is (= :retained (:status first)))
        (is (= 1 (:pending first)))
        (is (= :unchanged (:status (refresh data input))))
        (is (= 1 (count (streams/items @conn stream))))
        (write! owned "inbox/second.md" "New evidence")
        (let [next (refresh data input) registry (roots/roots (:store @owned) {:sync? true})]
          (is (= 2 (:pending next)))
          (is (not= root (:operation next)))
          (is (not (contains? registry root)))
          (is (contains? registry (:operation next)))
          (is (= :released (:knowledge.pin/state (pins/intent @conn root))))
          (is (= :released (:status (recover data root)))))
        (is (= other @attached))))))

(deftest native-ack-loss-is-recovered-by-inspection-without-recreating
  (with-fixture
    (fn [data]
      (let [input (request data) native roots/pin! calls (atom 0)]
        (with-redefs [roots/pin! (fn [& args] (swap! calls inc) (apply native args)
                                  (throw (ex-info "Native ACK lost" {})))]
          (is (thrown-with-msg? Exception #"Native ACK lost" (refresh data input))))
        (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "Recovery must only inspect")))]
          (is (= 1 (:pending (refresh data input)))))
        (is (= 1 @calls))))))

(deftest lost-discovery-ack-can-clean-up-after-the-default-changes
  (with-fixture
    (fn [{:keys [conn w owned attached attached-descriptor] :as data}]
      (let [input (request data) first (refresh data input) old (:operation first)
            publish artifact/publish-value!]
        (write! owned "inbox/new.md" "Another observation")
        (with-redefs [artifact/publish-value! (fn [store value callback]
                                              (let [result (publish store value callback)]
                                                (when (:changes value) (throw (ex-info "Discovery ACK lost" {})))
                                                result))]
          (is (thrown-with-msg? Exception #"Discovery ACK lost" (refresh data input))))
        (let [active (pins/active @conn (:stream first)) id (:knowledge.pin/id active)]
          (is (= :committed (:knowledge.pin/state active)))
          (d/transact conn [{:room/id (:room w) :room/default-repository
                            [:system/id (:registry-system attached-descriptor)]}])
          (let [other @attached]
            (is (= :retained (:status (recover data id))))
            (is (= other @attached)))
          (is (not (contains? (roots/roots (:store @owned) {:sync? true}) old)))
          (is (contains? (roots/roots (:store @owned) {:sync? true}) id))
          (is (= :retained (:status (recover data id)))))))))

(deftest release-ack-loss-is-inspected-before-finishing-the-journal
  (with-fixture
    (fn [{:keys [conn] :as data}]
      (let [input (request data) first (refresh data input) release roots/release!
            previous (streams/stream @conn (:stream first))]
        (write! (:owned data) "inbox/new.md" "New")
        (with-redefs [roots/release! (fn [& args] (apply release args)
                                      (throw (ex-info "Release ACK lost" {})))]
          (is (thrown-with-msg? Exception #"Release ACK lost" (refresh data input))))
        (let [id (:knowledge.pin/id (pins/active @conn (:stream first)))]
          (let [restore d/commit-as-db]
           (with-redefs [roots/release! (fn [& _] (throw (AssertionError. "Already absent")))
                         d/commit-as-db (fn [db id]
                                         (when (= id (:knowledge.stream/cursor-commit previous))
                                           (throw (AssertionError. "Released predecessor may already be collected")))
                                         (restore db id))]
            (is (= :retained (:status (recover data id)))))))))))

(deftest cleanup-uses-the-original-pinned-ref-after-a-logical-checkout
  (with-fixture
    (fn [{:keys [conn owned] :as data}]
      (let [input (request data) first (refresh data input) release roots/release!]
        (write! owned "inbox/new.md" "New evidence")
        (with-redefs [roots/release! (fn [& args] (apply release args)
                                      (throw (ex-info "Release ACK lost" {})))]
          (is (thrown? Exception (refresh data input))))
        (let [id (:knowledge.pin/id (pins/active @conn (:stream first)))]
          (repo/branch! owned "new-ref")
          (repo/checkout! owned "new-ref")
          (is (= :retained (:status (recover data id)))))))))

(deftest program-change-after-native-pin-holds-before-discovery-publication
  (with-fixture
    (fn [{:keys [conn owned] :as data}]
      (let [input (request data) pin roots/pin!]
        (with-redefs [roots/pin! (fn [& args]
                                  (let [result (apply pin args)]
                                    (write! owned "workflows/wiki/README.md" "Program changed during retention")
                                    result))]
          (is (thrown? Exception (refresh data input))))
        (is (empty? (d/datoms @conn :avet :knowledge.stream/id)))
        (is (= [:creating] (mapv #(-> (d/pull @conn '[*] (:e %)) :knowledge.pin/state)
                                  (d/datoms @conn :avet :knowledge.pin/id))))))))

(deftest changed-program-or-nonowner-never-creates-a-pin
  (with-fixture
    (fn [{:keys [host w owned] :as data}]
      (let [input (request data)]
        (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "No native pin")))]
          (is (thrown? Exception (discovery/refresh! host (ops/party-principal (:member w)) (str (:room w)) input)))
          (write! owned "workflows/wiki/README.md" "New program revision")
          (is (thrown? Exception (refresh data input))))))))

(deftest concurrent-refresh-does-not-reuse-the-creation-winner
  (with-fixture
    (fn [data]
      (let [input (request data) entered (promise) release (CountDownLatch. 1)
            pin roots/pin! worker (atom nil)]
        (try
          (with-redefs [roots/pin! (fn [& args]
                                    (deliver entered true)
                                    (when-not (.await release 15 TimeUnit/SECONDS) (throw (ex-info "Expired" {})))
                                    (apply pin args))]
            (reset! worker (future (refresh data input)))
            (is (= true (deref entered 15000 ::expired)))
            (is (thrown? Exception (refresh data input)))
            (.countDown release)
            (is (= :retained (:status (deref @worker 15000 ::expired)))))
          (finally (.countDown release) (when @worker (deref @worker 15000 nil))))))))

(deftest same-commit-id-with-unpublished-data-cannot-be-pinned
  (with-fixture
    (fn [{:keys [owned] :as data}]
      (let [input (request data) read reads/resolve-host-read!
            eid (:e (first (d/datoms @owned :avet :geschichte.work/path "inbox/meeting.md")))
            altered (:db-after (d/with @owned [[:db/add eid :geschichte.work/size 999]]))]
        (is (= (d/commit-id @owned) (d/commit-id altered)))
        (with-redefs [reads/resolve-host-read! (fn [& args] (assoc (apply read args) :db altered))
                      roots/pin! (fn [& _] (throw (AssertionError. "No unpublished pin")))]
          (is (= :unpublished-snapshot
                 (try (refresh data input) nil
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))))

(deftest lost-prepare-ack-holds-a-missing-native-root-without-recreation
  (with-fixture
    (fn [{:keys [conn] :as data}]
      (let [input (request data) publish artifact/publish-value!]
        (with-redefs [artifact/publish-value! (fn [store value callback]
                                              (let [result (publish store value callback)]
                                                (when (:origin value) (throw (ex-info "Prepare ACK lost" {})))
                                                result))]
          (is (thrown-with-msg? Exception #"Prepare ACK lost" (refresh data input))))
        (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "No recovery creation")))]
          (is (= :pin-unavailable
                 (try (refresh data input) nil
                      (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
        (is (empty? (d/datoms @conn :avet :knowledge.stream/id)))))))

(deftest writer-gate-sees-default-or-permission-changes-after-the-last-host-check
 (doseq [change [:default :permission]]
  (with-fixture
    (fn [{:keys [conn w attached-descriptor] :as data}]
      (let [input (request data) transact d/transact injected? (atom false)]
        (with-redefs [d/transact (fn [c tx]
                                  (when (and (identical? c conn) (= :db.fn/call (ffirst tx))
                                             (compare-and-set! injected? false true))
                                    (transact conn [(case change
                                                      :default {:room/id (:room w) :room/default-repository
                                                                [:system/id (:registry-system attached-descriptor)]}
                                                      :permission {:room/id (:room w) :room/owner [:party/id (:member w)]})]))
                                  (transact c tx))
                      roots/pin! (fn [& _] (throw (AssertionError. "Writer gate must refuse before pin")))]
          (is (thrown? Exception (refresh data input))))
        (is @injected?)
        (is (empty? (d/datoms @conn :avet :knowledge.pin/id))))))))

(deftest teardown-waits-for-physical-native-pin-exit
  (doseq [kind [:application :room]]
    (with-fixture
      (fn [{:keys [app-owner room] :as data}]
        (let [input (request data) entered (promise) release (CountDownLatch. 1)
              pin roots/pin! worker (atom nil)]
          (try
            (with-redefs [roots/pin! (fn [& args]
                                      (deliver entered true)
                                      ;; A native operation may finish even after
                                      ;; cancellation. Keep its actual owner until
                                      ;; the synchronous native call exits.
                                      (loop []
                                        (when-not (try (.await release 100 TimeUnit/MILLISECONDS)
                                                       (catch InterruptedException _ false))
                                          (recur)))
                                      (apply pin args))]
              (reset! worker (future (try (refresh data input) :unexpected
                                         (catch clojure.lang.ExceptionInfo _ :held))))
              (is (= true (deref entered 15000 ::expired)))
              (let [leases (vec (vals (get @(:meta room) preparation/leases-key)))]
                (is (= 1 (count leases)))
                (case kind
                  :application
                  (do (lifetime/fence! app-owner)
                      (is (= :physical-exit-unconfirmed
                             (try (lifetime/join! app-owner 0) nil
                                  (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
                  :room
                  (doseq [lease leases]
                    (preparation/cancel! lease)
                    (is (identical? lease (get-in @(:meta room) [preparation/leases-key (:id lease)])))
                    (is (= :dvergr.runtime.preparation/unconfirmed-drain
                           (try ((:join! lease) 0) nil
                                (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))))
                (.countDown release)
                (is (= :held (deref @worker 15000 ::expired)))
                (doseq [lease leases]
                  (is (nil? ((:join! lease) 1000)))
                  (is (nil? (get-in @(:meta room) [preparation/leases-key (:id lease)]))))
                (when (= :application kind) (is (nil? (lifetime/join! app-owner 1000))))))
            (finally (.countDown release) (when @worker (deref @worker 15000 nil)))))))))
