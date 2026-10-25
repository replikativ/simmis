(ns is.simm.ops.workflow-readiness-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.run :as run]
            [dvergr.model.registry :as registry]
            [dvergr.resource :as resource]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.model.model-catalog :as catalog]
            [is.simm.model.model-selection :as selection]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.authority.world :as world]
            [is.simm.ops.workflow-readiness :as readiness]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.workflows-remote])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(deftest exact-model-metadata-does-not-refresh-or-return-provider-secrets
  (with-redefs [registry/list-models
                (constantly [{:id "codex-subscription-luna-6" :provider :codex-subscription :api-type :openai-responses
                              :secret "never-return" :base-url "private-endpoint"}
                             {:id "unsupported-api" :provider :codex-subscription :api-type :unknown}
                             {:id "unsupported-provider" :provider :claude-code :api-type :openai-chat}
                             {:id "native-chat" :provider :openai :api-type :openai-chat}])
                catalog/choices (fn [& _] (throw (Exception. "Must not refresh")))
                selection/model-availability (fn [& _] (throw (Exception. "Must not query account")))
                selection/credential-present? #(= :codex-subscription %)
                catalog/model-label identity]
    (let [result (readiness/exact-model-options)]
      (is (= ["codex-subscription-luna-6" "native-chat"] (mapv :id (:models result))))
      (is (= [:present :undetected] (mapv :credential-status (:models result))))
      (is (= "Default credential not detected; connection not verified" (get-in result [:models 1 :credential-label])))
      (is (= "Login present; connection not tested" (get-in result [:models 0 :credential-label])))
      (is (not (:models-truncated? result)))
      (is (not (.contains (pr-str result) "never-return")))
      (is (not (.contains (pr-str result) "private-endpoint"))))))

(deftest inactive-room-read-never-hydrates-or-reads-a-wallet
  (let [binding {:room nil :party :human :room-id :room :slug "room"} calls (atom [])]
    (with-redefs [reads/resolve-registered-host-read-room! (fn [& _] (swap! calls conj :authorize) binding)
                  readiness/exact-model-options (constantly {:models [] :models-truncated? false})
                  rooms/live-room (fn [& _] (throw (Exception. "Must not hydrate")))
                  resource/balance (fn [& _] (throw (Exception. "Must not read absent Room")))]
      (is (= {:models [] :models-truncated? false :allowance {:status :unavailable :reason :room-not-active}}
             (readiness/read-on-host! {} {} "room")))
      (is (= [:authorize :authorize] @calls)))))

(deftest actual-available-amount-is-exact-and-zero-is-distinct-from-missing-wallet
  (doseq [[wallet expected]
          [[{resource/microdollars 9007199254740993M resource/model-dispatches 8M}
            {:status :observed :microdollars "9007199254740993" :model-dispatches "8"}]
           [{} {:status :observed :microdollars "0" :model-dispatches "0"}]
           [::missing {:status :unavailable :reason :wallet-not-configured}]]]
    (let [room {:ctx :native-context} binding {:room room :party :human :room-id :room :slug "room"}
          held? (atom false) validations (atom 0)]
      (with-redefs [reads/resolve-registered-host-read-room! (fn [& _] binding)
                    readiness/exact-model-options (constantly {:models [] :models-truncated? false})
                    reads/validate-host-read-room! (fn [& _] (is @held?) (swap! validations inc))
                    preparation/with-host-operation! (fn [r body]
                                                       (is (identical? room r))
                                                       (reset! held? true)
                                                       (try (body :lease) (finally (reset! held? false))))
                    preparation/with-host-current! (fn [_ body] (is @held?) (body))
                    resource/balance (fn [r]
                                       (is @held?) (is (identical? room r))
                                       (if (= ::missing wallet)
                                         (throw (ex-info "Private account detail" {:type :kontor.resource/account-not-found}))
                                         wallet))]
        (is (= expected (:allowance (readiness/read-on-host! {:execution-ctx :app} {} "room"))))
        (is (= 2 @validations))
        (is (false? @held?))))))

(deftest queued-readiness-retains-original-application-and-sanitizes-refusal
  (let [party (random-uuid) captured (atom []) ready? (atom true)
        thunk (with-redefs [application/capture-operation
                           (fn [] (swap! captured conj :capture)
                             (fn [body] (swap! captured conj :enter-original)
                               (when-not @ready? (throw (ex-info "Private path" {:private "secret"})))
                               (body :original-host)))]
                (readiness/prepare-readiness party "room"))]
    (is (= [:capture] @captured))
    (with-redefs [application/with-operation! (fn [& _] (throw (Exception. "Must not reacquire App")))
                  readiness/read-on-host! (fn [host principal handle]
                                           (is (= :original-host host))
                                           (is (= (str "party/" party) (:principal/id principal)))
                                           (is (= "room" handle)) :result)]
      (is (= :result (thunk)))
      (reset! ready? false)
      (let [error (try (thunk) nil (catch Exception e e))]
        (is (= ::readiness/unavailable (:type (ex-data error))))
        (is (nil? (ex-cause error)))
        (is (= "Workflow readiness unavailable" (ex-message error)))))))

(deftest real-ledger-observation-excludes-child-grants-and-observes-consumption-and-return
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w room host hydrations]}]
     (let [principal {:principal/kind :human :principal/id (str "party/" (:owner w))}
           handle (str (:room w))
           observed #(get (readiness/read-on-host! host principal handle) :allowance)]
       (with-redefs [readiness/exact-model-options (constantly {:models []})
                     rooms/live-room (fn [& _] (throw (Exception. "Must not hydrate")))]
         (is (= {:status :observed :microdollars "0" :model-dispatches "0"} (observed)))
         (resource/mint! room {:id (random-uuid)
                               :resources {resource/microdollars 9007199254740993M resource/model-dispatches 8M}})
         (is (= {:status :observed :microdollars "9007199254740993" :model-dispatches "8"} (observed)))
         (let [child (:run/id (run/start! room :fixture (random-uuid) nil {:kind :task}))]
           (try
             (resource/allocate-run! room child nil {resource/microdollars 3M resource/model-dispatches 2M})
             (is (= {:status :observed :microdollars "9007199254740990" :model-dispatches "6"} (observed)))
             (resource/consume! room child {:id (random-uuid)
                                            :resources {resource/microdollars 1M resource/model-dispatches 1M}})
             (resource/return! room child nil (resource/run-balance room child))
             (is (= {:status :observed :microdollars "9007199254740992" :model-dispatches "7"} (observed)))
             (finally (run/finish! child :completed))))
         (is (empty? @hydrations)))))))

(defn- finite-hold!
  ([gate] (finite-hold! gate nil))
  ([^CountDownLatch gate interrupted]
   (let [deadline (+ (System/nanoTime) (.toNanos TimeUnit/SECONDS 30))]
     (loop []
       (let [remaining (- deadline (System/nanoTime))]
         (when-not (pos? remaining) (throw (ex-info "Readiness test gate timed out" {})))
         (when-not (try (.await gate remaining TimeUnit/NANOSECONDS)
                        (catch InterruptedException _
                          (when interrupted (deliver interrupted true)) false))
           (recur)))))))

(deftest real-read-holds-app-and-native-room-until-physical-completion-and-rechecks-revocation
  (doseq [revoke? [false true]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [conn w room host]}]
       (let [owner (doto (lifetime/create) lifetime/open!)
             balance resource/balance entered (promise) gate (CountDownLatch. 1)]
         (try
           (with-redefs-fn
             {#'application/installation (atom {:host host :owner owner})
              #'readiness/exact-model-options (constantly {:models []})
              #'resource/balance (fn
                                   ([actual]
                                    (is (identical? room actual))
                                    (deliver entered true) (finite-hold! gate) (balance actual))
                                   ([actual account] (balance actual account)))}
             (fn []
               (let [body (readiness/prepare-readiness (:member w) (str (:room w)))
                     task (future (try (body) (catch Throwable error error)))]
                 (try
                   (is (= true (deref entered 10000 ::timeout)))
                   (is (thrown? clojure.lang.ExceptionInfo (preparation/assert-quiescent! room)))
                   (if revoke?
                     (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                     (do (lifetime/fence! owner)
                         (is (thrown? clojure.lang.ExceptionInfo (lifetime/join! owner 0)))))
                   (.countDown gate)
                   (let [result (deref task 10000 ::timeout)]
                     (if revoke?
                       (is (= :not-authorized (:type (ex-data result))))
                       (is (= :observed (get-in result [:allowance :status])))))
                   (is (nil? (preparation/assert-quiescent! room)))
                   (finally (.countDown gate) (deref task 30000 nil))))))
           (finally (.countDown gate) (lifetime/fence! owner) (lifetime/join! owner 30000))))))))

(deftest real-queued-read-cannot-borrow-a-replacement-app
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host]}]
     (let [old (doto (lifetime/create) lifetime/open!)
           replacement (doto (lifetime/create) lifetime/open!)
           slot (atom {:host host :owner old}) calls (atom 0)]
       (try
         (with-redefs-fn {#'application/installation slot
                         #'resource/balance (fn [& _] (swap! calls inc) {})}
           (fn []
             (let [body (readiness/prepare-readiness (:owner w) (str (:room w)))]
               (lifetime/fence! old)
               (reset! slot {:host host :owner replacement})
               (is (thrown? clojure.lang.ExceptionInfo (body)))
               (is (zero? @calls)))))
         (finally (lifetime/fence! old) (lifetime/join! old 30000)
                  (lifetime/fence! replacement) (lifetime/join! replacement 30000)))))))

(deftest actual-inbound-readiness-uses-member-read-permission-and-the-captured-app
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w room host hydrations]}]
     (let [owner (doto (lifetime/create) lifetime/open!) balance resource/balance calls (atom 0)]
       (try
         (with-redefs-fn
           {#'application/installation (atom {:host host :owner owner})
            #'readiness/exact-model-options (constantly {:models []})
            #'rooms/live-room (fn [& _] (throw (Exception. "Must not hydrate")))
            #'resource/balance (fn
                                  ([actual] (swap! calls inc) (balance actual))
                                  ([actual account] (balance actual account)))}
           (fn []
             (doseq [kind [:non-member :admin :anon :team-agent]]
               (is (world/refused? (world/inbound! (world/principal w kind) "load-workflow-readiness!"
                                     {:room-id-str (str (:room w))}))))
             (is (zero? @calls))
             (doseq [kind [:owner :member]]
               (let [reply (:result (world/inbound! (world/principal w kind) "load-workflow-readiness!"
                                      {:room-id-str (str (:room w))}))]
                 (is (= {:models [] :allowance {:status :observed :microdollars "0" :model-dispatches "0"}} reply))))
             (is (= 2 @calls))
             (is (empty? @hydrations))))
         (finally (lifetime/fence! owner) (lifetime/join! owner 30000)))))))

(deftest native-room-drain-cannot-publish-a-cancelled-wallet-observation
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w room host]}]
     (let [owner (doto (lifetime/create) lifetime/open!) balance resource/balance
           entered (promise) interrupted (promise) gate (CountDownLatch. 1)]
       (try
         (with-redefs-fn
           {#'application/installation (atom {:host host :owner owner})
            #'readiness/exact-model-options (constantly {:models []})
            #'resource/balance (fn
                                 ([actual] (deliver entered true) (finite-hold! gate interrupted) (balance actual))
                                 ([actual account] (balance actual account)))}
           (fn []
             (let [body (readiness/prepare-readiness (:owner w) (str (:room w)))
                   reader (future (try (body) (catch Throwable error error)))]
               (try
                 (is (= true (deref entered 10000 ::timeout)))
                 (let [drainer (future (try (preparation/drain-room! room) (catch Throwable error error)))]
                   (try
                     (is (= true (deref interrupted 3000 ::timeout)))
                     (is (not (realized? reader)))
                     (is (not (realized? drainer)))
                     (is (thrown? clojure.lang.ExceptionInfo (preparation/assert-quiescent! room)))
                     (.countDown gate)
                     (is (= ::readiness/unavailable (:type (ex-data (deref reader 10000 ::timeout)))))
                     (is (nil? (deref drainer 10000 ::timeout)))
                     (is (nil? (preparation/assert-quiescent! room)))
                     (finally (.countDown gate) (deref drainer 30000 nil))))
                 (finally (.countDown gate) (deref reader 30000 nil))))))
         (finally (.countDown gate) (lifetime/fence! owner) (lifetime/join! owner 30000)))))))

(deftest quantity-observation-bounds-before-expansion-or-printing
  (let [quantity (var-get (var is.simm.ops.workflow-readiness/exact-quantity))
        limit (.subtract (.pow java.math.BigInteger/TEN 512) java.math.BigInteger/ONE)]
    (doseq [value [nil 0 0M (java.math.BigDecimal. "0E+1000000000")
                   (java.math.BigDecimal. "0E-1000000000")]]
      (is (= "0" (quantity value))))
    (is (= (str limit) (quantity limit)))
    (is (= (str limit) (quantity (bigdec limit))))
    (is (= "123" (quantity 123.00M)))
    (doseq [value [(java.math.BigDecimal. "1E+1000000000")
                   (java.math.BigDecimal. "1E-1000000000")
                   (.pow java.math.BigInteger/TEN 512)
                   (.shiftLeft java.math.BigInteger/ONE 100000)
                   (java.math.BigDecimal. (.shiftLeft java.math.BigInteger/ONE 100000))
                   -1 -1M 1.5M 1.0 "1"]]
      (is (thrown? Exception (quantity value))))))
