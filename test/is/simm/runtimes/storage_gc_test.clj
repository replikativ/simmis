(ns is.simm.runtimes.storage-gc-test
  "The storage GC loop is OPT-IN: on datahike without #1117 a failed mark
   sweeps the whole store, which is how dev.simm.is lost its system DB. These
   control-loop tests replace sweeps; a separate isolated-store fault test
   verifies the loaded Datahike never reaches sweep after an incomplete mark."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.async :as async]
            [datahike.api :as d]
            [dvergr.substrate.paths :as paths]
            [dvergr.system.db :as sdb]
            [dvergr.system.rooms :as srooms]
            [is.simm.runtimes.storage-gc :as gc]
            [konserve.core :as k]
            [konserve.gc :as kgc]
            [taoensso.telemere :as tel]))

(def ^:private test-home "/tmp/simmis-storage-gc-test-home")

(deftest git-retention-installs-the-host-policy-and-refuses-incompatible-configuration
  (let [reasons (atom [])]
    (with-redefs [gc/enabled? (constantly false)
                  srooms/prohibit-storage-gc! #(do (swap! reasons conj %) true)]
      (is (true? (gc/install-git-retention-profile!)))
      (is (= [:is.simm.runtimes.storage-gc/git-retained-bases] @reasons)))
    (with-redefs [gc/enabled? (constantly true)
                  srooms/prohibit-storage-gc! #(throw (AssertionError. "Must refuse before installing"))]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"requires storage GC to remain disabled"
                            (gc/install-git-retention-profile!))))))

(deftest off-unless-asked
  (testing "no env, no config key: off"
    (is (false? (gc/enabled? nil {})))
    (is (false? (gc/enabled? nil {:storage-gc {}})))
    (is (false? (gc/enabled? "" {}))))
  (testing "config turns it on"
    (is (true? (gc/enabled? nil {:storage-gc {:enabled? true}}))))
  (testing "the environment decides when set"
    (is (true? (gc/enabled? "true" {})))
    (is (true? (gc/enabled? " TRUE " {})))
    (is (false? (gc/enabled? "false" {:storage-gc {:enabled? true}})))
    (testing "anything but true is off — a typo must not turn a deleter on"
      (is (false? (gc/enabled? "yes" {})))
      (is (false? (gc/enabled? "1" {}))))))

(deftest boot-starts-the-loop-only-when-enabled
  (let [starts (atom 0)]
    (with-redefs [gc/start! (fn [& _] (swap! starts inc) :started)]
      (is (= :disabled (gc/boot! (gc/enabled? nil {}))))
      (is (zero? @starts) "the default boot does not start the loop")
      (is (= :started (gc/boot! (gc/enabled? nil {:storage-gc {:enabled? true}}))))
      (is (= 1 @starts))
      (is (= :started (gc/boot! (gc/enabled? "true" {}))))
      (is (= 2 @starts)))))

(deftest flag-off-never-enables-deleting-gc
  (with-redefs [paths/home (constantly test-home)
                gc/start! (fn [& _] (throw (ex-info "start! must not run with the flag off" {})))]
    (srooms/enable-storage-gc! false)
    (is (= :disabled (gc/boot! (gc/enabled? nil {}))))
    (is (= :disabled (gc/boot! (gc/enabled? "false" {:storage-gc {:enabled? true}}))))
    (is (false? (srooms/storage-gc-enabled?)))))

(deftest flag-on-enables-for-simmis-home-and-stop-revokes
  ;; gc-stores! is mocked: the loop's boot sweep reaches it and goes no further.
  (let [swept (promise)]
    (with-redefs [paths/home (constantly test-home)
                  srooms/gc-stores! (fn [opts]
                                      (deliver swept {:enabled? (srooms/storage-gc-enabled?)
                                                      :opts opts})
                                      {:rooms 0 :system-db {:reclaimed 0}})
                  sdb/all-rooms (constantly [])]
      (try
        (is (= :started (gc/boot! (gc/enabled? nil {:storage-gc {:enabled? true}}))))
        (is (true? (srooms/storage-gc-enabled?)) "enabled for simmis's home")
        (let [r (deref swept 30000 ::no-sweep)]
          (is (not= ::no-sweep r) "the loop's boot sweep ran (against the mock)")
          (is (= {:enabled? true :opts {}} r)
              "orphan-only (no :remove-before), and enabled while it ran"))
        ;; Finish the worker's home-sensitive observation BEFORE temporarily
        ;; replacing this process-global Var. Otherwise the boot sweep can
        ;; observe the other-home probe and record a spurious false result.
        (testing "only that home: another dvergr home in this JVM stays off"
          (with-redefs [paths/home (constantly "/tmp/some-other-dvergr-home")]
            (is (false? (srooms/storage-gc-enabled?)))))
        (finally
          (is (= :stopped (gc/stop!)))))
      (is (false? (srooms/storage-gc-enabled?)) "stop! revokes it"))))

(deftest a-refused-sweep-is-an-error-log-not-a-crash
  (srooms/enable-storage-gc! false)
  (let [sigs (atom [])]
    (with-redefs [paths/home (constantly test-home)
                  ;; dvergr's real refusal fires before any store is touched,
                  ;; but keep the system DB out of reach regardless.
                  sdb/get-conn (fn [] (throw (ex-info "must not reach the system DB" {})))]
      (let [r (tel/with-handler :capture (fn [sig] (swap! sigs conj sig)) {:async nil}
                (gc/sweep!))]
        (is (string? (:error r)) "returns an error report instead of throwing")
        (is (some #(and (= ::gc/sweep-refused (:id %)) (= :error (:level %))) @sigs)
            "logged at :error as a refusal")))))

(deftest datahike-version-floor
  (testing "the floor is 0.8.1906, the first release containing datahike 51cf69d3"
    (is (= "0.8.1906" gc/datahike-fail-closed-gc-version)))
  (is (false? (gc/version-at-least? "0.8.1904" "0.8.1905")))
  (is (true? (gc/version-at-least? "0.8.1905" "0.8.1905")))
  (is (true? (gc/version-at-least? "0.8.2000" "0.8.1905")))
  (is (true? (gc/version-at-least? "0.9.1" "0.8.1905")))
  (is (false? (gc/version-at-least? "0.7.9999" "0.8.1905")))
  (testing "an unknown version (a source checkout) is not assumed safe"
    (is (false? (gc/version-at-least? nil "0.8.1905")))
    (is (false? (gc/version-at-least? "0.8.1905-SNAPSHOT" "0.8.1905"))))
  ;; Source pins need not ship Maven metadata. Their safety is tested below,
  ;; not inferred by relabeling an unknown version as a safe release.
  (testing "0.8.1905 shipped without it"
    (is (false? (gc/version-at-least? "0.8.1905" gc/datahike-fail-closed-gc-version)))))

(deftest unknown-source-version-still-warns-when-gc-is-explicitly-enabled
  (let [signals (atom [])]
    (with-redefs [gc/datahike-version (constantly nil)]
      (tel/with-handler :capture (fn [signal] (swap! signals conj signal)) {:async nil}
        (#'gc/warn-if-unsafe-datahike!)))
    (is (some #(and (= ::gc/datahike-gc-not-fail-closed (:id %)) (= :warn (:level %))) @signals))))

(deftest loaded-datahike-refuses-an-incomplete-mark-without-entering-sweep
  (let [attrs (make-array java.nio.file.attribute.FileAttribute 0)
        root (java.nio.file.Files/createTempDirectory "simmis-gc-mark-refusal-" attrs)
        cfg {:store {:backend :file :id (random-uuid) :path (str (.resolve root "store"))}
             :writer {:backend :self :writer-ownership :exclusive}
             :keep-history? true :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg) released? (atom false)]
      (try
        (d/transact conn [{:db/ident :fixture/age :db/valueType :db.type/long
                           :db/cardinality :db.cardinality/one}])
        (doseq [age (range 3)] (d/transact conn [{:fixture/age age}]))
        (let [store (:store @conn)
              keys! #(set (map :key (k/keys store {:sync? true})))
              before (keys!) get-in k/get-in sweep kgc/sweep!
              injected (atom 0) swept (atom 0)
              error (with-redefs [k/get-in
                                  (fn [s ks & more]
                                    (if (and (identical? store s)
                                             (some #(re-find #"datahike\.gc\$reachable_in_branch"
                                                             (.getClassName ^StackTraceElement %))
                                                   (.getStackTrace (Throwable.))))
                                      (do (swap! injected inc) (throw (AssertionError. "Injected mark failure")))
                                      (apply get-in s ks more)))
                                  kgc/sweep!
                                  (fn [s & more]
                                    (if (identical? store s)
                                      ;; Keep even a regressed implementation from
                                      ;; deleting the fixture. Reaching this is a
                                      ;; test failure, not a simulated success.
                                      (do (swap! swept inc) (throw (ex-info "Unexpected sweep" {})))
                                      (apply sweep s more)))]
                      (try
                        (let [result (d/gc-storage conn (java.util.Date. 0))
                              timeout (async/timeout 30000)
                              [value port] (async/alts!! [result timeout])]
                          (if (= port timeout) ::timeout value))
                        (catch Throwable error error)))]
          (is (pos? @injected) "The real branch mark reached our fault")
          (is (instance? Throwable error) "The loaded collector returns an explicit failure")
          (is (= :gc-incomplete-mark (when (instance? Throwable error) (:type (ex-data error)))))
          (is (zero? @swept) "The actual sweep entrypoint was never invoked")
          (is (= before (keys!)) "Every stored key survives")
          (d/release conn)
          (reset! released? true)
          (let [reopened (d/connect cfg)]
            (try (is (= 3 (count (d/q '[:find ?e :where [?e :fixture/age]] @reopened))))
                 (finally (d/release reopened)))))
        (finally
          (when-not @released? (d/release conn))
          (d/delete-database cfg))))))

(deftest a-failed-sweep-is-not-an-empty-sweep
  (is (= [] (gc/report-problems {:rooms 2 :system-db {:reclaimed 0}} 2)))
  (is (= [] (gc/report-problems {:rooms 2 :system-db {:reclaimed 17}} 2)))
  (is (= [:no-report] (gc/report-problems nil 2)))
  (is (= [:system-db-error] (gc/report-problems {:rooms 2 :system-db {:error "boom"}} 2)))
  (is (= [:system-db-timeout] (gc/report-problems {:rooms 2 :system-db {:reclaimed :timeout}} 2)))
  (is (= [:rooms-failed] (gc/report-problems {:rooms 1 :system-db {:reclaimed 0}} 2))
      "dvergr swallows a room's GC failure; the count is the only trace of it"))
