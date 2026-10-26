(ns is.simm.runtimes.operation-lifetime-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.spin.cps :refer [spin]])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- ticket-bindings []
  ;; Inspect actual conveyed binding values, including the private outer stack.
  [(get (get-thread-bindings) #'lifetime/*ticket*)
   (get (get-thread-bindings) (ns-resolve 'is.simm.runtimes.operation-lifetime '*tickets*))])

(deftest detached-construction-clears-all-nested-tickets-and-restores-short-handoff
  (let [outer (doto (lifetime/create) lifetime/open!)
        inner (doto (lifetime/create) lifetime/open!)]
    (lifetime/with-operation!
      outer
      #(lifetime/with-operation!
         inner
         (fn []
           (let [original (ticket-bindings)]
             (is (= 2 (count (second original))))
             (lifetime/call-detached
              (fn [within!]
                (is (= [nil []] (ticket-bindings)))
                (is (= original (within! ticket-bindings)))
                (is (= [nil []] @(future (ticket-bindings))))
                (is (= [nil []] ((bound-fn [] (ticket-bindings)))))
                (is (= [nil []] (ticket-bindings)))))
             (is (= original (ticket-bindings)))))))
    (doseq [owner [inner outer]] (lifetime/fence! owner) (lifetime/join! owner 1000))))

(deftest detached-spin-keeps-no-app-tickets-when-observed-outside-detached-extent
  (let [runtime (context/create-execution-context)
        owner (doto (lifetime/create) lifetime/open!)]
    (try
      (binding [ec/*execution-context* runtime]
        (lifetime/with-operation!
          owner
          (fn []
            (let [task (lifetime/call-detached (fn [_] (spin (ticket-bindings))))]
              (is (some? lifetime/*ticket*))
              (let [[ticket tickets] @task]
                (is (nil? ticket))
                (is (empty? tickets)))))))
      (finally
        (lifetime/fence! owner) (lifetime/join! owner 1000)
        (context/close-context! runtime)))))

(deftest detached-handoff-cannot-escape-its-extent-or-enter-another-thread
  (let [owner (doto (lifetime/create) lifetime/open!) escaped (atom nil)]
    (lifetime/with-operation!
      owner
      (fn []
        (lifetime/call-detached
         (fn [within!]
           (reset! escaped within!)
           (is (= :detached-handoff-expired
                  @(future (try (within! (constantly :wrong))
                                (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))))
        (is (= :detached-handoff-expired
               (try (@escaped (constantly :wrong))
                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))))
    (lifetime/fence! owner) (lifetime/join! owner 1000)))

(deftest retained-observation-does-not-admit-a-foreign-thread
  (let [owner (doto (lifetime/create) lifetime/open!) captured (atom nil)]
    (lifetime/with-operation! owner
      (fn []
        (reset! captured lifetime/*ticket*)
        (is (true? (lifetime/retained? @captured)))
        (let [result (future [(lifetime/retained? @captured)
                              (try (lifetime/check-current!) :admitted
                                   (catch clojure.lang.ExceptionInfo _ :refused))])]
          (is (= [true :refused] (deref result 5000 ::expired))))
        (deref (future (lifetime/fence! owner)) 5000 ::expired)
        (is (true? (lifetime/retained? @captured)))
        (is (false? (lifetime/ready? owner)))))
    (is (false? (lifetime/retained? @captured)))
    (lifetime/join! owner 1000)))

(deftest retained-cleanup-keeps-the-original-slot-until-successful-retry
  (let [owner (doto (lifetime/create) lifetime/open!)
        attempts (atom 0) captured (atom nil)]
    (with-redefs-fn {#'lifetime/capacity 1}
      #(lifetime/with-operation!
         owner
         (fn []
           (let [ticket lifetime/*ticket*]
             (is (= :not-owner @(future (try (lifetime/retain-operation-cleanup! (constantly nil)) nil
                                             (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))))
             (reset! captured
                     (lifetime/retain-operation-cleanup!
                      (fn [_]
                        (is (= :self-drain
                               (try (lifetime/assert-not-owner! owner) nil
                                    (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
                        (when (= 1 (swap! attempts inc)) (throw (ex-info "Controlled cleanup failure" {}))))))
             (is (= (:id ticket) (:id @captured)))
             (is (not (realized? (:origin-completion @captured))))))))
    (is (realized? (:origin-completion @captured)))
    (is (not (realized? (:completion @captured))))
    (lifetime/fence! owner)
    (is (thrown-with-msg? Exception #"Controlled cleanup failure" (lifetime/join! owner 30000)))
    (is (not (realized? (:completion @captured))))
    (is (nil? (lifetime/join! owner 30000)))
    (is (= 2 @attempts))
    (is (realized? (:completion @captured)))
    (is (nil? (lifetime/join! owner 0)))))

(deftest cleanup-transfer-after-fencing-still-waits-for-the-original-body
  (let [owner (doto (lifetime/create) lifetime/open!)
        entered (promise) transferred (promise) release (CountDownLatch. 1)
        allow-transfer (CountDownLatch. 1) calls (atom 0)
        worker (future
                 (lifetime/with-operation!
                   owner
                   (fn []
                     (deliver entered true)
                     (when-not (.await allow-transfer 30 TimeUnit/SECONDS) (throw (ex-info "Test expired" {})))
                     (deliver transferred (lifetime/retain-operation-cleanup! (fn [_] (swap! calls inc))))
                     (when-not (.await release 30 TimeUnit/SECONDS) (throw (ex-info "Test expired" {}))))))]
    (try
      (is (= true (deref entered 30000 ::expired)))
      (lifetime/fence! owner)
      (.countDown allow-transfer)
      (is (map? (deref transferred 30000 ::expired)))
      (is (= :physical-exit-unconfirmed
             (try (lifetime/join! owner 0) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e))))))
      (is (zero? @calls))
      (.countDown release)
      (is (not= ::expired (deref worker 30000 ::expired)))
      (is (nil? (lifetime/join! owner 30000)))
      (is (= 1 @calls))
      (finally (.countDown allow-transfer) (.countDown release) (deref worker 30000 nil)))))

(deftest shutdown-rechecks-a-slot-transferred-after-its-ticket-snapshot
  (let [owner (doto (lifetime/create) lifetime/open!) entered (promise) snapshotted (promise)
        transfer (CountDownLatch. 1) resume-join (CountDownLatch. 1)
        fail? (atom true) calls (atom 0) joining (atom nil)
        original-join @#'lifetime/join-ticket!
        worker (future
                 (lifetime/with-operation!
                   owner
                   (fn []
                     (deliver entered true)
                     (when-not (.await transfer 30 TimeUnit/SECONDS) (throw (ex-info "Test expired" {})))
                     (lifetime/retain-operation-cleanup!
                      (fn [_] (swap! calls inc)
                        (when @fail? (throw (ex-info "Replacement cleanup failed" {}))))))))]
    (try
      (is (= true (deref entered 30000 ::expired)))
      (lifetime/fence! owner)
      (with-redefs-fn
        {#'lifetime/join-ticket!
         (fn [o ticket deadline]
           (when-not (:origin-completion ticket)
             (deliver snapshotted true)
             (when-not (.await resume-join 30 TimeUnit/SECONDS) (throw (ex-info "Test expired" {}))))
           (original-join o ticket deadline))}
        #(do
           (reset! joining (future (try (lifetime/join! owner 30000) :incorrect-success
                                        (catch Exception e (ex-message e)))))
           (is (= true (deref snapshotted 30000 ::expired)))
           (.countDown transfer)
           (is (not= ::expired (deref worker 30000 ::expired)))
           (.countDown resume-join)
           (is (= "Replacement cleanup failed" (deref @joining 30000 ::expired)))))
      (is (= 1 @calls))
      (reset! fail? false)
      (is (nil? (lifetime/join! owner 30000)))
      (is (= 2 @calls))
      (finally
        (reset! fail? false) (.countDown transfer) (.countDown resume-join)
        (deref worker 30000 nil) (when @joining (deref @joining 30000 nil))))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(defn- ready-owner [] (let [owner (lifetime/create)] (lifetime/open! owner) owner))

(deftest admission-has-one-phase-authority-and-never-resurrects
  (let [owner (lifetime/create)]
    (is (false? (lifetime/ready? owner)))
    (is (= :not-ready (reason #(lifetime/with-operation! owner (constantly :wrong)))))
    (lifetime/open! owner)
    (is (lifetime/ready? owner))
    (is (= :cannot-open (reason #(lifetime/open! owner))))
    (is (= :value (lifetime/with-operation! owner (constantly :value))))
    (is (= :not-fenced (reason #(lifetime/join! owner 0))))
    (is (thrown-with-msg? Exception #"body failed"
                          (lifetime/with-operation! owner #(throw (Exception. "body failed")))))
    (lifetime/fence! owner)
    (is (nil? (lifetime/join! owner 0)))
    (is (false? (lifetime/ready? owner)))
    (is (= :cannot-open (reason #(lifetime/open! owner))))
    (is (= :not-ready (reason #(lifetime/with-operation! owner (constantly :wrong)))))))

(deftest same-thread-nesting-reuses-only-an-exact-live-ticket
  (let [owner (ready-owner) old (atom nil) delayed (atom nil)]
    (lifetime/with-operation!
      owner
      #(let [ticket lifetime/*ticket*]
         (reset! old ticket)
         (is (identical? ticket (lifetime/with-operation! owner (fn [] lifetime/*ticket*))))
         (reset! delayed (bound-fn [] (lifetime/with-operation! owner (fn [] lifetime/*ticket*))))))
    (is (not (identical? @old (@delayed))) "expired binding on the same thread starts fresh ownership")
    (lifetime/fence! owner)
    (is (= :not-ready (reason @delayed)))
    (lifetime/join! owner 0)))

(deftest fenced-operations-finish-nesting-but-shutdown-retains-unfinished-work
  (let [owner (ready-owner) entered (promise) nested (promise)
        gate (CountDownLatch. 1)
        task (future
               (lifetime/with-operation!
                 owner
                 #(let [ticket lifetime/*ticket*]
                    (deliver entered ticket)
                    (.await gate)
                    (deliver nested (identical? ticket (lifetime/with-operation! owner (fn [] lifetime/*ticket*))))
                    :done)))]
    (try
      (is (map? (deref entered 3000 ::timeout)))
      (lifetime/fence! owner)
      (is (= :physical-exit-unconfirmed (reason #(lifetime/join! owner 0))))
      (is (not (realized? task)))
      (is (= :not-ready (reason #(lifetime/with-operation! owner (constantly :wrong)))))
      (.countDown gate)
      (is (= :done (deref task 3000 ::timeout)))
      (is (= true (deref nested 3000 ::timeout)))
      (is (nil? (lifetime/join! owner 3000)))
      (finally (.countDown gate) (deref task 3000 nil)))))

(deftest inherited-bindings-on-another-thread-are-not-ownership
  (let [owner (ready-owner)]
    (lifetime/with-operation!
      owner
      #(let [outer lifetime/*ticket*
             child @(future (lifetime/with-operation! owner (fn [] lifetime/*ticket*)))]
         (is (not (identical? outer child)))
         (is (not (identical? (:thread outer) (:thread child))))
         @(future (lifetime/fence! owner))
         (is (= :not-ready @(future (reason (fn [] (lifetime/with-operation! owner (constantly :wrong)))))))))
    (lifetime/join! owner 0)))

(deftest self-drain-scans-outer-owner-tickets-before-changing-phase
  (let [outer (ready-owner) inner (ready-owner)]
    (lifetime/with-operation!
      outer
      #(lifetime/with-operation!
         inner
         (fn []
           (doseq [owner [outer inner]]
             (is (= :self-drain (reason (fn [] (lifetime/assert-not-owner! owner)))))
             (is (= :self-drain (reason (fn [] (lifetime/fence! owner)))))
             (is (lifetime/ready? owner))))))
    (doseq [owner [outer inner]] (lifetime/fence! owner) (lifetime/join! owner 0))))

(deftest cross-owner-nesting-reuses-the-original-ticket-even-after-fencing
  (let [outer (ready-owner) inner (ready-owner)]
    (lifetime/with-operation!
      outer
      #(let [original lifetime/*ticket*]
         (lifetime/with-operation!
           inner
           (fn []
             @(future (lifetime/fence! outer))
             (is (not (identical? original lifetime/*ticket*)))
             (is (identical? original (lifetime/with-operation! outer (fn [] lifetime/*ticket*))))))))
    (lifetime/join! outer 0)
    (lifetime/fence! inner)
    (lifetime/join! inner 0)))

(deftest bounded-independent-admission-does-not-reject-live-nesting
  (let [owner (ready-owner)]
    (with-redefs-fn
      {#'lifetime/capacity 1}
      #(lifetime/with-operation!
         owner
         (fn []
           (is (= :nested (lifetime/with-operation! owner (constantly :nested))))
           (is (= :capacity-exhausted
                  @(future (reason (fn [] (lifetime/with-operation! owner (constantly :wrong))))))))))
    (lifetime/fence! owner)
    (lifetime/join! owner 0)))

(deftest a-retired-captured-owner-cannot-borrow-its-replacement
  (let [old (ready-owner) current (atom old)
        captured #(lifetime/with-operation! old (constantly :old))]
    (lifetime/fence! old)
    (lifetime/join! old 0)
    (reset! current (ready-owner))
    (is (= :not-ready (reason captured)))
    (is (= :new (lifetime/with-operation! @current (constantly :new))))
    (lifetime/fence! @current)
    (lifetime/join! @current 0)))

(deftest lifecycle-does-not-block-the-engine-drain
  (let [owner (ready-owner)]
    (with-redefs [simple/on-drain-thread? (constantly true)]
      (doseq [f [#(lifetime/open! owner) #(lifetime/with-operation! owner (constantly :wrong))
                 #(lifetime/fence! owner) #(lifetime/join! owner 0)]]
        (is (= :engine-thread (reason f)))))
    (is (lifetime/ready? owner))
    (lifetime/fence! owner)
    (lifetime/join! owner 0)))
