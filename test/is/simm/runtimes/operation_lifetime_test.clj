(ns is.simm.runtimes.operation-lifetime-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.impl.simple :as simple])
  (:import [java.util.concurrent CountDownLatch]))

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
