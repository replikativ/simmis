(ns is.simm.documents.evaluation-lifetime-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [dvergr.runtime.preparation :as preparation]
            [dvergr.sandbox :as sandbox]
            [is.simm.documents.evaluation :as evaluation]
            [is.simm.runtimes.operation-lifetime :as lifetime])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- hold! [^CountDownLatch release]
  (let [deadline (+ (System/nanoTime) 30000000000)]
    (loop []
      (let [remaining (- deadline (System/nanoTime))]
        (when-not (pos? remaining) (throw (ex-info "Finite test hold expired" {})))
        (let [done? (try (.await release remaining TimeUnit/NANOSECONDS)
                         (catch InterruptedException _ false))]
          (when-not done? (recur)))))))

(deftest room-cancellation-cannot-be-swallowed-into-a-healthy-result
  (let [room (discourse/make-room {:id (keyword (str "human-room-cancel-" (random-uuid)))
                                   :store (memory/make)})
        owner (doto (lifetime/create) lifetime/open!) party (random-uuid) other (random-uuid)
        entered (promise) interrupted (promise) leased (promise)
        interrupt-wait (CountDownLatch. 1) release (CountDownLatch. 1)
        handle (atom nil) caller (atom nil)
        with-host preparation/with-host-operation! start sandbox/start-evaluation!
        run (fn [actor text]
              (evaluation/evaluate-repl!
               room actor {:text text :syntax :clojure :namespace "user"}
               lifetime/check-current! (fn [f] (lifetime/with-operation! owner f))))]
    (try
      (is (:success (run party "(def cancelled-value 1)")))
      (is (:success (run other "(def healthy-value 17)")))
      (with-redefs [preparation/with-host-operation!
                    (fn [r options f]
                      (with-host r options (fn [lease] (deliver leased lease) (f lease))))
                    sandbox/start-evaluation!
                    (fn [code options] (reset! handle (start code options)))
                    evaluation/repl-evaluate
                    (fn [_]
                      (deliver entered true)
                      (try
                        (when-not (.await interrupt-wait 30 TimeUnit/SECONDS)
                          (throw (ex-info "Finite interrupt wait expired" {})))
                        (catch InterruptedException _ (deliver interrupted true)))
                      (hold! release)
                      {:success true :value "caught the room interruption"})]
        (reset! caller (future (run party "ignored test body")))
        (is (= true (deref entered 30000 ::expired)))
        (preparation/cancel! (deref leased 30000 nil))
        (is (= true (deref interrupted 30000 ::expired)))
        (is (not (realized? @caller)) "No hard termination is inferred from a cancelled lease")
        (is (thrown? Exception (preparation/assert-quiescent! room)))
        (.countDown release)
        (let [receipt (deref @caller 30000 ::expired)]
          (is (false? (:success receipt)))
          (is (= "CancellationException" (:error-type receipt)))
          (is (= :room-cancelled (:stop-reason receipt)))
          (is (:evaluation-entered? receipt))
          (is (:partial? receipt)))
        (is (sandbox/join-evaluation! @handle 30000)))
      (is (lifetime/ready? owner))
      (is (false? (:success (run party "cancelled-value"))))
      (is (= "17" (:value (run other "healthy-value"))))
      (finally
        (.countDown interrupt-wait) (.countDown release)
        (when @caller (deref @caller 30000 nil))
        (when @handle (sandbox/cancel-evaluation! @handle) (sandbox/join-evaluation! @handle 30000))
        (lifetime/fence! owner) (lifetime/join! owner 30000)
        (discourse/close-room! room)))))

(deftest an-admitted-shared-world-waiter-cannot-reuse-an-unconfirmed-retirement
  (let [room (discourse/make-room {:id (keyword (str "human-shared-world-" (random-uuid)))
                                   :store (memory/make)})
        sibling (discourse/fork-room room {:isolation :none :clone-participants? false})
        owner (doto (lifetime/create) lifetime/open!) party (random-uuid)
        entered (promise) waiter-selected (promise) release (CountDownLatch. 1)
        waiter-thread (atom nil) first-handle (atom nil) first-caller (atom nil) waiter (atom nil)
        forbidden-calls (atom 0) fail-index? (atom true)
        start sandbox/start-evaluation! resolve-context sandbox/sci-context-in
        update-index @#'evaluation/update-session-index!
        run (fn [selected text waiter?]
              (evaluation/evaluate-repl!
               selected party {:text text :syntax :clojure :namespace "user"}
               lifetime/check-current!
               (fn [f] (lifetime/with-operation!
                         owner #(do (when waiter? (reset! waiter-thread (Thread/currentThread))) (f))))))]
    (try
      (is (:success (run room "(def shared-before-timeout 1)" false)))
      (with-redefs-fn
        {#'sandbox/start-evaluation!
         (fn [code options]
           (if (str/includes? code ":text \"first\"")
             (reset! first-handle (start code (assoc options :timeout-ms 500)))
             (start code (assoc options :timeout-ms 30000))))
         #'sandbox/sci-context-in
         (fn [world ref]
           (let [interpreter (resolve-context world ref)]
             (when (identical? @waiter-thread (Thread/currentThread))
               (deliver waiter-selected true))
             interpreter))
         #'evaluation/update-session-index!
         (fn [f & args]
           (when (and (identical? f dissoc) (compare-and-set! fail-index? true false))
             (throw (ex-info "Controlled index invalidation failure" {})))
           (apply update-index f args))
         #'evaluation/repl-evaluate
         (fn [{:keys [text]}]
           (if (= "first" text)
             (do (deliver entered true) (hold! release) {:success true :value "late"})
             (do (swap! forbidden-calls inc) {:success true :value "should never enter"})))}
        (fn []
          (reset! first-caller (future (run room "first" false)))
          (is (= true (deref entered 30000 ::expired)))
          (reset! waiter (future (run sibling "second" true)))
          (is (= true (deref waiter-selected 30000 ::expired)))
          (is (false? (:success (deref @first-caller 30000 ::expired))))
          (.countDown release)
          (is (sandbox/join-evaluation! @first-handle 30000))
          (let [receipt (deref @waiter 30000 ::expired)]
            (is (false? (:success receipt)))
            (is (false? (:evaluation-entered? receipt))))
          (is (zero? @forbidden-calls))
          (is (false? (lifetime/ready? owner)))))
      (is (nil? (lifetime/join! owner 30000)))
      (is (nil? (preparation/assert-quiescent! room)))
      (is (nil? (preparation/assert-quiescent! sibling)))
      (finally
        (.countDown release)
        (when @first-caller (deref @first-caller 30000 nil))
        (when @waiter (deref @waiter 30000 nil))
        (when @first-handle (sandbox/cancel-evaluation! @first-handle)
              (sandbox/join-evaluation! @first-handle 30000))
        (lifetime/fence! owner) (lifetime/join! owner 30000)
        (discourse/close-room! sibling {:close-context? false})
        (discourse/close-room! room)))))
