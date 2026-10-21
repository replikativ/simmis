(ns is.simm.uis.web.desktop.resource-slot-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.async :refer [promise-chan put! <!! alts!! timeout]]
            [is.simm.uis.web.desktop.resource-slot :as resource]))

(defn- ready [value] (doto (promise-chan) (put! value)))
(defn- await! [ch]
  (let [[value port] (alts!! [ch (timeout 3000)])]
    (when-not (= port ch) (throw (ex-info "Slot event did not arrive" {})))
    value))
(defn- request [generation & {:as opts}]
  (merge {:generation generation :valid? (constantly true)
          :open! #(ready generation) :dispose! (fn [_] (ready true))
          :publish! (fn [_ _]) :unpublish! (fn [_])} opts))

(deftest stale-construction-and-disposal-complete-before-replacement-opens
  (let [slot (resource/slot) opened (atom []) published (atom [])
        a-start (promise-chan) a-result (promise-chan)
        disposing (promise-chan) disposed (promise-chan)
        a (resource/request! slot (request :a
                                           :open! #(do (swap! opened conj :a) (put! a-start true) a-result)
                                           :dispose! (fn [value] (put! disposing value) disposed)
                                           :publish! (fn [value _] (swap! published conj value))))]
    (await! a-start)
    (let [b (resource/request! slot (request :b
                                             :open! #(do (swap! opened conj :b) (ready :b))
                                             :publish! (fn [value _] (swap! published conj value))))]
      (is (resource/superseded? (await! a)))
      (is (= [:a] @opened))
      (put! a-result :a)
      (is (= :a (await! disposing)))
      (is (= [:a] @opened))
      (put! disposed true)
      (is (= :b (:resource (await! b))))
      (is (= [:b] @published))
      (is (true? (await! (resource/retire! slot)))))))

(deftest failed-cleanup-quarantines-only-its-store-and-is-explicitly-retryable
  (let [slot (resource/slot) other (resource/slot) error (ex-info "Drain failed" {})
        attempts (atom 0)
        dispose! (fn [_] (ready (if (= 1 (swap! attempts inc)) error true)))]
    (is (= :a (:resource (await! (resource/request! slot (request :a :dispose! dispose!))))))
    (is (identical? error (await! (resource/retire! slot))))
    (is (= :held (:phase @(:state slot))))
    (is (identical? error (await! (resource/request! slot (request :b)))))
    (is (= :other (:resource (await! (resource/request! other (request :other))))))
    (is (true? (await! (resource/retry-cleanup! slot))))
    (is (= :b (:resource (await! (resource/request! slot (request :b))))))
    (is (= 2 @attempts))
    (await! (resource/retire! slot)) (await! (resource/retire! other))))

(deftest replacement-generation-can-be-requested-again-after-confirmed-cleanup
  (let [slot (resource/slot) attempts (atom 0) error (ex-info "A disposal failed" {})]
    (await! (resource/request! slot (request :a :dispose! (fn [_] (ready (if (= 1 (swap! attempts inc)) error true))))))
    (is (identical? error (await! (resource/request! slot (request :b)))))
    (is (true? (await! (resource/retry-cleanup! slot))))
    (is (= :b (:resource (await! (resource/request! slot (request :b))))))
    (await! (resource/retire! slot))))

(deftest retirement-during-construction-never-publishes-and-joins-disposal
  (let [slot (resource/slot) started (promise-chan) opened (promise-chan)
        disposed (promise-chan) publish-count (atom 0)
        result (resource/request! slot (request :a :open! #(do (put! started true) opened)
                                                :dispose! (fn [value] (put! disposed value) (ready true))
                                                :publish! (fn [_ _] (swap! publish-count inc))))]
    (await! started)
    (let [drained (resource/retire! slot)]
      (is (resource/superseded? (await! result)))
      (put! opened :a)
      (is (= :a (await! disposed)))
      (is (true? (await! drained)))
      (is (zero? @publish-count)))))

(deftest duplicate-generation-shares-the-result-and-construction
  (let [slot (resource/slot) opened (promise-chan) n (atom 0)
        opts (request :a :open! #(do (swap! n inc) opened))
        a (resource/request! slot opts) b (resource/request! slot opts)]
    (is (identical? a b))
    (put! opened :a)
    (is (= :a (:resource (await! a))))
    (is (= 1 @n))
    (await! (resource/retire! slot))))

(deftest retirement-drain-does-not-wait-for-the-future-replacement-to-retire
  (let [slot (resource/slot)]
    (await! (resource/request! slot (request :a)))
    (let [drained (resource/retire! slot) b (resource/request! slot (request :b))]
      (is (true? (await! drained)))
      (is (= :b (:resource (await! b))))
      (is (= :ready (:phase @(:state slot))))
      (await! (resource/retire! slot)))))

(deftest retiring-a-pending-constructor-joins-it-but-not-its-replacement
  (let [slot (resource/slot) started (promise-chan) opened (promise-chan)
        a (resource/request! slot (request :a :open! #(do (put! started true) opened)))]
    (await! started)
    (let [drained (resource/retire! slot) b (resource/request! slot (request :b))]
      (put! opened :a)
      (is (resource/superseded? (await! a)))
      (is (true? (await! drained)))
      (is (= :b (:resource (await! b))))
      (await! (resource/retire! slot)))))

(deftest synchronous-publication-supersession-cannot-report-the-old-resource-ready
  (let [slot (resource/slot) replacement (promise-chan) disposals (atom [])
        a (resource/request!
           slot (request :a
                         :dispose! (fn [value] (swap! disposals conj value) (ready true))
                         :publish! (fn [_ _]
                                     (put! replacement (resource/request! slot (request :b))))))]
    (is (resource/superseded? (await! a)))
    (is (= :b (:resource (await! (await! replacement)))))
    (is (= [:a] @disposals))
    (await! (resource/retire! slot))))

(deftest uncertain-constructor-errors-cannot-be-blindly-retried
  (let [slot (resource/slot) error (ex-info "Constructor cleanup unknown" {})]
    (is (identical? error (await! (resource/request! slot (request :a :open! #(ready error))))))
    (is (= :held (:phase @(:state slot))))
    (is (= :is.simm.uis.web.desktop.resource-slot/reload-required
           (:type (ex-data (await! (resource/retry-cleanup! slot))))))))

(deftest a-confirmed-clean-constructor-error-allows-a-later-generation
  (let [slot (resource/slot) error (ex-info "No native resource retained" {})]
    (is (identical? error (await! (resource/request! slot (request :a :open! #(ready error)
                                                                   :clean-error? (constantly true))))))
    (is (= :b (:resource (await! (resource/request! slot (request :b))))))
    (await! (resource/retire! slot))))

(deftest synchronous-constructor-and-recovery-errors-retain-the-recovery-owner
  (let [slot (resource/slot) original (ex-info "Open threw" {})
        recovery-error (ex-info "Recovery threw once" {}) n (atom 0)
        result (resource/request!
                slot (request :a :open! #(throw original)
                              :recover-open! (fn [error]
                                               (is (identical? original error))
                                               (if (= 1 (swap! n inc))
                                                 (throw recovery-error) (ready true)))))]
    (is (identical? original (await! result)))
    (is (identical? recovery-error (await! (resource/retry-cleanup! slot))))
    (is (true? (await! (resource/retry-cleanup! slot))))
    (is (= :b (:resource (await! (resource/request! slot (request :b))))))
    (await! (resource/retire! slot))))
