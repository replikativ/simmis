(ns is.simm.uis.web.desktop.aggregate-test
  (:require [is.simm.uis.web.desktop.aggregate :as aggregate]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(deftest reconnect-fences-old-results-and-completions
  (let [gate (aggregate/gate) calls (atom {})
        fetch (fn [key] (fn [done current?] (swap! calls assoc key [done current?])))
        start #(aggregate/run! gate %1 (constantly true) (fetch %2))]
    (start :old :a)
    (let [[a-done a-current?] (:a @calls)]
      (is (a-current?))
      (start :new :b)
      (is (not (a-current?)))
      (a-done)
      (a-done)
      (is ((second (:b @calls))))
      (start :new :c)
      (is (not (contains? @calls :c)))
      ((first (:b @calls)))
      (is (contains? @calls :c)))))

(deftest coalescing-keeps-latest-parameters-and-done-is-idempotent
  (let [gate (aggregate/gate) calls (atom [])
        fetch (fn [value] (fn [done _] (swap! calls conj [value done])))
        start #(aggregate/run! gate :same (constantly true) (fetch %))]
    (start :first)
    (start :superseded)
    (start :latest)
    ((second (first @calls)))
    (is (= [:first :latest] (mapv first @calls)))
    ((second (first @calls)))
    (is (= 2 (count @calls)))))

(deftest invalidated-epoch-cannot-publish-or-restart-pending-work
  (let [gate (aggregate/gate) live? (atom true) call (atom nil) n (atom 0)
        fetch (fn [done current?] (swap! n inc) (reset! call [done current?]))]
    (aggregate/run! gate :epoch #(deref live?) fetch)
    (aggregate/run! gate :epoch #(deref live?) fetch)
    (reset! live? false)
    (is (not ((second @call))))
    ((first @call))
    (is (= 1 @n))))

(deftest reopening-invalidates-the-previous-completion
  (let [gate (aggregate/gate) old (atom nil) new (atom nil)]
    (aggregate/run! gate :same (constantly true) (fn [done _] (reset! old done)))
    (aggregate/reopen! gate)
    (aggregate/run! gate :same (constantly true) (fn [_ current?] (reset! new current?)))
    (@old)
    (is (@new))))
