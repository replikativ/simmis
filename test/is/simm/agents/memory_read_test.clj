(ns is.simm.agents.memory-read-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.agents.memory-read :as memory]))

(deftest saved-vocabulary-resolves-once-per-call-without-retaining-source-or-db
  (let [calls (atom 0)
        current (atom {:status :ready :id :parent
                       :documents [{:path "a.md" :source "parent" :id :a}]})
        api (memory/bindings #(do (swap! calls inc) @current))]
    (is (= "parent" (get-in ((api 'read) "a.md") [:document :source])))
    (reset! current {:status :ready :id :child
                     :documents [{:path "a.md" :source "child" :id :a}]})
    (is (= "child" (get-in ((api 'read) "a.md") [:document :source])))
    (is (= :child (:id ((api 'documents)))))
    (is (= 3 @calls))
    (reset! current {:status :invalid :diagnostics [{:kind :duplicate-document-id}]})
    (is (= :invalid (:status ((api 'search) "child"))))
    (is (not (contains? ((api 'documents)) :documents)))
    (reset! current nil)
    (is (thrown? clojure.lang.ExceptionInfo ((api 'documents))))))

(deftest read-rechecks-authority-and-does-not-expose-raw-database
  (let [allowed? (atom true)
        api (memory/bindings #(if @allowed?
                                {:status :ready :db :private :documents []}
                                (throw (ex-info "Revoked" {:type :not-authorized}))))]
    (is (nil? (:db ((api 'documents)))))
    (reset! allowed? false)
    (doseq [[f args] [['documents []] ['read ["a.md"]] ['search ["a"]]]]
      (is (thrown? clojure.lang.ExceptionInfo (apply (api f) args))))))

(deftest literal-bounded-search-retains-utf16-source-offsets
  (let [api (memory/bindings
             (constantly {:status :ready :id :selection
                          :documents [{:path "a.md" :content :hash :source "😀 [x] [x]"}]}))
        result ((api 'search) "[x]")]
    (is (= [[3 6] [7 10]] (mapv (juxt :from :to) (:matches result))))
    (is (= :source-lifetime (:retention result)))
    (is (nil? (:document ((api 'read) "absent.md"))))
    (is (empty? (:matches ((api 'search) ".*"))))
    (is (thrown? clojure.lang.ExceptionInfo ((api 'search) ""))))
  (let [api (memory/bindings
             (constantly {:status :ready :documents [{:path "a.md" :source (apply str (repeat 100 "x"))}]}))]
    (is (= 50 (count (:matches ((api 'search) "x")))))))

(deftest pending-is-not-a-successful-empty-selection
  (let [api (memory/bindings (constantly {:status :pending :reason :source-changed}))]
    (is (= {:status :pending :reason :source-changed :retention :source-lifetime}
           ((api 'documents))))))
