(ns is.simm.documents.evaluation-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [org.replikativ.spindel.engine.context :as context]
            [is.simm.documents.evaluation :as evaluation]))

(defn request [code] {:path "examples/scratch.clj" :text code :scope :file :from 0 :to 0})

(deftest source-selection-is-inert-exact-and-bounded
  (is (= "(+ 2 3)" (:code (evaluation/request-code
                           (assoc (request "(+ 1 2)\n(+ 2 3)") :scope :selection :from 8 :to 15)))))
  (is (= "\n(+ 2 3)" (:code (evaluation/request-code
                             (assoc (request "(+ 1 2)\n(+ 2 3)") :scope :form :from 11 :to 11)))))
  (is (= "\r\n(+ 2 3)" (:code (evaluation/request-code
                               (assoc (request "(+ 1 2)\r\n(+ 2 3)") :scope :form :from 12 :to 12)))))
  (doseq [r [(assoc (request "(+ 1 2)") :path "a.cljs")
             (assoc (request "(+ 1 2)") :scope :unknown)
             (assoc (request "(+ 1 2)") :actor (random-uuid))
             (assoc (request "#=(throw (Exception.))") :scope :form)
             (assoc (request "x") :text (apply str (repeat 65537 "x")))]]
    (is (thrown? Exception (evaluation/request-code r)))))

(deftest human-interpreters-are-independent-and-room-owned
  (let [room (discourse/make-room {:id (keyword (str "human-eval-" (random-uuid))) :store (memory/make)})
        a (random-uuid) b (random-uuid)
        run #(evaluation/evaluate! room %1 (request %2) (fn [] true))]
    (try
      (is (:success (run a "(def human-number 41)")))
      (is (= "42" (:value (run a "(inc human-number)"))))
      (let [fork (context/fork-context (:ctx room))]
        (try
          (is (:success (evaluation/evaluate! (assoc room :ctx fork) a (request "(def human-number 99)") (constantly true))))
          (is (= "41" (:value (run a "human-number"))))
          (finally (context/close-context! fork))))
      (let [other (discourse/make-room {:id (keyword (str "other-human-eval-" (random-uuid))) :store (memory/make)})]
        (try
          (is (false? (:success (evaluation/evaluate! other a (request "human-number") (constantly true)))))
          (finally (discourse/close-room! other))))
      (is (false? (:success (run b "human-number"))))
      (is (= "hello\n" (:stdout (run a "(println \"hello\")"))))
      (is (= "3" (:value (run a "(+ 1 2)"))))
      (let [text "(ns demo.editor)\n(defn estimate [n p] (* n p))\n(estimate 2 3)"
            from (.lastIndexOf text "(estimate")]
        (is (:success (run a text)))
        (is (= "6" (:value (evaluation/evaluate! room a
                              (assoc (request text) :scope :selection :from from :to (count text)) (constantly true))))))
      (is (thrown? Exception (evaluation/evaluate! room a (request "(def denied 1)")
                                                   #(throw (ex-info "revoked" {})))))
      (is (false? (:success (run a "denied"))))
      (finally (discourse/close-room! room)))))
