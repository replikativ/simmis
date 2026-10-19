(ns is.simm.documents.evaluation-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [org.replikativ.spindel.engine.context :as context]
            [superficie.core :as superficie]
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

(deftest superficie-can-feed-the-same-forkable-sci-interpreter
  ;; Parser interoperability proof, not a second evaluator or a shipped REPL
  ;; input mode: transformed code still crosses the ordinary evaluator gate.
  (let [room (discourse/make-room {:id (keyword (str "dual-sci-" (random-uuid))) :store (memory/make)})
        human (random-uuid)
        run (fn [r syntax source]
              (evaluation/evaluate! r human
                (request (if (= syntax :superficie) (superficie/sup->clj source) source)) (constantly true)))]
    (try
      (is (:success (run room :superficie "defn square [x]:\n  x * x\nend")))
      (is (= "49" (:value (run room :clojure "(square 7)"))))
      (is (:success (run room :clojure "(def factor 2)")))
      (is (= "18" (:value (run room :superficie "factor * square(3)"))))
      (let [fork (context/fork-context (:ctx room)) child (assoc room :ctx fork)]
        (try
          (is (:success (run child :superficie "def factor: 3")))
          (is (= "2" (:value (run room :clojure "factor"))))
          (is (= "3" (:value (run child :clojure "factor"))))
          (finally (context/close-context! fork))))
      (finally (discourse/close-room! room)))))
