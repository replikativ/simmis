(ns is.simm.documents.evaluation-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [dvergr.sandbox :as sandbox]
            [org.replikativ.spindel.engine.context :as context]
            [superficie.core :as superficie]
            [is.simm.documents.evaluation :as evaluation]))

(defn request [code] {:path "examples/scratch.clj" :text code :scope :file :from 0 :to 0})

(deftest dual-mode-repl-evaluates-in-one-world-with-original-input-receipts
  (let [room (discourse/make-room {:id (keyword (str "repl-world-" (random-uuid))) :store (memory/make)})
        human (random-uuid)
        run (fn [r syntax text]
              (evaluation/evaluate-repl! r human {:syntax syntax :text text :namespace "demo.repl"} (constantly true)))]
    (try
      (let [source "defn square [x]:\n  x * x\nend" result (run room :superficie source)]
        (is (:success result) (pr-str result))
        (is (= source (:input-source result)))
        (is (= :superficie (:input-syntax result)))
        (is (re-find #"defn square" (:generated-clojure result))))
      (is (= "49" (:value (run room :clojure "(square 7)"))))
      (is (:success (run room :clojure "(def factor 2)")))
      (is (= "18" (:value (run room :superficie "factor * square(3)"))))
      (let [fork (context/fork-context (:ctx room)) child (assoc room :ctx fork)]
        (try
          (is (:success (run child :superficie "def factor: 3")))
          (is (= "2" (:value (run room :clojure "factor"))))
          (is (= "3" (:value (run child :superficie "factor"))))
          (finally (context/close-context! fork))))
      (is (:success (run room :clojure "(defmacro twice [x] `(+ ~x ~x))")))
      (is (= "10" (:value (run room :superficie "twice(5)"))))
      (is (:success (run room :clojure "(require '[clojure.string :as s])")))
      (is (= "\"HELLO\"" (:value (run room :superficie "s/upper-case(\"hello\")"))))
      (is (= ":clojure.string/key" (:value (run room :superficie "::s/key"))))
      (is (= "\"OKAY\"" (:value (run room :superficie "require('[clojure.string :as t])\nt/upper-case(\"okay\")"))))
      (is (= "9" (:value (run room :superficie "defmacro triple [x]:\n  list(quote(*), x, 3)\nend\ntriple(3)"))))
      (is (= "Ada\n" (:stdout (run room :superficie "println(\"Ada\")"))))
      (let [result (run room :superficie "def retained: 7\nmissingFunction()")]
        (is (false? (:success result)))
        (is (:partial? result))
        (is (re-find #"retained" (:generated-clojure result)))
        (is (= "7" (:value (run room :clojure "retained")))))
      (is (false? (:success (run room :superficie "square(2]"))))
      (is (= "other.repl" (:namespace (run room :clojure "(ns other.repl) (def x 5)"))))
      (is (= "5" (:value (run room :clojure "(require '[org.replikativ.spindel.spin.cps :refer [spin]]) (deref (spin (+ 2 3)))"))))
      (doseq [r [{:syntax :js :text "1" :namespace "user"}
                 {:syntax :clojure :text "1" :namespace "user) (def bad 1)"}
                 {:syntax :clojure :text "1" :namespace "user" :party (random-uuid)}]]
        (is (thrown? Exception (evaluation/evaluate-repl! room human r (constantly true)))))
      (is (thrown? Exception (evaluation/evaluate-repl! room human {:syntax :clojure :text "(def denied 1)" :namespace "user"}
                                                      #(throw (ex-info "revoked" {})))))
      (finally (discourse/close-room! room)))))

(deftest a-repl-timeout-retires-the-interpreter-without-affecting-other-humans
  (let [room (discourse/make-room {:id (keyword (str "repl-timeout-" (random-uuid))) :store (memory/make)})
        a (random-uuid) b (random-uuid)
        run #(evaluation/evaluate-repl! room %1 {:syntax :clojure :text %2 :namespace "user"} (constantly true))]
    (try
      (is (:success (run a "(def old-value 1)")))
      (is (:success (run b "(def old-value 2)")))
      (let [fork (context/fork-context (:ctx room))]
        (try
          (with-redefs [sandbox/eval-code (fn [& _] {:success false :error {:type "TimeoutException" :message "test timeout"}})]
            (is (:session-retired? (run a "old-value"))))
          (let [result (evaluation/evaluate-repl! (assoc room :ctx fork) a
                         {:syntax :clojure :text "old-value" :namespace "user"} (constantly true))]
            (is (= "1" (:value result)) (pr-str result)))
          (finally (context/close-context! fork))))
      (is (false? (:success (run a "old-value"))))
      (is (= "2" (:value (run b "old-value"))))
      (finally (discourse/close-room! room)))))

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
