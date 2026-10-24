(ns is.simm.authority.saved-cells-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.sandbox :as sandbox]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.repo :as repo]
            [is.simm.authority.world :as world]
            [is.simm.documents.cells-test :as cells-test]
            [is.simm.documents.evaluation :as evaluation]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.memory-remote :as remote])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(def path "wiki/Cells.md")
(def original "```clojure\n(+ cell-base 2)\n```\n")

(defn- write! [conn text]
  (repo/write! conn path (markdown/source-bytes text)))

(defn- with-fixture [f]
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn host room w owned] :as data}]
     (let [owner (doto (lifetime/create) lifetime/open!)]
       (try
         (write! owned original)
         (with-redefs-fn
           {#'application/installation (atom {:host host :owner owner})}
           (fn []
             (let [binding (owned/resolve! @conn (:owner w) :read (:room w) room)
                   file (files/read-file-in (:db binding) path)
                   request (assoc (cells-test/request file (first (cells-test/fences file)))
                                  :expected-target-stamp (:target-stamp binding))]
               (f (assoc data :owner owner :request request
                         :run #(remote/invoke-memory (:owner w) (str (:room w)) :cell %)
                         :repl #(remote/invoke-memory (:owner w) (str (:room w)) :repl
                                                     {:text % :syntax :clojure :namespace "user"}))))))
         (finally (lifetime/fence! owner) (lifetime/join! owner 30000)))))))

(deftest actual-inbound-policy-and-saved-literal-share-the-human-session
  (with-fixture
    (fn [{:keys [w request repl attached]}]
      (let [other @attached selected (workspace/selected-system-id) calls (atom 0)
            evaluate evaluation/evaluate-repl!]
        (with-redefs [evaluation/evaluate-repl! (fn [& args] (swap! calls inc) (apply evaluate args))]
          (doseq [kind [:member :non-member :admin :anon]]
            (is (world/refused? (world/inbound! (world/principal w kind) "evaluate-memory-cell!"
                                               {:room-id-str (str (:room w)) :request request}))))
          (is (zero? @calls))
          (is (= "40" (:value (repl "(def cell-base 40) cell-base"))))
          (doseq [kind [:owner]]
            (let [reply (:result (world/inbound! (world/principal w kind) "evaluate-memory-cell!"
                                                {:room-id-str (str (:room w)) :request request}))]
              (is (:success reply)) (is (= "42" (:value reply)))
              (is (= (:expected-target-stamp request) (get-in reply [:source :target-stamp])))
              (is (= (:room w) (get-in reply [:source :room-id])))
              (is (uuid? (get-in reply [:source :room-incarnation])))
              (is (= (subs original (:from request) (:to request)) (get-in reply [:source :original])))
              (is (not-any? #(contains? reply %) [:host :room :db :conn :file-binding]))))
          (is (= selected (workspace/selected-system-id)))
          (is (= other @attached)))))))

(deftest forged-and-stale-cell-requests-never-reach-the-evaluator
  (with-fixture
    (fn [{:keys [request run]}]
      (let [calls (atom 0)]
        (with-redefs [evaluation/evaluate-repl! (fn [& _] (swap! calls inc))]
          (doseq [q [(assoc request :code "(throw (Exception.))")
                     (assoc request :actor (random-uuid)) (assoc request :world :forged)
                     (assoc request :repository-id (random-uuid))
                     (assoc request :expected-target-stamp (random-uuid))
                     (assoc request :expected (assoc (:expected request) :content (random-uuid)))
                     (assoc request :revision (random-uuid)) (update request :from inc)
                     (assoc request :block (random-uuid))]]
            (is (thrown? Exception (run q))))
          (is (zero? @calls)))))))

(deftest changed-source-or-default-after-preflight-refuses-before-body
  (doseq [change [:source :delete :mode :default :revoke]]
    (with-fixture
      (fn [{:keys [conn w attached-descriptor owned request run repl]}]
        (is (:success (repl "(def cell-base 40)")))
        (let [evaluate evaluation/evaluate-repl! calls (atom 0)]
          (with-redefs [evaluation/evaluate-repl!
                        (fn [& args]
                          (case change
                            :source (write! owned "```clojure\n(+ cell-base 200)\n```\n")
                            :delete (repo/remove! owned path)
                            :mode (repo/write! owned path (markdown/source-bytes original) {:mode 33261})
                            :default (d/transact conn [{:room/id (:room w)
                                                       :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                            :revoke (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner [:party/id (:owner w)]]]))
                          (apply evaluate args))
                        evaluation/repl-evaluate (fn [& _] (swap! calls inc))]
            (let [reply (run request)]
              (is (false? (:success reply)))
              (is (false? (:evaluation-entered? reply)))
              (is (= (subs original (:from request) (:to request)) (get-in reply [:source :original])))
              (is (zero? @calls)))))))))

(deftest source-change-while-waiting-at-the-interpreter-gate-keeps-a-healthy-heap
  (with-fixture
    (fn [{:keys [owned request run repl]}]
      (is (:success (repl "(def cell-base 40)")))
      (let [selected (promise) release (CountDownLatch. 1) caller (atom nil)
            resolve-context sandbox/sci-context-in]
        (try
          (with-redefs [sandbox/sci-context-in
                        (fn [world ref]
                          (let [interpreter (resolve-context world ref)]
                            (deliver selected true)
                            (when-not (.await release 30 TimeUnit/SECONDS)
                              (throw (ex-info "Finite test gate expired" {})))
                            interpreter))]
            (reset! caller (future (run request)))
            (is (= true (deref selected 30000 ::expired)))
            (write! owned "```clojure\n(def cell-base 999)\n```\n")
            (.countDown release)
            (let [reply (deref @caller 30000 ::expired)]
              (is (map? reply))
              (is (false? (:success reply)))
              (is (false? (:evaluation-entered? reply)))
              (is (= (subs original (:from request) (:to request)) (get-in reply [:source :original])))))
          (is (= "40" (:value (repl "cell-base"))))
          (finally (.countDown release) (when @caller (deref @caller 30000 nil))))))))

(deftest source-edit-after-entry-does-not-change-the-executed-literal-or-receipt
  (with-fixture
    (fn [{:keys [owned request run repl]}]
      (is (:success (repl "(def cell-base 40)")))
      (let [entered (promise) release (CountDownLatch. 1) caller (atom nil)
            evaluate evaluation/repl-evaluate later "```clojure\n(+ cell-base 200)\n```\n"]
        (try
          (with-redefs [evaluation/repl-evaluate
                        (fn [& args]
                          (deliver entered true)
                          (when-not (.await release 30 TimeUnit/SECONDS)
                            (throw (ex-info "Finite body gate expired" {})))
                          (apply evaluate args))]
            (reset! caller (future (run request)))
            (is (= true (deref entered 30000 ::expired)))
            (write! owned later)
            (.countDown release)
            (let [reply (deref @caller 30000 ::expired)]
              (is (:success reply)) (is (= "42" (:value reply)))
              (is (= (:revision request) (get-in reply [:source :revision])))
              (is (= (:expected request) (get-in reply [:source :entry])))
              (is (= "(+ cell-base 2)\n" (:input-source reply)))
              (is (= (subs original (:from request) (:to request)) (get-in reply [:source :original])))
              (is (= later (:text (files/read-file-in @owned path))))))
          (finally (.countDown release) (when @caller (deref @caller 30000 nil))))))))

(deftest a-cell-prepared-on-an-old-application-cannot-borrow-its-replacement
  (with-fixture
    (fn [{:keys [owner host request run]}]
      (let [replacement (doto (lifetime/create) lifetime/open!)
            evaluate evaluation/evaluate-repl! calls (atom 0)]
        (try
          (with-redefs [evaluation/evaluate-repl!
                        (fn [& args]
                          (lifetime/fence! owner)
                          (with-redefs-fn {#'application/installation (atom {:host host :owner replacement})}
                            #(apply evaluate args)))
                        evaluation/repl-evaluate (fn [& _] (swap! calls inc))]
            (let [reply (run request)]
              (is (false? (:success reply)))
              (is (false? (:evaluation-entered? reply)))
              (is (= (:revision request) (get-in reply [:source :revision])))
              (is (zero? @calls))))
          (is (lifetime/ready? replacement))
          (finally (lifetime/fence! replacement) (lifetime/join! replacement 30000)))))))
