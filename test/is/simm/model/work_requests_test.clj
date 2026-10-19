(ns is.simm.model.work-requests-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [jsonista.core :as json]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.work-requests :as requests]))

(defn- with-store [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :keep-history? false :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try (d/transact conn requests/schema) (f conn)
           (finally (d/release conn))))))

(defn- intent []
  {:party (random-uuid) :room (random-uuid) :client-id (random-uuid) :kind :attempt
   :limits {:budget-microdollars 10000 :max-dispatches 2 :timeout-ms 1000}
   :intent {:task "deterministic fixture" :candidate {:kind :echo}}})

(deftest publication-basis-is-retained-with-the-exact-request-intent
  (with-store
    (fn [conn]
      (let [body {:version 1 :root "wiki" :target {:repository (random-uuid) :store (random-uuid)
                                                 :backend :memory :branch :db :ref "refs/heads/main"}
                  :files {} :directories []}
            basis (assoc body :id (hasch/uuid [:simmis/knowledge-publication-basis-v1 body]))
            input (-> (intent)
                      (assoc-in [:intent :workflow :input-selection :execution-id] (random-uuid))
                      (assoc-in [:intent :knowledge/publication-basis] basis))
            {:keys [request]} (requests/claim! conn input)
            id (:work.request/id request)]
        (is (= (:id basis) (:work.request/publication-basis-id request)))
        (is (= basis (requests/publication-basis conn id)))
        (is (false? (:claimed? (requests/claim! conn input))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (requests/claim! conn (assoc-in input [:intent :knowledge/publication-basis :root] "elsewhere"))))
        (d/transact conn [[:db/add (:db/id request) :work.request/publication-basis-id (random-uuid)]])
        (is (thrown? clojure.lang.ExceptionInfo (requests/publication-basis conn id)))
        (d/transact conn [[:db/add (:db/id request) :work.request/publication-basis-id (:id basis)]
                          [:db/add (:db/id request) :work.request/kind :different]])
        (is (thrown? clojure.lang.ExceptionInfo (requests/publication-basis conn id)))))))

(deftest self-hashing-does-not-make-incomplete-or-unsafe-occupancy-valid
  (let [body {:version 1 :root "wiki"
              :target {:repository (random-uuid) :store (random-uuid)
                       :backend :memory :branch :db :ref "refs/heads/main"}
              :files {} :directories []}
        seal #(assoc % :id (hasch/uuid [:simmis/knowledge-publication-basis-v1 %]))]
    (is (= (seal body) (publication/validate! (seal body))))
    (doseq [bad [(assoc body :target {})
                 (assoc-in body [:target :ref] " ")
                 (assoc body :directories ["wiki/../escape"])
                 (assoc body :directories ["wiki" "wiki"])
                 (assoc body :files {"wiki/a.md" {:content (random-uuid) :size -1 :mode 33188}})
                 (assoc body :files {"other/a.md" {:content (random-uuid) :size 1 :mode 33188}})
                 (assoc body :unexpected true)]]
      (is (thrown? clojure.lang.ExceptionInfo (publication/validate! (seal bad)))))))

(deftest json-integer-limits-persist-as-typed-longs-without-changing-intent
  (with-store
    (fn [conn]
      (let [limits (json/read-value "{\"budget-microdollars\":10000,\"max-dispatches\":2,\"max-model-steps\":4,\"max-worker-admissions\":1,\"timeout-ms\":1000}" json/keyword-keys-object-mapper)
            input (assoc (intent) :limits (assoc limits :settlement :discard))
            first (requests/claim! conn input)]
        (is (:claimed? first))
        (doseq [[k n] limits]
          (let [stored (get (:request first) (keyword "work.request" (name k)))]
            (is (= n stored))
            (is (instance? Long stored))))
        (is (= :discard (get-in first [:request :work.request/settlement])))
        (is (= (:limits input) (:limits (requests/payload conn (:request first)))))
        (is (false? (:claimed? (requests/claim! conn input))))
        (is (false? (:claimed? (requests/claim! conn (update input :limits #(reduce-kv (fn [m k v] (assoc m k (if (integer? v) (long v) v))) {} %))))))))))

(deftest concurrent-identical-claims-have-one-durable-winner
  (with-store
    (fn [conn]
      (let [input (intent)
            start (promise)
            workers (mapv (fn [_] (future @start (requests/claim! conn input))) (range 8))]
        (deliver start true)
        (let [results (mapv #(deref % 10000 ::timeout) workers)
              ids (mapv #(get-in % [:request :work.request/id]) results)]
          (is (not-any? #{::timeout} results))
          (is (= 1 (count (filter :claimed? results))))
          (is (= 1 (count (set ids))))
          (is (= 1 (count (requests/requests conn))))
          (is (= {:kind :attempt :limits (:limits input) :input (:intent input)}
                 (requests/payload conn (:request (first results))))))))))

(deftest reused-request-identity-cannot-change-intent-or-limits
  (with-store
    (fn [conn]
      (let [input (intent)
            first (requests/claim! conn input)]
        (is (:claimed? first))
        (is (false? (:claimed? (requests/claim! conn input))))
        (doseq [changed [(assoc input :kind :fund)
                         (assoc-in input [:intent :task] "different")
                         (assoc-in input [:limits :budget-microdollars] 1000000)]]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"different work"
                                (requests/claim! conn changed))))
        (is (:claimed? (requests/claim! conn (assoc input :party (random-uuid)))))
        (is (:claimed? (requests/claim! conn (assoc input :room (random-uuid)))))
        (is (= 3 (count (requests/requests conn))))))))

(deftest ambiguous-success-is-read-back-before-any-scheduling-decision
  (with-store
    (fn [conn]
      (let [input (intent)
            transact d/transact
            claim (with-redefs [d/transact (fn [c tx]
                                             (transact c tx)
                                             (throw (ex-info "lost transaction response" {})))]
                    (requests/claim! conn input))
            row (:request claim)
            id (:work.request/id row)
            job-id (random-uuid)]
        (is (:claimed? claim) "the durable token identifies this caller as the winner")
        (is (false? (:claimed? (requests/claim! conn input))))
        (with-redefs [d/transact (fn [c tx]
                                   (transact c tx)
                                   (throw (ex-info "lost pointer response" {})))]
          (is (= job-id (:work.request/job
                         (requests/record-job! conn id (:work.request/claim row) job-id)))))
        (is (thrown? clojure.lang.ExceptionInfo
                     (requests/record-job! conn id (random-uuid) job-id)))
        (is (thrown? clojure.lang.ExceptionInfo
                     (requests/record-job! conn id (:work.request/claim row) (random-uuid))))
        (requests/interrupt! conn id nil)
        (is (= :submitted (:work.request/state (requests/get-request conn id))))))))

(deftest interrupted-admissions-are-not-restarted-by-retry
  (with-store
    (fn [conn]
      (let [input (intent)
            row (:request (requests/claim! conn input))
            id (:work.request/id row)]
        (requests/interrupt! conn id (random-uuid))
        (let [retry (requests/claim! conn input)]
          (is (false? (:claimed? retry)))
          (is (= :interrupted (get-in retry [:request :work.request/state])))
          (is (thrown? clojure.lang.ExceptionInfo
                       (requests/record-job! conn id (:work.request/claim row) (random-uuid)))))))))

(deftest assignment-and-interruption-are-one-atomic-transition
  (with-store
    (fn [conn]
      (dotimes [_ 12]
        (let [row (:request (requests/claim! conn (intent)))
              id (:work.request/id row)
              job (random-uuid)
              start (promise)
              assign (future @start
                             (try (requests/record-job! conn id (:work.request/claim row) job)
                                  (catch Exception _ :lost)))
              interrupt (future @start (requests/interrupt! conn id (random-uuid)))]
          (deliver start true)
          (is (not= ::timeout (deref assign 10000 ::timeout)))
          (is (not= ::timeout (deref interrupt 10000 ::timeout)))
          (let [after (requests/get-request conn id)]
            (is (or (and (= :submitted (:work.request/state after))
                         (= job (:work.request/job after)))
                    (and (= :interrupted (:work.request/state after))
                         (nil? (:work.request/job after)))))))))))

(deftest failed-write-does-not-admit-work
  (with-store
    (fn [conn]
      (let [input (intent)]
        (with-redefs [d/transact (fn [& _] (throw (ex-info "writer unavailable" {})))]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"writer unavailable"
                                (requests/claim! conn input))))
        (is (empty? (requests/requests conn)))
        (is (:claimed? (requests/claim! conn input)))))))

(deftest competing-jobs-cannot-reassign-a-request
  (with-store
    (fn [conn]
      (let [row (:request (requests/claim! conn (intent)))
            id (:work.request/id row)
            start (promise)
            jobs (repeatedly 4 random-uuid)
            attempts (mapv (fn [job]
                             (future @start
                                     (try (requests/record-job! conn id (:work.request/claim row) job)
                                          (catch Exception _ :lost)))) jobs)]
        (deliver start true)
        (let [outcomes (mapv #(deref % 10000 ::timeout) attempts)
              winner (requests/get-request conn id)]
          (is (= 1 (count (filter map? outcomes))))
          (is (some #{(:work.request/job winner)} jobs))
          (is (= :submitted (:work.request/state winner))))))))

(deftest competing-intents-have-one-winner-and-one-explicit-conflict
  (with-store
    (fn [conn]
      (let [input (intent)
            start (promise)
            attempts (mapv (fn [text]
                             (future @start
                                     (try (requests/claim! conn (assoc-in input [:intent :task] text))
                                          (catch clojure.lang.ExceptionInfo e (:type (ex-data e))))))
                           ["first intent" "second intent"])]
        (deliver start true)
        (let [outcomes (mapv #(deref % 10000 ::timeout) attempts)]
          (is (= 1 (count (filter map? outcomes))))
          (is (= 1 (count (filter #{:is.simm.model.work-requests/conflict} outcomes))))
          (is (= 1 (count (requests/requests conn)))))))))
