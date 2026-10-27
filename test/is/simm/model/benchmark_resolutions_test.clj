(ns is.simm.model.benchmark-resolutions-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [is.simm.model.benchmark-resolutions :as model]
            [is.simm.model.work-requests :as requests]))

(defn- fixture [f]
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :keep-history? false :schema-flexibility :write}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn (into requests/schema model/schema))
        (let [request (:request (requests/claim! conn {:party (random-uuid) :room (random-uuid)
                                                      :client-id (random-uuid) :kind :benchmark
                                                      :intent {:offline true}}))
              request (requests/record-job! conn (:work.request/id request)
                                            (:work.request/claim request) (random-uuid))]
          (f conn request))
        (finally (d/release conn) (d/delete-database cfg))))))

(defn- plan []
  {:version 1 :semantic-id (random-uuid) :source-id (random-uuid)
   :substitution :clean-selected-fixture-checkpoint-v1
   :cases [{:ordinal 0 :case {:kind :default} :manifest-id (random-uuid)}
           {:ordinal 1 :case {:kind :named :name "default"} :manifest-id (random-uuid)}]})

(defn- tokens []
  {:id (random-uuid) :claim (random-uuid)
   :cases (mapv (fn [_] {:root (random-uuid) :claim (random-uuid)}) (range 2))})

(defn- publish! [conn value f]
  (artifact/publish-value! (artifact/datahike-store conn) value f))

(defn- prepare! [conn request plan tokens]
  (publish! conn plan #(d/transact conn [[:db.fn/call model/prepare-tx request plan % tokens]]))
  (model/resolution @conn (:work.request/id request)))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest plans-use-tagged-contiguous-cases
  (let [p (plan)]
    (is (= p (model/validate-plan! p)))
    (let [id (random-uuid)] (is (not= (model/case-id id 0) (model/case-id id 1))))
    (doseq [bad [(assoc-in p [:cases 1 :ordinal] 2)
                 (assoc-in p [:cases 1 :case] {:kind :default})
                 (assoc-in p [:cases 1 :case :name] "")
                 (assoc p :extra true)
                 (assoc p :cases [])
                 (update p :cases #(apply list %))]]
      (is (= :invalid-plan (reason #(model/validate-plan! bad)))))
  (let [p (plan)]
    (is (not= (model/plan-id p) (model/plan-id (assoc p :source-id (random-uuid))))))))

(deftest prepare-is-at-most-once-and-reserves-distinct-roots
  (fixture
   (fn [conn request]
     (let [p (plan) t (tokens) first (prepare! conn request p t)
           cases (model/cases @conn (:id t))]
       (is (= :preparing (:benchmark.resolution/state first)))
       (is (= (mapv :root (:cases t)) (mapv :benchmark.case/root cases)))
       (is (= [:planned :planned] (mapv :benchmark.case/state cases)))
       (is (= first (prepare! conn request p (tokens))))
       (is (= cases (model/cases @conn (:id t))))
       (is (= :resolution-changed
              (reason #(model/prepare-tx @conn request (assoc p :source-id (random-uuid)) nil (tokens)))))
     (doseq [bad [(assoc-in (tokens) [:cases 1 :root] nil)
                  (let [t (tokens)] (assoc-in t [:cases 1 :root] (get-in t [:cases 0 :root])))]]
       ;; Use a fresh Request to exercise creation, rather than replay.
       (let [req (-> request (dissoc :db/id) (assoc :work.request/id (random-uuid)))]
         (d/transact conn [req])
         (is (= :invalid-tokens
                (reason #(model/prepare-tx @conn (requests/get-request conn (:work.request/id req))
                                          (plan) nil bad))))))))))

(defn- retain-case! [conn request p r c]
  (let [selection-id (random-uuid)]
    (publish! conn {:selection selection-id}
              #(d/transact conn [[:db.fn/call model/case-pinning-tx request r p c selection-id %]]))
    (let [c (nth (model/cases @conn (:benchmark.resolution/id r)) (:benchmark.case/ordinal c))]
      (d/transact conn [[:db.fn/call model/case-retained-tx request r p c]]))))

(deftest readiness-needs-every-exact-case-and-keeps-the-original-pointer
  (fixture
   (fn [conn request]
     (let [p (plan) r (prepare! conn request p (tokens))
           resolution-id (:benchmark.resolution/id r)
           cs (model/cases @conn resolution-id)]
       (is (= :invalid-state (reason #(model/case-retained-tx @conn request r p (first cs)))))
       (is (= :cases-not-retained (reason #(model/ready-tx @conn request r p cs (random-uuid) nil))))
       (retain-case! conn request p r (first cs))
       (is (= :cases-not-retained
              (reason #(model/ready-tx @conn request r p (model/cases @conn resolution-id) (random-uuid) nil))))
       (retain-case! conn request p r (second cs))
       (let [retained (model/cases @conn resolution-id) digest (random-uuid)]
         (publish! conn {:resolved digest}
                   (fn [ref]
                     (is (= :cases-not-retained
                            (reason #(model/ready-tx @conn request r p (vec (reverse retained)) digest ref))))
                     (d/transact conn [[:db.fn/call model/ready-tx request r p retained digest ref]])
                     (let [ready (model/resolution @conn (:work.request/id request))]
                       (is (= :ready (:benchmark.resolution/state ready)))
                       (is (= [] (model/ready-tx @conn request ready p retained digest ref)))
                       (is (= :resolved-changed
                              (reason #(model/ready-tx @conn request ready p retained (random-uuid) ref))))
                       (is (= :resolution-changed
                              (reason #(model/ready-tx @conn request r p retained digest ref))))))))))))

(deftest stale-owner-and-selection-replacement-cannot-advance
  (fixture
   (fn [conn request]
     (let [p (plan) r (prepare! conn request p (tokens))
           c (first (model/cases @conn (:benchmark.resolution/id r)))]
       (doseq [attr [:work.request/job :work.request/claim :work.request/intent-id
                    :work.request/by :work.request/room :work.request/client-id]]
         (d/transact conn [[:db/add (:db/id request) attr (random-uuid)]])
         (is (= :request-changed
                (reason #(model/case-pinning-tx @conn request r p c (random-uuid) nil))))
         (d/transact conn [[:db/add (:db/id request) attr (get request attr)]]))
       (publish! conn {:selection :one}
                 (fn [ref]
                   (let [digest (random-uuid)]
                     (d/transact conn [[:db.fn/call model/case-pinning-tx request r p c digest ref]])
                     (let [pinning (first (model/cases @conn (:benchmark.resolution/id r)))]
                       (is (= [] (model/case-pinning-tx @conn request r p pinning digest ref)))
                       (is (= :case-changed
                              (reason #(model/case-retained-tx @conn request r p c))))
                       (is (= :selection-changed
                              (reason #(model/case-pinning-tx @conn request r p pinning (random-uuid) ref))))
                       (requests/mark-returned! conn (:work.request/id request))
                       (is (= :request-changed
                              (reason #(model/case-retained-tx @conn request r p pinning))))))))))))

(deftest orphan-case-id-and-root-reuse-refuse-instead-of-upserting
  (fixture
   (fn [conn request]
     (let [p (plan) t (tokens) id (model/case-id (:id t) 0)
           root (random-uuid)]
       (d/transact conn [{:benchmark.case/id id :benchmark.case/root root}])
       (is (= :identity-reused (reason #(model/prepare-tx @conn request p nil t))))
       (let [other (assoc-in (tokens) [:cases 0 :root] root)]
         (is (= :identity-reused (reason #(model/prepare-tx @conn request p nil other)))))
       (is (nil? (model/resolution @conn (:work.request/id request))))))))

(deftest out-of-plan-case-cannot-progress-and-fresh-request-observation-cannot-change-owner
  (fixture
   (fn [conn request]
     (let [p (plan) r (prepare! conn request p (tokens))
           id (:benchmark.resolution/id r)]
       (d/transact conn [{:benchmark.case/id (model/case-id id 2)
                          :benchmark.case/resolution id :benchmark.case/ordinal 2
                          :benchmark.case/claim (random-uuid) :benchmark.case/root (random-uuid)
                          :benchmark.case/state :planned}])
       (let [extra (nth (model/cases @conn id) 2)]
         (is (= :case-changed
                (reason #(model/case-pinning-tx @conn request r p extra (random-uuid) nil)))))
       (d/transact conn [[:db/add (:db/id request) :work.request/by (random-uuid)]])
       (let [changed (requests/get-request conn (:work.request/id request))]
         (is (= :resolution-changed
                (reason #(model/prepare-tx @conn changed p nil (tokens))))))))))

(deftest concurrent-preparation-keeps-one-original-token-family
  (fixture
   (fn [conn request]
     (let [p (plan) gate (promise) done (java.util.concurrent.CountDownLatch. 4)
           attempts (mapv (fn [_] (let [t (tokens)]
                                    {:tokens t :result
                                     (future (try @gate (prepare! conn request p t)
                                                  (finally (.countDown done))))})) (range 4))]
       (try
         (deliver gate true)
         (let [results (mapv #(deref (:result %) 10000 ::timeout) attempts)]
           (is (not-any? #{::timeout} results))
           (is (= 1 (count (set results))))
           (is (= 1 (count (filter true? (map (fn [attempt result]
                                               (= (get-in attempt [:tokens :id])
                                                  (:benchmark.resolution/id result))) attempts results)))))
           (is (= 2 (count (model/cases @conn (:benchmark.resolution/id (first results)))))))
         (finally (deliver gate true) (.await done)))))))

(deftest lost-write-ack-keeps-the-same-root-family-and-returned-requests-cannot-advance
  (fixture
   (fn [conn request]
     (let [p (plan) t (tokens) native d/transact]
       (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Injected ACK loss"
                            (with-redefs [d/transact (fn [& args]
                                                      (apply native args)
                                                      (throw (ex-info "Injected ACK loss" {})))]
                              (prepare! conn request p t))))
       (let [r (model/resolution @conn (:work.request/id request))
             cs (model/cases @conn (:id t))]
         (is (= (:claim t) (:benchmark.resolution/claim r)))
         (is (= (mapv :root (:cases t)) (mapv :benchmark.case/root cs)))
         (is (= r (prepare! conn request p (tokens))))
         (requests/mark-returned! conn (:work.request/id request))
         (let [returned (requests/get-request conn (:work.request/id request))]
           (doseq [f [#(model/prepare-tx @conn returned p nil (tokens))
                      #(model/case-pinning-tx @conn returned r p (first cs) (random-uuid) nil)
                      #(model/case-retained-tx @conn returned r p (first cs))
                      #(model/ready-tx @conn returned r p cs (random-uuid) nil)]]
             (is (= :request-changed (reason f))))))))))
