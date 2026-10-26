(ns is.simm.ops.knowledge-batch-dismissal-test
  "Real control transactions, original App ownership and durable frozen batch
   data. The Proposal fixture substitutes certification, not decision writes."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.model.access :as access]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.knowledge-batch-filing-test :as fixture]
            [is.simm.ops.knowledge-batch-proof :as proof]
            [is.simm.ops.knowledge-batch-resolution :as resolution]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.proposals :as proposals]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.proposals-remote :as remote])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(defn- fixture [f] (@#'fixture/proposed-fixture f))

(defn- dismiss! [{:keys [conn w value]}]
  (proposals/decide-in! conn (:proposal value) (:owner w) {:verb :dismiss-proposal} {:note "Skip this revision"}))

(deftest dismissal-skips-exact-selected-sources-and-preserves-historical-filing
  (fixture
   (fn [{:keys [conn w room ready value principal] :as data}]
     (is (= {:status :dismissed} (dismiss! data)))
     (let [batch (batches/batch @conn (:batch ready))]
       (is (= :closed (:knowledge.batch/state batch)))
       (is (= :dismissed (:knowledge.batch/outcome batch)))
       (is (string? (:knowledge.batch/decision-filing batch)))
       (is (nil? (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))
       (is (= 1 (count (filter #(= :skipped (:knowledge.item/state %)) (streams/items @conn (:stream ready))))))
       (is (not-any? #(= :processed (:knowledge.item/state %)) (streams/items @conn (:stream ready))))
       (is (= 1 (count (streams/pending-items @conn (:stream ready)))))
       (let [later (random-uuid)]
         (d/transact conn [[:db/add [:knowledge.stream/id (:stream ready)] :knowledge.stream/active-batch later]])
         (is (= value (publish/file-completed-request! conn principal (:room w) room (:request value))))
         (is (= later (:knowledge.stream/active-batch (streams/stream @conn (:stream ready))))))))))

(deftest requesting-changes-keeps-the-batch-held-for-review
  (fixture
   (fn [{:keys [conn w ready value]}]
     (let [result (proposals/decide-in! conn (:proposal value) (:owner w) {:verb :request-changes} {:body "Please revise"})]
       (is (uuid? (:comment result)))
       (is (= [] (:reopened result)))
       (is (= :review (:knowledge.batch/state (batches/batch @conn (:batch ready)))))
       (is (= (:batch ready) (:knowledge.stream/active-batch (streams/stream @conn (:stream ready)))))
       (is (not-any? #(#{:processed :skipped} (:knowledge.item/state %)) (streams/items @conn (:stream ready))))))))

(deftest author-withdrawal-does-not-require-room-merge-permission
  (fixture
   (fn [{:keys [conn w] :as data}]
     (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]
                       :room/parties [[:party/id (:owner w)] [:party/id (:member w)]]}])
     (is (false? (access/can? @conn (:owner w) :merge {:room (:room w)})))
     (is (= {:status :dismissed} (dismiss! data))))))

(deftest unresolved-publication-refuses-withdrawal-before-reading-batch-artifacts
  (fixture
   (fn [{:keys [conn w room value] :as data}]
     (adoption/claim! conn (:owner w) (:room w) room
                      (adoption/prepare conn (:owner w) (:room w) room (:proposal value)))
     (with-redefs [resolution/retained! (fn [& _] (throw (AssertionError. "Unresolved publication must refuse before artifact IO")))]
       (is (thrown? Exception (dismiss! data)))))))

(deftest lost-decision-ack-does-not-manufacture-original-success
  (fixture
   (fn [{:keys [conn w room ready value principal] :as data}]
     (let [transact d/transact lost? (atom false)]
       (with-redefs [d/transact (fn [c tx]
                                 (let [report (transact c tx)]
                                   (when (and (identical? c conn)
                                              (= :dismissed (:knowledge.batch/outcome (batches/batch @conn (:batch ready))))
                                              (compare-and-set! lost? false true))
                                     (throw (ex-info "Lost joint dismissal ACK" {})))
                                   report))]
         (is (thrown? Exception (dismiss! data))))
       (is @lost?)
       (is (= :closed (:knowledge.batch/state (batches/batch @conn (:batch ready)))))
       ;; Historical filing is an observation, not this decision's ACK.
       (is (= value (publish/file-completed-request! conn principal (:room w) room (:request value))))))))

(deftest partial-dismissed-only-state-and-changed-filing-hash-refuse-historical-success
  (doseq [damage [:partial :hash]]
    (fixture
     (fn [{:keys [conn w room ready value principal] :as data}]
       (dismiss! data)
       (let [batch (batches/batch @conn (:batch ready))]
         (d/transact conn
           (case damage
             :partial [{:db/id (:db/id batch) :knowledge.batch/state :review}]
             :hash [[:db/add [:proposal/id (:proposal value)] :proposal/filing-hash (apply str (repeat 64 "0"))]]))
         (is (thrown? Exception (publish/file-completed-request! conn principal (:room w) room (:request value)))))))))

(defn- write-commit! [conn path text]
  (repo/write! conn path (.getBytes ^String text "UTF-8"))
  (repo/stage-all! conn)
  (repo/commit! conn {:message "Source revision after dismissal"}))

(deftest skipped-source-version-is-not-automatically-queued-again
  (fixture
   (fn [{:keys [conn host w room ready expected owned] :as data}]
     (dismiss! data)
     (let [selected (first (get-in expected [:selection :items]))
           path (:path selected)
           original (get-in ready [:captured :inputs (str "/in/knowledge/sources/" path)])
           request (:batch-input data)]
       (write-commit! owned path "New source version")
       (discovery/refresh! host (:principal data) (str (:room w)) request)
       (is (= 2 (count (streams/pending-items @conn (:stream ready)))))
       (write-commit! owned path original)
       (discovery/refresh! host (:principal data) (str (:room w)) request)
       (is (= 1 (count (streams/pending-items @conn (:stream ready)))))
       (is (= :skipped (:knowledge.item/state
                       (first (filter #(= (:id selected) (:knowledge.item/id %)) (streams/items @conn (:stream ready)))))))))))

(deftest queued-decision-cannot-borrow-a-replacement-app
  (fixture
   (fn [{:keys [host conn w value]}]
     (let [old (doto (lifetime/create) lifetime/open!)
           replacement (doto (lifetime/create) lifetime/open!)
           current (atom old)
           capture (fn [] (let [original @current]
                            (fn [f] (lifetime/with-operation! original #(f host)))))]
       (try
         (with-redefs [application/capture-operation capture
                       proposals/decide-in! (fn [& _] (throw (AssertionError. "Retired queued operation must not decide")))]
           (let [body (remote/prepare-decision-operation (:owner w) :dismiss-proposal (str (:proposal value)) {})]
             (reset! current replacement)
             (lifetime/fence! old)
             (is (thrown? Exception (body)))))
         (is (= :open (:proposal/status (d/pull @conn [:proposal/status] [:proposal/id (:proposal value)]))))
         (finally
           (lifetime/fence! old) (lifetime/join! old 1000)
           (lifetime/fence! replacement) (lifetime/join! replacement 1000)))))))

(deftest app-shutdown-retains-a-blocked-artifact-reader-and-refuses-the-decision
  (fixture
   (fn [{:keys [conn host w ready value app-owner] :as data}]
     (let [request! proof/request! entered (promise) release (CountDownLatch. 1)
           worker (atom nil) ticket (atom nil)]
       (try
         (with-redefs [proof/request! (fn [& args]
                                       (reset! ticket lifetime/*ticket*)
                                       (deliver entered true)
                                       (loop []
                                         (when-not (try (.await release 100 TimeUnit/MILLISECONDS)
                                                        (catch InterruptedException _ false))
                                           (recur)))
                                       (apply request! args))]
           (reset! worker
                   (future ((get-in host [:config :host-lifecycle :with-operation])
                            #(try (dismiss! data) :unexpected (catch Exception _ :held)))))
           (is (= true (deref entered 15000 ::expired)))
           (is (lifetime/retained? @ticket))
           (is (nil? (deref (future (lifetime/fence! app-owner)) 5000 ::expired)))
           (is (lifetime/retained? @ticket))
           (is (= :physical-exit-unconfirmed
                  (deref (future (try (lifetime/join! app-owner 0) nil
                                      (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))
                         5000 ::join-timeout)))
           (.countDown release)
           (is (= :held (deref @worker 15000 ::expired)))
           (is (not (lifetime/retained? @ticket)))
           (is (= :open (:proposal/status (d/pull @conn [:proposal/status] [:proposal/id (:proposal value)]))))
           (is (= :review (:knowledge.batch/state (batches/batch @conn (:batch ready))))))
         (finally (.countDown release) (when @worker (deref @worker 15000 nil))))))))

(deftest already-fenced-app-refuses-a-fresh-decision-before-artifact-io
  (fixture
   (fn [{:keys [conn value app-owner] :as data}]
     (is (nil? (deref (future (lifetime/fence! app-owner)) 5000 ::expired)))
     (with-redefs [resolution/retained! (fn [& _] (throw (AssertionError. "Fenced fresh decision may not read artifacts")))]
       (is (thrown? Exception (dismiss! data))))
     (is (= :open (:proposal/status (d/pull @conn [:proposal/status] [:proposal/id (:proposal value)])))))))

(deftest structurally-valid-aborted-history-must-match-the-dismissed-filing
  (doseq [field [:proposal :disposition :room :filing :target]]
    (fixture
     (fn [{:keys [conn w room value principal] :as data}]
       (let [prepared (adoption/prepare conn (:owner w) (:room w) room (:proposal value))
             original (get-in prepared [:proof :binding])
             foreign (assoc original field (case field
                                             :filing (apply str (repeat 64 "0"))
                                             :target (assoc (:target original) :store (random-uuid))
                                             (random-uuid)))
             at (java.util.Date.)
             operation (adoptions/validate! (assoc (adoptions/make foreign (random-uuid) at)
                                                  :state :aborted :reason :fixture/not-dispatched))
             fork (get-in prepared [:proof :fork :db/id])]
         (dismiss! data)
         ;; Synthetic journal producer: structural withdrawal history is valid,
         ;; but no real publication or negative native receipt is claimed here.
         (d/transact conn [(@#'adoptions/entity operation)
                           [:db/add fork :proposal.fork/file-adoption [:file.adoption/id (:id operation)]]])
         (is (= operation (adoptions/withdrawn-operation @conn fork)))
         (is (thrown? Exception (publish/file-completed-request! conn principal (:room w) room (:request value)))))))))
