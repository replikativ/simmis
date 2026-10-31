(ns is.simm.ops.owned-room-evidence-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [dvergr.rooms.files :as native]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.model.room-evidence-collections :as receipts]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.ops.conversation-capture-test :as capture-fixture]
            [is.simm.ops.conversation-capture :as capture]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-knowledge-discovery-test :as discovery-fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-evidence :as collection]
            [is.simm.ops.room-collection-policy :as policy]
            [is.simm.ops.workflows :as workflows]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.documents.files :as files]))

(defn- fixture [f]
  (@#'capture-fixture/with-fixture
   (fn [{:keys [conn owned] :as data}]
     (d/transact conn receipts/schema)
     (doseq [[path text] discovery-fixture/source]
       (@#'discovery-fixture/write! owned (str "workflows/wiki/" path) text))
     (f data))))
(defn- input [{:keys [host w]}]
  (let [b (reads/resolve-host-read! host (ops/party-principal (:owner w)) (str (:room w)))]
    {:request (random-uuid) :name "wiki"
     :revision (sources/source-id b "wiki" (workflows/manifest-in (:db b) "wiki"))
     :bundle (random-uuid) :invocation {:client-id (random-uuid)}}))
(defn- collect [{:keys [host w capture-owner]} input]
  (lifetime/with-operation! capture-owner
    #(collection/collect! host (ops/party-principal (:owner w)) (str (:room w))
                          (reads/resolve-host-room! host (ops/party-principal (:owner w)) (str (:room w))) input)))

(deftest sidecar-is-explicit-closed-and-read-only
  (is (nil? (policy/declaration {})))
  (is (= policy/policy (policy/declaration {"collection-policy.edn" (pr-str policy/policy)})))
  (doseq [value [(assoc policy/policy :private-recordings true) (assoc policy/policy :max-messages 21)]]
    (is (thrown? Exception (policy/declaration {"collection-policy.edn" (pr-str value)})))))

(deftest only-original-winner-writes-and-exact-retained-recovery-never-resends
  (fixture
   (fn [{:keys [conn room w] :as data}]
     (@#'capture-fixture/message! room (:owner w) "A durable decision" {:role :user})
     (let [request (input data) calls (atom 0) native files/save-files-to!]
       (with-redefs [files/save-files-to! (fn [& args] (swap! calls inc) (apply native args))]
         (is (= :available (:status (collect data request))))
         (is (= :available (:status (collect data request))))
         (is (= 1 @calls))
         (is (= :write-acknowledged (get-in (collect data request) [:receipt :proof]))))
       (is (= :available (:room.collection/state (receipts/receipt @conn (:request request)))))))))

(deftest lost-native-ack-reconciles-observed-presence-without-resend
  (fixture
   (fn [{:keys [room w] :as data}]
     (@#'capture-fixture/message! room (:owner w) "Retained after uncertain ACK" {:role :user})
     (let [request (input data) native files/save-files-to!]
       (with-redefs [files/save-files-to! (fn [& args] (apply native args) (throw (ex-info "lost ACK" {:role :user})))]
         (is (thrown? Exception (collect data request))))
       (with-redefs [files/save-files-to! (fn [& _] (throw (AssertionError. "Never resend")))]
         (is (= :observed-present (get-in (collect data request) [:receipt :proof]))))))))

(deftest bounded-receipt-excludes-unselected-transcript-backlog
  (fixture
   (fn [{:keys [conn room w] :as data}]
     (@#'capture-fixture/message! room (:owner w) "The oldest selected original" {:role :user})
     (let [request (input data)
           original-source capture/room-source
           ;; These originals fit the scan count and per-message limit, but
           ;; their combined text exceeds the control artifact limit. The real
           ;; selector must still collect its ordinary bounded first batch.
           backlog (mapv (fn [_] {:id (random-uuid) :author (:owner w) :actor-kind :human
                                  :timestamp Long/MAX_VALUE :metadata {}
                                  :text (apply str (repeat 200000 "x"))}) (range 24))]
       (is (> (reduce + (map #(count (:text %)) backlog)) collection/max-intent-bytes))
       (with-redefs [capture/room-source (fn [binding]
                                         (update (original-source binding nil) :rows into backlog))]
         (is (= :available (:status (collect data request))))
         (let [row (receipts/receipt @conn (:request request))
               intent (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                                                          (:room.collection/intent row)
                                                          collection/max-intent-bytes))
               selected (set (map :message-id (get-in intent [:plan :artifacts])))
               retained (set (map :id (get-in intent [:source :rows])))]
           (is (= 3 (count selected)))
           (is (= selected retained) "Only selected originals enter the collection receipt")
           (is (< (count retained) (count backlog)))
           (is (= :available (:status (collect data request))) "Exact recovery remains available")))))))

(deftest lost-claim-ack-holds-missing-destinations-without-writing
  (fixture
   (fn [{:keys [room w] :as data}]
     (@#'capture-fixture/message! room (:owner w) "Claim acknowledgement may be lost" {:role :user})
     (let [request (input data) publish artifact/publish-value!]
       (with-redefs [artifact/publish-value! (fn [& args] (apply publish args) (throw (ex-info "lost claim ACK" {:role :user})))
                     files/save-files-to! (fn [& _] (throw (AssertionError. "No claim ACK means no write")))]
         (is (thrown? Exception (collect data request))))
       (with-redefs [files/save-files-to! (fn [& _] (throw (AssertionError. "No recovery resend")))]
         (is (thrown? Exception (collect data request))))))))

(deftest effect-gate-refuses-source-default-authority-and-lifetime-changes
  (doseq [change [:source :default :authority :lifetime]]
    (fixture
     (fn [{:keys [room w conn owned attached attached-descriptor capture-owner] :as data}]
       (let [id (@#'capture-fixture/message! room (:owner w) "Original" {:role :user})
             request (input data) before @owned other @attached write native/write-to!]
         (with-redefs [native/write-to!
                       (fn [& args]
                         (case change
                           :source (binding [ec/*execution-context* (:ctx room)]
                                     (d/transact (:conn (:store room)) [{:message/id id :message/content "Edited"}]))
                           :default (d/transact conn [{:room/id (:room w)
                                                       :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
                           :authority (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner]
                                                       [:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]])
                           :lifetime (lifetime/fence! capture-owner))
                         (apply write args))]
           (is (thrown? Exception (collect data request)) (str change)))
         (is (= before @owned)) (is (= other @attached)))))))

(deftest partial-and-conflicting-retained-plans-hold-without-resend
  (doseq [conflict? [false true]]
    (fixture
     (fn [{:keys [room w conn owned] :as data}]
       (@#'capture-fixture/message! room (:owner w) "One" {:role :user})
       (@#'capture-fixture/message! room (:owner w) "Two" {:role :user})
       (let [request (input data) publish artifact/publish-value!]
         (with-redefs [artifact/publish-value! (fn [& args] (apply publish args) (throw (ex-info "lost claim ACK" {})))]
           (is (thrown? Exception (collect data request))))
         (let [row (receipts/receipt @conn (:request request))
               intent (:value (artifact/get-bounded-value (artifact/datahike-store conn) (:room.collection/intent row) collection/max-intent-bytes))
               destination (first (:destinations intent))]
           (@#'discovery-fixture/write! owned (:path destination) (if conflict? "Conflicting contents" (:text destination))))
         (let [before @owned]
           (with-redefs [files/save-files-to! (fn [& _] (throw (AssertionError. "No recovery resend")))]
             (is (thrown? Exception (collect data request))))
           (is (= before @owned))))))))


(deftest unselected-appends-do-not-strand-the-selected-originals
  (fixture
   (fn [{:keys [room w] :as data}]
     (@#'capture-fixture/message! room (:owner w) "Selected original" {:role :user})
     (let [request (input data) write native/write-to!]
       (with-redefs [native/write-to! (fn [& args]
                                       (@#'capture-fixture/message! room (:owner w) "Unrelated append" {:role :user})
                                       (apply write args))]
         (is (= :available (:status (collect data request)))))))))

(defn- scheduled-invocation! [{:keys [conn w]}]
  (d/transact conn schedules/schema)
  (let [activation (random-uuid) revision (random-uuid) occurrence (random-uuid) claim (random-uuid)]
    (d/transact conn [{:schedule.activation/id activation :schedule.activation/revision revision
                       :schedule.activation/by (:owner w) :schedule.activation/room (:room w)
                       :schedule.activation/enabled? true :schedule.activation/active-occurrence occurrence}
                      {:schedule.occurrence/id occurrence :schedule.occurrence/activation activation
                       :schedule.occurrence/revision revision :schedule.occurrence/claim claim
                       :schedule.occurrence/state :admitting}])
    {:activation activation :revision revision :occurrence occurrence :claim claim}))

(deftest scheduled-collection-rechecks-claim-at-receipt-and-native-gates
  (doseq [gate [:receipt :native] change [:revision :claim :pointer :state]]
    (fixture
     (fn [{:keys [conn room w owned] :as data}]
       (@#'capture-fixture/message! room (:owner w) "Scheduled original" {:role :user})
       (let [invocation (scheduled-invocation! data)
             request (assoc (input data) :invocation invocation)
             mutate! #(d/transact conn
                         [(case change
                            :revision {:schedule.activation/id (:activation invocation) :schedule.activation/revision (random-uuid)}
                            :claim {:schedule.occurrence/id (:occurrence invocation) :schedule.occurrence/claim (random-uuid)}
                            :pointer {:schedule.activation/id (:activation invocation) :schedule.activation/active-occurrence (random-uuid)}
                            :state {:schedule.occurrence/id (:occurrence invocation) :schedule.occurrence/state :interrupted})])
             publish artifact/publish-value! write native/write-to! before @owned]
         (if (= :receipt gate)
           (with-redefs [artifact/publish-value! (fn [store value callback]
                                                  (publish store value (fn [ref] (mutate!) (callback ref))))]
             (is (thrown? Exception (collect data request))))
           (with-redefs [native/write-to! (fn [& args] (mutate!) (apply write args))]
             (is (thrown? Exception (collect data request)))))
         (is (= before @owned)))))))

(deftest pause-after-scheduled-claim-preserves-the-owned-admission
  (fixture
   (fn [{:keys [conn room w] :as data}]
     (@#'capture-fixture/message! room (:owner w) "Claimed before pause" {:role :user})
     (let [invocation (scheduled-invocation! data) request (assoc (input data) :invocation invocation)
           write native/write-to!]
       (with-redefs [native/write-to! (fn [& args]
                                       (schedules/set-enabled! conn (:activation invocation) false)
                                       (apply write args))]
         (is (= :available (:status (collect data request)))))))))


(deftest canonical-writer-row-gate-requires-real-available-receipt
  (fixture
   (fn [{:keys [conn room w] :as data}]
     (@#'capture-fixture/message! room (:owner w) "Available before request creation" {:role :user})
     (let [request (input data) result (collect data request) link (:receipt result)
           id (:request request) row (receipts/receipt @conn id)]
       (is (= link (receipts/link-current-in! @conn id link)))
       (is (thrown? Exception (receipts/link-current-in! @conn (random-uuid) link)))
       (is (thrown? Exception (receipts/link-current-in! @conn id {:status :skipped :reason :active-knowledge-review})))
       (is (thrown? Exception
                    (receipts/link-current-in!
                     (:db-after (d/with @conn [{:db/id (:db/id row) :room.collection/state :claimed}]))
                     id link)))))))
