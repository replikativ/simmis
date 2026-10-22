(ns is.simm.ops.owned-schedule-controls-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.model.registry :as models]
            [dvergr.ops :as dops]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workflow-schedules :as model]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-schedule-admission-test :as fixture]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-schedules :as schedules]
            [is.simm.ops.workflows :as workflows]))

(defn- principal [w k] (principals/party-principal (get w k)))
(defn- failure [f] (try (f) nil (catch clojure.lang.ExceptionInfo e e)))
(defn- with-activation [order kind f]
  (@#'fixture/with-fixture
   order
   (fn [{:keys [host w input] :as data}]
     (let [row (schedules/create-owned! host (principal w :owner) (str (:room w)) (input kind))]
       (f (assoc data :row row :handle (str (:room w)) :p (principal w :owner)))))))

(deftest list-and-pause-need-neither-current-default-nor-provider-in-both-orders
  (doseq [order [true false] kind [:workflow :benchmark]]
    (with-activation order kind
      (fn [{:keys [conn w host row handle p]}]
        (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository [:system/id (:repo-sys w)]]])
        (let [forbidden (fn [& _] (throw (AssertionError. "Metadata acquired a live execution dependency")))]
          (with-redefs [reads/resolve-host-read! forbidden rooms/live-room forbidden
                        models/get-model forbidden workflows/snapshot-in forbidden model/target forbidden]
            (let [[summary] (schedules/list-owned! host (principal w :member) handle)]
              (is (= (str (:schedule.activation/id row)) (:id summary)))
              (is (uuid? (parse-uuid (:source-revision summary))))
              (is (not (contains? summary :snapshot))))
            (is (false? (:schedule.activation/enabled?
                         (schedules/set-enabled-owned! host p handle (:schedule.activation/id row) false))))
            (is (empty? (d/datoms @conn :aevt :work.request/id)))))))))

(deftest replacement-owner-can-pause-but-cannot-resume-the-revoked-creators-work
  (with-activation true :workflow
    (fn [{:keys [conn w host row handle]}]
      (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}])
      (let [p (principal w :member) id (:schedule.activation/id row)]
        (is (= 1 (count (schedules/list-owned! host p handle))))
        (is (false? (:schedule.activation/enabled? (schedules/set-enabled-owned! host p handle id false))))
        (is (some? (failure #(schedules/set-enabled-owned! host p handle id true))))
        (is (false? (:schedule.activation/enabled? (model/activation conn id))))))))

(deftest benchmark-controls-need-current-operator-and-room-authority
  (with-activation true :benchmark
    (fn [{:keys [conn w host row handle p]}]
      (let [id (:schedule.activation/id row)]
        (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]])
        (is (= 1 (count (schedules/list-owned! host p handle))))
        (is (some? (failure #(schedules/set-enabled-owned! host p handle id false))))
        (is (true? (:schedule.activation/enabled? (model/activation conn id))))))))

(deftest valid-owned-resume-needs-no-current-file-default
  (doseq [kind [:workflow :benchmark]]
    (with-activation true kind
      (fn [{:keys [conn w host row handle p]}]
        (let [id (:schedule.activation/id row)]
          (schedules/set-enabled-owned! host p handle id false)
          (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository [:system/id (:repo-sys w)]]])
          (with-redefs [reads/resolve-host-read! (fn [& _] (throw (AssertionError. "Resume acquired default")))]
            (is (true? (:schedule.activation/enabled? (schedules/set-enabled-owned! host p handle id true)))))
          (is (= (:schedule.activation/revision row) (:schedule.activation/revision (model/activation conn id)))))))))

(deftest resume-advice-shares-the-writer-predicate-without-acquiring-execution
  (with-activation true :workflow
    (fn [{:keys [conn w host row handle p]}]
      (let [id (:schedule.activation/id row) oid (random-uuid)
            base {:schedule.occurrence/id oid :schedule.occurrence/activation id
                  :schedule.occurrence/revision (:schedule.activation/revision row)
                  :schedule.occurrence/intended-at (java.util.Date. 0)
                  :schedule.occurrence/created-at (java.util.Date. 0)
                  :schedule.occurrence/coalesced 0 :schedule.occurrence/state :pending}
            forbidden (fn [& _] (throw (AssertionError. "Resume advice acquired execution")))
            basis #(let [db @conn] {:commit (d/commit-id db) :max-tx (:max-tx db) :config (:config db)})]
        (schedules/set-enabled-owned! host p handle id false)
        (d/transact conn [base])
        (doseq [[change blocked?] [[{} false]
                                   [{:schedule.occurrence/state :linked} false]
                                   [{:schedule.occurrence/state :admitting} true]
                                   [{:schedule.occurrence/state :interrupted} true]
                                   [{:schedule.occurrence/activation (random-uuid)} true]
                                   [{:schedule.occurrence/revision (random-uuid)} true]
                                   [:missing true]]]
          (d/transact conn [(assoc (merge base (when (map? change) change))
                                  :db/id [:schedule.occurrence/id oid])
                            {:db/id (:db/id row)
                             :schedule.activation/active-occurrence (if (= :missing change) (random-uuid) oid)}])
          (let [before (basis)
                [summary] (with-redefs [reads/resolve-host-read! forbidden rooms/live-room forbidden
                                       models/get-model forbidden workflows/snapshot-in forbidden model/target forbidden
                                       work/prepare forbidden work/submit-prepared! forbidden]
                            (schedules/list-owned! host (principal w :member) handle))]
            (is (= (when blocked? :reconciliation-required) (:resume-blocked summary)) (str change))
            (is (= before (basis)) "Metadata advice never writes")
            (is (not (contains? summary :active-occurrence)) "Never expose a foreign pointer")
            (when blocked?
              (is (some? (failure #(schedules/set-enabled-owned! host p handle id true))))
              (is (= before (basis)) "Rejected Resume does not write"))))
        (let [active (:schedule.activation/active-occurrence (model/activation conn id))]
          (d/transact conn [[:db/retract [:schedule.activation/id id] :schedule.activation/active-occurrence active]]))
        (is (nil? (:resume-blocked (first (schedules/list-owned! host p handle)))))
        (is (true? (:schedule.activation/enabled? (schedules/set-enabled-owned! host p handle id true))))
        (is (empty? (d/datoms @conn :aevt :work.request/id)))))))

(deftest pause-withdraws-only-pending-occurrences
  (with-activation true :workflow
    (fn [{:keys [conn host row handle p]}]
      (let [id (:schedule.activation/id row) target (model/managed-target conn row)]
        (model/claim-due! conn row (:cadence target) (:schedule.activation/next-fire row))
        (let [oid (:schedule.activation/active-occurrence (model/activation conn id))]
          (is (uuid? oid))
          (schedules/set-enabled-owned! host p handle id false)
          (is (= :skipped (:schedule.occurrence/state (model/occurrence conn oid))))
          (is (nil? (:schedule.activation/active-occurrence (model/activation conn id)))))))))

(deftest legacy-can-be-inspected-and-paused-but-is-never-upgraded
  (with-activation true :workflow
    (fn [{:keys [conn host row handle p]}]
      (let [target (-> (model/managed-target conn row)
                       (assoc :version 1)
                       (dissoc :room :by :creation-channel :source-origin)
                       (update :execution dissoc :expected-owned-source))
            legacy (model/create! conn {:id (random-uuid) :room (:schedule.activation/room row)
                                        :by (:schedule.activation/by row) :target target
                                        :now (java.util.Date.) :next-fire (java.util.Date.)})
            id (:schedule.activation/id legacy)]
        (is (= :legacy-recreate (:resume-blocked (some #(when (= (str id) (:id %)) %)
                                                       (schedules/list-owned! host p handle)))))
        (schedules/set-enabled-owned! host p handle id false)
        (is (= ::schedules/legacy-recreate (:type (ex-data (failure #(schedules/set-enabled-owned! host p handle id true))))))
        (is (= legacy (assoc (model/activation conn id) :schedule.activation/enabled? true)))
        (is (= target (model/managed-target conn legacy)))))))

(deftest foreign-active-pointer-is-not-resumable-or-cleared
  (with-activation true :workflow
    (fn [{:keys [conn host row handle p]}]
      (let [id (:schedule.activation/id row) oid (random-uuid)]
        (schedules/set-enabled-owned! host p handle id false)
        (d/transact conn [{:schedule.occurrence/id oid :schedule.occurrence/activation (random-uuid)
                           :schedule.occurrence/revision (:schedule.activation/revision row)
                           :schedule.occurrence/state :linked}
                          {:db/id (:db/id row) :schedule.activation/active-occurrence oid}])
        (is (= ::schedules/conflict (:type (ex-data (failure #(schedules/set-enabled-owned! host p handle id true))))))
        (is (= oid (:schedule.activation/active-occurrence (model/activation conn id))))
        (is (false? (:schedule.activation/enabled? (model/activation conn id))))))))

(deftest pointer-change-between-inspection-and-resume-is-refused-transactionally
  (with-activation true :workflow
    (fn [{:keys [conn host row handle p]}]
      (let [id (:schedule.activation/id row) oid (random-uuid) original model/resume-managed!]
        (schedules/set-enabled-owned! host p handle id false)
        (with-redefs [model/resume-managed!
                      (fn [c r]
                        (d/transact c [{:db/id (:db/id r) :schedule.activation/active-occurrence oid}])
                        (original c r))]
          (is (some? (failure #(schedules/set-enabled-owned! host p handle id true)))))
        (is (= oid (:schedule.activation/active-occurrence (model/activation conn id))))
        (is (false? (:schedule.activation/enabled? (model/activation conn id))))))))

(deftest malformed-bounded-legacy-metadata-is-not-echoed-or-paused
  (with-activation true :workflow
    (fn [{:keys [conn host row handle p]}]
      (let [base (-> (model/managed-target conn row) (assoc :version 1)
                     (dissoc :room :by :creation-channel :source-origin)
                     (update :execution dissoc :expected-owned-source))]
        (doseq [target [(assoc-in base [:execution :name] (apply str (repeat 65 "a")))
                        (assoc-in base [:execution :expected-bundle] "not-a-uuid")
                        (assoc-in base [:cadence :extra] "Unbounded response field")
                        (assoc base :version 99)]]
          (let [bad (model/create! conn {:id (random-uuid) :room (:schedule.activation/room row)
                                         :by (:schedule.activation/by row) :target target
                                         :now (java.util.Date.) :next-fire (java.util.Date.)})
                id (:schedule.activation/id bad)]
            (is (some? (failure #(schedules/list-owned! host p handle))))
            (is (some? (failure #(schedules/set-enabled-owned! host p handle id false))))
            (is (= bad (model/activation conn id)))
            (d/transact conn [[:db/retractEntity (:db/id bad)]])))))))

(deftest creator-revocation-during-policy-inspection-is-rechecked
  (with-activation true :benchmark
    (fn [{:keys [conn host w row handle p]}]
      (let [id (:schedule.activation/id row) original schedules/model-policy]
        (schedules/set-enabled-owned! host p handle id false)
        (with-redefs [schedules/model-policy
                      (fn [kind execution]
                        (let [policy (original kind execution)]
                          (d/transact conn [[:db/retract [:party/id (:owner w)] :party/role :admin]])
                          policy))]
          (is (some? (failure #(schedules/set-enabled-owned! host p handle id true)))))
        (is (false? (:schedule.activation/enabled? (model/activation conn id))))))))

(deftest creator-is-checked-again-after-policy-on-the-final-control-snapshot
  ;; Current room ReBAC gives merge only to its single owner, not an admin or
  ;; ordinary member. Do not invent a second authorized caller for this test.
  ;; Spy on the real pure gate to distinguish the retained-creator principal
  ;; object from the original caller while preserving all actual decisions.
  (with-activation true :workflow
    (fn [{:keys [host row handle p]}]
      (let [id (:schedule.activation/id row) events (atom [])
            authorize work/authorize-in! policy schedules/model-policy]
        (schedules/set-enabled-owned! host p handle id false)
        (with-redefs [work/authorize-in! (fn [db who room]
                                           (swap! events conj (if (identical? p who) :caller :creator))
                                           (authorize db who room))
                      schedules/model-policy (fn [kind execution]
                                               (swap! events conj :policy)
                                               (policy kind execution))]
          (is (true? (:schedule.activation/enabled? (schedules/set-enabled-owned! host p handle id true)))))
        (is (= [:caller :creator :policy :caller :creator] @events))))))

(deftest invalid-identities-owners-and-principals-never-mutate
  (with-activation true :workflow
    (fn [{:keys [conn host w row handle p]}]
      (let [id (:schedule.activation/id row)]
        (doseq [bad [nil {} (assoc p :principal/kind :agent) (principal w :team-agent)
                     (principal w :non-member)]]
          (is (some? (failure #(schedules/list-owned! host bad handle))))
          (is (some? (failure #(schedules/set-enabled-owned! host bad handle id false)))))
        (is (some? (failure #(schedules/list-owned! (assoc host ::reads/owner nil) p handle))))
        (is (some? (failure #(schedules/set-enabled-owned! host p handle nil false))))
        (is (some? (failure #(schedules/set-enabled-owned! host p handle id nil))))
        (is (some? (failure #(schedules/set-enabled-owned! host (principal w :member) handle id false))))
        (is (= row (model/activation conn id)))))))

(deftest request-status-is-metadata-only-with-no-synthetic-source-guard
  (with-activation true :workflow
    (fn [{:keys [conn host row handle p]}]
      (let [client-id (random-uuid)
            request (:request (requests/claim! conn {:party (:schedule.activation/by row)
                                                     :room (:schedule.activation/room row)
                                                     :client-id client-id :kind :workflow
                                                     :intent {:legacy true} :limits {}}))
            forbidden (fn [& _] (throw (AssertionError. "Status hydrated a room or source")))]
        (with-redefs [api/room! forbidden reads/resolve-host-read! forbidden rooms/live-room forbidden]
          (let [result (api/owned-status! host p handle (:work.request/id request))]
            (is (= (:work.request/id request) (:id result)))
            (is (= :admitting (:state result)))
            (is (not (contains? result :intent)))
            (is (not (contains? result :expected-owned-source)))))
        (is (some? (failure #(api/owned-status! host p handle nil))))))))

(deftest status-helper-tags-and-diagnostics-never-manufacture-transport-outcomes
  (with-activation true :workflow
    (fn [{:keys [host handle p]}]
      (doseq [tag [:is.simm.ops.owned-workflows/invalid :is.simm.ops.owned-workflows/conflict]]
        (with-redefs [work/status (fn [& _] (throw (ex-info "SECRET internal diagnostic" {:type tag :status 409})))]
          (let [error (failure #(api/owned-status! host p handle (random-uuid)))]
            (is (= 503 (dops/error-status error)))
            (is (= ::api/owned-status-unavailable (:type (ex-data error))))
            (is (not (.contains (str error) "SECRET"))))))
      (try
        (with-redefs [work/status (fn [& _] (throw (ex-info "SECRET" {} (InterruptedException. "SECRET"))))]
          (let [error (failure #(api/owned-status! host p handle (random-uuid)))
                interrupted? (.isInterrupted (Thread/currentThread))]
            (is (= 503 (dops/error-status error)))
            (is interrupted?)))
        (finally (Thread/interrupted))))))
