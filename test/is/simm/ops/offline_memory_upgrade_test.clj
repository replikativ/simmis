(ns is.simm.ops.offline-memory-upgrade-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.authority.world :as world]
            [is.simm.documents.knowledge-upgrade :as foundation]
            [is.simm.documents.knowledge-upgrade-test :as fixture]
            [is.simm.documents.markdown :as md]
            [is.simm.documents.space :as space]
            [is.simm.documents.profile :as profile]
            [is.simm.documents.view :as view]
            [is.simm.model.memory-upgrades :as receipts]
            [is.simm.model.personal-rooms :as personal]
            [is.simm.ops.offline-memory-upgrade :as upgrade]))

(defn with-upgrade [f]
  (let [control (world/fresh-system-conn) cfg (:config @control)]
    (try
      (d/transact control receipts/schema)
      (fixture/with-repository
       (fn [conn]
         (let [actor (random-uuid) room (random-uuid) system (random-uuid) scope "/explicit/fixture"]
           (d/transact control [{:party/id actor :party/type :human}
                                {:system/id system :system/type :repo :system/scope scope}
                                {:room/id room :room/name "Existing" :room/slug "existing"
                                 :room/type :personal-ai :room/owner [:party/id actor]
                                 :room/parties [[:party/id actor]] :room/content-db-scope (random-uuid)
                                 :room/default-repository [:system/id system]}
                                {:grant/id (random-uuid) :grant/room [:room/id room]
                                 :grant/system [:system/id system] :grant/permission :owner}])
           (repo/write! conn "README.md" (md/source-bytes "# Existing authored notes\n"))
           (repo/stage-all! conn) (repo/commit! conn {:message "Keep this history"})
           (f {:control control :repo conn :actor actor :room room :system system
               :request {:id (random-uuid) :actor actor :room room :at (java.util.Date.)
                         :target {:system system :scope scope
                                  :repository (:geschichte.repo/id (query/repository @conn))
                                  :store (get-in @conn [:config :store :id])}}})) ))
      (finally (d/release control) (d/delete-database cfg)))))

(deftest published-upgrade-preserves-personal-room-bootstrap-and-git-state
  (with-upgrade
   (fn [{:keys [control repo actor room system request]}]
     (let [source (fixture/source-state repo)
           inspection (upgrade/inspect! control repo request)]
       (is (= :published (:status (upgrade/upgrade! control repo inspection))))
       (is (= source (fixture/source-state repo)))
       (is (= space/profile (:system/repository-profile (d/entity @control [:system/id system]))))
       (is (receipts/published-for? @control room system))
       (is (= room (@#'personal/existing-id @control actor)))
       (is (nil? (:room/provisioning-state (d/entity @control [:room/id room]))))
       (is (nil? (:room/created-by (d/entity @control [:room/id room]))))
       (is (= :published (:status (upgrade/recover! control repo (:id request)))))))))

(deftest lost-repository-ack-is-recovered-by-receipt-without-reinstalling
  (with-upgrade
   (fn [{:keys [control repo request]}]
     (let [inspection (upgrade/inspect! control repo request) install foundation/install!]
       (with-redefs [foundation/install! (fn [conn plan] (install conn plan) (throw (ex-info "Lost ACK" {})))]
         (is (thrown? clojure.lang.ExceptionInfo (upgrade/upgrade! control repo inspection))))
       (is (= :preparing (:memory.upgrade/state (receipts/in @control (:id request)))))
       (is (space/provisioned? @repo))
       (with-redefs [foundation/install! (fn [& _] (throw (AssertionError. "Do not replay installed repository")))]
         (is (= :published (:status (upgrade/recover! control repo (:id request))))))))))

(deftest stale-inspection-and-foreign-actor-cannot-install-anything
  (with-upgrade
   (fn [{:keys [control repo request]}]
     (is (thrown? clojure.lang.ExceptionInfo
                  (upgrade/inspect! control repo (assoc request :actor (random-uuid)))))
     (let [inspection (upgrade/inspect! control repo request)]
       (repo/write! repo "new.md" (md/source-bytes "# Later user work\n"))
       (is (thrown? clojure.lang.ExceptionInfo (upgrade/upgrade! control repo inspection)))
       (is (nil? (receipts/in @control (:id request))))
       (is (not (space/provisioned? @repo)))))))

(deftest lost-control-publication-ack-recovers-without-source-replay
  (with-upgrade
   (fn [{:keys [control repo request]}]
     (let [inspection (upgrade/inspect! control repo request) transact d/transact]
       (with-redefs [d/transact (fn [conn tx & opts]
                                 (let [result (apply transact conn tx opts)]
                                   (when (and (identical? conn control)
                                              (= :repository-ready (get-in tx [0 3]))
                                              (= :published (get-in tx [0 4])))
                                     (throw (ex-info "Lost publication ACK" {})))
                                   result))]
         (is (thrown? clojure.lang.ExceptionInfo (upgrade/upgrade! control repo inspection))))
       (is (= :published (:memory.upgrade/state (receipts/in @control (:id request)))))
       (let [before (fixture/source-state repo)]
         (with-redefs [foundation/install! (fn [& _] (throw (AssertionError. "No replay after publication")))]
           (is (= :published (:status (upgrade/recover! control repo (:id request))))))
         (is (= before (fixture/source-state repo))))))))

(deftest repository-owner-grant-is-required-in-addition-to-room-ownership
  (with-upgrade
   (fn [{:keys [control repo room request]}]
     (let [grant (d/q '[:find ?g . :in $ ?room :where [?r :room/id ?room] [?g :grant/room ?r]] @control room)]
       (d/transact control [[:db/retractEntity grant]])
       (is (thrown? clojure.lang.ExceptionInfo (upgrade/inspect! control repo request)))
       (is (not (space/provisioned? @repo)))))))

(deftest open-or-unknown-proposals-hold-the-upgrade
  (doseq [status [:open :unexpected nil]]
    (with-upgrade
     (fn [{:keys [control repo room request]}]
       (d/transact control [(cond-> {:proposal/id (random-uuid) :proposal/room room}
                              status (assoc :proposal/status status))])
       (is (thrown? clojure.lang.ExceptionInfo (upgrade/inspect! control repo request)))
       (is (not (space/provisioned? @repo)))))))

(deftest another-rooms-default-cannot-be-upgraded-through-one-rooms-receipt
  (with-upgrade
   (fn [{:keys [control repo actor system request]}]
     (d/transact control [{:room/id (random-uuid) :room/type :personal-ai
                           :room/owner [:party/id actor]
                           :room/default-repository [:system/id system]}])
     (is (thrown? clojure.lang.ExceptionInfo (upgrade/inspect! control repo request)))
     (is (not (space/provisioned? @repo))))))

(deftest registration-attests-both-stores-and-allows-authored-history-to-evolve
  (with-upgrade
   (fn [{:keys [control repo room system request]}]
     (is (false? (profile/legacy-room-ready? @control room @repo)))
     (upgrade/upgrade! control repo (upgrade/inspect! control repo request))
     (is (true? (profile/legacy-room-ready? @control room @repo)))
     (let [db @control repository @repo root (:db/id (query/repository repository))
           id (:id request)]
       (doseq [tx [[[:db/retract [:room/id room] :room/knowledge-upgrade id]]
                   [[:db/retractEntity [:memory.upgrade/id id]]]
                   [[:db/add [:memory.upgrade/id id] :memory.upgrade/state :preparing]]
                   [[:db/add [:system/id system] :system/scope "/foreign"]]
                   [[:db/add [:memory.upgrade/id id] :memory.upgrade/store (random-uuid)]]]]
         (is (false? (profile/legacy-room-ready? (:db-after (d/with db tx)) room repository))))
       (doseq [attribute [:document.repository/upgrade :document.repository/upgrade-basis]]
         (is (false? (profile/legacy-room-ready?
                      db room (:db-after (d/with repository [[:db/add root attribute (random-uuid)]]))))))
       (is (false? (profile/legacy-room-ready?
                    db room (assoc-in repository [:config :store :id] (random-uuid)))))
       (repo/write! repo "README.md" (md/source-bytes "---\nsimmis-id: broken-authored-id\n---\n# Editable raw source\n"))
       (repo/stage-all! repo)
       (repo/commit! repo {:message "Legitimate later file edit, requiring raw repair"})
       (is (not= :ready (:status (view/prepare @repo profile/selection-options))))
       (is (true? (profile/legacy-room-ready? @control room @repo))
           "Registration validates the foundation/receipt, never freezes history or requires parseable documents")))))

(deftest registration-policy-cannot-initialize-a-repository
  (let [hooks (get-in (profile/registration-options) [:repository-profiles space/profile])]
    (is (identical? profile/legacy-room-ready? (:legacy-room-ready? hooks)))
    (is (thrown? clojure.lang.ExceptionInfo ((:initialize! hooks) nil nil)))))
