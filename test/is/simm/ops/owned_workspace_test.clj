(ns is.simm.ops.owned-workspace-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as catalog]
            [dvergr.room.store :as store]
            [dvergr.substrate.geschichte :as workspace]
            [dvergr.system.db :as registry-db]
            [dvergr.system.rooms :as rooms]
            [geschichte.repo :as repo]
            [is.simm.authority.world :as world]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.knowledge-admission :as admission]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.work-test :as work-test]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.protocols :as protocols]))

(defn- seed! [conn text]
  (repo/write! conn "wiki/file.md" (markdown/source-bytes text))
  (repo/stage-all! conn)
  (repo/commit! conn {:message "Fixture source"}))

(defn- with-fixture [owned-first? f]
  (world/with-world [conn w]
    (@#'work-test/with-room
     (fn [room]
       (let [opened (atom [])
             open! #(let [c (catalog/open-memory-workspace! room)] (swap! opened conj c) c)]
         (try
           (let [first-conn (open!) second-conn (open!)
                 own (if owned-first? first-conn second-conn)
                 attached (if owned-first? second-conn first-conn)
                 systems (filter #(= :geschichte (protocols/system-type %)) (vals (ygg/registered-systems)))
                 descriptor (fn [c id]
                              (let [system (some #(when (identical? c (workspace/gy-connection %)) %) systems)]
                                {:system system :conn c :registry-system id
                                 :runtime-system (str (protocols/system-id system))}))
                 attached-id (random-uuid)
                 own-desc (descriptor own (:repo-sys w))
                 attached-desc (descriptor attached attached-id)
                 descriptors {(:repo-sys w) own-desc attached-id attached-desc}]
             (d/transact conn [{:room/id (:room w) :room/slug (store/room-id->slug (:id room))
                                :room/default-repository [:system/id (:repo-sys w)]}
                               {:system/id (:repo-sys w) :system/scope "/fixture/owned"}
                               {:system/id attached-id :system/type :repo :system/scope "/fixture/attached"}
                               {:grant/id (random-uuid) :grant/room [:room/id (:room w)]
                                :grant/system [:system/id attached-id] :grant/permission :owner}])
             (seed! own "# Owned\n")
             (seed! attached "# Attachment\n")
             (workspace/select-workspace! (:runtime-system attached-desc))
             ;; File-backed provisioning and registry UUID->ctx mapping are
             ;; isolated here. Authorization, DB ownership/default lookup,
             ;; canonical Tier1 Room lookup, real systems/connections and all
             ;; immutable file/blob reads remain real. Not a provisioned-home
             ;; or upstream filesystem-alias integration claim.
             (with-redefs [rooms/room-ctx-for #(when (= (:room w) %) (:ctx room))
                           rooms/default-workspace
                           (fn resolve-default
                             ([id] (resolve-default @conn id))
                             ([db id]
                              (is (= (:room w) id))
                              (or (get descriptors (:system/id (registry-db/default-repository db id)))
                                  (throw (ex-info "No owned default" {})))))]
               (f {:conn conn :w w :room room :owned own :attached attached
                   :own-descriptor own-desc :attached-descriptor attached-desc})))
           (finally (doseq [c @opened] (d/release c)))))))))

(deftest explicit-read-ignores-registration-order-and-selected-attachment
  (doseq [owned-first? [true false]]
    (with-fixture owned-first?
      (fn [{:keys [conn w room owned attached-descriptor]}]
        (let [before (workspace/selected-system-id)]
          (is (= before (:runtime-system attached-descriptor)))
          (with-redefs [workspace/current-workspace (fn [] (throw (AssertionError. "Ambient workspace read")))
                        workspace/select-workspace! (fn [& _] (throw (AssertionError. "Parent selection mutation")))]
            (let [binding (owned/resolve! @conn (:owner w) :read (:room w) room)
                  opened (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" nil)]
              (is (identical? owned (:conn binding)))
              (is (uuid? (:target-stamp binding)))
              (is (= (:target-stamp binding) (:target-stamp opened)))
              (is (= "# Owned\n" (:text opened)))
              (is (= :source-only (:execution-policy opened)))
              (is (= (:repository (:target binding)) (:repository-id opened)))
              (is (= (:entry opened) (:entry (files/read-file-in (:db binding) "wiki/file.md")))))
            (is (= before (workspace/selected-system-id)))))))))

(deftest snapshot-is-immutable-and-head-or-unrelated-control-edits-do-not-retarget
  (with-fixture true
    (fn [{:keys [conn w room owned]}]
      (let [binding (owned/resolve! @conn (:owner w) :read (:room w) room)]
        (seed! owned "# Later\n")
        (d/transact conn [{:room/id (:room w) :room/name "Unrelated presentation edit"}])
        (is (= "# Owned\n" (:text (files/read-file-in (:db binding) "wiki/file.md"))))
        (is (= "# Later\n" (:text (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" (:target-stamp binding)))))
        (is (= (:target-stamp binding) (:target-stamp (owned/validate! @conn (:owner w) :read (:room w) room binding))))))))

(deftest identical-bytes-default-replacement-and-aba-invalidate-the-opened-stamp
  (with-fixture true
    (fn [{:keys [conn w room attached attached-descriptor]}]
      (seed! attached "# Owned\n")
      (let [binding (owned/resolve! @conn (:owner w) :read (:room w) room)]
        (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
        (is (= "# Owned\n" (:text (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" nil))))
        (is (thrown? clojure.lang.ExceptionInfo (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" (:target-stamp binding))))
        (is (thrown? clojure.lang.ExceptionInfo (owned/validate! @conn (:owner w) :read (:room w) room binding)))
        (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:repo-sys w)]}])
        (is (not= (:target-stamp binding) (:target-stamp (owned/resolve! @conn (:owner w) :read (:room w) room))))))))

(deftest current-authority-and-canonical-room-are-required-without-merge-escalation
  (with-fixture true
    (fn [{:keys [conn w room]}]
      (is (= "# Owned\n" (:text (owned/read-file @conn (:member w) (:room w) room "wiki/file.md" nil))))
      (is (= :write (:action (owned/resolve! @conn (:member w) :write (:room w) room))))
      (doseq [[principal action] [[(:member w) :merge] [(:non-member w) :read] [nil :read] [(:owner w) :unknown]]]
        (with-redefs [admission/default-binding (fn [& _] (throw (AssertionError. "Denied caller reached target")))]
          (is (thrown? clojure.lang.ExceptionInfo (owned/resolve! @conn principal action (:room w) room)))))
      (is (thrown? clojure.lang.ExceptionInfo (owned/resolve! @conn (:owner w) :read (:room w) (assoc room :fixture :not-canonical))))
      (let [binding (owned/resolve! @conn (:member w) :read (:room w) room)]
        (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
        (is (thrown? clojure.lang.ExceptionInfo (owned/validate! @conn (:member w) :read (:room w) room binding)))))))

(deftest malformed-or-substituted-host-descriptors-do-not-fall-back
  (with-fixture true
    (fn [{:keys [conn w room own-descriptor attached]}]
      (doseq [descriptor [(assoc own-descriptor :conn attached)
                          (assoc own-descriptor :runtime-system "missing-runtime")
                          (dissoc own-descriptor :system)]]
        (with-redefs [admission/default-binding (fn [& _] descriptor)]
          (is (thrown? clojure.lang.ExceptionInfo (owned/resolve! @conn (:owner w) :read (:room w) room)))))
      (let [binding (owned/resolve! @conn (:owner w) :read (:room w) room)]
        (is (thrown? clojure.lang.ExceptionInfo
                     (owned/validate! @conn (:owner w) :read (:room w) room (assoc binding :conn attached))))))))
