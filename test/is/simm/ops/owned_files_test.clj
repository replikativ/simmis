(ns is.simm.ops.owned-files-test
  "Inactive host editor vertical, with real native writes in isolated Rooms.
   File-backed default provisioning is the existing memory fixture seam."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.rooms.files :as native-files]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [is.simm.documents.files :as files]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.owned-files :as actions]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.owned-workspace-test :as fixture]))

(defn- request [opened text]
  {:path (:path opened) :expected (:entry opened) :text text
   :expected-target-stamp (:target-stamp opened)})

(deftest read-edit-and-create-use-only-the-owned-repository-in-both-orders
  (doseq [owned-first? [true false]]
    (@#'fixture/with-fixture
     owned-first?
     (fn [{:keys [conn w room owned attached]}]
       (let [selected (workspace/selected-system-id)
             other @attached
             opened (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" nil)]
         (is (= "# Owned\n" (:text opened)))
         (with-redefs [workspace/current-workspace
                       (fn [] (throw (AssertionError. "Explicit edit used ambient workspace")))
                       workspace/select-workspace!
                       (fn [& _] (throw (AssertionError. "Explicit edit changed selection")))]
           (let [saved (actions/save! conn (:owner w) (:room w) room (request opened "# Edited\n"))
                 current (files/read-file-in @owned "wiki/file.md")
                 created (actions/save! conn (:owner w) (:room w) room
                                        {:path "wiki/new.md" :expected nil :text "New knowledge\n"
                                         :expected-target-stamp (:target-stamp opened)})]
             (is (true? (:saved? saved)))
             (is (= (:entry saved) (:entry current)))
             (is (= "# Edited\n" (:text current)))
             (is (= (:target-stamp opened) (:target-stamp saved) (:target-stamp created)))
             (is (true? (:saved? created)))
             (is (= "New knowledge\n" (:text (files/read-file-in @owned "wiki/new.md"))))))
         (is (= other @attached))
         (is (= (query/refs other) (query/refs @attached)))
         (is (= selected (workspace/selected-system-id))))))))

(deftest replaced-default-refuses-before-the-native-writer-or-payload-processing
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room attached-descriptor]}]
     (let [opened (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" nil)]
       (d/transact conn [{:room/id (:room w)
                          :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
       (with-redefs [files/save-file-to! (fn [& _] (throw (AssertionError. "Stale target reached payload/writer")))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (actions/save! conn (:owner w) (:room w) room (request opened "stale")))))))))

(deftest write-authority-is-checked-again-at-the-native-effect-gate
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn w room owned attached]}]
     (let [opened (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" nil)
           before @owned other @attached write-to! native-files/write-to!]
       (with-redefs [native-files/write-to!
                     (fn [& args]
                       ;; After host resolution, before the native boundary's
                       ;; fixed current authorization gate, revoke both owner
                       ;; and membership rather than merely merge permission.
                       (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner]
                                         [:db/retract [:room/id (:room w)] :room/parties
                                          [:party/id (:owner w)]]])
                       (apply write-to! args))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (actions/save! conn (:owner w) (:room w) room (request opened "denied")))))
       (is (= before @owned))
       (is (= other @attached))))))

(deftest missing-target-stamp-never-reaches-the-file-helper
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room]}]
     (with-redefs [files/save-file-to! (fn [& _] (throw (AssertionError. "Missing stamp reached save")))]
       (is (thrown? clojure.lang.ExceptionInfo
                    (actions/save! conn (:owner w) (:room w) room
                                   {:path "wiki/new.md" :expected nil :text "unstamped"})))))))

(deftest ordinary-room-write-does-not-require-proposal-merge-authority
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room owned]}]
     (let [member (:member w)
           opened (owned/read-file @conn member (:room w) room "wiki/file.md" nil)]
       (is (true? (access/can? @conn member :write {:room (:room w)})))
       (is (not (access/can? @conn member :merge {:room (:room w)})))
       (is (true? (:saved? (actions/save! conn member (:room w) room (request opened "Member edit\n")))))
       (is (= "Member edit\n" (:text (files/read-file-in @owned "wiki/file.md"))))))))

(deftest replacement-control-owner-is-refused-at-the-native-effect-gate
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn w room owned attached]}]
     (let [opened (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" nil)
           before @owned other @attached writer native-files/write-to!
           replacement (atom @conn)]
       (with-redefs [native-files/write-to!
                     (fn [& args]
                       ;; Even equal control snapshots are not the same host
                       ;; owner. The original registry cannot remain a lease.
                       (with-redefs [sdb/get-conn (constantly replacement)]
                         (apply writer args)))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (actions/save! conn (:owner w) (:room w) room (request opened "old control")))))
       (is (= before @owned))
       (is (= other @attached))))))

(deftest initial-markdown-read-bundles-an-optional-property-projection
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room]}]
     (let [opened (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" nil)
           reply (actions/read! conn (:owner w) (:room w) room
                                {:path "wiki/file.md" :expected-target-stamp (:target-stamp opened)})]
       (is (= (:text opened) (:text reply)))
       (is (= (:target-stamp opened) (:target-stamp reply)))
       (is (= {:kind :page} (get-in reply [:page-properties :target])))
       (is (= :unavailable (get-in reply [:page-properties :status])))))))
