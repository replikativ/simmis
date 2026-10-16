(ns is.simm.model.sidebar-kbs-test
  "The wikis the navigation lists (`kbs/sidebar-kbs`): owned and directly
   shared ones, plus those reached only through a grant to a room the viewer is
   a member of — read-only when the grant is `:read` — and nothing the viewer
   cannot read. Over the authority matrix's world (`authority.world`): the
   owner's Handbook is granted `:read-write` to the team room, which the member
   is in; Archive is the owner's and granted nowhere; the stranger's KB is in
   no room of theirs."
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [is.simm.authority.world :as world]
            [is.simm.model.knowledge-bases :as kbs]))

(defn- by-name [kbs] (into {} (map (juxt :kb/name identity)) kbs))

(defn- grant! [conn w room sys-key permission]
  (d/transact conn [{:grant/id (random-uuid) :grant/room [:room/id (get w room)]
                     :grant/system [:system/id (get w sys-key)]
                     :grant/permission permission :grant/created (java.util.Date.)}]))

(deftest a-member-sees-the-wikis-its-rooms-are-granted
  (world/with-world [conn w]
    (testing "before: get-party-kbs lists nothing for the member — the gap"
      (is (empty? (kbs/get-party-kbs (:member w)))))
    (let [listed (by-name (kbs/sidebar-kbs (:member w)))]
      (testing "the team room's :read-write grant lists Handbook, writable"
        (is (= #{"Handbook"} (set (keys listed))))
        (is (false? (:kb/read-only? (listed "Handbook"))))
        (is (= [{:room/id (:room w) :room/name "Team" :kb/permission :read-write}]
               (:kb/via-rooms (listed "Handbook")))))
      (testing "no other user's ids travel with it"
        (is (not (contains? (listed "Handbook") :kb/shared-with)))))
    (testing "a :read grant lists the wiki read-only"
      (grant! conn w :room :kb-private-sys :read)
      (let [listed (by-name (kbs/sidebar-kbs (:member w)))]
        (is (= #{"Handbook" "Archive"} (set (keys listed))))
        (is (true? (:kb/read-only? (listed "Archive"))))
        (is (= :read (-> (listed "Archive") :kb/via-rooms first :kb/permission)))))
    (testing "two rooms granting one wiki list it once, with both rooms"
      (grant! conn w :room-nogrant :kb-private-sys :read-write)
      (let [listed (kbs/sidebar-kbs (:member w))
            archive (first (filter #(= "Archive" (:kb/name %)) listed))]
        (is (= 1 (count (filter #(= "Archive" (:kb/name %)) listed))))
        (is (= #{"Team" "Scratch"} (set (map :room/name (:kb/via-rooms archive)))))
        (is (false? (:kb/read-only? archive)) "the :read-write grant writes")))))

(deftest nothing-from-rooms-the-viewer-is-not-in
  (world/with-world [conn w]
    (grant! conn w :other-room :kb-private-sys :read-write)
    (testing "the stranger's room's grant reaches the stranger, not the member"
      (is (not (contains? (by-name (kbs/sidebar-kbs (:member w))) "Archive"))))
    (testing "the stranger sees their own KB, and Archive through their room"
      (let [listed (by-name (kbs/sidebar-kbs (:non-member w)))]
        (is (= #{"Stranger Private" "Archive"} (set (keys listed))))
        (is (not (contains? listed "Handbook")))))
    (testing "the admin, in no room, is listed nothing it does not own"
      (is (empty? (kbs/sidebar-kbs (:admin w)))))
    (testing "the owner's own wikis are listed writable, without via-rooms"
      (let [listed (by-name (kbs/sidebar-kbs (:owner w)))]
        (is (= #{"Handbook" "Archive"} (set (keys listed))))
        (is (every? (comp false? :kb/read-only?) (vals listed)))
        (is (every? (comp nil? :kb/via-rooms) (vals listed)))))
    (testing "no party, nothing"
      (is (nil? (kbs/sidebar-kbs nil))))))
