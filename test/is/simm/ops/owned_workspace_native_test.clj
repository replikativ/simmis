(ns is.simm.ops.owned-workspace-native-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.substrate.geschichte :as native]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.owned-workspace-test :as fixture]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.protocols :as protocols]))

(defrecord ConnectionAlias [conn]
  protocols/SystemIdentity
  (system-id [_] "owned-workspace-test-datahike-alias")
  (system-type [_] :datahike)
  (capabilities [_] {}))

(deftest owned-reads-reject-a-differently-named-datahike-alias
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room owned]}]
     (let [binding (owned/resolve! @conn (:owner w) :read (:room w) room)
           registered (ygg/registered-systems)]
       ;; Registry enumeration is the ONLY seam here. The fixed native explicit
       ;; resolver performs the actual same-connection alias check.
       (with-redefs [ygg/registered-systems
                     (constantly (assoc registered "owned-workspace-test-datahike-alias"
                                        (->ConnectionAlias owned)))]
         (is (thrown? clojure.lang.ExceptionInfo
                      (owned/read-file @conn (:owner w) (:room w) room "wiki/file.md" nil)))
         (is (thrown? clojure.lang.ExceptionInfo
                      (owned/validate! @conn (:owner w) :read (:room w) room binding))))
       (is (= "# Owned\n" (:text (owned/read-file @conn (:owner w) (:room w) room
                                                  "wiki/file.md" (:target-stamp binding)))))))))

(deftest descriptor-and-file-snapshot-must-have-coherent-configuration
  (@#'fixture/with-fixture
   false
   (fn [{:keys [conn w room]}]
     (let [binding (owned/resolve! @conn (:owner w) :read (:room w) room)
           resolve-native native/workspace-for-system!]
       (is (map? (:workspace-config binding)))
       (is (not (contains? (:workspace-config binding) :conn)))
       ;; Inject descriptors captured from another config/ref. A projection
       ;; must not combine either with the connection's actual file DB.
       (doseq [change [(fn [descriptor]
                         (assoc-in descriptor [:repository :config :fixture-config-revision] 2))
                       (fn [descriptor]
                         (assoc-in descriptor [::native/physical-target :ref] "other-ref"))]]
         (with-redefs [native/workspace-for-system! #(change (resolve-native %))]
           (is (thrown? clojure.lang.ExceptionInfo
                        (owned/resolve! @conn (:owner w) :read (:room w) room)))
           (is (thrown? clojure.lang.ExceptionInfo
                        (owned/validate! @conn (:owner w) :read (:room w) room binding)))
           (is (thrown? clojure.lang.ExceptionInfo
                        (owned/read-file @conn (:owner w) (:room w) room
                                         "wiki/file.md" (:target-stamp binding))))))))))

(deftest canonical-configuration-affects-the-stamp-without-changing-the-target
  (let [room (random-uuid) mapping (random-uuid)
        target {:repository (random-uuid)}
        config {:config {:store {:backend :memory :id (random-uuid)}} :workspace-branch :db}
        stamp @#'owned/target-stamp]
    (is (= (stamp room mapping target config) (stamp room mapping target config)))
    (is (not= (stamp room mapping target config)
              (stamp room mapping target (assoc-in config [:config :fixture-config-revision] 2))))))
