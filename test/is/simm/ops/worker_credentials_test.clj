(ns is.simm.ops.worker-credentials-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.authority.world :as world]
            [is.simm.model.workers :as workers]
            [is.simm.ops.worker-credentials :as credentials])
  (:import [java.util Date]))

(defn fixture [conn w]
  (let [id (random-uuid) secret (workers/new-secret)
        worker (workers/enroll! conn {:id id :by (:owner w) :room (:room w) :secret secret
                                      :config {:adapter :fixture :version "1"}})
        assignment (workers/enqueue! conn {:id (random-uuid) :worker id :generation 1 :room (:room w)
                                           :request (random-uuid) :job (random-uuid) :input {:task "fixture"}})
        proof (workers/new-secret)
        run (:simmis.worker.assignment/id assignment)
        claim (workers/claim! conn run id 1 (random-uuid) proof (Date. 10000) 10000)
        lease (:simmis.worker.assignment/lease claim)]
    {:worker worker :secret secret :proof proof :run run :lease lease
     :enrollment-token (credentials/enrollment-token id secret)
     :lease-token (credentials/lease-token run lease proof)}))

(deftest issuance-is-stable-and-domain-bound-without-storing-the-credential
  (let [party (random-uuid) room (random-uuid) worker (random-uuid)
        secret (credentials/derive-secret "host-auth-secret" party room worker)]
    (is (= 43 (count secret)))
    (is (= secret (credentials/derive-secret "host-auth-secret" party room worker)))
    (doseq [other [(credentials/derive-secret "different-host-key" party room worker)
                   (credentials/derive-secret "host-auth-secret" (random-uuid) room worker)
                   (credentials/derive-secret "host-auth-secret" party (random-uuid) worker)
                   (credentials/derive-secret "host-auth-secret" party room (random-uuid))]]
      (is (not= secret other)))))

(deftest enrollment-and-lease-bearers-have-separate-attenuated-authority
  (world/with-world [conn w]
    (let [{:keys [enrollment-token lease-token secret proof run]} (fixture conn w)
          enrolled (credentials/authenticate conn {:bearer enrollment-token} (Date. 11000))
          proposed (credentials/authenticate conn {:bearer enrollment-token :headers {"simmis-lease-proof" proof}} (Date. 11000))
          leased (credentials/authenticate conn {:bearer lease-token} (Date. 11000))]
      (is (= :worker (:principal/kind enrolled) (:principal/kind leased)))
      (is (= #{"simmis_worker-pending"} (:principal/tools enrolled)))
      (is (= credentials/enrollment-tools (:principal/tools proposed)))
      (is (= credentials/lease-tools (:principal/tools leased)))
      (is (not= (:principal/id enrolled) (:principal/id leased)))
      (is (not (str/includes? (pr-str [enrolled proposed leased]) secret)))
      (is (not (str/includes? (pr-str [enrolled proposed leased]) proof)))
      (is (:allow? (credentials/authorize conn enrolled {:op :simmis/worker-pending :args {}} (Date. 11000))))
      (is (false? (:allow? (credentials/authorize conn enrolled {:op :simmis/worker-claim :args {}} (Date. 11000)))))
      (is (:allow? (credentials/authorize conn proposed {:op :simmis/worker-claim :args {}} (Date. 11000))))
      (is (:allow? (credentials/authorize conn leased {:op :simmis/worker-input :args {:run (str run)}} (Date. 11000))))
      (doseq [op [:room/read :room/write :room/list :agent/create :simmis/task :simmis/worker-pending]]
        (is (false? (:allow? (credentials/authorize conn leased {:op op :args {:run (str run)}} (Date. 11000))))))
      (is (false? (:allow? (credentials/authorize conn leased {:op :simmis/worker-input :args {:run (str (random-uuid))}} (Date. 11000))))))))

(deftest expiry-and-revocation-preserve-only-existing-shutdown-proof
  (world/with-world [conn w]
    (let [{:keys [worker enrollment-token lease-token run]} (fixture conn w)
          original (credentials/authenticate conn {:bearer lease-token} (Date. 11000))]
      (doseq [now [(Date. 20000) (Date. 21000)]]
        (let [expired (credentials/authenticate conn {:bearer lease-token} now)]
          (is (= credentials/shutdown-tools (:principal/tools expired)))
          (is (false? (:allow? (credentials/authorize conn expired {:op :simmis/worker-input :args {:run (str run)}} (Date. 11000)))))
          (is (false? (:allow? (credentials/authorize conn original {:op :simmis/worker-input :args {:run (str run)}} now))))
          (is (:allow? (credentials/authorize conn expired {:op :simmis/worker-stop :args {:run (str run)}} now)))))
      (workers/revoke! conn (:simmis.worker/id worker))
      (is (nil? (credentials/authenticate conn {:bearer enrollment-token} (Date. 11000))))
      (is (= credentials/shutdown-tools
             (:principal/tools (credentials/authenticate conn {:bearer lease-token} (Date. 11000)))))
      (is (false? (:allow? (credentials/authorize conn original {:op :simmis/worker-complete :args {:run (str run)}} (Date. 11000)))))
      (is (:allow? (credentials/authorize conn original {:op :simmis/worker-stop :args {:run (str run)}} (Date. 11000)))))))

(deftest already-open-session-rechecks-enroller-room-membership-and-ownership
  (world/with-world [conn w]
    (let [{:keys [lease-token enrollment-token run]} (fixture conn w)
          original (credentials/authenticate conn {:bearer lease-token} (Date. 11000))]
      (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:owner w)]]
                        [:db/add [:room/id (:room w)] :room/owner [:party/id (:non-member w)]]])
      (is (nil? (credentials/authenticate conn {:bearer enrollment-token} (Date. 11000))))
      (is (= credentials/shutdown-tools (:principal/tools (credentials/authenticate conn {:bearer lease-token} (Date. 11000)))))
      (doseq [op [:simmis/worker-input :simmis/worker-complete]]
        (is (false? (:allow? (credentials/authorize conn original {:op op :args {:run (str run)}} (Date. 11000))))))
      (is (:allow? (credentials/authorize conn original {:op :simmis/worker-heartbeat :args {:run (str run)}} (Date. 11000)))))))

(deftest malformed-proofs-cross-run-handles-and-human-principals-are-refused
  (world/with-world [conn w]
    (let [{:keys [worker run lease secret]} (fixture conn w)]
      (doseq [bearer [nil "" "not-a-worker" (apply str (repeat 300 "x"))
                      (credentials/enrollment-token (:simmis.worker/id worker) (workers/new-secret))
                      (credentials/enrollment-token (random-uuid) secret)
                      (credentials/lease-token run lease (workers/new-secret))
                      (credentials/lease-token (random-uuid) lease secret)]]
        (is (nil? (credentials/authenticate conn {:bearer bearer} (Date. 11000)))))
      (is (nil? (credentials/authenticate conn {:bearer (credentials/enrollment-token (:simmis.worker/id worker) secret)
                                                :headers {"simmis-lease-proof" "invalid"}} (Date. 11000))))
      (is (false? (:allow? (credentials/authorize conn {:principal/kind :human :principal/id (str "party/" (:owner w))}
                                                  {:op :simmis/worker-pending :args {}} (Date. 11000))))))))
