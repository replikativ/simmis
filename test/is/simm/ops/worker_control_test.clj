(ns is.simm.ops.worker-control-test
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [dvergr.model.registry :as registry]
            [dvergr.ops :as ops]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.worker-benchmarks :as benchmarks]
            [is.simm.ops.worker-control :as control]
            [is.simm.ops.worker-service :as service]
            [is.simm.ops.worker-work :as work]
            [is.simm.runtimes.auth-config :as auth]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.worker-maintenance :as maintenance]))

(def room-id #uuid "88888888-8888-4888-8888-888888888888")
(def worker-id #uuid "99999999-9999-4999-8999-999999999999")
(def request-id #uuid "77777777-7777-4777-8777-777777777777")
(def config {:adapter "codex" :version "operator-pinned-runtime" :model "fixture-codex" :policy "local-trusted-v1"})
(def principal {:principal/id "party/33333333-3333-4333-8333-333333333333" :principal/kind :human})
(def host {:execution-ctx context/server-context})
(def enroll {:room "room" :worker (str worker-id) :config config})
(def revoke (dissoc enroll :config))
(def allowance {:room "room" :request-id (str request-id) :max-worker-admissions 2})
(def task {:room "room" :request-id (str request-id) :worker (str worker-id) :task "Do bounded work"
           :paths ["docs/input.md"] :limits {:timeout-ms 120000 :settlement "review"}})
(def workflow (-> (dissoc task :task :paths)
                  (assoc :name "echo" :expected-bundle (str request-id)
                         :params {:subject "literal"} :inputs {(keyword "/docs/input.md") "text"})))
(def benchmark (-> (dissoc workflow :params :inputs)
                   (assoc :repetitions 2 :cases ["one" "two"])
                   (assoc-in [:limits :settlement] "discard")))

(defn call [op args] ((get-in control/specifications [op :impl]) host args))

(deftest benchmark-control-is-bounded-and-uses-the-authenticated-room
  (let [seen (atom [])]
    (with-redefs [benchmarks/submit! (fn [& args] (swap! seen conj args) {:job request-id})]
      (is (= {:job request-id} (call :simmis/local-worker-benchmark benchmark)))
      (is (= [::conn context/server-context principal room-id ::room]
             (vec (take 5 (first @seen)))))
      (is (= (-> (dissoc benchmark :room)
                 (assoc :request-id request-id :worker worker-id :expected-bundle request-id))
             (last (first @seen))))
      (doseq [bad [(assoc benchmark :repetitions 3)
                   (assoc benchmark :repetitions 0)
                   (assoc benchmark :cases ["1" "2" "3" "4" "5"])
                   (assoc-in benchmark [:limits :settlement] "review")
                   (assoc benchmark :model "forbidden")
                   (assoc benchmark :params {"x" "override"})
                   (assoc benchmark :worker "not-a-uuid")]]
        (is (thrown? clojure.lang.ExceptionInfo (call :simmis/local-worker-benchmark bad))))
      (is (= 1 (count @seen))))))

(deftest workflow-control-normalizes-only-literal-map-keys
  (let [seen (atom nil)]
    (with-redefs [work/submit-workflow! (fn [& args] (reset! seen (last args)) {:job request-id})]
      (is (= {:job request-id} (call :simmis/local-worker-workflow workflow)))
      (is (= {"subject" "literal"} (:params @seen)))
      (is (= {"/docs/input.md" "text"} (:inputs @seen)))
      (is (= request-id (:expected-bundle @seen)))
      (doseq [extra [{:model "forbidden"} {:task "forbidden"} {:protocol {}}]]
        (is (thrown? clojure.lang.ExceptionInfo
                     (call :simmis/local-worker-workflow (merge workflow extra)))))
      (is (thrown? clojure.lang.ExceptionInfo
                   (call :simmis/local-worker-workflow (assoc-in workflow [:params "subject"] "duplicate")))))))

(use-fixtures :each
  (fn [f]
    (with-redefs [control/admission-open? (constantly true)
                  maintenance/claims-ready? (constantly true)
                  api/room! (fn [_ arg] (is (= "room" arg)) [::conn room-id ::room])
                  registry/get-model (constantly {:provider :codex-subscription})
                  auth/get-auth-config (constantly {:jwt {:secret "fixture-issuer-key"}})]
      (binding [ops/*principal* principal] (f)))))

(deftest canonical-control-adapters-use-authenticated-principal-and-owned-room
  (let [calls (atom [])]
    (with-redefs [service/enroll! (fn [& args] (swap! calls conj [:enroll args]) {:credential "fixture-only"})
                  service/revoke! (fn [& args] (swap! calls conj [:revoke args]) {:revoked true})
                  work/provision! (fn [& args] (swap! calls conj [:allowance args]) {:funded true})
                  work/submit! (fn [& args] (swap! calls conj [:task args]) {:job request-id})]
      (is (= {:credential "fixture-only"} (call :simmis/local-worker-enroll enroll)))
      (is (= {:revoked true} (call :simmis/local-worker-revoke revoke)))
      (is (= {:funded true} (call :simmis/local-worker-allowance allowance)))
      (is (= {:job request-id} (call :simmis/local-worker-task task)))
      (is (= [::conn principal room-id "fixture-issuer-key" worker-id config] (vec (second (first @calls)))))
      (is (= [::conn principal room-id worker-id] (vec (second (second @calls)))))
      (doseq [[kind args] (drop 2 @calls)]
        (is (= [::conn context/server-context principal room-id ::room] (vec (take 5 args)))))
      (is (= request-id (-> @calls last second last :request-id)))
      (is (= worker-id (-> @calls last second last :worker))))))

(deftest disabled-admission-keeps-revoke-but-never-enrolls-funds-or-submits
  (let [writes (atom [])]
    (with-redefs [control/admission-open? (constantly false)
                  service/enroll! (fn [& _] (swap! writes conj :enroll))
                  service/revoke! (fn [& _] (swap! writes conj :revoke) {:revoked true})
                  work/provision! (fn [& _] (swap! writes conj :fund))
                  work/submit! (fn [& _] (swap! writes conj :submit))]
      (doseq [[op args] [[:simmis/local-worker-enroll enroll] [:simmis/local-worker-allowance allowance]
                         [:simmis/local-worker-task task] [:simmis/local-worker-workflow workflow]
                         [:simmis/local-worker-benchmark benchmark]]]
        (is (thrown? clojure.lang.ExceptionInfo (call op args))))
      (is (empty? @writes))
      (is (= {:revoked true} (call :simmis/local-worker-revoke revoke)))
      (is (= [:revoke] @writes)))))

(deftest missing-recovery-service-refuses-new-admission-without-removing-revoke
  (let [writes (atom [])]
    (with-redefs [maintenance/claims-ready? (constantly false)
                  service/enroll! (fn [& _] (swap! writes conj :enroll))
                  service/revoke! (fn [& _] (swap! writes conj :revoke))]
      (is (thrown? clojure.lang.ExceptionInfo (call :simmis/local-worker-enroll enroll)))
      (is (empty? @writes))
      (call :simmis/local-worker-revoke revoke)
      (is (= [:revoke] @writes)))))

(deftest closed-config-and-registry-validation-do-not-open-native-provider-auth
  (let [enrolls (atom 0)]
    (with-redefs [service/enroll! (fn [& _] (swap! enrolls inc))]
      (doseq [bad [(assoc enroll :secret "forbidden")
                   (assoc-in enroll [:config :adapter] "claude-code")
                   (assoc-in enroll [:config :command] ["/bin/sh"])
                   (assoc enroll :worker "not-a-worker-identity")]]
        (is (thrown? clojure.lang.ExceptionInfo (call :simmis/local-worker-enroll bad))))
      (doseq [definition [nil {:provider :anthropic} {:provider :openai}]]
        (with-redefs [registry/get-model (constantly definition)]
          (is (thrown? clojure.lang.ExceptionInfo (call :simmis/local-worker-enroll enroll)))))
      (is (zero? @enrolls)))))
