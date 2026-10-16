(ns is.simm.model.task-models-test
  "`:models {:vision … :cheap …}`: unset → dvergr's default; a family resolves
   to its newest available version WITH the task's capability; configured and
   unresolvable refuses with the reason — never another model."
  (:require [clojure.test :refer [deftest is testing]]
            [dvergr.model.registry :as registry]
            [dvergr.substrate.config :as dcfg]
            [is.simm.model.model-selection :as selection]
            [is.simm.model.task-models :as tm]))

(def ^:private kimi "accounts/fireworks/models/kimi-*")

(defn- with-world [config available f]
  (with-redefs [dcfg/config (constantly config)
                selection/known-versions-in (fn [_ family]
                                              (if (= family kimi) ["k3" "k2p6" "k2p5"] []))
                selection/available-model? (fn ([id] (contains? available id))
                                             ([_ id] (contains? available id)))
                selection/model-availability (fn [id] {:available? (contains? available id)
                                                       :state (if (contains? available id)
                                                                :available :unavailable-to-account)})
                registry/get-model (fn [id]
                                     (case id
                                       "accounts/fireworks/models/kimi-k3" {:capabilities #{:tools}}
                                       "accounts/fireworks/models/kimi-k2p6" {:capabilities #{:tools :vision}}
                                       "accounts/fireworks/models/kimi-k2p5" {:capabilities #{:tools :vision}}
                                       {:capabilities #{:tools}}))]
    (f)))

(deftest unset-uses-dvergrs-default
  (with-world {} #{} #(is (= {} (tm/model-opts :vision)))))

(deftest a-vision-family-skips-text-only-versions
  (with-world {:models {:vision kimi}}
    #{"accounts/fireworks/models/kimi-k3" "accounts/fireworks/models/kimi-k2p5"}
    #(is (= {:model "accounts/fireworks/models/kimi-k2p5"} (tm/model-opts :vision))
         "k3 is served but text-only; k2p6 is withdrawn")))

(deftest the-cheap-family-takes-the-newest-available
  (with-world {:models {:cheap kimi}}
    #{"accounts/fireworks/models/kimi-k3"}
    #(is (= {:model "accounts/fireworks/models/kimi-k3"} (tm/model-opts :cheap)))))

(deftest configured-and-unresolvable-refuses-and-warns-once
  (with-world {:models {:vision kimi :cheap "accounts/fireworks/models/minimax-m2p7"}}
    #{"accounts/fireworks/models/kimi-k3"}
    (fn []
      (testing "refused with the reason, no substitute"
        (let [e (try (tm/model-opts :vision) nil (catch clojure.lang.ExceptionInfo e e))]
          (is (= :task-model-unavailable (:type (ex-data e))))
          (is (re-find #"no available vision-capable model in" (ex-message e)))))
      (testing "the boot check names each unusable task"
        (is (= #{:vision :cheap} (set (keys (tm/warn-unusable!)))))))))

(deftest a-non-string-is-a-configuration-error
  (with-world {:models {:vision 42}} #{}
    #(is (thrown? clojure.lang.ExceptionInfo (tm/model-opts :vision)))))
