(ns is.simm.model.model-selection-test
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [dvergr.model.registry :as registry]
            [is.simm.model.fake-models-server :as fake]
            [is.simm.model.model-selection :as selection]))

(def ^:private openai-key "fixture-openai-key")
(def ^:private fireworks-key "fixture-fireworks-key")
(def ^:private anthropic-key "fixture-anthropic-key")

(use-fixtures
  :each
  (fn [f]
    (let [before @registry/registry]
      (selection/reset-catalog!)
      (registry/register-model!
       {:id "accounts/fireworks/models/glm-5p2"
        :name "GLM 5.2"
        :provider :fireworks
        :api-type :openai-chat
        :capabilities #{:tools :streaming :system-prompt}
        :context 131072
        :max-output 8192
        :pricing {:input 1 :output 1}
        :quirks {}})
      (try (f)
           (finally
             (reset! registry/registry before)
             (selection/reset-catalog!))))))

(defn- fixture-bases [base-url]
  {:openai (str base-url "/openai")
   :fireworks (str base-url "/fireworks")
   :anthropic (str base-url "/anthropic")})

(defn- with-config [fixture env f]
  (binding [selection/*env-lookup* env
            selection/*provider-base-urls* (fixture-bases (:base-url fixture))]
    (selection/reset-catalog!)
    (f)))

(defn- by-provider [records]
  (into {} (map (juxt :provider identity)) records))

(defn- catalog-facts [record]
  (select-keys record [:provider :base-url :credential-source :reachability
                       :reachable? :model-id :endpoint-kind :native-openai?]))

(def ^:private configured-endpoints-var
  (ns-resolve 'is.simm.model.model-selection 'configured-endpoints))

(def ^:private fetch-endpoint-var
  (ns-resolve 'is.simm.model.model-selection 'fetch-endpoint!))

(def ^:private catalog-cache-var
  (ns-resolve 'is.simm.model.model-selection 'catalog-cache))

(def ^:private await-refresh-var
  (ns-resolve 'is.simm.model.model-selection 'await-refresh))

(def ^:private concurrency-endpoint
  {:provider :openai
   :base-url "https://catalog-concurrency.test/v1"
   :credential-source "OPENAI_API_KEY"
   :endpoint-kind :openai-compatible
   :native-openai? false
   :catalog-contract :openai-compatible-models-list
   :credential "fixture-only"})

(defn- cached-model-ids []
  (->> @(var-get catalog-cache-var)
       :endpoints
       vals
       (mapcat :models)
       (mapv :model-id)))

(deftest concurrent-refreshes-of-one-configuration-are-single-flight
  (let [calls (atom 0)
        first-started (promise)
        second-joined (promise)
        original-await (var-get await-refresh-var)
        release-first (promise)]
    (with-redefs-fn
      {configured-endpoints-var (constantly [concurrency-endpoint])
       await-refresh-var (fn [ticket]
                           (deliver second-joined true)
                           (original-await ticket))
       fetch-endpoint-var
       (fn [_]
         (case (swap! calls inc)
           1 (do
               (deliver first-started true)
               @release-first
               {:outcome :unreachable})
           {:outcome :reachable :model-ids ["gpt-should-not-be-fetched"]}))}
      (fn []
        (let [first-refresh (future (selection/catalog true))]
          (is (= true (deref first-started 1000 ::timeout)))
          (let [second-refresh (future (selection/catalog true))]
            (is (= true (deref second-joined 1000 ::timeout)))
            (deliver release-first true)
            (is (= [] (deref first-refresh 1000 ::timeout)))
            (is (= [] (deref second-refresh 1000 ::timeout)))
            (is (= 1 @calls))))))))

(deftest reset-fences-an-older-failure-from-a-newer-success
  (let [calls (atom 0)
        old-started (promise)
        release-old (promise)]
    (with-redefs-fn
      {configured-endpoints-var (constantly [concurrency-endpoint])
       fetch-endpoint-var
       (fn [_]
         (if (= 1 (swap! calls inc))
           (do
             (deliver old-started true)
             @release-old
             {:outcome :unreachable})
           {:outcome :reachable :model-ids ["gpt-new-success"]}))}
      (fn []
        (let [old-refresh (future (selection/catalog true))]
          (is (= true (deref old-started 1000 ::timeout)))
          (selection/reset-catalog!)
          (is (= ["gpt-new-success"]
                 (mapv :model-id (selection/catalog true))))
          (deliver release-old true)
          (is (= [] (deref old-refresh 1000 ::timeout)))
          (is (= ["gpt-new-success"] (cached-model-ids))
              "the pre-reset completion cannot replace post-reset evidence")
          (is (= ["gpt-new-success"]
                 (mapv :model-id (selection/catalog)))))))))

(deftest reset-fences-a-configuration-read-that-has-not-claimed-the-cache
  (let [configuration-reads (atom 0)
        fetched-bases (atom [])
        old-configuration-read (promise)
        release-old-configuration (promise)
        old-endpoint (assoc concurrency-endpoint
                            :base-url "https://old-catalog.test/v1")
        new-endpoint (assoc concurrency-endpoint
                            :base-url "https://new-catalog.test/v1")]
    (with-redefs-fn
      {configured-endpoints-var
       (fn []
         (if (= 1 (swap! configuration-reads inc))
           (do
             (deliver old-configuration-read true)
             @release-old-configuration
             [old-endpoint])
           [new-endpoint]))
       fetch-endpoint-var
       (fn [endpoint]
         (swap! fetched-bases conj (:base-url endpoint))
         {:outcome :reachable
          :model-ids [(if (= endpoint old-endpoint)
                        "gpt-old-configuration"
                        "gpt-new-configuration")]})}
      (fn []
        ;; This caller captured the old generation but has not yet obtained a
        ;; refresh ticket: endpoint discovery itself is still blocked.
        (let [old-call (future (selection/catalog))]
          (is (= true (deref old-configuration-read 1000 ::timeout)))
          (selection/reset-catalog!)
          (is (= ["gpt-new-configuration"]
                 (mapv :model-id (selection/catalog true))))
          (deliver release-old-configuration true)
          (is (= ["gpt-new-configuration"]
                 (mapv :model-id (deref old-call 1000 ::timeout))))
          (is (= ["https://new-catalog.test/v1"] @fetched-bases)
              "the pre-reset endpoint configuration never reaches the fetch boundary")
          (is (= ["gpt-new-configuration"] (cached-model-ids))))))))

(deftest version-family-and-ordering
  (is (= "5.6" (selection/version-of "gpt-5.6-luna")))
  (is (= "gpt-*-luna" (selection/family-of "gpt-5.6-luna")))
  (is (= "accounts/fireworks/models/glm-*"
         (selection/family-of "accounts/fireworks/models/glm-5p2"))))

(deftest pure-availability-state-matrix
  (let [base {:provider-known? true
              :credential-present? true
              :registered? true
              :implemented? true
              :catalog-required? true
              :catalog-reachability :reachable
              :served? true}
        cases [["served + registered + implemented"
                {}
                :available]
               ["missing provider credential"
                {:credential-present? false}
                :needs-credential]
               ["served + unregistered"
                {:registered? false}
                :not-implemented]
               ["registered + adapter gap"
                {:implemented? false}
                :not-implemented]
               ["registered + not served"
                {:served? false}
                :unavailable-to-account]
               ["neither served nor registered"
                {:registered? false :served? false}
                :not-implemented]
               ["transient catalog failure"
                {:catalog-reachability :temporarily-unreachable}
                :temporarily-unreachable]
               ["provider refused the credential"
                {:catalog-reachability :credential-rejected}
                :credential-rejected]
               ["a refused credential outranks last-known served evidence"
                {:catalog-reachability :credential-rejected :served? false}
                :credential-rejected]
               ["provider without a catalog endpoint"
                {:catalog-required? false
                 :catalog-reachability :not-required
                 :served? false}
                :available]
               ["unknown provider/adapter"
                {:provider-known? false}
                :not-implemented]]]
    (doseq [[label overrides expected] cases]
      (testing label
        (is (= expected
               (selection/availability-state (merge base overrides)))))))

  (is (= :registry-missing
         (selection/availability-reason
          {:provider-known? true :credential-present? true
           :registered? false :implemented? false
           :catalog-required? true :catalog-reachability :reachable
           :served? true})))
  (is (= :adapter-missing
         (selection/availability-reason
          {:provider-known? true :credential-present? true
           :registered? true :implemented? false
           :catalog-required? true :catalog-reachability :reachable
           :served? true}))))

(defn- availability-stub [usable seen]
  (fn
    ([id]
     ((availability-stub usable seen) (selection/infer-provider id) id))
    ([provider id]
     (swap! seen conj [provider id])
     (let [available? (contains? @usable [(keyword provider) id])]
       {:state (if available? :available :unavailable-to-account)
        :available? available?
        :provider (keyword provider)
        :model-id id}))))

(deftest latest-stores-a-family-and-upgrades-to-the-newest-usable-version
  (let [versions (atom ["5.5"])
        usable (atom #{[:openai "gpt-5.5-luna"]})
        seen (atom [])]
    (with-redefs [selection/known-versions-in (fn [_ _] @versions)
                  selection/model-availability (availability-stub usable seen)]
      (let [selection {:provider :openai
                       :family "gpt-*-luna"
                       :version :auto}
            before (selection/resolve-selection selection)]
        (is (= :latest (:selection-kind before)))
        (is (= "gpt-5.5-luna" (:model before)))
        (is (false? (:preferred? before)))
        (reset! versions ["5.6" "5.5"])
        (swap! usable conj [:openai "gpt-5.6-luna"])
        (let [after (selection/resolve-selection selection)]
          (is (= "gpt-5.6-luna" (:model after)))
          (is (= "gpt-*-luna" (:family selection))
              "Latest remains a stored family rather than becoming a version"))))))

(deftest preferred-version-remains-selected-while-usable
  (let [usable (atom #{[:openai "gpt-5.5-luna"]})
        seen (atom [])]
    (with-redefs [selection/known-versions-in (fn [_ _] ["5.6" "5.5"])
                  selection/model-availability (availability-stub usable seen)]
      (let [result (selection/resolve-selection
                    {:provider :openai :model "gpt-5.5-luna"})]
        (is (= :preferred-version (:selection-kind result)))
        (is (= "gpt-5.5-luna" (:preferred-model result)))
        (is (= "gpt-5.5-luna" (:candidate result)))
        (is (= "gpt-5.5-luna" (:model result)))
        (is (false? (:fallback? result)))
        (is (:available? result))))))

(deftest withdrawn-preferred-version-falls-forward-within-family
  (let [usable (atom #{[:openai "gpt-5.6-luna"]})
        seen (atom [])]
    (with-redefs [selection/known-versions-in (fn [provider family]
                                               (is (= :openai provider))
                                               (is (= "gpt-*-luna" family))
                                               ["5.6" "5.5"])
                  selection/model-availability (availability-stub usable seen)]
      (let [result (selection/resolve-selection
                    {:provider :openai
                     :family "gpt-*-luna"
                     :version "5.5"})]
        (is (= "gpt-5.5-luna" (:preferred-model result)))
        (is (= :unavailable-to-account
               (get-in result [:preferred-availability :state])))
        (is (= "gpt-5.6-luna" (:model result)))
        (is (= "gpt-5.6-luna" (:fallback-model result)))
        (is (= :preferred-version-unavailable (:fallback-reason result)))
        (is (:fallback? result))
        (is (:available? result))
        (is (= :available (get-in result [:resolved-availability :state])))))))

(deftest unavailable-preferred-version-with-no-newer-candidate-is-unavailable
  (let [usable (atom #{[:openai "gpt-5.4-luna"]
                       [:fireworks selection/default-model]})
        seen (atom [])]
    (with-redefs [selection/known-versions-in (fn [_ _] ["5.6" "5.5" "5.4"])
                  selection/model-availability (availability-stub usable seen)]
      (let [result (selection/resolve-selection
                    {:provider :openai :model "gpt-5.5-luna"})]
        (is (= "gpt-5.5-luna" (:preferred-model result)))
        (is (nil? (:model result)))
        (is (false? (:fallback? result)))
        (is (false? (:available? result)))
        (is (not-any? #(= [:openai "gpt-5.4-luna"] %) @seen)
            "an older usable version is never considered")
        (is (not-any? #(= [:fireworks selection/default-model] %) @seen)
            "the product fallback is never considered")))))

(deftest preferred-fallback-refuses-other-families
  (let [usable (atom #{[:openai "gpt-5.6-sol"]
                       [:fireworks selection/default-model]})
        seen (atom [])]
    (with-redefs [selection/known-versions-in
                  (fn [provider family]
                    (is (= [:openai "gpt-*-luna"] [provider family]))
                    ["5.5"])
                  selection/model-availability (availability-stub usable seen)]
      (let [result (selection/resolve-selection
                    {:provider :openai :model "gpt-5.5-luna"})]
        (is (nil? (:model result)))
        (is (every? #(= "gpt-5.5-luna" (second %)) @seen))
        (is (false? (:available? result)))))))

(deftest preferred-fallback-is-provider-isolated
  (let [usable (atom #{[:fireworks "gpt-5.6-luna"]})
        seen (atom [])]
    (with-redefs [selection/known-versions-in
                  (fn [provider family]
                    (is (= [:openai "gpt-*-luna"] [provider family]))
                    ["5.6" "5.5"])
                  selection/model-availability (availability-stub usable seen)]
      (let [result (selection/resolve-selection
                    {:provider :openai :model "gpt-5.5-luna"})]
        (is (nil? (:model result)))
        (is (false? (:fallback? result)))
        (is (every? #(= :openai (first %)) @seen)
            "another provider's same-looking id is never consulted")))))

(deftest preferred-version-recovers-automatically-when-it-reappears
  (let [usable (atom #{[:openai "gpt-5.6-luna"]})
        seen (atom [])
        selection {:provider :openai :model "gpt-5.5-luna"}]
    (with-redefs [selection/known-versions-in (fn [_ _] ["5.6" "5.5"])
                  selection/model-availability (availability-stub usable seen)]
      (let [during-withdrawal (selection/resolve-selection selection)]
        (is (= "gpt-5.5-luna" (:preferred-model during-withdrawal)))
        (is (= "gpt-5.6-luna" (:model during-withdrawal)))
        (is (:fallback? during-withdrawal)))
      (swap! usable conj [:openai "gpt-5.5-luna"])
      (let [after-recovery (selection/resolve-selection selection)]
        (is (= "gpt-5.5-luna" (:preferred-model after-recovery)))
        (is (= "gpt-5.5-luna" (:model after-recovery)))
        (is (false? (:fallback? after-recovery)))
        (is (= :available
               (get-in after-recovery [:preferred-availability :state])))
        (is (= {:provider :openai :model "gpt-5.5-luna"} selection)
            "fallback is computed, never persisted over the preference")))))

(deftest latest-names-the-newest-supported-version-when-none-is-usable
  ;; Under an outage or a missing credential nothing in the family is usable,
  ;; but the Latest row still has to name something. Naming the newest KNOWN
  ;; version picked up ids the provider serves and dvergr does not implement,
  ;; which reported a reachability problem as "Not supported".
  (binding [selection/*env-lookup* {}]
    (with-redefs [selection/known-versions-in (fn [& _] ["9p9" "5p2"])]
      (let [result (selection/resolve-selection
                    {:provider :fireworks
                     :family "accounts/fireworks/models/glm-*"
                     :version :auto})]
        (is (= "accounts/fireworks/models/glm-5p2" (:candidate result))
            "an unregistered newer version never becomes the named candidate")
        (is (nil? (:model result)))
        (is (false? (:available? result)))
        (is (= :needs-credential (get-in result [:availability :state])))))))

(deftest a-family-with-no-known-version-still-reports-a-state
  ;; A stored Latest preference can outlive the registry entries behind it.
  ;; The resolver used to answer that with no availability at all, and every
  ;; display surface then had nothing to render.
  (binding [selection/*env-lookup* {"OPENAI_API_KEY" openai-key}]
    (with-redefs [selection/known-versions-in (fn [_ _] [])
                  selection/provider-catalog-status
                  (constantly {:reachability :reachable :served-model-ids #{}})]
      (let [result (selection/resolve-selection
                    {:provider :openai :family "gpt-*-luna" :version :auto})]
        (is (= :latest (:selection-kind result)))
        (is (nil? (:candidate result)))
        (is (nil? (:model result)))
        (is (false? (:available? result)))
        (is (= :not-implemented (get-in result [:availability :state])))
        (is (= :registry-missing (get-in result [:availability :reason])))))))

(deftest no-keys-do-not-invent-availability
  (fake/with-server
   (fn [fixture]
     (with-config fixture {}
       (fn []
         (is (empty? (selection/provider-endpoints)))
         (is (= [] (selection/catalog true)))
         (is (= [] (selection/available-catalog)))
         (is (empty? @(:requests fixture))))))))

(deftest each-provider-key-is-used-alone
  (fake/with-server
   (fn [{:keys [base-url requests] :as fixture}]
     (fake/respond! fixture "/openai/models" openai-key ["gpt-5.6-luna"])
     (with-config fixture {"OPENAI_API_KEY" openai-key}
       (fn []
         (let [records (selection/catalog true)]
           (is (= [{:provider :openai
                    :base-url (str base-url "/openai")
                    :credential-source "OPENAI_API_KEY"
                    :reachability :reachable
                    :reachable? true
                    :model-id "gpt-5.6-luna"
                    :endpoint-kind :openai-native
                    :native-openai? true}]
                  (mapv catalog-facts records)))
           (is (not-any? #(contains? % :credential) records)))))
     (reset! requests [])
     (fake/respond! fixture "/fireworks/models" fireworks-key
                    ["accounts/fireworks/models/glm-5p2"])
     (with-config fixture {"FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (let [records (selection/catalog true)]
           (is (= [{:provider :fireworks
                    :base-url (str base-url "/fireworks")
                    :credential-source "FIREWORKS_API_KEY"
                    :reachability :reachable
                    :reachable? true
                    :model-id "accounts/fireworks/models/glm-5p2"
                    :endpoint-kind :openai-compatible
                    :native-openai? false}]
                  (mapv catalog-facts records)))
           (is (not-any? #(contains? % :credential) records)))))
     (is (= [{:path "/fireworks/models"
              :authorization (str "Bearer " fireworks-key)}]
            (mapv #(select-keys % [:path :authorization]) @requests))))))

(deftest both-provider-keys-remain-scoped
  (fake/with-server
   (fn [{:keys [requests] :as fixture}]
     (fake/respond! fixture "/openai/models" openai-key ["gpt-5.6-luna"])
     (fake/respond! fixture "/fireworks/models" fireworks-key
                    ["accounts/fireworks/models/glm-5p2"])
     (with-config fixture {"OPENAI_API_KEY" openai-key
                           "FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (let [records (selection/catalog true)]
           (is (= #{:openai :fireworks} (set (map :provider records))))
           (is (= {"/openai/models" (str "Bearer " openai-key)
                   "/fireworks/models" (str "Bearer " fireworks-key)}
                  (into {} (map (juxt :path :authorization)) @requests)))))))))

(deftest custom-openai-base-is-compatible-not-native
  (fake/with-server
   (fn [{:keys [base-url requests] :as fixture}]
     (let [custom-base (str base-url "/compatible")]
       (fake/respond! fixture "/compatible/models" openai-key ["gpt-5.5"])
       (with-config fixture {"OPENAI_API_KEY" openai-key
                             "OPENAI_BASE_URL" custom-base}
         (fn []
           (let [record (first (selection/catalog true))]
             (is (= custom-base (:base-url record)))
             (is (= :openai (:provider record)))
             (is (= :openai-compatible (:endpoint-kind record)))
             (is (false? (:native-openai? record)))
             (is (= [{:path "/compatible/models"
                      :authorization (str "Bearer " openai-key)}]
                    (mapv #(select-keys % [:path :authorization])
                          @requests))))))))))

(deftest custom-fireworks-base-is-used-for-catalog-and-is-only-compatible
  (fake/with-server
   (fn [{:keys [base-url requests] :as fixture}]
     (let [custom-base (str base-url "/fireworks-gateway")]
       (fake/respond! fixture "/fireworks-gateway/models" fireworks-key
                      ["accounts/fireworks/models/glm-5p2"])
       (with-config fixture {"FIREWORKS_API_KEY" fireworks-key
                             "FIREWORKS_BASE_URL" custom-base}
         (fn []
           (let [record (first (selection/catalog true))
                 endpoint (first (selection/provider-endpoints))]
             (is (= custom-base (:base-url record)))
             (is (= :fireworks (:provider record)))
             (is (= :openai-compatible (:endpoint-kind record)))
             (is (= :openai-compatible-models-list
                    (:catalog-contract endpoint)))
             (is (= [{:path "/fireworks-gateway/models"
                      :authorization (str "Bearer " fireworks-key)}]
                    (mapv #(select-keys % [:path :authorization])
                          @requests))))))))))

(deftest identical-urls-do-not-collapse-provider-records
  (fake/with-server
   (fn [{:keys [base-url requests] :as fixture}]
     (let [shared-base (str base-url "/shared")]
       (fake/respond! fixture "/shared/models" openai-key ["gpt-5.5"])
       (fake/respond! fixture "/shared/models" fireworks-key
                      ["accounts/fireworks/models/glm-5p2"])
       (binding [selection/*env-lookup* {"OPENAI_API_KEY" openai-key
                                        "OPENAI_BASE_URL" shared-base
                                        "FIREWORKS_API_KEY" fireworks-key}
                 selection/*provider-base-urls* {:openai shared-base
                                                 :fireworks shared-base}]
         (let [records (by-provider (selection/catalog true))]
           (is (= #{:openai :fireworks} (set (keys records))))
           (is (= shared-base (:base-url (:openai records))))
           (is (= shared-base (:base-url (:fireworks records))))
           (is (= "OPENAI_API_KEY" (:credential-source (:openai records))))
           (is (= "FIREWORKS_API_KEY" (:credential-source (:fireworks records))))
           (is (= #{(str "Bearer " openai-key) (str "Bearer " fireworks-key)}
                  (set (map :authorization @requests))))))))))

(deftest partial-outage-preserves-only-that-providers-last-good-state
  (fake/with-server
   (fn [fixture]
     (fake/respond! fixture "/openai/models" openai-key ["gpt-5.5"])
     (fake/respond! fixture "/fireworks/models" fireworks-key
                    ["accounts/fireworks/models/glm-5p2"])
     (with-config fixture {"OPENAI_API_KEY" openai-key
                           "FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (selection/catalog true)
         (fake/respond! fixture "/openai/models" openai-key ["gpt-5.6-luna"])
         (fake/outage! fixture "/fireworks/models" fireworks-key)
         (let [records (by-provider (selection/catalog true))]
           (is (= "gpt-5.6-luna" (:model-id (:openai records))))
           (is (= :reachable (:reachability (:openai records))))
           (is (= "accounts/fireworks/models/glm-5p2"
                  (:model-id (:fireworks records))))
           (is (= :unreachable (:reachability (:fireworks records))))
           (is (= ["gpt-5.6-luna"]
                  (mapv :model-id (selection/available-catalog))))
           (is (= #{"accounts/fireworks/models/glm-5p2"}
                  (:served-model-ids
                   (selection/provider-catalog-status :fireworks)))
               "last-known served evidence is retained")
           (is (= :temporarily-unreachable
                  (:state
                   (selection/model-availability
                    :fireworks "accounts/fireworks/models/glm-5p2"))))
           (is (nil? (selection/resolve-model
                      {:provider :fireworks
                       :model "accounts/fireworks/models/glm-5p2"})))))))))

(deftest total-outage-retains-history-without-claiming-availability
  (fake/with-server
   (fn [fixture]
     (fake/respond! fixture "/openai/models" openai-key ["gpt-5.5"])
     (fake/respond! fixture "/fireworks/models" fireworks-key
                    ["accounts/fireworks/models/glm-5p2"])
     (with-config fixture {"OPENAI_API_KEY" openai-key
                           "FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (selection/catalog true)
         (fake/outage! fixture "/openai/models" openai-key)
         (fake/outage! fixture "/fireworks/models" fireworks-key)
         (let [records (selection/catalog true)]
           (is (= 2 (count records)))
           (is (every? #(= :unreachable (:reachability %)) records))
           (is (= [] (selection/available-catalog)))
           (is (= [] (selection/versions-in "gpt-*-luna"))))
         (selection/reset-catalog!)
         (is (= [] (selection/catalog true))
             "an outage with no last-known-good state invents no model"))))))

;; ---------------------------------------------------------------------------
;; Catalog contract — each endpoint is read under its own, named contract
;; ---------------------------------------------------------------------------

(deftest each-endpoint-declares-the-contract-it-is-read-under
  ;; Fireworks and OpenAI both answer an OpenAI-shaped model list, and only one
  ;; of them documents it. The record has to say which, so no reader concludes
  ;; that "OpenAI-compatible" implies OpenAI's Models API.
  (fake/with-server
   (fn [{:keys [base-url] :as fixture}]
     (fake/respond! fixture "/openai/models" openai-key ["gpt-5.6-luna"])
     (fake/respond! fixture "/fireworks/models" fireworks-key
                    ["accounts/fireworks/models/glm-5p2"])
     (with-config fixture {"OPENAI_API_KEY" openai-key
                           "FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (let [records (by-provider (selection/catalog true))]
           (is (= :openai-models-api (:catalog-contract (:openai records))))
           (is (= :fireworks-inference-models-list
                  (:catalog-contract (:fireworks records)))))))
     (with-config fixture {"OPENAI_API_KEY" openai-key
                           "OPENAI_BASE_URL" (str base-url "/compatible")}
       (fn []
         (fake/respond! fixture "/compatible/models" openai-key ["gpt-5.5"])
         (is (= :openai-compatible-models-list
                (:catalog-contract (first (selection/catalog true))))))))))

(deftest the-documented-and-observed-contracts-are-labelled-as-such
  (is (true? (:documented? (:openai-models-api selection/catalog-contracts))))
  (is (false? (:documented?
               (:fireworks-inference-models-list selection/catalog-contracts)))
      "Fireworks documents completions and chat completions on the inference
       base, not a models list")
  (is (false? (:documented?
               (:openai-compatible-models-list selection/catalog-contracts))))
  ;; Pagination is per contract too. For the three OpenAI-shaped contracts a
  ;; page marker is a contract change and fails closed; Anthropic DOCUMENTS a
  ;; cursor walk, and refusing to walk it would have reported everything past
  ;; the first page as unavailable to the account.
  (is (every? #(false? (:paginated? (selection/catalog-contracts %)))
              [:openai-models-api :fireworks-inference-models-list
               :openai-compatible-models-list]))
  (is (true? (:paginated? (:anthropic-models-api selection/catalog-contracts))))
  (is (true? (:documented? (:anthropic-models-api selection/catalog-contracts))))
  (is (= :anthropic-api-key
         (:auth (:anthropic-models-api selection/catalog-contracts)))
      "Anthropic is not an OpenAI-compatible provider and does not use a bearer token")
  (is (every? #(= :bearer (:auth (selection/catalog-contracts %)))
              [:openai-models-api :fireworks-inference-models-list
               :openai-compatible-models-list])))

(deftest a-fireworks-answer-carries-provider-specific-member-fields
  ;; The observed Fireworks entries carry kind, context_length and supports_*
  ;; beside the id. Extra members are data, not a malformed response.
  (fake/with-server
   (fn [fixture]
     (fake/respond-with!
      fixture "/fireworks/models" fireworks-key 200
      {:object "list"
       :data [{:id "accounts/fireworks/models/glm-5p2"
               :object "model"
               :owned_by "fireworks"
               :kind "HF_BASE_MODEL"
               :context_length 1048576
               :supports_chat true
               :supports_tools true}
              {:id "accounts/fireworks/routers/glm-5p2-fast"
               :object "model"
               :owned_by "fireworks"}]})
     (with-config fixture {"FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (is (= ["accounts/fireworks/models/glm-5p2"
                 "accounts/fireworks/routers/glm-5p2-fast"]
                (mapv :model-id (selection/catalog true))))
         (is (true? (:available?
                     (selection/model-availability
                      :fireworks "accounts/fireworks/models/glm-5p2")))))))))

(deftest the-native-fireworks-schema-is-not-read-as-an-empty-account
  ;; `{"models": [...], "nextPageToken": ...}` is Fireworks' NATIVE
  ;; /v1/accounts/{id}/models answer. Read as a compatible body it has no
  ;; `data`, and treating that as "this account serves nothing" would disable
  ;; every Fireworks row with a permanent-sounding verdict.
  (fake/with-server
   (fn [fixture]
     (fake/respond! fixture "/fireworks/models" fireworks-key
                    ["accounts/fireworks/models/glm-5p2"])
     (with-config fixture {"FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (selection/catalog true)
         (fake/respond-with!
          fixture "/fireworks/models" fireworks-key 200
          {:models [{:name "accounts/fireworks/models/glm-5p2"
                     :supportsServerless true}]
           :nextPageToken "cD0y"
           :totalSize 300})
         (let [record (first (selection/catalog true))
               availability (selection/model-availability
                             :fireworks "accounts/fireworks/models/glm-5p2")]
           (is (= "accounts/fireworks/models/glm-5p2" (:model-id record))
               "the last-known id is retained, not replaced by a name field")
           (is (= :unreachable (:reachability record)))
           (is (= :temporarily-unreachable (:state availability)))
           (is (false? (:available? availability)))))))))

(deftest a-page-marker-fails-closed-instead-of-truncating-the-account
  ;; A partial list would report every id below the cut as
  ;; :unavailable-to-account — a permanent verdict on incomplete evidence.
  (fake/with-server
   (fn [fixture]
     (fake/respond-with!
      fixture "/fireworks/models" fireworks-key 200
      {:object "list"
       :data [{:id "accounts/fireworks/models/glm-5p2"}]
       :has_more true})
     (with-config fixture {"FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (is (= [] (selection/catalog true)))
         (is (= :temporarily-unreachable
                (:state (selection/model-availability
                         :fireworks "accounts/fireworks/models/glm-5p2")))))))))

(deftest a-malformed-body-is-not-an-empty-account
  (fake/with-server
   (fn [fixture]
     (fake/respond! fixture "/fireworks/models" fireworks-key
                    ["accounts/fireworks/models/glm-5p2"])
     (with-config fixture {"FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (selection/catalog true)
         (fake/respond-with! fixture "/fireworks/models" fireworks-key 200
                             {:object "list" :data "not-a-list"})
         (selection/catalog true)
         (let [availability (selection/model-availability
                             :fireworks "accounts/fireworks/models/glm-5p2")]
           (is (= :temporarily-unreachable (:state availability)))
           (is (false? (:available? availability)))
           (is (= #{"accounts/fireworks/models/glm-5p2"}
                  (:served-model-ids
                   (selection/provider-catalog-status :fireworks)))
               "last-known evidence survives a malformed refresh")))))))

(deftest a-rejected-credential-is-not-an-outage
  ;; 401/403 is the provider answering: this key will not work until it is
  ;; replaced. Reported as an outage, it told an operator to wait instead.
  (fake/with-server
   (fn [fixture]
     (fake/respond! fixture "/fireworks/models" fireworks-key
                    ["accounts/fireworks/models/glm-5p2"])
     (with-config fixture {"FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (selection/catalog true)
         (fake/reject-credential! fixture "/fireworks/models" fireworks-key)
         (selection/catalog true)
         (let [status (selection/provider-catalog-status :fireworks)
               availability (selection/model-availability
                             :fireworks "accounts/fireworks/models/glm-5p2")]
           (is (= :credential-rejected (:reachability status)))
           (is (= :credential-rejected (:state availability)))
           (is (false? (:available? availability)))
           (is (= [] (selection/available-catalog)))
           (is (nil? (selection/resolve-model
                      {:provider :fireworks
                       :model "accounts/fireworks/models/glm-5p2"})))))))))

(deftest a-rejected-credential-on-one-provider-leaves-the-other-alone
  (fake/with-server
   (fn [fixture]
     (fake/respond! fixture "/openai/models" openai-key ["gpt-5.6-luna"])
     (fake/reject-credential! fixture "/fireworks/models" fireworks-key)
     (with-config fixture {"OPENAI_API_KEY" openai-key
                           "FIREWORKS_API_KEY" fireworks-key}
       (fn []
         (selection/catalog true)
         (is (= :reachable
                (:reachability (selection/provider-catalog-status :openai))))
         (is (= :credential-rejected
                (:reachability (selection/provider-catalog-status :fireworks))))
         (is (= ["gpt-5.6-luna"]
                (mapv :model-id (selection/available-catalog)))))))))

;; ---------------------------------------------------------------------------
;; Anthropic — a native contract, not an OpenAI-compatible one
;; ---------------------------------------------------------------------------

(deftest anthropic-is-addressed-under-its-own-contract
  ;; Anthropic authenticates with `x-api-key` and REQUIRES `anthropic-version`
  ;; on every request. A bearer token gets a 401 there, which this code would
  ;; have reported as `:credential-rejected` — telling an operator to replace a
  ;; key that was never the problem.
  (fake/with-server
   (fn [{:keys [requests] :as fixture}]
     (fake/anthropic-pages! fixture "/anthropic/models" anthropic-key
                            [["claude-opus-4-7"]])
     (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
       (fn []
         (let [records (selection/catalog true)
               request (first @requests)]
           (is (= [:anthropic] (mapv :provider records)))
           (is (= :anthropic-models-api (:catalog-contract (first records))))
           (is (= :anthropic-native (:endpoint-kind (first records))))
           (is (false? (:native-openai? (first records))))
           (is (= anthropic-key (:api-key request))
               "the credential travels in x-api-key")
           (is (nil? (:authorization request))
               "and never as a bearer token")
           (is (= selection/anthropic-api-version
                  (:anthropic-version request)))
           (is (not-any? #(contains? % :credential) records))))))))

(deftest anthropic-pages-are-walked-to-the-end
  ;; The documented page default is 20 and `has_more`/`last_id` carry the walk.
  ;; Reading only the first page would report every model after it as
  ;; `:unavailable-to-account` — a permanent verdict produced by pagination.
  (fake/with-server
   (fn [{:keys [requests] :as fixture}]
     (fake/anthropic-pages! fixture "/anthropic/models" anthropic-key
                            [["claude-opus-4-7" "claude-sonnet-4-6"]
                             ["claude-opus-4-6" "claude-haiku-4-5"]])
     (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
       (fn []
         (is (= ["claude-opus-4-7" "claude-sonnet-4-6"
                 "claude-opus-4-6" "claude-haiku-4-5"]
                (mapv :model-id (selection/catalog true))))
         (is (= [nil "claude-sonnet-4-6"]
                (mapv #(get (:query %) "after_id") @requests))
             "the second page is fetched with the first page's last_id")
         (is (= ["1000" "1000"] (mapv #(get (:query %) "limit") @requests)))
         (doseq [id ["claude-opus-4-7" "claude-haiku-4-5"]]
           (is (true? (:available?
                       (selection/model-availability :anthropic id)))
               (str id " is served on some page"))))))))

(deftest an-anthropic-model-absent-from-the-account-is-not-available
  ;; A complete, contract-shaped list that simply does not contain the model is
  ;; the ONE piece of evidence that may say `:unavailable-to-account`.
  (fake/with-server
   (fn [fixture]
     (fake/anthropic-pages! fixture "/anthropic/models" anthropic-key
                            [["claude-sonnet-4-6"]])
     (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
       (fn []
         (selection/catalog true)
         (is (true? (:available?
                     (selection/model-availability
                      :anthropic "claude-sonnet-4-6"))))
         (let [absent (selection/model-availability
                       :anthropic "claude-opus-4-7")]
           (is (= :unavailable-to-account (:state absent)))
           (is (false? (:available? absent)))
           (is (nil? (selection/resolve-model {:model "claude-opus-4-7"}))
               "and it does not fall through to any other model")))))))

(deftest a-registered-anthropic-model-needs-account-evidence
  ;; The regression this closes. The key is present and dvergr's registry knows
  ;; the model, which is exactly the state that used to report `:available`.
  ;; With no successful list, availability is unknown — never confirmed.
  (fake/with-server
   (fn [{:keys [requests] :as fixture}]
     (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
       (fn []
         (is (some? (registry/get-model "claude-opus-4-7"))
             "precondition: the registry carries the model")
         (selection/catalog true)
         (let [availability (selection/model-availability
                             :anthropic "claude-opus-4-7")]
           (is (not= :available (:state availability)))
           (is (false? (:available? availability)))
           (is (= :temporarily-unreachable (:state availability))
               "an endpoint that never answered is a wait, not a verdict"))
         (is (seq @requests)
             "and a credential alone no longer skips asking the provider"))))))

(deftest anthropic-with-no-credential-asks-nothing
  (fake/with-server
   (fn [{:keys [requests] :as fixture}]
     (with-config fixture {}
       (fn []
         (let [availability (selection/model-availability
                             :anthropic "claude-opus-4-7")]
           (is (= :needs-credential (:state availability)))
           (is (= "ANTHROPIC_API_KEY" (:credential-source availability))))
         (is (empty? @requests)))))))

(deftest an-anthropic-rejection-is-not-an-outage
  ;; 401 and 403 both mean this key will not work until it is replaced.
  (doseq [status [401 403]]
    (fake/with-server
     (fn [fixture]
       (fake/anthropic-pages! fixture "/anthropic/models" anthropic-key
                              [["claude-opus-4-7"]])
       (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
         (fn []
           (selection/catalog true)
           (is (true? (:available? (selection/model-availability
                                    :anthropic "claude-opus-4-7"))))
           (fake/reject-anthropic-credential!
            fixture "/anthropic/models" anthropic-key status)
           (selection/catalog true)
           (let [status' (selection/provider-catalog-status :anthropic)
                 availability (selection/model-availability
                               :anthropic "claude-opus-4-7")]
             (is (= :credential-rejected (:reachability status'))
                 (str "HTTP " status))
             (is (= :credential-rejected (:state availability)))
             (is (false? (:available? availability)))
             (is (= [] (selection/available-catalog)))
             (is (nil? (selection/resolve-model
                        {:model "claude-opus-4-7"}))))))))))

(deftest an-anthropic-outage-retains-last-known-ids-without-claiming-them
  (fake/with-server
   (fn [fixture]
     (fake/anthropic-pages! fixture "/anthropic/models" anthropic-key
                            [["claude-opus-4-7"]])
     (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
       (fn []
         (selection/catalog true)
         (fake/respond-with! fixture "/anthropic/models" anthropic-key 503
                             {:type "error"
                              :error {:type "api_error"
                                      :message "Internal server error"}})
         (selection/catalog true)
         (let [status (selection/provider-catalog-status :anthropic)
               availability (selection/model-availability
                             :anthropic "claude-opus-4-7")]
           (is (= :temporarily-unreachable (:reachability status)))
           (is (= #{"claude-opus-4-7"} (:served-model-ids status))
               "last-known evidence survives the outage")
           (is (= :temporarily-unreachable (:state availability)))
           (is (false? (:available? availability)))
           (is (= [] (selection/available-catalog)))))))))

(deftest an-unreachable-anthropic-endpoint-is-not-an-empty-account
  ;; A connection that never completes is the timeout shape: no answer at all.
  ;; It must not read as an account that serves nothing.
  (fake/with-server
   (fn [fixture]
     (fake/anthropic-pages! fixture "/anthropic/models" anthropic-key
                            [["claude-opus-4-7"]])
     (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
       (fn []
         (selection/catalog true)
         ;; Same credential, an endpoint that refuses the connection outright.
         (binding [selection/*provider-base-urls*
                   {:anthropic "http://127.0.0.1:1/v1"}]
           (selection/catalog true)
           (let [availability (selection/model-availability
                               :anthropic "claude-opus-4-7")]
             (is (= :temporarily-unreachable (:state availability)))
             (is (false? (:available? availability)))
             (is (= [] (selection/available-catalog))))))))))

(deftest an-anthropic-body-that-is-not-the-contract-is-refused
  ;; Three shapes that must never be read as "this account serves nothing":
  ;; an OpenAI-shaped answer (no `has_more`), a non-list `data`, and a
  ;; `has_more` with no cursor to follow.
  (doseq [[label body]
          [["an OpenAI-shaped body carries no has_more"
            {:object "list" :data [{:id "claude-opus-4-7"}]}]
           ["data is not a list"
            {:data "not-a-list" :has_more false}]
           ["has_more with no last_id cannot be walked"
            {:data [{:id "claude-opus-4-7"}] :has_more true :last_id nil}]
           ["not a JSON object at all"
            ["claude-opus-4-7"]]]]
    (testing label
      (fake/with-server
       (fn [fixture]
         (fake/anthropic-pages! fixture "/anthropic/models" anthropic-key
                                [["claude-opus-4-7"]])
         (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
           (fn []
             (selection/catalog true)
             (fake/respond-with! fixture "/anthropic/models" anthropic-key
                                 200 body)
             (selection/catalog true)
             (let [status (selection/provider-catalog-status :anthropic)
                   availability (selection/model-availability
                                 :anthropic "claude-opus-4-7")]
               (is (= :temporarily-unreachable (:state availability)))
               (is (false? (:available? availability)))
               (is (= #{"claude-opus-4-7"} (:served-model-ids status))
                   "last-known evidence survives a malformed refresh")
               (is (= [] (selection/available-catalog)))))))))))

(deftest a-failure-part-way-through-the-walk-discards-the-whole-list
  ;; Pages one and two are real, page three fails. Keeping the first two would
  ;; report everything after the cut as `:unavailable-to-account`, which reads
  ;; as permanent — on evidence that is merely incomplete.
  (fake/with-server
   (fn [fixture]
     (fake/anthropic-pages! fixture "/anthropic/models" anthropic-key
                            [["claude-opus-4-7"] ["claude-sonnet-4-6"]])
     (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
       (fn []
         ;; Page two now claims a third page that the fixture never serves.
         (fake/respond-at-cursor!
          fixture "/anthropic/models" anthropic-key "claude-opus-4-7" 200
          (fake/anthropic-models-body ["claude-sonnet-4-6"] true))
         (is (= [] (selection/catalog true)))
         (doseq [id ["claude-opus-4-7" "claude-sonnet-4-6"]]
           (let [availability (selection/model-availability :anthropic id)]
             (is (= :temporarily-unreachable (:state availability)) id)
             (is (false? (:available? availability)) id))))))))

(deftest anthropic-dated-snapshots-resolve-under-their-alias
  ;; Anthropic's list answers with pinned ids; before the 4.6 generation the
  ;; registry spells the same model dateless. Matched raw, an account that
  ;; plainly serves Haiku 4.5 reported it `:unavailable-to-account`.
  (fake/with-server
   (fn [fixture]
     (fake/anthropic-pages! fixture "/anthropic/models" anthropic-key
                            [["claude-haiku-4-5-20251001" "claude-opus-4-7"]])
     (with-config fixture {"ANTHROPIC_API_KEY" anthropic-key}
       (fn []
         (is (= ["claude-haiku-4-5-20251001" "claude-haiku-4-5"
                 "claude-opus-4-7"]
                (mapv :model-id (selection/catalog true)))
             "the pinned id is kept and the alias is added beside it")
         (is (true? (:available? (selection/model-availability
                                  :anthropic "claude-haiku-4-5"))))
         (is (= "claude-haiku-4-5"
                (selection/anthropic-alias-id "claude-haiku-4-5-20251001")))
         (is (nil? (selection/anthropic-alias-id "claude-opus-4-7"))
             "a dateless id is its own pinned snapshot, not an alias"))))))

(deftest one-providers-anthropic-outage-leaves-the-others-alone
  (fake/with-server
   (fn [fixture]
     (fake/respond! fixture "/fireworks/models" fireworks-key
                    ["accounts/fireworks/models/glm-5p2"])
     (fake/reject-anthropic-credential!
      fixture "/anthropic/models" anthropic-key 401)
     (with-config fixture {"FIREWORKS_API_KEY" fireworks-key
                           "ANTHROPIC_API_KEY" anthropic-key}
       (fn []
         (selection/catalog true)
         (is (= :reachable
                (:reachability (selection/provider-catalog-status :fireworks))))
         (is (= :credential-rejected
                (:reachability (selection/provider-catalog-status :anthropic))))
         (is (= ["accounts/fireworks/models/glm-5p2"]
                (mapv :model-id (selection/available-catalog)))))))))
