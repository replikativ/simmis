(ns is.simm.model.agent-model-inheritance-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [dvergr.agent.room-context :as room-ctx]
            [dvergr.chat.schema :as chat-schema]
            [dvergr.room.registry :as rreg]
            [dvergr.room.store :as rstore]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.agents.templates :as templates]
            [is.simm.model.model-catalog :as catalog]
            [is.simm.model.model-selection :as selection]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.system-db :as system-db]
            [is.simm.uis.web.desktop.chat-remote :as chat-remote]))

(def ^:dynamic *conn* nil)

(def ^:private joined-var
  (ns-resolve 'is.simm.agents.room-agents 'joined))

(use-fixtures
 :once
 (fn [run]
   (let [cfg {:store {:backend :memory :id (random-uuid)}
              :schema-flexibility :write
              :keep-history? false}]
     (d/create-database cfg)
     (let [conn (d/connect cfg)]
       (chat-schema/ensure-full-schema! conn)
       (d/transact conn system-db/schema)
       (binding [*conn* conn]
         (with-redefs [system-db/get-conn (constantly conn)]
           (run)))
       (d/release conn)))))

(defn- seed-owner!
  ([] (seed-owner! nil))
  ([preferred-model]
   (let [party-id (random-uuid)]
     (d/transact *conn*
                 [(cond-> {:actor/id (parties/party-id->actor-id party-id)
                           :actor/kind :human
                           :actor/name "Owner"
                           :actor/status :online
                           :actor/created-at (java.util.Date.)
                           :party/id party-id}
                    preferred-model
                    (assoc :party/preferred-model preferred-model))])
     party-id)))

(defn- stored-config [party-id]
  (some-> (d/q '[:find ?config .
                 :in $ ?actor-id
                 :where [?e :actor/id ?actor-id]
                        [?e :actor/config ?config]]
               @*conn* (parties/party-id->actor-id party-id))
          edn/read-string))

(defn- rejection [f]
  (try
    (f)
    nil
    (catch clojure.lang.ExceptionInfo e
      (ex-data e))))

(deftest creation-inherits-unless-an-override-was-explicit
  (let [owner-id (seed-owner! "gpt-*-luna")
        existing (parties/create-agent!
                  owner-id
                  {:display-name "Preferred"
                   :model "gpt-5.5"
                   :provider :openai
                   :template :researcher})
        existing-before (stored-config (:party/id existing))
        inherited (parties/create-agent!
                   owner-id
                   {:display-name "Inherited"
                    :template :secretary})
        explicit-family (parties/create-agent!
                         owner-id
                         {:display-name "Family"
                          :model-family "gpt-*-luna"})]
    (testing "a new ordinary agent stores no model selection"
      (is (= {:auto-respond? true :template :secretary}
             (stored-config (:party/id inherited))))
      (is (not-any? #(contains? inherited %)
                    [:party/model :party/model-family :party/model-version])))
    (testing "a deliberate family choice is still stored"
      (is (= "gpt-*-luna" (:model-family
                            (stored-config (:party/id explicit-family)))))
      (is (= :auto (:model-version
                    (stored-config (:party/id explicit-family))))))
    (testing "creating an inheriting agent does not rewrite an existing agent"
      (is (= existing-before (stored-config (:party/id existing))))
      (is (= "gpt-5.5" (:model existing-before)))
      (is (= :openai (:provider existing-before))))))

(deftest owner-preference-change-activates-only-inheriting-participants
  (let [owner-id (seed-owner! "gpt-*-luna")
        inherited (parties/create-agent! owner-id {:display-name "Inherited"})
        explicit (parties/create-agent!
                  owner-id {:display-name "Explicit" :model "gpt-5.5"})
        activations (atom [])]
    (with-redefs [room-agents/activate-agent-model!
                  (fn [agent-id expected]
                    (swap! activations conj [agent-id expected]))]
      (parties/update-preferred-model! owner-id "gpt-*-sol"))
    (is (= "gpt-*-sol" (:party/preferred-model (parties/get-party owner-id))))
    (is (= [[(:party/id inherited) "gpt-*-sol"]] @activations)
        "the inherited runtime is activated; an explicit override stays untouched")))

(deftest live-model-activation-preserves-the-participant-and-revalidates-state
  (let [owner-choice "gpt-*-luna"
        owner-id (seed-owner! owner-choice)
        agent (parties/create-agent! owner-id {:display-name "Inherited"})
        agent-id (:party/id agent)
        room-id (random-uuid)
        joined-atom (var-get joined-var)
        joined-before @joined-atom
        posts (atom [])]
    (try
      (reset! joined-atom #{[room-id agent-id]})
      (with-redefs [rooms/get-room (fn [id]
                                    (when (= room-id id)
                                      {:room/slug "activation-room"}))
                    rreg/lookup (constantly ::live-room)
                    discourse/post! (fn [room message]
                                      (swap! posts conj [room message]))
                    discourse/leave (fn [& _]
                                       (throw (ex-info "must not leave" {})))
                    room-ctx/drop-ctx! (fn [& _]
                                         (throw (ex-info "must not drop ctx" {})))
                    selection/describe-resolution (constantly {:configured? false})
                    room-agents/describe-model-resolution
                    (constantly {:available? true
                                 :provider :openai
                                 :model "gpt-5.6-luna"})]
        (is (= {:status :activated
                :provider :openai
                :model "gpt-5.6-luna"
                :rooms [room-id]}
               (room-agents/activate-agent-model! agent-id owner-choice)))
        (is (= [[::live-room
                 {:to (room-agents/party->actor-kw agent-id)
                  :type :directive/switch-model
                  :payload {:provider :openai :model "gpt-5.6-luna"}}]]
               @posts))
        (is (= #{[room-id agent-id]} @joined-atom)
            "activation keeps participation and inbox ownership live")

        (reset! posts [])
        (is (= :superseded
               (:status (room-agents/activate-agent-model!
                         agent-id "gpt-stale-write"))))
        (is (empty? @posts)
            "a delayed preference writer cannot post its captured choice")

        (with-redefs [room-agents/describe-model-resolution
                      (constantly {:available? false
                                   :provider :openai
                                   :candidate "gpt-unavailable"
                                   :availability :temporarily-unreachable
                                   :availability-reason nil})]
          (let [error (try
                        (room-agents/activate-agent-model! agent-id owner-choice)
                        nil
                        (catch clojure.lang.ExceptionInfo e e))]
            (is (= :model-unavailable (:type (ex-data error))))
            (is (empty? @posts))
            (is (= #{[room-id agent-id]} @joined-atom)
                "unavailability leaves the previous live participant intact"))))
      (finally
        (reset! joined-atom joined-before)))))

(deftest live-model-activation-continues-after-a-room-post-fails
  (let [owner-choice "gpt-*-luna"
        owner-id (seed-owner! owner-choice)
        agent-id (:party/id
                  (parties/create-agent! owner-id {:display-name "Inherited"}))
        failed-room-id (random-uuid)
        successful-room-id (random-uuid)
        joined-atom (var-get joined-var)
        joined-before @joined-atom
        attempts (atom [])
        failed-room-key (rstore/slug->room-id "failed-activation-room")
        successful-room-key (rstore/slug->room-id "successful-activation-room")]
    (try
      (reset! joined-atom #{[failed-room-id agent-id]
                            [successful-room-id agent-id]})
      (with-redefs [selection/describe-resolution (constantly {:configured? false})
                    room-agents/describe-model-resolution
                    (constantly {:available? true
                                 :provider :openai
                                 :model "gpt-5.6-luna"})
                    rooms/get-room
                    (fn [room-id]
                      {:room/slug (if (= room-id failed-room-id)
                                    "failed-activation-room"
                                    "successful-activation-room")})
                    rreg/lookup
                    (fn [room-key]
                      (cond
                        (= failed-room-key room-key) ::failed-room
                        (= successful-room-key room-key) ::successful-room))
                    discourse/post!
                    (fn [room _message]
                      (swap! attempts conj room)
                      (when (= ::failed-room room)
                        (throw (ex-info "synthetic room post failure"
                                        {:type :post-failed}))))]
        (let [error (try
                      (room-agents/activate-agent-model! agent-id owner-choice)
                      nil
                      (catch clojure.lang.ExceptionInfo e e))
              data (ex-data error)]
          (is (= :model-activation-partial (:type data)))
          (is (= #{::failed-room ::successful-room} (set @attempts))
              "every live room is attempted despite one post failure")
          (is (= [successful-room-id] (:rooms data)))
          (is (= [{:room-id failed-room-id
                   :error-class "clojure.lang.ExceptionInfo"
                   :type :post-failed}]
                 (:room-failures data)))
          (is (= #{[failed-room-id agent-id]
                   [successful-room-id agent-id]}
                 @joined-atom))))
      (finally
        (reset! joined-atom joined-before)))))

(deftest owner-preference-commit-is-serialized-with-live-activation
  (let [initial-choice "gpt-initial"
        first-choice "gpt-first"
        second-choice "gpt-second-unavailable"
        owner-id (seed-owner! initial-choice)
        agent-id (:party/id
                  (parties/create-agent! owner-id {:display-name "Inherited"}))
        room-id (random-uuid)
        joined-atom (var-get joined-var)
        joined-before @joined-atom
        first-resolved (promise)
        release-first (promise)
        second-started (promise)
        posted-models (atom [])]
    (try
      (reset! joined-atom #{[room-id agent-id]})
      (with-redefs [selection/describe-resolution (constantly {:configured? false})
                    rooms/get-room (constantly {:room/slug "serialized-owner-room"})
                    rreg/lookup (constantly ::serialized-owner-room)
                    discourse/post!
                    (fn [_room message]
                      (swap! posted-models conj (get-in message [:payload :model])))
                    room-agents/describe-model-resolution
                    (fn [_agent]
                      (case (:party/preferred-model (parties/get-party owner-id))
                        "gpt-first"
                        (do
                          (deliver first-resolved true)
                          @release-first
                          {:available? true
                           :provider :openai
                           :model "gpt-first-runtime"})

                        "gpt-second-unavailable"
                        {:available? false
                         :provider :openai
                         :candidate "gpt-second-runtime"
                         :availability :temporarily-unreachable}))]
        (let [first-write
              (future
                (parties/update-preferred-model! owner-id first-choice)
                :first-activated)]
          (is (= true (deref first-resolved 1000 ::timeout)))
          (let [second-write
                (future
                  (deliver second-started true)
                  (try
                    (parties/update-preferred-model! owner-id second-choice)
                    nil
                    (catch clojure.lang.ExceptionInfo e e)))]
            (is (= true (deref second-started 1000 ::timeout)))
            (is (= first-choice
                   (:party/preferred-model (parties/get-party owner-id)))
                "the newer commit waits for the older activation boundary")
            (deliver release-first true)
            (is (= :first-activated (deref first-write 1000 ::timeout)))
            (let [second-error (deref second-write 1000 ::timeout)]
              (is (= :preferred-model-activation-partial
                     (:type (ex-data second-error))))
              (is (= second-choice
                     (:party/preferred-model (parties/get-party owner-id))))
              (is (= ["gpt-first-runtime"] @posted-models)
                  "the unavailable newer choice retains the model active before its commit")))))
      (finally
        (deliver release-first true)
        (reset! joined-atom joined-before)))))

(deftest owner-preference-activation-failure-does-not-skip-later-agents
  (let [owner-id (seed-owner! "gpt-*-luna")
        first-agent (parties/create-agent! owner-id {:display-name "First"})
        second-agent (parties/create-agent! owner-id {:display-name "Second"})
        explicit (parties/create-agent!
                  owner-id {:display-name "Explicit" :model "gpt-5.5"})
        fail-id (first (sort-by str [(:party/id first-agent)
                                    (:party/id second-agent)]))
        calls (atom [])
        error (with-redefs [room-agents/activate-agent-model!
                            (fn [agent-id _expected]
                              (swap! calls conj agent-id)
                              (when (= fail-id agent-id)
                                (throw (ex-info "synthetic activation failure"
                                                {:type :model-unavailable
                                                 :model "gpt-unavailable"
                                                 :provider :openai
                                                 :availability
                                                 :temporarily-unreachable}))))]
                (try
                  (parties/update-preferred-model! owner-id "gpt-*-sol")
                  nil
                  (catch clojure.lang.ExceptionInfo e e)))]
    (is (= "gpt-*-sol" (:party/preferred-model (parties/get-party owner-id))))
    (is (= (set [(:party/id first-agent) (:party/id second-agent)])
           (set @calls))
        "both inheriting agents are attempted despite one failure")
    (is (not (some #{(:party/id explicit)} @calls)))
    (is (= :preferred-model-activation-partial (:type (ex-data error))))
    (is (true? (:preference-committed? (ex-data error))))
    (is (= [fail-id] (mapv :agent-id (:failures (ex-data error)))))
    (is (= {:type :model-unavailable
            :model "gpt-unavailable"
            :provider :openai
            :availability :temporarily-unreachable}
           (select-keys (first (:failures (ex-data error)))
                        [:type :model :provider :availability])))))

(deftest override-cleared-during-owner-save-is-included-in-activation
  (let [owner-id (seed-owner! "gpt-*-luna")
        explicit (parties/create-agent!
                  owner-id {:display-name "Becoming inherited"
                            :model "gpt-5.5"})
        agent-id (:party/id explicit)
        original-update parties/update-party!
        owner-write-entered (promise)
        allow-owner-write (promise)
        activations (atom [])]
    (with-redefs [parties/update-party!
                  (fn [party-id updates]
                    (if (= owner-id party-id)
                      (do
                        (deliver owner-write-entered true)
                        @allow-owner-write
                        (original-update party-id updates))
                      (original-update party-id updates)))
                  room-agents/activate-agent-model!
                  (fn [id expected]
                    (swap! activations conj [id expected]))]
      (let [preference-save
            (future
              (parties/update-preferred-model! owner-id "gpt-*-sol")
              :saved)]
        (is (= true (deref owner-write-entered 1000 ::timeout)))
        ;; This is the losing interleaving from the review: the agent was
        ;; excluded when the owner operation began, then its override vanished
        ;; before the owner preference committed.
        (parties/update-agent!
         agent-id {:party/model nil
                   :party/model-family nil
                   :party/model-version nil
                   :party/provider nil})
        (deliver allow-owner-write true)
        (is (= :saved (deref preference-save 1000 ::timeout)))
        (is (= [[agent-id "gpt-*-sol"]] @activations)
            "post-commit classification activates the newly inheriting agent")
        (is (= "gpt-*-sol"
               (:party/preferred-model (parties/get-party owner-id))))))))

(deftest persona-options-never-copy-template-model-metadata
  (let [opts (templates/agent-options
              "Researcher"
              {:id :researcher
               :system-prompt "Research carefully."
               :model "gpt-5.5"
               :model-family "gpt-*-luna"
               :model-version :auto
               :provider :openai})]
    (is (= {:display-name "Researcher"
            :auto-respond? true
            :template :researcher
            :system-prompt "Research carefully."}
           opts))))

(deftest explicit-override-and-clearing-round-trip-through-storage
  (let [owner-choice "gpt-*-luna"
        owner-id (seed-owner! owner-choice)
        agent (parties/create-agent! owner-id {:display-name "Agent"})
        agent-id (str (:party/id agent))
        validated (atom [])]
    (with-redefs [catalog/require-available-choice!
                  (fn [value]
                    (swap! validated conj value)
                    {:value value :available? true})
                  catalog/require-usable-preference!
                  (fn [value]
                    (swap! validated conj value)
                    {:value value :available? true})
                  room-agents/describe-model-resolution identity]
      (testing "an exact override stores only the exact model form"
        (chat-remote/update-agent-config-server agent-id "" "gpt-5.5" nil)
        (is (= "gpt-5.5" (:model (stored-config (:party/id agent)))))
        (is (not (contains? (stored-config (:party/id agent)) :model-family))))
      (testing "a family override replaces the exact form"
        (chat-remote/update-agent-config-server agent-id "" "gpt-*-luna" nil)
        (is (= "gpt-*-luna" (:model-family
                              (stored-config (:party/id agent)))))
        (is (= :auto (:model-version
                      (stored-config (:party/id agent)))))
        (is (not (contains? (stored-config (:party/id agent)) :model))))
      (testing "the inheritance row validates the owner choice then removes all override keys"
        (chat-remote/update-agent-config-server
         agent-id "" catalog/inherit-choice-value nil)
        (is (= ["gpt-5.5" "gpt-*-luna" owner-choice] @validated))
        (is (= {:auto-respond? true}
               (stored-config (:party/id agent))))))))

(deftest owner-without-preference-inherits-the-validated-product-default
  (let [owner-id (seed-owner!)
        agent (parties/create-agent!
               owner-id {:display-name "Preferred" :model "gpt-5.5"})
        validated (atom [])]
    (with-redefs [catalog/require-available-choice!
                  (fn [value]
                    (swap! validated conj value)
                    {:value value :available? true})
                  catalog/require-usable-preference!
                  (fn [value]
                    (swap! validated conj value)
                    {:value value :available? true})
                  room-agents/describe-model-resolution identity]
      (chat-remote/update-agent-config-server
       (str (:party/id agent)) "" catalog/inherit-choice-value nil))
    (is (= [parties/default-model] @validated))
    (is (= {:auto-respond? true}
           (stored-config (:party/id agent))))))

(deftest unavailable-owner-preference-cannot-clear-an-override
  (let [owner-choice "gpt-*-luna"
        owner-id (seed-owner! owner-choice)
        agent (parties/create-agent!
               owner-id {:display-name "Preferred" :model "gpt-5.5"})
        before (stored-config (:party/id agent))]
    (with-redefs [catalog/require-usable-preference!
                  (fn [value]
                    (throw (ex-info "Model choice is unavailable"
                                    {:type :model-choice-unavailable
                                     :model-choice value
                                     :availability :needs-credential})))]
      (let [data (rejection
                  #(chat-remote/update-agent-config-server
                    (str (:party/id agent)) "Renamed"
                    catalog/inherit-choice-value "Changed prompt"))]
        (is (= :model-choice-unavailable (:type data)))
        (is (= owner-choice (:model-choice data)))
        (is (= :needs-credential (:availability data)))))
    (is (= before (stored-config (:party/id agent)))
        "model, name, and prompt remain unchanged when inheritance is invalid")))

(deftest picker-row-and-summary-distinguish-inherited-from-explicit
  (binding [selection/*env-lookup* {}]
    (selection/reset-catalog!)
    (let [owner-id (seed-owner! "gpt-*-luna")
          inherited (parties/create-agent! owner-id {:display-name "Inherited"})
          explicit (parties/create-agent!
                    owner-id {:display-name "Explicit" :model "gpt-5.5"})
          inherited-info (room-agents/describe-model-resolution inherited)
          explicit-info (room-agents/describe-model-resolution explicit)
          inherit-row (:inheritance-choice inherited-info)]
      (testing "inheritance is a selected state, not an implicit explicit model"
        (is (false? (:configured? inherited-info)))
        (is (:inherited? inherited-info))
        (is (= :owner-preference (:selection-source inherited-info)))
        (is (= "Inherited from owner preference" (:selection-label inherited-info))))
      (testing "the first-class row names and validates the resolved owner preference"
        (is (= catalog/inherit-choice-value (:value inherit-row)))
        (is (= "Use owner preference — GPT Luna (Latest)" (:label inherit-row)))
        (is (= :owner-preference (:inheritance-source inherit-row)))
        (is (false? (:available? inherit-row))))
      (testing "an existing explicit choice stays visibly explicit"
        (is (:configured? explicit-info))
        (is (false? (:inherited? explicit-info)))
        (is (= :agent-override (:selection-source explicit-info)))
        (is (= "Explicit override" (:selection-label explicit-info))))
      (testing "display data describes desired resolution, not active state"
        (is (= "Resolves to" (:resolution-label inherited-info)))
        (is (= :not-inspected (:runtime-state inherited-info)))
        (is (= "Not inspected" (:runtime-label inherited-info)))
        (is (re-find #"already joined participant"
                     (:runtime-explanation inherited-info)))
        (is (not (contains? inherited-info :active-model)))
        (is (not (contains? inherited-info :running-model)))))))

(deftest a-preference-whose-family-has-no-known-version-is-reported-not-thrown
  ;; The registry entries behind a stored family can be gone — an older dvergr
  ;; on the classpath, or a withdrawn family. Resolution then has no candidate,
  ;; and `describe-model-resolution` used to throw out of the whole
  ;; room-details response instead of reporting the row as unusable.
  (binding [selection/*env-lookup* {}]
    (selection/reset-catalog!)
    (with-redefs [selection/known-versions-in (fn [& _] [])]
      (let [owner-id (seed-owner! "gpt-*-luna")
            agent (parties/create-agent! owner-id {:display-name "Inherited"})
            info (room-agents/describe-model-resolution agent)]
        (is (false? (:available? info)))
        (is (nil? (:model info)))
        (is (nil? (:candidate info)))
        (is (= :needs-credential (:availability info)))
        (is (= "Credential required" (:availability-label info)))
        (is (= "GPT Luna (Latest)" (:choice-label info)))))))

(deftest room-settings-copy-follows-availability
  ;; Room settings prints `:next-join-copy` verbatim. With no credential the
  ;; join refuses with :model-unavailable, so the sentence must not promise a
  ;; resolution.
  (binding [selection/*env-lookup* {}]
    (selection/reset-catalog!)
    (let [owner-id (seed-owner! "gpt-*-luna")
          agent (parties/create-agent! owner-id {:display-name "Inherited"})
          info (room-agents/describe-model-resolution agent)]
      (is (false? (:available? info)))
      (is (= "Credential required — will not join until this resolves"
             (:next-join-copy info)))
      (is (not (re-find #"Resolves to" (:next-join-copy info)))))))

(deftest owner-without-preference-is-labelled-as-product-default-inheritance
  (binding [selection/*env-lookup* {}]
    (selection/reset-catalog!)
    (let [owner-id (seed-owner!)
          agent (parties/create-agent! owner-id {:display-name "Inherited"})
          info (room-agents/describe-model-resolution agent)]
      (is (false? (:configured? info)))
      (is (:inherited? info))
      (is (= :product-default (:selection-source info)))
      (is (= "Inherited from product default" (:selection-label info)))
      (is (re-find #"^Use owner preference — not set; product default "
                   (get-in info [:inheritance-choice :label]))))))
