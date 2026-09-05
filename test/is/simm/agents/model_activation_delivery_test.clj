(ns is.simm.agents.model-activation-delivery-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.room-context :as room-context]
            [dvergr.chat.context :as chat-context]
            [dvergr.discourse :as discourse]
            [dvergr.discourse.llm :as llm]
            [dvergr.room.registry :as room-registry]
            [dvergr.room.store.memory :as memory-store]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.model-selection :as model-selection]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [org.replikativ.spindel.core :as sp]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private joined-var
  (ns-resolve 'is.simm.agents.room-agents 'joined))

(defn- await-spin [room spin-fn wait-ms]
  (let [result (promise)]
    (binding [ec/*execution-context* (:ctx room)]
      (sp/spawn!
       (sp/spin (deliver result (sp/await (spin-fn room))))))
    (deref result wait-ms ::timeout)))

(defn- await-condition [pred wait-ms]
  (let [deadline (+ (System/currentTimeMillis) wait-ms)]
    (loop []
      (cond
        (pred) true
        (>= (System/currentTimeMillis) deadline) false
        :else (do (Thread/sleep 5) (recur))))))

(deftest owner-preference-switch-preserves-live-delivery-and-canonical-messages
  (let [room-id (random-uuid)
        owner-id (random-uuid)
        agent-id (random-uuid)
        actor-id (room-agents/party->actor-kw agent-id)
        preference "gpt-*-luna"
        room (discourse/make-room
              {:id :owner-model-activation-delivery
               :store (memory-store/make)})
        working-chat-ctx
        (binding [ec/*execution-context* (:ctx room)]
          (chat-context/create-chat-context
           {:title "owner model activation delivery"
            :budget-dollars 10.0
            :execution-context (:ctx room)
            :with-sci? false}))
        joined-atom (var-get joined-var)
        joined-before @joined-atom
        first-call-entered (promise)
        release-first-call (promise)
        calls (atom [])
        call-number (atom 0)
        run-turn-fn
        (fn [chat-ctx opts]
          (swap! calls conj
                 {:provider (:provider opts)
                  :model (:model opts)
                  :spec (select-keys (:spec opts) [:provider :model])})
          (case (swap! call-number inc)
            1 (do
                (deliver first-call-entered (:cancel? opts))
                @release-first-call
                (if ((:cancel? opts)) :cancelled :complete))
            2 (do
                (chat-context/add-message!
                 chat-ctx {:role :assistant :content "first reply on new model"})
                :complete)
            3 (do
                (chat-context/add-message!
                 chat-ctx {:role :assistant :content "queued reply on new model"})
                :complete)
            :complete))]
    (try
      ;; Keep this delivery regression hermetic: use a non-SCI working context
      ;; while retaining the real Room, participant, arbiter, inbox, directives,
      ;; Run lifecycle, and canonical store. SCI bootstrapping is orthogonal and
      ;; may clone the sandbox standard library on a pristine machine.
      (with-redefs [room-context/ensure-ctx!
                    (fn [& _] working-chat-ctx)
                    room-context/append-inbound!
                    (fn [_room _agent-id _msg-id role content _author _ts]
                      (chat-context/add-message!
                       working-chat-ctx {:role role :content content})
                      true)]
        (binding [ec/*execution-context* (:ctx room)]
          (discourse/join
           room
           (llm/llm-agent {:id actor-id
                           :spec {:provider :openai :model "gpt-old"}
                           :budget {:dollars 10.0}
                           :run-turn-fn run-turn-fn})))
        (let [participant-before (get @(:participants room) actor-id)
              first-reply
              (future
                (await-spin room
                            #(discourse/ask % actor-id {:content "first request"})
                            10000))]
          (is (fn? (deref first-call-entered 3000 ::timeout)))
        ;; A distinct root is queued by the active arbiter rather than steering
        ;; away the first request. The following model directive must survive
        ;; behind it in the same participant inbox.
        (discourse/post!
         room (discourse/message :human actor-id "queued request"))
        (reset! joined-atom #{[room-id agent-id]})
        (with-redefs [model-selection/describe-resolution
                      (constantly {:configured? false})
                      parties/get-party
                      (fn [id]
                        (cond
                          (= id agent-id) {:party/id agent-id
                                           :party/owner {:party/id owner-id}}
                          (= id owner-id) {:party/id owner-id
                                           :party/preferred-model preference}))
                      room-agents/describe-model-resolution
                      (constantly {:available? true
                                   :provider :openai
                                   :model "gpt-new"})
                      rooms/get-room (constantly {:room/slug "activation-delivery"})
                      room-registry/lookup (constantly room)]
          (is (= :activated
                 (:status
                  (room-agents/activate-agent-model! agent-id preference)))))
        (let [cancel? (deref first-call-entered 1000 ::timeout)]
          (is (true? (await-condition cancel? 3000))
              "the live participant consumed the switch directive"))
        (is (identical? participant-before (get @(:participants room) actor-id))
            "activation does not replace the participant or its inbox")
        (deliver release-first-call true)
        (is (= "first reply on new model" (:content (deref first-reply 5000 ::timeout))))
        (is (true? (await-condition
                    #(some (fn [message]
                             (= "queued reply on new model" (:content message)))
                           (discourse/messages room))
                    5000)))
        (is (identical? participant-before (get @(:participants room) actor-id)))
        (let [contents (mapv :content (discourse/messages room))]
          (is (some #{"first request"} contents))
          (is (some #{"queued request"} contents))
          (is (some #{"first reply on new model"} contents))
          (is (some #{"queued reply on new model"} contents)))
          (is (= [{:provider :openai
                   :model "gpt-old"
                   :spec {:provider :openai :model "gpt-old"}}
                  {:provider :openai
                   :model "gpt-new"
                   :spec {:provider :openai :model "gpt-new"}}
                  {:provider :openai
                   :model "gpt-new"
                   :spec {:provider :openai :model "gpt-new"}}]
                 @calls)
              "both restarted and queued work admit the switched flat policy")))
      (finally
        (deliver release-first-call true)
        (reset! joined-atom joined-before)
        (discourse/close-room! room)))))
