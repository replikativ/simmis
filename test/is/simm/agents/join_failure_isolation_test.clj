(ns is.simm.agents.join-failure-isolation-test
  "BUG-B: one agent with an unavailable model must not block the room.

   `ensure-agent-joined!` fails closed when a participant's model cannot run —
   that is deliberate. This covers the caller: `post-user-message!` isolates
   the failure per agent, so the human's message is posted, the healthy agents
   still join and answer, and the failure is visible in both channels (a
   Telemere :warn for the operator, a room note for the person watching the
   silence)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [dvergr.agent.room-context :as room-ctx]
            [dvergr.discourse :as d]
            [dvergr.room.store :as room-store]
            [dvergr.room.store.memory :as memory-store]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [taoensso.telemere :as tel]))

(defn- room-agents-var [sym]
  (ns-resolve 'is.simm.agents.room-agents sym))

(def ^:private var-vor (random-uuid))
(def ^:private sol (random-uuid))
(def ^:private lun (random-uuid))
(def ^:private sender (random-uuid))

(defn- agent-party [id nm]
  {:party/id id :party/display-name nm :party/type :agent :party/auto-respond? true})

(def ^:private agents
  [(agent-party var-vor "Vár") (agent-party sol "Sol") (agent-party lun "Lun")])

(defn- unavailable-model-ex [agent-id]
  (ex-info "Agent model is unavailable"
           {:type :model-unavailable
            :agent-id agent-id
            :model "accounts/fireworks/models/qwen3p6-plus"
            :provider :fireworks
            :availability :unavailable-to-account
            :availability-reason nil
            :availability-label "Unavailable to account"
            :availability-explanation
            "Fireworks does not make this model available to this account."}))

(defn- run-send
  "Run `post-user-message!` against stubbed room plumbing, with `joinable` the
   agents whose join succeeds. Returns {:result :posted :joined :signals}."
  [text joinable & [in-reply-to]]
  (let [room-uuid (random-uuid)
        room {:ctx nil :id :test-room}
        posted (atom [])
        joined (atom [])
        {:keys [value signals]}
        (tel/with-signals
          (with-redefs-fn
            {(room-agents-var 'ensure-providers!) (constantly nil)
             #'rooms/get-room-parties (constantly agents)
             #'rooms/get-room (constantly {})
             #'parties/get-party (constantly {:party/id sender
                                              :party/type :human
                                              :party/display-name "You"})
             #'room-agents/live-room (constantly room)
             #'room-agents/ensure-room-party-entity! (constantly nil)
             #'room-agents/ensure-room-projector! (constantly nil)
             #'room-agents/ensure-agent-joined!
             (fn [_room _room-uuid agent _conn]
               (if (contains? (set joinable) (:party/id agent))
                 (swap! joined conj (:party/id agent))
                 (throw (unavailable-model-ex (:party/id agent)))))
             #'d/room-target (constantly nil)
             #'d/post! (fn [_room msg] (swap! posted conj msg) msg)}
            (fn [] (room-agents/post-user-message! room-uuid text sender nil
                                                   nil in-reply-to))))]
    {:result value
     :room-uuid room-uuid
     :posted @posted
     :joined @joined
     :signals signals}))

(defn- user-messages [posted text]
  (filter #(= text (:content %)) posted))

(defn- notes [posted text]
  (remove #(= text (:content %)) posted))

(defn- unexpected-ex []
  ;; Deliberately shaped like an error we must never echo into a room.
  (ex-info "postgres://operator:secret@example.test/rooms?token=leak"
           {:provider :test-provider}))

(defn- with-clean-join-cache [f]
  (let [join-cache (var-get (room-agents-var 'joined))
        before @join-cache]
    (try
      (reset! join-cache #{})
      (f)
      (finally
        (reset! join-cache before)))))

(deftest one-unavailable-agent-does-not-block-the-room
  (let [text "status please"
        {:keys [result posted joined signals]} (run-send text [var-vor sol])]

    (testing "the user's message is posted anyway"
      (is (= :ok (:status result)))
      (let [sent (user-messages posted text)]
        (is (= 2 (count sent)) "one copy per JOINED recipient")
        (is (= 1 (count (set (map :id sent))))
            "all copies share one id so the projector dedupes them")
        (is (= #{(room-agents/party->actor-kw var-vor)
                 (room-agents/party->actor-kw sol)}
               (set (map :to sent))))))

    (testing "the healthy agents still join and are still addressed"
      (is (= #{var-vor sol} (set joined)))
      (is (= [var-vor sol] (:recipients result)))
      (is (= [lun] (:unavailable result))))

    (testing "the failure is logged at :warn with agent, model and reason"
      (let [warn (first (filter #(= ::room-agents/agent-join-failed (:id %)) signals))]
        (is (some? warn) "a Telemere signal names the join failure")
        (is (= :warn (:level warn)))
        (is (= "Lun" (-> warn :data :agent)))
        (is (= lun (-> warn :data :agent-id)))
        (is (= "accounts/fireworks/models/qwen3p6-plus" (-> warn :data :model)))
        (is (= :unavailable-to-account (-> warn :data :availability)))))

    (testing "a room note names the failing agent and why it cannot run"
      (let [note (first (notes posted text))]
        (is (some? note) "the room gets a note, not just the log")
        (is (= (room-agents/party->actor-kw lun) (:from note))
            "authored by the agent that cannot run")
        (is (str/includes? (:content note) "Lun"))
        (is (str/includes? (:content note) "accounts/fireworks/models/qwen3p6-plus"))
        (is (str/includes? (:content note) "Unavailable to account"))
        (is (= :system (get-in note [:metadata :role])))))

    (testing "the note is addressed back to the sender, so no agent is woken"
      (let [note (first (notes posted text))]
        (is (= (room-agents/party->actor-kw sender) (:to note)))))

    (testing "the note says the rest of the room still works"
      (is (str/includes? (:content (first (notes posted text)))
                         "other agents in this room are unaffected")))))

(deftest the-failure-note-lands-in-the-thread-that-asked
  (testing "asked inside a thread, answered inside that thread"
    (let [parent (random-uuid)
          text "still there?"
          {:keys [posted]} (run-send text [var-vor sol] parent)
          sent (first (user-messages posted text))
          note (first (notes posted text))]
      (is (= parent (:in-reply-to sent))
          "the human's message is a reply into the thread it was typed in")
      (is (d/same-thread? sent note)
          "so is the note — a person who asks inside a thread must not watch it
           stay silent while the explanation appears at the room root")
      (is (= (:id sent) (:in-reply-to note))
          "and it is a reply to that message, the way a healthy agent answers")))

  (testing "asked at the room root, answered under it"
    (let [text "anyone?"
          {:keys [posted]} (run-send text [var-vor])
          sent (first (user-messages posted text))
          note (first (notes posted text))]
      (is (nil? (:in-reply-to sent)))
      (is (d/same-thread? sent note))
      (is (= (:id sent) (:in-reply-to note))))))

(deftest every-agent-unavailable-still-posts-the-message
  (let [text "anyone home?"
        {:keys [result posted joined signals]} (run-send text [])]

    (testing "the send succeeds — the message was sent, the agents are broken"
      (is (= :ok (:status result)))
      (is (empty? joined))
      (is (empty? (:recipients result)))
      (is (= #{var-vor sol lun} (set (:unavailable result)))))

    (testing "the message is still in the timeline, addressed to no agent"
      (let [sent (user-messages posted text)]
        (is (= 1 (count sent)))
        (is (= :_room-log (:to (first sent)))
            "the reserved log endpoint: persisted and projected, delivered to no participant")))

    (testing "the room says why, once per failing agent"
      (is (= 3 (count (notes posted text))))
      (is (= #{"Vár" "Sol" "Lun"}
             (set (for [n (notes posted text)
                        nm ["Vár" "Sol" "Lun"]
                        :when (str/includes? (:content n) nm)]
                    nm)))))

    (testing "no note claims other agents are unaffected — none of them are"
      (is (not-any? #(str/includes? (:content %) "unaffected") (notes posted text))))

    (testing "every failure reaches the operator log too"
      (is (= 3 (count (filter #(= ::room-agents/agent-join-failed (:id %)) signals)))))))

(deftest persistence-failure-before-context-is-isolated-and-safe
  (let [text "keep going"
        room-uuid (random-uuid)
        room {:ctx nil :id :test-room}
        posted (atom [])
        healthy #{var-vor sol}
        {:keys [value signals]}
        (tel/with-signals
          (with-redefs-fn
            {(room-agents-var 'ensure-providers!) (constantly nil)
             #'rooms/get-room-parties (constantly agents)
             #'rooms/get-room (constantly {})
             #'parties/get-party (constantly {:party/id sender :party/type :human})
             #'room-agents/live-room (constantly room)
             #'room-agents/ensure-room-projector! (constantly nil)
             #'room-agents/ensure-room-party-entity!
             (fn [_ party]
               (when (= lun (:party/id party))
                 (throw (unexpected-ex))))
             #'room-agents/ensure-agent-joined!
             (fn [_ _ agent _]
               (is (contains? healthy (:party/id agent))))
             #'d/room-target (constantly nil)
             #'d/post! (fn [_ msg] (swap! posted conj msg) msg)}
            (fn [] (room-agents/post-user-message! room-uuid text sender nil))))]
    (testing "a storage exception before context creation does not abort the send"
      (is (= :ok (:status value)))
      (is (= #{var-vor sol} (set (:recipients value))))
      (is (= [lun] (:join-errors value)))
      (is (= 2 (count (user-messages @posted text)))))
    (testing "the room note has no raw error, credential, URL, or internal class"
      (let [note (:content (first (notes @posted text)))]
        (is (str/includes? note "server-side fault"))
        (is (not (str/includes? note "secret")))
        (is (not (str/includes? note "postgres")))
        (is (not (str/includes? note "token=")))
        (is (not (str/includes? note "Exception")))))
    (testing "the operator log keeps the unexpected failure distinct"
      (let [event (first (filter #(= ::room-agents/agent-join-failed (:id %)) signals))]
        (is (= :error (:level event)))
        (is (= :join-error (get-in event [:data :failure-type])))
        (is (some? (:error event)))))))

(deftest failed-initialization-rolls-back-context-and-participant-before-retry
  (with-clean-join-cache
    (fn []
      (let [room-uuid (random-uuid)
            actor (room-agents/party->actor-kw lun)
            room {:ctx nil :id :test-room :participants (atom {})}
            ctx-present? (atom false)
            dropped (atom [])
            join-attempts (atom 0)
            fail? (atom true)
            agent (agent-party lun "Lun")
            redefs {#'room-agents/describe-model-resolution
                    (constantly {:model "test-model" :provider :test :available? true})
                    (room-agents-var 'join-participant!)
                    (fn [r _ _ _ _]
                      (swap! join-attempts inc)
                      (reset! ctx-present? true)       ; ctx + namespaces created
                      (swap! (:participants r) assoc actor {:id actor}) ; d/join partly registered
                      (when @fail? (throw (unexpected-ex))))
                    (ns-resolve 'dvergr.agent.room-context 'lookup) (fn [& _] (when @ctx-present? :ctx))
                    (ns-resolve 'dvergr.agent.room-context 'drop-ctx!) (fn [& _] (reset! ctx-present? false))
                    #'d/leave (fn [r id] (swap! (:participants r) dissoc id) (swap! dropped conj id))}]
        (with-redefs-fn redefs
          (fn []
            (testing "a fault after ctx creation and partial d/join is typed and undone"
              (let [e (try
                        (room-agents/ensure-agent-joined! room room-uuid agent nil)
                        nil
                        (catch Exception e e))]
                (is (= :join-error (:type (ex-data e))))
                (is (= #{:context :participant} (:rolled-back (ex-data e))))
                (is (false? @ctx-present?))
                (is (empty? @(:participants room)))))
            (testing "retry starts clean and creates one participant"
              (reset! fail? false)
              (room-agents/ensure-agent-joined! room room-uuid agent nil)
              (is (= 2 @join-attempts))
              (is (= 1 (count @(:participants room))))
              (is (= [actor] @dropped)))))))))

(deftest concurrent-dispatches-create-one-participant
  (with-clean-join-cache
    (fn []
      (let [room-uuid (random-uuid)
            agent-id (random-uuid)
            actor (room-agents/party->actor-kw agent-id)
            room {:ctx nil :id :concurrent-room :participants (atom {})}
            agent (agent-party agent-id "Concurrent")
            join-cache (var-get (room-agents-var 'joined))
            entered (promise)
            release (promise)
            second-started (promise)
            join-attempts (atom 0)
            redefs {#'room-agents/describe-model-resolution
                    (constantly {:model "test-model" :provider :test
                                 :available? true})
                    (room-agents-var 'join-participant!)
                    (fn [r rid party _ _]
                      (swap! join-attempts inc)
                      (deliver entered true)
                      @release
                      (swap! (:participants r) assoc actor {:id actor})
                      (swap! join-cache conj [rid (:party/id party)]))}]
        (with-redefs-fn
          redefs
          (fn []
            (let [first-join (future
                               (room-agents/ensure-agent-joined!
                                room room-uuid agent nil)
                               :joined)]
              (is (= true (deref entered 1000 ::timeout)))
              (let [second-join (future
                                  (deliver second-started true)
                                  (room-agents/ensure-agent-joined!
                                   room room-uuid agent nil)
                                  :joined)]
                (is (= true (deref second-started 1000 ::timeout)))
                (is (= ::timeout (deref second-join 100 ::timeout))
                    "the second dispatch waits for the slot owner")
                (is (= 1 @join-attempts)
                    "only one ctx/subscription construction enters")
                (deliver release true)
                (is (= :joined (deref first-join 1000 ::timeout)))
                (is (= :joined (deref second-join 1000 ::timeout)))
                (is (= 1 @join-attempts))
                (is (= 1 (count @(:participants room))))))))))))

(deftest collective-reset-cannot-miss-a-first-ever-join
  (doseq [reset-kind [:room :agent]]
    (testing (name reset-kind)
      (with-clean-join-cache
        (fn []
          (let [room-uuid (random-uuid)
                agent-id (random-uuid)
                actor (room-agents/party->actor-kw agent-id)
                room {:ctx nil :id :reset-race-room :participants (atom {})}
                agent (agent-party agent-id "Reset race")
                join-cache (var-get (room-agents-var 'joined))
                entered (promise)
                release (promise)
                reset-started (promise)
                dropped (atom [])
                redefs {#'room-agents/describe-model-resolution
                        (constantly {:model "test-model" :provider :test
                                     :available? true})
                        (room-agents-var 'join-participant!)
                        (fn [r rid party _ _]
                          (deliver entered true)
                          @release
                          (swap! (:participants r) assoc actor {:id actor})
                          (swap! join-cache conj [rid (:party/id party)]))
                        (room-agents-var 'leave-participant!)
                        (fn [_ id]
                          (swap! (:participants room) dissoc id)
                          (swap! dropped conj :participant))
                        #'rooms/get-room (constantly {:room/slug "reset-race"})
                        #'room-ctx/drop-room! (fn [& _] (swap! dropped conj :room))
                        #'room-ctx/drop-ctx! (fn [& _] (swap! dropped conj :agent))}]
            (with-redefs-fn
              redefs
              (fn []
                (let [join-future
                      (future
                        (room-agents/ensure-agent-joined!
                         room room-uuid agent nil)
                        :joined)]
                  (is (= true (deref entered 1000 ::timeout)))
                  (let [reset-future
                        (future
                          (deliver reset-started true)
                          (case reset-kind
                            :room (room-agents/reset-room-context! room-uuid)
                            :agent (room-agents/reset-agent-contexts! agent-id))
                          :reset)]
                    (is (= true (deref reset-started 1000 ::timeout)))
                    (is (= ::timeout (deref reset-future 100 ::timeout))
                        "reset waits until first admission has published its slot")
                    (deliver release true)
                    (is (= :joined (deref join-future 1000 ::timeout)))
                    (is (= :reset (deref reset-future 1000 ::timeout)))
                    (is (empty? @(:participants room)))
                    (is (not (contains? @join-cache [room-uuid agent-id])))
                    (is (some #{:participant} @dropped))
                    (is (some #{reset-kind} @dropped))))))))))))

(deftest the-room-note-survives-a-room-that-actually-stores-messages
  ;; `(d/room id)` has NO store, so the integration test below never reaches
  ;; `validate-message-metadata!`. Every real room has one, and dvergr#51 closed
  ;; that vocabulary: an unmodelled key is REJECTED, `post-join-failure-note!`
  ;; swallows the throw in its catch, and the room goes silent about why an
  ;; agent did not answer — with only the operator warning to show for it.
  (let [room (d/make-room {:id :join-isolation-durable :store (memory-store/make)})]
    (try
      (with-redefs-fn
        {(room-agents-var 'ensure-providers!) (constantly nil)
         #'rooms/get-room-parties (constantly agents)
         #'rooms/get-room (constantly {})
         #'parties/get-party (constantly {:party/id sender :party/type :human
                                          :party/display-name "You"})
         #'room-agents/live-room (constantly room)
         #'room-agents/ensure-room-party-entity! (constantly nil)
         #'room-agents/ensure-room-projector! (constantly nil)
         #'room-agents/ensure-agent-joined!
         (fn [_ _ agent _]
           (when (= lun (:party/id agent)) (throw (unavailable-model-ex lun))))}
        (fn []
          (let [result (room-agents/post-user-message! (random-uuid) "durable?" sender nil)
                stored (d/messages room)]
            (is (= [lun] (:unavailable result)))
            (is (some #(= "durable?" (:content %)) stored)
                "the human's message is stored")
            (is (some #(re-find #"cannot answer here" (str (:content %))) stored)
                "and so is the note naming the agent that could not run"))))
      (finally (d/close-room! room)))))

(deftest the-failure-note-metadata-is-a-vocabulary-dvergr-models
  (doseq [t [:model-unavailable :join-error]]
    (is (= {:role :system :kind t}
           (room-store/validate-message-metadata! {:role :system :kind t}))
        "the note's metadata must survive the durable vocabulary check")))

(deftest real-discourse-and-projector-keep-the-send-and-healthy-reply
  (let [room-uuid (random-uuid)
        room (d/room :join-isolation-integration)
        persisted (atom [])
        projected (promise)
        projector-cache (var-get (room-agents-var 'projected-rooms))
        cache-before @projector-cache
        sender-party {:party/id sender :party/type :human :party/display-name "You"}
        healthy-party (agent-party var-vor "Vár")]
    (try
      (reset! projector-cache #{})
      (with-redefs-fn
        {(room-agents-var 'ensure-providers!) (constantly nil)
         #'rooms/get-room-parties (constantly [healthy-party (agent-party lun "Lun")])
         #'rooms/get-room (constantly {})
         #'parties/get-party (fn [id] (when (= id sender) sender-party))
         #'room-agents/live-room (constantly room)
         #'room-agents/ensure-room-party-entity! (constantly nil)
         #'room-agents/ensure-agent-joined!
         (fn [r _ agent _]
           (if (= lun (:party/id agent))
             (throw (unexpected-ex))
             (d/join r (d/scripted (room-agents/party->actor-kw agent)
                                   ["healthy reply"] (:ctx r)))))
         #'room-agents/persist-message!
         (fn [_ msg-id content _ author _]
           (swap! persisted conj {:id msg-id :content content :author author})
           (when (= "healthy reply" content)
             (deliver projected true)))
         #'is.simm.model.knowledge-bases/resolve-room-links
         (fn [_ content] {:content content :unresolved []})
         #'is.simm.model.message-notify-broadcast/notify-message! (constantly nil)}
        (fn []
          (let [result (room-agents/post-user-message! room-uuid "continue" sender nil)]
            (testing "the real room bus receives the human message and the safe note"
              (is (= :ok (:status result)))
              (is (= [var-vor] (:recipients result)))
              (is (= [lun] (:join-errors result)))
              (is (some #(= "continue" (:content %)) (d/messages room)))
              (is (some #(str/includes? (:content %) "server-side fault") (d/messages room))))
            (testing "the healthy participant answers and the real projector observes it"
              (is (true? (deref projected 1000 false)))
              (is (some #(= "healthy reply" (:content %)) (d/messages room)))
              (is (some #(= "healthy reply" (:content %)) @persisted))))))
      (finally
        (reset! projector-cache cache-before)
        (d/close-room! room)))))

(deftest a-room-with-no-agents-still-broadcasts
  (let [text "mirror this"
        room-uuid (random-uuid)
        room {:ctx nil :id :test-room}
        posted (atom [])
        result (with-redefs-fn
                 {(room-agents-var 'ensure-providers!) (constantly nil)
                  #'rooms/get-room-parties (constantly [])
                  #'rooms/get-room (constantly {})
                  #'parties/get-party (constantly {:party/id sender
                                                   :party/type :human
                                                   :party/display-name "You"})
                  #'room-agents/live-room (constantly room)
                  #'room-agents/ensure-room-party-entity! (constantly nil)
                  #'room-agents/ensure-room-projector! (constantly nil)
                  #'d/room-target (constantly nil)
                  #'d/post! (fn [_room msg] (swap! posted conj msg) msg)}
                 (fn [] (room-agents/post-user-message! room-uuid text sender nil)))]
    (testing "the broadcast target is unchanged, so mirrors keep relaying"
      (is (= :ok (:status result)))
      (is (= 1 (count @posted)))
      (is (nil? (:to (first @posted)))))))
