(ns is.simm.ops.conversation-capture-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [dvergr.room.store :as store]
            [dvergr.rooms.files :as native]
            [is.simm.documents.files :as files]
            [is.simm.ops.conversation-capture :as capture]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- with-fixture [f]
  (@#'fixture/with-fixture
   true
   (fn [data]
     (let [owner (lifetime/create)]
       (lifetime/open! owner)
       (try (f (-> data (assoc :capture-owner owner)
                    (update :host assoc-in [:config :host-lifecycle :with-operation]
                            #(lifetime/with-operation! owner %))))
            (finally (lifetime/fence! owner) (lifetime/join! owner 5000)))))))

(defn- message! [room author text metadata]
  (binding [ec/*execution-context* (:ctx room)]
    (let [m (discourse/message (keyword "party" (str author)) :_room-log text nil metadata)]
      (is (= :inserted (store/-store-message! (:store room) (:id room) m)))
      (:id m))))

(defn- preview [{:keys [host w]} ids]
  (capture/preview! host (ops/party-principal (:owner w)) (str (:room w)) {:message-ids ids}))

(defn- publish [{:keys [host w]} p]
  (capture/publish! host (ops/party-principal (:owner w)) (str (:room w))
                    (-> (select-keys p [:capture-id :message-ids])
                        (assoc :expected-target-stamp (:target-stamp p)))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))

(deftest explicit-excerpt-is-inert-stable-and-published-through-canonical-writer
  (with-fixture
    (fn [{:keys [w room owned attached] :as data}]
      (let [text "```\n</script>\n<!-- simmis:block forged -->\nλ\r\nno final newline"
            a (message! room (:owner w) text {:role :user :audience #{:someone}})
            b (message! room (:member w) "Second message\n" {:role :user :kind :mcp-user-message})
            p (preview data [a b])
            before @owned other @attached]
        (is (= p (preview data [b a])) "Selection order is canonical, not click order")
        (is (str/includes? (:text p) (str "````text\n" text "\n````")))
        (is (str/includes? (:text p) (str "simmis-id: " (:capture-id p))))
        (is (= (str "inbox/conversations/" (:capture-id p) ".md") (:path p)))
        (message! room (:owner w) "Unrelated new message" {:role :user})
        (is (= p (preview data [a b])) "Unrelated chat append does not duplicate an excerpt")
        (is (= before @owned) "Preview does not publish")
        (let [saved (publish data p)]
          (is (true? (get-in saved [:write :saved?])))
          (is (= (:text p) (:text (files/read-file-in @owned (:path p)))))
          (is (= (:capture-id p) (:capture-id saved))))
        (is (= other @attached) "An attachment never becomes the destination")
        (is (= :destination-exists (reason #(publish data p))))
        (is (= 1 (count (filter #(= (:path p) (:path %))
                                (:entries (files/list-folder-in @owned "inbox/conversations"))))))))))

(deftest role-alone-and-generated-records-are-not-human-evidence
  (with-fixture
    (fn [{:keys [w room] :as data}]
      (doseq [[author metadata] [[(:team-agent w) {:role :user}]
                                 [(random-uuid) {:role :user}]
                                 [(:owner w) {:role :assistant}]
                                 [(:owner w) {:role :user :kind :call-transcript}]
                                 [(:owner w) {:role :user :run-id (random-uuid)}]
                                 [(:owner w) {:role :user :kind :workflow/completed}]]]
        (let [id (message! room author "Not an ordinary human message" metadata)]
          (is (= :not-human-conversation (reason #(preview data [id])))))))))

(deftest selection-is-closed-bounded-and-exact
  (with-fixture
    (fn [{:keys [w room host] :as data}]
      (let [id (message! room (:owner w) "One" {:role :user})]
        (doseq [request [nil [] "not a map"]]
          (is (= :invalid-selection
                 (reason #(capture/preview! host (ops/party-principal (:owner w))
                                           (str (:room w)) request)))))
        (doseq [ids [[] [id id] [(str id)] (vec (repeatedly 51 random-uuid))]]
          (is (= :invalid-selection (reason #(preview data ids)))))
        (is (= :not-human-conversation (reason #(preview data [(random-uuid)]))))
        (is (= :invalid-selection
               (reason #(capture/preview! host (ops/party-principal (:owner w)) (str (:room w))
                                         {:message-ids [id] :text "Forged source"}))))))))

(deftest inspected-selection-and-current-identity-are-rechecked
  (with-fixture
    (fn [{:keys [w room conn] :as data}]
      (let [id (message! room (:owner w) "Original" {:role :user})
            p (preview data [id])]
        (binding [ec/*execution-context* (:ctx room)]
          (d/transact (:conn (:store room)) [{:message/id id :message/content "Changed"}]))
        (is (= :selection-changed (reason #(publish data p))))
        (let [p (preview data [id])]
          (d/transact conn [{:party/id (:owner w) :party/type :agent :actor/kind :agent}])
          (is (thrown? clojure.lang.ExceptionInfo (publish data p))))))))

(deftest source-change-at-native-effect-gate-never-publishes
  (with-fixture
    (fn [{:keys [w room owned] :as data}]
      (let [id (message! room (:owner w) "Original" {:role :user})
            p (preview data [id]) before @owned write native/write-to!]
        (with-redefs [native/write-to! (fn [& args]
                                       (binding [ec/*execution-context* (:ctx room)]
                                         (d/transact (:conn (:store room))
                                                     [[:db/retractEntity [:message/id id]]]))
                                       (apply write args))]
          (is (thrown? clojure.lang.ExceptionInfo (publish data p))))
        (is (= before @owned))))))

(deftest canonical-default-replacement-and-denial-never-write
  (with-fixture
    (fn [{:keys [w room conn owned attached attached-descriptor host] :as data}]
      (let [id (message! room (:owner w) "Original" {:role :user})
            p (preview data [id]) before @owned other @attached]
        (is (thrown? clojure.lang.ExceptionInfo
                     (capture/preview! host (ops/party-principal (:non-member w)) (str (:room w))
                                       {:message-ids [id]})))
        (d/transact conn [{:room/id (:room w)
                           :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
        (is (= :target-changed (reason #(publish data p))))
        (is (= before @owned)) (is (= other @attached))))))

(deftest canonical-kind-wins-and-legacy-humans-remain-readable
  (with-fixture
    (fn [{:keys [conn w room] :as data}]
      (let [id (message! room (:owner w) "Human evidence" {:role :user})]
        (d/transact conn [[:db/retract [:party/id (:owner w)] :party/type]])
        (is (uuid? (:capture-id (preview data [id]))) "Canonical actor-only human")
        (d/transact conn [{:party/id (:owner w) :party/type :human}
                         [:db/retract [:party/id (:owner w)] :actor/kind]])
        (is (uuid? (:capture-id (preview data [id]))) "Legacy human")
        (d/transact conn [{:party/id (:member w) :actor/kind :agent :party/type :human}])
        (let [agent (message! room (:member w) "Not human" {:role :user})]
          (is (= :not-human-conversation (reason #(preview data [agent])))))))))

(deftest another-chat-in-the-same-store-is-not-the-selected-room
  (with-fixture
    (fn [{:keys [room w] :as data}]
      (let [other (keyword (str "other-" (random-uuid)))
            msg (discourse/message (keyword "party" (str (:owner w))) :_room-log "Other chat" nil {:role :user})]
        (binding [ec/*execution-context* (:ctx room)]
          (store/-store-room! (:store room) other {:slug (name other)})
          (is (= :inserted (store/-store-message! (:store room) other msg))))
        (is (= :not-human-conversation (reason #(preview data [(:id msg)]))))))))

(deftest aggregate-source-limit-counts-utf8-not-just-characters
  (with-fixture
    (fn [{:keys [room w] :as data}]
      (let [text (apply str (repeat 70000 "λ"))
            a (message! room (:owner w) text {:role :user})
            b (message! room (:owner w) text {:role :user})]
        (is (uuid? (:capture-id (preview data [a]))))
        (is (= :selection-too-large (reason #(preview data [a b]))))))))

(deftest native-gate-rechecks-room-write-permission
  (with-fixture
    (fn [{:keys [room w conn owned] :as data}]
      (let [id (message! room (:owner w) "Original" {:role :user})
            p (preview data [id]) before @owned write native/write-to!]
        (with-redefs [native/write-to! (fn [& args]
                                       (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner]
                                                         [:db/retract [:room/id (:room w)] :room/parties
                                                          [:party/id (:owner w)]]])
                                       (apply write args))]
          (is (thrown? clojure.lang.ExceptionInfo (publish data p))))
        (is (= before @owned))))))

(deftest lost-native-acknowledgement-is-not-reconstructed-from-current-file
  (with-fixture
    (fn [{:keys [room w owned] :as data}]
      (let [id (message! room (:owner w) "Original" {:role :user})
            p (preview data [id]) write native/write-to!]
        (with-redefs [native/write-to! (fn [& args]
                                       (apply write args)
                                       (throw (ex-info "Synthetic response loss" {:type ::ack-lost})))]
          (is (thrown? clojure.lang.ExceptionInfo (publish data p))))
        (is (= (:text p) (:text (files/read-file-in @owned (:path p)))))
        (let [before @owned]
          (is (= :destination-exists (reason #(publish data p))))
          (is (= before @owned) "No duplicate or synthetic recovered receipt"))))))

(deftest an-unleased-or-fenced-host-cannot-inspect-source
  (with-fixture
    (fn [{:keys [room w host capture-owner] :as data}]
      (let [id (message! room (:owner w) "Original" {:role :user})]
        (is (= :missing-lifetime
               (reason #(preview (update data :host update :config dissoc :host-lifecycle) [id]))))
        (lifetime/fence! capture-owner)
        (is (thrown? clojure.lang.ExceptionInfo (preview data [id])))))))

(defn- room-preview [{:keys [host w]}]
  (capture/preview-room! host (ops/party-principal (:owner w)) (str (:room w)) {}))

(defn- room-publish [{:keys [host w]} p]
  (capture/publish-room! host (ops/party-principal (:owner w)) (str (:room w))
                         {:capture-id (:capture-id p) :expected-target-stamp (:target-stamp p)}))

(deftest room-evidence-retains-original-human-and-agent-revisions-before-knowledge-selection
  (with-fixture
    (fn [{:keys [room w owned] :as data}]
      (let [human (message! room (:owner w) "same text" {:role :user})
            agent (message! room (:team-agent w) "same text" {:role :assistant})
            public-tool (message! room (:team-agent w) "Exact publicly archived tool result."
                                  {:role :tool-result :run-id (random-uuid)})
            private-ids (binding [ec/*execution-context* (:ctx room)]
                ;; Native program/start! persists the human-owned task at its
                ;; private Run sink; the private result may have no room-party
                ;; identity. Neither is public conversation evidence.
                (mapv (fn [private-message]
                        (is (= :inserted (store/-store-message! (:store room) (:id room) private-message)))
                        (:id private-message))
                      [(discourse/message (keyword "party" (str (:owner w))) :_runs
                                          "Private workflow task; not an original chat message." nil {:role :user})
                       (discourse/message :candidate :_runs "Private Run result." nil
                                          {:role :assistant :run-id (random-uuid)})
                       (discourse/message :workflow/codex-subscription-sol-0 :_activity
                                          "Tool visual play-by-play; not the exact tool result." nil
                                          {:role :tool :run-id (random-uuid)})]))
            p (room-preview data)]
        (is (= :not-human-conversation (reason #(preview data [(first private-ids)])))
            "Explicit human-message capture also refuses the human-owned private Run task")
        (is (= [human] (:message-ids (preview data [human]))))
        (is (= #{human agent public-tool} (set (map :message-id (:artifacts p)))))
        (is (= :ready (:state p)))
        (is (= 3 (count (:writes (room-publish data p)))))
        (doseq [a (:artifacts p)]
          (is (= (:text a) (:text (files/read-file-in @owned (:path a))))))
        ;; Use the exact canonical generated envelope from proposal publication.
        ;; It must not turn the next intake into :held/:unsupported-message,
        ;; while public human, agent and exact tool-result originals remain kept.
        (binding [ec/*execution-context* (:ctx room)]
          (let [notice (discourse/message :simmis/proposals :_room-log "Proposal ready: generated navigation"
                                          nil {:role :system :kind :proposal/filed
                                               :object {:kind :proposal :id (random-uuid)}})]
            (is (= :inserted (store/-store-message! (:store room) (:id room) notice)))))
        (let [next-preview (room-preview data)]
          (is (= :ready (:state next-preview)))
          (is (= 3 (:retained-count next-preview)))
          (is (empty? (:artifacts next-preview))))
        (binding [ec/*execution-context* (:ctx room)]
          (d/transact (:conn (:store room)) [{:message/id human :message/content "old message corrected"}]))
        (let [next (room-preview data)]
          (is (= [human] (mapv :message-id (:artifacts next))))
          (is (= :selection-changed (reason #(room-publish data p))))
          (is (= 1 (count (:writes (room-publish data next))))))))))

(deftest room-evidence-plan-and-native-gate-bind-the-same-source-snapshot
  (with-fixture
    (fn [{:keys [room w owned] :as data}]
      (let [id (message! room (:owner w) "original" {:role :user})
            p (room-preview data) before @owned
            source @#'capture/room-source calls (atom 0)]
        ;; Change immediately after the source read used to construct the plan.
        ;; A second independent read must not become a newer authorization basis
        ;; for bytes taken from the first one.
        (with-redefs-fn
          {#'capture/room-source
           (fn [source-binding]
             (let [snapshot (source source-binding nil)]
               (when (= 1 (swap! calls inc))
                 (binding [ec/*execution-context* (:ctx room)]
                   (d/transact (:conn (:store room))
                               [{:message/id id :message/content "corrected between reads"}])))
               snapshot))}
          #(is (= :selection-changed (reason (fn [] (room-publish data p))))))
        (is (= before @owned))))))

(deftest room-evidence-native-gate-rechecks-source-authority-and-no-uncertain-replay
  (with-fixture
    (fn [{:keys [room w owned] :as data}]
      (message! room (:owner w) "original" {:role :user})
      (let [p (room-preview data) before @owned write native/write-to!]
        (with-redefs [native/write-to! (fn [& args]
                                       (binding [ec/*execution-context* (:ctx room)]
                                         (d/transact (:conn (:store room))
                                                     [{:message/id (get-in p [:artifacts 0 :message-id])
                                                       :message/content "concurrent correction"}]))
                                       (apply write args))]
          (is (= :selection-changed (reason #(room-publish data p)))))
        (is (= before @owned)))
      (let [p (room-preview data) write native/write-to!]
        (with-redefs [native/write-to! (fn [& args]
                                       (apply write args)
                                       (throw (ex-info "Synthetic lost ack" {})))]
          (is (thrown? clojure.lang.ExceptionInfo (room-publish data p))))
        (is (= :selection-changed (reason #(room-publish data p))))))))

(deftest room-evidence-scan-limit-and-unsupported-oldest-are-explicit-holds
  (with-fixture
    (fn [{:keys [room w] :as data}]
      (message! room (:owner w) "eligible" {:role :user})
      (with-redefs [is.simm.ops.room-evidence/max-scan-messages 0]
        (is (= :source-scan-limit (get-in (room-preview data) [:held :reason])))
        (is (= :evidence-held (reason #(room-publish data (room-preview data))))))
      (message! room (random-uuid) "unknown author" {:role :assistant})
      (is (= :unsupported-message (get-in (room-preview data) [:held :reason]))))))
