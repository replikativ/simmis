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
                                 [(:owner w) {:role :user :kind :workflow-completed}]]]
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
