(ns is.simm.uis.wiki-write-path-test
  "A wiki edit in the browser reaches the server, or says that it did not.

   From datahike 0.8.1882 on (simmis took 0.8.1899 in #23) the editor's trunk
   writes never left the browser. They were `opt/transact!` on the KB replica:
   a WRITER-backed overlay entry, whose durable half is the replica's
   KabelWriter. That writer now calls `kabel.remote/invoke`, and simmis composes
   distributed-scope's middleware, not kabel.remote's — so the invoke waited
   for a route that never came. No frame was sent, nothing failed, the edit
   showed as pending, and a reload brought the old text back.

   The writes are now predictions over `block_remote` RPCs
   (`optimistic-write/predict-remote!`). Three things are tested here:

   - the client mechanism, on a real datahike conn and a real overlay: a
     prediction shows at once, is dropped once the replica holds the write
     (whichever of the reply and the sync comes first), and a refusal rejects
     it and reports;
   - the server half, through the network-inbound path (`world/inbound!`): the
     handlers a prediction relies on return the head, honour the uuid the
     browser already shows, refuse a uuid that names something, and a reader
     is refused;
   - that no browser code goes back to `opt/transact!` — now one entry point
     of several that `browser-write-guard-test` refuses."
  (:require [clojure.core.async :as a :refer [<!! >!! go]]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.optimistic :as opt]
            [is.simm.authority.world :as world]
            [is.simm.model.schema :as schema]
            [is.simm.model.seed :as seed]
            [is.simm.uis.web.desktop.block-remote :as br]
            [is.simm.uis.web.desktop.optimistic-write :as ow]))

(defn- fresh-kb []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn schema/full-schema)
      (seed/ensure-seed-data! conn)
      conn)))

(def ^:private page-uuid #uuid "eeeeeeee-0000-0000-0000-000000000001")
(def ^:private block-uuid #uuid "eeeeeeee-0000-0000-0000-000000000002")

(defn- seed-page! [conn]
  (d/transact conn [{:entity/uuid page-uuid
                     :entity/name "Page eeeeeeee"
                     :instance/of-role [:entity/name "S/Page"]
                     :S.Page/title "Getting Started"}])
  (d/transact conn [{:entity/uuid block-uuid
                     :instance/of-role [:entity/name "S/Block"]
                     :block/content "<p>old</p>"
                     :block/order "a0"
                     :block/parent [:entity/uuid page-uuid]}])
  conn)

(defn- content [db uuid]
  (:block/content (d/entity db [:entity/uuid uuid])))

;; =============================================================================
;; The client mechanism
;; =============================================================================

(defn- status-chan
  "A channel of the overlay's status events."
  [overlay]
  (let [ch (a/chan 64)]
    (opt/listen-status! overlay ::test (fn [e] (a/put! ch e)))
    ch))

(defn- await-status
  "The first event with a status in `statuses`, or ::none within 10s."
  [ch statuses]
  (let [deadline (a/timeout 10000)]
    (loop []
      (let [[e port] (a/alts!! [ch deadline])]
        (cond (= port deadline) ::none
              (statuses (:status e)) e
              :else (recur))))))

(defn- pending [overlay] (count (:entries (:model @(:state overlay)))))

(deftest a-prediction-shows-at-once-and-leaves-once-the-write-is-held
  (testing "sync lands BEFORE the reply (the common case): dropped on the reply"
    (let [conn (seed-page! (fresh-kb))
          overlay (opt/open conn)
          statuses (status-chan overlay)
          release (a/promise-chan)
          result (ow/predict-remote!
                  overlay [{:entity/uuid block-uuid :block/content "<p>new</p>"}]
                  ;; the server: transact (the replica IS this conn, so this is
                  ;; also the sync), then answer with the head
                  (fn [] (go (a/<! release)
                             (d/transact conn [{:entity/uuid block-uuid :block/content "<p>new</p>"}])
                             {:block-uuid block-uuid :max-tx (:max-tx @conn)}))
                  {})]
      (is (= "<p>new</p>" (content (opt/db overlay) block-uuid)) "visible before the server answers")
      (is (= "<p>old</p>" (content @conn block-uuid)) "and not yet durable")
      (>!! release true)
      (is (= block-uuid (:block-uuid (<!! result))))
      (is (not= ::none (await-status statuses #{:abandoned :reconciled})))
      (is (zero? (pending overlay)) "nothing left pending")
      (is (= "<p>new</p>" (content @conn block-uuid) (content (opt/db overlay) block-uuid)))
      (opt/close! overlay)))

  (testing "the reply lands BEFORE the sync: kept until the replica reaches the head"
    (let [conn (seed-page! (fresh-kb))
          overlay (opt/open conn)
          statuses (status-chan overlay)
          head (inc (:max-tx @conn))
          result (ow/predict-remote!
                  overlay [{:entity/uuid block-uuid :block/content "<p>new</p>"}]
                  (fn [] (go {:block-uuid block-uuid :max-tx head}))
                  {})]
      (is (some? (<!! result)))
      (is (not= ::none (await-status statuses #{:acknowledged})))
      (is (= 1 (pending overlay)) "acknowledged, but the replica does not hold it yet")
      (is (= "<p>new</p>" (content (opt/db overlay) block-uuid)))
      ;; the sync arrives
      (d/transact conn [{:entity/uuid block-uuid :block/content "<p>new</p>"}])
      (is (= head (:max-tx @conn)))
      (is (not= ::none (await-status statuses #{:reconciled})))
      (is (zero? (pending overlay)))
      (opt/close! overlay))))

(deftest a-refused-write-is-rejected-and-reported
  (let [conn (seed-page! (fresh-kb))
        overlay (opt/open conn)
        statuses (status-chan overlay)
        reported (a/promise-chan)
        refusal (ex-info "Remote invocation failed"
                         {:error "{:error \"not authorized\", :type :not-authorized}"})
        result (ow/predict-remote!
                overlay [{:entity/uuid block-uuid :block/content "<p>mine</p>"}]
                (fn [] (go refusal))
                {:on-failure #(a/put! reported %)})]
    (is (nil? (<!! result)) "a failure yields nil")
    (is (= refusal (<!! reported)) "and is handed to the caller to show")
    (is (not= ::none (await-status statuses #{:rejected})))
    (is (zero? (pending overlay)) "a refused write does not stay pending")
    (is (= "<p>old</p>" (content (opt/db overlay) block-uuid)) "the view returns to the server's text")
    (opt/close! overlay)))

(deftest an-answer-without-a-head-is-not-an-acknowledgement
  ;; the old handlers answered with a bare uuid; a write is acknowledged only
  ;; by a head the replica can be checked against
  (let [conn (seed-page! (fresh-kb))
        overlay (opt/open conn)
        reported (a/promise-chan)
        result (ow/predict-remote!
                overlay [{:entity/uuid block-uuid :block/content "<p>x</p>"}]
                (fn [] (go block-uuid))
                {:on-failure #(a/put! reported (or % ::nil))})]
    (is (nil? (<!! result)))
    (is (some? (<!! reported)))
    (is (zero? (pending overlay)))
    (opt/close! overlay)))

(deftest the-writes-of-one-overlay-are-sent-in-order
  ;; Enter creates a block the next save edits; Backspace deletes one whose
  ;; editor blurs with a last save. The server runs calls concurrently, so the
  ;; second RPC must not leave before the first has answered.
  (let [conn (seed-page! (fresh-kb))
        overlay (opt/open conn)
        log (atom [])
        release-first (a/promise-chan)
        ;; the first RPC leaves on a go thread: wait for it, not for a clock
        first-sent (a/promise-chan)
        r1 (ow/predict-remote! overlay [{:entity/uuid block-uuid :block/content "<p>1</p>"}]
                               (fn [] (swap! log conj :sent-1) (a/put! first-sent true)
                                 (go (a/<! release-first)
                                     (d/transact conn [{:entity/uuid block-uuid :block/content "<p>1</p>"}])
                                     (swap! log conj :answered-1)
                                     {:max-tx (:max-tx @conn)}))
                               {})
        r2 (ow/predict-remote! overlay [{:entity/uuid block-uuid :block/content "<p>2</p>"}]
                               (fn [] (swap! log conj :sent-2)
                                 (go (d/transact conn [{:entity/uuid block-uuid :block/content "<p>2</p>"}])
                                     {:max-tx (:max-tx @conn)}))
                               {})]
    (is (= "<p>2</p>" (content (opt/db overlay) block-uuid)) "both show at once, in order")
    (<!! first-sent)
    (is (= [:sent-1] @log) "the second waits for the first")
    (>!! release-first true)
    (is (some? (<!! r1)))
    (is (some? (<!! r2)))
    (is (= [:sent-1 :answered-1 :sent-2] @log))
    (is (= "<p>2</p>" (content @conn block-uuid)) "the later write wins on the server too")
    (opt/close! overlay)))

(deftest an-unpredicted-write-queues-with-the-predicted-ones
  ;; A widget declares an attribute (not predicted: the replica cannot apply
  ;; data whose type it does not know) and writes data under it in the same
  ;; click. The declaration must reach the server first, and a refusal of it
  ;; must be reported like any other.
  (let [conn (seed-page! (fresh-kb))
        overlay (opt/open conn)
        log (atom [])
        release (a/promise-chan)
        schema-sent (a/promise-chan)
        failures (atom [])
        r1 (ow/send-in-order! overlay
                              (fn [] (swap! log conj :schema-sent) (a/put! schema-sent true)
                                (go (a/<! release) (swap! log conj :schema-answered)
                                    {:max-tx (:max-tx @conn)}))
                              {})
        r2 (ow/predict-remote! overlay [{:entity/uuid block-uuid :block/content "<p>w</p>"}]
                               (fn [] (swap! log conj :data-sent)
                                 (go (d/transact conn [{:entity/uuid block-uuid :block/content "<p>w</p>"}])
                                     {:max-tx (:max-tx @conn)}))
                               {})]
    (<!! schema-sent)
    (is (= [:schema-sent] @log) "the data write waits for the declaration")
    (>!! release true)
    (is (some? (<!! r1)))
    (is (some? (<!! r2)))
    (is (= [:schema-sent :schema-answered :data-sent] @log))
    (testing "a refused declaration is reported, and yields nil"
      (is (nil? (<!! (ow/send-in-order! overlay
                                        (fn [] (go (ex-info "not authorized" {:type :not-authorized})))
                                        {:on-failure #(swap! failures conj %)}))))
      (is (= 1 (count @failures)))
      (is (nil? (<!! (ow/send-in-order! overlay (fn [] (go {:no "head"}))
                                        {:on-failure #(swap! failures conj %)})))
          "an answer without a head is not an acknowledgement")
      (is (= 2 (count @failures))))
    (opt/close! overlay)))

;; =============================================================================
;; The server half, through the network-inbound path
;; =============================================================================

(defn- with-kb-stores
  "Run `f` with `world`'s KB scopes resolving to fresh in-memory stores, as
   `block_remote`'s `resolve-conn` would resolve them."
  [w f]
  (let [conns {(:kb-granted-scope w) (seed-page! (fresh-kb))
               (:kb-private-scope w) (seed-page! (fresh-kb))}]
    (with-redefs [br/resolve-conn (fn [scope & _] (get conns scope))]
      (f conns))))

(deftest a-member-who-may-write-writes-and-gets-the-head
  (world/with-world [_ w]
    (with-kb-stores w
      (fn [conns]
        (let [conn (get conns (:kb-granted-scope w))
              member (world/principal w :member)]
          (testing "update-block-content"
            (let [r (world/inbound! member "update-block-content"
                                    {:block-uuid block-uuid :content "<p>member</p>"
                                     :db-scope (:kb-granted-scope w)})]
              (is (nil? (:error r)) (str (:error r)))
              (is (= "<p>member</p>" (content @conn block-uuid)))
              (is (= (:max-tx @conn) (get-in r [:result :max-tx])) "the head after the write")))
          (testing "create-sibling-after keeps the uuid the browser already shows"
            (let [new-uuid (random-uuid)
                  r (world/inbound! member "create-sibling-after"
                                    {:block-uuid block-uuid :content "" :new-uuid new-uuid
                                     :db-scope (:kb-granted-scope w) :branch-kw :db})]
              (is (= new-uuid (get-in r [:result :block-uuid])) (str (:error r)))
              (is (= page-uuid (-> (d/entity @conn [:entity/uuid new-uuid]) :block/parent :entity/uuid)))
              (is (= (:max-tx @conn) (get-in r [:result :max-tx])))))
          (testing "create-sibling-after refuses a uuid that names something (an upsert, not a create)"
            (let [before (d/pull @conn '[*] [:entity/uuid page-uuid])
                  r (world/inbound! member "create-sibling-after"
                                    {:block-uuid block-uuid :content "<p>hijack</p>"
                                     :new-uuid page-uuid
                                     :db-scope (:kb-granted-scope w)})]
              (is (some? (:error r)))
              (is (not (world/refused? r)) "the handler refused, not the gate")
              (is (= before (d/pull @conn '[*] [:entity/uuid page-uuid])) "the page is untouched")))
          (testing "ensure-page creates the first block under the browser's uuid"
            (let [p (random-uuid) first-block (random-uuid)
                  r (world/inbound! member "ensure-page"
                                    {:page-uuid p :title "New Page" :first-block-uuid first-block
                                     :db-scope (:kb-granted-scope w)})]
              (is (= :created (get-in r [:result :status])) (str (:error r)))
              (is (= p (-> (d/entity @conn [:entity/uuid first-block]) :block/parent :entity/uuid)))
              (is (= (:max-tx @conn) (get-in r [:result :max-tx])))))
          (testing "update-block-collapsed and delete-block answer with the head"
            (let [r (world/inbound! member "update-block-collapsed"
                                    {:block-uuid block-uuid :collapsed true
                                     :db-scope (:kb-granted-scope w)})]
              (is (= (:max-tx @conn) (get-in r [:result :max-tx])) (str (:error r))))
            (let [r (world/inbound! member "delete-block"
                                    {:block-uuid block-uuid :cascade? true
                                     :db-scope (:kb-granted-scope w)})]
              (is (= (:max-tx @conn) (get-in r [:result :max-tx])) (str (:error r)))
              (is (nil? (d/q '[:find ?e . :in $ ?u :where [?e :entity/uuid ?u]] @conn block-uuid))))
            (testing "a late save of the deleted block does not bring it back"
              (let [r (world/inbound! member "update-block-content"
                                      {:block-uuid block-uuid :content "<p></p>"
                                       :db-scope (:kb-granted-scope w)})]
                (is (some? (:error r)))
                (is (nil? (d/q '[:find ?e . :in $ ?u :where [?e :entity/uuid ?u]] @conn block-uuid))
                    "no parentless fragment was upserted")))))))))

(deftest a-reader-is-refused-and-nothing-changes
  (world/with-world [sys w]
    ;; the member READS the owner's Archive through a :read grant to the room
    (d/transact sys [{:grant/id (random-uuid) :grant/room [:room/id (:room w)]
                      :grant/system [:system/id (:kb-private-sys w)]
                      :grant/permission :read :grant/created (java.util.Date.)}])
    (with-kb-stores w
      (fn [conns]
        (let [conn (get conns (:kb-private-scope w))
              before (:max-tx @conn)]
          (doseq [[fn-name args] [["update-block-content" {:block-uuid block-uuid :content "<p>reader</p>"}]
                                  ["create-sibling-after" {:block-uuid block-uuid :content "" :new-uuid (random-uuid)}]
                                  ["delete-block" {:block-uuid block-uuid :cascade? true}]]]
            (testing fn-name
              (is (world/refused? (world/inbound! (world/principal w :member) fn-name
                                                  (assoc args :db-scope (:kb-private-scope w)))))))
          (is (= before (:max-tx @conn)) "no write landed")
          (is (= "<p>old</p>" (content @conn block-uuid)))
          (testing "and its owner may"
            (is (nil? (:error (world/inbound! (world/principal w :owner) "update-block-content"
                                              {:block-uuid block-uuid :content "<p>owner</p>"
                                               :db-scope (:kb-private-scope w)}))))
            (is (= "<p>owner</p>" (content @conn block-uuid)))))))))
