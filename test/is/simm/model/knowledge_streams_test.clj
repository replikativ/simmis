(ns is.simm.model.knowledge-streams-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [is.simm.model.knowledge-streams :as streams]))

(defn- fixture [f]
  (let [config {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write
                :keep-history? false :commit-graph? true :max-string-length 0
                :max-bytes-length 0 :max-tuple-string-length 0}]
    (d/create-database config)
    (let [conn (d/connect config)]
      (try (d/transact conn streams/schema)
           (f conn (streams/descriptor {:by (random-uuid) :room (random-uuid) :target (random-uuid)
                                        :policy (random-uuid) :program (random-uuid) :epoch (random-uuid)}))
           (finally (d/release conn) (d/delete-database config))))))

(def file-a {:role :trigger :content #uuid "2a345d11-215b-4338-8bfc-aa0519ec3c31" :size 10 :mode 33188})
(def file-b (assoc file-a :content #uuid "7d33d39a-8425-44da-b8bd-cba4d75ec203" :size 20))
(def basis {:repository #uuid "4b9c8eb4-8018-40f1-b270-9debc6dbb569"
            :store #uuid "bb00c12f-7eb3-4d38-ae24-f690381dc5cd" :backend :memory
            :branch :db :ref "refs/heads/main" :commit #uuid "113036be-4dd2-4b32-930f-6879a5defe2e"
            :configuration #uuid "468d4730-4b8a-4076-bce8-35d4c9026460" :datoms 50 :max-tx 100})
(def initial {:before nil :after basis
              :changes [{:path "inbox/a.md" :kind :added :before nil :after file-a}
                        {:path "wiki/context.md" :kind :added :before nil :after (assoc file-a :role :context)}]})
(defn- later [before changes]
  {:before before :after (assoc before :commit (random-uuid) :max-tx (inc (:max-tx before))) :changes changes})
(defn- read-items [conn descriptor]
  (streams/items @conn (:id descriptor)))

(deftest descriptor-excludes-execution-settings-and-requires-explicit-revisit-epoch
  (let [fields {:by (random-uuid) :room (random-uuid) :target (random-uuid)
                :policy (random-uuid) :program (random-uuid) :epoch (random-uuid)}]
    (is (= (streams/descriptor fields) (streams/descriptor fields)))
    (is (not= (:id (streams/descriptor fields)) (:id (streams/descriptor (assoc fields :epoch (random-uuid))))))
    (is (thrown? clojure.lang.ExceptionInfo (streams/descriptor (assoc fields :model "example"))))))

(deftest discovery-retains-exact-plan-and-enqueues-only-triggers
  (fixture
   (fn [conn value]
     (let [root (random-uuid) id (streams/discover! conn value nil initial root)
           row (streams/stream @conn (:id value))
           event (d/pull @conn '[*] [:knowledge.discovery/id id])]
       (is (= 1 (:knowledge.stream/revision row)))
       (is (= root (:knowledge.stream/cursor-root row)))
       (is (= (:commit basis) (:knowledge.stream/cursor-commit row)))
       (is (= initial (artifact/get-value (artifact/datahike-store conn) (:knowledge.discovery/payload event))))
       (is (= ["inbox/a.md"] (mapv :knowledge.item/path (read-items conn value))))
       (is (= [:pending] (mapv :knowledge.item/state (read-items conn value))))
       (is (= id (streams/discover! conn value nil initial root)))
       (is (= row (streams/stream @conn (:id value))))
       (is (= 1 (count (read-items conn value))))
       (is (thrown? Exception (streams/discover! conn value nil initial (random-uuid))))))))

(deftest edits-and-deletions-supersede-unclaimed-items-with-stable-historical-identities
  (fixture
   (fn [conn value]
     (streams/discover! conn value nil initial (random-uuid))
     (let [first-row (streams/stream @conn (:id value))
           second-plan (later basis [{:path "inbox/a.md" :kind :modified :before file-a :after file-b}])]
       (streams/discover! conn value first-row second-plan (random-uuid))
       (is (= [:superseded :pending] (mapv :knowledge.item/state (read-items conn value))))
       (let [second-row (streams/stream @conn (:id value))
             deleted (later (:after second-plan) [{:path "inbox/a.md" :kind :removed :before file-b :after nil}])]
         (streams/discover! conn value second-row deleted (random-uuid))
         (is (= [:superseded :removed] (mapv :knowledge.item/state (read-items conn value))))
         (let [third-row (streams/stream @conn (:id value))
               restored (later (:after deleted) [{:path "inbox/a.md" :kind :added :before nil :after file-b}])]
           (streams/discover! conn value third-row restored (random-uuid))
           (is (= 2 (count (read-items conn value))))
           (is (= [:superseded :pending] (mapv :knowledge.item/state (read-items conn value))))
           (is (= 3 (:knowledge.stream/next-order (streams/stream @conn (:id value)))))))))))

(deftest processed-and-claimed-work-is-never-automatically-marked-or-requeued
  (fixture
   (fn [conn value]
     (streams/discover! conn value nil initial (random-uuid))
     (let [iid (streams/item-id (:id value) "inbox/a.md" file-a)
           ;; Fixture stands in for a future certified filing transition. This
           ;; test does not establish that this state change is a valid filing.
           _ (d/transact conn [{:db/id [:knowledge.item/id iid] :knowledge.item/state :processed}])
           row (streams/stream @conn (:id value))
           changed (later basis [{:path "inbox/a.md" :kind :modified :before file-a :after file-b}])]
       (streams/discover! conn value row changed (random-uuid))
       (let [again (later (:after changed) [{:path "inbox/a.md" :kind :modified :before file-b :after file-a}])]
         (streams/discover! conn value (streams/stream @conn (:id value)) again (random-uuid))
         (is (= [:processed :superseded] (mapv :knowledge.item/state (read-items conn value)))))))))

(deftest active-batch-protects-the-old-discovery-pin-until-input-retention-and-filing
  (fixture
   (fn [conn value]
     (streams/discover! conn value nil initial (random-uuid))
     (d/transact conn [{:db/id [:knowledge.stream/id (:id value)] :knowledge.stream/active-batch (random-uuid)}])
     (let [row (streams/stream @conn (:id value)) next-plan (later basis [])]
       (is (thrown? Exception (streams/discover! conn value row next-plan (random-uuid))))
       (is (= row (streams/stream @conn (:id value))))))))

(deftest lost-ack-is-reconciled-by-exact-journal-replay-without-duplicate-items
  (fixture
   (fn [conn value]
     (let [root (random-uuid) transact d/transact]
       (with-redefs [d/transact (fn [& args] (apply transact args) (throw (ex-info "Lost ACK" {})))]
         (is (thrown? Exception (streams/discover! conn value nil initial root))))
       (let [row (streams/stream @conn (:id value))]
         (is (= 1 (:knowledge.stream/revision row)))
         (streams/discover! conn value nil initial root)
         (is (= row (streams/stream @conn (:id value))))
         (is (= 1 (count (read-items conn value)))))))))

(deftest concurrent-discovery-can-advance-the-cursor-only-once
  (fixture
   (fn [conn value]
     (let [gate (promise)
           invoke (fn [] @gate (try {:id (streams/discover! conn value nil initial (random-uuid))}
                                   (catch Exception _ {:conflict true})))
           a (future (invoke)) b (future (invoke))]
       (deliver gate true)
       (let [results [@a @b]]
         (is (= 1 (count (filter :id results))))
         (is (= 1 (count (filter :conflict results))))
         (is (= 1 (:knowledge.stream/revision (streams/stream @conn (:id value)))))
         (is (= 1 (count (read-items conn value)))))))))

(deftest physical-source-identity-is-bound-through-the-cursor-cas
  (fixture
   (fn [conn value]
     (streams/discover! conn value nil initial (random-uuid))
     (let [row (streams/stream @conn (:id value))]
       (doseq [[field replacement] [[:repository (random-uuid)] [:store (random-uuid)]
                                    [:ref "refs/heads/other"] [:branch :other]
                                    [:backend :file] [:configuration (random-uuid)]]]
         ;; Alter BOTH bases, retaining exactly the cursor's commit and tx.
         ;; Merely comparing before/after to each other would accept this.
         (let [forged (-> (later basis [])
                          (assoc-in [:before field] replacement)
                          (assoc-in [:after field] replacement))]
           (is (thrown? Exception (streams/discover! conn value row forged (random-uuid))))))
       (is (= row (streams/stream @conn (:id value))))))))

(deftest malformed-plans-and-journal-state-cannot-advance-discovery
  (fixture
   (fn [conn value]
     (doseq [plan [(assoc-in initial [:after :max-tx] -1)
                   (update initial :after dissoc :configuration)
                   (assoc-in initial [:changes 0 :path] "inbox/../escape.md")]]
       (is (thrown? Exception (streams/discover! conn value nil plan (random-uuid)))))
     (streams/discover! conn value nil initial (random-uuid))
     (let [iid (streams/item-id (:id value) "inbox/a.md" file-a)]
       (d/transact conn [{:db/id [:knowledge.item/id iid] :knowledge.item/state :claimed}])
       (let [row (streams/stream @conn (:id value))]
         (is (thrown? Exception
                      (streams/discover! conn value row (later basis [{:path "inbox/a.md" :kind :modified
                                                                      :before file-a :after file-b}]) (random-uuid))))
         (is (= row (streams/stream @conn (:id value)))))
       (d/transact conn [{:db/id [:knowledge.item/id iid] :knowledge.item/state :unknown}])
       (is (thrown? Exception
                    (streams/discover! conn value (streams/stream @conn (:id value))
                                      (later basis [{:path "inbox/a.md" :kind :added :before nil :after file-a}]) (random-uuid))))
       (d/transact conn [{:db/id [:knowledge.stream/id (:id value)] :knowledge.stream/next-order -1}])
       (is (thrown? Exception
                    (streams/discover! conn value (streams/stream @conn (:id value)) (later basis []) (random-uuid))))))))

(deftest pending-selection-is-independent-of-the-historical-audit-limit
  (fixture
   (fn [conn value]
     (streams/discover! conn value nil initial (random-uuid))
     (d/transact conn
       (mapv (fn [n] {:knowledge.item/id (random-uuid) :knowledge.item/stream (:id value)
                      :knowledge.item/path (str "inbox/history-" n ".md") :knowledge.item/content (random-uuid)
                      :knowledge.item/size 1 :knowledge.item/mode 33188 :knowledge.item/order n
                      :knowledge.item/state :processed :knowledge.item/discovery (random-uuid)}) (range 5)))
     (with-redefs [streams/max-changes 3]
       (is (thrown? Exception (streams/items @conn (:id value))))
       (is (= ["inbox/a.md"] (mapv :knowledge.item/path (streams/pending-items @conn (:id value)))))))))

(deftest historical-replay-is-exact-and-never-rewinds-a-newer-cursor
  (fixture
   (fn [conn value]
     (let [root (random-uuid) first-id (streams/discover! conn value nil initial root)
           first-row (streams/stream @conn (:id value))
           next-plan (later basis [{:path "inbox/a.md" :kind :modified :before file-a :after file-b}])
           next-root (random-uuid)]
       (streams/discover! conn value first-row next-plan next-root)
       (let [newer (streams/stream @conn (:id value))]
         (is (= first-id (streams/discover! conn value nil initial root)))
         (is (= newer (streams/stream @conn (:id value))))
         (is (thrown? Exception
                      (streams/discover! conn value (assoc first-row :knowledge.stream/next-order 999)
                                        next-plan next-root)))
         (is (= newer (streams/stream @conn (:id value)))))))))
