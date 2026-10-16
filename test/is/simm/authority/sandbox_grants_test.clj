(ns is.simm.authority.sandbox-grants-test
  "dvergr's own sandbox surface (`dvergr.room/db`, the guarded `d/connect`,
   `*room*`, `transact`) follows the room's grant since dvergr 0.1.286
   (dvergr#326), and asks the host what a grant permits. These run the
   attacks through simmis's room sandbox, built as simmis builds it: rooms
   provisioned and attached in the shared system DB, simmis's hook installed
   (`web/install-grant-permits!`), the rooms hydrated as at boot, the agent's
   working context from dvergr's room-context registry, and the code evaluated
   through `eval-in-room-server`, the handler of the `eval-in-room!` RPC.

   Room A is under attack. B's KB is attached to A `:read`, C's KB `:merge` —
   simmis's word, which dvergr's default does not know. Every attack asserts
   that it fails and the store is unchanged; next to it, the legitimate use of
   the same path works.

   Also: the hook and `can?` give the same answer for every grant permission
   and action, so dvergr and simmis cannot disagree about a grant."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [datahike.api :as d]
            [dvergr.agent.room-context :as room-ctx]
            [dvergr.orchestration.daemon :as daemon]
            [dvergr.rooms :as drooms]
            [dvergr.room.registry :as rreg]
            [dvergr.room.store :as rstore]
            [dvergr.substrate.paths :as paths]
            [dvergr.system.db :as sdb]
            [dvergr.system.rooms :as srooms]
            [is.simm.agents.room-agents :as ra]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]
            [is.simm.model.system-db :as system-db]
            [is.simm.runtimes.web :as web]
            [is.simm.uis.web.desktop.sandbox-remote :as sandbox-remote]
            [kontor.governance :as governance]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as rtc]))

;; =============================================================================
;; The hook agrees with can?
;; =============================================================================

(deftest the-grant-hook-is-access-ladder
  ;; A member of a room holding a grant with `permission` on a registry system
  ;; (a data DB, and a KB owned by someone else) may do exactly what the hook
  ;; tells dvergr the grant permits — for every permission simmis records and
  ;; one it does not know, and for every action. (A grant without a permission
  ;; is asked about `:write` only, the one action dvergr asks: dvergr's
  ;; `attach!` always records one, and `can?` reads a missing one as `:read`
  ;; on a KB and as no grant on any other system — neither permits a write.)
  (let [hook @@#'srooms/grant-permits-hook]  ; the fixture's, restored below
    (try
      (web/install-grant-permits!)
      (world/with-world [conn w]
        (doseq [[permission action] (concat (for [p (conj access/permission-order :bogus)
                                                  a [:read :write :merge :grant :admin]]
                                              [p a])
                                            [[nil :write]])
                kind [:data :kb]]
          (let [sys (random-uuid)
                kb (random-uuid)]
            (d/transact conn (cond-> [{:db/id "s" :system/id sys :system/type kind :system/name (str sys)}
                                      (cond-> {:grant/id (random-uuid) :grant/room [:room/id (:room w)]
                                               :grant/system "s" :grant/created (java.util.Date.)}
                                        permission (assoc :grant/permission permission))]
                               (= :kb kind) (conj {:kb/id kb :kb/name (str kb) :kb/owner (:non-member w)
                                                   :kb/db-scope (random-uuid) :kb/system-id sys
                                                   :kb/created (java.util.Date.)})))
            (is (= (access/can? @conn (world/principal w :member) action sys)
                   (srooms/grant-permits? permission action))
                (str kind " " permission " " action))))
        (testing "what dvergr's default would have said: :merge read-only"
          (srooms/set-grant-permits! nil)
          (is (false? (srooms/grant-permits? :merge :write)))
          (web/install-grant-permits!)
          (is (true? (srooms/grant-permits? :merge :write)))))
      (finally (srooms/set-grant-permits! hook)))))

;; =============================================================================
;; The sandbox
;; =============================================================================

(def ^:private world (atom nil))

(defn- delete-tree! [path]
  (when (.exists (io/file path))
    (run! io/delete-file (reverse (file-seq (io/file path))))))

(defn- kb-of [{:keys [room-id]}]
  (some #(when (= :kb (get-in % [:system :system/type])) (:system %))
        (sdb/systems-for-room room-id)))

(defn- with-rooms [f]
  (let [original-home (paths/home)
        original-conn @system-db/conn
        home (str (io/file (System/getProperty "java.io.tmpdir")
                           (str "simmis-sandbox-grants-" (random-uuid))))
        agent-uuid (random-uuid)
        ;; a root context of its own (the server's, in production): the live
        ;; rooms registered here go when it is closed
        root (daemon/create-shared-context :with-git? false)
        slug (let [n (subs (str (random-uuid)) 0 8)] #(str "sg-" % "-" n))]
    (try
      (paths/set-home! home)
      (sdb/reset-conn!)
      (srooms/clear-room-ctxs!)
      (reset! system-db/conn nil)
      (system-db/init!)
      (let [provision #(binding [rtc/*execution-context* root]
                         (srooms/provision-room! {:slug (slug %) :name (slug %)}))
            a (provision "a") b (provision "b") c (provision "c")
            b-kb (kb-of b) c-kb (kb-of c)]
        ;; attach as simmis does (`kbs/attach-kb-to-room!` is this call)
        (sdb/attach! (:room-id a) (:system/id b-kb) :read)
        (sdb/attach! (:room-id a) (:system/id c-kb) :merge)
        ;; the boot order of `web/start-server!`: the hook, then hydration
        (web/install-grant-permits!)
        (binding [rtc/*execution-context* root]
          (srooms/hydrate-rooms!)
          (drooms/hydrate-registry! root))
        (let [room (binding [rtc/*execution-context* root]
                     (rreg/lookup (rstore/slug->room-id (slug "a"))))]
          ;; the agent's working context, which is where the room's sandbox lives
          (room-ctx/ensure-ctx! room (ra/party->actor-kw agent-uuid) {:system-prompt "test"})
          (swap! @#'ra/joined conj [(:room-id a) agent-uuid])
          (reset! world {:root root :room room :a a :b b :c c :b-kb (:system/name b-kb) :c-kb (:system/name c-kb)
                         :a-msgs (some #(when (= :msgs (get-in % [:system :system/type]))
                                          (get-in % [:system :system/name]))
                                       (sdb/systems-for-room (:room-id a)))
                         :agent agent-uuid})
          (f)))
      (finally
        (srooms/set-grant-permits! nil)
        (when-let [{:keys [a b c agent room]} @world]
          (swap! @#'ra/joined disj [(:room-id a) agent])
          (room-ctx/drop-room! (:id room))
          (doseq [{:keys [room-id]} [a b c]]
            (try (binding [rtc/*execution-context* (srooms/room-ctx-for room-id)]
                   (some-> (srooms/room-msgs-conn room-id) governance/ungovern!))
                 (catch Throwable _))))
        (reset! world nil)
        (srooms/release-room-stores!)
        (sdb/reset-conn!)
        (reset! system-db/conn original-conn)
        (try (context/close-context! root) (catch Throwable _))
        (paths/set-home! original-home)
        (delete-tree! home)))))

(use-fixtures :once with-rooms)

(defn- ev
  "Evaluate `code` in room A's sandbox through the `eval-in-room!` handler, as
   its owner would: `{:ok \"=> …\"}` or `{:err \"Error: …\"}`. The root
   context is bound, as the RPC binds the server's."
  [code]
  (let [{:keys [a root]} @world
        {:keys [success result]}
        (binding [rtc/*execution-context* root]
          (sandbox-remote/eval-in-room-server
           (str (:room-id a))
           (str "(require '[datahike.api :as d] '[dvergr.room :as room]) " code)
           (random-uuid)))]
    (if success {:ok result} {:err result})))

(defn- refused? [r] (boolean (re-find #"read-only" (str (:err r)))))

(defn- in-room [k f]
  (binding [rtc/*execution-context* (srooms/room-ctx-for (get-in @world [k :room-id]))] (f)))

(defn- titles [k]
  (in-room k #(set (d/q '[:find [?t ...] :where [_ :entity/title ?t]]
                        @(srooms/room-kb-conn (get-in @world [k :room-id]))))))

(defn- message-contents []
  (in-room :a #(set (d/q '[:find [?c ...] :where [_ :message/content ?c]]
                         @(srooms/room-msgs-conn (get-in @world [:a :room-id]))))))

(deftest the-sandbox-has-a-context-to-run-in
  (is (= {:ok "=> 2"} (ev "(+ 1 1)")))
  (is (= {:ok "=> [false true]"}
         (ev (str "(mapv (fn [n] (:writable? (first (filter #(= n (:name %)) (room/databases)))))"
                  " [\"" (:b-kb @world) "\" \"" (:c-kb @world) "\"])")))
      "dvergr reports B's KB read-only and C's (:merge) writable"))

(deftest a-kb-granted-read-is-not-written-from-the-sandbox
  (let [kb (:b-kb @world)
        before (titles :b)]
    (doseq [[attack code]
            {:db (str "(d/transact (room/db \"" kb "\") [{:entity/title \"forged-db\"}])")
             :db-keyword (str "(d/transact (room/db :" kb ") [{:entity/title \"forged-kw\"}])")
             :transact! (str "@(d/transact! (room/db \"" kb "\") [{:entity/title \"forged-bang\"}])")
             :connect (str "(d/transact (d/connect {:store {:id \"" kb "\"}}) [{:entity/title \"forged-connect\"}])")
             :tx-map (str "(d/transact (room/db \"" kb "\") {:tx-data [{:entity/title \"forged-map\"}]})")
             :retract (str "(d/transact (room/db \"" kb "\") [[:db/retractEntity 1]])")}]
      (testing (name attack)
        (let [r (ev code)]
          (is (refused? r) (pr-str r)))))
    (is (= before (titles :b)) "B's KB is unchanged")
    (testing "legitimate: A reads it"
      (in-room :b #(d/transact (srooms/room-kb-conn (get-in @world [:b :room-id]))
                               [{:entity/title "b-note"}]))
      (is (= {:ok "=> true"}
             (ev (str "(contains? (set (d/q '[:find [?t ...] :where [_ :entity/title ?t]]"
                      " @(room/db \"" kb "\"))) \"b-note\")")))))))

(deftest the-room-message-store-is-not-written-from-the-sandbox
  (let [msgs (:a-msgs @world)
        before (message-contents)]
    (doseq [code ["(d/transact room/*room* [{:message/id (random-uuid) :message/content \"forged-room\"}])"
                  "@(d/transact! room/*room* [{:message/id (random-uuid) :message/content \"forged-bang\"}])"
                  (str "(d/transact (room/db \"" msgs "\") [{:message/id (random-uuid) :message/content \"forged-db\"}])")
                  (str "(d/transact (d/connect {:store {:id \"" msgs "\"}}) [{:message/id (random-uuid) :message/content \"forged-connect\"}])")]]
      (let [r (ev code)]
        (is (refused? r) (str code " " (pr-str r)))))
    (is (= before (message-contents)) "no message was written")
    (testing "legitimate: the sandbox reads it"
      (is (= {:ok (str "=> " (count before))}
             (ev "(count (d/q '[:find ?e :where [?e :message/content _]] @room/*room*))"))))))

(deftest a-kb-granted-merge-is-written-from-the-sandbox
  (let [kb (:c-kb @world)]
    (testing "by name and by connect"
      (is (= {:ok "=> 2"} (ev (str "(count (:tx-data (d/transact (room/db \"" kb "\") [{:entity/title \"c-by-db\"}])))"))))
      (is (= {:ok "=> 2"} (ev (str "(count (:tx-data (d/transact (d/connect {:store {:id \"" kb "\"}}) [{:entity/title \"c-by-connect\"}])))"))))
      (is (every? (titles :c) ["c-by-db" "c-by-connect"])))
    (testing "only because simmis says :merge writes: dvergr's default refuses it"
      (try
        (srooms/set-grant-permits! nil)
        (let [r (ev (str "(d/transact (room/db \"" kb "\") [{:entity/title \"c-no-hook\"}])"))]
          (is (refused? r) (pr-str r)))
        (is (not (contains? (titles :c) "c-no-hook")))
        (finally (web/install-grant-permits!))))
    (testing "and the room's own KB, as before"
      (is (= {:ok "=> 2"} (ev "(count (:tx-data (d/transact room/*kb* [{:entity/title \"a-own\"}])))")))
      (is (contains? (titles :a) "a-own")))))
