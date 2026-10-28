(ns is.simm.agents.knowledge-target-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [sci.core :as sci]
            [is.simm.agents.campaign-test :as fixture]
            [is.simm.agents.room-agents :as ra]
            [is.simm.model.system-db :as sdb]
            [is.simm.model.access :as access]
            [is.simm.runtimes.branching :as branching]
            [is.simm.runtimes.context :as ctx]))

(defn- page-id [conn title]
  (d/q '[:find ?u . :in $ ?t :where [?e :S.Page/title ?t] [?e :entity/uuid ?u]] @conn title))

(deftest knowledge-tool-and-sci-share-a-recovered-proposal-target
  (ctx/with-server-context
    (let [system (#'fixture/fresh-system-db)]
      (with-redefs [sdb/get-conn (fn [] system)]
        (let [[scope trunk] (#'fixture/fresh-kb)
              {:keys [room kb-conns] [agent] :agents} (#'fixture/team! system scope)
              overlay-fn #(#'ra/active-overlay room agent)
              cell (#'ra/private-overlay room agent)
              tool (#'ra/wrap-knowledge-add-tool {:execute (fn [& _] (throw (Exception. "Unexpected fallback")))} kb-conns agent overlay-fn)
              execute #(get-in tool [:execute])
              call! (fn [title] ((execute) {:title title :summary "A finding"} {}))
              interpreter (#'fixture/agent-sandbox kb-conns room agent)]
          (testing "tool created before the proposal follows the current target"
            (reset! cell {:title "Research"})
            (sci/eval-string* interpreter "(kb/ensure-page! \"Handbook\" \"Shared page\")")
            (let [branch (get-in @cell [:forks [scope agent]])
                  conn (branching/get-kb-conn-on-branch scope branch)
                  uuid (page-id conn "Shared page")]
              (call! "Shared page")
              (is (= uuid (page-id conn "Shared page")))
              (is (nil? (page-id trunk "Shared page")))
              (testing "portable descriptor recovery and a fresh tool retain branch identity"
                (let [saved (clojure.edn/read-string (pr-str @cell))]
                  (reset! cell nil)
                  (reset! cell saved)
                  (let [new-tool (#'ra/wrap-knowledge-add-tool {} kb-conns agent overlay-fn)]
                    ((:execute new-tool) {:title "After recovery" :summary "Retained"} {}))
                  (is (some? (page-id conn "After recovery")))
                  (is (nil? (page-id trunk "After recovery")))
                  (is (= branch (get-in @cell [:forks [scope agent]])))))))
          (testing "unknown explicit KB never defaults to the first target"
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown writable"
                                  ((:execute tool) {:kb "Typo" :title "Wrong target"} {})))
            (is (nil? (page-id trunk "Wrong target"))))
          (testing "filed proposals refuse tools, not silently write trunk"
            (reset! cell {:closed true :title "Research"})
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"filed" (call! "Must refuse")))
            (is (nil? (page-id trunk "Must refuse"))))
          (testing "explicitly released work writes trunk"
            (reset! cell nil)
            (call! "Direct finding")
            (is (some? (page-id trunk "Direct finding"))))
          (testing "a tool follows campaign membership selected after tool installation"
            (let [campaign (#'ra/campaign-overlay room)]
              (reset! campaign {:title "Joint research" :members #{agent}})
              (call! "Campaign finding")
              (let [branch (get-in @campaign [:forks [scope agent]])]
                (is (some? branch))
                (is (some? (page-id (branching/get-kb-conn-on-branch scope branch) "Campaign finding")))
                (is (nil? (page-id trunk "Campaign finding"))))
              (reset! campaign nil)))
          (testing "authorization is checked again on each call"
            (with-redefs [access/require-sandbox-kb! (fn [& _] (throw (ex-info "revoked" {})))]
              (is (thrown-with-msg? clojure.lang.ExceptionInfo #"revoked" (call! "Forbidden"))))
            (is (nil? (page-id trunk "Forbidden")))))))))

(deftest missing-governed-target-never-falls-through
  (ctx/with-server-context
   (let [cell (atom {:title "Research"})
        target {:db-scope (random-uuid) :conn :captured-trunk}]
    (with-redefs-fn {#'access/require-sandbox-kb! (fn [& _])
                    #'ra/overlay-branch! (fn [& _] nil)
                    #'branching/get-kb-conn (fn [& _] (throw (Exception. "Must not resolve trunk")))}
      #(is (thrown-with-msg? clojure.lang.ExceptionInfo #"could not be created"
                            (#'ra/resolve-kb-conn! target (constantly cell) (random-uuid) :write)))))))

(deftest absent-server-target-refuses-captured-connection
  (ctx/with-server-context
    (with-redefs [access/require-sandbox-kb! (fn [& _])
                  branching/get-kb-conn (fn [& _] nil)]
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"unavailable"
                            (#'ra/resolve-kb-conn! {:db-scope (random-uuid) :conn :captured-trunk}
                                                   (constantly (atom nil)) (random-uuid) :write))))))

(deftest unrelated-world-cannot-borrow-root-product-kb
  (require '[org.replikativ.spindel.engine.context :as execution]
           '[org.replikativ.spindel.engine.core :as engine])
  (let [world ((requiring-resolve 'org.replikativ.spindel.engine.context/create-execution-context))]
    (try
      (with-redefs [access/require-sandbox-kb! (fn [& _])
                    ra/live-room (fn [_] nil)]
        (with-bindings {(requiring-resolve 'org.replikativ.spindel.engine.core/*execution-context*) world}
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"owning room"
                                (#'ra/resolve-kb-conn! {:db-scope (random-uuid) :conn :captured-trunk}
                                                       (constantly (atom nil)) (random-uuid) :write)))))
      (finally ((requiring-resolve 'org.replikativ.spindel.engine.context/close-context!) world)))))

(deftest owning-room-can-resolve-server-product-kb-without-minting-on-read
  (require '[dvergr.discourse :as discourse])
  (ctx/with-server-context
    (let [system (#'fixture/fresh-system-db)]
      (with-redefs [sdb/get-conn (fn [] system)]
        (let [[scope trunk] (#'fixture/fresh-kb)
              {:keys [room kb-conns] [agent] :agents} (#'fixture/team! system scope)
              owner-context ((requiring-resolve 'org.replikativ.spindel.engine.context/create-execution-context))
              actual-room ((requiring-resolve 'dvergr.discourse/room) :routing-test owner-context)
              overlay (atom {:title "Unwritten proposal"})]
          (try
            (with-redefs [ra/live-room (fn [id] (when (= id room) actual-room))]
              (with-bindings {(requiring-resolve 'org.replikativ.spindel.engine.core/*execution-context*) owner-context}
                (is (nil? (branching/get-kb-conn scope)) "Product KB is absent from the room context")
                (is (identical? trunk (#'ra/resolve-kb-conn! (first kb-conns) (constantly overlay) agent :read)))
                (is (nil? (:forks @overlay)) "Reading a fresh proposal never mints a branch")
                (is (some? (#'ra/resolve-kb-conn! (first kb-conns) (constantly overlay) agent :write)))
                (is (some? (get-in @overlay [:forks [scope agent]])))))
            (finally ((requiring-resolve 'dvergr.discourse/close-room!) actual-room))))))))
