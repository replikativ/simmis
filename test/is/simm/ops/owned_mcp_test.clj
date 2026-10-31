(ns is.simm.ops.owned-mcp-test
  "Inactive host authority and rich binding over real EACL/native repositories.
   Upstream JSON-RPC gateway integration remains a separate dependency gate."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.ops :as dops]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.documents.files :as documents]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp :as mcp]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.owned-workspace-test :as fixture]
            [is.simm.ops.worker-credentials :as workers]
            [is.simm.runtimes.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- principal [w kind]
  (assoc (ops/party-principal (get w kind))
         :principal/mcp? true :principal/scopes #{:dvergr/read-only}
         :principal/tools #{"room_files" "room_read" "catalog_list"}))

(defn- request [handle]
  {:op :room/read :action :read :resource [:room handle] :resolved nil
   :args {:room handle :path "wiki/file.md"}})

(defn- allow? [host principal request]
  (:allow? ((get-in host [:config :auth :authorize]) principal request)))

(defn- read! [host principal handle]
  ((get-in host [:config :room-files :resolve-read]) principal
                                                     {:operation :read :room handle :path "wiki/file.md"}))

(defn- workspace-snapshot [conn]
  (let [db @conn]
    {:commit (d/commit-id db) :config (:config db) :worktree (query/worktree db)}))

(defn- with-fixture
  ([owned-first? f] (with-fixture owned-first? {} f))
  ([owned-first? options f]
  (let [configs (atom []) completed? (atom false)]
    (try
      (let [result
            (@#'fixture/with-fixture
             owned-first? options
             (fn [{:keys [conn w room owned attached] :as data}]
               ;; work-test/with-room owns its chat connection/store. This
               ;; wrapper owns only the control and two repository databases.
               (reset! configs (mapv #(-> % deref :config) [conn owned attached]))
               (let [hydrations (atom [])
                     hydrate (fn [id]
                               (swap! hydrations conj id)
                               (is (= (:room w) id) "Hydrate only the originally authorized canonical UUID")
                               room)]
                 (with-redefs [context/server-context (:ctx room)
                               rooms/live-room hydrate]
                   (f (assoc data :host (mcp/host conn (:ctx room))
                             :hydrations hydrations :hydrate hydrate))))))]
        (reset! completed? true)
        result)
      (finally
        ;; Only normal shared-fixture return acknowledges physical Room/store
        ;; cleanup. Preserve diagnostic stores if callback/teardown throws.
        (when @completed?
          (doseq [cfg @configs]
            (when-not (and (= :memory (get-in cfg [:store :backend]))
                           (uuid? (get-in cfg [:store :id])))
              (throw (ex-info "Refuse non-fixture cleanup" {})))
            (d/delete-database cfg))))))))

(deftest cold-uuid-and-slug-authority-never-hydrates-or-uses-selected-workspace
  (doseq [owned-first? [true false]]
    (with-fixture
      owned-first? {:system-schema :core}
      (fn [{:keys [conn w room host attached hydrations]}]
        (let [member (principal w :member)
              uuid (str (:room w)) slug (:room/slug (d/entity @conn [:room/id (:room w)]))
              selected (workspace/selected-system-id) other (workspace-snapshot attached)]
          (with-redefs [dops/resolve-room (fn [& _] nil)]
            (doseq [handle [uuid slug]]
              (is (nil? (dops/authorize! host member :room/read {:room handle :path "wiki/file.md"}))))
            (is (dops/visible? host member :room/read [:room uuid])))
          (is (empty? @hydrations) "Even positive cold-room authorization opens no target storage")
          (doseq [handle [uuid slug]]
            (binding [ec/*execution-context* nil]
              (let [binding (read! host member handle)]
                (is (= #{:db :target-stamp} (set (keys binding))))
                (is (uuid? (parse-uuid (:target-stamp binding))))
                (is (= "# Owned\n" (:text (documents/read-file-in (:db binding) "wiki/file.md")))))))
          (is (= [(:room w) (:room w)] @hydrations))
          (is (= selected (workspace/selected-system-id)))
          (is (= other (workspace-snapshot attached))))))))

(deftest visibility-is-absence-of-args-not-malformed-call-or-retargeted-resource
  (with-fixture
    true {:system-schema :core}
    (fn [{:keys [w host hydrations]}]
      (let [member (principal w :member) handle (str (:room w)) base (request handle)]
        (is (allow? host member (dissoc base :args)))
        (is (allow? host member (assoc base :resolved {:room "untrusted-selected-fork"})))
        (doseq [invalid [(assoc base :args nil) (assoc base :args {})
                         (assoc base :action :write) (assoc base :resource [:global])
                         (assoc base :resource [:room (str (:other-room w))])
                         (assoc base :resource [:room handle :extra])
                         (assoc-in base [:args :offset] -1)]]
          (is (false? (allow? host member invalid))))
        (is (empty? @hydrations))))))

(deftest invalid-identities-and-dtos-refuse-before-any-hydration
  (with-fixture
    true {:system-schema :core}
    (fn [{:keys [w host hydrations]}]
      (let [member (principal w :member) handle (str (:room w))
            resolve-read (get-in host [:config :room-files :resolve-read])
            base {:operation :read :room handle :path "wiki/file.md"}]
        (doseq [invalid [nil {} (dissoc member :principal/mcp?)
                         (assoc member :principal/id "party/not-a-uuid")
                         (assoc member :principal/id (str (:principal/id member) "/extra"))
                         (assoc member :principal/id nil) (assoc member :principal/kind nil)
                         (principal w :non-member) (principal w :team-agent)]]
          (is (false? (allow? host invalid (request handle))))
          (is (thrown? clojure.lang.ExceptionInfo (read! host invalid handle))))
        (doseq [invalid [(assoc member :principal/kind :worker)
                         (assoc member :principal/kind :agent)]]
          (is (thrown? clojure.lang.ExceptionInfo (read! host invalid handle))))
        (doseq [invalid [(assoc base :actor (:owner w)) (assoc base :conn :forged)
                         (assoc base :max 999999999) (assoc base :operation :write)
                         (assoc base :path "../outside") (assoc base :path ".env")
                         (assoc base :length 0) (assoc base :expected-target-stamp (random-uuid))]]
          (is (thrown? clojure.lang.ExceptionInfo (resolve-read member invalid))))
        (is (empty? @hydrations))))))

(deftest canonical-uuid-collisions-never-fall-back-but-ordinary-short-slugs-remain-slugs
  (with-fixture
    true {:system-schema :core}
    (fn [{:keys [conn w host hydrations]}]
      (let [member (principal w :member) unknown (str (random-uuid))]
        (d/transact conn [{:room/id (:other-room w) :room/slug unknown
                           :room/parties [[:party/id (:member w)]]}])
        (is (false? (allow? host member (request unknown))))
        (is (thrown? clojure.lang.ExceptionInfo (read! host member unknown)))
        (is (false? (allow? host member (request "malformed-unknown-uuid"))))
        (is (empty? @hydrations))
        (d/transact conn [{:room/id (:room w) :room/slug "1-1-1-1-1"}])
        (is (allow? host member (request "1-1-1-1-1"))
            "Java's permissive abbreviated UUID parser must not steal an ordinary canonical slug")
        (is (empty? @hydrations))))))

(deftest current-human-kind-and-revocation-are-rechecked-in-the-same-control-snapshot
  (with-fixture
    true {:system-schema :core}
    (fn [{:keys [conn w host hydrations]}]
      (let [member (principal w :member) handle (str (:room w))]
        (d/transact conn [[:db/add [:party/id (:member w)] :actor/kind :agent]])
        (is (false? (allow? host member (request handle))) "Actor kind wins over legacy human type")
        (is (thrown? clojure.lang.ExceptionInfo (read! host member handle)))
        (d/transact conn [[:db/retract [:party/id (:member w)] :actor/kind :agent]])
        (is (allow? host member (request handle)) "Legacy party/type fallback is explicit")
        (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
        (is (false? (allow? host member (request handle))))
        (is (thrown? clojure.lang.ExceptionInfo (read! host member handle)))
        (is (empty? @hydrations))
        (d/transact conn [[:db/add [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
        (is (= "# Owned\n" (:text (documents/read-file-in (:db (read! host member handle)) "wiki/file.md"))))))))

(deftest host-owner-and-drain-refusals-precede-hydration
  (with-fixture
    true {:system-schema :core}
    (fn [{:keys [w host hydrations]}]
      (let [member (principal w :member) handle (str (:room w))]
        (doseq [replace [#(with-redefs [system-db/get-conn (constantly nil)] (%))
                         #(with-redefs [context/server-context nil] (%))
                         #(with-redefs [simple/on-drain-thread? (constantly true)] (%))]]
          (replace #(do (is (false? (allow? host member (request handle))))
                        (is (thrown? clojure.lang.ExceptionInfo (read! host member handle))))))
        (is (empty? @hydrations))))))

(deftest control-mapping-and-authority-are-rechecked-after-fixed-hydration
  (doseq [change [:revoke :retarget :control-owner :context-owner]]
    (with-fixture
      true {:system-schema :core}
      (fn [{:keys [conn w room host hydrations hydrate]}]
        (let [member (principal w :member) slug (:room/slug (d/entity @conn [:room/id (:room w)]))
              replaced? (atom false)]
          (with-redefs [context/server-context (:ctx room)
                        rooms/live-room
                        (fn [id]
                          (let [room (hydrate id)]
                            (case change
                              :revoke (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                              :retarget (do (d/transact conn [{:room/id (:room w) :room/slug "renamed-original"}])
                                            (d/transact conn [{:room/id (:other-room w) :room/slug slug
                                                               :room/parties [[:party/id (:member w)]]}]))
                              :control-owner (reset! replaced? true)
                              ;; Model a replacement owner with an inert facade,
                              ;; not another engine/executor. with-redefs restores
                              ;; the fixture Var even after refusal/exception.
                              :context-owner (alter-var-root #'context/server-context
                                                             (constantly (assoc (:ctx room) :fixture/context-replacement true))))
                            room))
                        system-db/get-conn #(when-not @replaced? conn)
                        owned/resolve! (fn [& _] (throw (AssertionError. "Changed control authority reached target storage")))]
            (is (thrown? clojure.lang.ExceptionInfo (read! host member slug))))
          (is (= [(:room w)] @hydrations)))))))

(deftest default-replacement-and-aba-change-the-stamp-without-selected-fallback
  (with-fixture
    false {:system-schema :core}
    (fn [{:keys [conn w host attached attached-descriptor]}]
      (let [member (principal w :member) handle (str (:room w)) first (read! host member handle)
            selected (workspace/selected-system-id)]
        (repo/write! attached "wiki/file.md" (.getBytes "# Owned\n" "UTF-8"))
        (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
        (let [second (read! host member handle)]
          (is (= "# Owned\n" (:text (documents/read-file-in (:db second) "wiki/file.md"))))
          (is (not= (:target-stamp first) (:target-stamp second))))
        (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:repo-sys w)]}])
        (is (not= (:target-stamp first) (:target-stamp (read! host member handle))))
        (is (= selected (workspace/selected-system-id)))
        (is (= "# Owned\n" (:text (documents/read-file-in @attached "wiki/file.md"))))))))

(deftest global-catalog-and-worker-delegation-retain-original-authority
  (with-fixture
    true {:system-schema :core}
    (fn [{:keys [conn w host hydrations]}]
      (let [member (principal w :member)
            global {:op :catalog/list :action :read :resource [:global] :resolved nil :args {}}
            worker {:principal/kind :worker :principal/mcp? true :principal/id "worker/fixture"}
            request (request (str (:room w))) calls (atom [])]
        (is (= (ops/authorize member global) ((get-in host [:config :auth :authorize]) member global)))
        (is (false? (allow? host member (assoc global :action :write))))
        (with-redefs [workers/authorize (fn [actual principal request]
                                          (is (identical? conn actual))
                                          (swap! calls conj [principal request])
                                          {:allow? false :reason "Worker control remains separate"})]
          (is (= {:allow? false :reason "Worker control remains separate"}
                 ((get-in host [:config :auth :authorize]) worker request)))
          (is (= [[worker request]] @calls)))
        (is (empty? @hydrations))))))

(deftest extracted-dto-predicate-keeps-non-mcp-and-visibility-behavior
  (let [principal {:principal/mcp? true} base (request "a-room")]
    (is (ops/external-arguments-valid? principal base))
    (is (ops/external-arguments-valid? principal (dissoc base :args)))
    (is (false? (ops/external-arguments-valid? principal (assoc base :args nil))))
    (is (false? (ops/external-arguments-valid? principal (assoc-in base [:args :offset] -1))))
    (is (ops/external-arguments-valid? {} (assoc base :args nil)))))
