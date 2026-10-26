(ns is.simm.ops.owned-reads-test
  "Transport-neutral service over the tested canonical/native factory fixture.
   No transport authentication simulation, runtime activation or new grants."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.discourse :as discourse]
            [dvergr.ops :as dops]
            [dvergr.room.registry :as registry]
            [dvergr.system.rooms :as native-rooms]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.documents.files :as documents]
            [is.simm.authority.world :as world]
            [is.simm.model.system-db :as system-db]
            [is.simm.model.access :as access]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.runtimes.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- snapshot [conn]
  (let [db @conn]
    {:commit (d/commit-id db) :config (:config db) :worktree (query/worktree db)}))

(defn- request [room]
  {:op :room/read :action :read :resource [:room room]
   :args {:room room :path "wiki/file.md"}})

(deftest writer-observation-uses-its-own-authority-snapshot-without-source-access
  (@#'fixture/with-fixture
   true
   (fn [{:keys [host conn w room]}]
     (let [principal (ops/party-principal (:owner w)) handle (str (:room w))
           original (reads/resolve-host-room! host principal handle)
           revoked (:db-after (d/with @conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}]))
           forbidden (fn [& _] (throw (AssertionError. "Writer room observation must not resolve source or hydrate")))]
       (with-redefs [owned/resolve! forbidden workspace/workspace-for-system! forbidden rooms/live-room forbidden]
         (is (identical? room (:room (reads/validate-host-room-in! host principal handle original @conn))))
         (is (thrown? Exception (reads/validate-host-room-in! host principal handle original revoked)))
         (with-redefs [registry/lookup (constantly (assoc room :incarnation (random-uuid)))]
           (is (thrown? Exception (reads/validate-host-room-in! host principal handle original @conn)))))))))

(deftest ordinary-human-and-mcp-share-one-owned-service-with-separate-transport-gates
  (doseq [owned-first? [true false]]
    (@#'fixture/with-fixture
     owned-first?
     (fn [{:keys [conn w room host attached hydrations]}]
       (let [browser (ops/party-principal (:member w))
             mcp (assoc browser :principal/mcp? true)
             handle (str (:room w))
             slug (:room/slug (d/entity @conn [:room/id (:room w)]))
             authorize (get-in host [:config :auth :authorize])
             callback (get-in host [:config :room-files :resolve-read])
             selected (workspace/selected-system-id) other (snapshot attached)]
         (is (not (contains? browser :principal/mcp?)))
         (doseq [original [handle slug]]
           (is (= {:party (:member w) :room-id (:room w) :slug slug}
                  (reads/authorize-read! @conn browser original))))
         (is (empty? @hydrations) "Pure current authorization never opens storage")
         (is (false? (:allow? (authorize browser (request handle)))))
         (is (thrown? Exception (callback browser {:operation :read :room handle :path "wiki/file.md"})))
         (is (empty? @hydrations) "Browser never gains an invented MCP marker")
         (doseq [principal [browser mcp]]
           (binding [ec/*execution-context* nil
                     dops/*principal* {:principal/id "party/not-the-captured-user"}]
             (let [binding (reads/resolve-read! conn (:ctx room) principal handle)]
               (is (= (:room w) (:room-id binding)))
               (is (= (:member w) (:party binding)))
               (is (identical? room (:room binding)))
               (is (uuid? (:target-stamp binding)))
               (is (:control-db binding))
               (is (= "# Owned\n" (:text (documents/read-file-in (:db binding) "wiki/file.md")))))))
         (is (true? (:allow? (authorize mcp (request handle)))))
         (let [out (callback mcp {:operation :read :room handle :path "wiki/file.md"})]
           (is (= #{:db :target-stamp} (set (keys out))))
           (is (uuid? (parse-uuid (:target-stamp out)))))
         (is (= [(:room w) (:room w) (:room w)] @hydrations))
         (is (= selected (workspace/selected-system-id)))
         (is (= other (snapshot attached))))))))

(deftest workers-agents-local-and-noncanonical-human-identities-cannot-become-owner-reads
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room hydrations]}]
     (let [human (ops/party-principal (:member w)) handle (str (:room w))]
       (doseq [bad [(assoc human :principal/kind :worker :principal/mcp? true)
                    (assoc human :principal/kind :agent)
                    (assoc human :principal/kind :local)
                    (assoc human :principal/id (str ":party/" (:member w)))
                    (assoc human :principal/id (str "party/" (random-uuid)))]]
         (is (thrown? Exception (reads/authorize-read! @conn bad handle)))
         (is (thrown? Exception (reads/resolve-read! conn (:ctx room) bad handle))))
       (is (empty? @hydrations))))))

(deftest current-host-owner-and-physical-worker-guards-are-shared-not-transport-bypassed
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room attached host hydrations]}]
     (let [principal (ops/party-principal (:member w)) handle (str (:room w))]
       (doseq [[control host-ctx] [[attached (:ctx room)] [conn nil] [conn (Object.)]]]
         (is (thrown? Exception (reads/current-control-db! control host-ctx)))
         (is (thrown? Exception (reads/resolve-read! control host-ctx principal handle))))
       (with-redefs [simple/on-drain-thread? (constantly true)]
         (is (thrown? Exception (reads/current-control-db! conn (:ctx room))))
         (is (thrown? Exception (reads/resolve-read! conn (:ctx room) principal handle)))
         ;; Unrelated delegated ops retain the SAME pre-existing owner/drain
         ;; guard; extracting human reads must not skip it.
         (is (false? (:allow? ((get-in host [:config :auth :authorize])
                               principal {:op :room/runs :action :read :resource [:room handle]})))))
       (is (empty? @hydrations))))))

(deftest post-hydration-current-authority-and-room-mapping-still-refuse-stale-binding
  (doseq [mode [:revoke :mapping :context]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [conn w room hydrations]}]
       (let [principal (ops/party-principal (:member w)) handle (str (:room w))
             hydrate (fn [id]
                       (swap! hydrations conj id)
                       (case mode
                         :revoke (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties
                                                    [:party/id (:member w)]]])
                         :mapping (d/transact conn [[:db/add [:room/id (:room w)] :room/slug "Changed-during-hydration"]])
                         nil)
                       room)]
         (with-redefs [rooms/live-room hydrate]
           (if (= :context mode)
             (let [current! reads/current-control-db! lookups (atom 0)]
               (with-redefs [reads/current-control-db!
                             (fn [control host-ctx]
                               (if (= 2 (swap! lookups inc))
                                 ;; Only the second real owner guard observes
                                 ;; replacement; no persistent global mutation.
                                 (with-redefs [context/server-context (Object.)]
                                   (current! control host-ctx))
                                 (current! control host-ctx)))]
                 (is (thrown? Exception (reads/resolve-read! conn (:ctx room) principal handle)))
                 (is (= 1 (count @hydrations)))))
             ;; UUID remains canonical after a mere slug rename, so use the
             ;; original slug for the mapping-change refusal case.
             (let [original (if (= :mapping mode)
                              (:room/slug (d/entity @conn [:room/id (:room w)])) handle)]
               (is (thrown? Exception (reads/resolve-read! conn (:ctx room) principal original)))))))))))

(deftest rich-control-snapshot-is-exactly-the-final-owned-resolution-input
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room]}]
     (let [principal (ops/party-principal (:member w)) resolve! owned/resolve!
           observed (atom nil) handle (str (:room w))]
       (with-redefs [rooms/live-room (fn [_]
                                       (d/transact conn [{:room/id (:room w) :room/name "Final captured control"}])
                                       room)
                     owned/resolve! (fn [db party action id canonical]
                                      (reset! observed db)
                                      (is (= [principal :read (:room w)] [party action id]))
                                      (resolve! db party action id canonical))]
         (let [binding (reads/resolve-read! conn (:ctx room) principal handle)]
           (is (identical? @observed (:control-db binding)))
           (is (= "Final captured control"
                  (:room/name (d/entity (:control-db binding) [:room/id (:room w)]))))))))))

(deftest room-only-resolution-and-validation-ignore-missing-default-and-selected-files
  (doseq [order [true false]]
    (@#'fixture/with-fixture
     order
     (fn [{:keys [conn w host room owned attached hydrations]}]
       (let [p (ops/party-principal (:owner w)) handle (str (:room w))
             slug (:room/slug (d/entity @conn [:room/id (:room w)]))
             selected (workspace/selected-system-id) before (snapshot owned) other (snapshot attached)
             calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Room-only gate accessed workspace")))]
         (d/transact conn [[:db/retract [:room/id (:room w)] :room/default-repository [:system/id (:repo-sys w)]]])
         (with-redefs [owned/resolve! forbidden native-rooms/default-workspace forbidden
                       workspace/current-workspace forbidden documents/read-file-in forbidden]
           (doseq [original [handle slug]]
             (let [binding (reads/resolve-host-room! host p original)]
               (is (= #{:room :room-id :party :slug :control-db ::reads/room-owner} (set (keys binding))))
               (is (identical? room (:room binding)))
               (is (= [(:room w) (:owner w) slug] ((juxt :room-id :party :slug) binding)))
               (is (identical? conn (:control-conn (::reads/room-owner binding))))
               (with-redefs [rooms/live-room forbidden]
                 (let [validated (reads/validate-host-room! host p original binding)]
                   (is (identical? room (:room validated)))
                   (is (:control-db validated))))))
           (is (zero? @calls)))
         (is (= [(:room w) (:room w)] @hydrations))
         (is (= selected (workspace/selected-system-id)))
         (is (= before (snapshot owned))) (is (= other (snapshot attached))))))))

(deftest leased-installation-refuses-a-mapping-switch-before-target-io
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host room]}]
     (let [p (ops/party-principal (:owner w)) handle (str (:room w))
           resolved (reads/resolve-host-install-room! host p handle)
           leased-host (assoc host ::reads/install-room-binding resolved)
           calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "No new Room or target IO")))]
       (with-redefs [rooms/live-room forbidden owned/resolve! forbidden]
         (doseq [malformed [nil false {} (dissoc resolved :room)]]
           (is (thrown? Exception
                        (reads/resolve-host-install! (assoc host ::reads/install-room-binding malformed) p handle)))))
       (d/transact conn [{:room/id (:room w) :room/slug "switched-after-room-lease"}])
       (with-redefs [rooms/live-room forbidden owned/resolve! forbidden]
         (is (thrown? Exception (reads/resolve-host-install! leased-host p handle)))
         (is (thrown? Exception (reads/validate-host-install! leased-host p handle {:room room}))))
       (is (zero? @calls))))))

(deftest installation-room-gate-does-not-borrow-merge-permission
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host room hydrations]}]
     (let [principal (ops/party-principal (:owner w)) handle (str (:room w))
           can? access/can? actions (atom [])]
       (with-redefs [access/can? (fn [db party action resource]
                                 (swap! actions conj action)
                                 (and (not= :merge action) (can? db party action resource)))]
         (let [resolved (reads/resolve-host-install-room! host principal handle)]
           (is (identical? room (:room resolved)))
           (is (identical? room (:room (reads/validate-host-install-room! host principal handle resolved))))
           (is (not-any? #{:merge} @actions))
           (is (every? (set @actions) [:read :write :admin])))
         (let [before @hydrations]
           (is (thrown? Exception (reads/resolve-host-room! host principal handle)))
           (is (= before @hydrations)))
         (doseq [p [(ops/party-principal (:member w))
                    (assoc principal :principal/kind :agent)]]
           (let [before @hydrations]
             (is (thrown? Exception (reads/resolve-host-install-room! host p handle)))
             (is (= before @hydrations)))))))))

(deftest room-only-requires-human-read-and-merge-before-hydration
  (@#'fixture/with-fixture
   true (fn [{:keys [w host hydrations]}]
          (let [owner (ops/party-principal (:owner w)) handle (str (:room w))]
            (doseq [principal [(ops/party-principal (:member w))
                               (ops/party-principal (:admin w))
                               (assoc owner :principal/kind :worker)
                               (assoc owner :principal/kind :agent)]]
              (is (thrown? Exception (reads/resolve-host-room! host principal handle))))
            (is (empty? @hydrations))))))

(deftest room-only-rechecks-revocation-slug-and-owner-expiry-after-hydration
  (doseq [change [:merge :slug :control :context]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [conn w host room]}]
       (let [p (ops/party-principal (:owner w)) handle (str (:room w))
             current! reads/current-control-db! calls (atom 0) hydrations (atom 0)]
         (with-redefs [rooms/live-room
                       (fn [_]
                         (swap! hydrations inc)
                         (case change
                           :merge (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]
                                                     :room/parties [[:party/id (:owner w)]]}])
                           :slug (d/transact conn [{:room/id (:room w) :room/slug "Changed-during-Room-hydration"}])
                           nil)
                         room)
                       reads/current-control-db!
                       (fn [control ctx]
                         (let [n (swap! calls inc)]
                           (if (= n 2)
                             (case change
                               :control (with-redefs [system-db/get-conn (constantly (Object.))] (current! control ctx))
                               :context (with-redefs [context/server-context (Object.)] (current! control ctx))
                               (current! control ctx))
                             (current! control ctx))))]
           (is (thrown? Exception (reads/resolve-host-room! host p handle)))
           (is (= 1 @hydrations))))))))

(deftest room-only-validator-never-rehydrates-after-revocation-or-slug-remapping
  (doseq [change [:merge :slug]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [conn w host]}]
       (let [p (ops/party-principal (:owner w)) handle (str (:room w))
             binding (reads/resolve-host-room! host p handle) calls (atom 0)]
         (case change
           :merge (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]
                                     :room/parties [[:party/id (:owner w)]]}])
           :slug (d/transact conn [{:room/id (:room w) :room/slug "Changed-after-Room-binding"}]))
         (with-redefs [rooms/live-room (fn [& _] (swap! calls inc))]
           (is (thrown? Exception (reads/validate-host-room! host p handle binding)))
           (is (zero? @calls))))))))

(defn- with-replacement [room f]
  ;; Real native lifecycle/Room constructor, no facade or new context. The
  ;; shared fixture owns its original ctx/store and ultimately closes them.
  (let [replacement (atom nil)]
    (try
      (registry/unregister! (:id room))
      (let [next (discourse/make-room {:id (:id room) :slug (:slug room) :ctx (:ctx room) :store (:store room)})]
        (reset! replacement next)
        (f next))
      (finally
        (when @replacement (discourse/close-room! @replacement {:close-context? false}))
        (registry/register! room)))))

(deftest room-only-refuses-native-replacement-during-hydration-and-on-final-validation
  (doseq [phase [:hydrate :validate]]
    (@#'fixture/with-fixture
     true
     (fn [{:keys [w host room]}]
       (let [p (ops/party-principal (:owner w)) handle (str (:room w))]
         (if (= :hydrate phase)
           (let [replacement (atom nil)]
             (try
               (with-redefs [rooms/live-room
                             (fn [_]
                               (registry/unregister! (:id room))
                               (let [next (discourse/make-room {:id (:id room) :slug (:slug room) :ctx (:ctx room) :store (:store room)})]
                                 (reset! replacement next)
                                 next))]
                 (is (thrown? Exception (reads/resolve-host-room! host p handle)))
                 (is (some? @replacement)))
               (finally
                 (when @replacement (discourse/close-room! @replacement {:close-context? false}))
                 (registry/register! room))))
           (let [binding (reads/resolve-host-room! host p handle)]
             (with-replacement
               room (fn [next]
                      (is (not (identical? next room)))
                      (is (not= (:incarnation next) (:incarnation room)))
                      (let [calls (atom 0)]
                        (with-redefs [rooms/live-room (fn [& _] (swap! calls inc))]
                          (is (thrown? Exception (reads/validate-host-room! host p handle binding)))
                          (is (zero? @calls)))))))))))))

(deftest room-only-owner-expiry-and-drain-refuse-before-lookup-or-hydration
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host]}]
     (let [p (ops/party-principal (:owner w)) handle (str (:room w))
           binding (reads/resolve-host-room! host p handle)
           calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Expired owner touched native Room")))]
       (with-redefs [registry/lookup forbidden rooms/live-room forbidden]
         (doseq [mode [:control :context :drain]]
           (let [run (fn []
                       (is (thrown? Exception (reads/resolve-host-room! host p handle)))
                       (is (thrown? Exception (reads/validate-host-room! host p handle binding))))]
             (case mode
               :control (with-redefs [system-db/get-conn (constantly (Object.))] (run))
               :context (with-redefs [context/server-context (Object.)] (run))
               :drain (with-redefs [simple/on-drain-thread? (constantly true)] (run)))))
         (is (zero? @calls)))
       (is (identical? conn (system-db/get-conn)))))))

(deftest room-only-newly-valid-owner-cannot-adopt-an-earlier-binding
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host room]}]
     (let [p (ops/party-principal (:owner w)) handle (str (:room w))
           binding (reads/resolve-host-room! host p handle)
           other (world/fresh-system-conn) cfg (:config @other)
           next-host (assoc host ::reads/owner (reads/host-owner other (:ctx room)))
           calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "New owner adopted old Room binding")))]
       (try
         (d/transact other [{:party/id (:owner w) :party/type :human}
                            {:room/id (:room w) :room/slug (:slug binding)
                             :room/owner [:party/id (:owner w)] :room/parties [[:party/id (:owner w)]]}])
         (with-redefs [system-db/get-conn (constantly other)]
           ;; The replacement owner is genuinely current and authorizes the
           ;; SAME human/room tuple. It would otherwise be able to adopt it.
           (is (= (:room w) (:room-id (reads/preflight-host-read! next-host p handle))))
           (with-redefs [registry/lookup forbidden rooms/live-room forbidden]
             (is (thrown? Exception (reads/validate-host-room! next-host p handle binding)))
             (doseq [bad [nil {} (assoc binding ::reads/room-owner nil)
                          (assoc binding ::reads/room-owner {:control-conn other})]]
               (is (thrown? Exception (reads/validate-host-room! next-host p handle bad))))
             (is (zero? @calls))))
         (finally
           ;; No Room/Spin/provider was admitted on this exact extra control DB.
           (d/release other)
           (assert (and (= :memory (get-in cfg [:store :backend])) (uuid? (get-in cfg [:store :id]))))
           (d/delete-database cfg)))))))

(deftest room-only-refuses-a-reserved-native-lifecycle-and-a-wrong-room-context
  (@#'fixture/with-fixture
   true
   (fn [{:keys [w host]}]
     (let [p (ops/party-principal (:owner w)) handle (str (:room w))
           binding (reads/resolve-host-room! host p handle)
           calls (atom 0) forbidden (fn [& _] (swap! calls inc) (throw (AssertionError. "Unavailable native Room was hydrated")))]
       ;; Existing upstream registry tests prove actual transition reservation;
       ;; these helper oracles prove its fixed predicate is not bypassed here.
       (doseq [mode [:retiring :context]]
         (with-redefs [rooms/live-room forbidden]
           (let [run (fn []
                       (is (thrown? Exception (reads/resolve-host-room! host p handle)))
                       (is (thrown? Exception (reads/validate-host-room! host p handle binding))))]
             (case mode
               :retiring (with-redefs [registry/admitted-incarnation? (constantly false)] (run))
               :context (with-redefs [native-rooms/room-ctx-for (constantly (Object.))] (run))))))
       (is (zero? @calls))))))

(deftest room-only-binding-carries-the-final-control-snapshot-not-a-prehydrate-copy
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host room]}]
     (let [p (ops/party-principal (:owner w)) handle (str (:room w))
           after (atom nil) current! reads/current-control-db!]
       (with-redefs [rooms/live-room (fn [_] (d/transact conn [{:room/id (:room w) :room/name "After Room hydration"}]) room)
                     reads/current-control-db! (fn [& args] (let [db (apply current! args)] (reset! after db) db))]
         (let [binding (reads/resolve-host-room! host p handle)]
           (is (identical? @after (:control-db binding)))
           (is (= "After Room hydration" (:room/name (d/entity (:control-db binding) [:room/id (:room w)]))))
           (let [validated (reads/validate-host-room! host p handle binding)]
             (is (identical? @after (:control-db validated))))))))))

(deftest registered-metadata-binding-never-hydrates-and-retains-current-human-authority
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room host hydrations]}]
     (let [member (ops/party-principal (:member w)) handle (str (:room w))]
       (with-redefs [rooms/live-room (fn [& _] (throw (Exception. "Metadata must not hydrate")))]
         (let [bound (reads/resolve-registered-host-read-room! host member handle)]
           (is (identical? room (:room bound)))
           (is (= (:member w) (:party bound)))
           (is (= (:room w) (:room-id bound)))
           (is (some? (reads/validate-host-read-room! host member handle bound))))
         (with-redefs [registry/lookup (constantly nil)]
           (is (nil? (:room (reads/resolve-registered-host-read-room! host member handle)))))
         (with-redefs [registry/lookup (constantly (assoc room :incarnation (random-uuid)))]
           (is (thrown? clojure.lang.ExceptionInfo
                        (reads/resolve-registered-host-read-room! host member handle))))
         (doseq [party [(:non-member w) (:admin w)]]
           (is (uuid? party))
           (is (thrown? clojure.lang.ExceptionInfo
                        (reads/resolve-registered-host-read-room! host (ops/party-principal party) handle))))
         (is (empty? @hydrations)))))))
