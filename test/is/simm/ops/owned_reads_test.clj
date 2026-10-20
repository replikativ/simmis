(ns is.simm.ops.owned-reads-test
  "Transport-neutral service over the tested canonical/native factory fixture.
   No transport authentication simulation, runtime activation or new grants."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.ops :as dops]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.documents.files :as documents]
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
