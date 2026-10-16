(ns is.simm.workflows.review-fixture
  "Real rooms for the review-escalation tests (`is.simm.workflows.review`):
   provisioned by simmis's `rooms/create-room!` in a throwaway dvergr home with
   a real system DB, their worlds real dvergr forks with files under wiki/ in
   the room's repository, reviewed by dvergr's `review!` with a SCRIPTED
   reviewer. Use `with-home` :once and `in-server-context` :each."
  (:require [clojure.java.io :as io]
            [datahike.api :as d]
            [dvergr.catalog :as catalog]
            [dvergr.orchestration.daemon :as daemon]
            [dvergr.principal :as principal]
            [dvergr.room.registry :as rreg]
            [dvergr.substrate.paths :as paths]
            [dvergr.system.db :as dsdb]
            [dvergr.system.rooms :as srooms]
            [is.simm.agents.room-agents :as ra]
            [is.simm.model.parties :as parties]
            [is.simm.model.rooms :as rooms]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.dvergr :as sops]
            [is.simm.runtimes.context :as ctx]
            [is.simm.workflows.review :as wr]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as rtc]))

;; =============================================================================
;; Fixture: a throwaway home, a real system DB, two people
;; =============================================================================

(def env (atom nil))

(defn delete-tree! [path]
  (when (.exists (io/file path))
    (run! io/delete-file (reverse (file-seq (io/file path))))))

(defn party! [display]
  (let [id (random-uuid)]
    (d/transact (system-db/get-conn)
                [{:party/id id :party/type :human :party/display-name display
                  :actor/id (parties/party-id->actor-id id) :actor/kind :human
                  :actor/name display :actor/status :online
                  :actor/created-at (java.util.Date.)}])
    id))

(defn with-home [f]
  (let [original-home (paths/home)
        original-conn @system-db/conn
        home (str (io/file (System/getProperty "java.io.tmpdir")
                           (str "simmis-review-test-" (random-uuid))))
        root (daemon/create-shared-context :with-git? false)]
    (try
      (paths/set-home! home)
      (dsdb/reset-conn!)
      (srooms/clear-room-ctxs!)
      (reset! system-db/conn nil)
      (system-db/init!)
      (with-redefs [ctx/server-context root]
        (reset! env {:root root :owner (party! "Olga Owner") :member (party! "Mats Member")})
        (f))
      (finally
        (reset! env nil)
        (srooms/release-room-stores!)
        (dsdb/reset-conn!)
        (reset! system-db/conn original-conn)
        (try (context/close-context! root) (catch Throwable _))
        (paths/set-home! original-home)
        (delete-tree! home)))))


(defn in-server-context
  "An :each fixture: the test thread works in the server's context, as an RPC
   thread does."
  [f]
  (binding [rtc/*execution-context* (:root @env)] (f)))

(defn owner [] (:owner @env))
(defn member [] (:member @env))

(defn local-op [room op args]
  (sops/invoke (:ctx room) principal/local-principal op args))

(defn texts [m] (into {} (map (fn [[p t]] [p {:text t}])) m))

(defn room-with-world
  "A simmis room (owner + member) with `base` committed, and a world: a fork
   of it holding `changes`. `{:room-id :room :world}`."
  [base changes]
  (let [{room-id :room/id} (rooms/create-room! (owner) "Wiki" :group [(member)])
        room (ra/live-room room-id)
        _ (when (seq base) (local-op room :room/write {:room (name (:id room)) :files (texts base)}))
        world (:id (local-op room :room/fork {:room (name (:id room))}))]
    (when (seq changes) (local-op room :room/write {:room world :files (texts changes)}))
    {:room-id room-id :room room :world world}))

(defn parent-file [room path]
  (get (catalog/read-tree room "/") path))

(defn world-live? [room world]
  (some? (binding [rtc/*execution-context* (:ctx room)] (rreg/lookup (keyword world)))))

(def wiki-spec
  {:review/checks [:fork/review-tier [:diff/only-paths ["wiki/"]]]
   :review/reviewer {:instructions "Approve faithful wiki updates."}})

(defn scripted
  "A reviewer that answers `verdict` and counts its calls."
  [calls verdict]
  (fn [input] (swap! calls conj input) verdict))

(defn reviewed [room-id world spec reviewer & {:as opts}]
  @(apply wr/review-run! room-id world spec
          (mapcat identity (merge {:reviewer reviewer :reviewer-party (owner)} opts))))

(defn proposals-for [review-id]
  (d/q '[:find [?pid ...] :in $ ?r :where [?p :proposal/review ?r] [?p :proposal/id ?pid]]
       @(system-db/get-conn) review-id))

(def escalate
  {:review/verdict :escalate :review/reasons ["cites no source"] :review/concerns ["page A may be invented"]})

