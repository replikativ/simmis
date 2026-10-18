(ns is.simm.ops.experiments
  "Experiment rooms and their leaderboards, read through dvergr's own ops.

   dvergr keeps every experiment in a room of its own (one per ExperimentDef,
   created by `room-run`, `runner/run!` in a home, or `catalog_benchmark`), and
   that room's store holds the certified Attempts, the Scorecard and the
   report. A room keeps experiments when its store holds an Attempt of one
   (`:attempt/experiment-content-id`); that is what the room list marks.

   Such a room is made by dvergr, not by simmis: it has no members, so nobody
   sees it until someone joins it. An admin sees the unjoined ones and joins
   them (`join!`), which makes the room an ordinary simmis room: its store is
   the room's content scope and the admin is a member, so every read below
   goes through the usual room authority.

   The leaderboard is dvergr's read model, not one derived here: the
   Scorecard's leaderboard and paired comparisons are `scorecard_detail`, an
   unfinished experiment's are `experiment_progress` and `experiment_report`,
   and tokens and time per candidate are the report's frontier
   (`dvergr.catalog.report/rows`). Each op runs with the ROOM's execution
   context as its daemon context: a room's work is read in the room's ctx.

   Each op also runs for a principal (`dvergr.principal`): the party the
   leaderboard is read for (`is.simm.ops.dvergr/party-principal`), never the process itself, and
   through `is.simm.ops.dvergr/invoke`, which installs simmis's authorizer: a
   party that may not read the room is refused by dvergr's ops themselves, not
   only by the `load-leaderboard!` gate in front of them."
  (:require [datahike.api :as d]
            [dvergr.agent.experiment :as experiment]
            [dvergr.catalog.report :as report]
            [dvergr.room.store :as rstore]
            [dvergr.system.rooms :as srooms]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.ops.dvergr :as ops]
            [is.simm.model.system-db :as system-db]
            [org.replikativ.spindel.engine.core :as rtc]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Which rooms keep experiments
;; =============================================================================

(defn- room-store-db
  "The current value of `room-id`'s own store, read on the room's context;
   nil when the room is not hydrated here (or its hydration failed)."
  [room-id]
  (when-let [rctx (try (srooms/room-ctx-for room-id) (catch Exception _ nil))]
    (binding [rtc/*execution-context* rctx]
      (some-> (srooms/room-msgs-conn room-id) deref))))

(defn store-keeps-experiments?
  "Does the store value `db` hold an Attempt of an experiment?"
  [db]
  (boolean
   (and db
        (contains? (d/schema db) :attempt/experiment-content-id)
        (first (d/datoms db :aevt :attempt/experiment-content-id)))))

(defn keeps-experiments?
  "Does the room `room-id` keep experiments? False when its store cannot be
   read here: the room list must not fail over a mark."
  [room-id]
  (try
    (store-keeps-experiments? (room-store-db room-id))
    (catch Exception e
      (log/log! {:level :debug :id ::experiment-check-failed
                 :data {:room-id (str room-id) :error (ex-message e)}})
      false)))

(defn unjoined-experiment-rooms
  "Rooms of the system DB that keep experiments and that nobody has joined:
   what dvergr made for an experiment. Archived rooms (a benchmark's fixture
   room, retired once its job settled) are left out. `[{:room/id :room/slug
   :room/name}]`, by name."
  []
  (when-let [conn (system-db/get-conn)]
    (->> (d/q '[:find ?id ?slug ?name
                :where
                [?r :room/id ?id]
                [?r :room/slug ?slug]
                [(get-else $ ?r :room/name ?slug) ?name]
                (not [?r :room/parties _])
                (not [?r :room/archived-at _])]
              @conn)
         (filter (fn [[id]] (keeps-experiments? id)))
         (map (fn [[id slug name]] {:room/id id :room/slug slug :room/name name}))
         (sort-by (juxt :room/name :room/slug))
         vec)))

(defn join!
  "Make the experiment room `room-id` a simmis room with `party-id` in it: its
   own store becomes its content scope (dvergr's store id, as for every simmis
   room) and the party a member. Refuses a room that keeps no experiments, so
   this cannot be used to enter an arbitrary room."
  [room-id party-id]
  (system-db/require-id! :room-id room-id)
  (system-db/require-id! :party-id party-id)
  (let [conn (system-db/get-conn)
        db @conn]
    (when-not (d/q '[:find ?r . :in $ ?id :where [?r :room/id ?id]] db room-id)
      (throw (ex-info "No such room" {:error :no-room :room-id (str room-id)})))
    (when-not (keeps-experiments? room-id)
      (throw (ex-info "This room keeps no experiment" {:error :not-an-experiment-room
                                                       :room-id (str room-id)})))
    (let [scope (or (d/q '[:find ?s . :in $ ?id :where [?r :room/id ?id] [?r :room/content-db-scope ?s]]
                         db room-id)
                    (srooms/room-msgs-store-id room-id)
                    (throw (ex-info "The room's store is not provisioned"
                                    {:error :no-store :room-id (str room-id)})))]
      (d/transact conn [{:room/id room-id
                         :room/content-db-scope scope
                         :room/parties [:party/id party-id]}])
      (log/log! {:level :info :id ::experiment-room-joined
                 :data {:room-id (str room-id) :party-id (str party-id)}})
      {:room/id room-id :room/content-db-scope scope})))

;; =============================================================================
;; The leaderboard
;; =============================================================================

(defn- op
  "dvergr op `k` on the live Room `room`, in the room's own context, for
   `principal`, authorized by simmis (`is.simm.ops.dvergr/invoke`)."
  [room principal k args]
  (ops/invoke (:ctx room) principal k (assoc args :room room)))

(defn- id-str
  "A candidate id as dvergr's ops print it: namespaced in full."
  [k]
  (cond (keyword? k) (if (namespace k) (str (namespace k) "/" (name k)) (name k))
        (some? k) (str k)
        :else nil))

(defn- uuid [s] (if (uuid? s) s (parse-uuid (str s))))

(defn- workflow-info
  "Only bounded provenance from the frozen ExperimentDef, never current files
   or arbitrary experiment metadata. Legacy/non-workflow experiments omit it."
  [definition]
  (let [{:keys [name title bundle]} (get-in definition [:experiment/metadata :simmis/workflow])
        bundle-id (cond (uuid? bundle) bundle
                        (and (string? bundle) (= 36 (count bundle))) (parse-uuid bundle))]
    (when (and (string? name) (<= 1 (count name) 64)
               (re-matches #"[a-z0-9][a-z0-9-]*" name) bundle-id)
      (cond-> {:name name :bundle (str bundle-id)}
        (and (string? title) (seq title)) (assoc :title (subs title 0 (min 256 (count title))))))))

(defn- evaluation-info
  "Bounded frozen dataset provenance; never infer a split for old experiments."
  [definition]
  (let [{:keys [id version split cases]} (get-in definition [:experiment/metadata :simmis/evaluation])
        dataset-id (when (and (string? id) (= 36 (count id))) (parse-uuid id))]
    (when (and dataset-id (= 1 version) (#{"development" "validation" "held-out"} split)
               (vector? cases) (<= 1 (count cases) 100)
               (every? #(and (string? %) (<= 1 (count %) 100)) cases)
               (= (count cases) (count (set cases))))
      {:id (str dataset-id) :version version :split split :cases cases})))

(defn- execution-info [definition]
  (let [execution (get-in definition [:experiment/metadata :simmis/execution])]
    (when (and (= :claude-cli (:adapter execution)) (= :text (:mode execution))
               (= :cli-run-admission (:accounting execution)))
      {:adapter :claude-cli :mode :text :accounting :cli-run-admission
       :cli-version (let [v (get-in execution [:provider-policy :cli-version])]
                      (when (and (string? v) (<= 1 (count v) 100)) v))})))

(defn- frontier
  "Presentation evidence from the authorized Scorecard: frozen workflow
   identity and the report's tokens/time rows, keyed by candidate id."
  [room scorecard-id]
  (binding [rtc/*execution-context* (:ctx room)]
    (when-let [sc (experiment/scorecard room (uuid scorecard-id))]
      (let [st (:store room)
            cid (rstore/conversation-id room)
            receipts (vec (keep (fn [{:attempt/keys [id]}]
                                  (some-> (rstore/-load-attempt st cid id) :attempt/receipt
                                          (assoc :attempt/id id)))
                                (:scorecard/entries sc)))]
        {:workflow (workflow-info (:scorecard/experiment sc))
         :evaluation (evaluation-info (:scorecard/experiment sc))
         :execution (execution-info (:scorecard/experiment sc))
         :resources (into {}
                          (map (fn [r] [(id-str (:candidate r))
                                        (select-keys r [:tokens-per-attempt :input-per-attempt :output-per-attempt
                                                        :median-seconds :p90-seconds :wall-seconds])]))
                          (report/rows {:scorecard sc :receipts receipts}))}))))

(defn- completed
  "An experiment with a Scorecard: `scorecard_detail`'s leaderboard and paired
   comparisons, with the frontier's tokens and time."
  [room principal scorecard-id]
  (let [detail (op room principal :scorecard/detail {:id (str scorecard-id)})
        {:keys [resources workflow evaluation execution]} (frontier room scorecard-id)
        exp (:experiment detail)]
    {:experiment (:content-id exp)
     :experiment-id (:id exp)
     :workflow workflow
     :evaluation evaluation
     :execution execution
     :complete? true
     :scorecard (:id detail)
     :environments (:environments exp)
     :repetitions (:repetitions exp)
     :cells (:cells detail)
     :verifier-trust (:verifier-trust detail)
     :candidates (mapv (fn [row]
                         (merge (select-keys row [:candidate :attempts :passed :pass-rate :pass-rate-interval
                                                  :reward-mean :reward-interval
                                                  :microdollars-per-attempt :microdollars-per-pass
                                                  :notional-microdollars-per-attempt
                                                  :notional-microdollars-per-pass])
                                {:microdollars (get-in row [:spend :microdollars])
                                 :notional-microdollars (get-in row [:spend :notional-microdollars])}
                                (select-keys (:spend row) [:priced? :notional-priced? :usage-reported?
                                                          :unpriced-attempts :unreported-attempts])
                                (get resources (:candidate row))))
                       (:leaderboard detail))
     :paired (:paired detail)}))

(defn- in-progress
  "An experiment without a Scorecard yet: per candidate what
   `experiment_progress` counts, and `experiment_report`'s paired comparisons
   over the cells certified so far."
  [room principal {:keys [candidates] content-id :experiment/content-id}]
  (let [report (op room principal :experiment/report {:experiment (str content-id)})]
    {:experiment (str content-id)
     :complete? false
     :cells (reduce + 0 (map :done candidates))
     :candidates (mapv (fn [c]
                         (merge (select-keys c [:priced? :notional-priced? :usage-reported?
                                                :unpriced-attempts :unreported-attempts])
                          {:candidate (id-str (:candidate/id c))
                          :attempts (:done c)
                          :verdicts (:verdicts c)
                          :faults (:faults c)
                          :reward-mean (:reward-mean c)
                           :microdollars (:microdollars c)}))
                       (sort-by (comp - #(or % -1) :reward-mean) candidates))
     :paired (:paired report)}))

(defn leaderboard
  "The experiments the live Room `room` keeps, newest Scorecard first, then
   those still without one: `{:experiments [...]}`. Each completed experiment
   is its newest Scorecard. The reads run for `principal`."
  [room principal]
  (let [scorecards (op room principal :scorecard/list {:limit 200})
        newest (reduce (fn [m sc]
                         (let [cid (get-in sc [:experiment :content-id])]
                           (if (contains? m cid) m (assoc m cid (:id sc)))))
                       {} scorecards)
        progress (op room principal :experiment/progress {})
        unscored (remove #(contains? newest (str (:experiment/content-id %))) (:experiments progress))]
    {:experiments (into (vec (for [sc scorecards
                                   :let [cid (get-in sc [:experiment :content-id])]
                                   :when (= (:id sc) (get newest cid))]
                               (completed room principal (:id sc))))
                        (map #(in-progress room principal %))
                        unscored)
     :running (count (:running progress))}))

(defn room-leaderboard
  "`leaderboard` of the simmis room `room-id` read for the party `party-id`,
   or nil when the room is not live."
  [room-id party-id]
  (when-let [room (room-agents/live-room room-id)]
    (leaderboard room (ops/party-principal party-id))))

