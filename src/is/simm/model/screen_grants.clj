(ns is.simm.model.screen-grants
  "Screen-share grants — a time-boxed window in which one room may see a user's
   screen capture (doc/archive/screen-capture-scoping.md).

   Capture is owned by the user; a grant is the ONLY thing that makes a slice of
   it visible to a room. The share button toggles a grant, not a capture. A grant
   is `active` while its `:until` is absent AND its heartbeat is fresh — so a
   client that dies mid-share fails the window SAFE (the room stops seeing the
   stream) rather than leaking indefinitely.

   Grants live in the shared system DB next to rooms and parties."
  (:require [is.simm.model.system-db :as system-db]
            [datahike.api :as d]
            [taoensso.telemere :as log]))

;; A grant with no heartbeat within this window is treated as closed. The client
;; beats every ~10s while a share session runs; 30s tolerates a couple of misses.
;; NOT ^:const — tests rebind it, and a compile-time-inlined value can't be.
(def stale-after-ms 30000)

(defn- conn [] (system-db/get-conn))

(defn- active?
  "Is this grant entity currently open? (:until absent and heartbeat fresh)"
  [g now]
  (and g
       (nil? (:screen-grant/until g))
       (let [beat (:screen-grant/beat g)]
         (and beat (< (- now beat) stale-after-ms)))))

(defn- grant-eid [db party room]
  (when (and (some? party) (some? room))
    (ffirst (d/q '[:find ?e
                   :in $ ?p ?r
                   :where [?e :screen-grant/party ?p]
                          [?e :screen-grant/room ?r]]
                 db party room))))

(defn open-grant!
  "Open (or refresh) the window exposing `party`'s stream to `room`. Idempotent:
   an already-open grant is heartbeated; a closed or missing one is (re)opened
   with a fresh `from`. Returns the grant uuid."
  [party room]
  (system-db/require-id! :party party)
  (system-db/require-id! :room room)
  (let [c (conn)
        db @c
        now (System/currentTimeMillis)
        eid (grant-eid db party room)
        existing (when eid (d/entity db eid))]
    (if (active? existing now)
      (do (d/transact c [{:db/id eid :screen-grant/beat now}])
          (:screen-grant/id existing))
      (let [id (or (:screen-grant/id existing) (java.util.UUID/randomUUID))]
        (d/transact c [(cond-> {:screen-grant/id id
                                :screen-grant/party party
                                :screen-grant/room room
                                :screen-grant/from now
                                :screen-grant/beat now}
                         ;; reopening a previously-closed grant: drop the stale :until
                         eid (assoc :db/id eid))])
        (when eid
          (when (:screen-grant/until existing)
            (d/transact c [[:db/retract eid :screen-grant/until
                            (:screen-grant/until existing)]])))
        (log/log! {:level :info :id ::grant-opened :data {:party party :room room}})
        id))))

(defn heartbeat!
  "Refresh the heartbeat on `party`'s open grant to `room`. No-op if none open."
  [party room]
  (let [c (conn) db @c
        eid (grant-eid db party room)]
    (when (and eid (active? (d/entity db eid) (System/currentTimeMillis)))
      (d/transact c [{:db/id eid :screen-grant/beat (System/currentTimeMillis)}])
      true)))

(defn close-grant!
  "Close `party`'s window onto `room` (sets :until). No-op if not open."
  [party room]
  (let [c (conn) db @c
        eid (grant-eid db party room)]
    (when (and eid (nil? (:screen-grant/until (d/entity db eid))))
      (d/transact c [{:db/id eid :screen-grant/until (System/currentTimeMillis)}])
      (log/log! {:level :info :id ::grant-closed :data {:party party :room room}})
      true)))

(defn active-rooms-for-party
  "Rooms whose window onto `party`'s stream is open right now."
  [party]
  (let [db @(conn) now (System/currentTimeMillis)]
    (->> (when (some? party)
           (d/q '[:find [?e ...] :in $ ?p :where [?e :screen-grant/party ?p]] db party))
         (map #(d/entity db %))
         (filter #(active? % now))
         (mapv :screen-grant/room))))

(defn active-parties-for-room
  "Parties currently sharing their screen into `room`, with each one's open
   window bounds — [{:party uuid :from ms :beat ms} …]."
  [room]
  (let [db @(conn) now (System/currentTimeMillis)]
    (->> (when (some? room)
           (d/q '[:find [?e ...] :in $ ?r :where [?e :screen-grant/room ?r]] db room))
         (map #(d/entity db %))
         (filter #(active? % now))
         (mapv (fn [g] {:party (:screen-grant/party g)
                        :from (:screen-grant/from g)
                        :beat (:screen-grant/beat g)})))))

(defn active-window
  "The open window bounds for `party`→`room`, or nil — {:from ms} for filtering
   the owner's recording/frame archive to what this room is entitled to see."
  [party room]
  (let [db @(conn)
        eid (grant-eid db party room)
        g (when eid (d/entity db eid))]
    (when (active? g (System/currentTimeMillis))
      {:from (:screen-grant/from g) :beat (:screen-grant/beat g)})))

;; ---------------------------------------------------------------------------
;; Personal-room default (doc/archive/screen-capture-scoping.md): whenever a session is
;; live, the sharer's own :personal-ai room ("My Agents") sees the stream too —
;; "my agents see as much as possible". Managed server-side so the client needs
;; no knowledge of its personal room: the client's existing per-room heartbeat
;; also refreshes the personal grant, and stopping the last share stops the
;; heartbeats, so the personal window goes stale and closes with everything else.
;; ---------------------------------------------------------------------------

(defn personal-room-id
  "The party's :personal-ai room uuid (they are a member of exactly one), or nil."
  [party]
  (when (some? party)
    (d/q '[:find ?rid .
           :in $ ?pid
           :where [?p :party/id ?pid]
                  [?e :room/parties ?p]
                  [?e :room/type :personal-ai]
                  [?e :room/id ?rid]]
         @(conn) party)))

(defn open-grant-with-personal!
  "Open a grant for `room`, and — unless `room` already IS it — a standing grant
   for the party's personal room."
  [party room]
  (open-grant! party room)
  (when-let [personal (personal-room-id party)]
    (when (not= personal room)
      (open-grant! party personal))))

(defn heartbeat-with-personal!
  "Heartbeat `room` and (if distinct) the personal room, so the standing default
   stays fresh off the same client beat."
  [party room]
  (heartbeat! party room)
  (when-let [personal (personal-room-id party)]
    (when (not= personal room)
      (heartbeat! party personal))))

(defn close-grant-with-personal!
  "Close the grant for `room`. If the party is no longer sharing into any room
   but their personal one, close the personal grant too (no 30s lingering)."
  [party room]
  (close-grant! party room)
  (when-let [personal (personal-room-id party)]
    (when (and (not= personal room)
               (every? #(= % personal) (active-rooms-for-party party)))
      (close-grant! party personal))))
