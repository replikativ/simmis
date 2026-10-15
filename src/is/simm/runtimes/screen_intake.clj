(ns is.simm.runtimes.screen-intake
  "Live screen sharing WITH AGENTS (Track 4c), scoped by USER
   (doc/archive/screen-capture-scoping.md).

   A user's screen CAPTURE is owned by the user: frames are described by
   dvergr.media.vision and stored in the USER's screens DB
   (`data/simmis-screens/<party-id>`) and a per-user hot ring buffer. The
   client posts to `/screen-frames` — no room in the path; the sharer's identity
   comes from the JWT.

   A room sees a user's stream only through an active GRANT
   (`is.simm.model.screen-grants`): the share button toggles a time-boxed window,
   not a capture. The agent reads it through the `screen/*` SCI vocabulary
   (`sci-namespace`), which resolves the room's active grants → the parties
   sharing into it → their recent frames as DATA, ATTRIBUTED by name. So an agent
   can tell whose screen it is looking at and PROGRAM over it, and a member's raw
   frames are never pooled where the room can browse them — the room gets the
   agent-mediated view; the owner's gallery (search-own) shows their own stream.

   DELIBERATELY NOT chat messages: auto-respond agents would reply to every
   frame. Agents PULL via `screen/*` in clojure_eval — it is a sandbox
   vocabulary, not a bespoke tool."
  (:require [is.simm.model.screen-grants :as grants]
            [is.simm.model.parties :as parties]
            [is.simm.model.blobs :as blobs]
            [is.simm.model.system-db :as system-db]
            [clojure.string :as str]
            [datahike.api :as d]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Durable per-USER screens DB (gallery + fulltext search)
;; =============================================================================

(def ^:private screens-schema
  [{:db/ident :screenshot/at :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :screenshot/blob-id :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}
   {:db/ident :screenshot/transcript :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one}])

(def screens-fulltext-ident :screens/fulltext)

(defonce ^:private screens-conns (atom {}))

(defn- screens-cfg [party-uuid]
  {:store {:backend :file
           :path (str "data/simmis-screens/" party-uuid)
           ;; STABLE id derived from the party — a fresh randomUUID per connect
           ;; makes reconnecting to an EXISTING store fail with a store-identity
           ;; mismatch after any restart (the recordings store learned this).
           :id (java.util.UUID/nameUUIDFromBytes
                 (.getBytes (str "simmis-screens/" party-uuid) "UTF-8"))}
   :schema-flexibility :write
   :keep-history? false
   :crypto-hash? true})

(defn ensure-screens-conn
  "Connection to a USER's screens DB, creating DB + schema + fulltext index on
   first use."
  [party-uuid]
  (or (get @screens-conns party-uuid)
      (locking screens-conns
        (or (get @screens-conns party-uuid)
            (let [cfg (screens-cfg party-uuid)
                  _ (when-not (d/database-exists? cfg) (d/create-database cfg))
                  conn (d/connect cfg)]
              (d/transact conn screens-schema)
              (when-not (:db.secondary/type (d/entity @conn screens-fulltext-ident))
                (try ((requiring-resolve 'dvergr.search.secondary/declare-index!)
                      conn screens-fulltext-ident [:screenshot/transcript]
                      (str "data/simmis-screens/" party-uuid "-ft"))
                     (catch Throwable t
                       (log/log! {:level :warn :id ::fulltext-declare-failed
                                  :data {:party party-uuid :error (ex-message t)}}))))
              (swap! screens-conns assoc party-uuid conn)
              conn)))))

(defn- persist-frame! [party-uuid {:keys [at blob-id text]}]
  (try
    (d/transact (ensure-screens-conn party-uuid)
                [{:screenshot/at at
                  :screenshot/blob-id blob-id
                  :screenshot/transcript text}])
    (catch Throwable t
      (log/log! {:level :error :id ::persist-frame-failed
                 :data {:party party-uuid :error (str t)}}))))

(defn search-frames
  "Frames of ONE party's stream: ranked fulltext for `query`, or the latest `n`
   when query is blank. Optional `since` (epoch ms) clips to a grant window.
   Returns [{:at :blob-id :text :score}]."
  ([party-uuid query n] (search-frames party-uuid query n nil))
  ([party-uuid query n since]
   (let [conn (ensure-screens-conn party-uuid)
         db @conn
         after? (fn [at] (or (nil? since) (>= (long at) (long since))))]
     (if (str/blank? (str query))
       (->> (d/q '[:find ?at ?blob ?text
                   :where [?e :screenshot/at ?at]
                          [?e :screenshot/blob-id ?blob]
                          [?e :screenshot/transcript ?text]]
                 db)
            (filter (fn [[at _ _]] (after? at)))
            (sort-by first >)
            (take n)
            (mapv (fn [[at blob text]] {:at at :blob-id blob :text text})))
       (->> ((requiring-resolve 'dvergr.search.secondary/search)
             db screens-fulltext-ident (str query))
            (map (fn [[eid score]]
                   (let [e (d/entity db eid)]
                     {:score score :at (:screenshot/at e)
                      :blob-id (:screenshot/blob-id e)
                      :text (:screenshot/transcript e)})))
            (filter #(after? (:at %)))
            (take n)
            vec)))))

(defn search-own
  "The owner's own gallery — their full screen stream, unfiltered by grant."
  [party-uuid query n]
  (search-frames party-uuid query n nil))
;; delete-frame! is defined below, after the `state` ring buffer it drops from.

;; =============================================================================
;; Per-user hot ring buffer — "what's on this user's screen NOW"
;; =============================================================================

(defonce ^:private state
  ;; party-uuid → {:frames [{:at ms :text desc :blob-id id} …] (newest last, ≤5)
  ;;               :last-at ms :busy? bool}
  (atom {}))

(def ^:private min-interval-ms 8000)
(def ^:private keep-frames 5)

(def ^:private describe-prompt
  "You are the note taker for a live screen share. Describe this
screenshot for a colleague who cannot see it:
- If a browser/app is visible, name it and any URL or document title.
- Transcribe the important visible text VERBATIM, structured with
  markdown (headings/lists/code blocks as on screen). Prioritize what
  the user is working on over chrome/menus.
- End with one line: what the user appears to be doing.")

(defn handle-frame!
  "Accept a posted frame for `party-uuid` (the authenticated sharer). Stores it
   in the user's stream; grants decide who may see it. Returns a ring response."
  [party-uuid ^bytes bytes mime]
  (let [now (System/currentTimeMillis)
        {:keys [last-at busy?]} (get @state party-uuid)]
    (cond
      (nil? party-uuid)
      {:status 401 :body "unauthenticated"}

      (or busy? (and last-at (< (- now last-at) min-interval-ms)))
      {:status 202 :body "throttled"}

      :else
      (do
        (swap! state update party-uuid assoc :busy? true :last-at now)
        (future
          (try
            (let [blob (blobs/store! bytes (or mime "image/jpeg"))
                  describe (requiring-resolve 'dvergr.media.vision/describe)
                  desc (describe bytes (or mime "image/jpeg")
                                 {:prompt describe-prompt :max-tokens 8000})]
              (if (and desc (not (str/blank? desc)))
                (let [frame {:at now :text desc :blob-id (:blob/id blob)}]
                  (swap! state update party-uuid
                         (fn [s]
                           (-> s
                               (update :frames (fnil conj []) frame)
                               (update :frames #(vec (take-last keep-frames %)))
                               (assoc :busy? false))))
                  (persist-frame! party-uuid frame)
                  (log/log! {:level :info :id ::frame-described
                             :data {:party party-uuid :blob (:blob/id blob)
                                    :chars (count desc)}}))
                (do (swap! state update party-uuid assoc :busy? false)
                    (log/log! {:level :warn :id ::describe-failed :data {:party party-uuid}}))))
            (catch Throwable e
              (swap! state update party-uuid assoc :busy? false)
              (log/log! {:level :error :id ::frame-failed
                         :data {:party party-uuid :error (str e)}}))))
        {:status 200 :body "accepted"}))))

(defn- recent-frames [party-uuid n]
  (take-last n (get-in @state [party-uuid :frames])))

(defn delete-frame!
  "Delete a frame from the OWNER's stream by blob-id. Retracts the datom and
   drops it from the hot buffer. The CAS blob itself is left for GC (it is
   content-addressed and may be shared). Returns {:deleted n}."
  [party-uuid blob-id]
  ;; a nil blob-id is UNBOUND to datahike: the query would match, and retract,
  ;; every screenshot the owner has
  (system-db/require-id! :party party-uuid)
  (system-db/require-id! :blob-id blob-id)
  (let [conn (ensure-screens-conn party-uuid)
        eids (d/q '[:find [?e ...] :in $ ?b :where [?e :screenshot/blob-id ?b]] @conn blob-id)]
    (when (seq eids)
      (d/transact conn (mapv (fn [e] [:db/retractEntity e]) eids)))
    (swap! state update party-uuid
           (fn [s] (update s :frames (fn [fs] (vec (remove #(= blob-id (:blob-id %)) fs))))))
    (log/log! {:level :info :id ::frame-deleted :data {:party party-uuid :blob blob-id}})
    {:deleted (count eids)}))

;; =============================================================================
;; The agent's window — a `screen/*` SCI vocabulary, NOT a tool.
;;
;; simmis agents get a sandbox, not bespoke tools: they program over the screen
;; the way they program over sheets, KBs and the drive. So this returns DATA
;; (vectors of maps) the agent can filter, aggregate and cross-reference — never
;; a pre-formatted string. Room-scoped and grant-resolved: an agent sees only
;; what humans have actively shared INTO its room, attributed by who shared it.
;; =============================================================================

(defn- party-label [party-uuid]
  (let [p (parties/get-party party-uuid)]
    (or (:party/display-name p) (:party/handle p) (:party/email p)
        (str "user " (subs (str party-uuid) 0 8)))))

(defn room-sharers
  "Who is sharing their screen INTO `room-uuid` right now —
   [{:party <uuid-str> :name <label> :since <epoch-ms>} …]."
  [room-uuid]
  (mapv (fn [{:keys [party from]}]
          {:party (str party) :name (party-label party) :since from})
        (grants/active-parties-for-room room-uuid)))

(defn room-frames
  "The `n` most recent described frames visible to `room-uuid`, across every
   active sharer, newest first, each attributed —
   [{:party :name :at :blob-id :text} …]."
  [room-uuid n]
  (let [n (max 1 (min n keep-frames))]
    (->> (for [{:keys [party from]} (grants/active-parties-for-room room-uuid)
               f (->> (recent-frames party keep-frames)
                      (filter #(>= (long (:at %)) (long from))))]
           (assoc f :party (str party) :name (party-label party)))
         (sort-by :at >)
         (take n)
         vec)))

(defn room-frame-search
  "Fulltext over what each sharer has granted `room-uuid` (their whole shared
   history within the window), attributed — [{:party :name :at :blob-id :text :score} …]."
  [room-uuid query n]
  (->> (for [{:keys [party from]} (grants/active-parties-for-room room-uuid)
             hit (search-frames party query n from)]
         (assoc hit :party (str party) :name (party-label party)))
       (sort-by (fn [h] (or (:score h) 0)) >)
       (take n)
       vec))

(defn sci-namespace
  "Build the `screen/*` map for `sci/add-namespace!`, bound to `room-uuid`.
   Everything is room-scoped through grants and returns plain data."
  [room-uuid]
  {'sharers (fn [] (room-sharers room-uuid))
   'frames  (fn ([] (room-frames room-uuid 1))
              ([n] (room-frames room-uuid n)))
   'search  (fn ([query] (room-frame-search room-uuid query 10))
              ([query n] (room-frame-search room-uuid query n)))
   'text    (fn [] (->> (room-frames room-uuid keep-frames)
                        (map (fn [{:keys [name text]}] (str name ": " text)))
                        (str/join "\n\n")))})

(def prompt-block
  "What the agent is told about `screen/*`. Kept next to the code so they cannot
   drift."
  (str "\n\nSCREEN SHARES are DATA, not a tool. Humans in this room may share"
       " their screen with you; you read it in clojure_eval via `screen/*`"
       " (each returns plain data you can filter/aggregate):\n"
       "- `(screen/sharers)` — who is sharing right now:"
       " [{:party :name :since}]\n"
       "- `(screen/frames)` / `(screen/frames n)` — recent described frames,"
       " newest first, attributed: [{:name :at :blob-id :text}]\n"
       "- `(screen/search \"billing dashboard\")` — fulltext over what has been"
       " shared into this room\n"
       "- `(screen/text)` — the recent screen text concatenated, for a quick look\n"
       "All are empty when nobody is sharing. Use them when the conversation"
       " refers to something on screen ('this', 'here', an error, a document…)."))
