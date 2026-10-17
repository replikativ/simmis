(ns repl
  "REPL conveniences for the running dev system.

   Loaded into `user`, so everything here is callable unqualified at the
   prompt and in a one-liner:

       clj-nrepl-eval -p 47888 \"(status)\"
       clj-nrepl-eval -p 47888 \"(rooms)\"

   The point is to remove three things that cost time every session: writing
   the same `require` before every probe, writing a datalog query to answer
   \"what rooms exist\", and answering \"is it actually up and is the bundle
   current\" from outside the JVM with `ss`, `grep` and `ls -la`.

   Everything here READS. Nothing in this namespace mutates the system —
   lifecycle lives in `user` (start!/stop!/restart-web!) where it is harder to
   call by accident."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [datahike.api :as d]
            [is.simm.model.system-db :as sdb]
            [is.simm.model.parties :as parties]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.room-databases :as room-dbs]
            [is.simm.runtimes.instance :as instance]))

;; =============================================================================
;; Is it up?
;; =============================================================================

(defn- port-open?
  "Whether something is LISTENING, rather than whether we once started it.
   `user/status` reports its own bookkeeping, which stays true after a
   component has died."
  [port]
  (with-open [_ (java.net.Socket. "127.0.0.1" (int port))]
    true))

(defn- listening? [port]
  (try (port-open? port) (catch Exception _ false)))

(defn- newest-source
  "mtime of the most recently edited CLJS-visible source."
  []
  (->> (file-seq (io/file "src"))
       (filter #(re-find #"\.clj[sc]$" (.getName ^java.io.File %)))
       (map #(.lastModified ^java.io.File %))
       (reduce max 0)))

(defn- bundle-state []
  (let [f (io/file "public/js/app.js")]
    (if-not (.exists f)
      {:built? false}
      (let [built (.lastModified f)
            newest (newest-source)]
        {:built? true
         :size-mb (long (/ (.length f) 1048576))
         :age-min (long (/ (- (System/currentTimeMillis) built) 60000))
         ;; The failure this exists for: a build that FAILED leaves the
         ;; previous bundle in place, so "Build completed" in the log and a
         ;; served page can both be from before your edit.
         :stale? (> newest built)}))))

(defn status
  "What is actually running, and is the bundle current.

   Ports are probed, not remembered — this instance's ports, i.e. the defaults
   plus SIMMIS_PORT_OFFSET, so an offset instance reports on itself and not on
   whatever holds the default ports. `:stale?` compares the bundle's mtime
   against the newest .cljs/.cljc source — a failed build leaves the old
   bundle serving, which looks identical to success from the outside."
  []
  (let [conn (try (sdb/get-conn) (catch Exception _ nil))
        db (some-> conn deref)
        ports (instance/ports)]
    {:instance (instance/summary)
     :ports (update-vals ports listening?)
     ;; Observation must not LOAD Shadow while the main startup thread is
     ;; loading it. Clojure registers namespace headers before their bodies;
     ;; a concurrent require can then compile against still-missing vars.
     :shadow (try (let [running? (some-> (find-ns 'shadow.cljs.devtools.api)
                                         (ns-resolve 'worker-running?))]
                    {:app-watching? (if (and running? (bound? running?))
                                      (boolean (running? :app)) :unknown)})
                  (catch Throwable _ {:app-watching? :unknown}))
     :bundle (bundle-state)
     :system-db (if db
                  {:rooms   (count (d/q '[:find ?e :where [?e :room/id _]] db))
                   :parties (count (d/q '[:find ?e :where [?e :party/id _]] db))
                   :kbs     (count (d/q '[:find ?e :where [?e :kb/id _]] db))}
                  :not-initialized)}))

;; =============================================================================
;; What is in there?
;; =============================================================================

(defn conn
  "The system DB conn — parties, rooms, KBs, grants, proposals."
  []
  (sdb/get-conn))

(defn db [] @(conn))

(defn q
  "Query the system DB without the ceremony: (q '[:find ?n :where [_ :room/name ?n]])"
  [query & args]
  (apply d/q query (db) args))

(defn rooms
  "Every room, newest first."
  []
  (->> (d/q '[:find [(pull ?e [:room/id :room/name :room/slug :room/type
                               :room/created :room/content-db-scope]) ...]
              :where [?e :room/id _]]
            (db))
       (sort-by :room/created)
       reverse
       vec))

(defn parties
  "Every party (humans and agents)."
  []
  (parties/list-parties))

(defn knowledge-bases []
  (->> (d/q '[:find [(pull ?e [:kb/id :kb/name :kb/db-scope :kb/owner]) ...]
              :where [?e :kb/id _]]
            (db))
       vec))

(defn room
  "One room by slug, uuid, or a substring of its name."
  [ident]
  (let [s (str ident)]
    (or (first (filter #(or (= s (:room/slug %))
                            (= s (str (:room/id %))))
                       (rooms)))
        (first (filter #(str/includes? (str/lower-case (str (:room/name %)))
                                       (str/lower-case s))
                       (rooms))))))

(defn room-conn
  "The conn to a room's own store, by the same ident `room` accepts.

   Goes through `room-databases/room-store-conn`, which resolves on the ROOM's
   execution context — yggdrasil's registry is context-backed, so looking the
   store up from the server context finds nothing and returns nil with no
   error. nil here means the room is not hydrated in this process."
  [ident]
  (some-> (room ident) :room/content-db-scope room-dbs/room-store-conn))

(defn kb-conn
  "The conn to a KB's store, by db-scope uuid."
  [db-scope]
  (kbs/connect-kb-database db-scope))

;; =============================================================================

(defn help []
  (println "
  status              ports (probed), shadow watcher, bundle freshness, counts
  conn / db / q       system DB conn, its value, and (q '[:find ...])

  rooms               every room, newest first
  parties             humans and agents
  knowledge-bases     every KB
  room                one room by slug, uuid or name substring
  room-conn           that room's own store conn (nil = not hydrated here)
  kb-conn             a KB's conn by db-scope

  lifecycle lives in `user`:
  (user/status) (user/restart-web!) (user/start!) (user/stop!)
  NOT (user/restart-shadow!) over nREPL — it takes the whole JVM with it.

  CLJS: clj-nrepl-eval -p 9631 \"(shadow/repl :app)\" first, then evaluate.")
  :ok)
