(ns is.simm.runtimes.storage-gc
  "Periodic storage GC across every persistent store.

   Datahike's index is immutable: every transaction writes new index-node blobs
   and leaves the superseded ones orphaned, reclaimed ONLY by an explicit,
   offline GC that never runs automatically. Without this the stores grow
   without bound — dvergr's own `gc-stores!` docstring reports a long-lived
   `.dvergr` reaching tens of GB of orphaned `.ksv` blobs.

   dvergr has the sweep (`dvergr.system.rooms/gc-stores!`, which coordinates
   system-db + each room's kb/msgs/repo/data through one `ygg/gc!` per room ctx),
   but the loop that drives it lives in dvergr's own daemon — and simmis boots
   through `is.simm.runtimes.web`, not that daemon, so nothing has ever swept.

   ORPHAN-ONLY, DELIBERATELY. We pass no `:remove-before`, so `gc-stores!`
   defaults to epoch and reclaims only unreachable garbage: every branch head and
   its full history is kept. A retention window would collapse datahike snapshots
   older than a cutoff — that is a data-retention policy decision, not a
   housekeeping one, and it is not ours to make implicitly. When we do want one,
   it belongs behind an explicit, per-store setting (media wants aggressive
   retention; the books want none), not a global default.

   Runs in the writer's process on purpose: `d/gc-storage` must, or it can sweep
   blobs belonging to in-flight commits. This is the simmis web JVM, which is the
   writer for these stores — so this namespace must not be turned into a sidecar
   or a cron script.

   OFF BY DEFAULT. The dev.simm.is system DB was wiped by this loop: a mark
   phase died on an Error, the mark came back empty, and the sweep deleted every
   key but `:branches`. datahike#1117 makes the mark fail closed; until a datahike
   release carrying it is on the classpath, an Error inside a branch walk can
   still do the same. So the loop starts only when asked to —
   `:storage-gc {:enabled? true}` in config.local.edn, or
   `SIMMIS_STORAGE_GC=true` (the environment wins when set) — and while it is on
   with an older datahike, every sweep warns."
  (:require [clojure.string :as str]
            [datahike.tools :as dh-tools]
            [dvergr.substrate.config :as dcfg]
            [dvergr.system.db :as sdb]
            [dvergr.system.rooms :as srooms]
            [is.simm.runtimes.context :as ctx]
            [taoensso.telemere :as log]))

(defonce ^:private gc-thread (atom nil))

(def ^:private default-interval-ms
  "6 hours — matches dvergr's daemon default. Orphan reclamation is not urgent;
   the cost of sweeping too often is real (it walks every store)."
  21600000)

;; ---------------------------------------------------------------------------
;; Is it on, and is it safe?
;; ---------------------------------------------------------------------------

(def datahike-fail-closed-gc-version
  "The first datahike release whose `gc-storage!` refuses to sweep on an
   incomplete mark (datahike#1117). Below it, an Error inside a branch walk
   empties the mark and the sweep deletes the store.

   TODO: #1117 is not merged yet, so no release carries it. This is a LOWER
   BOUND — the release after the one simmis pins (0.8.1904), and no release
   at or below that has the fix. Set it to the exact release that ships #1117
   when it does; until then a datahike bump to 0.8.1905 that lacks the fix
   would silence the warning."
  "0.8.1905")

(defn- parse-version
  "\"0.8.1904\" → [0 8 1904]; nil for anything that is not dotted numbers
   (a source checkout reports no version at all)."
  [v]
  (when (and (string? v) (re-matches #"\d+(\.\d+)*" v))
    (mapv parse-long (str/split v #"\."))))

(defn version-at-least?
  "Does version string `v` reach `floor`? An unknown `v` does not."
  [v floor]
  (let [a (parse-version v) b (parse-version floor)]
    (boolean (and a b (not (neg? (compare (vec (take (count b) (concat a (repeat 0))))
                                          b)))))))

(defn datahike-version
  "The datahike release on the classpath, from its jar's pom.properties; nil
   for a source checkout (`:local`)."
  []
  (dh-tools/get-version 'org.replikativ/datahike))

(defn enabled?
  "Is the periodic sweep switched on? `SIMMIS_STORAGE_GC` decides when it is
   set (\"true\" or anything else); otherwise `:storage-gc {:enabled? true}` in
   the config. Default OFF — see the namespace doc for why."
  ([] (enabled? (System/getenv "SIMMIS_STORAGE_GC") (dcfg/config)))
  ([env config]
   (if-let [v (some-> env str/trim not-empty)]
     (= "true" (str/lower-case v))
     (true? (get-in config [:storage-gc :enabled?])))))

(defn- warn-if-unsafe-datahike! []
  (let [v (datahike-version)]
    (when-not (version-at-least? v datahike-fail-closed-gc-version)
      (log/log! {:level :warn :id ::datahike-gc-not-fail-closed
                 :msg (str "Storage GC is ON with datahike " (or v "<source checkout, version unknown>")
                           ", which predates the fail-closed mark (datahike#1117; this check wants >= "
                           datahike-fail-closed-gc-version "). An Error inside a branch walk "
                           "empties the mark and the sweep deletes the store.")
                 :data {:datahike-version v
                        :required datahike-fail-closed-gc-version}}))))

;; ---------------------------------------------------------------------------
;; One sweep
;; ---------------------------------------------------------------------------

(defn report-problems
  "What is wrong with a `gc-stores!` report, as a vector of keywords; empty
   when nothing is. `room-count` is how many rooms the system DB lists.

   A nil report, a system-DB `:error` or a `:timeout` are failures, not
   empty sweeps. So is a room count below the rooms that exist: dvergr
   swallows a room's GC failure and counts only the rooms that succeeded.

   NOT caught here: dvergr turns a NIL result from `d/gc-storage` into
   `{:reclaimed 0}` (the `:else 0` arm in `gc-stores!`), so it reads as an
   honest empty sweep. Telling the two apart needs dvergr to keep the nil."
  [report room-count]
  (let [{:keys [system-db rooms]} report]
    (cond-> []
      (nil? report)                        (conj :no-report)
      (and report (nil? system-db))        (conj :no-system-db-report)
      (:error system-db)                   (conj :system-db-error)
      (= :timeout (:reclaimed system-db))  (conj :system-db-timeout)
      (and report (number? room-count) (number? rooms) (< rooms room-count))
      (conj :rooms-failed))))

(defn sweep!
  "Run one orphan-only GC sweep across all stores. Returns the report map.
   Callable by hand from the REPL — which runs it whether or not the loop is
   enabled, so mind the datahike warning it logs."
  []
  (ctx/with-server-context
    (warn-if-unsafe-datahike!)
    (try
      (let [r (srooms/gc-stores! {})
            room-count (try (count (sdb/all-rooms)) (catch Throwable _ nil))
            problems (report-problems r room-count)]
        (if (seq problems)
          (log/log! {:level :error :id ::sweep-incomplete
                     :msg "Storage GC sweep did not complete — this is NOT an empty sweep"
                     :data {:problems problems :report r :rooms-listed room-count}})
          (log/log! {:level :info :id ::sweep-complete
                     :msg "Storage GC sweep complete (orphan-only)"
                     :data r}))
        r)
      (catch Throwable e
        (log/log! {:level :error :id ::sweep-failed
                   :msg "Storage GC sweep failed"
                   :error e})
        {:error (.getMessage e)}))))

(defn start!
  "Start the periodic sweep (boot sweep, then every `interval-ms`). Idempotent."
  ([] (start! default-interval-ms))
  ([interval-ms]
   (when-not @gc-thread
     (let [t (Thread.
              (fn []
                (loop []
                  (sweep!)
                  (Thread/sleep (long interval-ms))
                  (when @gc-thread (recur))))
              "simmis-storage-gc")]
       (.setDaemon t true)
       (reset! gc-thread t)
       (.start t)
       (log/log! {:level :info :id ::started
                  :msg (str "Storage GC started (orphan-only, every "
                            (quot interval-ms 3600000) "h)")})))
   :started))

(defn stop! []
  (reset! gc-thread nil)
  :stopped)

(defn boot!
  "Start the loop if it is enabled; otherwise say once why it is not.
   Returns `:started` or `:disabled`. `enabled` defaults to `(enabled?)`."
  ([] (boot! (enabled?)))
  ([enabled]
   (if enabled
     (do (warn-if-unsafe-datahike!)
         (start!))
     (do (log/log! {:level :info :id ::disabled
                    :msg (str "Storage GC is OFF (default). It sweeps orphaned index nodes, but on "
                              "datahike without #1117 an Error in the mark makes it delete the whole "
                              "store — that is how the dev.simm.is system DB was lost. Enable with "
                              ":storage-gc {:enabled? true} in config.local.edn or SIMMIS_STORAGE_GC=true.")
                    :data {:datahike-version (datahike-version)}})
         :disabled))))
