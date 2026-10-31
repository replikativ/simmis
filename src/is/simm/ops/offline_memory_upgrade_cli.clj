(ns is.simm.ops.offline-memory-upgrade-cli
  "Private maintenance CLI. The deployment harness stops and runtime-masks
   simmis-dev and owns the coordinated backup before invoking this process.
   No flag can turn a live application connection into an offline capability."
  (:refer-clojure :exclude [run!])
  (:require [clojure.core.async :as async]
            [datahike.kabel.connector :as connector]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [datahike.api :as d]
            [is.simm.model.memory-upgrades :as receipts]
            [is.simm.ops.offline-memory-upgrade :as upgrade])
  (:import [java.nio.file Files Path Paths LinkOption StandardOpenOption]
           [java.nio.file.attribute FileAttribute PosixFilePermission PosixFilePermissions]
           [java.nio.channels FileChannel]))

(defn- refuse! [reason]
  (throw (ex-info "Memory upgrade maintenance check failed" {:reason reason})))

(defn- command [args]
  (let [process (.start (doto (ProcessBuilder. ^java.util.List args) (.redirectErrorStream true)))
        done (.waitFor process 10 java.util.concurrent.TimeUnit/SECONDS)]
    (when-not done (.destroyForcibly process) (refuse! :maintenance-command-timeout))
    (let [text (slurp (.getInputStream process))]
      (when-not (zero? (.exitValue process)) (refuse! :maintenance-command-failed))
      text)))

(defn require-stopped!
  "Kernel/service checks supplement the operator's exclusive-writer contract.
   They do not claim that an advisory lock excludes independent host processes."
  []
  (let [state (into {} (map #(str/split % #"=" 2))
                    (str/split-lines (command ["systemctl" "show" "simmis-dev"
                                              "--property=LoadState,ActiveState,MainPID"])))
        cgroup (io/file "/sys/fs/cgroup/system.slice/simmis-dev.service/cgroup.events")]
    (when-not (and (= "masked" (get state "LoadState"))
                   (#{"inactive" "failed"} (get state "ActiveState"))
                   (= "0" (get state "MainPID")))
      (refuse! :service-must-be-stopped-and-runtime-masked))
    (when (.exists cgroup)
      (when-not (= "0" (get (into {} (map #(str/split % #" ")) (str/split-lines (slurp cgroup))) "populated"))
        (refuse! :service-descendants-remain)))
    (doseq [line (remove str/blank? (str/split-lines (command ["ss" "-ltnH"])))
            :let [port (last (str/split (nth (str/split (str/trim line) #"\s+") 3) #":"))]]
      (when (#{"47295" "47888" "47889"} port) (refuse! :application-listener-remains)))))

(defn- canonical-path [value]
  (when-not (and (string? value) (str/starts-with? value "/")) (refuse! :absolute-path-required))
  (let [path (.normalize (.toAbsolutePath (.toPath (io/file value))))
        real (.toRealPath path (make-array LinkOption 0))]
    (when-not (= path real) (refuse! :symlink-or-path-alias))
    real))

(defn- private-file! [^Path path]
  (when-not (and (Files/isRegularFile path (make-array LinkOption 0))
                 (empty? (set/intersection
                          (set (Files/getPosixFilePermissions path (make-array LinkOption 0)))
                          #{PosixFilePermission/GROUP_READ PosixFilePermission/GROUP_WRITE
                            PosixFilePermission/GROUP_EXECUTE PosixFilePermission/OTHERS_READ
                            PosixFilePermission/OTHERS_WRITE PosixFilePermission/OTHERS_EXECUTE})))
    (refuse! :private-regular-file-required)))

(defn- read-private [path]
  (let [path (canonical-path path)]
    (private-file! path)
    (when (> (Files/size path) 1048576) (refuse! :manifest-too-large))
    (with-open [r (java.io.PushbackReader. (io/reader (.toFile path)))]
      (let [opts {:readers {} :default (fn [& _] (refuse! :tagged-input)) :eof ::eof}
            value (edn/read opts r)]
        (when (or (= ::eof value) (not= ::eof (edn/read opts r))) (refuse! :invalid-input))
        value))))

(def allowed-config-keys
  #{:store :writer :branch :index :index-config :attribute-refs? :crypto-hash?
    :keep-history? :commit-graph? :schema-flexibility :search-cache-size :store-cache-size})

(defn- config! [root config]
  (when-not (and (map? config) (set/subset? (set (keys config)) allowed-config-keys)
                 (= :file (get-in config [:store :backend]))
                 (uuid? (get-in config [:store :id]))
                 (= :self (get-in config [:writer :backend]))
                 (= :db (:branch config))) (refuse! :unsupported-config))
  (let [path (canonical-path (get-in config [:store :path]))
        storage (.resolve ^Path root ".dvergr")]
    (when-not (and (.startsWith path storage) (not= path storage)) (refuse! :store-outside-installation))
    (when-not (d/database-exists? config) (refuse! :missing-database))
    path))

(defn- write-result! [^Path root name value]
  (when-not (and (string? name) (re-matches #"[a-z0-9-]+\.edn" name)) (refuse! :invalid-result-name))
  (let [path (.resolve root name)
        attrs (into-array FileAttribute [(PosixFilePermissions/asFileAttribute
                                          (PosixFilePermissions/fromString "rw-------"))])]
    (Files/createFile path attrs)
    (with-open [writer (io/writer (.toFile path))] (.write writer (str (pr-str value) "\n")))
    (with-open [channel (FileChannel/open path (into-array java.nio.file.OpenOption [StandardOpenOption/WRITE]))]
      (.force channel true))
    (with-open [channel (FileChannel/open root (make-array java.nio.file.OpenOption 0))]
      (.force channel true))
    path))

(defn release! [conn]
  (let [kabel? (some? (:datahike/retire! (meta conn)))]
    (d/release conn)
    (when kabel?
      (let [cleanup (connector/await-release! conn)
            [result selected] (async/alts!! [cleanup (async/timeout 30000)] :priority true)]
        (when (or (not (identical? selected cleanup)) (not (true? result)))
          (refuse! :store-cleanup-unconfirmed))))))

(defn verify-recovery-request! [db request]
  (when-not (= request (some-> (receipts/in db (:id request)) receipts/request-of))
    (refuse! :recovery-manifest-does-not-match-journal))
  request)

(defn run!
  "Manifest selects only fixed operations and exact protected source configs.
   Installation backup and exclusion of independent writers belong to the
   operator harness. The CLI never creates a database or starts an application."
  [manifest-path]
  (let [{:keys [version operation installation-root evidence-root control-config repository-config
                request inspection-file result-name expected-control-store] :as manifest} (read-private manifest-path)]
    (when-not (and (= 1 version) (#{:inspect :upgrade :recover} operation)
                   (= #{:version :operation :installation-root :evidence-root :control-config
                        :repository-config :request :inspection-file :result-name :expected-control-store} (set (keys manifest))))
      (refuse! :invalid-manifest))
    (require-stopped!)
    (let [root (canonical-path installation-root) evidence (canonical-path evidence-root)
          control-path (config! root control-config) repo-path (config! root repository-config)]
      (when (or (= control-path repo-path) (.startsWith evidence root)) (refuse! :unsafe-evidence-or-store-path))
      ;; Bind physical paths to the actual installation mapping, not merely
      ;; any copied store with the same embedded UUID beneath .dvergr.
      (when-not (and (uuid? expected-control-store)
                     (= expected-control-store (get-in control-config [:store :id]))
                     (= (get-in request [:target :store]) (get-in repository-config [:store :id]))
                     (= control-path (.resolve root ".dvergr/system-db"))
                     (= repo-path (canonical-path (str (get-in request [:target :scope]) "/datahike"))))
        (refuse! :physical-store-mapping-mismatch))
      (let [control (d/connect control-config)]
        (try
          ;; In recovery the journal is the authority for the frozen target.
          ;; Match it BEFORE opening the repository selected by the manifest.
          (when (= :recover operation) (verify-recovery-request! @control request))
          (let [repo (d/connect repository-config)]
            (try
              (require-stopped!)
              (let [result (case operation
                             :inspect (upgrade/inspect! control repo request)
                             :upgrade (let [inspection (read-private inspection-file)]
                                        (when-not (= request (:request inspection)) (refuse! :inspection-mismatch))
                                        (upgrade/upgrade! control repo inspection))
                             :recover (upgrade/recover! control repo (:id request)))]
                (require-stopped!)
                (write-result! evidence result-name result)
                {:operation operation :status (if (= :inspect operation) :inspected (:status result))
                 :id (:id request) :evidence result-name})
              (finally (release! repo))))
          (finally (release! control)))))))

(defn -main [& args]
  (try
    (when-not (= 1 (count args)) (refuse! :one-private-manifest-required))
    (println (pr-str (run! (first args))))
    (shutdown-agents)
    (System/exit 0)
    (catch Throwable error
      (binding [*out* *err*]
        (println "Offline Memory upgrade failed; keep ingress gated and inspect the journal."
                 (pr-str (select-keys (ex-data error) [:reason :type]))))
      (shutdown-agents)
      (System/exit 1))))
