(ns is.simm.runtimes.instance
  "Where this simmis instance keeps its state and which ports it listens on.

   Everything an instance owns on one machine is resolved here, so that a
   second instance — an agent's end-to-end run next to the user's demo — can be
   moved out of the first one's way with environment variables alone. With NONE
   of them set, every value below is exactly what simmis used before this
   namespace existed: the ports 47888/9631/9630/8080/47295, the home `.dvergr`
   and the data root `data`, both relative to the working directory.

     SIMMIS_PORT_OFFSET  Added to every dev port (see `default-ports`). One
                         number rather than a variable per port: the ports only
                         work as a SET — the page's websocket URL, shadow's
                         proxy target and the server's bind must agree — and an
                         offset cannot move one of them without the others.
     DVERGR_HOME         The state home (system DB, room stores, JWT and other
                         generated secrets). dvergr's own variable, honoured by
                         `dvergr.substrate.paths`; simmis's own files in the
                         home resolve through the same function rather than a
                         second name for the same place.
     SIMMIS_DATA_DIR     simmis's own stores (`data/simmis-v2`, KBs, blobs,
                         drives, pages, screens, recordings, mail).
     DVERGR_CONFIG       The config file instead of ./config.local.edn
                         (dvergr's variable, read by `dvergr.substrate.config`).

   `SIMMIS_WS_URL` and `SIMMIS_NREPL_PORT` are DEPLOYMENT settings and keep
   their meaning: an explicit websocket URL wins over the offset."
  (:require [clojure.string :as str]
            [dvergr.substrate.paths :as paths]))

(def default-ports
  "The dev ports of an instance with no offset.

     :nrepl-clj    the dev JVM's nREPL (clj-nrepl-eval)
     :nrepl-cljs   shadow-cljs's nREPL
     :shadow-http  shadow-cljs's own server (devtools websocket, UI)
     :http         shadow's :dev-http, which serves the page
     :websocket    the simmis server (kabel websocket + HTTP routes)"
  {:nrepl-clj 47888
   :nrepl-cljs 9631
   :shadow-http 9630
   :http 8080
   :websocket 47295})

(defn parse-port-offset
  "The offset in `s`; 0 when blank or absent. Anything else that is not a
   non-negative integer throws, rather than starting on the default ports
   that the setting existed to stay off."
  [s]
  (let [s (some-> s str/trim)]
    (if (str/blank? s)
      0
      (let [n (parse-long s)]
        (when-not (and n (<= 0 n 10000))
          (throw (ex-info (str "SIMMIS_PORT_OFFSET must be an integer in 0..10000, got " (pr-str s))
                          {:value s})))
        n))))

(defn port-offset [] (parse-port-offset (System/getenv "SIMMIS_PORT_OFFSET")))

(defn development-bind-address
  "The open development token minter must never listen on the LAN. Normal
   hosted/password authentication preserves the existing binding behavior."
  ([] (development-bind-address (System/getenv "SIMMIS_DEV_AUTH")))
  ([setting] (when (= "true" setting) "127.0.0.1")))

(defn ports
  "The dev ports, offset by `offset` (default: SIMMIS_PORT_OFFSET)."
  ([] (ports (port-offset)))
  ([offset] (update-vals default-ports #(+ % offset))))

(defn port [k] (get (ports) k))

(defn websocket-url
  "The URL the simmis server binds. SIMMIS_WS_URL when set (deployment),
   else localhost on the instance's websocket port."
  []
  (or (not-empty (System/getenv "SIMMIS_WS_URL"))
      (str "ws://localhost:" (port :websocket))))

(defn blob-cookie-name
  "The blob-read cookie's name. Cookies are scoped by host, NOT by port, so two
   instances on localhost would overwrite each other's cookie on every login and
   renewal (the other instance's token then fails validation: a different JWT
   secret). The default instance keeps the established name; an offset
   instance appends its offset."
  ([] (blob-cookie-name (port-offset)))
  ([offset] (if (zero? offset) "simmis_blob" (str "simmis_blob_" offset))))

;; =============================================================================
;; State
;; =============================================================================

(defn- relative-to-cwd
  "`abs` relative to the working directory when it lies inside it, else `abs`.
   Keeps paths that end up PERSISTED (a secondary index's directory in a room
   DB) in the same relative form they had before the home was configurable, so
   a home that moves along with its checkout keeps working."
  [^String abs]
  (let [cwd (.toPath (java.io.File. (System/getProperty "user.dir")))
        p (.toPath (java.io.File. abs))]
    (if (.startsWith p cwd)
      (str (.relativize cwd p))
      abs)))

(defn home-path
  "A path under the state home (DVERGR_HOME, else `.dvergr`), e.g.
   (home-path \"jwt-secret\") => \".dvergr/jwt-secret\" by default.
   Creates the parent directory, as `dvergr.substrate.paths/path` does."
  [& segments]
  (relative-to-cwd (apply paths/path segments)))

(defn data-dir
  "simmis's data root: SIMMIS_DATA_DIR, else \"data\"."
  []
  (or (some-> (System/getenv "SIMMIS_DATA_DIR") str/trim not-empty
              (str/replace #"/+$" ""))
      "data"))

(defn data-path
  "A path under the data root, as the plain string the stores have always been
   configured with: (data-path \"simmis-kbs\" id) => \"data/simmis-kbs/<id>\"."
  [& segments]
  (str/join "/" (cons (data-dir) segments)))

(defn summary
  "What this instance resolved, for the boot log and `(status)`."
  []
  {:port-offset (port-offset)
   :ports (ports)
   :home (paths/home)
   :data-dir (data-dir)
   :config (or (System/getenv "DVERGR_CONFIG") "./config.local.edn")})
