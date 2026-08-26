(ns build
  "Uberjar build for simmis.

   Usage:
     clojure -T:build uber        ; full build: CLJS release + uberjar
     clojure -T:build uber :skip-cljs true   ; reuse existing public/js

   Produces target/simmis-<version>-standalone.jar — a self-contained,
   freely-distributable artifact:
     java -jar target/simmis-*-standalone.jar

   Design:
   - NO AOT — avoids the datahike/spindel AOT hazards entirely, at the
     cost of ~10s startup. Canonical invocation (systemd unit + docs):
       java -cp target/simmis-*-standalone.jar clojure.main -m is.simm.runtimes.web
   - The release CLJS build (public/) is copied INTO the jar under
     public/, served via the classpath fallback in
     is.simm.runtimes.web's static handler. A public/ DIRECTORY next to
     the process still overrides (deploy-dir hot-patch seam).
   - Config is runtime-only: env vars (SIMMIS_WS_URL, SIMMIS_JWT_SECRET,
     SIMMIS_NREPL_PORT) + config.local.edn in the working directory.
     Nothing secret is baked into the artifact."
  (:require [clojure.tools.build.api :as b]))

(def lib 'is.simm/simmis)
(def version (format "0.1.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")
(def uber-file (format "target/simmis-%s-standalone.jar" version))

(defn- basis []
  (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn cljs-release
  "Build the release browser bundle and stylesheet."
  [_]
  (println "Building browser release (CLJS + CSS)...")
  (let [{:keys [exit]} (b/process {:command-args ["npm" "run" "release"]})]
    (when-not (zero? exit)
      (throw (ex-info "browser release failed" {:exit exit})))))

(defn uber
  "Build the standalone uberjar. Pass :skip-cljs true to reuse the
   existing public/js (e.g. when iterating on server code only)."
  [{:keys [skip-cljs]}]
  (clean nil)
  (when-not skip-cljs
    (cljs-release nil))
  (println "Copying sources + static assets...")
  (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
  ;; The web app: index.html, css, release js. Excluded: cljs-runtime
  ;; (dev-only), source maps stay (small, useful for beta debugging).
  (b/copy-dir {:src-dirs ["public"]
               :target-dir (str class-dir "/public")
               :ignores [#"js/cljs-runtime/.*" #"dev-session\.json"]})
  (println "Building uberjar" uber-file "...")
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis (basis)
           ;; No :main / no AOT — launch with:
           ;;   java -cp simmis.jar clojure.main -m is.simm.runtimes.web
           ;; (systemd unit + README document this canonical invocation).
           :exclude [#"META-INF/license/.*" #"license/.*"]})
  (println "Done:" uber-file))
