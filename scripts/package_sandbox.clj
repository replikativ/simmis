;; Build-only, offline acquisition from an already fetched checkout. Run:
;;   clojure -M scripts/package_sandbox.clj /path/to/dvergr-sandbox
;; Never add that checkout's root (and user.clj) to a host/runtime classpath.
(require '[clojure.java.io :as io]
         '[clojure.string :as str]
         '[is.simm.documents.starter-bundle :as bundle])

(defn git-bytes [checkout & args]
  (let [process (.start (doto (ProcessBuilder. (into ["git" "--no-replace-objects" "-C" checkout] args))
                          (.redirectError java.lang.ProcessBuilder$Redirect/INHERIT)))
        bytes (with-open [input (.getInputStream process)]
                (.readNBytes input (inc bundle/max-resource-bytes)))]
    (when (> (alength bytes) bundle/max-resource-bytes)
      (.destroyForcibly process)
      (.waitFor process)
      (throw (ex-info "Git output exceeds bundle bound" {})))
    (when-not (zero? (.waitFor process)) (throw (ex-info "Pinned Git source unavailable" {:args args})))
    bytes))

(let [checkout (or (first *command-line-args*) (throw (ex-info "Supply an already fetched sandbox checkout" {})))
      actual (str/trim (bundle/decode-utf8 (git-bytes checkout "rev-parse" (str bundle/commit "^{commit}"))))
      _ (when-not (= actual bundle/commit) (throw (ex-info "Unexpected source commit" {})))
      rows (str/split (bundle/decode-utf8 (git-bytes checkout "ls-tree" "-r" "-z" bundle/commit)) #"\u0000")
      files (into (sorted-map)
                  (keep (fn [row]
                          (let [[header path] (str/split row #"\t" 2)
                                [mode kind] (str/split header #" ")]
                            (when (bundle/allowed-path? path)
                              (when-not (and (= mode "100644") (= kind "blob"))
                                (throw (ex-info "Starter accepts only regular non-executable files" {:path path})))
                              [path (bundle/decode-utf8 (git-bytes checkout "show" (str bundle/commit ":" path)))])))) rows)
      _ (when-not (every? #(contains? files %) ["LICENSE" "AGENTS.md" "README.md" "user.clj"
                                                "dvergr/intake/core.clj" "dvergr/intake/schema.clj" "dvergr/mail/inbox.clj"])
          (throw (ex-info "Pinned starter is incomplete" {})))
      packaged (bundle/assemble files)
      destination (io/file "resources" bundle/resource)]
  (io/make-parents destination)
  (spit destination (str (pr-str packaged) "\n") :encoding "UTF-8")
  (println "Packaged" (count files) "files from" actual)
  (println "Review and pin manifest digest:" (:digest packaged)))
