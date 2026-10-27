(ns is.simm.ops.owned-workflow-authoring
  "Inactive fixed human HOST workflow saves. No route, journal or selected fallback."
  (:require [clojure.string :as str]
            [datahike.blob :as blob]
            [dvergr.effects :as effects]
            [dvergr.rooms.files :as native]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as owned]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m])
  (:import [java.nio CharBuffer]
           [java.nio.charset CharacterCodingException CodingErrorAction StandardCharsets]))

(def ^:private request-schema
  [:map {:closed true}
   [:name workflows/name-schema]
   [:expected-revision [:re #"(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"]]
   [:files [:map-of {:min 1 :max workflows/max-files}
            [:string {:min 1 :max 512}]
            [:maybe [:string {:max workflows/max-file-bytes}]]]]])

(defn- fail! [kind message]
  (throw (ex-info message {:type kind})))

(defn- invalid! [] (fail! ::invalid "Invalid owned workflow edits"))

(defn- interrupted? [failure]
  (loop [cause failure seen #{}]
    (cond (nil? cause) false
          (instance? InterruptedException cause) true
          (contains? seen cause) false
          :else (recur (.getCause ^Throwable cause) (conj seen cause)))))

(defn- storage! [f]
  (try (f)
       (catch Throwable error
         (when (interrupted? error) (.interrupt (Thread/currentThread)))
         (fail! ::unavailable "Owned workflow save unavailable"))))

(defn- strict-utf8! [text]
  (try
    (let [encoder (doto (.newEncoder StandardCharsets/UTF_8)
                    (.onMalformedInput CodingErrorAction/REPORT)
                    (.onUnmappableCharacter CodingErrorAction/REPORT))
          buffer (.encode encoder (CharBuffer/wrap ^String text))
          bytes (byte-array (.remaining buffer))]
      (.get buffer bytes)
      bytes)
    (catch CharacterCodingException _ (invalid!))))

(defn- utf8! [text]
  (let [bytes (strict-utf8! text)]
    (when (> (alength ^bytes bytes) workflows/max-file-bytes) (invalid!))
    bytes))

(defn- validate-edits! [name files]
  ;; NORMAL request validation, not classification of a native/helper Throwable.
  ;; The shared native normalizer is still applied independently afterwards.
  ;; Its broader sensitive-host-path policy is NOT duplicated here. Further
  ;; native refusals remain safe503 before hydration, never tagged as conflict.
  (let [prefix (str "/workflows/" name "/")
        encoded
        (reduce-kv
         (fn [out path text]
           ;; Key identity must also survive UTF-8 without replacement aliases.
           ;; Path validation is independent of the per-FILE body byte limit.
           (strict-utf8! path)
           (let [segments (remove #{"" "."} (str/split path #"/"))
                 target (str prefix (str/join "/" segments))]
             (when (or (str/starts-with? path "/") (re-find #"[\\\p{Cntrl}]" path)
                       (empty? segments) (some #(or (str/blank? %) (= ".." %) (= ".git" (str/lower-case %))) segments)
                       (> (count target) 512) (contains? out target))
               (invalid!))
             (assoc out target (when text (utf8! text))))) {} files)]
    (when (> (reduce + 0 (keep #(some-> % alength) (vals encoded))) workflows/max-bundle-bytes)
      (invalid!))
    encoded))

(defn- after-source [before prefix entries encoded]
  (let [source (reduce-kv
                (fn [{:keys [files tree]} absolute value]
                  (let [path (subs absolute (count prefix))]
                    (if (nil? value)
                      {:files (dissoc files path) :tree (dissoc tree path)}
                      (let [bytes (get encoded absolute)]
                        {:files (assoc files path (:text value))
                         :tree (assoc tree path {:content (blob/blob-id bytes)
                                                 :size (long (alength ^bytes bytes))
                                                 :mode (or (get-in tree [path :mode]) 33188)})}))))
                {:files (:files before)
                 :tree (into (sorted-map) (map (fn [entry] [(:path entry) (dissoc entry :path)])) (:manifest before))}
                entries)
        tree (:tree source)]
    {:files (:files source) :manifest (mapv (fn [[path entry]] (assoc entry :path path)) tree)}))

(defn- write-with-gates!
  [resolve! validate! host principal original-handle {:keys [name expected-revision files] :as request}]
  (when-not (and (m/validate request-schema request)
                 (re-matches #"[a-z0-9][a-z0-9-]{0,63}" name)
                 (re-matches #"(?i)[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" expected-revision))
    (invalid!))
  (let [encoded (validate-edits! name files)
        {:keys [prefix entries]} (storage! #(workflows/normalized-edits name files))
        _ (doseq [[absolute _] entries]
            (when-not (and (.startsWith ^String absolute prefix) (<= (count absolute) 512)) (invalid!)))
        binding (storage! #(resolve! host principal original-handle))
        manifest (storage! #(workflows/manifest-in (:db binding) name))
        revision (storage! #(owned/source-id binding name manifest))]
    (when-not (= (parse-uuid expected-revision) revision)
      (fail! ::conflict "Workflow source changed; reload metadata"))
    (when-not (every? #(#{33188 33261} (:mode %)) manifest) (invalid!))
    (let [before (storage! #(workflows/snapshot-in (:db binding) name))
          after (storage! #(after-source before prefix entries encoded))
          _ (when (or (> (count (:manifest after)) workflows/max-files)
                      (> (reduce + 0 (map :size (:manifest after))) workflows/max-bundle-bytes))
              (invalid!))
          ;; The ACK certifies the complete workflow subtree. Agent file tools
          ;; can leave unchanged siblings untracked, so publishing only the edit
          ;; delta cannot establish the native index/commit proof. Submit every
          ;; inspected AFTER file plus canonical deletions; never stage outside
          ;; this exact CAS-guarded subtree or weaken the native proof.
          publication-entries
          (merge (:entries (storage! #(workflows/normalized-edits name (:files after))))
                 (into {} (filter (comp nil? val)) entries))
          _ (when (> (count publication-entries) native/max-batch-files) (invalid!))
          condition {:prefix prefix :entries (into (sorted-map) (map (fn [entry] [(:path entry) (dissoc entry :path)])) manifest)}
          required {:prefix prefix :entries (into (sorted-map) (map (fn [entry] [(:path entry) (dissoc entry :path)])) (:manifest after))}
          details (storage! #(owned/details-of binding name after))
          base (native/host-boundary (:room binding) (effects/make-sink))
          boundary (fn []
                     (update (base) :handlers conj
                             (fn [effect next]
                               (validate! host principal original-handle binding)
                               (next effect))))
          _ (when-not (contains? native/write-capabilities :expected-subtree)
              (fail! ::unavailable "Owned workflow save unavailable"))
          completion (storage! #(native/write-to-completion!
                                 (:room binding) (:system binding) publication-entries
                                 {:expected-subtree condition :boundary boundary
                                  :message (str "Update workflow " name " by " (:principal/id principal))}))
          result (:result completion)]
      (case (:completion completion)
        :returned (if (and (true? (:applied? result)) (= required (:subtree result)))
                    {:source details :write result :target-stamp (:target-stamp binding)}
                    (fail! ::pending "Workflow save outcome is pending; do not retry automatically"))
        :pending (fail! ::pending "Workflow save outcome is pending; do not retry automatically")
        (fail! ::unavailable "Owned workflow save unavailable")))))

(defn write!
  "Fixed HOST writer. REQUIRED inspected owned revision; no retry journal.
   Success describes the original sealed native ACK, never a later read.
   Pending is not rollback evidence and NEVER authorizes automatic replay."
  [host principal original-handle request]
  (write-with-gates! reads/resolve-host-write! reads/validate-host-write!
                     host principal original-handle request))

(defn install!
  "Fixed HOST install writer using the SAME native CAS/ACK implementation, with
   stricter canonical-space and room-governance checks. No transport or runtime
   activation. Callers cannot supply a gate, callback or authority selector."
  [host principal original-handle request]
  (write-with-gates! reads/resolve-host-install! reads/validate-host-install!
                     host principal original-handle request))
