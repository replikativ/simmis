(ns is.simm.ops.retained-knowledge-sources
  "Read frozen workflow citations through their canonical Proposal component.
   Never resolves a current workspace, hydrates a Run, executes or adopts."
  (:require [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-storage :as storage]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.results :as results]
            [is.simm.ops.workflow-policy :as policy]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.application-ops :as application]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Arrays Base64]))

;; Admission bounds source/prepared/submission text to 2 MiB each (up to 6x
;; EDN escaping). The encoded publication basis is <=8 MiB; its target also
;; appears in verifier, setup and source-origin, each bounded by that basis.
;; 36+8+24 MiB plus <=8 MiB paths/parameters/tasks/wrappers fits below128 MiB.
;; This is a reader ceiling, NOT a wider admission limit or unbounded fallback.
(def max-payload-bytes (* 128 1024 1024))

(defn- unavailable! []
  (throw (ex-info "Retained knowledge sources are unavailable" {:type ::unavailable})))

(defn- text-size [text]
  (when (string? text) (alength (markdown/source-bytes text))))

(defn- source-origin! [room input target]
  (let [{:keys [revision target-stamp manifest] :as origin} (:source-origin input)
        source (:source input)
        name (get-in input [:submission :name])]
    (when-not (and (map? source) (<= 1 (count source) workflows/max-files)
                   (vector? manifest) (= (count source) (count manifest))
                   (every? uuid? [revision target-stamp])
                   (m/validate workflows/name-schema name)
                   (= target (:target origin))
                   (= revision (get-in input [:submission :expected-owned-source]))
                   (= revision (sources/source-id {:room-id room :target-stamp target-stamp}
                                                  name manifest))
                   (= (set (keys source)) (set (map :path manifest))))
      (unavailable!))
    (doseq [{:keys [path content size mode] :as entry} manifest]
      (let [text (get source path)]
        (when-not (and (= #{:path :content :size :mode} (set (keys entry)))
                       (string? path) (<= 1 (count path) 512)
                       (integer? mode) (= 0x8000 (bit-and mode 0xF000))
                       (integer? size) (<= 0 size workflows/max-file-bytes)
                       (= size (text-size text))
                       (= content (blob/blob-id (markdown/source-bytes text))))
          (unavailable!))))
    (when (> (reduce + 0 (map :size manifest)) workflows/max-bundle-bytes)
      (unavailable!))))

(defn- verified! [room proposal owner manifest row retained]
  (let [binding (dispositions/request-binding row)
        disposition (dispositions/disposition row)
        input (:input retained)
        basis (publication/validate! (:knowledge/publication-basis input))
        target (:target basis)]
    (when-not (and (= #{:kind :limits :input} (set (keys retained)))
                   (map? (:limits retained)) (map? input)
                   (= :workflow (:kind retained) (:work.request/kind row))
                   (= (:work.request/intent-id row) (hasch/uuid retained))
                   (= room (:work.request/room row) (:room manifest))
                   (= (:work.request/id row) (:request manifest) (:request disposition))
                   (= :proposed (:kind disposition))
                   (= proposal (:proposal disposition) (:proposal/id owner))
                   (= (:id manifest) (:manifest disposition))
                   (= (:work.request/by binding) (:proposal/author owner))
                   (= (:candidate disposition) (:attempt manifest) (:proposal/run owner))
                   (= (:output disposition) (:output manifest))
                   (= (:id basis) (:basis disposition) (:basis manifest)
                      (:work.request/publication-basis-id row))
                   (= target (:target manifest)) (publication/host-target? target)
                   (map? (:inputs input))
                   (<= (count (:inputs input)) (:max-input-files policy/policy))
                   (every? (fn [[path text]]
                             (and (string? path) (<= 1 (count path) 512)
                                  (string? text) (<= (text-size text) workflows/max-file-bytes)))
                           (:inputs input))
                   (<= (reduce + 0 (map text-size (vals (:inputs input))))
                       (:max-input-bytes policy/policy)))
      (unavailable!))
    (source-origin! room input target)
    (let [{:keys [environment workflow-bundle captured]} (filing/verify-frozen-workflow! input)
          selection (get-in input [:workflow :input-selection])]
      (when-not (and (= (hasch/uuid environment) (:environment manifest))
                     (= workflow-bundle (:workflow manifest)))
        (unavailable!))
      {:request (:work.request/id row) :selection selection :captured captured})))

(defn- with-sources! [host party proposal component f]
  (when-not (every? uuid? [party proposal component]) (unavailable!))
  (let [{conn :control-conn execution-ctx :execution-ctx} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [db (reads/current-host-control-db! host)
            owner (manifests/owner db component)
            room (:room owner)
            principal {:principal/kind :human :principal/id (str "party/" party)}]
        (when-not (and owner (= proposal (:proposal owner))) (unavailable!))
        (reads/authorize-read! db principal (str room))
        (let [manifest (:manifest (storage/retained-in db component))
              header (d/pull db [:proposal/id :proposal/room :proposal/author :proposal/run]
                             (:proposal-eid owner))
              row (d/pull db '[*] [:work.request/id (:request manifest)])]
          (when-not (= room (:proposal/room header)) (unavailable!))
          (let [retained (:value (artifact/get-bounded-value
                                  (artifact/datahike-store conn) (:work.request/payload-ref row)
                                  max-payload-bytes))
                verified (verified! room proposal header manifest row retained)
                result (f verified)
                current (reads/current-host-control-db! host)]
            ;; Current permission and the component's original owner must still
            ;; hold after reading/formatting; a URL is not a retained grant.
            (reads/authorize-read! current principal (str room))
            (when-not (= owner (manifests/owner current component)) (unavailable!))
            result))))))

(defn- safely [f]
  (try {:status :ok :data (f)}
       (catch Throwable error
         (when (adoption/causal-interruption? error) (.interrupt (Thread/currentThread)))
         {:status :unavailable})))

(defn- identity-fields [proposal component {:keys [request selection]}]
  {:proposal (str proposal) :component (str component) :request (str request)
   :selection (into {} (map (fn [key] [key (str (get selection key))]))
                    [:trigger-id :execution-id :manifest-id])})

(defn- listing
  "Current room readers inspect only original manifest-listed frozen sources.
   Resolved Proposals remain readable. No artifact id or path supplied by caller."
  [host party proposal component]
  (with-sources! host party proposal component
      (fn [{:keys [captured] :as verified}]
        (assoc (identity-fields proposal component verified)
               :files (mapv (fn [{:keys [input path role size content]}]
                              {:path input :source-path path :role role :size size
                               :content-id (str content)})
                            (get-in captured [:manifest :files]))))))

(defn- chunk
  "One <=16 KiB base64 chunk from the exact retained selection. Reassemble
   bytes before UTF-8 decoding; a character may straddle chunk boundaries."
  [host party proposal component opts]
  (do
     (when-not (m/validate results/file-options-schema opts) (unavailable!))
     (with-sources! host party proposal component
       (fn [{:keys [captured] :as verified}]
         (let [{:keys [path offset length] :or {offset 0 length results/max-chunk-bytes}} opts
               entry (first (filter #(= path (:input %)) (get-in captured [:manifest :files])))
               _ (when-not entry (unavailable!))
               bytes (markdown/source-bytes (get-in captured [:inputs path]))
               size (alength bytes)]
           (when (> offset size) (unavailable!))
           (let [end (+ offset (min length (- size offset)))]
             (cond-> (merge (identity-fields proposal component verified)
                            {:path path :content-id (str (:content entry)) :size size :offset offset
                             :length (- end offset) :encoding "base64" :media-type "text/plain; charset=utf-8"
                             :eof? (= end size)
                             :base64 (.encodeToString (Base64/getEncoder)
                                                      (Arrays/copyOfRange ^bytes bytes (int offset) (int end)))})
               (< end size) (assoc :next-offset end))))))))

(defn list!
  "List exact frozen sources. The HOST arity requires an already-owned operation on the captured App."
  ([party proposal component]
   (safely #(application/with-operation! (fn [host] (listing host party proposal component)))))
  ([host party proposal component]
   (safely #(listing host party proposal component))))

(defn read!
  "Read one bounded frozen chunk. The HOST arity requires an already-owned operation on the captured App."
  ([party proposal component opts]
   (safely #(application/with-operation! (fn [host] (chunk host party proposal component opts)))))
  ([host party proposal component opts]
   (safely #(chunk host party proposal component opts))))
