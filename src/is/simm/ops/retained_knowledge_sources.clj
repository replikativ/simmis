(ns is.simm.ops.retained-knowledge-sources
  "Read frozen workflow citations through their canonical Request or Proposal.
   Never resolves a current workspace, hydrates a Run, executes or adopts."
  (:require [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.summary-reader :as summary-reader]
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

(defn verify-request!
  "Pure integrity verification of one frozen request. Does not authorize.
   Internal result includes the verifier environment for Proposal integrity
   checks. Wire readers project only manifest-selected original sources."
  [room request row retained]
  (let [input (:input retained)
        basis (publication/validate! (:knowledge/publication-basis input))
        target (:target basis)]
    (when-not (and (= #{:kind :limits :input} (set (keys retained)))
                   (map? (:limits retained)) (map? input)
                   (= :workflow (:kind retained) (:work.request/kind row))
                   (= (:work.request/intent-id row) (hasch/uuid retained))
                   (= room (:work.request/room row))
                   (= request (:work.request/id row))
                   (or (not (contains? input :knowledge/source-citations))
                       (= {:version 1 :room room :request request} (:knowledge/source-citations input)))
                   (= (:id basis) (:work.request/publication-basis-id row))
                   (or (not (contains? input :knowledge/source-citations))
                       (let [citation (:knowledge/source-citations input)]
                         (and (= #{:version :room :request} (set (keys citation)))
                              (= 1 (:version citation))
                              (= room (:room citation)) (= request (:request citation)))))
                   (publication/host-target? target)
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
    (let [{:keys [environment workflow-bundle captured]} (filing/verify-frozen-workflow! input)]
      {:request request :selection (get-in input [:workflow :input-selection])
       :captured captured :basis basis :environment environment :workflow-bundle workflow-bundle})))

(defn- verified! [room proposal owner manifest row retained]
  (let [{:keys [basis environment workflow-bundle] :as verified}
        (verify-request! room (:work.request/id row) row retained)
        binding (dispositions/request-binding row)
        disposition (dispositions/disposition row)]
    (when-not (and (= room (:room manifest))
                   (= (:work.request/id row) (:request manifest) (:request disposition))
                   (= :proposed (:kind disposition))
                   (= proposal (:proposal disposition) (:proposal/id owner))
                   (= (:id manifest) (:manifest disposition))
                   (= (:work.request/by binding) (:proposal/author owner))
                   (= (:candidate disposition) (:attempt manifest) (:proposal/run owner))
                   (= (:output disposition) (:output manifest))
                   (= (:id basis) (:basis disposition) (:basis manifest))
                   (= (:target basis) (:target manifest))
                   (= (hasch/uuid environment) (:environment manifest))
                   (= workflow-bundle (:workflow manifest)))
      (unavailable!))
    (select-keys verified [:request :selection :captured])))

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


(defn- with-knowledge-request-sources! [host party room request f]
  (when-not (every? uuid? [party room request]) (unavailable!))
  (let [{conn :control-conn execution-ctx :execution-ctx} (::reads/owner host)
        principal {:principal/kind :human :principal/id (str "party/" party)}]
    (binding [ec/*execution-context* execution-ctx]
      (let [db (reads/current-host-control-db! host)]
        ;; Authority and exact room binding precede ALL retained artifact I/O.
        (reads/authorize-read! db principal (str room))
        (let [row (d/pull db '[*] [:work.request/id request])
              binding (select-keys row [:work.request/id :work.request/room :work.request/kind
                                        :work.request/by :work.request/intent-id :work.request/payload-ref
                                        :work.request/publication-basis-id])]
          (when-not (= room (:work.request/room row)) (unavailable!))
          (let [retained (:value (artifact/get-bounded-value
                                  (artifact/datahike-store conn) (:work.request/payload-ref row)
                                  max-payload-bytes))
                verified (verify-request! room request row retained)
                result (f verified)
                current (reads/current-host-control-db! host)]
            ;; Recheck the original owner and authority at the captured App's
            ;; operation boundary. Status/disposition changes do not change the
            ;; immutable request identity or invalidate an adopted citation.
            (reads/authorize-read! current principal (str room))
            (when-not (= binding (select-keys (d/pull current '[*] [:work.request/id request])
                                              (keys binding)))
              (unavailable!))
            result))))))

(defn- with-request-sources! [host party room request f]
  (when-not (every? uuid? [party room request]) (unavailable!))
  (let [principal {:principal/kind :human :principal/id (str "party/" party)}
        db (reads/current-host-control-db! host)
        _ (reads/authorize-read! db principal (str room))
        row (requests/request-in db request)
        _ (when (and row (not= room (:work.request/room row))) (unavailable!))]
    (if (or (nil? row) (:work.request/summary-revision row))
      ;; An independently retained native summary survives Request retirement.
      ;; The pointer only selects the reader; it is never source authority.
      (summary-reader/with-request!
       host principal room request
       (fn [verified]
         (when (and row (not= (:work.request/summary-revision row) (:revision verified)))
           (unavailable!))
         (f verified)))
      (with-knowledge-request-sources! host party room request f))))

(defn- request-identity-fields [room {:keys [request selection kind revision]}]
  (cond-> {:room (str room) :request (str request)
           :selection (into {} (map (fn [key] [key (str (get selection key))]))
                            (if (= :room-summary kind) [:coverage-id :context-id :manifest-id]
                                [:trigger-id :execution-id :manifest-id]))}
    (= :room-summary kind) (assoc :kind kind :revision (str revision))))

(defn- request-listing [host party room request]
  (with-request-sources! host party room request
    (fn [{:keys [captured] :as verified}]
      (assoc (request-identity-fields room verified)
             :files (mapv (fn [{:keys [input path role size content]}]
                            {:path input :source-path path :role role :size size :content-id (str content)})
                          (get-in captured [:manifest :files]))))))

(defn- request-chunk [host party room request opts]
  (when-not (m/validate results/file-options-schema opts) (unavailable!))
  (with-request-sources! host party room request
    (fn [{:keys [captured] :as verified}]
      (let [{:keys [path offset length] :or {offset 0 length results/max-chunk-bytes}} opts
            entry (first (filter #(= path (:input %)) (get-in captured [:manifest :files])))
            _ (when-not entry (unavailable!))
            bytes (markdown/source-bytes (get-in captured [:inputs path]))
            size (alength bytes)]
        (when (> offset size) (unavailable!))
        (let [end (+ offset (min length (- size offset)))]
          (cond-> (merge (request-identity-fields room verified)
                        {:path path :content-id (str (:content entry)) :size size :offset offset
                         :length (- end offset) :encoding "base64" :media-type "text/plain; charset=utf-8"
                         :eof? (= end size)
                         :base64 (.encodeToString (Base64/getEncoder)
                                                  (Arrays/copyOfRange ^bytes bytes (int offset) (int end)))})
            (< end size) (assoc :next-offset end)))))))

(defn request-list!
  "Current Room reader's manifest-listed originals, independent of output or
   Proposal lifecycle. HOST arity needs the captured App operation owner."
  ([party room request]
   (safely #(application/with-operation! (fn [host] (request-listing host party room request)))))
  ([host party room request]
   (safely #(request-listing host party room request))))

(defn request-read!
  "One frozen original chunk; never substitutes today's Memory for missing evidence."
  ([party room request opts]
   (safely #(application/with-operation! (fn [host] (request-chunk host party room request opts)))))
  ([host party room request opts]
   (safely #(request-chunk host party room request opts))))
