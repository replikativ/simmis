(ns is.simm.ops.room-digest
  "Optional digest preview and certified retained-result reader. No model runner,
   publication, knowledge acknowledgement, live-result fallback or cached authority."
  (:require [clojure.string :as str]
            [datahike.blob :as blob]
            [dvergr.artifact :as artifact]
            [dvergr.runtime.preparation :as preparation]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.access :as access]
            [is.simm.model.conversation-digest :as digest]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-coverage :as edn-value]
            [is.simm.ops.digest-inputs :as digest-inputs]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workflow-authoring :as authoring]
            [is.simm.ops.owned-workflow-admission :as admission]
            [is.simm.ops.workflow-readiness :as readiness]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.worker-files :as worker-files]
            [is.simm.ops.workflow-result :as workflow-result]
            [is.simm.ops.summary-reader :as summary-reader]
            [is.simm.ops.workflow-runs :as workflows-run]
            [is.simm.ops.workflows :as workflows]
            [is.simm.starter.conversation-digest :as starter]
            [org.replikativ.spindel.engine.core :as ec]))

(def max-original-bytes (* 256 1024))
(def max-payload-bytes (* 32 1024 1024))
(def launch-limits {:budget-microdollars 25000 :max-dispatches 4 :max-model-steps 8
                    :timeout-ms 180000 :settlement "discard"})

(defn- refuse! [reason]
  (throw (ex-info "Room digest is unavailable" {:type ::unavailable :reason reason})))

(defn- paths! [paths]
  (when-not (and (vector? paths) (<= 1 (count paths) digest/max-sources)
                 (= (count paths) (count (set paths))))
    (refuse! :invalid-selection))
  (doseq [path paths]
    (files/path! path)
    (when-not (and (<= (count (str "/" path)) 512)
                   (or (str/starts-with? path "inbox/") (str/starts-with? path "transcripts/")
                       (str/starts-with? path "captures/"))
                   (re-find #"(?i)\.(md|markdown)$" path))
      (refuse! :not-original-evidence)))
  paths)

(defn selection-of-inputs
  "Pure generic document-safe refs from exact frozen originals. A path or file's
   authored metadata never establishes human-message provenance or authority."
  [paths input-files]
  (paths! paths)
  (let [artifacts
        (mapv (fn [path]
                (let [text (get input-files (str "/" path))]
                  (when-not (and (string? text) (<= (count text) max-original-bytes))
                    (refuse! :missing-frozen-original))
                  (let [bytes (markdown/source-bytes text) content (blob/blob-id bytes)]
                    (when (> (alength bytes) max-original-bytes) (refuse! :oversized-original))
                    {:source-id (hasch/uuid [:simmis/digest-document-v1 path])
                     :source-revision (hasch/uuid [:simmis/digest-document-revision-v1 path content])
                     :file-content content :path path
                     :source-kind :document :authority :unknown}))) paths)
        total (reduce + 0 (map #(alength (markdown/source-bytes (get input-files (str "/" %)))) paths))]
    (when (> total max-original-bytes) (refuse! :selection-too-large))
    (digest/selection artifacts)))

(defn- with-room! [host principal handle rich? f]
  (let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (when-not (fn? with-operation) (refuse! :missing-lifetime))
    (with-operation
     (fn []
       (let [original (reads/resolve-host-read-room! host principal handle) room (:room original)]
         (binding [ec/*execution-context* (:execution-ctx host)]
           (preparation/with-host-operation!
            room
            (fn [lease]
              (preparation/with-host-current! lease (constantly true))
              (reads/validate-host-read-room! host principal handle original)
              (let [binding (if rich? (reads/resolve-host-read! host principal handle) original)]
                (when-not (identical? room (:room binding)) (refuse! :room-changed))
                (when rich?
                  (owned/validate! (reads/current-host-control-db! host) principal :read
                                   (:room-id original) room binding))
                (let [result (f binding)]
                  (reads/validate-host-read-room! host principal handle original)
                  (when rich?
                    (owned/validate! (reads/current-host-control-db! host) principal :read
                                     (:room-id original) room binding))
                  (preparation/with-host-current! lease (constantly true))
                  result))))))))))

(defn preview!
  "Read-only explicit already-retained originals in the original Room's default
   canonical Memory. Returns frozen generic inputs; creates no saved source/Job."
  [host principal handle request]
  (when-not (and (map? request) (= #{:paths} (set (keys request)))) (refuse! :invalid-request))
  (paths! (:paths request))
  (let [result
  (with-room! host principal handle true
    (fn [binding]
      (let [db (:db binding)
            metadata (mapv #(files/entry db %) (:paths request))]
        (when-not (and (every? #(and (= 33188 (:mode %)) (uuid? (:content %))
                                     (integer? (:size %)) (<= 0 (:size %) max-original-bytes)) metadata)
                       (<= (reduce + 0 (map :size metadata)) max-original-bytes))
          (refuse! :unsupported-original))
        (let [inputs (into (sorted-map)
                           (map (fn [path entry]
                                  (let [text (:text (files/read-file-in db path))]
                                    (when-not (and (string? text)
                                                   (= (:content entry) (blob/blob-id (markdown/source-bytes text))))
                                      (refuse! :original-mismatch))
                                    [(str "/" path) text])) (:paths request) metadata))
              selection (selection-of-inputs (:paths request) inputs)
              snapshot (workflows/snapshot-in db starter/name)
              inspection (when (seq (:manifest snapshot)) (workflows/inspect-snapshot starter/name snapshot :program))]
          {:status :preview :room (:room-id binding) :target-stamp (:target-stamp binding)
           :selection selection :inputs inputs :params {"evidence-manifest" (pr-str selection)}
           :workflow (cond (empty? (:manifest snapshot)) :missing
                           (and (:ok inspection) (digest-inputs/declaration (:files snapshot))) :installed
                           :else :unavailable)
           :source-revision (sources/source-id binding starter/name (:manifest snapshot))
           :bundle (when (:ok inspection) (:bundle inspection)) :limits launch-limits}))))]
    (assoc result :default-model (:default-model (readiness/read-on-host! host principal handle)))))

(defn install!
  "Explicit inactive empty-subtree installation through the existing owned CAS
   writer. Never replaces customized source, grants resources or starts work."
  [host principal handle request]
  (when-not (and (= #{:expected-target-stamp} (set (keys request)))
                 (uuid? (:expected-target-stamp request))) (refuse! :invalid-request))
  (let [binding (reads/resolve-host-install! host principal handle)
        snapshot (workflows/snapshot-in (:db binding) starter/name)
        source (starter/source)]
    (reads/validate-host-install! host principal handle binding)
    (when-not (= (:expected-target-stamp request) (:target-stamp binding)) (refuse! :target-changed))
    (cond
      (and (= source (:files snapshot)) (every? #(= 33188 (:mode %)) (:manifest snapshot)))
      {:status :already-installed}
      (seq (:manifest snapshot)) {:status :customized}
      :else (do (authoring/install! host principal handle
                                    {:name starter/name :expected-revision (str (sources/source-id binding starter/name (:manifest snapshot)))
                                     :files source})
                {:status :installed}))))

(defn launch!
  "One explicit finite ordinary saved-workflow admission. Frozen originals and
   exact source/default identities are rechecked; no lease spans execution."
  [host principal handle {:keys [paths selection-id expected-target-stamp expected-source-revision request-id model] :as request}]
  (when-not (and (= #{:paths :selection-id :expected-target-stamp :expected-source-revision :request-id :model} (set (keys request)))
                 (every? uuid? [selection-id expected-target-stamp expected-source-revision request-id])
                 (string? model) (<= 1 (count model) 200))
    (refuse! :invalid-request))
  (let [preview (preview! host principal handle {:paths paths})]
    (when-not (and (= :installed (:workflow preview))
                   (= selection-id (get-in preview [:selection :id]))
                   (= expected-target-stamp (:target-stamp preview))
                   (= expected-source-revision (:source-revision preview)))
      (refuse! :preview-changed))
    (admission/submit! host principal handle
                       {:request-id request-id :name starter/name :expected-bundle (parse-uuid (:bundle preview))
                        :expected-owned-source expected-source-revision :model model :limits launch-limits
                        :params (:params preview) :inputs (:inputs preview)})))

(defn verify-retained!
  "Pure exact Request/payload/original check. Never consults today's repository.
   The caller separately authorizes the original Room and certifies its Attempt."
  [room request row retained]
  (when-not (and (= #{:kind :limits :input} (set (keys retained)))
                 (= room (:work.request/room row)) (= request (:work.request/id row))
                 (= request (requests/request-id (:work.request/by row) room (:work.request/client-id row)))
                 (= :workflow (:work.request/kind row) (:kind retained))
                 (= (:work.request/intent-id row) (hasch/uuid retained)))
    (refuse! :request-mismatch))
  (let [input (:input retained)
        prepared (workflows-run/prepare-source {:files (:source input)}
                                              (select-keys (:submission input) workflows-run/source-fields))
        selection (edn-value/read-value (get-in input [:workflow :params "evidence-manifest"]))
        _ (digest/validate-selection! selection)
        actual (digest-inputs/validate-inputs! (:source input) (get-in input [:workflow :params]) (:inputs input))]
    (when-not (and (= starter/name (get-in input [:workflow :name]))
                   (nil? (get-in input [:workflow :input-selection]))
                   (not (contains? input :knowledge/publication-basis))
                   (= (:workflow prepared) (:workflow input))
                   (= (:inputs prepared) (:inputs input))
                   (= :simmis/saved-workflow (get-in input [:environment :environment/id]))
                   (= 1 (get-in input [:environment :environment/version]))
                   (= (:workflow prepared) (get-in input [:environment :environment/metadata :workflow]))
                   (= (:task prepared) (get-in input [:environment :environment/task]))
                   (= selection actual))
      (refuse! :frozen-original-mismatch))
    {:selection selection :input input
     :originals (mapv (fn [source] (assoc source :text (get (:inputs input) (str "/" (:path source)))))
                      (:sources selection))}))

(defn- completed-attempt! [room row input]
  (or (workflow-result/completed-attempt room row input)
      (refuse! :not-certified-completion)))

(defn- manual-result!
  "Read a completed generic digest from its certified Attempt and exact retained
   Request originals. Current default files are never a fallback for citations."
  [host principal handle {:keys [request-id] :as request}]
  (when-not (and (= #{:request-id} (set (keys request))) (uuid? request-id)) (refuse! :invalid-request))
  (with-room! host principal handle false
    (fn [binding]
      (let [conn (:control-conn (::reads/owner host))
            row (requests/request-in (reads/current-host-control-db! host) request-id)
            _ (when-not (= (:room-id binding) (:work.request/room row)) (refuse! :request-mismatch))
            retained (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                                                         (:work.request/payload-ref row) max-payload-bytes))
            verified (verify-retained! (:room-id binding) request-id row retained)
            certified (completed-attempt! (:room binding) row (:input verified))
            value (get-in certified [:attempt/evidence :files "/out/digest.edn"])
            _ (when-not (if (string? value) (<= (count value) digest/max-digest-bytes)
                            (and (map? value) (integer? (:bytes value))
                                 (<= 0 (:bytes value) digest/max-digest-bytes)
                                 (string? (:base64 value)) (<= (count (:base64 value)) 87384)))
                (refuse! :oversized-digest))
            bytes (if (string? value) (markdown/source-bytes value) (worker-files/file-bytes! value))]
        (when (> (alength ^bytes bytes) digest/max-digest-bytes) (refuse! :oversized-digest))
        (let [report (digest/validate! (:selection verified) (edn-value/read-value (markdown/decode bytes)))
              current (requests/request-in (reads/current-host-control-db! host) request-id)]
          (when-not (= (select-keys row [:work.request/id :work.request/room :work.request/intent-id :work.request/payload-ref :work.request/job])
                       (select-keys current [:work.request/id :work.request/room :work.request/intent-id :work.request/payload-ref :work.request/job]))
            (refuse! :request-changed))
          {:status :verified :room (:room-id binding) :request request-id
           :digest-id (digest/digest-id (:selection verified) report) :digest report
           :selection (:selection verified) :originals (:originals verified)})))))

(defn result!
  "Read either retained manual digest or native daily summary through the same
   authenticated Room read. No current files substitute for frozen originals."
  [host principal handle {:keys [request-id] :as request}]
  (when-not (and (= #{:request-id} (set (keys request))) (uuid? request-id))
    (refuse! :invalid-request))
  (let [original (reads/preflight-host-read! host principal handle)
        row (requests/request-in (reads/current-host-control-db! host) request-id)]
    (if (or (nil? row) (:work.request/summary-revision row))
      (summary-reader/result! host principal (:room-id original) request-id)
      (manual-result! host principal handle request))))
