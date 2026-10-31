(ns is.simm.ops.knowledge-batch-proof
  "Fixed HOST verification of retained batch/request data. Callers authorize
   the human/room BEFORE entering. No source lookup, Room hydration, provider,
   native source handle, request insertion or dispatch occurs here."
  (:require [clojure.string :as str]
            [dvergr.agent.environment :as environment]
            [dvergr.artifact :as artifact]
            [dvergr.catalog.room :as catalog]
            [dvergr.scheduler.cron :as cron]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.work-limits :as work-limits]
            [is.simm.ops.knowledge-batch-inputs :as inputs]
            [is.simm.ops.knowledge-inputs :as evidence]
            [is.simm.ops.knowledge-citations :as citations]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workflow-snapshot :as snapshot]
            [is.simm.ops.workflow-policy :as policy]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]))

(defn- refuse! [reason]
  (throw (ex-info "Retained knowledge batch proof is unavailable" {:type ::held :reason reason})))

(def control-schema [:map {:closed true} [:mapping :uuid]])

(defn retained-selection!
  "Bounded metadata proof, NOT permission or copied-byte validation. Caller
   authorizes first. Uses retained pin payloads; never opens or recreates a root."
  [conn party room batch-id]
  (when-not (every? uuid? [party room batch-id])
    (refuse! :invalid-origin))
  (let [db @conn row (batches/batch db batch-id)]
    (when-not (and row (= party (:knowledge.batch/by row)) (= room (:knowledge.batch/room row))
                   (= batch-id (:knowledge.batch/selection-id row))
                   (#{:capturing :ready :submitted :review :closed} (:knowledge.batch/state row)))
      (refuse! :batch-header-mismatch))
    (let [store (artifact/datahike-store conn)
          selected (:value (artifact/get-bounded-value store (:knowledge.batch/selection row) (* 2 1024 1024)))]
      (inputs/verify-selection selected)
      (when-not (= batch-id (:id selected)) (refuse! :selection-artifact-mismatch))
      (let [value (:stream selected) current (streams/stream db (:id value))
            pin (pins/intent db (get-in selected [:expected :knowledge.stream/cursor-root]))]
        (when-not (and (= party (:by value)) (= room (:room value))
                       (= (:id value) (:knowledge.batch/stream row))
                       (= (:target value) (:knowledge.batch/target row))
                       (= [party room (:target value)]
                          [(:knowledge.pin/by pin) (:knowledge.pin/room pin) (:knowledge.pin/target pin)])
                       (= (:id value) (:knowledge.pin/stream pin)))
          (refuse! :source-binding-mismatch))
        (when-not current (refuse! :missing-stream))
        (streams/validate-binding! value current)
        (let [original (pins/payload conn pin)]
          (when-not (and (= value (:stream original)) (= (:basis selected) (:basis original))
                         (= (:target value) (pins/target-id (get-in original [:origin :target]))))
            (refuse! :original-source-mismatch))
          {:row row :selection selected :source-origin (:origin original)})))))

(defn retained!
  "Verified data, NOT permission. Header ownership is checked before artifacts.
   The original checkpoint's retained metadata remains usable after its native
   root is released; no historical source DB or live default is opened."
  [conn party room origin]
  (when-not (and (every? uuid? [party room]) (m/validate batches/origin-schema origin))
    (refuse! :invalid-origin))
  (let [row (batches/batch @conn (:batch origin))]
    (when-not (and row (= party (:knowledge.batch/by row)) (= room (:knowledge.batch/room row))
                   (= (:selection origin) (:knowledge.batch/selection-id row))
                   (= (:frozen origin) (:knowledge.batch/frozen-id row))
                   (#{:ready :submitted :review :closed} (:knowledge.batch/state row)))
      (refuse! :batch-header-mismatch)))
  (let [{:keys [row selection source-origin] :as proof} (retained-selection! conn party room (:batch origin))]
    (when-not (and (= (:frozen origin) (:knowledge.batch/frozen-id row))
                   (#{:ready :submitted :review :closed} (:knowledge.batch/state row)))
      (refuse! :batch-header-mismatch))
    (let [frozen (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                          (:knowledge.batch/frozen row) inputs/max-frozen-bytes))]
      (when-not (and (= (:frozen origin) (hasch/uuid frozen))
                     (= selection (:batch frozen)) (= (:selection origin) (:id selection))
                     (= (:target source-origin) (get-in frozen [:publication-basis :target])))
        (refuse! :frozen-artifact-mismatch))
      (inputs/verify frozen)
      (assoc proof :origin origin :frozen frozen))))

(defn ready!
  "Before provider preparation: require a verified, unlinked ready batch.
   Only the original HOST handoff may use this; restart does not grant dispatch."
  [conn party room origin]
  (let [{:keys [row] :as proof} (retained! conn party room origin)
        db @conn current (batches/batch db (:knowledge.batch/id row))
        stream (streams/stream db (:knowledge.batch/stream row))]
    (when-not (and (= row current) stream
                   (= (:knowledge.batch/id row) (:knowledge.stream/active-batch stream))
                   (= :ready (:knowledge.batch/state row)) (nil? (:knowledge.batch/request row)))
      (refuse! :batch-not-ready))
    proof))

(defn- text-map! [entries max-count max-bytes]
  (when-not (and (map? entries) (<= (count entries) max-count)
                 (every? #(and (string? %) (<= (count %) 512)) (keys entries))
                 (every? #(and (string? %) (<= (count %) workflows/max-file-bytes)) (vals entries)))
    (refuse! :oversized-input))
  (reduce (fn [total text]
            (let [size (alength (.getBytes ^String text "UTF-8")) next (+ total size)]
              (when (or (> size workflows/max-file-bytes) (> next max-bytes))
                (refuse! :oversized-input))
              next)) 0 (vals entries)))

(defn verify-input!
  "Match actual prepared/retained input to verified frozen bytes and the
   original program. Pure revalidation, including actual fixture reservations;
   this does not recapture, grant ownership or certify an Attempt."
  [{:keys [origin frozen selection source-origin row] :as proof} input]
  (let [submission (:submission input) name (:name submission)
        workflow (:workflow input) env (environment/validate-environment (:environment input))
        entries (:inputs input) source (:source input) declared (:source-origin input)]
    (text-map! entries (:max-input-files policy/policy) (:max-input-bytes policy/policy))
    (text-map! source workflows/max-files workflows/max-bundle-bytes)
    (when-not (and (map? entries) (<= (count entries) (:max-input-files policy/policy))
                   (every? string? (keys entries)) (every? string? (vals entries))
                   (map? source) (<= 1 (count source) workflows/max-files) (every? string? (vals source))
                   (= origin (:knowledge/batch input))
                   (m/validate control-schema (:knowledge/batch-control input))
                   (= (:workflow source-origin) name (:name workflow))
                   (not (seq (:case submission))) (nil? (:case workflow))
                   (not (seq (:params submission))) (not (seq (:inputs submission)))
                   (= (:target source-origin) (:target declared))
                   (= (:target-stamp source-origin) (:target-stamp declared))
                   (= (:source-revision source-origin) (:revision declared) (:expected-owned-source submission))
                   (= (:revision declared)
                      (sources/source-id {:room-id (:knowledge.batch/room row) :target-stamp (:target-stamp declared)}
                                         name (:manifest declared)))
                   (= (:knowledge/publication-basis input) (:publication-basis frozen))
                   (= workflow (get-in env [:environment/metadata :workflow]))
                   (= (:policy workflow) policy/policy))
      (refuse! :prepared-input-mismatch))
    (let [knowledge (into (sorted-map) (filter #(str/starts-with? (key %) "/in/knowledge/")) entries)
          auxiliary (into (sorted-map) (remove #(str/starts-with? (key %) "/in/knowledge/")) entries)
          reserved {:files (count auxiliary)
                    :bytes (reduce + 0 (map #(alength (.getBytes ^String % "UTF-8")) (vals auxiliary)))}
          program (hasch/uuid [:simmis/knowledge-program-v1
                              (select-keys workflow [:name :bundle :case :params])
                              (citations/semantic-task input) auxiliary])
          bundle (catalog/bundle name source :program)
          declaration (inputs/declaration source)
          captured (:captured frozen)]
      (when-not (and (= knowledge (:inputs captured)) (= reserved (:reserved selection))
                     (= program (get-in selection [:stream :program]))
                     (= (str (:id bundle)) (:bundle workflow) (str (:expected-bundle submission)))
                     (= (:policy declaration) (:batch-policy selection))
                     (= (:epoch declaration) (get-in selection [:stream :epoch]))
                     (= (evidence/declaration source) (get-in captured [:manifest :policy]))
                     (= (:input-selection workflow)
                        {:kind :knowledge :policy (get-in captured [:manifest :policy])
                         :trigger-id (:trigger-id captured) :execution-id (:execution-id captured)
                         :manifest-id (hasch/uuid (:manifest captured))}))
        (refuse! :prepared-program-mismatch)))
    proof))

(def ^:private scheduled-execution-schema
  ;; Incremental batching currently admits only defaults. Ordinary workflow
  ;; case/input/parameter DTOs remain unchanged; no legacy re-interpretation.
  [:map {:closed true} [:name workflows/name-schema] [:expected-bundle :uuid]
   [:expected-owned-source :uuid] [:model [:string {:min 1 :max 200}]]
   [:limits work-limits/schema]
   [:params {:optional true} [:map {:closed true}]]
   [:inputs {:optional true} [:map {:closed true}]]])

(defn schedule-input!
  "Additional original-deployment verification after verify-input!. Caller has
   already authorized the exact party/Room. Bounded control artifacts only;
   no current source, provider, active/ready gate, or dispatch ownership.
   Returns a storage-anchored binding for the fixed writer, or nil for manual."
  [conn party room input]
  (if-let [o (scheduled/bound-occurrence @conn (get-in input [:knowledge/batch :batch]))]
    (let [a (schedules/activation conn (:schedule.occurrence/activation o))]
      (when-not (and a (= [party room] [(:schedule.activation/by a) (:schedule.activation/room a)]))
        (refuse! :schedule-owner-mismatch))
      (let [value (scheduled/retained conn o)
            anchor {:value value :reference (:schedule.occurrence/batch-ref o)}
            target (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                            (:target-ref value) schedules/max-managed-target-bytes))]
        (when-not (and value (= [party room] [(:by value) (:room value)])
                       (m/validate snapshot/deployment-schema target)
                       (m/validate scheduled-execution-schema (:execution target))
                       (= (:revision value) (hasch/uuid target))
                       (= (:cadence target) (cron/normalize-spec (:cadence target)))
                       (= #{(get-in target [:execution :model])} (set (keys (:model-policy target))))
                       (= :workflow (:kind target))
                       (= [party room] [(:by target) (:room target)])
                       (= :owned-retained-source-v2 (get-in target [:host-policy :source]))
                       (= (:batch value) (:knowledge/batch input))
                       (= {:binding (:id value)} (:knowledge/schedule input))
                       (= {:activation (:activation value) :revision (:revision value)
                           :occurrence (:occurrence value) :creation-channel (:creation-channel target)}
                          (:schedule-origin input))
                       (= (assoc (:execution target) :request-id (:occurrence value)) (:submission input))
                       (= (get-in target [:snapshot :files]) (:source input))
                       (= (:source-origin target) (:source-origin input)))
          (refuse! :scheduled-deployment-mismatch))
        (snapshot/verify! room (:execution target) (:snapshot target) (:source-origin target))
        anchor))
    (do
      (when (or (contains? input :knowledge/schedule) (contains? input :schedule-origin))
        (refuse! :unbound-schedule-marker))
      nil)))

(def ^:private request-binding-fields
  [:work.request/id :work.request/client-id :work.request/by :work.request/room :work.request/kind
   :work.request/intent-id :work.request/payload-ref :work.request/publication-basis-id
   :work.request/knowledge-batch :work.request/knowledge-frozen :work.request/claim :work.request/created-at])

(defn- canonical-request! [conn row]
  (let [current (requests/request-in @conn (:work.request/id row))]
    (when-not (and current (= (select-keys current request-binding-fields) (select-keys row request-binding-fields)))
      (refuse! :canonical-request-mismatch))
    current))

(defn request!
  "Historical/source-free exact proof, including both sides of joint request
   linkage. No matching submission or request row alone permits a new Job."
  ([conn row]
   (let [row (canonical-request! conn row)]
     (request! conn row (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                                   (:work.request/payload-ref row) (* 128 1024 1024))))))
  ([conn row retained]
   (canonical-request! conn row)
   (when-not (and (= #{:kind :limits :input} (set (keys retained)))
                  (= :workflow (:kind retained) (:work.request/kind row))
                  (= (:work.request/intent-id row) (hasch/uuid retained))
                  (= (:work.request/client-id row) (get-in retained [:input :submission :request-id]))
                  (= (:limits retained) (update (get-in retained [:input :submission :limits]) :settlement keyword))
                  (= (:work.request/publication-basis-id row) (get-in retained [:input :knowledge/publication-basis :id]))
                  (= (:work.request/id row) (requests/request-id (:work.request/by row) (:work.request/room row)
                                                               (:work.request/client-id row))))
     (refuse! :request-payload-mismatch))
   (let [input (:input retained) origin (:knowledge/batch input)
         proof (retained! conn (:work.request/by row) (:work.request/room row) origin)]
     (when-not (= [(:batch origin) (:frozen origin)]
                  [(:work.request/knowledge-batch row) (:work.request/knowledge-frozen row)])
       (refuse! :request-marker-mismatch))
     (verify-input! proof input)
     (let [anchor (schedule-input! conn (:work.request/by row) (:work.request/room row) input)]
       (when anchor (scheduled/linked! @conn anchor row input))
       (when-not (empty? (batches/request-tx @conn origin row (:work.request/intent-id row)))
         (refuse! :partial-request-link))
       (cond-> proof anchor (assoc :schedule anchor))))))
