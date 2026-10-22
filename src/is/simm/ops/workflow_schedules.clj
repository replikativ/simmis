(ns is.simm.ops.workflow-schedules
  "Pinned scheduled work through canonical admission. No new wallet, evaluator
   or execution status; finite allowance is the existing shared room allowance."
  (:require [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.artifact :as artifact]
            [dvergr.agent.attempt :as attempt]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.experiment.runner :as runner]
            [dvergr.agent.run :as run]
            [dvergr.discourse :as discourse]
            [dvergr.jobs :as jobs]
            [dvergr.model.registry :as models]
            [dvergr.resource :as resource]
            [dvergr.room.store :as room-store]
            [dvergr.catalog.room :as catalog]
            [dvergr.rooms.files :as native-files]
            [dvergr.scheduler.cron :as cron]
            [hasch.core :as hasch]
            [is.simm.model.access :as access]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.knowledge-dispositions :as dispositions]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.knowledge-publish :as knowledge-publish]
            [is.simm.ops.knowledge-inputs :as knowledge-inputs]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflows :as owned-sources]
            [is.simm.ops.owned-workspace :as owned-workspace]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as workflows-run]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [taoensso.telemere :as log])
  (:import [java.nio CharBuffer]
           [java.nio.charset CodingErrorAction StandardCharsets]))

(def cadence-schema
  [:map {:closed true}
   [:tz [:string {:min 1 :max 100}]]
   [:interval-ms {:optional true} [:int {:min 60000 :max 31536000000}]]
   [:every {:optional true} [:enum "day" "week" "month"]]
   [:at {:optional true} [:re #"^\d{2}:\d{2}$"]]
   [:on {:optional true} [:enum "monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday"]]
   [:on-day {:optional true} [:int {:min 1 :max 28}]]])

(def create-schema
  [:map {:closed true} [:request-id :uuid] [:kind [:enum "workflow" "benchmark"]]
   [:cadence cadence-schema]
   ;; The exact execution schema is checked below, separately for each kind.
   [:execution :map]])

(defn host-policy []
  {:version 1 :libraries (runner/library-versions) :effects work/effects
   :workflow workflows-run/policy :models :explicit-registry-id
   :input-selection {:version 1 :capture :occurrence :unchanged :skip :failed :hold
                     ;; Alpha policy change intentionally requires redeploying
                     ;; old activations (including generic/benchmark ones).
                     :publication-target :owned-default-v1
                     :processing :durable-filing-v1
                     :cursor :processed-not-adopted}
   :overlap :skip :missed :latest :allowance :shared-room})

(defn model-policy [kind execution]
  ;; Registry refreshes can change concrete ids in place. Freeze their
  ;; execution/pricing metadata digest, not credentials or display names.
  (into (sorted-map)
        (map (fn [id]
               [id (hasch/uuid (select-keys (models/get-model id)
                                            [:id :provider :api-type :model :context :max-output
                                             :pricing :quirks :capabilities]))]))
        (if (= kind :benchmark) (:models execution) [(:model execution)])))

(defn- policy-current? [target]
  (and (= (host-policy) (:host-policy target))
       (= (model-policy (:kind target) (:execution target)) (:model-policy target))))

(defn- cadence! [value]
  (when-not (m/validate cadence-schema value)
    (throw (ex-info "Invalid schedule cadence" {:type ::invalid})))
  (try (java.time.ZoneId/of (:tz value))
       (catch java.time.DateTimeException _
         (throw (ex-info "Invalid schedule timezone" {:type ::invalid}))))
  (when (and (:interval-ms value) (not (zero? (mod (:interval-ms value) 60000))))
    (throw (ex-info "Schedule intervals use whole minutes" {:type ::invalid})))
  (when-not (if (:interval-ms value)
              (not-any? #(contains? value %) [:every :at :on :on-day])
              (and (:every value) (:at value)
                   (= (contains? value :on) (= "week" (:every value)))
                   (= (contains? value :on-day) (= "month" (:every value)))))
    (throw (ex-info "Use one interval or one explicit calendar cadence" {:type ::invalid})))
  (try
    (let [spec (cron/normalize-spec
                (cond-> (dissoc value :interval-ms)
                  (:interval-ms value) (assoc :every-ms (:interval-ms value))
                  (:every value) (update :every keyword)
                  (:on value) (update :on keyword)))]
      ;; Exercise time parsing before publishing an activation.
      (cron/next-fire-time spec (java.time.Instant/now))
      spec)
    (catch IllegalArgumentException _ (throw (ex-info "Invalid schedule time" {:type ::invalid})))
    (catch java.time.DateTimeException _ (throw (ex-info "Invalid schedule time" {:type ::invalid})))))

(defn- authorize! [conn principal room kind]
  ((if (= kind :benchmark) benchmarks/authorize! work/authorize!) conn principal room))

(defn- execution! [kind input]
  (let [schema (if (= kind :benchmark) benchmarks/schema workflows-run/schema)]
    (when-not (and (not (contains? input :request-id))
                   (m/validate schema (assoc input :request-id (random-uuid))))
      (throw (ex-info "Invalid scheduled execution" {:type ::invalid}))))
  ;; A deployment pins a registry id, not a mutable alias/family.
  (doseq [id (if (= kind :benchmark) (:models input) [(:model input)])]
    (when-not (= id (:id (models/get-model id)))
      (throw (ex-info "Select an explicit installed model id for a schedule" {:type ::invalid}))))
  input)

(defn create! [conn ctx principal room-id room input]
  (when-not (m/validate create-schema input)
    (throw (ex-info "Invalid workflow schedule" {:type ::invalid})))
  (binding [ec/*execution-context* ctx]
    (let [kind (keyword (:kind input)) party (authorize! conn principal room-id kind)
          spec (cadence! (:cadence input))
          execution (execution! kind (:execution input))
          id (hasch/uuid [:simmis/schedule-activation-v1 party room-id (:request-id input)])
          ;; Retries after authoring edits observe their admitted revision.
          existing (schedules/activation conn id)]
      (if existing
        (let [prior (schedules/target conn existing)]
          (when-not (= (select-keys prior [:kind :cadence :execution])
                       {:kind kind :cadence spec :execution execution})
            (throw (ex-info "Schedule request identity names different work" {:type ::conflict})))
          existing)
        (let [snapshot (workflows/snapshot room (:name execution))
              request (assoc execution :request-id id)
              _ (if (= kind :benchmark) (benchmarks/prepare snapshot request)
                    (let [input (select-keys execution workflows-run/source-fields)
                          planned (workflows-run/plan-source snapshot input)]
                      (when (contains? (:inputs planned) "/in/knowledge/manifest.edn")
                        (workflows-run/prepare-source snapshot input))))
              target {:version 1 :kind kind :cadence spec :execution execution
                      :snapshot snapshot :host-policy (host-policy)}
              target (assoc target :model-policy (model-policy kind execution))
              now (java.util.Date.)]
          (schedules/create! conn {:id id :room room-id :by party :target target :now now
                                   :next-fire (java.util.Date/from (cron/next-fire-time spec (.toInstant now)))}))))))

;; These LOCAL schemas do not widen existing transports or execution DTOs.
(defn- owned-execution-schema [schema]
  (into [:map {:closed true}]
        (concat (remove #(= :request-id (first %)) (drop 2 schema))
                [[:expected-owned-source :uuid]])))
(def ^:private owned-workflow-schema (owned-execution-schema workflows-run/schema))
(def ^:private owned-benchmark-schema (owned-execution-schema benchmarks/schema))
(def ^:private retained-entry-schema
  [:map {:closed true} [:path [:string {:min 1 :max 512}]] [:content :uuid]
   [:size [:int {:min 0 :max workflows/max-file-bytes}]] [:mode [:int {:min 0 :max Integer/MAX_VALUE}]]])
(def ^:private owned-target-schema
  [:map {:closed true}
   [:version [:= 2]] [:kind [:enum :workflow :benchmark]] [:room :uuid] [:by :uuid]
   [:cadence :map] [:execution :map] [:host-policy :map]
   [:model-policy [:map-of {:max 4} [:string {:min 1 :max 200}] :uuid]]
   [:creation-channel [:enum :host :mcp]]
   [:snapshot [:map {:closed true}
               [:files [:map-of {:max workflows/max-files} [:string {:min 1 :max 512}]
                        [:string {:max workflows/max-file-bytes}]]]
               [:manifest [:vector {:max workflows/max-files} retained-entry-schema]]]]
   [:source-origin [:map {:closed true} [:revision :uuid] [:target-stamp :uuid]
                    [:target publication/target-schema]
                    [:manifest [:vector {:max workflows/max-files} retained-entry-schema]]]]])

(defn- owned-unavailable! []
  (throw (ex-info "Owned schedule admission unavailable; inspect activation before retrying"
                  {:type ::owned-unavailable})))
(defn- causal-interruption? [error]
  (loop [cause error seen #{}]
    (cond (nil? cause) false (instance? InterruptedException cause) true
          (contains? seen cause) false
          :else (recur (.getCause ^Throwable cause) (conj seen cause)))))
(defn- owned-storage! [f]
  (try (f)
       (catch Throwable error
         (when (causal-interruption? error) (.interrupt (Thread/currentThread)))
         (owned-unavailable!))))
(defn- owned-action! [db principal room kind]
  ((if (= kind :benchmark) benchmarks/authorize-in! work/authorize-in!) db principal room))
(defn- owned-preflight! [host principal handle kind]
  (owned-storage!
   #(let [{:keys [control-db room-id] :as current} (reads/preflight-host-read! host principal handle)]
      (owned-action! control-db principal room-id kind)
      current)))
(defn- owned-current! [host principal handle kind original binding]
  (let [current (owned-preflight! host principal handle kind)]
    (when-not (= (select-keys original [:room-id :party]) (select-keys current [:room-id :party]))
      (owned-unavailable!))
    (when binding
      (owned-storage! #(owned-workspace/validate! (:control-db current) principal :merge
                                                  (:room-id original) (:room binding) binding)))
    current))

(defn- owned-existing? [row id original]
  (let [allowed (into #{:db/id} (keep #(when (= "schedule.activation" (namespace (:db/ident %))) (:db/ident %))) schedules/schema)]
    (and (map? row) (every? allowed (keys row))
         (integer? (:db/id row))
         (= id (:schedule.activation/id row))
         (= (:room-id original) (:schedule.activation/room row))
         (= (:party original) (:schedule.activation/by row))
         (uuid? (:schedule.activation/revision row))
         (uuid? (:schedule.activation/target-ref row))
         (boolean? (:schedule.activation/enabled? row))
         (instance? java.util.Date (:schedule.activation/created-at row))
         (instance? java.util.Date (:schedule.activation/next-fire row))
         (every? #(or (not (contains? row %)) (uuid? (get row %)))
                 [:schedule.activation/last-occurrence :schedule.activation/active-occurrence :schedule.activation/last-processed-input])
         (or (not (contains? row :schedule.activation/reason)) (keyword? (:schedule.activation/reason row))))))

(defn- owned-target! [row target]
  ;; Pure retained structure/provenance; NEVER installed models, current policy,
  ;; hydration, current source, provider preparation or retained authority.
  (when-not (and (m/validate owned-target-schema target)
                 (= (:room target) (:schedule.activation/room row))
                 (= (:by target) (:schedule.activation/by row))
                 (= :owned-retained-source-v2 (get-in target [:host-policy :source]))
                 (= (:cadence target) (cron/normalize-spec (:cadence target)))
                 (m/validate (if (= :benchmark (:kind target)) owned-benchmark-schema owned-workflow-schema) (:execution target))
                 (re-matches #"[a-z0-9][a-z0-9-]{0,63}" (get-in target [:execution :name])))
    (owned-unavailable!))
  (let [{:keys [execution snapshot source-origin kind]} target
        {:keys [files manifest]} snapshot
        expected-models (if (= :benchmark kind) (:models execution) [(:model execution)])
        _ (when-not (and (= (set expected-models) (set (keys (:model-policy target))))
                         (= (:manifest source-origin) manifest)
                         (= (mapv :path manifest) (vec (sort (keys files))))
                         (publication/host-target? (:target source-origin)))
            (owned-unavailable!))
        sizes
        (mapv (fn [{:keys [path size mode content]}]
                (let [encoder (doto (.newEncoder StandardCharsets/UTF_8)
                                (.onMalformedInput CodingErrorAction/REPORT)
                                (.onUnmappableCharacter CodingErrorAction/REPORT))
                      ;; EDN publication must not replace a malformed path's
                      ;; surrogate and silently change retained file identity.
                      _ (.encode encoder (CharBuffer/wrap ^String path))
                      buffer (.encode encoder (CharBuffer/wrap ^String (get files path)))
                      bytes (byte-array (.remaining buffer))
                      full-path (str "/workflows/" (:name execution) "/" path)]
                  (.get buffer bytes)
                  (when-not (and (= full-path (native-files/workspace-path full-path))
                                 (<= (count full-path) 512)
                                 (= 0x8000 (bit-and mode 0xF000))
                                 (= size (alength ^bytes bytes)) (= content (blob/blob-id bytes)))
                    (owned-unavailable!))
                  size)) manifest)
        source-id (owned-sources/source-id {:room-id (:room target) :target-stamp (:target-stamp source-origin)}
                                           (:name execution) manifest)]
    (when-not (and (<= (reduce + 0 sizes) workflows/max-bundle-bytes)
                   (uuid? source-id)
                   (= source-id (:revision source-origin) (:expected-owned-source execution))
                   (= (:expected-bundle execution) (:id (catalog/bundle (:name execution) files))))
      (owned-unavailable!)))
  target)

(defn create-owned!
  "Create an enabled fixed-human HOST activation retaining inspected code.
   Does not synchronously capture inputs, dispatch a Job, fund work or start a
   dispatcher. An already running owned dispatcher may execute it when due."
  [host principal original-handle input]
  (when-not (and (m/validate create-schema input)
                 (m/validate (if (= "benchmark" (:kind input)) owned-benchmark-schema owned-workflow-schema) (:execution input))
                 (re-matches #"[a-z0-9][a-z0-9-]{0,63}" (get-in input [:execution :name])))
    (throw (ex-info "Invalid owned schedule request" {:type ::invalid})))
  (let [kind (keyword (:kind input)) original (owned-preflight! host principal original-handle kind)
        {:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [spec (owned-storage! #(cadence! (:cadence input)))
            id (owned-storage! #(hasch/uuid [:simmis/schedule-activation-v1 (:party original) (:room-id original) (:request-id input)]))
            _ (when-not (uuid? id) (owned-unavailable!))
            existing (owned-storage! #(schedules/activation control-conn id))
            _ (when-not (or (nil? existing) (owned-existing? existing id original)) (owned-unavailable!))]
        (if (some? existing)
          (let [_ (when-not (= {:schedule.activation/id id :schedule.activation/room (:room-id original)
                                :schedule.activation/by (:party original)}
                               (select-keys existing [:schedule.activation/id :schedule.activation/room :schedule.activation/by]))
                    (owned-unavailable!))
                prior (owned-storage! #(schedules/managed-target control-conn existing))
                _ (case (:version prior) 1 nil 2 (owned-storage! #(owned-target! existing prior)) (owned-unavailable!))]
            (when-not (and (= 2 (:version prior))
                           (= {:kind kind :cadence spec :execution (:execution input)}
                              (select-keys prior [:kind :cadence :execution])))
              (throw (ex-info "Schedule request identity names different work" {:type ::conflict})))
            (owned-current! host principal original-handle kind original nil)
            existing)
          (let [binding (owned-storage! #(reads/resolve-host-read! host principal original-handle))
                _ (when-not (= (select-keys original [:room-id :party]) (select-keys binding [:room-id :party])) (owned-unavailable!))
                _ (owned-storage! #(owned-action! (:control-db binding) principal (:room-id binding) kind))
                execution (:execution input)
                manifest (owned-storage! #(workflows/manifest-in (:db binding) (:name execution)))
                revision (owned-storage! #(owned-sources/source-id binding (:name execution) manifest))
                _ (when-not (uuid? revision) (owned-unavailable!))]
            (when-not (= revision (:expected-owned-source execution))
              (throw (ex-info "Workflow source changed; reload metadata" {:type ::conflict})))
            (let [snapshot (owned-storage! #(workflows/snapshot-in (:db binding) (:name execution)))
                  _ (when-not (= manifest (:manifest snapshot)) (owned-unavailable!))
                  _ (owned-current! host principal original-handle kind original binding)
                  legacy (dissoc execution :expected-owned-source)
                  _ (owned-storage! #(execution! kind legacy))
                  _ (owned-storage!
                     #(if (= kind :benchmark)
                        (benchmarks/prepare snapshot (assoc legacy :request-id id))
                        (let [selected (select-keys legacy workflows-run/source-fields)
                              planned (workflows-run/plan-source snapshot selected)]
                          (when (contains? (:inputs planned) "/in/knowledge/manifest.edn")
                            (workflows-run/prepare-source snapshot selected)))))
                  target {:version 2 :room (:room-id original) :by (:party original) :kind kind :cadence spec
                          :execution execution :snapshot snapshot
                          :host-policy (owned-storage! #(assoc (host-policy) :source :owned-retained-source-v2))
                          :model-policy (owned-storage! #(model-policy kind legacy))
                          :creation-channel (if (:principal/mcp? principal) :mcp :host)
                          :source-origin {:revision revision :target-stamp (:target-stamp binding)
                                          :target (:target binding) :manifest manifest}}
                  _ (owned-storage! #(owned-target! {:schedule.activation/room (:room-id original)
                                                     :schedule.activation/by (:party original)} target))
                  ;; Enforce ACTUAL encoded size before the GC-guarded publisher.
                  _ (owned-storage! #(artifact/prepare-value target schedules/max-managed-target-bytes))
                  now (java.util.Date.)
                  next-fire (owned-storage! #(java.util.Date/from (cron/next-fire-time spec (.toInstant now))))]
              (owned-current! host principal original-handle kind original binding)
              (owned-storage! #(schedules/create! control-conn {:id id :room (:room-id original) :by (:party original)
                                                                :target target :now now :next-fire next-fire})))))))))

(defn set-enabled! [conn ctx principal room-id id enabled?]
  (binding [ec/*execution-context* ctx]
    (let [row (schedules/activation conn id)]
      (when-not (= room-id (:schedule.activation/room row))
        (throw (ex-info "No such workflow schedule in this room" {:type ::missing})))
      (let [target (schedules/target conn row)]
        (authorize! conn principal room-id (:kind target))
        (when enabled?
          (when-let [oid (:schedule.activation/active-occurrence row)]
            (when (= :interrupted (:schedule.occurrence/state (schedules/occurrence conn oid)))
              (throw (ex-info "Interrupted admission requires reconciliation before resuming" {:type ::conflict}))))
          ;; The stored delegator, not the caller resuming it, pays/acts.
          (authorize! conn (principals/party-principal (:schedule.activation/by row)) room-id (:kind target))
          (when-not (policy-current? target)
            (throw (ex-info "Host policy changed; admit a new schedule revision" {:type ::conflict}))))
        (schedules/set-enabled! conn id enabled?)))))

(defn- request-id [row occurrence]
  (requests/request-id (:schedule.activation/by row) (:schedule.activation/room row)
                       (:schedule.occurrence/id occurrence)))

(defn- off-engine! []
  (when (simple/on-drain-thread?)
    (throw (ex-info "Scheduled work must run off the engine drain" {:type ::engine-thread}))))

(defn recover!
  "Startup only, after the previous dispatcher is physically gone and canonical
   orphan recovery ran. A claim without a linked request is NOT replayed."
  [conn ctx]
  (off-engine!)
  (binding [ec/*execution-context* ctx]
    (doseq [occurrence (schedules/admitting-occurrences conn)]
      (let [row (schedules/activation conn (:schedule.occurrence/activation occurrence))
            rid (request-id row occurrence) request (requests/get-request conn rid)]
        (schedules/finish-admission! conn (:schedule.occurrence/id occurrence)
                                     (if (= :submitted (:work.request/state request)) :linked :interrupted)
                                     (when request rid) :restart)
        (when-not (= :submitted (:work.request/state request))
          (schedules/block! conn row :interrupted))))))

(defn completion-message [row occurrence target request job]
  (when-not (instance? java.util.Date (:finished-at job))
    (throw (ex-info "Completed Job has no durable completion time" {:type ::job-unavailable})))
  (let [id (hasch/uuid [:simmis/schedule-completion-v1 (:schedule.occurrence/id occurrence)])]
    (assoc (discourse/message
            :simmis/workflows :_room-log
            (str "Scheduled " (name (:kind target)) " · " (get-in target [:execution :name])
                 " · " (name (:status job))
                 "\nOpen the Run for captured files, evaluation results and any proposal. Completion alone does not establish correctness.")
            nil {:role :system :kind :workflow/completed :source :workflow-schedule
                 :run-id (:work.request/job request)})
           :id id :thread-root-id id
           :ts (.getTime ^java.util.Date (:finished-at job)))))

(defn- publish-completion-target! [conn row occurrence request job room target]
  (when-not (:schedule.occurrence/announced? occurrence)
    (when-not room (throw (ex-info "Scheduled result room is unavailable" {:type ::room-unavailable})))
    (binding [ec/*execution-context* (:ctx room)]
      ;; Reserved id and immutable envelope make a crash between these two
      ;; stores retry-safe, like canonical proposal publication. No agent wakes.
      (discourse/post! room (completion-message row occurrence target request job)))
    (schedules/mark-announced! conn (:schedule.occurrence/id occurrence))))

(defn publish-completion! [conn row occurrence request job room]
  (when-not (:schedule.occurrence/announced? occurrence)
    (when-not room (throw (ex-info "Scheduled result room is unavailable" {:type ::room-unavailable})))
    ;; Preserve the legacy reader's execution context while sharing only the
    ;; fixed already-resolved publisher with the owned path.
    (binding [ec/*execution-context* (:ctx room)]
      (publish-completion-target! conn row occurrence request job room (schedules/target conn row)))))

(defn- input-processed? [conn request room]
  ;; A completed Job means its orchestration returned, not that its candidate
  ;; passed. Read the canonical certified Attempt, bound to this request's
  ;; environment and direct candidate Run. Never infer success from a reward
  ;; or from an unrelated/hired descendant's receipt.
  (let [expected (get-in (requests/payload conn request) [:input :environment])]
    (binding [ec/*execution-context* (:ctx room)]
      (let [job-id (:work.request/job request)
            rows (run/runs room {:root-run-id job-id :limit 1000})
            candidates (filter #(= job-id (:run/parent %)) rows)]
        (when (and expected (< (count rows) 1000) (= 1 (count candidates)))
          (let [candidate (first candidates)
                certified (episode/attempt room (:run/id candidate))]
            (when certified
              (attempt/validate-attempt certified)
              (let [receipt (:attempt/receipt certified)
                    checks (:attempt/checks receipt)]
                (and (= expected (:attempt/environment certified))
                     (= (:run/id candidate) (:attempt/run-id certified))
                     (= :completed (:run/status candidate) (:attempt/status receipt))
                     (true? (:completed? checks))
                     (every? true? (vals checks)))))))))))

(defn- owned-resolution!
  "Semantic retained-input check against historical code, not today's source.
   Observation alone never confers first-dispatch ownership."
  [target retained]
  (let [planned (when (= :workflow (:kind target))
                  (workflows-run/plan-source (:snapshot target)
                                             (select-keys (:execution target) workflows-run/source-fields)))
        policy (:selection-policy planned)
        frozen? (contains? (:inputs planned) "/in/knowledge/manifest.edn")]
    (when retained
      (when-not (and policy (= (:target-revision retained) (hasch/uuid target)))
        (owned-unavailable!))
      (let [captured (if frozen?
                       (knowledge-inputs/from-inputs policy (:inputs planned))
                       (knowledge-inputs/verify-capture policy (:captured retained)))
            selection (dissoc captured :inputs)]
        (when-not (and (= selection (:selection retained))
                       (if frozen?
                         (every? nil? ((juxt :captured :publication-basis :knowledge-origin) retained))
                         (and (:captured retained) (:publication-basis retained)
                              (= (:execution-id captured) (get-in retained [:knowledge-origin :selection-id])))))
          (owned-unavailable!))))
    (when (and policy (nil? retained)) (owned-unavailable!))
    retained))

(defn- publication-required?
  "Classify the frozen deployment, not a mutable request's truthy marker.
   Live knowledge cannot silently become an unbound fixture when its journal
   or retained publication binding is missing/corrupt."
  [conn target occurrence request]
  (let [retained (when (:schedule.occurrence/input-ref occurrence)
                   ((if (= 2 (:version target)) schedules/retained-owned-input schedules/retained-input) conn occurrence))
        planned (when (= :workflow (:kind target))
                  (workflows-run/plan-source
                   (:snapshot target) (select-keys (:execution target) workflows-run/source-fields)))
        live? (and (:selection-policy planned)
                   (not (contains? (:inputs planned) "/in/knowledge/manifest.edn")))
        marked? (or (contains? request :work.request/publication-basis-id)
                    (some? (:publication-basis retained)))]
    (when (= 2 (:version target)) (owned-resolution! target retained))
    (when (or live? marked?)
      (let [basis (requests/publication-basis conn (:work.request/id request))]
        (when-not (and live? retained basis (= basis (:publication-basis retained)))
          (throw (ex-info "Scheduled knowledge publication binding is unavailable"
                          {:type ::publication-binding-unavailable}))))
      true)))

(defn- durable-completion
  "Restart fallback after the canonical physical-return flag is false. A
   process-local Job lookup is helpful, not the authority for completed work."
  [room request target]
  (binding [ec/*execution-context* (:ctx room)]
    (let [id (:work.request/job request)
          r (when (uuid? id) (run/run room id))]
      (when (and r (false? (:work.request/return-pending? request))
                 (= (if (= :benchmark (:kind target)) :experiment :workflow) (:run/kind r))
                 (= (room-store/conversation-id room) (:run/room r))
                 (= (:work.request/by request) (access/actor->party-uuid (:run/actor r)))
                 (nil? (:run/parent r))
                 (contains? #{:completed :failed :cancelled} (:run/status r))
                 (instance? java.util.Date (:run/ended-at r)))
        {:id id :status (:run/status r) :finished-at (:run/ended-at r) :settled? true}))))

(defn- file-input! [conn row occurrence request room]
  ;; The original delegator files, not the scheduler service or whoever
  ;; resumed the activation. Historical retry does not need the Attempt.
  (let [value (knowledge-publish/file-completed-request!
               conn (principals/party-principal (:schedule.activation/by row))
               (:schedule.activation/room row) room (:work.request/id request))
        proof (schedules/processing-proof conn (:schedule.occurrence/id occurrence))]
    ;; Do not adopt a different freshly read, merely self-hashed receipt in
    ;; place of the exact filing the host just validated. The cursor writer
    ;; compares this frozen proof again against its own serialized snapshot.
    (when-not (and proof (= value (:disposition proof)))
      (throw (ex-info "Scheduled output filing changed" {:type ::filing-unavailable})))
    (schedules/mark-input-processed! conn (:schedule.occurrence/id occurrence) proof)))

(defn- process-input! [conn row occurrence request job room target]
  (if (= :completed (:status job))
    (if (publication-required? conn target occurrence request)
      (file-input! conn row occurrence request room)
      (if (input-processed? conn request room)
        (schedules/mark-input-processed! conn (:schedule.occurrence/id occurrence))
        (schedules/block! conn row :execution-failed)))
    (schedules/block! conn row :execution-failed)))

(defn- release-settled! [conn row resolve-room]
  (when-let [oid (:schedule.activation/active-occurrence row)]
    (let [o (schedules/occurrence conn oid)
          r (when-let [rid (:schedule.occurrence/request o)] (requests/get-request conn rid))]
      (when (and (= :linked (:schedule.occurrence/state o))
                 (false? (:work.request/return-pending? r)))
        (try
          (let [room (or (resolve-room (:schedule.activation/room row))
                         (throw (ex-info "Scheduled result room is unavailable" {:type ::room-unavailable})))
                target (schedules/target conn row)
                filed (when (:schedule.occurrence/input-ref o)
                        (schedules/processing-proof conn oid))]
            (if (and filed (true? (:schedule.occurrence/announced? o)))
              (do
                ;; The already announced immutable completion and exact durable
                ;; filing survive expiry of Job/Attempt records. Reauthorize via
                ;; the fixed host writer; never invent a completion timestamp.
                (when-not (publication-required? conn target o r)
                  (throw (ex-info "Unexpected scheduled knowledge filing" {:type ::filing-unavailable})))
                (file-input! conn row o r room)
                (schedules/clear-active! conn row oid))
              ;; Hydrate the Room before lookup, including after restart. The
              ;; fully filed/announced branch above never consults Job/Attempt.
              (let [job (or (binding [ec/*execution-context* (:ctx room)]
                              (jobs/status (:work.request/job r)))
                            (durable-completion room r target))]
                (when (and filed (nil? job))
                  (throw (ex-info "Scheduled completion record is unavailable"
                                  {:type ::completion-record-unavailable})))
                (when (and (:settled? job) (contains? #{:completed :failed :cancelled} (:status job)))
                  (publish-completion! conn row o r job room)
                  (when (:schedule.occurrence/input-ref o)
                    (process-input! conn row o r job room target))
                  (schedules/clear-active! conn row oid)))))
          (catch InterruptedException e (throw e))
          (catch Exception e
            ;; Retain the unresolved pointer until its room announcement is
            ;; durable; retry filing/publication, never the completed execution.
            (log/log! {:level :warn :id ::completion-publication-pending
                       :data {:occurrence oid :exception-class (.getName (class e))}})))))))

(defn- submit-occurrence! [conn ctx principal row room target o resolution]
  (binding [ec/*execution-context* (:ctx room)]
    (let [balance (resource/balance room)]
      (when-not (every? (fn [[unit amount]] (>= (get balance unit 0M) amount))
                        (work/resources (get-in target [:execution :limits])))
        (throw (ex-info "Shared room allowance exhausted" {:type ::budget-exhausted})))))
  (let [oid (:schedule.occurrence/id o) input (assoc (:execution target) :request-id oid)]
    (if (= :benchmark (:kind target))
      (benchmarks/submit-snapshot! conn ctx principal (:schedule.activation/room row) room input (:snapshot target))
      (if resolution
        (workflows-run/submit! conn ctx principal (:schedule.activation/room row) room input (:snapshot target) resolution)
        (workflows-run/submit! conn ctx principal (:schedule.activation/room row) room input (:snapshot target))))
    (schedules/finish-admission! conn oid :linked (request-id row o) nil)))

(defn- dispatch-input! [conn ctx principal row room target o claim]
  (let [oid (:schedule.occurrence/id o)
        submit (fn [resolution] (submit-occurrence! conn ctx principal row room target o resolution))]
    (if-let [retained (schedules/retained-input conn (schedules/occurrence conn oid))]
      (do
        (when-not (= (:schedule.activation/revision row) (:target-revision retained))
          (throw (ex-info "Scheduled capture belongs to another target revision"
                          {:type :is.simm.model.workflow-schedules/corrupt})))
        (submit retained))
      (workflows-run/with-admission-selection
        (:schedule.activation/room row) room (:snapshot target) (select-keys (:execution target) workflows-run/source-fields)
        (fn [selection capture]
          (if-not selection
            (submit nil)
            (let [input-id (hasch/uuid [:simmis/scheduled-input-v1 (:schedule.activation/revision row)
                                        (:trigger-id selection)])]
              (when (= :capture (schedules/select-input! conn oid claim input-id (:empty? selection)))
                (schedules/retain-input!
                 conn oid claim (merge (capture) {:version 1 :input-id input-id
                                                  :target-revision (:schedule.activation/revision row)
                                                  :selection selection}))
               ;; Read the durable artifact, not a transient recapture, even on
               ;; the first submission. Restart uncertainty remains held.
                (submit (schedules/retained-input conn (schedules/occurrence conn oid)))))))))))

(defn- dispatch! [conn ctx resolve-room row o]
  (when-let [claim (schedules/admit! conn (:schedule.occurrence/id o))]
    (let [oid (:schedule.occurrence/id o) rid (request-id row o)]
      (try
        (let [target (schedules/target conn row)
              _ (when-not (policy-current? target)
                  (throw (ex-info "Host policy changed" {:type ::policy-changed})))
              principal (principals/party-principal (:schedule.activation/by row))
              _ (authorize! conn principal (:schedule.activation/room row) (:kind target))
              room (or (resolve-room (:schedule.activation/room row))
                       (throw (ex-info "Room is not hydrated" {:type ::room-unavailable})))]
          (if (= :benchmark (:kind target))
            (submit-occurrence! conn ctx principal row room target o nil)
            (dispatch-input! conn ctx principal row room target o claim)))
        (catch Exception e
          ;; An uncertain post-submission response is reconciled from the
          ;; canonical request, never interpreted as permission to resubmit.
          (let [request (requests/get-request conn rid)
                submitted? (= :submitted (:work.request/state request))
                reason (if request :interrupted (or (:type (ex-data e)) :admission-failed))]
            (schedules/finish-admission! conn oid (if submitted? :linked (if request :interrupted :blocked))
                                         (when request rid) reason)
            (when-not submitted?
              (schedules/block! conn row reason)
              (when-not request (schedules/clear-active! conn row oid)))
            (when (instance? InterruptedException e)
              (.interrupt (Thread/currentThread))
              (throw e))))))))

(defn tick!
  "Off-engine only. One pass coalesces due times, drains pending occurrences
   through existing admission, and retains overlap until real cleanup settles."
  [conn ctx resolve-room now]
  (off-engine!)
  (binding [ec/*execution-context* ctx]
    (doseq [row (schedules/activations conn)]
      (when (.isInterrupted (Thread/currentThread))
        (throw (InterruptedException. "Workflow dispatcher stopped")))
      (try
        ;; Classify with the bounded SAME-store reader BEFORE even completion
        ;; release can hydrate a Room. Legacy dispatch cannot execute v2.
        (let [target (schedules/managed-target conn row)]
          (case (:version target)
            1 nil
            2 (throw (ex-info "Owned schedule dispatch is not activated" {:type ::owned-dispatch-unavailable}))
            (throw (ex-info "Schedule target version is unsupported" {:type ::target-version-unavailable}))))
        (release-settled! conn row resolve-room)
        (when (:schedule.activation/enabled? row)
          (let [target (schedules/target conn row)]
            (schedules/claim-due! conn row (:cadence target) now)))
        (when-let [oid (:schedule.activation/active-occurrence
                        (schedules/activation conn (:schedule.activation/id row)))]
          (let [o (schedules/occurrence conn oid)]
            (when (= :pending (:schedule.occurrence/state o))
              (dispatch! conn ctx resolve-room row o))))
        (catch InterruptedException e
          (.interrupt (Thread/currentThread))
          (throw e))
        (catch Exception e
          ;; One corrupt target/backlog must not starve unrelated schedules.
          (schedules/block! conn row (or (:type (ex-data e)) :dispatcher-failed))
          (log/log! {:level :error :id ::activation-held
                     :data {:activation (:schedule.activation/id row)
                            :exception-class (.getName (class e))}}))))))

(defn- owned-row! [host row target]
  (let [principal (principals/party-principal (:schedule.activation/by row))
        handle (str (:schedule.activation/room row))
        original (owned-preflight! host principal handle (:kind target))]
    (when-not (and (owned-existing? row (:schedule.activation/id row) original)
                   (= (:party original) (:by target)) (= (:room-id original) (:room target)))
      (owned-unavailable!))
    {:principal principal :handle handle :original original}))

(defn- owned-owner! [host]
  (when (.isInterrupted (Thread/currentThread))
    (throw (InterruptedException. "Owned scheduler stopped")))
  (owned-storage! #(reads/current-host-control-db! host)))

(defn- owned-claim! [host row o claim]
  (let [db (owned-owner! host)
        actual (d/entity db [:schedule.occurrence/id (:schedule.occurrence/id o)])
        activation (d/entity db [:schedule.activation/id (:schedule.activation/id row)])]
    (when-not (and (uuid? claim) (= claim (:schedule.occurrence/claim actual))
                   (= :admitting (:schedule.occurrence/state actual))
                   (= (:schedule.activation/id row) (:schedule.occurrence/activation actual))
                   (= (:schedule.activation/revision row) (:schedule.occurrence/revision actual)
                      (:schedule.activation/revision activation))
                   (= (:schedule.occurrence/id o) (:schedule.activation/active-occurrence activation)))
      (owned-unavailable!))))

(defn- owned-retry! [conn row o target]
  (let [input (assoc (:execution target) :request-id (:schedule.occurrence/id o))
        result (owned-storage!
                #(if (= :benchmark (:kind target))
                   (benchmarks/retry-request :benchmark conn (:schedule.activation/by row)
                                             (:schedule.activation/room row) input)
                   (workflows-run/retry-request conn (:schedule.activation/by row)
                                                (:schedule.activation/room row) input)))
        rid (owned-storage! #(request-id row o))]
    (when-not (and (uuid? rid)
                   (or (= {:status :absent} result)
                       (and (map? result) (= #{:status :id} (set (keys result)))
                            (#{:retry :conflict} (:status result)) (= rid (:id result)))))
      (owned-unavailable!))
    (when (= :conflict (:status result)) (owned-unavailable!))
    (when (= :retry (:status result))
      (let [request (owned-storage! #(requests/get-request conn rid))]
        (when-not (and (= rid (:work.request/id request))
                       (= (:schedule.occurrence/id o) (:work.request/client-id request))
                       (= (:by target) (:work.request/by request))
                       (= (:room target) (:work.request/room request))
                       (= (:kind target) (:work.request/kind request)))
          (owned-unavailable!))
        request))))

(defn- owned-link! [conn row o request]
  (let [submitted? (= :submitted (:work.request/state request))]
    (schedules/finish-admission! conn (:schedule.occurrence/id o)
                                 (if submitted? :linked :interrupted)
                                 (:work.request/id request) (when-not submitted? :interrupted))
    (when-not submitted? (schedules/block! conn row :interrupted))))

(defn- owned-room-current! [host context target room-binding rich]
  (let [{:keys [principal handle original]} context
        checked (owned-storage! #(reads/validate-host-room! host principal handle room-binding))]
    (when-not (= (select-keys original [:party :room-id :slug])
                 (select-keys checked [:party :room-id :slug]))
      (owned-unavailable!))
    (owned-storage! #(owned-action! (:control-db checked) principal (:room-id checked) (:kind target)))
    (when rich
      (when-not (identical? (:room checked) (:room rich)) (owned-unavailable!))
      (owned-storage! #(owned-workspace/validate! (:control-db checked) principal :merge
                                                  (:room-id checked) (:room checked) rich)))
    checked))

(defn- owned-policy-current! [target]
  (when-not (and (= (:host-policy target) (assoc (host-policy) :source :owned-retained-source-v2))
                 (= (:model-policy target) (model-policy (:kind target) (:execution target))))
    (owned-unavailable!)))

(defn- owned-submit! [host conn row o claim target context room-binding rich resolution]
  (let [{:keys [principal original]} context
        room (:room room-binding)
        {:keys [execution-ctx]} (::reads/owner host)
        original-input (assoc (:execution target) :request-id (:schedule.occurrence/id o))
        legacy-input (dissoc original-input :expected-owned-source)]
    (owned-claim! host row o claim)
    (owned-room-current! host context target room-binding rich)
    ;; Fresh admissions only: no current model, policy, balance or provider work
    ;; is reachable through the canonical exact-retry branch above.
    (owned-storage! #(owned-policy-current! target))
    (owned-storage! #(execution! (:kind target) (dissoc legacy-input :request-id)))
    (owned-storage!
     #(binding [ec/*execution-context* (:ctx room)]
        (let [balance (resource/balance room)]
          (when-not (every? (fn [[unit amount]] (>= (get balance unit 0M) amount))
                            (work/resources (:limits legacy-input)))
            (owned-unavailable!)))))
    (let [prepared (owned-storage!
                    #(if (= :benchmark (:kind target))
                       (benchmarks/adapter execution-ctx room
                                           (benchmarks/prepare (:snapshot target) legacy-input) principal)
                       (workflows-run/bind-publication
                        (if-let [captured (:captured resolution)]
                          (workflows-run/prepare (:snapshot target) legacy-input captured)
                          (workflows-run/prepare (:snapshot target) legacy-input)) resolution)))
          prepared (-> prepared
                       (assoc :request-kind (:kind target) :submission-channel :host)
                       (assoc-in [:basis :submission] original-input)
                       (assoc-in [:basis :source-origin] (:source-origin target))
                       (cond-> (= :benchmark (:kind target)) (assoc-in [:basis :admission-channel] :host))
                       (assoc-in [:basis :schedule-origin]
                                 {:activation (:schedule.activation/id row)
                                  :revision (:schedule.activation/revision row)
                                  :occurrence (:schedule.occurrence/id o)
                                  :creation-channel (:creation-channel target)})
                       (cond-> (:knowledge-origin resolution)
                         (assoc-in [:basis :knowledge-origin] (:knowledge-origin resolution))))]
      (owned-claim! host row o claim)
      (owned-room-current! host context target room-binding rich)
      (owned-storage! #(work/submit-prepared! conn execution-ctx principal (:room-id original)
                                              room original-input prepared))
      ;; Canonical submission, not the interceptor's arbitrary returned answer,
      ;; determines whether this occurrence can be linked or must remain held.
      (owned-owner! host)
      (if-let [request (owned-retry! conn row o target)]
        (owned-link! conn row o request)
        (owned-unavailable!)))))

(defn- owned-dispatch! [host conn row o target]
  (let [context (owned-row! host row target)
        {:keys [principal handle]} context]
    (if-let [request (owned-retry! conn row o target)]
      (do (owned-row! host row target) (owned-link! conn row o request))
      ;; A retained root without this call's original first-install ACK can be
      ;; inspected, but is never resumed/re-published as a fresh admission.
      (if (:schedule.occurrence/input-ref o)
        (do (owned-storage! #(owned-resolution! target (schedules/retained-owned-input conn o)))
            (owned-unavailable!))
        (let [_ (owned-owner! host)
              claim (owned-storage! #(schedules/admit! conn (:schedule.occurrence/id o)))]
          (when-not (or (nil? claim) (uuid? claim)) (owned-unavailable!))
          (when claim
            (try
              (owned-claim! host row o claim)
              (let [room-binding (owned-storage! #(reads/resolve-host-room! host principal handle))
                    _ (owned-room-current! host context target room-binding nil)
                    planned (when (= :workflow (:kind target))
                              (owned-storage!
                               #(workflows-run/plan-source (:snapshot target)
                                                           (select-keys (:execution target) workflows-run/source-fields))))
                    policy (:selection-policy planned)
                    frozen? (contains? (:inputs planned) "/in/knowledge/manifest.edn")
                    rich (when (and policy (not frozen?))
                           (owned-storage! #(reads/resolve-host-read! host principal handle)))
                    _ (owned-room-current! host context target room-binding rich)
                    selection (when policy
                                (owned-storage!
                                 #(if frozen?
                                    (dissoc (knowledge-inputs/from-inputs policy (:inputs planned)) :inputs)
                                    (dissoc (knowledge-inputs/selection (:db rich) policy) :manifest-text))))
                    input-id (when selection
                               (owned-storage! #(hasch/uuid [:simmis/scheduled-input-v1
                                                             (:schedule.activation/revision row) (:trigger-id selection)])))
                    decision (when selection
                               (owned-storage! #(schedules/select-input! conn (:schedule.occurrence/id o) claim
                                                                         input-id (:empty? selection))))]
                (when (and selection (not (#{:capture :no-inputs :unchanged} decision)))
                  (owned-unavailable!))
                (when (or (nil? selection) (= :capture decision))
                  (let [resolution
                        (when selection
                          (let [captured (when-not frozen? (owned-storage! #(knowledge-inputs/capture (:db rich) policy)))
                                basis (when-not frozen?
                                        (owned-storage! #(knowledge-inputs/publication-basis
                                                          (:db rich) (select-keys (:target rich) [:registry-system :runtime-system]))))
                                value {:version 2 :occurrence-id (:schedule.occurrence/id o) :input-id input-id
                                       :target-revision (:schedule.activation/revision row) :selection selection
                                       :captured captured :publication-basis basis
                                       :knowledge-origin (when rich {:target (:target rich) :target-stamp (:target-stamp rich)
                                                                     :selection-id (:execution-id selection)})}
                                _ (owned-storage! #(owned-resolution! target value))
                                _ (owned-room-current! host context target room-binding rich)
                                ack (owned-storage! #(schedules/retain-owned-input! conn (:schedule.occurrence/id o) claim value))
                                retained (owned-storage! #(schedules/retained-owned-input conn ack))]
                            (when-not (= value retained) (owned-unavailable!))
                            (owned-storage! #(owned-resolution! target retained))))]
                    (owned-submit! host conn row o claim target context room-binding rich resolution))))
              (catch Throwable error
              ;; Once admitted, missing ACKs or unknown outcomes hold the active
              ;; pointer. Existing canonical work may reconcile, never replay.
                (when (or (causal-interruption? error) (.isInterrupted (Thread/currentThread)))
                  (.interrupt (Thread/currentThread))
                  (throw error))
                (owned-owner! host)
                (if-let [request (owned-storage! #(owned-retry! conn row o target))]
                  (owned-link! conn row o request)
                  (do (schedules/finish-admission! conn (:schedule.occurrence/id o) :interrupted nil :interrupted)
                      (schedules/block! conn row :interrupted)))
                (when (.isInterrupted (Thread/currentThread)) (throw error))))))))))

(defn- release-owned-settled! [host conn row target]
  (when-let [oid (:schedule.activation/active-occurrence row)]
    (let [o (schedules/occurrence conn oid)
          request (when-let [rid (:schedule.occurrence/request o)] (requests/get-request conn rid))]
      (when (and (= :linked (:schedule.occurrence/state o))
                 (false? (:work.request/return-pending? request)))
        (let [context (owned-row! host row target)
              canonical (owned-retry! conn row o target)
              _ (when-not (and canonical (= :submitted (:work.request/state canonical))
                               (= (:work.request/id canonical) (:schedule.occurrence/request o)
                                  (:work.request/id request)))
                  (owned-unavailable!))
              room-binding (owned-storage! #(reads/resolve-host-room! host (:principal context) (:handle context)))
              room (:room room-binding)
              _ (owned-room-current! host context target room-binding nil)
              filed (when (:schedule.occurrence/input-ref o) (schedules/processing-proof conn oid))]
          (if (and filed (true? (:schedule.occurrence/announced? o)))
            (do (when-not (publication-required? conn target o request) (owned-unavailable!))
                (owned-room-current! host context target room-binding nil)
                (file-input! conn row o request room)
                (schedules/clear-active! conn row oid))
            (let [job (or (binding [ec/*execution-context* (:ctx room)] (jobs/status (:work.request/job request)))
                          (durable-completion room request target))]
              (when (and filed (nil? job)) (owned-unavailable!))
              (when (and (:settled? job) (#{:completed :failed :cancelled} (:status job)))
                (owned-room-current! host context target room-binding nil)
                (publish-completion-target! conn row o request job room target)
                (when (:schedule.occurrence/input-ref o) (process-input! conn row o request job room target))
                (schedules/clear-active! conn row oid)))))))))

(defn recover-owned!
  "Startup reconciliation after the old dispatcher physically exits.
   Current owner/creator gates first; no missing-request admission is replayed."
  [host]
  (owned-owner! host)
  (let [{:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (doseq [o (schedules/admitting-occurrences control-conn)]
        (owned-owner! host)
        (let [row (schedules/activation control-conn (:schedule.occurrence/activation o))]
          (try
            (let [target (owned-storage! #(owned-target! row (schedules/managed-target control-conn row)))]
              (owned-row! host row target)
              (if-let [request (owned-retry! control-conn row o target)]
                (do (owned-owner! host) (owned-link! control-conn row o request))
                (do (owned-owner! host)
                    (schedules/finish-admission! control-conn (:schedule.occurrence/id o) :interrupted nil :restart)
                    (schedules/block! control-conn row :interrupted))))
            (catch Throwable error
              ;; One unsupported/corrupt activation cannot prevent unrelated
              ;; startup reconciliation. Expired owner/interruption is fatal.
              (when (or (causal-interruption? error) (.isInterrupted (Thread/currentThread)))
                (.interrupt (Thread/currentThread)) (throw error))
              (owned-owner! host)
              (schedules/finish-admission! control-conn (:schedule.occurrence/id o) :interrupted nil :owned-dispatch-unavailable)
              (when row (schedules/block! control-conn row :owned-dispatch-unavailable)))))))))

(defn tick-owned!
  "Fixed HOST pass. No callback resolver, startup or selected-source
   fallback. Exact retries precede hydration, model/policy and funding work."
  [host now]
  (when-not (instance? java.util.Date now) (owned-unavailable!))
  (owned-owner! host)
  (let [{:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (doseq [row (schedules/activations control-conn)]
        (owned-owner! host)
        (try
          (let [target (owned-storage! #(owned-target! row (schedules/managed-target control-conn row)))]
            (owned-row! host row target)
            (release-owned-settled! host control-conn row target)
            (let [current (schedules/activation control-conn (:schedule.activation/id row))]
              (when (:schedule.activation/enabled? current)
                (schedules/claim-due! control-conn current (:cadence target) now))
              (when-let [oid (:schedule.activation/active-occurrence
                              (schedules/activation control-conn (:schedule.activation/id row)))]
                (let [o (schedules/occurrence control-conn oid)]
                  (when (= :pending (:schedule.occurrence/state o))
                    (owned-dispatch! host control-conn current o target))))))
          (catch Throwable error
            (when (or (causal-interruption? error) (.isInterrupted (Thread/currentThread)))
              (.interrupt (Thread/currentThread))
              (throw error))
            (owned-owner! host)
            (schedules/block! control-conn row :owned-dispatch-unavailable)
            (when (.isInterrupted (Thread/currentThread)) (throw error)))))
      nil)))

(defn- occurrence-request?
  "Metadata binding only, after room read authorization. A foreign/corrupt
   occurrence pointer must not expose another request's Job or filing links."
  [row occurrence target request]
  (and (every? uuid? (map row [:schedule.activation/id :schedule.activation/by :schedule.activation/room]))
       (uuid? (:schedule.occurrence/id occurrence))
       (= (:schedule.activation/id row) (:schedule.occurrence/activation occurrence))
       (= (:schedule.activation/revision row) (:schedule.occurrence/revision occurrence))
       (= (request-id row occurrence) (:schedule.occurrence/request occurrence) (:work.request/id request))
       (= (:schedule.occurrence/id occurrence) (:work.request/client-id request))
       (= (:schedule.activation/by row) (:work.request/by request))
       (= (:schedule.activation/room row) (:work.request/room request))
       (= (:kind target) (:work.request/kind request))
       (= :submitted (:work.request/state request))))

(defn- filing-summary [request]
  (when-let [value (dispositions/disposition request)]
    (cond-> {:status (:kind value)}
      (:proposal value) (assoc :proposal (str (:proposal value))))))

(defn- occurrence-job? [native-room row target request job]
  (and (some? native-room)
       (= native-room (:room job))
       (= (:work.request/job request) (:id job))
       (= (:schedule.activation/by row) (access/actor->party-uuid (:owner job)))
       (= (if (= :benchmark (:kind target)) :experiment :workflow) (:kind job))))

(defn- schedule-summary [conn room-id row target]
  (let [slug (:room/slug (d/pull @conn [:room/slug] [:room/id room-id]))
        native-room (when slug (room-store/slug->room-id slug))]
   {:id (str (:schedule.activation/id row)) :room-id (str room-id)
   :name (get-in target [:execution :name]) :kind (:kind target)
   :revision (str (:schedule.activation/revision row))
   :bundle (str (get-in target [:execution :expected-bundle]))
   :by (str (:schedule.activation/by row)) :enabled? (:schedule.activation/enabled? row)
   :reason (:schedule.activation/reason row) :cadence (:cadence target)
   :processed-input (some-> (:schedule.activation/last-processed-input row) str)
   :next-fire (.getTime ^java.util.Date (:schedule.activation/next-fire row))
   :occurrences
   (mapv (fn [o]
           (let [candidate (when-let [rid (:schedule.occurrence/request o)] (requests/get-request conn rid))
                 r (when (occurrence-request? row o target candidate) candidate)
                 job (when-let [id (:work.request/job r)] (jobs/status id))
                 j (when (occurrence-job? native-room row target r job) job)
                 filing (when r (filing-summary r))]
             (cond-> {:id (str (:schedule.occurrence/id o)) :state (:schedule.occurrence/state o)
              :input-id (some-> (:schedule.occurrence/input-id o) str)
              :input-revision (some-> (:schedule.occurrence/input-revision o) str)
              :intended-at (.getTime ^java.util.Date (:schedule.occurrence/intended-at o))
              :coalesced (:schedule.occurrence/coalesced o) :reason (:schedule.occurrence/reason o)
              :request (some-> (:work.request/id r) str)
              :job (some-> (:id j) str) :status (:status j)
              :settled? (:settled? j)}
               filing (assoc :filing filing))))
         (schedules/recent-occurrences conn (:schedule.activation/id row) 12))}))

(defn- owned-control-target! [conn original row]
  ;; Creation retries are creator-specific; room controls deliberately are not.
  (when-not (and (uuid? (:schedule.activation/id row))
                 (uuid? (:schedule.activation/by row))
                 (owned-existing? row (:schedule.activation/id row)
                                  (assoc original :party (:schedule.activation/by row))))
    (owned-unavailable!))
  (let [target (schedules/managed-target conn row)]
    (let [cadence (:cadence target)
          external (cond-> (dissoc cadence :every-ms)
                     (:every-ms cadence) (assoc :interval-ms (:every-ms cadence))
                     (keyword? (:every cadence)) (update :every name)
                     (keyword? (:on cadence)) (update :on name))]
      (when-not (and (#{:workflow :benchmark} (:kind target))
                     (map? (:execution target))
                     (string? (get-in target [:execution :name]))
                     (re-matches #"[a-z0-9][a-z0-9-]{0,63}" (get-in target [:execution :name]))
                     (uuid? (get-in target [:execution :expected-bundle]))
                     (map? cadence)
                     (every? #{:every-ms :every :tz :at :on :on-day} (keys cadence))
                     (= cadence (cadence! external)))
        (owned-unavailable!)))
    (case (:version target)
      2 (owned-target! row target)
      1 nil
      (owned-unavailable!))
    target))

(defn list-owned!
  "HOST human schedule metadata. No room hydration, current default,
   live source, provider preparation or execution. Legacy targets stay legacy."
  [host principal handle]
  (let [original (owned-storage! #(reads/preflight-host-read! host principal handle))
        {:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [rows (owned-storage! #(filterv (fn [row] (= (:room-id original) (:schedule.activation/room row)))
                                           (schedules/activations control-conn)))
            result (owned-storage!
                    #(mapv (fn [row]
                             (let [target (owned-control-target! control-conn original row)]
                               (cond-> (schedule-summary control-conn (:room-id original) row target)
                                 (= 1 (:version target)) (assoc :resume-blocked :legacy-recreate)
                                 (= 2 (:version target))
                                 (assoc :source-revision (str (get-in target [:source-origin :revision])))))) rows))
            current (owned-storage! #(reads/preflight-host-read! host principal handle))]
        (when-not (= (select-keys original [:party :room-id]) (select-keys current [:party :room-id]))
          (owned-unavailable!))
        result))))

(defn set-enabled-owned!
  "HOST human pause/resume. Pause withdraws pending work through the existing
   transaction; resume rechecks creator and pinned policy, and lets an already
   running dispatcher execute due work. Does not fund or start a dispatcher."
  [host principal handle id enabled?]
  (when-not (and (uuid? id) (boolean? enabled?))
    (throw (ex-info "Invalid schedule control" {:type ::invalid})))
  (let [original (owned-storage! #(reads/preflight-host-read! host principal handle))
        {:keys [control-conn execution-ctx]} (::reads/owner host)]
    (binding [ec/*execution-context* execution-ctx]
      (let [row (owned-storage! #(schedules/activation control-conn id))
            target (owned-storage! #(owned-control-target! control-conn original row))
            kind (:kind target)]
        (owned-current! host principal handle kind original nil)
        (when enabled?
          (when-not (= 2 (:version target))
            (throw (ex-info "Recreate this legacy schedule before resuming" {:type ::legacy-recreate})))
          (when-let [oid (:schedule.activation/active-occurrence row)]
            (let [o (owned-storage! #(schedules/occurrence control-conn oid))]
              (when-not (and (= oid (:schedule.occurrence/id o))
                             (= id (:schedule.occurrence/activation o))
                             (= (:schedule.activation/revision row) (:schedule.occurrence/revision o))
                             (#{:pending :linked} (:schedule.occurrence/state o)))
                (throw (ex-info "Uncertain admission requires reconciliation before resuming" {:type ::conflict})))))
          (owned-preflight! host (principals/party-principal (:schedule.activation/by row)) handle kind)
          (owned-storage! #(owned-policy-current! target)))
        (let [current (owned-current! host principal handle kind original nil)]
          (when enabled?
            (owned-storage!
             #(let [creator (principals/party-principal (:schedule.activation/by row))]
                (reads/authorize-read! (:control-db current) creator handle)
                (owned-action! (:control-db current) creator (:room-id current) kind))))
          (owned-storage! #(if enabled?
                             (schedules/resume-managed! control-conn row)
                             (schedules/set-enabled! control-conn id false))))))))

(defn list-room [conn ctx principal room-id]
  (binding [ec/*execution-context* ctx]
    (let [party (access/actor->party-uuid (:principal/id principal))]
      (when-not (access/can? @conn party :read {:room room-id})
        (throw (ex-info "Room read authority required" {:type ::denied})))
      (mapv #(schedule-summary conn room-id % (schedules/target conn %))
            (filter #(= room-id (:schedule.activation/room %)) (schedules/activations conn))))))
