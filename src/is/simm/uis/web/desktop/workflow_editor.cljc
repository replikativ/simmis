(ns is.simm.uis.web.desktop.workflow-editor
  "Pure draft and bounded launch presentation; execution remains server-owned."
  (:require [clojure.string :as str]
            [is.simm.model.knowledge-input-check :as input-check]))

(defn room-label [room-data roster]
  (or (some (fn [room]
              (when (= (str (:room-id room-data)) (str (:room/id room))) (:room/name room)))
            (if (vector? roster) roster (:rooms roster)))
      (:room-name room-data) "Room"))

(def scaffold
  {"workflow.edn" "{:title \"New workflow\"\n :task \"{goal} Read /docs/input.md and write /out/result.md.\"\n :params {:goal \"Summarize the input.\"}\n :capture [\"/out\"]}\n"
   "fixtures/docs/input.md" "Replace this with your input.\n"
   "gold.edn" "{}\n"
   "checker.clj" "(fn [_] (throw (ex-info \"Replace this placeholder with a real checker before benchmarking\" {})))\n"})

(def defaults
  {:backend "hosted" :worker "" :worker-admissions "1"
   :model "" :case "" :mode "run" :split "development" :repetitions "1" :cli-runs "2"
   :schedule-minutes "1440" :schedule-tz "UTC" :schedule-cadence "interval"
   :schedule-at "09:00" :schedule-on "monday" :schedule-day "1"
   :budget-microdollars "100000" :max-dispatches "4"
   :max-model-steps "4" :timeout-ms "120000" :timeout-default? true :settlement "review"})

(defn set-form-field
  "Change a field, preserving explicit time limits across backend switches.
   Only newly created, default-owned values follow the backend's default;
   legacy forms without ownership metadata are left unchanged."
  [form key value]
  (let [updated (assoc form key value)
        timeout (get {"hosted" "120000" "local" "300000"} value)]
    (cond
      (and (= :backend key) (= "cli" value)) (assoc updated :mode "benchmark")
      (= :timeout-ms key) (assoc updated :timeout-default? false)
      (= :split key) (assoc updated :case "")
      (and (= :backend key) (true? (:timeout-default? form)) timeout)
      (assoc updated :timeout-ms timeout)
      :else updated)))

(defn parameters [source]
  (into (sorted-map) (map (fn [[k v]] [(name k) v]))
        ;; Match ordinary admission: this reserved selector configures grading,
        ;; not a user task parameter. Exclude it before stripping namespaces.
        (dissoc (get-in source [:definition :params]) :simmis/checker-profile)))

(defn draft [source]
  {:source source :files (:files source) :path (or (when (contains? (:files source) "workflow.edn") "workflow.edn")
                                                   (first (sort (keys (:files source)))))
   :form (cond-> (assoc defaults :params (parameters source))
           (contains? (:files source) "execution-origin.edn") (assoc :settlement "discard"))})

(defn dirty? [draft] (not= (:files draft) (get-in draft [:source :files])))

(defn workflow-activations
  "Room-scoped observations, including paused and old-source deployments.
   An unknown observation is not an empty schedule list or execution authority."
  [state room name]
  (when (= :ready (:activation-status state))
    (->> (:activations state)
         (filter #(and (= room (:room-id %)) (= name (:name %))))
         (sort-by :id)
         vec)))

(defn activation-label [row]
  (cond
    (:reason row) (str "Held · " (name (:reason row)))
    (= :legacy-recreate (:resume-blocked row)) "Needs recreation · earlier schedule format"
    (= :reconciliation-required (:resume-blocked row)) "Held · admission requires reconciliation"
    (:enabled? row) "Active"
    :else "Paused"))

(defn batch-label
  "A metadata observation, independent of model/Job completion or adoption."
  [batch]
  (case (:status batch)
    :unavailable "Input batch status unavailable"
    :observed (case (:phase batch)
                :capturing "Preparing input batch"
                :ready "Input batch prepared"
                :submitted "Input batch admitted"
                :review "Input batch awaiting review"
                :closed "Input batch closed"
                "Input batch status unavailable")
    nil))

(defn- setup-facts [reply status]
  (assoc (select-keys reply [:revision :installed-version :shipped-version]) :status status))

(defn- valid-upgrade-offer? [reply]
  (and (integer? (:installed-version reply)) (pos? (:installed-version reply))
       (integer? (:shipped-version reply)) (< (:installed-version reply) (:shipped-version reply))
       (string? (:revision reply))
       (re-matches #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}" (:revision reply))))

(defn observe-wiki-setup [state reply]
  (let [status (if (and (= "maintain-wiki" (:name reply)) (= :unchanged (:activation reply))
                        (= :current-source (:observation reply))
                        (#{:missing :installed :customized :upgrade-available} (:status reply))
                        (or (not= :upgrade-available (:status reply)) (valid-upgrade-offer? reply)))
                 (:status reply) :unavailable)]
    (if (get-in state [:wiki-setup :uncertain?])
      (assoc-in state [:wiki-setup :observed-status] status)
      (assoc state :wiki-setup (setup-facts reply status)))))

(defn begin-wiki-setup
  ([state] (assoc state :wiki-setup {:status :pending :uncertain? true}))
  ([state intent] (assoc state :wiki-setup {:status :pending :uncertain? true :intent intent})))

(defn acknowledge-wiki-setup [state reply]
  ;; Missing source after an uncertain write is NOT permission to replay it.
  (if (and (= "maintain-wiki" (:name reply)) (= :unchanged (:activation reply))
           (= :acknowledged (:observation reply)) (#{:installed :customized :changed} (:status reply)))
    (assoc state :wiki-setup (setup-facts reply (:status reply)))
    (update state :wiki-setup merge {:status :pending :uncertain? true})))

(defn reloaded [old source]
  (merge (draft source) (select-keys old [:submission :schedule-submission :fund-request :fund-backend :funded? :fund-reply])))

(defn source-current?
  "An observation is not a permission grant. This only prevents a local UI
   from knowingly issuing fresh work against an unverified or changed source."
  [draft]
  (and (not (:source-stale? draft)) (nil? (:incoming-source draft))))

(defn stream-request
  "Read the saved program, independent of local code/form edits. No caller
   author, stream, epoch or target selector; the HOST derives those facts."
  [state]
  (let [name (:selected state) draft (get-in state [:drafts name]) source (:source draft)
        revision (:revision source) bundle (get-in source [:inspection :bundle])
        uuid-text? #(and (string? %) (re-matches #"(?i)^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$" %))]
    (when (and draft (source-current? draft) (get-in source [:inspection :ok])
               (uuid-text? revision) (uuid-text? bundle))
      {:name name :expected-source revision :expected-bundle bundle})))

(defn forget-stream [state]
  (cond-> (dissoc state :workflow-stream :workflow-stream-status :observed-stream-generation)
    (contains? state :observations) (update :observations dissoc :workflow-stream)
    (contains? state :observation-accepted) (update :observation-accepted dissoc :workflow-stream)))

(defn observe-stream [state request reply]
  (if (not= request (stream-request state))
    state
    (do
      (when-not (and (= (:name request) (:name reply))
                     (= (:expected-source request) (:source-revision reply))
                     (= (:expected-bundle request) (:bundle reply))
                     (#{:not-enabled :not-discovered :observed} (:status reply))
                     (or (not= :observed (:status reply))
                         (and (#{:active :retiring :retired} (:lifecycle reply))
                              (integer? (get-in reply [:pending :count]))
                              (integer? (get-in reply [:pending :limit]))
                              (<= 0 (get-in reply [:pending :count]) (get-in reply [:pending :limit]) 20000)
                              (true? (get-in reply [:pending :complete?]))
                              (#{:current :changed :unknown} (get-in reply [:freshness :status]))
                              (#{:none :observed :unavailable} (get-in reply [:batch :status]))
                              (or (not (contains? reply :input-check))
                                  (input-check/valid? (:input-check reply))))))
        (throw (ex-info "Workflow stream observation is invalid" {})))
      (assoc state :workflow-stream {:request request :reply reply} :workflow-stream-status :ready))))

(defn collectable-output-request
  "Pinned submission presentation, not current source or form settings.
   The server independently checks the actual canonical request and Attempt."
  [draft]
  (let [submission (:submission draft) reply (:reply submission)]
    (when (and (= "run" (:mode submission)) (= "hosted" (:backend submission "hosted")) (:knowledge? submission)
               (= :workflow (:kind reply)) (:id reply) (:job reply))
      (str (:id reply)))))

(defn apply-output-result [draft request-id reply]
  (if (not= request-id (collectable-output-request draft))
    draft
    (assoc draft :wiki-output
           (if (and (= request-id (:request reply))
                    (or (= :no-op (:status reply))
                        (and (= :proposed (:status reply)) (string? (:proposal reply)))))
             (select-keys reply [:status :request :proposal])
             {:status :unavailable :request request-id}))))

(defn collected-output [draft]
  (when-let [request-id (collectable-output-request draft)]
    (when (= request-id (get-in draft [:wiki-output :request])) (:wiki-output draft))))

(defn acknowledge-output
  "Keep a sealed result receipt even when its own filing invalidates metadata.
   No source, permissions or missing/replaced draft may be restored by this ACK."
  [state name request-id reply]
  (if (contains? (:drafts state) name)
    (update-in state [:drafts name] apply-output-result request-id reply)
    state))

(defn unverified [draft]
  (assoc draft :source-stale? true))

(defn observed
  "Reconcile an authorized source observation without discarding local intent.
   Dirty text keeps its original base and exposes an incoming-change conflict.
   Clean text advances, preserving the selected file where possible, explicit
   form overrides and all original admission/retry ledgers. No implicit save."
  [old source]
  (cond
    (nil? old) (draft source)
    (= (get-in old [:source :revision]) (:revision source))
    (-> old (assoc :source source) (dissoc :source-stale? :incoming-source))
    (dirty? old) (assoc old :source-stale? true :incoming-source source)
    :else
    (let [defaults (parameters source)
          prior-defaults (parameters (:source old))
          overrides (into {} (filter (fn [[k v]] (not= v (get prior-defaults k))))
                          (get-in old [:form :params]))
          path (if (contains? (:files source) (:path old)) (:path old) (:path (draft source)))]
      (-> old
          (assoc :source source :files (:files source) :path path)
          (assoc-in [:form :params] (merge defaults (select-keys overrides (keys defaults))))
          (dissoc :source-stale? :incoming-source)))))

(defn saved [draft sent reply]
  (let [defaults (parameters {:definition (:definition reply)})
        previous-defaults (parameters (:source draft))
        overrides (into {} (filter (fn [[k v]] (not= v (get previous-defaults k))))
                        (get-in draft [:form :params]))]
    (-> draft
        (assoc :saved? true)
        (assoc-in [:source :files] sent)
        (assoc-in [:source :revision] (get-in reply [:workflow :revision]))
        (assoc-in [:source :inspection]
                  (if (:source-changed? reply)
                    (assoc (:workflow reply) :ok false
                           :problems ["Source changed or could not be read after saving; reload before launching."])
                    (:workflow reply)))
        (assoc-in [:source :definition] (:definition reply))
        (assoc-in [:form :params] (merge defaults (select-keys overrides (keys defaults)))))))

(defn acknowledged-save
  "Advance only the original save base using its sealed ACK and sent bytes.
   A newer authorized observation must never be rolled back. Any incoming
   source is reconciled AFTER base advancement, preserving edits made in flight."
  [draft original-revision sent reply]
  (let [current (get-in draft [:source :revision])
        acknowledged (get-in reply [:workflow :revision])]
    (if (and acknowledged (or (= current original-revision) (= current acknowledged)))
      (let [incoming (:incoming-source draft)
            next (-> (saved draft sent reply) (dissoc :incoming-source))]
        (if incoming (observed next incoming) next))
      draft)))

(defn edits [draft]
  (into (sorted-map)
        (keep (fn [path]
                (let [before (get-in draft [:source :files path]) after (get-in draft [:files path])]
                  (when (not= before after) [path after]))))
        (into (set (keys (get-in draft [:source :files]))) (keys (:files draft)))))

(defn current-response?
  "A reply belongs to this request and, when supplied, a display selection."
  [state slot token selected]
  (and (= token (get-in state [:requests slot]))
       (or (nil? selected) (= selected (:selected state)))))

(defn- invalid! [message] (throw (ex-info message {:type ::invalid})))

(defn- bounded-integer [form key low high]
  (let [s (str (get form key ""))]
    (when-not (re-matches #"[0-9]{1,9}" s) (invalid! (str (name key) " must be an integer")))
    (let [n #?(:clj (Long/parseLong s) :cljs (js/Number s))]
      (when-not (<= low n high) (invalid! (str (name key) " is outside its allowed range")))
      n)))

(defn local? [form] (= "local" (:backend form)))
(defn cli? [form] (= "cli" (:backend form)))

(defn benchmark-cases [inspection form]
  (if-let [splits (get-in inspection [:evaluation :splits])]
    (get splits (keyword (get form :split "development")))
    (:cases inspection)))

(defn limits [form]
  (when-not (#{"hosted" "local" "cli"} (get form :backend "hosted"))
    (invalid! "Choose an execution backend"))
  (cond-> {:timeout-ms (bounded-integer form :timeout-ms 1000 600000)
           :settlement (if (= "benchmark" (:mode form)) "discard"
                           (if (#{"review" "discard"} (:settlement form)) (:settlement form)
                               (invalid! "Choose review or discard settlement")))}
    (cli? form)
    (assoc :max-cli-runs (bounded-integer form :cli-runs 1 8)
           :max-model-steps (bounded-integer form :max-model-steps 1 32))
    (and (not (local? form)) (not (cli? form)))
    (assoc :budget-microdollars (bounded-integer form :budget-microdollars 1 5000000)
           :max-dispatches (bounded-integer form :max-dispatches 1 32)
           :max-model-steps (bounded-integer form :max-model-steps 1 32))))

(defn allowance [form request-id]
  (cond
    (cli? form) {:request-id request-id :max-cli-runs (bounded-integer form :cli-runs 1 8)}
    (local? form)
    {:request-id request-id :max-worker-admissions (bounded-integer form :worker-admissions 1 32)}
    :else (assoc (select-keys (limits form) [:budget-microdollars :max-dispatches]) :request-id request-id)))

(defn benchmark-models [form]
  (if (and (= "hosted" (:backend form "hosted")) (contains? form :models))
    (vec (:models form))
    [(str/trim (:model form ""))]))

(defn choose-action [draft action]
  ;; Navigation reveals configuration; it never replaces a frozen submission.
  (-> draft
      (assoc :execution-open? true :schedule-open? (= :schedule action))
      (update :form set-form-field :mode (if (= :benchmark action) "benchmark" "run"))))

(defn submission [draft request-id]
  (when-not (source-current? draft) (invalid! "Source changed or is being verified; reconcile it before starting new work"))
  (when (dirty? draft) (invalid! "Save and inspect these changes before running"))
  (let [{:keys [source form]} draft
        inspection (:inspection source)
        cases (:cases inspection)
        selected (:case form)
        benchmark? (= "benchmark" (:mode form))
        params (:params form)
        local? (local? form)
        model (str/trim (:model form))
        models (if benchmark? (benchmark-models form) [model])]
    (when-not (:ok inspection) (invalid! "Repair this workflow before running"))
    (when (and benchmark? (false? (:benchmark-ready? inspection)))
      (invalid! "Prepare executable benchmark cases and a checker before benchmarking"))
    (when-not (and (string? (:revision source))
                   (re-matches #"[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}" (:revision source)))
      (invalid! "Reload this workflow's owned source before running"))
    (when (and (cli? form) (not benchmark?)) (invalid! "CLI text execution currently supports benchmarks only"))
    (when (and (not local?)
               (or (not (<= 1 (count models) 3))
                   (not= (count models) (count (distinct models)))
                   (some #(or (not (string? %)) (str/blank? %) (> (count %) 200)) models)))
      (invalid! "Choose one to three distinct model candidates"))
    (when (and local? (not (re-matches #"[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}" (str (:worker form)))))
      (invalid! "Select an explicitly enrolled local worker"))
    (when (and (seq selected) (not (some #{selected} cases))) (invalid! "Select a case from this revision"))
    (when (and (not benchmark?) (seq cases) (str/blank? selected)) (invalid! "Select an explicit case"))
    (let [base (cond-> {:request-id request-id :name (:name source) :expected-bundle (:bundle inspection)
                        :expected-owned-source (:revision source)
                        :limits (limits form)}
                 local? (assoc :worker (:worker form)))]
      (if benchmark?
        (let [repetitions (bounded-integer form :repetitions 1 2)
              allowed (benchmark-cases inspection form)
              split (get form :split "development")
              cell-cases (if (seq selected) [selected] allowed)]
          (when (and (:evaluation inspection)
                     (or (empty? allowed) (not-every? (set allowed) cell-cases)))
            (invalid! "Choose cases from the selected evaluation split"))
          (when (not= params (parameters source)) (invalid! "Benchmarking uses saved defaults; save parameter changes in workflow.edn first"))
          (when (or (> (count cell-cases) 4) (> (* (max 1 (count cell-cases)) repetitions (if local? 1 (count models))) 8))
            (invalid! "Choose fewer benchmark cases (maximum eight cells)"))
          (cond-> (assoc base :repetitions repetitions)
            (:evaluation inspection) (assoc :split split)
            (not local?) (assoc :models models)
            (seq cell-cases) (assoc :cases (vec cell-cases))))
        (do
          (when-not (and (<= (count params) 32) (every? string? (vals params)))
            (invalid! "Ordinary workflow parameters must be literal strings"))
          (cond-> (assoc base :params params)
            (not local?) (assoc :model model)
            (seq selected) (assoc :case selected)))))))

(defn cadence [form]
  (let [kind (:schedule-cadence form "interval")
        tz (str/trim (:schedule-tz form "UTC"))
        at (:schedule-at form "09:00")]
    (when (or (str/blank? tz) (> (count tz) 100)) (invalid! "Enter an explicit timezone"))
    (if (= "interval" kind)
      {:interval-ms (* 60000 (bounded-integer form :schedule-minutes 1 525600)) :tz tz}
      (do
        (when-not (and (contains? #{"day" "week" "month"} kind)
                       (string? at) (re-matches #"(?:[01][0-9]|2[0-3]):[0-5][0-9]" at))
          (invalid! "Choose a calendar cadence and a valid 24-hour time"))
        (when (and (= "week" kind)
                   (not (contains? #{"monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday"}
                                   (:schedule-on form))))
          (invalid! "Choose a weekday"))
        (cond-> {:every kind :at at :tz tz}
          (= "week" kind) (assoc :on (:schedule-on form))
          (= "month" kind) (assoc :on-day (bounded-integer form :schedule-day 1 28)))))))

(defn cadence-label [cadence]
  (str (if-let [interval (:interval-ms cadence)]
         (str "Every " (quot interval 60000) " minutes")
         (str (case (:every cadence) "day" "Daily" "week" (str "Weekly on " (:on cadence))
                                      "month" (str "Monthly on day " (:on-day cadence)) "Calendar schedule")
              " at " (:at cadence)))
       " · " (:tz cadence)))

(defn scheduled-submission
  ([draft request-id] (scheduled-submission draft request-id true))
  ([draft request-id enabled?]
  (when-not (boolean? enabled?) (invalid! "Choose enabled or paused schedule creation"))
  (when-not (= "hosted" (get-in draft [:form :backend] "hosted"))
    (invalid! "Schedules currently require hosted execution"))
  (let [execution (dissoc (submission draft request-id) :request-id)
        cadence (cadence (:form draft))]
    {:request-id request-id :kind (if (= "benchmark" (get-in draft [:form :mode])) "benchmark" "workflow")
     :enabled? enabled? :cadence cadence :execution execution})))

(defn deployment-preview
  "Presentation only. Unresolved requests display their original immutable
   intent, never edited form values. A preview is not admission or live status."
  [draft]
  (let [pending (:schedule-submission draft)]
    (if (and (:payload pending) (nil? (:reply pending)))
      {:pending? true :payload (:payload pending)}
      (when (and (source-current? draft) (not (dirty? draft)))
        (try
          {:pending? false :payload (dissoc (scheduled-submission draft nil) :request-id)}
          (catch #?(:clj Exception :cljs :default) _ nil))))))

(defn deployment-description [{:keys [kind execution]}]
  (let [benchmark? (= "benchmark" kind)
        limits (:limits execution)]
    {:model (if benchmark? (str/join ", " (:models execution)) (:model execution))
     :resources (str "Per occurrence: at most " (:budget-microdollars limits)
                     " microdollars and " (:max-dispatches limits) " dispatches"
                     (when benchmark? ", shared across benchmark trials") ". "
                     "Draws from the shared room allowance; it does not replenish it.")
     :execution-ceilings (str (if benchmark? "Per benchmark trial: " "Per occurrence: ")
                             "at most " (:max-model-steps limits) " model steps and "
                             (:timeout-ms limits) " ms.")
     :results (cond benchmark? "Checker-backed benchmark results; nothing merges automatically."
                    (= "review" (:settlement limits)) "Retained output requires explicit review; nothing merges automatically."
                    :else "Work is discarded after output capture; nothing merges automatically.")}))

(defn automatic-model
  "Fill only an unchosen model. Explicit human choices are never replaced."
  [draft readiness]
  (if (and (str/blank? (get-in draft [:form :model] ""))
           (= :selected (get-in readiness [:default-model :status])))
    (assoc-in draft [:form :model] (get-in readiness [:default-model :id]))
    draft))

(defn setup-configuration
  "Inert initial form from saved intent. Once configured, preserve human edits.
   This does not establish readiness or confer execution authority."
  [draft readiness action]
  (when (= :invalid (get-in draft [:source :setup :status]))
    (invalid! "Ask the agent to repair proposal.edn before confirming setup"))
  (let [intent (get-in draft [:source :setup :intent])
        action (or action (:action intent) :run)
        configured? (= (:setup-form-revision draft) (get-in draft [:source :revision]))
        explicit (or (:model intent) (first (:models intent)))
        model (if configured?
                (if (and (nil? explicit) (= (get-in draft [:form :model]) (:setup-default-model draft)))
                  (get-in readiness [:default-model :id]) (get-in draft [:form :model]))
                (or explicit (get-in readiness [:default-model :id]) ""))
        cadence (:cadence intent)
        form (if configured? (assoc (:form draft) :model model)
               (merge (dissoc (:form draft) :models)
                      {:backend "hosted" :mode (if (= :benchmark action) "benchmark" "run") :model model}
                      (when (and (= :benchmark action) (:models intent)) {:models (:models intent)})
                      (when (and (= :benchmark action) (:repetitions intent)) {:repetitions (str (:repetitions intent))})
                      (when (:case intent) {:case (:case intent)})
                      (into {} (map (fn [[key value]] [key (str value)])) (:limits intent))
                      (when (and (= :schedule action) cadence)
                        {:schedule-cadence (or (:every cadence) "interval")
                         :schedule-tz (:tz cadence) :schedule-at (:at cadence)
                         :schedule-on (:on cadence) :schedule-day (str (:on-day cadence))
                         :schedule-minutes (str (when-let [ms (:interval-ms cadence)] (quot ms 60000)))})))]
    {:action action :form (assoc form :mode (if (= :benchmark action) "benchmark" "run"))
     :cadence cadence :initial? (not configured?)
     :model-source (cond (= model explicit) :workflow-proposal
                         (= model (get-in readiness [:default-model :id])) (get-in readiness [:default-model :source])
                         :else :human-choice)}))

(defn setup-offer
  "Freeze inert setup into an existing admission payload. The UI must fence
   current observations and obtain explicit confirmation before dispatch."
  [draft readiness action]
  (let [{:keys [action form cadence initial? model-source]} (setup-configuration draft readiness action)
        model (:model form)
        models (if (= :benchmark action) (benchmark-models form) [model])
        rows (mapv (fn [id] (some #(when (= id (:id %)) %) (:models readiness))) models)
        row (first rows)
        _ (when-not (and (seq rows) (every? #(= :present (:credential-status %)) rows))
            (invalid! "Configure your preferred model or choose a model under Advanced"))
        _ (when-not (and (contains? #{:run :schedule :benchmark} action)
                         (= (if (= :benchmark action) "benchmark" "run") (:mode form)) (= "hosted" (:backend form)))
            (invalid! "Use Advanced for alternative execution backends"))
        _ (when (and initial? (= :schedule action) (nil? cadence))
            (invalid! "Ask the agent to propose a time and timezone, or set them under Advanced"))
        _ (when (and initial? cadence (:interval-ms cadence)
                     (or (contains? cadence :every) (not (integer? (:interval-ms cadence)))
                         (not (zero? (mod (:interval-ms cadence) 60000)))))
            (invalid! "Use whole-minute intervals or an explicit calendar schedule"))
        prepared (assoc draft :form form)
        payload (if (= :schedule action) (scheduled-submission prepared nil)
                    (submission prepared nil))
        _ (when (and initial? (= :schedule action) (not= cadence (:cadence payload)))
            (invalid! "Use only the supported fields for the proposed schedule cadence"))]
    {:action action :model-label (str/join ", " (map :label rows)) :model-source model-source
     :payload payload :form form
     :title (or (get-in draft [:source :definition :title]) (get-in draft [:source :name]))
     :task (get-in draft [:source :definition :task])
     :outputs (get-in draft [:source :definition :capture])}))
