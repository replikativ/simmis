(ns is.simm.uis.web.desktop.workflow-editor
  "Pure draft and bounded launch presentation; execution remains server-owned."
  (:require [clojure.string :as str]))

(def scaffold
  {"workflow.edn" "{:title \"New workflow\"\n :task \"{goal} Read /docs/input.md and write /out/result.md.\"\n :params {:goal \"Summarize the input.\"}\n :capture [\"/out\"]}\n"
   "fixtures/docs/input.md" "Replace this with your input.\n"
   "gold.edn" "{}\n"
   "checker.clj" "(fn [_] (throw (ex-info \"Replace this placeholder with a real checker before benchmarking\" {})))\n"})

(def defaults
  {:backend "hosted" :worker "" :worker-admissions "1"
   :model "" :case "" :mode "run" :split "development" :repetitions "1" :cli-runs "2"
   :schedule-minutes "1440" :schedule-tz "UTC"
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
   :form (assoc defaults :params (parameters source))})

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
                              (#{:none :observed :unavailable} (get-in reply [:batch :status])))))
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
        model (str/trim (:model form))]
    (when-not (:ok inspection) (invalid! "Repair this workflow before running"))
    (when-not (and (string? (:revision source))
                   (re-matches #"[0-9a-fA-F]{8}(-[0-9a-fA-F]{4}){3}-[0-9a-fA-F]{12}" (:revision source)))
      (invalid! "Reload this workflow's owned source before running"))
    (when (and (cli? form) (not benchmark?)) (invalid! "CLI text execution currently supports benchmarks only"))
    (when (and (not local?) (or (str/blank? model) (> (count model) 200))) (invalid! "Enter an explicit model id"))
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
          (when (or (> (count cell-cases) 4) (> (* (max 1 (count cell-cases)) repetitions) 8))
            (invalid! "Choose fewer benchmark cases (maximum eight cells)"))
          (cond-> (assoc base :repetitions repetitions)
            (:evaluation inspection) (assoc :split split)
            (not local?) (assoc :models [model])
            (seq cell-cases) (assoc :cases (vec cell-cases))))
        (do
          (when-not (and (<= (count params) 32) (every? string? (vals params)))
            (invalid! "Ordinary workflow parameters must be literal strings"))
          (cond-> (assoc base :params params)
            (not local?) (assoc :model model)
            (seq selected) (assoc :case selected)))))))

(defn scheduled-submission
  ([draft request-id] (scheduled-submission draft request-id true))
  ([draft request-id enabled?]
  (when-not (boolean? enabled?) (invalid! "Choose enabled or paused schedule creation"))
  (when-not (= "hosted" (get-in draft [:form :backend] "hosted"))
    (invalid! "Schedules currently require hosted execution"))
  (let [execution (dissoc (submission draft request-id) :request-id)
        minutes (bounded-integer (:form draft) :schedule-minutes 1 525600)
        tz (str/trim (get-in draft [:form :schedule-tz] "UTC"))]
    (when (or (str/blank? tz) (> (count tz) 100)) (invalid! "Enter an explicit timezone"))
    {:request-id request-id :kind (if (= "benchmark" (get-in draft [:form :mode])) "benchmark" "workflow")
     :enabled? enabled? :cadence {:interval-ms (* 60000 minutes) :tz tz} :execution execution})))

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
