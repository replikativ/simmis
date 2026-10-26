(ns is.simm.ops.workflow-runs
  "Ordinary saved-workflow reuse, not authored checker execution. One pinned
   task, one inherited room fork, the existing bounded-work runner/evaluator."
  (:require [clojure.string :as str]
            [dvergr.agent.environment :as environment]
            [dvergr.artifact :as artifact]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.rooms.files :as files]
            [hasch.core :as hasch]
            [is.simm.documents.execution-view :as execution-view]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.work :as work]
            [is.simm.ops.knowledge-inputs :as knowledge-inputs]
            [is.simm.ops.knowledge-execution-origin :as execution-origin]
            [is.simm.ops.knowledge-batch-proof :as batch-proof]
            [is.simm.ops.knowledge-admission :as knowledge-admission]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.ops.knowledge-outputs :as knowledge-outputs]
            [is.simm.ops.knowledge-workspace :as knowledge-workspace]
            [is.simm.ops.workflow-policy :as source-policy]
            [is.simm.ops.wiki-scoring :as wiki-scoring]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def policy source-policy/policy)
(def parameter-name [:re #"^[A-Za-z][A-Za-z0-9_-]{0,63}$"])
(def parameters [:map-of {:max 32} parameter-name [:string {:max 4000}]])
(def inputs [:map-of {:max 100} [:string {:min 1 :max 512}]
             [:string {:max workflows/max-file-bytes}]])
(def source-schema
  [:map {:closed true}
   [:name workflows/name-schema]
   [:expected-bundle :uuid]
   [:case {:optional true} [:string {:min 1 :max 100}]]
   [:params {:optional true} parameters]
   [:inputs {:optional true} inputs]])

(def source-fields [:name :expected-bundle :case :params :inputs])

(def schema
  (into source-schema [[:request-id :uuid] [:model [:string {:min 1 :max 200}]]
                       [:limits work/limits-schema]]))

(defn transport-schema-for [schema]
  ;; The HTTP JSON decoder keywordizes object keys, even paths. Normalize
  ;; ONLY map keys at this boundary, then apply the strict internal schema.
  (mapv (fn [entry]
          (if (and (vector? entry) (contains? #{:params :inputs} (first entry)))
            (update entry 2 #(assoc % 2 [:or (nth % 2) :keyword]))
            entry)) schema))

(def transport-schema (transport-schema-for schema))

(defn- invalid! [message]
  (throw (ex-info message {:type ::invalid})))

(defn transport-result
  "Pure normalization with CLOSED normal outcomes, not exception tags. Never
   includes offending values in invalid results; absent fields stay absent."
  [schema input]
  (if-not (m/validate (transport-schema-for schema) input)
    {:status :invalid :reason :shape}
    (let [result (workflows/keyed-fields-result input [:params :inputs])]
      (if (and (= :ok (:status result)) (not (m/validate schema (:input result))))
        {:status :invalid :reason :shape}
        result))))

(defn transport-for [schema input]
  (let [result (transport-result schema input)]
    (case (:status result)
      :ok (:input result)
      :invalid (invalid! (if (= :duplicate-key (:reason result))
                           "Duplicate workflow input key" "Invalid workflow execution")))))

(defn transport [input] (transport-for schema input))

(defn- byte-count [^String s] (alength (.getBytes s "UTF-8")))

(defn- effective-parameters [definition overrides]
  (wiki-scoring/selected-profile {:definition definition})
  ;; The fixed grading observation selector is not a task-template parameter
  ;; and cannot be overridden through the ordinary string-parameter surface.
  (let [defaults (dissoc (:params definition {}) :simmis/checker-profile)]
    (when-not (and (<= (count defaults) (:max-params policy))
                   (every? (fn [[k v]]
                             (and (keyword? k) (nil? (namespace k))
                                  (m/validate parameter-name (name k))
                                  (string? v) (<= (count v) 4000))) defaults))
      (invalid! "Ordinary workflows require at most 32 unqualified, literal string parameters"))
    (let [defaults (into (sorted-map) (map (fn [[k v]] [(name k) v])) defaults)]
      (when-not (every? #(contains? defaults %) (keys overrides))
        (invalid! "Workflow parameter was not declared in the pinned definition"))
      (let [params (merge defaults overrides)]
        (when (> (reduce + 0 (map byte-count (vals params))) (:max-param-bytes policy))
          (invalid! "Workflow parameters exceed the total byte limit"))
        params))))

(defn- render [task params]
  ;; One pass over the TEMPLATE, never over inserted values. catalog/task's
  ;; sequential replacement can expand tokens inside parameter values again.
  ;; Check each piece BEFORE appending: a small template with many repeated
  ;; tokens must not allocate a huge expansion just to reject it afterwards.
  (let [matcher (re-matcher #"\{([A-Za-z][A-Za-z0-9_-]{0,63})\}" task)
        out (StringBuilder.)]
    (loop [offset 0 size 0]
      (let [matched? (.find matcher)
            literal (subs task offset (if matched? (.start matcher) (count task)))
            key (when matched? (.group matcher 1))
            value (if-not matched? ""
                          (if (contains? params key) (get params key)
                              (invalid! "Workflow task references an undeclared parameter")))
            size (+ size (byte-count literal) (byte-count value))]
        (when (> size (:max-task-bytes policy)) (invalid! "Rendered workflow task exceeds the byte limit"))
        (.append out ^String literal)
        (.append out ^String value)
        (if matched?
          (recur (.end matcher) size)
          (do (when (zero? size) (invalid! "Rendered workflow task is empty")) (str out)))))))

(defn- input-files [prefix fixtures explicit]
  (let [entries (merge fixtures explicit)]
    (when (> (count entries) (:max-input-files policy)) (invalid! "Too many workflow input files"))
    (doseq [[path text] entries]
      (when-not (and (string? path)
                     (try (= path (files/workspace-path path)) (catch Exception _ false))
                     (not (or (str/starts-with? prefix (str path "/"))
                              (str/starts-with? path prefix)))
                     (<= (byte-count text) workflows/max-file-bytes))
        (invalid! "Workflow inputs must be bounded canonical paths outside the pinned bundle")))
    (when (> (reduce + 0 (map byte-count (vals entries))) (:max-input-bytes policy))
      (invalid! "Workflow input files exceed the total byte limit"))
    entries))

(defn- setup [name bundle source entries collector?]
  (let [prefix (str "/workflows/" name "/")
        digest (str (hasch/uuid entries))]
    (evaluation/make-world-setup
     {:id :simmis/workflow-inputs :version (if collector? 3 1)
      :basis (cond-> {:name name :bundle (str (:id bundle)) :inputs digest :policy policy}
               collector? (assoc :cleanup knowledge-workspace/cleanup-policy
                                 :source-role execution-view/profile))
      :prepare
      (fn [{world :room}]
        ;; This is the RUN WORLD, never the source room. Replace the entire
        ;; named subtree: extra helper files from a newer inherited revision
        ;; must not leak into the pinned execution. No source is evaluated.
        (workspace/ensure-workspace! world)
        (let [inherited (:files (workflows/snapshot world name))
              edits (merge (zipmap (map #(str prefix %) (keys inherited)) (repeat nil))
                           (into {} (map (fn [[p text]] [(str prefix p) {:text text}])) source))]
          (when (> (count edits) files/max-batch-files)
            (invalid! "Pinned workflow subtree exceeds the canonical writer batch limit"))
          (when collector? (knowledge-workspace/clear-owned! world))
          (files/write! world edits {:message (str "Pin workflow " name " at " (:id bundle))}))
        (when (seq entries) (workspace/seed! world entries))
        (when collector?
          (binding [ec/*execution-context* (:ctx world)]
            (execution-view/install! world)))
        {:bundle (str (:id bundle)) :inputs digest :files (count entries)
         :checker-executed? false :source-pinned? true})})))

(defn- supported-definition? [definition]
  (and (contains? #{nil "developer"} (:profile definition))
       (contains? #{nil ["/out"]} (:capture definition))
       (not-any? #(contains? definition %)
                 [:judge :web :fetched :allowed-sources :blocked-sources
                  ;; Saved workflows do not yet propagate retained-world
                  ;; adoption policy. Never silently accept these declarations.
                  :review :workflow/writes])))

(defn plan-source
  "Provider-independent source validation/rendering. A declared live input
   policy is returned as data here; execution must resolve it before setup."
  [snapshot {:keys [name expected-bundle case params inputs] :as input}]
  (when-not (m/validate source-schema input) (invalid! "Invalid workflow source selection"))
  (let [inspection (workflows/inspect-snapshot name snapshot)]
    (when-not (:ok inspection) (invalid! "Workflow bundle is not structurally valid"))
    (when-not (= (str expected-bundle) (:bundle inspection))
      (throw (ex-info "Workflow revision changed; inspect the new revision before running it"
                      {:type ::conflict}))))
  (let [bundle (catalog/bundle name (:files snapshot))
        definition (:definition bundle)]
    (when-not (supported-definition? definition)
      (invalid! "Ordinary workflows require developer /out capture and no web, judge or adoption policy"))
    (when-not (if (seq (:cases bundle)) (contains? (:cases bundle) case) (nil? case))
      (invalid! "Select an explicit existing case for a case-pack workflow, and no case otherwise"))
    (let [params (effective-parameters definition params)
          task (render (:task definition) params)
          prefix (str "/workflows/" name "/")
          entries (input-files prefix (merge (:fixtures bundle) (get-in bundle [:cases case :fixtures])) inputs)
          provenance {:name name :bundle (str expected-bundle) :case case :params params :policy policy}]
      (when (knowledge-inputs/declaration (:files snapshot))
        (knowledge-workspace/validate-inputs! entries))
      {:bundle bundle :task task :inputs entries :source (:files snapshot)
       :selection-policy (knowledge-inputs/declaration (:files snapshot)) :workflow provenance})))

(defn prepare-source
  "Prepare one immutable source revision, never a live selector. Host callers
   supply captured inputs explicitly; retained benchmark fixtures use the same
   verified frozen manifest. Unresolved declarations refuse, not empty runs."
  [snapshot input & [captured]]
  (let [{:keys [bundle inputs source selection-policy] :as planned} (plan-source snapshot input)
        resolved (when selection-policy
                   (if captured
                     (do
                       (when (some #(str/starts-with? % "/in/knowledge/") (keys inputs))
                         (invalid! "Workflow inputs collide with the collector-owned namespace"))
                       (knowledge-inputs/verify-capture selection-policy captured))
                     (knowledge-inputs/from-inputs selection-policy inputs)))
        _ (when (and captured (nil? selection-policy))
            (invalid! "Workflow does not declare a knowledge input selection"))
        entries (if resolved
                  (input-files (str "/workflows/" (:name input) "/") inputs (:inputs resolved)) inputs)
        provenance (when resolved
                     {:kind :knowledge :policy selection-policy
                      :trigger-id (:trigger-id resolved) :execution-id (:execution-id resolved)
                      :manifest-id (hasch/uuid (:manifest resolved))})]
    (cond-> (assoc planned :inputs entries :world-setup (setup (:name input) bundle source entries (some? resolved)))
      provenance (assoc-in [:workflow :input-selection] provenance))))

(defn- with-live-selection [db identity selection-policy publication? f]
  (let [selected (knowledge-inputs/selection db selection-policy)]
    (f (dissoc selected :manifest-text)
       #(hash-map :captured (knowledge-inputs/capture db selection-policy)
                  :publication-basis (when publication?
                                       (knowledge-inputs/publication-basis db identity))))))

(defn resolve-admission-in
  "HOST fixed immutable-DB source resolution. Caller supplies the validated
   owned DB and registry/runtime identity; no authority or ambient lookup."
  [db target-identity snapshot input]
  (let [{:keys [selection-policy inputs]} (plan-source snapshot input)]
    (if selection-policy
      (if (contains? inputs "/in/knowledge/manifest.edn")
        (do (knowledge-inputs/from-inputs selection-policy inputs)
            {:captured nil :publication-basis nil})
        (with-live-selection db target-identity selection-policy true
          (fn [_ resolve] (resolve))))
      {:captured nil :publication-basis nil})))

(defn- with-selection
  "Host admission only, after authorization. Callback receives a metadata-only
   selection and a zero-argument resolution function over that SAME immutable DB.
   It can skip unchanged work without loading bodies. Retained fixture inputs
   need no live capture; ordinary pure preparation verifies them again. Never
   persist the callback/DB, only its captured byte data."
  [room-id room snapshot input publication? f]
  (let [{:keys [selection-policy inputs]} (plan-source snapshot input)]
    (if selection-policy
      (if (contains? inputs "/in/knowledge/manifest.edn")
        (let [retained (knowledge-inputs/from-inputs selection-policy inputs)]
          (f (select-keys retained [:trigger-id :execution-id :empty? :manifest])
             (constantly {:captured nil :publication-basis nil})))
        (binding [ec/*execution-context* (:ctx room)]
          (let [target (when publication? (knowledge-admission/default-target room-id room))
                conn (if publication? (:conn target)
                         (:conn ((requiring-resolve 'dvergr.substrate.geschichte/current-workspace))))]
            (when-not conn (invalid! "Room has no knowledge source workspace"))
            (with-live-selection @conn (:identity target) selection-policy publication? f))))
      (f nil (constantly {:captured nil :publication-basis nil})))))

(defn with-admission-selection
  "Host-only same-snapshot input and publication resolution, after authorization."
  [room-id room snapshot input f]
  (with-selection room-id room snapshot input true f))

(defn with-source-selection
  "Compatibility projection for callers that only need frozen model inputs."
  [room snapshot input f]
  (with-selection nil room snapshot input false
    (fn [selection resolve] (f selection #(:captured (resolve))))))

(defn resolve-admission
  "Host-only resolution. Fixture/model-authored inputs never supply a basis."
  [room-id room snapshot input]
  (with-admission-selection room-id room snapshot input (fn [_ resolve] (resolve))))

(defn resolve-source
  "Resolve declared sources after authorization, without submission or spend."
  [room snapshot input]
  (with-source-selection room snapshot input (fn [_ capture] (capture))))

(defn bind-publication
  "Bind host-only target occupancy to the canonical request, never model inputs.
   This retains historical evidence, not permission to publish an output."
  [prepared {:keys [publication-basis captured]}]
  (if publication-basis
    (do
      (when-not (and (publication/host-target? (:target publication-basis))
                     (uuid? (:execution-id captured))
                     (= (:execution-id captured)
                        (get-in prepared [:basis :workflow :input-selection :execution-id])))
        (invalid! "Publication basis has no matching frozen knowledge execution"))
      (publication/validate! publication-basis)
      (let [target (:target publication-basis)
            setup (knowledge-workspace/with-publication-target (:world-setup prepared) target)
            evaluator (knowledge-outputs/with-publication-target (:evaluator prepared) target)
            verifier (evaluation/evaluator-ref evaluator)
            old (environment/validate-environment (:environment prepared))
            env (environment/make-environment
                 (cond-> {:id (:environment/id old) :version (:environment/version old)
                          :task (:environment/task old) :limits (:environment/limits old)
                          :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier)
                                     :basis (:verifier/basis verifier)}
                          :world (assoc (:environment/world old) :setup (evaluation/world-setup-ref setup))}
                   (contains? old :environment/metadata) (assoc :metadata (:environment/metadata old))))]
        (-> prepared
            (assoc :environment env :world-setup setup :evaluator evaluator)
            (assoc-in [:basis :environment] env)
            (assoc-in [:basis :knowledge/publication-basis] publication-basis))))
    prepared))

(defn prepare
  "Construct from ONE immutable source snapshot. The bundle is task data;
   provider, tool/effect surface, limits and evaluator remain host policy."
  [snapshot {:keys [model limits] :as input} & [captured checkpoint-selection]]
  (when-not (m/validate schema input) (invalid! "Invalid workflow execution"))
  (let [{:keys [task inputs source world-setup workflow]} (prepare-source snapshot (select-keys input source-fields) captured)
        checkpoint-policy (execution-origin/validate-execution! source limits)
        _ (when-not (= (some? checkpoint-policy) (some? checkpoint-selection))
            (invalid! "Checkpoint workflows require the fixed retained-batch HOST admission"))
        world-setup (if checkpoint-selection
                      (knowledge-workspace/with-checkpoint world-setup checkpoint-selection)
                      world-setup)
        prepared (work/prepare {:request-id (:request-id input) :task task :model model :limits limits})
        base (:environment prepared)
        evaluator (cond-> (:evaluator prepared)
                    (:input-selection workflow) knowledge-outputs/with-capture)
        verifier (evaluation/evaluator-ref evaluator)
        env (environment/make-environment
             {:id :simmis/saved-workflow :task task
              :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier)
                         :basis (:verifier/basis verifier)}
              :limits (:environment/limits base)
              :world (assoc (:environment/world base) :setup (evaluation/world-setup-ref world-setup))
              :metadata {:workflow workflow}})]
    (assoc prepared :request-kind :workflow :execution-kind :saved-workflow
           :environment env :evaluator evaluator :world-setup world-setup
           :basis (assoc (:basis prepared) :submission input :environment env
                         :source source :inputs inputs :workflow workflow))))

(defn prepare-frozen-batch
  "Shared fixed manual/scheduled builder after original-owner and ready-proof
   checks. Prepare ONLY verified frozen evidence and retained source. This
   performs ordinary provider preparation, not a claim or execution. Native
   owner data remains outside the persisted basis and is rechecked by work."
  [snapshot input proof mapping owner]
  (let [{:keys [frozen source-origin origin]} proof
        selected (execution-origin/selection (:files snapshot) proof)
        prepared (bind-publication
                  (prepare snapshot (dissoc input :expected-owned-source) (:captured frozen) selected)
                  (select-keys frozen [:captured :publication-basis]))
        prepared (-> prepared
                     (assoc-in [:basis :submission] input)
                     (assoc-in [:basis :knowledge/batch] origin)
                     (assoc-in [:basis :knowledge/batch-control] {:mapping mapping})
                     (assoc-in [:basis :source-origin]
                               {:revision (:source-revision source-origin) :target-stamp (:target-stamp source-origin)
                                :target (:target source-origin) :manifest (:manifest snapshot)})
                     (assoc :knowledge/owner owner)
                     (cond-> selected (assoc-in [:basis :knowledge/checkpoint] selected)))]
    (batch-proof/verify-input! proof (:basis prepared))
    prepared))

(defn retry-request
  "HOST workflow journal comparison only. NORMAL conflict is data, not an
   exception classification. No authorization, source, provider or hydration."
  [conn party room-id input]
  (let [id (requests/request-id party room-id (:request-id input))]
    (if-let [row (requests/get-request conn id)]
      (let [retained (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                                                         (:work.request/payload-ref row) (* 128 1024 1024)))
            batch? (or (:work.request/knowledge-batch row) (:work.request/knowledge-frozen row)
                       (contains? (:input retained) :knowledge/batch)
                       (contains? (:input retained) :knowledge/batch-control))]
        ;; Submission equality alone is no proof of retained request integrity.
        ;; Batch retries additionally need both canonical sides of the receipt,
        ;; without consulting today's source, default repository or provider.
        (when-not (and (= (:work.request/intent-id row) (hasch/uuid retained))
                       (= (:work.request/kind row) (:kind retained))
                       (= id (requests/request-id (:work.request/by row) (:work.request/room row)
                                                  (:work.request/client-id row))))
          (invalid! "Retained workflow request integrity failed"))
        (when batch? (batch-proof/request! conn row retained))
        {:status (if (and (= :workflow (:work.request/kind row))
                          (= input (get-in retained [:input :submission])))
                   :retry :conflict)
         :id id})
      {:status :absent})))

(defn submit!
  "Submit current source, or a host-retained immutable snapshot. Exact retries
   are resolved before either source read or provider preparation. A host-retained
   resolution supplies :captured (possibly nil for already-frozen fixtures) and
   explicitly prevents collecting different live inputs."
  [conn host-ctx principal room-id room input & [snapshot resolution]]
  (when-not (m/validate schema input) (invalid! "Invalid workflow execution"))
  (binding [ec/*execution-context* host-ctx]
    (let [party (work/authorize! conn principal room-id)
          {:keys [status id]} (retry-request conn party room-id input)]
      (when (= :conflict status)
        (throw (ex-info "Request identity was used for different work" {:type ::conflict})))
      (if (= :retry status)
        (work/status conn host-ctx principal room-id id)
        (let [snapshot (or snapshot (workflows/snapshot room (:name input)))
              selected (select-keys input source-fields)
              resolution (if resolution
                           (do (when-not (contains? resolution :captured)
                                 (invalid! "Retained resolution has no captured input decision"))
                               resolution)
                           (resolve-admission room-id room snapshot selected))
              captured (:captured resolution)]
          (work/submit-prepared! conn host-ctx principal room-id room input
                                 (bind-publication
                                  (if captured (prepare snapshot input captured) (prepare snapshot input))
                                  resolution)))))))
