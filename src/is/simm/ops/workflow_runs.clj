(ns is.simm.ops.workflow-runs
  "Ordinary saved-workflow reuse, not authored checker execution. One pinned
   task, one inherited room fork, the existing bounded-work runner/evaluator."
  (:require [clojure.string :as str]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.room :as catalog]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.rooms.files :as files]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def policy {:version 1 :max-params 32 :max-param-bytes 65536
             :max-task-bytes 16000 :max-input-files 100 :max-input-bytes 2097152
             :source :pinned-bundle :checker :not-executed :parent :room-fork})
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

(defn transport-for [schema input]
  (when-not (m/validate (transport-schema-for schema) input) (invalid! "Invalid workflow execution"))
  (reduce (fn [input field]
            (if (contains? input field)
              (update input field
                      #(reduce-kv
                        (fn [out key value]
                          (let [key (if (keyword? key) (subs (str key) 1) key)]
                            (when (contains? out key) (invalid! "Duplicate workflow input key"))
                            (assoc out key value))) {} %))
              input)) input [:params :inputs]))

(defn transport [input] (transport-for schema input))

(defn- byte-count [^String s] (alength (.getBytes s "UTF-8")))

(defn- effective-parameters [definition overrides]
  (let [defaults (:params definition {})]
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

(defn- setup [name bundle source entries]
  (let [prefix (str "/workflows/" name "/")
        digest (str (hasch/uuid entries))]
    (evaluation/make-world-setup
     {:id :simmis/workflow-inputs :version 1
      :basis {:name name :bundle (str (:id bundle)) :inputs digest :policy policy}
      :prepare
      (fn [{world :room}]
        ;; This is the RUN WORLD, never the source room. Replace the entire
        ;; named subtree: extra helper files from a newer inherited revision
        ;; must not leak into the pinned execution. No source is evaluated.
        (workspace/ensure-workspace! world)
        (let [inherited (:files (workflows/snapshot world name))
              edits (merge (zipmap (map #(str prefix %) (keys inherited)) (repeat nil))
                           (into {} (map (fn [[p text]] [(str prefix p) {:text text}])) source))]
          (files/write! world edits {:message (str "Pin workflow " name " at " (:id bundle))}))
        (when (seq entries) (workspace/seed! world entries))
        {:bundle (str (:id bundle)) :inputs digest :files (count entries)
         :checker-executed? false :source-pinned? true})})))

(defn prepare-source
  "Validate and render ONE immutable workflow revision without resolving a
   model or executing checker source. Shared by execution backends; the source
   pin, literal parameter rendering and input overlay rules are identical."
  [snapshot {:keys [name expected-bundle case params inputs] :as input}]
  (when-not (m/validate source-schema input) (invalid! "Invalid workflow source selection"))
  (let [inspection (workflows/inspect-snapshot name snapshot)]
    (when-not (:ok inspection) (invalid! "Workflow bundle is not structurally valid"))
    (when-not (= (str expected-bundle) (:bundle inspection))
      (throw (ex-info "Workflow revision changed; inspect the new revision before running it"
                      {:type ::conflict}))))
  (let [bundle (catalog/bundle name (:files snapshot))
        definition (:definition bundle)]
    (when-not (and (contains? #{nil "developer"} (:profile definition))
                   (contains? #{nil ["/out"]} (:capture definition))
                   (not-any? #(contains? definition %)
                             [:judge :web :fetched :allowed-sources :blocked-sources]))
      (invalid! "Ordinary workflows require the developer profile, /out capture and no web/judge policy"))
    (when-not (if (seq (:cases bundle)) (contains? (:cases bundle) case) (nil? case))
      (invalid! "Select an explicit existing case for a case-pack workflow, and no case otherwise"))
    (let [params (effective-parameters definition params)
          task (render (:task definition) params)
          prefix (str "/workflows/" name "/")
          entries (input-files prefix (merge (:fixtures bundle) (get-in bundle [:cases case :fixtures])) inputs)
          provenance {:name name :bundle (str expected-bundle) :case case :params params :policy policy}]
      {:bundle bundle :task task :inputs entries :source (:files snapshot)
       :world-setup (setup name bundle (:files snapshot) entries) :workflow provenance})))

(defn prepare
  "Construct from ONE immutable source snapshot. The bundle is task data;
   provider, tool/effect surface, limits and evaluator remain host policy."
  [snapshot {:keys [model limits] :as input}]
  (when-not (m/validate schema input) (invalid! "Invalid workflow execution"))
  (let [{:keys [task inputs source world-setup workflow]} (prepare-source snapshot (select-keys input source-fields))
        prepared (work/prepare {:request-id (:request-id input) :task task :model model :limits limits})
        base (:environment prepared)
        verifier (:environment/verifier base)
        env (environment/make-environment
             {:id :simmis/saved-workflow :task task
              :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier)
                         :basis (:verifier/basis verifier)}
              :limits (:environment/limits base)
              :world (assoc (:environment/world base) :setup (evaluation/world-setup-ref world-setup))
              :metadata {:workflow workflow}})]
    (assoc prepared :request-kind :workflow :execution-kind :saved-workflow
           :environment env :world-setup world-setup
           :basis (assoc (:basis prepared) :submission input :environment env
                         :source source :inputs inputs :workflow workflow))))

(defn submit!
  "Submit current source, or a host-retained immutable snapshot. Exact retries
   are resolved before either source read or provider preparation."
  [conn host-ctx principal room-id room input & [snapshot]]
  (when-not (m/validate schema input) (invalid! "Invalid workflow execution"))
  (binding [ec/*execution-context* host-ctx]
    (let [party (work/authorize! conn principal room-id)
          id (requests/request-id party room-id (:request-id input))]
      (if-let [row (requests/get-request conn id)]
        (do
          (when-not (and (= :workflow (:work.request/kind row))
                         (= input (get-in (requests/payload conn row) [:input :submission])))
            (throw (ex-info "Request identity was used for different work" {:type ::conflict})))
          (work/status conn host-ctx principal room-id id))
        (work/submit-prepared! conn host-ctx principal room-id room input
                               (prepare (or snapshot (workflows/snapshot room (:name input))) input))))))
