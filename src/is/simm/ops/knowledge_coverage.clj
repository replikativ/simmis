(ns is.simm.ops.knowledge-coverage
  "Bounded original-evidence coverage claims. A valid report is not proof of
   understanding, factual truth, authority or adoption. Unresolved work holds
   the entire batch; this codec never acknowledges or publishes sources."
  (:require [clojure.edn :as edn]
            [clojure.set :as set]
            [dvergr.agent.evaluation :as evaluation]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-scope :as scope]
            [malli.core :as m])
  (:import [java.io PushbackReader StringReader]))

(def max-report-bytes 65536)
(def output-path "/out/coverage.edn")
(def declaration-value {:version 1 :output output-path :unresolved :hold-batch})

(defn- refuse! [reason]
  (throw (ex-info "Original evidence coverage is held" {:type ::held :reason reason})))

(defn read-value
  "One inert bounded EDN value, with no custom readers or trailing forms."
  [text]
  (when-not (and (string? text) (<= (count text) max-report-bytes)
                 (<= (alength (.getBytes ^String text "UTF-8")) max-report-bytes))
    (refuse! :invalid-report))
  (try
    (with-open [reader (PushbackReader. (StringReader. text))]
      (let [eof (Object.)
            options {:eof eof :readers {} :default (fn [& _] (refuse! :invalid-report))}
            value (edn/read options reader)]
        (when (or (identical? eof value) (not (identical? eof (edn/read options reader))))
          (refuse! :invalid-report))
        value))
    (catch StackOverflowError _ (refuse! :invalid-report))
    (catch Exception _ (refuse! :invalid-report))))

(defn declaration
  "Explicit source opt-in. Old starter versions acquire no new requirement."
  [source]
  (when-let [text (get source "coverage-policy.edn")]
    (when-not (= declaration-value (read-value text)) (refuse! :invalid-declaration))
    declaration-value))

(def item-schema
  [:map {:closed true}
   [:input [:string {:min 1 :max 1024}]] [:content :uuid]
   [:status [:enum :incorporated :already-represented :no-change :deferred]]
   [:targets [:vector {:max 100} [:string {:min 1 :max 512}]]]
   [:note [:string {:min 1 :max 2000}]]])

(def report-schema
  [:map {:closed true} [:version [:= 1]] [:trigger-id :uuid]
   [:items [:vector {:max 98} item-schema]]])

(defn expected
  "Exact selected trigger membership from an already HOST-validated manifest.
   This small descriptor retains no bodies or executable values."
  [manifest]
  (let [files (:files manifest)
        selected (filterv #(= :trigger (:role %)) files)
        items (mapv #(select-keys % [:input :content]) selected)]
    (when-not (and (uuid? (:trigger-id manifest)) (vector? files)
                   (<= (count files) 98)
                   (every? #(and (string? (:input %)) (uuid? (:content %))) items)
                   (= (count items) (count (set (map :input items)))))
      (refuse! :invalid-selection))
    {:version 1 :trigger-id (:trigger-id manifest) :items items}))

(defn inspect
  "Validate exact coverage claims against original selected inputs and actual
   proposed targets. TARGET? is a host-selected pure path predicate, never a
   function read from authored source. DEFERRED never becomes a successful no-op.
   Original evidence need not appear in any digest to belong to this selection."
  [selection report proposed-targets target?]
  (when-not (and (m/validate report-schema report)
                 (= (:trigger-id selection) (:trigger-id report))
                 (vector? (:items selection))
                 (= (count (:items selection)) (count (:items report))))
    (refuse! :invalid-membership))
  (let [selected (set (:items selection))
        reported (mapv #(select-keys % [:input :content]) (:items report))]
    (when-not (and (= (count selected) (count (:items selection)))
                   (= (count reported) (count (set reported)))
                   (= selected (set reported)))
      (refuse! :invalid-membership)))
  (doseq [{:keys [status targets]} (:items report)]
    (when-not (and (= (count targets) (count (set targets)))
                   (every? target? targets)
                   (case status
                     :incorporated (and (seq targets) (every? proposed-targets targets))
                     :already-represented (and (seq targets)
                                              (every? (set (:existing-targets selection)) targets))
                     (:no-change :deferred) (empty? targets)
                     false))
      (refuse! :invalid-targets)))
  (let [claimed (into #{} (mapcat :targets) (filter #(= :incorporated (:status %)) (:items report)))]
    (when-not (set/subset? (set proposed-targets) claimed)
      (refuse! :unattributed-output)))
  {:version 1 :selection (:trigger-id selection)
   :status (if (some #(= :deferred (:status %)) (:items report)) :held :ready)
   :report-id (hasch/uuid [:simmis/original-evidence-coverage-v1 report])
   :items (:items report)})

(defn with-existing-targets
  "Add regular Markdown occupancy from the HOST-retained publication basis.
   The reference check proves existence, not the meaning of an existing note."
  [selection publication-basis output-scope]
  (assoc selection :existing-targets
         (into (sorted-set)
               (keep (fn [[path entry]]
                       (when (and (= 33188 (:mode entry))
                                  (re-find #"(?i)\.(md|markdown)$" path)
                                  (try (scope/target-path! output-scope path) true
                                       (catch Exception _ false))) path)))
               (:files publication-basis))))

(defn assess-evidence
  "Capture-side validation. Retain useful output even when coverage is held;
   an unresolved/invalid report cannot become successful maintenance."
  [selection output-scope evidence]
  (try
    (inspect selection (read-value (get-in evidence [:files output-path]))
             (set (keys (get-in evidence [:knowledge-output :files])))
             (fn [path] (try (scope/target-path! output-scope path) true
                            (catch Exception _ false))))
    (catch clojure.lang.ExceptionInfo error
      (if (= ::held (:type (ex-data error)))
        {:version 1 :selection (:trigger-id selection) :status :held
         :reason (:reason (ex-data error))}
        (throw error)))))

(defn with-capture
  "Wrap the normal completion evaluator, preserving its captured outputs and
   tier. The exact original selection participates in verifier identity."
  [base selection output-scope]
  (scope/normalize! output-scope)
  (evaluation/make-evaluator
   {:id :simmis/original-evidence-completion :version 1
    :basis {:base (evaluation/evaluator-ref base) :selection selection :scope output-scope
            :policy declaration-value}
    :tier (evaluation/evaluator-tier base)
    :capture (fn [context]
               (let [evidence ((:capture base) context)]
                 (assoc evidence :knowledge-coverage
                        (assess-evidence selection output-scope evidence))))
    :observe (fn [context]
               (assoc ((:observe base) context)
                      :knowledge-coverage (get-in context [:execution/evidence :knowledge-coverage])))
    :verify (fn [context observed]
              (let [result ((:verify base) context observed)
                    ready? (= :ready (get-in observed [:knowledge-coverage :status]))]
                (-> result
                    (update :checks assoc :original-evidence-covered? ready?)
                    (assoc :reward (if ready? (:reward result) 0.0)))))}))

(defn require-ready!
  "Recheck retained report against exact frozen membership and actual output
   before filing. Never acknowledge a partial batch from authored claims."
  [selection output-scope evidence]
  (let [result (assess-evidence selection output-scope evidence)]
    (when-not (and (= :ready (:status result)) (= result (:knowledge-coverage evidence)))
      (refuse! :coverage-incomplete))
    result))
