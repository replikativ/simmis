(ns is.simm.uis.web.desktop.workflow-editor
  "Pure draft and bounded launch presentation; execution remains server-owned."
  (:require [clojure.string :as str]))

(def scaffold
  {"workflow.edn" "{:title \"New workflow\"\n :task \"{goal} Read /docs/input.md and write /out/result.md.\"\n :params {:goal \"Summarize the input.\"}\n :capture [\"/out\"]}\n"
   "fixtures/docs/input.md" "Replace this with your input.\n"
   "gold.edn" "{}\n"
   "checker.clj" "(fn [_] (throw (ex-info \"Replace this placeholder with a real checker before benchmarking\" {})))\n"})

(def defaults
  {:model "" :case "" :mode "run" :repetitions "1"
   :budget-microdollars "100000" :max-dispatches "4"
   :max-model-steps "4" :timeout-ms "120000" :settlement "review"})

(defn parameters [source]
  (into (sorted-map) (map (fn [[k v]] [(name k) v])) (get-in source [:definition :params])))

(defn draft [source]
  {:source source :files (:files source) :path (or (when (contains? (:files source) "workflow.edn") "workflow.edn")
                                                   (first (sort (keys (:files source)))))
   :form (assoc defaults :params (parameters source))})

(defn dirty? [draft] (not= (:files draft) (get-in draft [:source :files])))

(defn reloaded [old source]
  (merge (draft source) (select-keys old [:submission :fund-request :funded? :fund-reply])))

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

(defn limits [form]
  {:budget-microdollars (bounded-integer form :budget-microdollars 1 5000000)
   :max-dispatches (bounded-integer form :max-dispatches 1 32)
   :max-model-steps (bounded-integer form :max-model-steps 1 32)
   :timeout-ms (bounded-integer form :timeout-ms 1000 600000)
   :settlement (if (= "benchmark" (:mode form)) "discard"
                   (if (#{"review" "discard"} (:settlement form)) (:settlement form)
                       (invalid! "Choose review or discard settlement")))})

(defn submission [draft request-id]
  (when (dirty? draft) (invalid! "Save and inspect these changes before running"))
  (let [{:keys [source form]} draft
        inspection (:inspection source)
        cases (:cases inspection)
        selected (:case form)
        benchmark? (= "benchmark" (:mode form))
        params (:params form)
        model (str/trim (:model form))]
    (when-not (:ok inspection) (invalid! "Repair this workflow before running"))
    (when (or (str/blank? model) (> (count model) 200)) (invalid! "Enter an explicit model id"))
    (when (and (seq selected) (not (some #{selected} cases))) (invalid! "Select a case from this revision"))
    (when (and (not benchmark?) (seq cases) (str/blank? selected)) (invalid! "Select an explicit case"))
    (let [base {:request-id request-id :name (:name source) :expected-bundle (:bundle inspection)
                :limits (limits form)}]
      (if benchmark?
        (let [repetitions (bounded-integer form :repetitions 1 2)
              cell-cases (if (seq selected) [selected] cases)]
          (when (not= params (parameters source)) (invalid! "Benchmarking uses saved defaults; save parameter changes in workflow.edn first"))
          (when (or (> (count cell-cases) 4) (> (* (max 1 (count cell-cases)) repetitions) 8))
            (invalid! "Choose fewer benchmark cases (maximum eight cells)"))
          (cond-> (assoc base :models [model] :repetitions repetitions)
            (seq cell-cases) (assoc :cases (vec cell-cases))))
        (do
          (when-not (and (<= (count params) 32) (every? string? (vals params)))
            (invalid! "Ordinary workflow parameters must be literal strings"))
          (cond-> (assoc base :model model :params params)
            (seq selected) (assoc :case selected)))))))
