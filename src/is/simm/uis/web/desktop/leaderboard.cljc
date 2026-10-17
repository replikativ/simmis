(ns is.simm.uis.web.desktop.leaderboard
  "The words and figures of an experiment's leaderboard. The data is dvergr's
   (`is.simm.ops.experiments/leaderboard`: `scorecard_detail`,
   `experiment_progress`, `experiment_report`); this namespace only says it,
   in the words dvergr's report uses, so the board and `report.md` read the
   same.")

(defn experiment-label
  "Prefer the workflow title frozen at admission, not mutable authoring files."
  [{:keys [workflow experiment-id]}]
  (or (not-empty (:title workflow)) (not-empty (:name workflow)) experiment-id "Experiment"))

(defn candidate-label
  "A candidate's name without its namespace (`workflow/haiku-0` → `haiku-0`)."
  [c]
  (let [s (str c)
        i #?(:clj (.lastIndexOf ^String s "/") :cljs (.lastIndexOf s "/"))]
    (subs s (inc i))))

(defn p-label
  "A p-value as dvergr's report prints it: two significant digits, and
   \"< 0.0001\" below that."
  [p]
  (cond
    (nil? p) "—"
    (< p 1e-4) "< 0.0001"
    :else #?(:clj (.toPlainString (.round (bigdec p) (java.math.MathContext. 2)))
             :cljs (let [s (.toPrecision (js/Number p) 2)]
                     ;; toPrecision may answer in exponent form (1e-3 → "0.0010"
                     ;; is fine, but 5e-5 is caught above); keep it plain
                     (if (re-find #"e" s) (str (js/Number s)) s)))))

(defn verdict-label
  "A paired comparison's verdict in plain words: which of A and B is better,
   or that these cases do not separate them, and why when they cannot."
  [{:keys [a b verdict decidable?]}]
  (case (str verdict)
    "a-better" (str (candidate-label a) " better")
    "b-better" (str (candidate-label b) " better")
    (if decidable?
      "no detectable difference at these cases"
      "no detectable difference: too few disagreements to decide")))

(defn verdict-kind
  "`:better` when the pair differs, `:undecidable` when it could not, else
   `:none`: what the row's styling says."
  [{:keys [verdict decidable?]}]
  (cond (#{"a-better" "b-better"} (str verdict)) :better
        (not decidable?) :undecidable
        :else :none))

(defn policy-note
  "What the paired comparisons compare, from their `:policy`."
  [{:keys [policy baseline pairs-omitted family-size comparisons alpha]}]
  (let [m (or family-size (count comparisons))]
    (str (case (str policy)
           "against-baseline" (str "Each candidate against the baseline, " (candidate-label baseline) ". ")
           "against-best" (str "Each candidate against the one with the highest pass rate, "
                               (candidate-label baseline) " (the other " pairs-omitted " pairs are not shown). ")
           "Every pair of candidates. ")
         "A difference is a p below " (or alpha 0.05)
         (cond (<= m 1) ""
               (= m (count comparisons)) (str ", after Holm's correction for the " m " comparisons shown")
               :else (str ", after Holm's correction for all " m " pairs (choosing the best looked at all of them)"))
         ".")))

(defn repetitions?
  "Do the comparisons count cases over several repetitions (a *Tied* column)?"
  [{:keys [comparisons]}]
  (boolean (some #(< 1 (or (:repetitions %) 1)) comparisons)))

(defn exclusions
  "Pairs that left cases or cells out (a fault, or not run yet), as
   `[{:a :b :cases :cells}]`."
  [{:keys [comparisons]}]
  (vec (for [{:keys [a b excluded-cases excluded-cells]} comparisons
             :when (or (pos? (or excluded-cases 0)) (pos? (or excluded-cells 0)))]
         {:a a :b b :cases (or excluded-cases 0) :cells (or excluded-cells 0)})))

(defn seconds-label
  "Seconds as \"12 s\" or \"3.5 min\"; \"—\" when unknown."
  [s]
  (cond
    (nil? s) "—"
    (< s 120) (str (Math/round (double s)) " s")
    :else (let [m (/ (double s) 60.0)]
            #?(:clj (format "%.1f min" m) :cljs (str (.toFixed m 1) " min")))))

(defn tokens-label
  "A token count, grouped by thousands (\"42,236\"); \"—\" when unknown."
  [n]
  (if (nil? n)
    "—"
    (let [s (str (long n))
          head (mod (count s) 3)]
      (->> (concat (when (pos? head) [(subs s 0 head)])
                   (map #(apply str %) (partition 3 (subs s head))))
           (interpose ",")
           (apply str)))))
