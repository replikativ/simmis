(ns is.simm.uis.web.desktop.attempt-board
  "A room's certified Attempts as leaderboards, read from its replica: per
   job (a workflow or experiment Run and the Attempts of its children), per
   experiment candidate, and per model. Every figure is a query over typed
   Attempt and Run attributes, so the board is live wherever the replica is."
  (:require [datahike.api :as d]))

(def job-kinds
  "Run kinds that are jobs: long work whose children are the Attempts."
  #{:workflow :experiment})

(def ^:private attempt-base
  '[:attempt/id :attempt/model :attempt/provider :attempt/status
    :attempt/reward :attempt/elapsed-ms :attempt/started-at
    :attempt/environment-id {:attempt/run [:run/id :run/parent]}])

(def ^:private attempt-optional
  ;; Typed by dvergr since #143; older replicas lack them.
  '[:attempt/microdollars :attempt/experiment-content-id
    :attempt/experiment-candidate])

(defn- installed? [db ident]
  (some? (d/q '[:find ?e . :in $ ?ident :where [?e :db/ident ?ident]] db ident)))

(defn passed?
  "An Attempt passed when its verifier gave it full reward."
  [{:keys [reward]}]
  (boolean (and reward (>= reward 1.0))))

(defn failed?
  "An Attempt failed when it did not complete (the model or its path failed),
   as opposed to completing with a low reward."
  [{:keys [status]}]
  (not= :completed status))

(defn- normalize [a]
  (let [run (:attempt/run a)]
    (cond-> {:id (str (:attempt/id a))
             :run-id (some-> (:run/id run) str)
             :parent-id (some-> (:run/parent run) str)
             :model (:attempt/model a)
             :provider (:attempt/provider a)
             :status (:attempt/status a)
             :reward (:attempt/reward a)
             :elapsed-ms (:attempt/elapsed-ms a)
             :started-at (some-> (:attempt/started-at a) .getTime)
             :environment (:attempt/environment-id a)}
      (contains? a :attempt/microdollars) (assoc :microdollars (:attempt/microdollars a))
      (:attempt/experiment-content-id a)
      (assoc :experiment (str (:attempt/experiment-content-id a)))
      (:attempt/experiment-candidate a)
      (assoc :candidate (:attempt/experiment-candidate a)))))

(defn attempts
  "Every certified Attempt in the replica `db`, oldest first."
  [db]
  (if-not (installed? db :attempt/id)
    []
    (let [pattern (into attempt-base (filter #(installed? db %)) attempt-optional)]
      (->> (d/q '[:find [(pull ?a pattern) ...]
                  :in $ pattern
                  :where [?a :attempt/id _]]
                db pattern)
           (map normalize)
           (sort-by (juxt #(or (:started-at %) 0) :id))
           vec))))

(defn- median [xs]
  (when (seq xs)
    (let [v (vec (sort xs)) n (count v)]
      (if (odd? n)
        (nth v (quot n 2))
        (quot (+ (nth v (dec (quot n 2))) (nth v (quot n 2))) 2)))))

(defn summary
  "Attempts, passes, failures, mean reward, spend and median time of `xs`.
   `:microdollars` is nil when no Attempt records its spend."
  [xs]
  (let [rewards (keep :reward xs)
        spends (keep :microdollars xs)
        passed (count (filter passed? xs))]
    {:attempts (count xs)
     :passed passed
     :failed (count (filter failed? xs))
     :pass-rate (when (seq xs) (/ (double passed) (count xs)))
     :mean-reward (when (seq rewards) (/ (reduce + rewards) (count rewards)))
     :microdollars (when (seq spends) (reduce + spends))
     :microdollars-per-pass (when (and (seq spends) (pos? passed))
                              (quot (reduce + spends) passed))
     :median-elapsed-ms (median (keep :elapsed-ms xs))}))

(defn- ranked
  "Rows of `(summary xs)` per group, best mean reward first, then cheaper."
  [k groups]
  (->> groups
       (map (fn [[g xs]] (assoc (summary xs) k g)))
       (sort-by (juxt #(- (or (:mean-reward %) -1))
                      #(or (:microdollars-per-pass %) #?(:clj Long/MAX_VALUE :cljs js/Number.MAX_SAFE_INTEGER))
                      #(str (get % k))))
       vec))

(defn by-model [xs] (ranked :model (group-by :model xs)))

(defn by-candidate [xs] (ranked :candidate (group-by :candidate xs)))

(defn- job-runs [db]
  (->> (d/q '[:find ?id ?kind ?status ?started
              :in $ [?kind ...]
              :where
              [?r :run/kind ?kind]
              [?r :run/id ?id]
              [?r :run/status ?status]
              [?r :run/started-at ?started]]
            db (vec job-kinds))
       (map (fn [[id kind status started]]
              {:id (str id) :kind kind :status status :started-at (.getTime started)}))))

(defn- child-statuses
  "Statuses of every Run with a parent, by parent id."
  [db]
  (->> (d/q '[:find ?parent ?child ?status
              :where
              [?c :run/parent ?parent]
              [?c :run/id ?child]
              [?c :run/status ?status]]
            db)
       (reduce (fn [m [parent _ status]] (update m (str parent) (fnil conj []) status)) {})))

(defn board
  "The replica `db`'s Attempts as leaderboards:
   `{:total summary :models rows :jobs [...] :experiments [...]}`. A job is a
   workflow or experiment Run: its children are running or certified; while
   one runs, its `:running` count and certified Attempts move live."
  [db]
  (let [xs (attempts db)
        runs? (installed? db :run/kind)
        by-parent (group-by :parent-id xs)
        children (if runs? (child-statuses db) {})
        scored (if (installed? db :scorecard/experiment-content-id)
                 (set (map str (d/q '[:find [?e ...] :where [_ :scorecard/experiment-content-id ?e]] db)))
                 #{})]
    {:total (summary xs)
     :models (by-model xs)
     :jobs (->> (if runs? (job-runs db) [])
                (map (fn [job]
                       (let [mine (get by-parent (:id job) [])
                             statuses (get children (:id job) [])]
                         (assoc job
                                :children (count statuses)
                                :running (count (filter #{:running} statuses))
                                :summary (summary mine)
                                :models (by-model mine)))))
                (sort-by :started-at >)
                vec)
     :experiments (->> (filter :experiment xs)
                       (group-by :experiment)
                       (map (fn [[id ys]]
                              {:id id
                               :scorecard? (contains? scored id)
                               :started-at (apply min (keep :started-at ys))
                               :summary (summary ys)
                               :candidates (by-candidate ys)}))
                       (sort-by :started-at >)
                       vec)}))

(defn dollars
  "Microdollars as a short dollar label; \"—\" when unknown."
  [microdollars]
  (cond
    (nil? microdollars) "—"
    (zero? microdollars) "$0"
    (< microdollars 10000) (str "$" #?(:clj (format "%.4f" (/ microdollars 1e6))
                                      :cljs (.toFixed (/ microdollars 1e6) 4)))
    :else (str "$" #?(:clj (format "%.2f" (/ microdollars 1e6))
                      :cljs (.toFixed (/ microdollars 1e6) 2)))))
