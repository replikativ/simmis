(ns is.simm.uis.web.desktop.views.attempt-board
  "A room's Attempts: jobs with their live progress, experiment candidates and
   models ranked by reward and cost."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.attempt-board :as board]
            [is.simm.uis.web.desktop.views.chat :as chat]
            [is.simm.uis.web.desktop.views.core :as vc])
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el])))

(defn- percent [x]
  (if x (str (Math/round (* 100 (double x))) "%") "—"))

(defn- reward-label [x]
  (if x
    #?(:clj (format "%.2f" (double x)) :cljs (.toFixed x 2))
    "—"))

(defn- range-label
  "A 95% range as \"28–97%\" (rates) or \"0.61–0.99\" (rewards); \"—\"
   when there is none (one attempt says nothing about spread)."
  [[lo hi] fmt]
  (if lo (str (fmt lo) "–" (fmt hi)) "—"))

(defn- pct [x] (str (Math/round (* 100 (double x)))))

(defn- signed [x]
  (if x (str (when (pos? x) "+") (reward-label x)) "—"))

(defn- elapsed-label [ms]
  (cond
    (nil? ms) "—"
    (< ms 1000) (str ms " ms")
    (< ms 60000) (str (quot ms 1000) " s")
    :else (str (quot ms 60000) " min")))

(defn- time-label [millis]
  #?(:cljs (when millis (chat/msg-timestamp (js/Date. millis)))
     :clj (some-> millis str)))

(defn- short-id [id]
  (let [s (str id)] (subs s 0 (min 8 (count s)))))

(defn- label [x]
  (cond (keyword? x) (name x)
        (nil? x) "—"
        :else (str x)))

(defn- table
  "A leaderboard of summary `rows`, keyed by `k` (its column titled `title`)."
  [title k rows]
  (el/table {:class "attempt-board-table"}
    (el/thead {}
      (el/tr {}
        (el/th {} title)
        (el/th {:class "num"} "Attempts")
        (el/th {:class "num"} "Passed")
        (el/th {:class "num" :title "95% range of the pass rate"} "95% range")
        (el/th {:class "num"} "Failed")
        (el/th {:class "num"} "Mean reward")
        (el/th {:class "num" :title "What was paid"} "Spend")
        (el/th {:class "num" :title "What the tokens are worth at list price (a subscription run's too)"} "At list price")
        (el/th {:class "num" :title "Cost per passed attempt, at list price"} "Per pass")
        (el/th {:class "num"} "Median time")))
    (el/tbody {}
      (map (fn [row]
             (el/tr {:key (str (get row k))}
               (el/td {:class "attempt-board-name"} (label (get row k)))
               (el/td {:class "num"} (str (:attempts row)))
               (el/td {:class "num"} (str (:passed row) " · " (percent (:pass-rate row))))
               (el/td {:class "num attempt-board-range"} (str (range-label (:pass-rate-interval row) pct)
                                                              (when (:pass-rate-interval row) "%")))
               (el/td {:class (str "num" (when (pos? (:failed row)) " attempt-board-failed"))}
                 (str (:failed row)))
               (el/td {:class "num"}
                 (reward-label (:mean-reward row))
                 (when (:reward-interval row)
                   (el/span {:class "attempt-board-range"}
                     (str " (" (range-label (:reward-interval row) reward-label) ")"))))
               (el/td {:class "num"} (board/dollars (:microdollars row)))
               (el/td {:class "num"} (board/dollars (:notional-microdollars row)))
               (el/td {:class "num"} (board/dollars (:notional-microdollars-per-pass row)))
               (el/td {:class "num"} (elapsed-label (:median-elapsed-ms row)))))
           rows))))

(defn- comparison-table
  "Every other candidate against the best one: how likely it is as good, the
   reward difference on the same worlds, and what a pass saves."
  [{:keys [baseline rows]}]
  (el/div {:class "attempt-board-comparison"}
    (el/h4 {} (str "Against " (label baseline)))
    (el/table {:class "attempt-board-table"}
      (el/thead {}
        (el/tr {}
          (el/th {} "Candidate")
          (el/th {:class "num" :title "Probability that its pass rate is higher"} "P(better)")
          (el/th {:class "num" :title "Probability that its pass rate is no more than 5 points lower"} "P(no worse)")
          (el/th {:class "num" :title "Mean reward difference on the worlds both ran, with its 95% range"} "Reward Δ")
          (el/th {:class "num"} "Worlds")
          (el/th {:class "num"} "Saved per pass")
          (el/th {:class "num" :title "Its cost per attempt at list price, as a share of the best's"} "Cost vs best")))
      (el/tbody {}
        (map (fn [r]
               (el/tr {:key (str (:candidate r))}
                 (el/td {:class "attempt-board-name"} (label (:candidate r)))
                 (el/td {:class "num"} (percent (:p-higher r)))
                 (el/td {:class "num"} (percent (:p-no-worse r)))
                 (el/td {:class "num"}
                   (signed (:reward-difference r))
                   (when (:reward-difference-interval r)
                     (el/span {:class "attempt-board-range"}
                       (let [[lo hi] (:reward-difference-interval r)]
                         (str " (" (signed lo) " to " (signed hi) ")")))))
                 (el/td {:class "num"} (str (:paired-worlds r)))
                 (el/td {:class "num"} (board/dollars (:microdollars-per-pass-saved r)))
                 (el/td {:class "num"} (percent (or (:notional-cost-ratio r) (:cost-per-pass-ratio r))))))
             rows)))))

(def ^:private checks-shown
  "Failing checks listed per experiment; an Attempt that failed outright
   fails every check, and the list would say nothing more."
  8)

(defn- checks-table
  "The checks some candidate does not always pass, with each candidate's pass
   rate, most failed first."
  [candidates checks]
  (el/div {:class "attempt-board-checks"}
    (el/h4 {} "Checks that fail")
    (el/table {:class "attempt-board-table"}
      (el/thead {}
        (el/tr {}
          (el/th {} "Check")
          (map (fn [c] (el/th {:key (str c) :class "num"} (label c))) candidates)))
      (el/tbody {}
        (map (fn [{:keys [check rates]}]
               (el/tr {:key (str check)}
                 (el/td {:class "attempt-board-name"} (if (keyword? check) (str (when (namespace check) (str (namespace check) "/")) (name check)) (str check)))
                 (map (fn [c] (el/td {:key (str c)
                                      :class (str "num" (when (some-> (get rates c) (< 1.0)) " attempt-board-failed"))}
                                (percent (get rates c))))
                      candidates)))
             (take checks-shown checks))))
    (when (< checks-shown (count checks))
      (el/p {:class "attempt-board-range"}
        (str "and " (- (count checks) checks-shown) " more")))))

(defn- job-card [{:keys [id kind status started-at children running summary models]} on-open-run]
  (el/section {:key id :class "attempt-board-job" :data-job-id id}
    (el/button {:class "run-history-card"
                :title "Open the job's Run"
                :on-click (fn [event] (when on-open-run (on-open-run event {:id id :actor-name (str (name kind) " job")})))}
      (el/span {:class (str "run-history-status-dot run-history-status-dot--" (name (or status :unknown)))})
      (el/span {:class "run-history-card-main"}
        (el/span {:class "run-history-card-title"} (str (str/capitalize (name kind)) " job"))
        (el/span {:class "run-history-card-detail"}
          (str (label status)
               " · " (:attempts summary) " of " children " certified"
               (when (pos? running) (str " · " running " running"))
               " · " (board/dollars (:microdollars summary)))))
      (el/span {:class "run-history-card-tail"}
        (el/span {:class "run-history-time"} (time-label started-at))
        (el/span {:class "run-history-id"} (short-id id)))
      (vc/icon "chevron-right" {:class "run-history-open-icon"}))
    (when (seq models)
      (table "Model" :model models))))

(defn view
  [{:keys [room-name board on-open-run on-back-room]}]
  (let [{:keys [total models jobs experiments]} board
        running (reduce + 0 (map :running jobs))]
    (el/div {:class "run-history attempt-board"}
      (el/header {:class "run-history-header"}
        (el/button {:class "run-history-back"
                    :title "Back to room"
                    :on-click (fn [_] (when on-back-room (on-back-room)))}
          (vc/icon "arrow-left"))
        (el/div {:class "run-history-heading"}
          (el/div {:class "run-history-kicker"}
            (vc/icon "trophy")
            (or room-name "Room"))
          (el/h2 {} "Attempts")
          (el/p {} "Certified Attempts by job, experiment candidate and model: reward with its 95% range, spend, failures, and each candidate against the best.")))
      (el/main {:class "run-history-body"}
        (el/div {:class "run-history-summary"}
          (el/span {} (str (:attempts total) " attempts"))
          (el/span {} (str (:passed total) " passed"))
          (when (pos? (:failed total))
            (el/span {:class "run-history-summary-failed"} (str (:failed total) " failed")))
          (el/span {} (str "spend " (board/dollars (:microdollars total))
                           (when (not= (:notional-microdollars total) (:microdollars total))
                             (str " · " (board/dollars (:notional-microdollars total)) " at list price"))))
          (when (pos? running)
            (el/span {:class "run-history-summary-active"} (str running " running"))))
        (if (and (zero? (:attempts total)) (empty? jobs))
          (el/div {:class "run-history-empty"}
            (vc/icon "trophy")
            (el/h3 {} "No Attempts yet")
            (el/p {} "Start a workflow or a benchmark (workflow_start, catalog_benchmark) and its Attempts appear here."))
          (el/div {:class "attempt-board-sections"}
            (when (seq models)
              (el/section {:class "attempt-board-section"}
                (el/h3 {} "Models")
                (table "Model" :model models)))
            (when (seq experiments)
              (el/section {:class "attempt-board-section"}
                (el/h3 {} "Experiments")
                (map (fn [{:keys [id scorecard? candidates summary comparison checks]}]
                       (el/div {:key id :class "attempt-board-experiment"}
                         (el/div {:class "attempt-board-experiment-title"}
                           (el/span {:class "run-history-id"} (short-id id))
                           (el/span {} (str (:attempts summary) " cells"))
                           (el/span {:class (if scorecard? "attempt-board-scored" "run-history-summary-active")}
                             (if scorecard? "Scorecard certified" "in progress")))
                         (table "Candidate" :candidate candidates)
                         (when comparison (comparison-table comparison))
                         (when (seq checks) (checks-table (map :candidate candidates) checks))))
                     experiments)))
            (when (seq jobs)
              (el/section {:class "attempt-board-section"}
                (el/h3 {} "Jobs")
                (map #(job-card % on-open-run) jobs)))))))))
