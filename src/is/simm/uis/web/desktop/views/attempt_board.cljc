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
        (el/th {:class "num"} "Failed")
        (el/th {:class "num"} "Mean reward")
        (el/th {:class "num"} "Spend")
        (el/th {:class "num"} "Per pass")
        (el/th {:class "num"} "Median time")))
    (el/tbody {}
      (map (fn [row]
             (el/tr {:key (str (get row k))}
               (el/td {:class "attempt-board-name"} (label (get row k)))
               (el/td {:class "num"} (str (:attempts row)))
               (el/td {:class "num"} (str (:passed row) " · " (percent (:pass-rate row))))
               (el/td {:class (str "num" (when (pos? (:failed row)) " attempt-board-failed"))}
                 (str (:failed row)))
               (el/td {:class "num"} (reward-label (:mean-reward row)))
               (el/td {:class "num"} (board/dollars (:microdollars row)))
               (el/td {:class "num"} (board/dollars (:microdollars-per-pass row)))
               (el/td {:class "num"} (elapsed-label (:median-elapsed-ms row)))))
           rows))))

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
          (el/p {} "Certified Attempts by job, experiment candidate and model: reward, spend, failures.")))
      (el/main {:class "run-history-body"}
        (el/div {:class "run-history-summary"}
          (el/span {} (str (:attempts total) " attempts"))
          (el/span {} (str (:passed total) " passed"))
          (when (pos? (:failed total))
            (el/span {:class "run-history-summary-failed"} (str (:failed total) " failed")))
          (el/span {} (str "spend " (board/dollars (:microdollars total))))
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
                (map (fn [{:keys [id scorecard? candidates summary]}]
                       (el/div {:key id :class "attempt-board-experiment"}
                         (el/div {:class "attempt-board-experiment-title"}
                           (el/span {:class "run-history-id"} (short-id id))
                           (el/span {} (str (:attempts summary) " cells"))
                           (el/span {:class (if scorecard? "attempt-board-scored" "run-history-summary-active")}
                             (if scorecard? "Scorecard certified" "in progress")))
                         (table "Candidate" :candidate candidates)))
                     experiments)))
            (when (seq jobs)
              (el/section {:class "attempt-board-section"}
                (el/h3 {} "Jobs")
                (map #(job-card % on-open-run) jobs)))))))))
