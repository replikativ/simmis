(ns is.simm.uis.web.desktop.views.leaderboard
  "A room's experiments as leaderboards: per experiment its candidates with the
   pass rate and its 95% interval, cost at list price, tokens and time, and
   the paired comparisons (exact McNemar, Holm-adjusted) with a verdict in
   plain words. Every figure is dvergr's (`is.simm.ops.experiments`); the
   styling is the Attempts board's."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.attempt-board :as board]
            [is.simm.uis.web.desktop.leaderboard :as lb]
            [is.simm.uis.web.desktop.views.core :as vc]
            #?(:cljs [is.simm.uis.web.desktop.signals :as sig])
            #?(:cljs [clojure.core.async :refer [go <! put! promise-chan] :include-macros true])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.experiments-remote :as er])
            #?(:cljs [is.simm.uis.web.desktop.aggregate :as agg])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.incremental.interval]))
  #?(:cljs (:require [org.replikativ.spindel.incremental.interval :as iv]
                     [org.replikativ.spindel.effects.track :refer [track]])))

;; =============================================================================
;; Loading
;; =============================================================================

#?(:cljs (defonce ^:private gates (atom {})))

#?(:cljs
   (defonce ^:private requested
     ;; room-id -> the Attempt count last asked for: a render while that fetch
     ;; is out asks for nothing new
     (atom {})))

#?(:cljs
   (defn- gate-for [room-id]
     (or (get @gates room-id)
         (get (swap! gates #(if (contains? % room-id) % (assoc % room-id (agg/gate)))) room-id))))

#?(:cljs
   (defn load-leaderboard!
     "Fetch `room-id`'s leaderboard into `sig/leaderboards`, recording the
      replica's experiment fingerprint `attempts` it was asked at
      (`attempt-board/experiment-fingerprint`). Coalesced per room
      by the aggregate gate."
     [room-id attempts]
     (swap! requested assoc room-id attempts)
     (agg/run!
      (gate-for room-id)
      (fn [done]
        (go
          (let [ch (promise-chan)]
            (binding [rtc/*execution-context* runtime]
              (let [s (er/load-leaderboard! web/server-id room-id)]
                (s (fn [r] (put! ch {:ok r})) (fn [e] (put! ch {:err e})))))
            (let [{:keys [ok err]} (<! ch)]
              (binding [rtc/*execution-context* runtime]
                (swap! sig/leaderboards assoc room-id
                       (if err
                         {:error (or (rem/error-text err) (ex-message err) (str err))
                          :attempts attempts}
                         {:data ok :attempts attempts})))
              (done))))))))

(defn- percent [x]
  (if x (str (Math/round (* 100 (double x))) "%") "—"))

(defn- reward-label [x]
  (if x
    #?(:clj (format "%.2f" (double x)) :cljs (.toFixed x 2))
    "—"))

(defn- interval-label
  "A 95% interval of a rate as \"76–100%\"; \"—\" when there is none."
  [[lo hi]]
  (if lo (str (Math/round (* 100 (double lo))) "–" (Math/round (* 100 (double hi))) "%") "—"))

(defn- short-id [id]
  (let [s (str id)] (subs s 0 (min 8 (count s)))))

(defn- frontier-table
  "A completed experiment's candidates, in dvergr's leaderboard order."
  [candidates]
  (el/table {:class "attempt-board-table"}
    (el/thead {}
      (el/tr {}
        (el/th {} "Candidate")
        (el/th {:class "num"} "Attempts")
        (el/th {:class "num"} "Passed")
        (el/th {:class "num" :title "95% interval of the pass rate (Jeffreys)"} "95% interval")
        (el/th {:class "num"} "Mean reward")
        (el/th {:class "num" :title "Per attempt, at the models' list prices (a subscription run's too)"} "Cost / attempt")
        (el/th {:class "num" :title "Per passed attempt, at list price"} "Cost / pass")
        (el/th {:class "num" :title "Input and output tokens per attempt"} "Tokens / attempt")
        (el/th {:class "num"} "Median time")))
    (el/tbody {}
      (map (fn [c]
             (el/tr {:key (str (:candidate c))}
               (el/td {:class "attempt-board-name" :title (str (:candidate c))} (lb/candidate-label (:candidate c)))
               (el/td {:class "num"} (str (:attempts c)))
               (el/td {:class "num"} (str (:passed c) " · " (percent (:pass-rate c))))
               (el/td {:class "num attempt-board-range"} (interval-label (:pass-rate-interval c)))
               (el/td {:class "num"} (reward-label (:reward-mean c)))
               (el/td {:class "num"} (board/dollars (:notional-microdollars-per-attempt c)))
               (el/td {:class "num"} (board/dollars (:notional-microdollars-per-pass c)))
               (el/td {:class "num"} (lb/tokens-label (:tokens-per-attempt c)))
               (el/td {:class "num"} (lb/seconds-label (:median-seconds c)))))
           candidates))))

(defn- progress-table
  "An unfinished experiment's candidates: the cells certified so far."
  [candidates]
  (el/table {:class "attempt-board-table"}
    (el/thead {}
      (el/tr {}
        (el/th {} "Candidate")
        (el/th {:class "num"} "Cells")
        (el/th {:class "num" :title "Completed, or failed by the model itself"} "Verdicts")
        (el/th {:class "num" :title "Failures of the path to the model; re-run on resume"} "Faults")
        (el/th {:class "num"} "Mean reward")
        (el/th {:class "num" :title "What was paid"} "Spend")))
    (el/tbody {}
      (map (fn [c]
             (el/tr {:key (str (:candidate c))}
               (el/td {:class "attempt-board-name" :title (str (:candidate c))} (lb/candidate-label (:candidate c)))
               (el/td {:class "num"} (str (:attempts c)))
               (el/td {:class "num"} (str (:verdicts c)))
               (el/td {:class (str "num" (when (pos? (or (:faults c) 0)) " attempt-board-failed"))}
                 (str (:faults c)))
               (el/td {:class "num"} (reward-label (:reward-mean c)))
               (el/td {:class "num"} (board/dollars (:microdollars c)))))
           candidates))))

(defn- paired-table
  "The paired comparisons: on the cases both candidates have a verdict for,
   how often they agree and disagree, the exact McNemar p, Holm-adjusted, and
   the verdict."
  [paired]
  (let [reps? (lb/repetitions? paired)
        holm? (< 1 (or (:family-size paired) (count (:comparisons paired))))
        excluded (lb/exclusions paired)]
    (el/div {:class "attempt-board-comparison leaderboard-paired"}
      (el/h4 {} "Paired comparisons")
      (el/p {:class "attempt-board-range"}
        (str "Every candidate answers the same cases, so two of them compare on the cases where they disagree: "
             "A only counts the cases A passed and B failed, B only the reverse. "
             (when reps? "With repetitions a case counts once, won by the higher pass rate over its repetitions; Tied are cases both passed equally often. ")
             (lb/policy-note paired)))
      (el/table {:class "attempt-board-table"}
        (el/thead {}
          (el/tr {}
            (el/th {} "A")
            (el/th {} "B")
            (el/th {:class "num"} "Cases")
            (el/th {:class "num"} "Both passed")
            (el/th {:class "num"} "A only")
            (el/th {:class "num"} "B only")
            (el/th {:class "num"} "Both failed")
            (when reps? (el/th {:class "num"} "Tied"))
            (el/th {:class "num" :title "Exact McNemar test of A only against B only"} "p")
            (when holm? (el/th {:class "num" :title "Holm-adjusted over the family of comparisons"} "p (Holm)"))
            (el/th {} "Verdict")))
        (el/tbody {}
          (map (fn [c]
                 (el/tr {:key (str (:a c) "|" (:b c))}
                   (el/td {:class "attempt-board-name" :title (str (:a c))} (lb/candidate-label (:a c)))
                   (el/td {:class "attempt-board-name" :title (str (:b c))} (lb/candidate-label (:b c)))
                   (el/td {:class "num"} (str (:cases c)))
                   (el/td {:class "num"} (str (:both-passed c)))
                   (el/td {:class "num"} (str (:a-only c)))
                   (el/td {:class "num"} (str (:b-only c)))
                   (el/td {:class "num"} (str (:both-failed c)))
                   (when reps? (el/td {:class "num"} (str (:tied-mixed c))))
                   (el/td {:class "num"} (lb/p-label (:p c)))
                   (when holm? (el/td {:class "num"} (lb/p-label (:p-adjusted c))))
                   (el/td {:class (str "leaderboard-verdict leaderboard-verdict--" (name (lb/verdict-kind c)))}
                     (lb/verdict-label c))))
               (:comparisons paired))))
      (when (seq excluded)
        (el/p {:class "attempt-board-range"}
          (str "Left out, where only one candidate (or neither) has a verdict (a fault, or not run yet): "
               (str/join "; " (for [{:keys [a b cases cells]} excluded]
                                (str (lb/candidate-label a) " / " (lb/candidate-label b) ": "
                                     (str/join ", " (cond-> []
                                                      (pos? cases) (conj (str cases " case" (when (not= 1 cases) "s")))
                                                      (pos? cells) (conj (str cells " cell" (when (not= 1 cells) "s")
                                                                              " without a counterpart")))))))
               ".")))
      (el/p {:class "attempt-board-range"}
        "\"No detectable difference\" is not \"equal\": more cases, or cases where the candidates differ, can still separate them."))))

(defn- experiment-section
  [{:keys [experiment experiment-id workflow evaluation complete? candidates paired cells environments repetitions verifier-trust] :as result}]
  (el/section {:key (str experiment) :class "attempt-board-experiment" :data-experiment experiment}
    (el/div {:class "attempt-board-experiment-title"}
      (el/span {:class "leaderboard-experiment-name" :title experiment-id} (lb/experiment-label result))
      (when workflow
        (el/span {:class "run-history-id" :title (str "Frozen workflow " (:name workflow) "; bundle " (:bundle workflow))}
          (str (:name workflow) " · bundle " (short-id (:bundle workflow)))))
      (when evaluation
        (el/span {:class "run-history-id"
                  :title (str "Frozen dataset " (:id evaluation) "; cases: "
                              (str/join ", " (:cases evaluation))
                              (when (= "held-out" (:split evaluation))
                                "; acceptance evaluation, not prompt tuning"))}
          (str (:split evaluation) " · dataset " (short-id (:id evaluation)))))
      (el/span {:class "run-history-id" :title (str "ExperimentDef " experiment)} (short-id experiment))
      (el/span {} (str cells " cells"
                       (when environments (str " · " environments " cases"))
                       (when (and repetitions (< 1 repetitions)) (str " × " repetitions))))
      (when (seq verifier-trust)
        (el/span {:title "Who vouches for the rewards"} (str "verifier: " (str/join ", " verifier-trust))))
      (el/span {:class (if complete? "attempt-board-scored" "run-history-summary-active")}
        (if complete? "Scorecard certified" "in progress")))
    (if complete?
      (frontier-table candidates)
      (progress-table candidates))
    (if (seq (:comparisons paired))
      (paired-table paired)
      (el/p {:class "attempt-board-range"} "Paired comparisons need two candidates with verdicts on the same cases."))))

(defn view
  "`leaderboard` is `{:data :error}` as loaded (`is.simm.ops.experiments`)."
  [{:keys [room-name leaderboard on-back-room on-refresh]}]
  (let [{:keys [data error]} leaderboard
        {:keys [experiments running]} data]
    (el/div {:class "run-history attempt-board leaderboard"}
      (el/header {:class "run-history-header"}
        (el/button {:class "run-history-back"
                    :title "Back to room"
                    :on-click (fn [_] (when on-back-room (on-back-room)))}
          (vc/icon "arrow-left"))
        (el/div {:class "run-history-heading"}
          (el/div {:class "run-history-kicker"}
            (vc/icon "medal")
            (or room-name "Room"))
          (el/h2 {} "Leaderboard")
          (el/p {} "Each experiment's candidates: how often they passed with the 95% interval, cost at list price, tokens and time, and every pair compared on the same cases."))
        (el/button {:class "run-history-back"
                    :title "Load again"
                    :on-click (fn [_] (when on-refresh (on-refresh)))}
          (vc/icon "refresh-cw")))
      (el/main {:class "run-history-body"}
        (when (pos? (or running 0))
          (el/div {:class "run-history-summary"}
            (el/span {:class "run-history-summary-active"} (str running " running"))))
        (cond
          error (el/div {:class "proposal-error"} (str "Couldn't load the leaderboard — " error))
          (nil? data) (el/div {:class "run-history-empty"} (el/p {} "Loading…"))
          (empty? experiments)
          (el/div {:class "run-history-empty"}
            (vc/icon "medal")
            (el/h3 {} "No experiments yet")
            (el/p {} "Benchmark candidates on a workflow (catalog_benchmark, room-run) and its leaderboard appears here."))
          :else
          (el/div {:class "attempt-board-sections"}
            (map experiment-section experiments)))))))

(defn leaderboard-tab
  "The Leaderboard tab of a room: loads its leaderboard on first sight and
   again whenever the replica's experiment fingerprint `attempts` (Attempts,
   Scorecards, running Runs) moves past the one it was loaded at, so a
   running experiment's board follows its cells and its completion."
  [{:keys [room-id attempts] :as props}]
  #?(:cljs
     (spin
       (let [boards (iv/get-new (track sig/leaderboards))
             entry (get boards room-id)
             _ (when (or (not (contains? @requested room-id))
                         (and attempts (not= attempts (get @requested room-id))))
                 (load-leaderboard! room-id attempts))]
         (view (assoc props
                      :leaderboard entry
                      :on-refresh #(load-leaderboard! room-id attempts)))))
     :clj (view (assoc props :leaderboard nil))))
