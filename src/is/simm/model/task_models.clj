(ns is.simm.model.task-models
  "Which model simmis's own one-shot calls use: screen-frame descriptions
   (`:vision`) and the summarizer/merger (`:cheap`).

   Both calls go through dvergr (`dvergr.media.vision/describe`,
   `dvergr.tools.llm-call/cheap-llm-call`), which has its own fixed defaults.
   A deployment overrides them with

     :models {:vision \"accounts/fireworks/models/kimi-*\"
              :cheap  \"accounts/fireworks/models/minimax-*\"}

   in config.local.edn, beside `:default` (the agents' product default,
   `model-selection/default-model-choice`) — an exact id or a FAMILY, resolved
   per call through `model-selection` like an agent's model. Unset means
   dvergr's default.

   Configured and unresolvable is NOT unset. The call is refused with the
   reason (`model-opts` throws), and nothing substitutes dvergr's default or
   another family — a frame whose describe was refused is stored as failed
   with that reason (`screen-intake`). One boot warning names the problem
   (`warn-unusable!`).

   WORKAROUND, temporary: dvergr's fixed defaults name ids Fireworks
   withdrew (kimi-k2p6 for vision, minimax-m2p7 for the cheap call). dvergr
   branch fix/retired-fireworks-models replaces them with served defaults.
   Once simmis is on the dvergr release containing that branch, the defaults
   work unconfigured and this namespace only carries the override."
  (:require [clojure.string :as str]
            [dvergr.model.registry :as registry]
            [dvergr.substrate.config :as dcfg]
            [is.simm.model.model-selection :as selection]
            [taoensso.telemere :as log]))

(def tasks
  "The calls a deployment may point at a model, and the registry capability a
   candidate must carry for each (nil = any)."
  {:vision {:capability :vision
            :label "screen-frame descriptions (vision)"}
   :cheap  {:capability nil
            :label "the summarizer and merger (cheap call)"}})

(defn configured
  "The configured id or family for `task`, or nil (read by
   `model-selection/configured-model`, the one reader of `:models`)."
  ([task] (configured task (dcfg/config)))
  ([task config] (selection/configured-model task config)))

(defn- capable? [capability model-id]
  (or (nil? capability)
      (contains? (set (:capabilities (registry/get-model model-id))) capability)))

(defn resolve-task
  "Resolution of `task`'s configured model:
   nil                                   — not configured (dvergr's default)
   {:configured v :model id}             — usable now
   {:configured v :model nil :reason s}  — configured but not usable now

   A family resolves to its newest version that is available AND carries the
   task's capability — kimi-k3 is served but registered text-only, so
   `kimi-*` for vision means the newest IMAGE-CAPABLE kimi, not kimi-k3."
  ([task] (resolve-task task (configured task)))
  ([task value]
   (when value
     (let [{:keys [capability]} (get tasks task)
           model (if (selection/family? value)
                   (let [provider (selection/infer-provider value)]
                     (->> (selection/known-versions-in provider value)
                          (map #(selection/id-for value %))
                          (filter #(capable? capability %))
                          (filter #(selection/available-model? provider %))
                          first))
                   (when (and (capable? capability value)
                              (selection/available-model? value))
                     value))]
       (if model
         {:configured value :model model}
         {:configured value :model nil
          :reason (if (selection/family? value)
                    (str "no available " (when capability (str (name capability) "-capable "))
                         "model in " value)
                    (let [a (selection/model-availability value)]
                      (if (and (:available? a) (not (capable? capability value)))
                        (str value " is not registered as " (name capability) "-capable")
                        (str value " is not available: " (name (or (:state a) :unknown))))))})))))

(defn model-opts
  "Opts to merge into the dvergr call for `task`: {} when unconfigured,
   {:model id} when the configured model resolves. Throws `:task-model-unavailable`
   with the reason when it is configured and does not — never a silent
   fallback to dvergr's default."
  [task]
  (if-let [{:keys [model reason configured]} (resolve-task task)]
    (if model
      {:model model}
      (throw (ex-info (str "The configured " (name task) " model is unusable: " reason)
                      {:type :task-model-unavailable :task task
                       :configured configured :reason reason})))
    {}))

(defn warn-unusable!
  "Boot check: one warning per configured task model that resolves to nothing.
   Returns {task reason} for what it warned about."
  []
  (into {}
        (keep (fn [[task {:keys [label]}]]
                (let [r (try (resolve-task task)
                             (catch clojure.lang.ExceptionInfo e
                               {:configured (:configured (ex-data e)) :reason (ex-message e)}))]
                  (when (and r (nil? (:model r)))
                    (log/log! {:level :warn :id ::task-model-unusable
                               :data {:task task :configured (:configured r) :reason (:reason r)}
                               :msg (str "`:models {" task " " (pr-str (:configured r)) "}` resolves to"
                                         " no usable model (" (:reason r) "). " label
                                         " will fail with this reason until it does;"
                                         " nothing falls back to another model.")})
                    [task (:reason r)]))))
        tasks))
