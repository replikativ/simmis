(ns is.simm.model.candidate-models
  "The models a workflow run or benchmark may name, as registry metadata.

   One listing for every surface that lets someone CHOOSE a model for work —
   the Workflows view, MCP clients and room agents — so they cannot disagree
   about what is runnable. Depends only on the registry and the model
   catalog/selection namespaces, so agent vocabulary can use it without
   reaching into the work runtime.

   A reasoning effort is part of the model choice, not a separate knob:
   `<id>@<effort>` (e.g. `codex-subscription-luna-6@low`) is a candidate
   identity of its own in Dvergr's registry. Only the base id reaches the
   provider; attribution, Scorecards and schedules keep the full id, so two
   efforts of one model are two comparable benchmark candidates."
  (:require [clojure.string :as str]
            [dvergr.model.registry :as registry]
            [is.simm.model.model-catalog :as catalog]
            [is.simm.model.model-selection :as selection]))

(def dispatch-accounted-providers
  "Native transports whose individual model HTTP attempts use Dvergr's
   admission gateway. Subscription dollars are zero incremental spend, not
   zero usage; dispatch, model-step and wall limits still apply. No CLI."
  #{:openai :anthropic :fireworks :codex-subscription})

(def effort-syntax
  "How a caller selects a reasoning effort, stated once for every surface."
  (str "Select a reasoning effort by appending it to the model id: <id>@<effort>, "
       "e.g. codex-subscription-luna-6@low. Accepted wherever a workflow run, benchmark "
       "or schedule takes a model id. Only the listed :reasoning-efforts of that model "
       "are accepted; any other is refused, never mapped to a nearby level. A bare id "
       "uses the provider's default effort."))

(defn options
  "Locally registered, dispatch-accounted models with their reasoning efforts
   and credential presence. Metadata only: no catalog refresh, provider
   initialization, token refresh, model request or implicit selection."
  []
  (let [models (->> (registry/list-models)
                    (filter (fn [{:keys [id provider api-type]}]
                              (and (string? id) (<= 1 (count id) 200)
                                   (not (str/blank? id)) (not (re-find #"[\p{Cntrl}]" id))
                                   (contains? dispatch-accounted-providers provider)
                                   (contains? (:implemented-api-types (selection/provider-contract provider)) api-type))))
                    (sort-by :id) vec)
        present (into {} (map (fn [provider] [provider (selection/credential-present? provider)]))
                      (set (map :provider models)))]
    {:models
     (mapv (fn [{:keys [id provider api-type reasoning-efforts]}]
             (let [label (str (catalog/model-label id))]
               {:id id :provider provider :api-type api-type
                :label (subs label 0 (min 200 (count label)))
                :reasoning-efforts (vec reasoning-efforts)
                :provider-label (catalog/provider-label provider)
                :credential-status (if (get present provider) :present :undetected)
                :credential-label (if (get present provider)
                                    (if (= :codex-subscription provider) "Login present; connection not tested"
                                        "Credential configured; account access not checked")
                                    "Default credential not detected; connection not verified")}))
           models)
     :models-truncated? (> (count models) 256)
     :effort-syntax effort-syntax}))
