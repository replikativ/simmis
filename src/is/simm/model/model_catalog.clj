(ns is.simm.model.model-catalog
  "The model list every picker shows, built server-side.

   Two halves, and the split is the point. The FAMILIES are curated: a family
   string makes a poor name, and which families to put in front of someone is a
   judgement call. The VERSIONS under each family are DERIVED from last-known
   provider and registry facts. Availability is explicit on every row; a family
   never disappears merely because its credential or account access does.

   Hand-writing both halves is what made the list incoherent: GLM had a preferred
   5.2 row because someone typed one, while the GPT families had none because
   nobody did. Now every family gets the same treatment by construction, and a
   version that ships stops being invisible until a human notices."
  (:require [clojure.string :as str]
            [is.simm.model.model-selection :as ms]
            [dvergr.model.registry :as registry]))

(def curated
  "Ordered. A `:family` entry expands into a Latest row plus one row per
   version on offer; a `:model` entry is a single row for something that has no
   version to follow."
  [{:family "accounts/fireworks/models/glm-*" :label "GLM" :provider "fireworks"}
   {:family "accounts/fireworks/models/kimi-*" :label "Kimi" :provider "fireworks"}
   {:family "accounts/fireworks/models/minimax-*" :label "MiniMax" :provider "fireworks"}
   {:family "accounts/fireworks/models/deepseek-*-pro" :label "DeepSeek Pro" :provider "fireworks"}
   {:model "accounts/fireworks/models/qwen3p6-plus" :label "Qwen3.6 Plus" :provider "fireworks"}

   {:family "gpt-*-sol" :label "GPT Sol" :provider "openai"}
   {:family "gpt-*-terra" :label "GPT Terra" :provider "openai"}
   {:family "gpt-*-luna" :label "GPT Luna" :provider "openai"}
   {:family "gpt-*" :label "GPT" :provider "openai"}
   {:family "gpt-*-mini" :label "GPT mini" :provider "openai"}

   {:model "claude-sonnet-4-6" :label "Claude Sonnet 4.6" :provider "anthropic"}
   {:model "claude-opus-4-7" :label "Claude Opus 4.7" :provider "anthropic"}])

(def inherit-choice-value
  "Sentinel sent by the first-class agent picker row that clears an override."
  "__inherit-owner-preference__")

(defn version-label
  "A version token as a person writes it. `5p2` is Fireworks' spelling of 5.2,
   and `k2p6` of K2.6."
  [v]
  (let [s (-> (str v) (str/replace #"(?<=\d)p(?=\d)" "."))]
    (if (re-find #"^[a-z]" s) (str (str/upper-case (subs s 0 1)) (subs s 1)) s)))

(def provider-labels
  "How a provider is written for a person. One map, used by the picker rows and
   by `room-agents/describe-model-resolution`, so desired-resolution surfaces
   use one vocabulary. This does not introspect a joined participant."
  {:openai "OpenAI"
   :fireworks "Fireworks"
   :anthropic "Anthropic"
   :claude-code "Claude Code"})

(defn short-id
  "A model id as a person reads it.

   Fireworks addresses a model by path, `accounts/fireworks/models/glm-5p2`,
   while OpenAI uses a bare `gpt-5.6-luna`. Printed raw, one resolved-model row
   showed a path and the next showed a name, for the same kind of fact. The
   prefix carries no information the Provider row does not already give."
  [id]
  (some-> id str (str/split #"/") last))

(defn provider-label [provider]
  (get provider-labels (keyword provider) (some-> provider name)))

(defn family-label
  "Curated person-facing name for `family`, or the family string as a fallback."
  [family]
  (or (some #(when (= family (:family %)) (:label %)) curated)
      family))

(defn model-label
  "Person-facing explicit-version/model label for a concrete id.

   Derive version labels from the curated family vocabulary even when a
   withdrawn preference has disappeared from the current shortlist, then fall
   back to registry metadata and the provider id.

   nil in, nil out: a selection that resolves to nothing has no name. Without
   the guard, `(:model curated-entry)` was nil for every family entry, so a nil
   id matched the first one and an unresolvable OpenAI family was labelled
   \"GLM\"."
  [id]
  (when (seq (str id))
    (let [family (ms/family-of id)
          version (ms/version-of id)
          exact-entry (some #(when (= id (:model %)) %) curated)
          family-entry (some #(when (= family (:family %)) %) curated)]
      (or (:label exact-entry)
          (when (and family-entry version)
            (str (:label family-entry) " " (version-label version)))
          (:name (registry/get-model id))
          id))))

(defn preferred-status-copy
  "Status sentence for an unusable preferred version.

   The shorter family noun makes the UI read 'using a newer Luna' while the
   Model and Resolves-to rows keep the full 'GPT Luna 5.x' labels."
  [{:keys [preferred? preferred-family preferred-version
           preferred-availability fallback? available?]}]
  (when (and preferred? (not (:available? preferred-availability)))
    (let [family-name (family-label preferred-family)
          short-family (if (str/starts-with? family-name "GPT ")
                         (subs family-name 4)
                         family-name)
          version-name (version-label preferred-version)]
      (cond
        fallback? (str version-name " unavailable; using a newer " short-family)
        (not available?) (str version-name " unavailable; no newer "
                              short-family " is usable")))))

(def reasoning-off-copy
  "ONE phrase for the reasoning caveat. The configuration panel puts it after a
   `Reasoning` label, the picker row prefixes it with the word, and both read as
   the same sentence. Two phrasings for one fact is how a reader ends up
   believing there are two facts."
  "off while tools are attached")

(def reasoning-on-copy "on")

(def reasoning-off-explanation
  "The tooltip behind that phrase. Names the tools, because \"tools\" on its own
   means nothing to someone who has not read the turn code."
  (str "Every turn carries this room's tools: clojure_eval, shell, file reading "
       "and editing, search, tests. This model refuses tool calls unless its "
       "reasoning is switched off, so simmis switches it off and keeps the tools."))

(defn- provenance
  "Last-known endpoint facts for a provider/model pair. Picker rows retain
   these instead of flattening availability back to an unscoped id."
  [provider id]
  (merge
   {:credential-source (:credential-source (ms/provider-contract provider))}
   (some #(when (and (= (keyword provider) (:provider %))
                     (= id (:model-id %)))
            (select-keys % [:base-url :credential-source :reachability
                            :reachable? :model-id :endpoint-kind
                            :native-openai? :catalog-contract]))
         (ms/catalog))))

(defn reasoning-disabled-for-tools?
  "Whether the configured provider will apply this model's tools workaround.

   The registry identifies affected OpenAI models. dvergr deliberately applies
   the workaround only to native OpenAI requests; a custom OPENAI_BASE_URL is
   merely protocol-compatible and must not receive an OpenAI-native field."
  [provider id]
  (let [provider (keyword provider)
        endpoint (some #(when (= provider (:provider %)) %) (ms/provider-endpoints))]
    (boolean
     (and (registry/get-quirk id :chat-tools-need-effort-none?)
          (or (not= :openai provider)
              (:native-openai? endpoint))))))

(defn next-join-copy
  "What a configuration surface says about the NEXT join.

   Room settings composed this sentence itself and always promised a
   resolution, so an agent whose model needs a credential still read
   \"Resolves to glm-5p2 when next joined\" — and that join then refused with
   `:model-unavailable`. A surface must not promise a resolution the join
   boundary will reject. Copy lives here, beside every other row's copy."
  [{:keys [available? model-short availability-label]}]
  (if available?
    (str "Resolves to " (or model-short "—") " when next joined")
    (str (or availability-label "Unavailable")
         " — will not join until this resolves")))

(defn reasoning-copy
  "The words a screen shows for this model's reasoning, plus the tooltip."
  [no-reasoning?]
  {:reasoning-copy (if no-reasoning? reasoning-off-copy reasoning-on-copy)
   :reasoning-explanation (when no-reasoning? reasoning-off-explanation)})

(defn- ensure-model-metadata!
  "Load dvergr's resource-backed model definitions before deciding availability.

   dvergr's built-in registry map carries the Anthropic and OpenAI entries, but
   the Fireworks ones live in `resources/models.edn` and are read only by
   `load-models-resource!` — which simmis reaches through provider
   initialization, on the first agent turn. Availability is registry-gated, so
   a picker built before that turn saw no Fireworks model at all and reported
   every Fireworks family, the product default included, as \"Not yet
   supported\". Opening Settings on a fresh boot was enough.

   `ensure-models-loaded!` is dvergr's own operation for exactly this call
   site — it reads the resource once and then answers from the loaded registry.
   The raw `load-models-resource!` is a MERGE, so calling it per render put the
   resource's value back over anything registered since: a runtime or custom
   entry lost its metadata the next time a picker drew or a preference was
   validated. Rendering a list must not rewrite the registry it reads.

   A failed read leaves dvergr's flag clear, so the next picker retries. No
   reporting is lost here: simmis never caught or logged this call, and a model
   still missing from the registry renders as its own explicit availability
   state. Deliberately not `providers/ensure-initialized!` — whether a provider
   record exists is a different question from whether the metadata is loaded,
   and that check short-circuits once any provider (a local Claude CLI, say)
   has registered."
  []
  (registry/ensure-models-loaded!))

(def ^:private max-versions
  "How many preferred versions to offer under a family.

   A picker is a shortlist, not an archive: two keeps Latest plus the nearest
   explicit versions. Server validation accepts only rows in this shortlist."
  2)

(defn availability-copy
  "Human copy for one authoritative availability result."
  [provider {:keys [state credential-source]}]
  (case state
    :available
    {:availability-label "Available"
     :availability-explanation "Available to this account and supported by simmis."}

    :needs-credential
    {:availability-label "Credential required"
     :availability-explanation
     (str "Set " credential-source " in the server environment, then restart simmis.")}

    :not-implemented
    ;; NOT "not yet". This state is reached from both directions: a version the
    ;; provider has just released and dvergr has no entry for, and an older
    ;; variant dvergr never carried. "Not yet" reads as a promise about the
    ;; first and is simply wrong about the second.
    {:availability-label "Not supported"
     :availability-explanation
     "simmis has no metadata or adapter for this model, so it cannot run here."}

    :unavailable-to-account
    {:availability-label "Unavailable to account"
     :availability-explanation
     (str (provider-label provider) " does not make this model available to this account.")}

    :credential-rejected
    ;; NOT an outage. The provider answered, and refused the key. Waiting fixes
    ;; nothing; replacing the key does. The two used to share one sentence,
    ;; which told an operator with a revoked key to wait for a refresh.
    {:availability-label "Credential rejected"
     :availability-explanation
     (str (provider-label provider) " rejected " (or credential-source "the API key")
          ". Set a valid key in the server environment, then restart simmis.")}

    :temporarily-unreachable
    {:availability-label "Temporarily unreachable"
     :availability-explanation
     (str (provider-label provider)
          " model availability could not be refreshed. Last-known status is retained, "
          "but this choice cannot be used until the provider responds.")}

    ;; Total by construction. The six states above are everything
    ;; `model-selection/availability-state` produces, but this function also
    ;; renders results that were computed elsewhere. A `case` without this
    ;; clause threw out of the whole room-details response rather than showing
    ;; a row as unusable.
    {:availability-label "Unavailable"
     :availability-explanation
     (str (provider-label provider)
          " could not confirm this model for this account.")}))

(defn- with-availability
  [row]
  (let [availability (ms/model-availability (:provider row) (:resolves row))]
    (merge row
           {:availability (:state availability)
            :availability-reason (:reason availability)
            :available? (:available? availability)
            :disabled? (not (:available? availability))}
           (select-keys availability [:credential-source :last-success-at])
           (availability-copy (:provider row) availability)
           (reasoning-copy (:no-reasoning? row))
           (provenance (:provider row) (:resolves row)))))

(defn- family-rows
  "One always-visible Latest row, then last-known/registered version rows."
  [{:keys [family label provider]}]
  (let [versions (take max-versions (ms/known-versions-in provider family))
        latest-id (:candidate (ms/resolve-selection {:family family
                                                     :version :auto
                                                     :provider provider}))]
    (into [(with-availability
            {:kind :family
             :value family
             :label (str label " (Latest)")
             :provider provider
             :provider-label (provider-label provider)
             :resolves latest-id
             :no-reasoning? (reasoning-disabled-for-tools? provider latest-id)})]
          (map (fn [v]
                 (let [id (ms/id-for family v)]
                   (with-availability
                    {:kind :version
                     :value id
                     :label (str label " " (version-label v))
                     :provider provider
                     :provider-label (provider-label provider)
                     :resolves id
                     :no-reasoning? (reasoning-disabled-for-tools? provider id)})))
               versions))))

(defn choices
  "Every row a model picker should show, in order. Each row carries its own
   copy, so the client never composes a sentence of its own."
  []
  (ensure-model-metadata!)
  (mapv identity
        (mapcat (fn [{:keys [family model label provider] :as entry}]
                  (if family
                    (family-rows entry)
                    [(with-availability
                      {:kind :model
                       :value model
                       :label label
                       :provider provider
                       :provider-label (provider-label provider)
                       :resolves model
                       :no-reasoning? (reasoning-disabled-for-tools? provider model)})]))
                curated)))

(defn choice
  "Picker row for `value`, or nil when the value is outside the curated list."
  [value]
  (some #(when (= value (:value %)) %) (choices)))

(defn require-available-choice!
  "Return the curated available row, or reject an unavailable/unknown choice."
  [value]
  (let [row (choice value)]
    (if (:available? row)
      row
      (throw (ex-info "Model choice is unavailable"
                      {:type :model-choice-unavailable
                       :model-choice value
                       :availability (or (:availability row) :not-implemented)
                       ;; `:not-curated` describes a value the list does not
                       ;; offer. A curated row that merely needs a credential
                       ;; carries no sub-reason, and an `or` turned that nil
                       ;; into the claim that GLM 5.2 is not curated.
                       :availability-reason (if row
                                              (:availability-reason row)
                                              :not-curated)
                       :credential-source (:credential-source row)})))))

(defn require-usable-preference!
  "Resolve an already-stored preference and require an executable result.

   Unlike `require-available-choice!`, this accepts a preferred version whose
   own row was withdrawn when a newer same-family/provider fallback is usable.
   It is used when an agent returns to owner inheritance: the preference was
   previously persisted, and validation must follow the same soft-resolution
   policy used by participant join without rewriting that preference."
  [value]
  (ensure-model-metadata!)
  (let [resolved (if (ms/family? value)
                   (ms/resolve-selection {:family value :version :auto})
                   (ms/resolve-selection {:model value}))
        availability (:availability resolved)]
    (if (:available? resolved)
      resolved
      (throw (ex-info "Model preference is unavailable"
                      {:type :model-choice-unavailable
                       :model-choice value
                       :availability (or (:state availability) :not-implemented)
                       :availability-reason (or (:reason availability)
                                                :no-same-family-forward-candidate)
                       :preferred-model (:preferred-model resolved)
                       :provider (:provider resolved)
                       :credential-source (:credential-source availability)})))))

(defn selected?
  "Is `row` the configured choice? `model-resolution` comes from
   `room-agents/describe-model-resolution` and is not active-state data."
  [row {:keys [family auto? model candidate preferred-model]}]
  (if (and family auto?)
    (= (:value row) family)
    (= (:value row) (or preferred-model candidate model))))

(defn choice-label
  "The picker's own label for what this config selected, so the configuration
   panel names a model exactly the way the list that set it does. nil when the
   config points at something the picker does not offer."
  [model-resolution]
  (->> (choices)
       (filter #(selected? % model-resolution))
       first
       :label))
