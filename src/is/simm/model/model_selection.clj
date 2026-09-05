(ns is.simm.model.model-selection
  "Pick a model by FAMILY and VERSION, and resolve to a concrete provider id at
   TURN time.

   The bug this exists to kill: the model id used to be frozen into an agent's
   :actor/config when the agent was created. Bumping the code default then did
   nothing for agents that already existed — Vár ran glm-5p1 for eleven days
   after we'd 'switched' to 5p2, and every provider quirk we chased in that time
   came from a model we believed we had stopped using. Configuration captured at
   creation cannot be corrected by changing code; it can only be migrated, and
   nobody remembers to write the migration.

   So: an agent stores a model only when a HUMAN explicitly overrides its
   owner's preference — a family with a preferred version or :auto, or an exact
   id. With no override it stores no model keys and follows owner preference,
   then the product fallback. The concrete id is computed when the participant
   is joined or rejoined. A new release is picked up by restarting, not by a
   migration.

   FAMILY is the id with its version slot blanked, which makes it provider-shaped
   rather than a name we have to curate:

     accounts/fireworks/models/glm-5p2      → accounts/fireworks/models/glm-*
     accounts/fireworks/models/kimi-k2p6    → accounts/fireworks/models/kimi-*
     accounts/fireworks/models/deepseek-v4-pro → accounts/fireworks/models/deepseek-*-pro
     gpt-5.6-luna                           → gpt-*-luna

   VERSION is the token that filled the slot (\"5p2\", \"k2p6\", \"v4\", \"5.6\"),
   ordered by its digit groups — 5p2 > 5p1, k2p6 > k2p5, 5.6 > 5.5. :auto means
   \"newest that is served, registered, and implemented\", combining provider
   evidence with the static registry rather than maintaining another version
   list.

   The CATALOG asks every endpoint this machine has a key for which model ids
   it serves, and retains which provider, credential and base URL produced each
   one. Each endpoint is read under its OWN contract — see `catalog-contracts`,
   which records what the vendor documents and what is merely observed of an
   OpenAI-compatible base. Availability is endpoint-local: one provider going
   dark cannot erase another provider's answer or turn stale ids into claims
   that they are currently reachable."
  (:require [clojure.string :as str]
            [dvergr.model.registry :as registry]
            [taoensso.telemere :as log]))

(def default-model
  "The explicit product default used only when nobody selected a model.

   It is still subject to the same availability check as every saved choice;
   an unavailable configured model never falls through to this id. Single
   source of truth — `parties/default-model` re-exports it."
  "accounts/fireworks/models/glm-5p2")

(def provider-contracts
  "The provider facts simmis can know without asking a provider.

   `:catalog-required?` means a model list from this provider's own endpoint
   participates in availability. WHICH list, and on whose authority, is per
   endpoint rather than per protocol — see `catalog-contracts`.

   Anthropic used to sit here with `:catalog-required? false`, so a present
   `ANTHROPIC_API_KEY` plus a curated row was reported as `:available` —
   `available?` true, saveable, promised to join. Neither fact is evidence
   about the account: a key from an organization without Opus access, or one
   revoked an hour ago, produced exactly the same green row, and the first
   thing that ever contradicted it was a turn failing inside a room. Anthropic
   documents a native Models API that answers precisely the missing question,
   so it is read like every other catalog provider.

   Keeping the exact environment variable beside the adapter contract is what
   lets every unavailable surface give the same actionable explanation."
  {:openai {:credential-source "OPENAI_API_KEY"
            :catalog-required? true
            :implemented-api-types #{:openai-chat}}
   :fireworks {:credential-source "FIREWORKS_API_KEY"
               :catalog-required? true
               :implemented-api-types #{:openai-chat}}
   :anthropic {:credential-source "ANTHROPIC_API_KEY"
               :catalog-required? true
               :implemented-api-types #{:anthropic-messages}}})

(defn availability-state
  "Pure, authoritative state matrix for one provider/model candidate.

   The order is intentional: a missing credential is the first actionable
   problem; a registry/adapter gap is certain even during an endpoint outage;
   only a fully implemented candidate can be temporarily unreachable or absent
   from the account. `served?` is last-known provider evidence, never a reason
   to register a model dynamically.

   A REJECTED credential is separated from an outage. Both leave the account's
   model list unknown, but one is a permanent configuration fault the operator
   can fix and the other is a wait. Collapsing them told an operator whose key
   had been revoked to sit and wait for a refresh that could never succeed."
  [{:keys [provider-known? credential-present? registered? implemented?
           catalog-required? catalog-reachability served?]}]
  (cond
    (not provider-known?) :not-implemented
    (not credential-present?) :needs-credential
    (not registered?) :not-implemented
    (not implemented?) :not-implemented
    (and catalog-required? (= :credential-rejected catalog-reachability))
    :credential-rejected
    (and catalog-required? (= :temporarily-unreachable catalog-reachability))
    :temporarily-unreachable
    (and catalog-required? (not served?)) :unavailable-to-account
    :else :available))

(defn availability-reason
  "The machine-readable reason below an availability state."
  [{:keys [provider-known? registered? implemented?] :as facts}]
  (case (availability-state facts)
    :not-implemented (cond
                       (not provider-known?) :provider-adapter-missing
                       (not registered?) :registry-missing
                       (not implemented?) :adapter-missing)
    nil))

;; ---------------------------------------------------------------------------
;; Version grammar
;; ---------------------------------------------------------------------------

(def ^:private version-token-re
  ;; 5p2, k2p6, v4, 5.6 — an optional letter, digits, optionally a minor part
  ;; after `p` (Fireworks writes 5p2) or `.` (OpenAI writes 5.6).
  ;; Deliberately rejects "120b" and "oss": a size or a codename is not a
  ;; version, and treating it as one would let `auto` pick gpt-oss-120b over
  ;; gpt-oss-20b as if it were an upgrade.
  #"^[a-z]?\d+([p.]\d+)?$")

(defn- tokens [id] (str/split (str id) #"-"))

(defn version-of
  "The version token of a model id, or nil when it carries no version."
  [id]
  (->> (tokens id)
       (filter #(re-matches version-token-re %))
       last))

(defn family-of
  "The id with its version slot blanked — the family key. nil when unversioned
   (such a model is its own family and can never be auto-upgraded)."
  [id]
  (when-let [v (version-of id)]
    (->> (tokens id)
         (map #(if (= % v) "*" %))
         (str/join "-"))))

(def ^:private version-key-width 4)

(defn- version-key
  "Sortable key for a version token: its digit groups, in order, PADDED with
   zeros. \"k2p6\" → [2 6 0 0].

   The padding is the whole point. Clojure compares vectors by COUNT first, so
   the unpadded keys made \"3.5\" [3 5] outrank \"4\" [4] — `:auto` on the
   gpt-*-turbo family would have picked gpt-3.5-turbo over gpt-4-turbo. Padded,
   the comparison is major-then-minor, which is what a version means."
  [v]
  (let [groups (mapv parse-long (re-seq #"\d+" (str v)))]
    (into groups (repeat (- version-key-width (count groups)) 0))))

(defn id-for
  "The concrete id in `family` carrying `version`."
  [family version]
  (str/replace (str family) "*" (str version)))

;; ---------------------------------------------------------------------------
;; Provider catalog — what is ACTUALLY on offer, not what we remember
;; ---------------------------------------------------------------------------

(def ^:private catalog-ttl-ms (* 10 60 1000))

(defonce ^:private catalog-cache
  ;; `:refresh` is a process-local single-flight ticket. It is deliberately kept
  ;; beside the cache it protects: replacing/resetting this map fences an older
  ;; in-flight request without holding a monitor across provider I/O.
  (atom {:generation 0 :at 0 :configuration [] :endpoints {} :refresh nil}))

(def ^:private openai-base "https://api.openai.com/v1")
(def ^:private fireworks-base "https://api.fireworks.ai/inference/v1")
(def ^:private anthropic-base "https://api.anthropic.com/v1")

(def anthropic-api-version
  "Value of Anthropic's REQUIRED `anthropic-version` request header.

   Anthropic versions the API by header, not by URL, and documents `2023-06-01`
   as the current version; within a version it promises to preserve existing
   input and output parameters and to add only optional inputs and new output
   values. That promise is what lets this code treat a MISSING documented field
   as a contract violation rather than as an omission it should tolerate.

   <https://platform.claude.com/docs/en/api/versioning>"
  "2023-06-01")

(def ^:dynamic *env-lookup*
  "Environment lookup seam. Tests bind this to a map so catalog verification
   never depends on, or discloses, developer credentials."
  #(System/getenv %))

(def ^:dynamic *provider-base-urls*
  "Default provider bases. Tests bind these to local HTTP fixtures; production
   uses the vendors' documented endpoints. OPENAI_BASE_URL and
   FIREWORKS_BASE_URL, when present, override only their matching entries."
  {:openai openai-base
   :fireworks fireworks-base
   :anthropic anthropic-base})

(def ^:private openai-snapshot-id-re
  ;; gpt-5.5-2026-04-23 — a dated snapshot of a model we already list under its
  ;; rolling id. The date's digit groups would parse as a version token, so
  ;; leaving these in would have `:auto` chasing release dates inside families
  ;; that exist only because a snapshot id got split apart.
  #".*-\d{4}-\d{2}-\d{2}$")

(def ^:private anthropic-snapshot-id-re
  ;; claude-haiku-4-5-20251001 — Anthropic writes the date UNDASHED, so the
  ;; OpenAI pattern above never matched one. The two providers also want
  ;; opposite treatment, which is why this is a per-contract policy rather than
  ;; one shared regex: OpenAI's dated ids are DROPPED, Anthropic's are KEPT and
  ;; additionally contributed under their alias (see `anthropic-alias-id`).
  #"^(.*)-(\d{8})$")

(def catalog-contracts
  "How ONE configured endpoint's model list is read, and on whose authority.

   Two endpoints speaking the same wire protocol do not have the same
   documented contract, and flattening that away is how an undocumented
   compatibility extension ends up cited as a vendor promise.

   `:openai-models-api` is OpenAI's documented List models operation:
   `GET /v1/models`, answering `{\"object\": \"list\", \"data\": [{\"id\": ...}]}`,
   with no pagination.

   `:fireworks-inference-models-list` is that same shape OBSERVED at Fireworks'
   documented inference base, `https://api.fireworks.ai/inference/v1/models`.
   Fireworks' OpenAI-compatibility page documents completions and chat
   completions; it documents no models list, so this is observed behavior and
   not a promised contract. What it answers is the set of ids this credential
   can address for inference — on 2026-08-27, 24 ids: 20 serverless models plus
   4 `accounts/fireworks/routers/...` ids.

   Fireworks' documented NATIVE list operation is a different API answering a
   different question. `GET /v1/accounts/{account_id}/models` answers
   `{\"models\": [{\"name\": ..., \"supportsServerless\": ...}], \"nextPageToken\": ...}`:
   entries are named by `name`, not `id`; it pages at 200 rows (300 rows under
   `accounts/fireworks` on 2026-08-27); and it enumerates a vendor account's
   whole collection rather than what this key may call. Its documented
   `filter=supports_serverless=true` narrows that to the 20 serverless models —
   a strict subset of the inference base's answer, still not scoped to this
   credential, and still missing the router ids. So it is NOT the source this
   picker reads, and nothing here may be documented as if it were.

   `:openai-compatible-models-list` covers an operator-supplied
   `OPENAI_BASE_URL`. Whoever set that variable asserted protocol
   compatibility; no vendor promised a models list there either.

   `:anthropic-models-api` is Anthropic's own documented List models operation
   and is NOT an OpenAI-compatible endpoint — routing it through the parser
   above would be wrong in three separate ways at once. It authenticates with
   `x-api-key`, not `Authorization: Bearer`; it requires the `anthropic-version`
   header on every request; and it PAGES, answering
   `{\"data\": [{\"id\": ..., \"type\": \"model\", ...}], \"has_more\": bool,
   \"first_id\": ..., \"last_id\": ...}` with a default page of 20 and a maximum
   of 1000, walked forward by passing `last_id` back as `after_id`. Under the
   OpenAI parser its mandatory `has_more` reads as an unexpected page marker and
   every page after the first is simply lost.

   `:auth` names how the credential is presented, `:paginated?` whether the
   contract has more than one page to collect, and `:dated-snapshots` what each
   vendor's dated ids mean here. `:documented?` records who stands behind the
   response, not whether it works. Every contract is checked on each fetch and
   fails closed."
  {:openai-models-api
   {:path "/models"
    :envelope :openai-models-list
    :auth :bearer
    :paginated? false
    :dated-snapshots :drop
    :documented? true
    :documentation "https://platform.openai.com/docs/api-reference/models/list"}

   :fireworks-inference-models-list
   {:path "/models"
    :envelope :openai-models-list
    :auth :bearer
    :paginated? false
    :dated-snapshots :drop
    :documented? false
    :observed "2026-08-27: GET https://api.fireworks.ai/inference/v1/models"
    :documentation "https://docs.fireworks.ai/tools-sdks/openai-compatibility"
    :native-alternative "https://docs.fireworks.ai/api-reference/list-models"}

   :openai-compatible-models-list
   {:path "/models"
    :envelope :openai-models-list
    :auth :bearer
    :paginated? false
    :dated-snapshots :drop
    :documented? false}

   :anthropic-models-api
   {:path "/models"
    :envelope :anthropic-models-list
    :auth :anthropic-api-key
    :paginated? true
    :page-limit 1000
    :dated-snapshots :alias
    :documented? true
    :documentation "https://platform.claude.com/docs/en/api/models/list"}})

(def ^:private max-catalog-pages
  "How many pages one paginated contract may collect before the walk is called
   broken.

   At Anthropic's documented maximum page size this is 10,000 model ids, orders
   of magnitude above any real account. The cap exists for the case where a
   provider keeps answering `has_more: true` — without it the fetch would spin
   forever holding the refresh. Hitting it is reported as a FAILURE, never as a
   short list: a truncated account is the one outcome that turns missing
   evidence into `:unavailable-to-account`."
  10)

(def ^:private continuation-keys
  "Keys with which a paging answer says \"there is more\".

   None of the three contracts above pages, so any of these is a contract
   change — and a TRUNCATED list is worse than no list at all: every id below
   the cut would be reported `:unavailable-to-account`, a permanent-sounding
   verdict, on evidence that is merely incomplete."
  [:has_more :next_page :next_page_token :nextPageToken :page_token])

(defn- continuation-marker
  "The continuation key present in `body`, or nil. `false` and an empty string
   are how these fields say \"no more pages\"."
  [body]
  (some (fn [k]
          (let [v (get body k)]
            (when (and v (not (and (string? v) (str/blank? v)))) k)))
        continuation-keys))

(defn- openai-models-list-ids
  "Ids from an OpenAI Models-API-shaped body, `{:data [{:id ...}]}`.

   Unknown member fields are ignored by design — Fireworks adds `kind`,
   `context_length` and `supports_*` to every entry — but a missing or
   non-sequential `data` is refused rather than read as an empty account."
  [body]
  (when-not (map? body)
    (throw (ex-info "Model list response is not a JSON object" {})))
  (when-let [k (continuation-marker body)]
    (throw (ex-info "Model list response is paginated; this contract is not"
                    {:continuation-key k})))
  (let [data (:data body)]
    (when-not (sequential? data)
      (throw (ex-info "Model list response has no data list" {})))
    (->> data
         (map :id)
         (filter string?)
         distinct
         vec)))

(defn- anthropic-models-list-ids
  "Ids from ONE page of Anthropic's Models API, `{:data [{:id ...}], :has_more
   bool}`.

   `has_more` is a documented, non-optional member of the response, and within
   the `2023-06-01` version Anthropic promises not to remove it. Requiring it is
   therefore how this parser refuses a body that is not this contract — an
   OpenAI-shaped `{\"object\": \"list\", \"data\": [...]}` answer carries no
   `has_more`, and reading one here would silently accept a single page as a
   complete account.

   Member fields beyond `id` (`display_name`, `created_at`, `max_input_tokens`,
   `max_tokens`, `capabilities`) are ignored by design: the catalog answers
   which ids this credential may address, and dvergr's registry — not a model
   list — owns metadata."
  [body]
  (when-not (map? body)
    (throw (ex-info "Model list response is not a JSON object" {})))
  (when-not (contains? body :has_more)
    (throw (ex-info "Model list response is missing Anthropic's has_more field"
                    {})))
  (let [data (:data body)]
    (when-not (sequential? data)
      (throw (ex-info "Model list response has no data list" {})))
    (->> data (map :id) (filter string?) distinct vec)))

(defn anthropic-alias-id
  "The alias a dated Anthropic snapshot id resolves to, or nil.

   Anthropic documents that for models before the 4.6 generation \"the alias is
   a convenience pointer that resolves to the dated ID\", spelled
   `claude-haiku-4-5` for `claude-haiku-4-5-20251001`; from 4.6 on, ids are
   dateless and are their own pinned snapshot.

   The list operation answers with pinned ids, and dvergr's registry carries the
   dateless spelling — `claude-haiku-4-5`, `claude-sonnet-4-5`,
   `claude-opus-4-5`. Matched as raw strings those two never meet, so an account
   that plainly serves Haiku 4.5 would have reported it
   `:unavailable-to-account`: a permanent verdict about the account produced by
   a naming convention. Contributing the alias BESIDE the pinned id resolves
   either spelling and invents nothing — the alias is a pointer to that exact
   snapshot the account was just observed to serve.

   This is load-bearing, not defensive. Observed 2026-08-27 against a real
   account: `GET /v1/models` answered 10 ids, and every pre-4.6-generation model
   appeared ONLY in pinned form — `claude-opus-4-5-20251101`,
   `claude-haiku-4-5-20251001`, `claude-sonnet-4-5-20250929`, with no dateless
   spelling anywhere in the response. Those are exactly the three ids dvergr's
   registry carries dateless. With this mapping removed, all three report
   `:unavailable-to-account` on an account that plainly serves them.

   <https://platform.claude.com/docs/en/about-claude/models/model-ids-and-versions>"
  [model-id]
  (second (re-matches anthropic-snapshot-id-re (str model-id))))

(defn- parse-catalog-body
  "Model ids under ONE endpoint's contract.

   Provider-specific by construction: there is no single \"OpenAI-compatible\"
   parser here, only a parser per contract. Anthropic is not an
   OpenAI-compatible provider and never reaches the parser above."
  [catalog-contract body]
  (case catalog-contract
    :openai-models-api (openai-models-list-ids body)

    :anthropic-models-api (anthropic-models-list-ids body)

    :fireworks-inference-models-list
    ;; Fireworks' native ListModels answers `{"models": [...], "nextPageToken":
    ;; ...}` and carries no `data` at all. Naming that shape here keeps a
    ;; reshaped or misrouted answer from being read as an account that serves
    ;; nothing — which would disable every Fireworks row as
    ;; `:unavailable-to-account`.
    (do (when (and (map? body)
                   (or (contains? body :models) (contains? body :nextPageToken)))
          (throw (ex-info "Fireworks answered the native ListModels schema at the compatible path"
                          {:contract catalog-contract})))
        (openai-models-list-ids body))

    :openai-compatible-models-list (openai-models-list-ids body)

    ;; An endpoint configured with no contract is a bug here, not a provider
    ;; fault. Say that in the log instead of quietly answering "no models".
    (throw (ex-info "Endpoint has no model-list contract"
                    {:contract catalog-contract}))))

(defn- normalize-model-ids
  "Served ids as the registry and the picker spell them, under one contract.

   Two vendors, two opposite policies for the same phenomenon — a dated id:

   `:drop` (OpenAI and the compatible bases) removes `gpt-5.5-2026-04-23`,
   because the date's digit groups parse as a version token and `:auto` would
   chase release dates through families that exist only because a snapshot id
   got split apart.

   `:alias` (Anthropic) KEEPS the pinned id and adds its alias, because
   Anthropic's list answers with pinned ids while the registry spells the same
   model dateless. Dropping them instead would delete the only evidence the
   account gave.

   Order is preserved and duplicates collapse, so an account that already lists
   both spellings gains nothing and loses nothing."
  [catalog-contract model-ids]
  (case (:dated-snapshots (get catalog-contracts catalog-contract))
    :alias (into [] (comp (mapcat (juxt identity anthropic-alias-id))
                          (remove nil?)
                          (distinct))
                 model-ids)
    (into [] (comp (remove #(re-matches openai-snapshot-id-re %)) (distinct))
          model-ids)))

(defn- normalize-base-url [base]
  (str/replace (str base) #"/+$" ""))

(defn- configured-endpoints
  "Every configured provider endpoint as provider-aware records.

   Provider identity is configuration, never inferred from the URL. This is
   why OPENAI_BASE_URL may equal Fireworks' base without either entry replacing
   the other. Each entry reads exactly one named credential; keys are never
   borrowed across providers.

   `:endpoint-kind` distinguishes native OpenAI from OpenAI-compatible request
   behavior, and marks Anthropic as neither; `:catalog-contract` names which
   model-list contract this endpoint answers under (see `catalog-contracts`).
   The credential itself is intentionally private to this fetch boundary and is
   never copied into catalog results or cache state.

   `OPENAI_BASE_URL` deliberately does not re-point Anthropic: it re-points the
   key that names it, and an OpenAI-compatible base does not implement
   Anthropic's Models API."
  []
  (let [openai-key (*env-lookup* "OPENAI_API_KEY")
        fireworks-key (*env-lookup* "FIREWORKS_API_KEY")
        anthropic-key (*env-lookup* "ANTHROPIC_API_KEY")
        custom-openai-base (*env-lookup* "OPENAI_BASE_URL")
        custom-fireworks-base (*env-lookup* "FIREWORKS_BASE_URL")]
    (cond-> []
      (seq anthropic-key)
      (conj {:provider :anthropic
             :base-url (normalize-base-url (:anthropic *provider-base-urls*))
             :credential-source "ANTHROPIC_API_KEY"
             :endpoint-kind :anthropic-native
             :native-openai? false
             :catalog-contract :anthropic-models-api
             :credential anthropic-key})

      (seq fireworks-key)
      (conj {:provider :fireworks
             :base-url (normalize-base-url
                        (or (not-empty custom-fireworks-base)
                            (:fireworks *provider-base-urls*)))
             :credential-source "FIREWORKS_API_KEY"
             :endpoint-kind :openai-compatible
             :native-openai? false
             ;; The observed Fireworks list contract is evidence only for the
             ;; vendor inference base. An operator-supplied base asserts generic
             ;; OpenAI compatibility; it must not inherit a vendor-specific
             ;; promise merely because the credential remains Fireworks-scoped.
             :catalog-contract (if (seq custom-fireworks-base)
                                 :openai-compatible-models-list
                                 :fireworks-inference-models-list)
             :credential fireworks-key})

      (seq openai-key)
      (conj {:provider :openai
             :base-url (normalize-base-url
                        (or (not-empty custom-openai-base)
                            (:openai *provider-base-urls*)))
             :credential-source "OPENAI_API_KEY"
             :endpoint-kind (if (seq custom-openai-base)
                              :openai-compatible
                              :openai-native)
             :native-openai? (not (seq custom-openai-base))
             :catalog-contract (if (seq custom-openai-base)
                                 :openai-compatible-models-list
                                 :openai-models-api)
             :credential openai-key}))))

(def ^:private public-endpoint-keys
  [:provider :base-url :credential-source :endpoint-kind :native-openai?
   :catalog-contract])

(defn- public-endpoint [endpoint]
  (select-keys endpoint public-endpoint-keys))

(defn provider-endpoints
  "Configured provider endpoint records, without credential values. The named
   `:credential-source` is safe to inspect; the secret itself never leaves the
   private fetch boundary."
  []
  (mapv public-endpoint (configured-endpoints)))

(defn provider-contract
  "Static contract for `provider`, including its exact credential variable."
  [provider]
  (get provider-contracts (keyword provider)))

(defn credential-present?
  "Whether this process started with the provider's credential available."
  [provider]
  (when-let [credential-source (:credential-source (provider-contract provider))]
    (boolean (seq (*env-lookup* credential-source)))))

(defn- endpoint-key [endpoint]
  [(:provider endpoint) (:base-url endpoint) (:credential-source endpoint)])

(defn- model-record [endpoint model-id reachability]
  (assoc (public-endpoint endpoint)
         :model-id model-id
         :reachability reachability
         :reachable? (= :reachable reachability)))

(defn- catalog-request-headers
  "How ONE contract presents its credential.

   Anthropic authenticates with `x-api-key` and REQUIRES `anthropic-version` on
   every request; sending it an OpenAI bearer token gets a 401, which would have
   read as `:credential-rejected` and told the operator to replace a key that
   was fine. The credential is passed in and used here; it is never returned,
   logged, or stored."
  [catalog-contract credential]
  (case (:auth (get catalog-contracts catalog-contract))
    :anthropic-api-key {"x-api-key" credential
                        "anthropic-version" anthropic-api-version}
    :bearer {"Authorization" (str "Bearer " credential)}
    (throw (ex-info "Endpoint contract declares no credential presentation"
                    {:contract catalog-contract}))))

(defn- page-url
  "One page request URL. `after` is the previous page's `last_id` cursor."
  [base-url {:keys [path page-limit]} after]
  (str base-url path
       (when page-limit
         (str "?limit=" page-limit
              (when after
                (str "&after_id="
                     (java.net.URLEncoder/encode (str after) "UTF-8")))))))

(defn- next-page-cursor
  "The cursor for the page after `body`, or nil when this was the last page.

   Anthropic signals more pages with `has_more` and hands back `last_id` to
   pass as `after_id`. `has_more` WITHOUT a usable `last_id` is a contract this
   code cannot walk, and answering it with a short list would report every id
   below the cut as `:unavailable-to-account` — so it throws instead."
  [body]
  (when (true? (:has_more body))
    (let [last-id (:last_id body)]
      (when-not (and (string? last-id) (seq last-id))
        (throw (ex-info "Model list says has_more but supplies no last_id cursor"
                        {})))
      last-id)))

(defn- fetch-endpoint!
  "The model ids one endpoint currently serves, under that endpoint's contract.

   Answers `{:outcome :reachable :model-ids [...]}`, or an outcome that says
   why there are none. A REJECTED credential is reported apart from an outage:
   a key the provider refuses is a permanent, actionable configuration fault,
   and calling it \"could not be refreshed\" sends an operator to wait for a
   recovery that cannot arrive. Neither failure ever produces ids, so no
   failure — and no malformed, truncated, or foreign-schema body — can be read
   as availability.

   A PAGINATED contract is complete only when the walk ends on its own. A
   failure on page three discards pages one and two rather than reporting them:
   a partial account is indistinguishable from a small one, and the difference
   between them is `:unavailable-to-account`, which reads as permanent."
  [{:keys [base-url credential catalog-contract] :as endpoint}]
  (let [{:keys [paginated?] :as contract} (get catalog-contracts catalog-contract)
        headers (catalog-request-headers catalog-contract credential)]
    (try
      (loop [after nil
             page 1
             collected []]
        (when (> page max-catalog-pages)
          (throw (ex-info "Model list did not finish paging"
                          {:pages max-catalog-pages})))
        (let [resp ((requiring-resolve 'babashka.http-client/get)
                    (page-url base-url contract after)
                    {:headers headers
                     :timeout 10000
                     ;; Classify the status here rather than letting the client
                     ;; throw: 401 and 403 have to survive as themselves.
                     :throw false})
              status (:status resp)]
          (cond
            (contains? #{401 403} status)
            (do (log/log! {:level :warn :id ::catalog-credential-rejected
                           :msg "Provider refused the configured credential"
                           :data {:provider (:provider endpoint)
                                  :base-url base-url
                                  :credential-source (:credential-source endpoint)
                                  :catalog-contract catalog-contract
                                  :status status}})
                {:outcome :credential-rejected})

            (not (<= 200 status 299))
            (throw (ex-info "Model endpoint returned a non-success status"
                            {:status status}))

            :else
            (let [body ((requiring-resolve 'jsonista.core/read-value)
                        (:body resp)
                        ((requiring-resolve 'jsonista.core/object-mapper)
                         {:decode-key-fn true}))
                  ids (into collected (parse-catalog-body catalog-contract body))
                  cursor (when paginated? (next-page-cursor body))]
              (if cursor
                (recur cursor (inc page) ids)
                {:outcome :reachable
                 :model-ids (normalize-model-ids catalog-contract ids)})))))
      (catch Throwable t
        (log/log! {:level :warn :id ::catalog-fetch-failed
                   :data {:provider (:provider endpoint)
                          :base-url base-url
                          :credential-source (:credential-source endpoint)
                          :catalog-contract catalog-contract
                          :error (ex-message t)}})
        {:outcome :unreachable}))))

(defn- refresh-endpoint
  "Refresh one provider without disturbing any other provider's state.

   A failed fetch retains only that exact endpoint's last-known ids, marks them
   unreachable, and keeps the reachability that names the failure. With no
   last-known-good state it returns no model records; a configured key or a
   code default is not evidence of availability."
  [endpoint previous]
  (let [{:keys [outcome model-ids]} (fetch-endpoint! endpoint)]
    (if (= :reachable outcome)
      {:endpoint (public-endpoint endpoint)
       :reachability :reachable
       :last-success-at (System/currentTimeMillis)
       :models (mapv #(model-record endpoint % :reachable) model-ids)}
      {:endpoint (public-endpoint endpoint)
       :reachability (if (= :credential-rejected outcome)
                       :credential-rejected
                       :temporarily-unreachable)
       :last-success-at (:last-success-at previous)
       :models (mapv #(assoc % :reachability :unreachable :reachable? false)
                     (:models previous))})))

(defn- catalog-records [cache]
  (->> (:endpoints cache)
       vals
       (mapcat :models)
       vec))

(defn reset-catalog!
  "Forget cached provider results and fence refreshes that began before reset.
   Intended for explicit configuration changes and deterministic tests; normal
   callers use the TTL or `(catalog true)`."
  []
  (swap! catalog-cache
         (fn [current]
           {:generation (inc (long (or (:generation current) 0)))
            :at 0
            :configuration []
            :endpoints {}
            :refresh nil})))

(defn- await-refresh
  "Wait for one synchronous catalog refresh shared by concurrent callers."
  [{:keys [completion]}]
  (let [{:keys [records error]} @completion]
    (if error
      (throw error)
      records)))

(def ^:private retry-catalog-refresh ::retry-catalog-refresh)

(defn- catalog-at-generation
  "Read or refresh one endpoint configuration while `generation` remains live.

   Returns `retry-catalog-refresh` if reset happened after the caller captured
   its generation, including while endpoint configuration itself was read."
  [force? generation endpoints configuration]
  (loop []
    (let [{raw-generation :generation
           :keys [at refresh]
           :as cached} @catalog-cache
          current-generation (long (or raw-generation 0))
          fresh? (and (= configuration (:configuration cached))
                      (< (- (System/currentTimeMillis) at) catalog-ttl-ms))]
      (cond
        (not= generation current-generation)
        retry-catalog-refresh

        (and fresh? (not force?))
        (catalog-records cached)

        ;; A forced caller joins a refresh already under way instead of
        ;; starting a second request that can complete out of order. An
        ;; ordinary caller joins too once the cached result has expired.
        (= configuration (:configuration refresh))
        (await-refresh refresh)

        :else
        (let [token (Object.)
              completion (promise)
              ticket {:configuration configuration
                      :generation generation
                      :token token
                      :completion completion}
              claimed (assoc cached :refresh ticket)]
          (if-not (compare-and-set! catalog-cache cached claimed)
            (recur)
            (try
              (let [previous (:endpoints cached)
                    endpoint-states
                    (into {}
                          (map (fn [endpoint]
                                 (let [k (endpoint-key endpoint)]
                                   [k (refresh-endpoint endpoint (get previous k))])))
                          endpoints)
                    refreshed {:generation generation
                               :at (System/currentTimeMillis)
                               :configuration configuration
                               :endpoints endpoint-states
                               :refresh nil}
                    records (catalog-records refreshed)]
                ;; Reset or a differently configured refresh may have replaced
                ;; this ticket while provider I/O was in flight. Publish only
                ;; while this exact claim still owns the same generation.
                (swap! catalog-cache
                       (fn [current]
                         (if (and (= generation (long (or (:generation current) 0)))
                                  (identical? token (get-in current [:refresh :token])))
                           refreshed
                           current)))
                (deliver completion {:records records})
                records)
              (catch Throwable t
                (swap! catalog-cache
                       (fn [current]
                         (if (and (= generation (long (or (:generation current) 0)))
                                  (identical? token (get-in current [:refresh :token])))
                           (assoc current :refresh nil)
                           current)))
                (deliver completion {:error t})
                (throw t)))))))))

(defn catalog
  "Provider-aware model records from each endpoint's own model-list contract.

   Successful records are reachable now. Failed providers retain their own
   last-known ids as `:reachability :unreachable`; consumers deciding what is
   available must use `available-catalog`. There is deliberately no default-id
   fallback: configuration and registry knowledge do not prove reachability."
  ([] (catalog false))
  ([force?]
   (loop []
     ;; Capture the reset generation BEFORE reading environment-derived endpoint
     ;; configuration. A reset that overlaps this read invalidates both the
     ;; resulting configuration and any attempt to claim the cache with it.
     (let [generation (long (or (:generation @catalog-cache) 0))
           endpoints (configured-endpoints)
           configuration (mapv endpoint-key endpoints)
           result (catalog-at-generation force? generation endpoints configuration)]
       (if (= retry-catalog-refresh result)
         (recur)
         result)))))

(defn available-catalog
  "Catalog records whose provider answered the current fetch successfully."
  []
  (filterv :reachable? (catalog)))

(defn provider-catalog-status
  "Current fetch state and last-known served ids for one catalog provider.

   A transient failure leaves `:served-model-ids` intact and changes only
   `:reachability`; an initial failure has an empty last-known set. A missing
   credential is configuration, not a failed fetch; a REJECTED credential is
   neither, and reports `:credential-rejected`."
  [provider]
  (let [provider (keyword provider)
        contract (provider-contract provider)]
    (cond
      (not (:catalog-required? contract))
      {:reachability :not-required :served-model-ids #{}}

      (not (credential-present? provider))
      {:reachability :not-configured :served-model-ids #{}}

      :else
      (do
        (catalog)
        (if-let [endpoint-state
                 (some #(when (= provider (get-in % [:endpoint :provider])) %)
                       (vals (:endpoints @catalog-cache)))]
          {:reachability (:reachability endpoint-state)
           :last-success-at (:last-success-at endpoint-state)
           :served-model-ids (into #{} (map :model-id) (:models endpoint-state))}
          ;; A configured endpoint always receives a state during refresh. If
          ;; that invariant is broken, fail closed instead of interpreting the
          ;; missing state as a successful empty account.
          {:reachability :temporarily-unreachable :served-model-ids #{}})))))

(defn infer-provider
  "Provider identity for an id/family, without choosing a different provider.

   The registry is authoritative when it has the id. Prefixes only recover the
   identity already encoded in an unregistered id; there is deliberately no
   catch-all provider."
  [id-or-family]
  (or (some-> (registry/get-model id-or-family) :provider)
      (some->> (registry/list-models)
               (filter #(= id-or-family (family-of (:id %))))
               first
               :provider)
      (cond
        (str/starts-with? (str id-or-family) "accounts/fireworks/models/") :fireworks
        (str/starts-with? (str id-or-family) "gpt-") :openai
        (str/starts-with? (str id-or-family) "claude-") :anthropic)))

(defn model-availability
  "Authoritative availability result for an exact provider/model pair.

   Registry metadata is never synthesized from a model list. A served unknown
   id is therefore `:not-implemented`/`:registry-missing`; a registered id
   absent from a SUCCESSFUL account list is `:unavailable-to-account`; a fetch
   failure retains the last-known served set while reporting
   `:temporarily-unreachable`; and a refused credential reports
   `:credential-rejected`. Only a successful, complete, contract-shaped
   response can make a candidate absent — evidence that is missing or partial
   never becomes a verdict about the account."
  ([model-id]
   (model-availability (infer-provider model-id) model-id))
  ([provider model-id]
   (let [provider (some-> provider keyword)
         contract (provider-contract provider)
         definition (registry/get-model model-id)
         registered? (boolean (and definition (= provider (:provider definition))))
         implemented? (boolean
                       (and registered?
                            (contains? (:implemented-api-types contract)
                                       (:api-type definition))))
         catalog-status (provider-catalog-status provider)
         facts {:provider-known? (boolean contract)
                :credential-present? (credential-present? provider)
                :registered? registered?
                :implemented? implemented?
                :catalog-required? (boolean (:catalog-required? contract))
                :catalog-reachability (:reachability catalog-status)
                :served? (or (not (:catalog-required? contract))
                             (contains? (:served-model-ids catalog-status) model-id))}
         state (availability-state facts)]
     (merge facts
            {:state state
             :reason (availability-reason facts)
             :available? (= :available state)
             :provider provider
             :model-id model-id
             :credential-source (:credential-source contract)
             :last-success-at (:last-success-at catalog-status)}))))

(defn available-model?
  "Whether the exact provider/model pair is executable now."
  ([model-id]
   (:available? (model-availability model-id)))
  ([provider model-id]
   (:available? (model-availability provider model-id))))

(defn versions-in
  "Versions of `family` currently offered, newest first. With `provider`, only
   that provider's endpoint contributes versions."
  ([family]
   (versions-in nil family))
  ([provider family]
   (->> (available-catalog)
        (filter #(or (nil? provider) (= (keyword provider) (:provider %))))
        (map :model-id)
        (filter #(= family (family-of %)))
        (map version-of)
        distinct
        (sort-by version-key #(compare %2 %1))
        vec)))

(defn known-versions-in
  "Last-known served OR registered versions of `family`, newest first.

   This union keeps withdrawn and newly served/unregistered states observable;
   availability is decided separately for each exact id."
  ([family]
   (known-versions-in nil family))
  ([provider family]
   (let [provider (some-> (or provider (infer-provider family)) keyword)
         served (->> (:served-model-ids (provider-catalog-status provider))
                     (filter #(= family (family-of %))))
         registered (->> (registry/list-models)
                         (filter #(= provider (:provider %)))
                         (map :id)
                         (filter #(= family (family-of %))))]
     (->> (concat served registered)
          (map version-of)
          (remove nil?)
          distinct
          (sort-by version-key #(compare %2 %1))
          vec))))

(defn newest-usable
  "Newest version of `family` that the provider serves AND the registry knows.

   Not simply the newest on offer. Fireworks serves kimi-k3 and minimax-m3 today
   while dvergr's registry stops at k2p6 and m2p7, and `get-model!` throws on an
   id it does not know — so `:auto` on those families would have picked a model
   that kills the turn. One version behind beats a turn that cannot start.

   Returns nil when none is available. It never substitutes an unregistered,
   withdrawn, unreachable, or differently provided model."
  ([family]
   (newest-usable nil family))
  ([provider family]
   (let [provider (or provider (infer-provider family))
         vs (known-versions-in provider family)
         usable (filter #(available-model? provider (id-for family %)) vs)]
     (when (and (seq vs) (seq usable) (not= (first vs) (first usable)))
       (log/log! {:level :info :id ::newer-version-not-registered
                  :data {:provider provider
                         :family family
                         :serving (first vs)
                         :using (first usable)}}))
     (first usable))))

(defn newest-supported
  "Newest version of `family` this build can RUN, ignoring reachability.

   `newest-usable` answers `:auto` and is gated on everything, reachability
   included. When it finds nothing, something must still be NAMED: the picker
   shows a Latest row and the resolver reports why that row is unusable. Naming
   the newest KNOWN version instead picked up ids a provider serves but dvergr
   does not implement, so a Fireworks outage relabelled the Kimi family
   \"Not supported\" and pointed the row at kimi-k3 — a model simmis would
   never run — while the k2p6 row beside it correctly read
   \"Temporarily unreachable\".

   Registered for this provider and implemented by its adapter is the honest
   name. The reason then comes from that candidate's own availability."
  [provider family]
  (let [provider (some-> provider keyword)
        implemented (:implemented-api-types (provider-contract provider))]
    (->> (known-versions-in provider family)
         (filter (fn [version]
                   (let [definition (registry/get-model (id-for family version))]
                     (and definition
                          (= provider (:provider definition))
                          (contains? implemented (:api-type definition))))))
         first)))

(defn families
  "Families on offer → {family [versions newest-first]}. Powers a UI picker:
   choose the family, then :auto or a preferred version."
  []
  (->> (available-catalog)
       (map :model-id)
       (keep family-of)
       distinct
       (map (fn [f] [f (versions-in f)]))
       (into {})))

;; ---------------------------------------------------------------------------
;; Resolution
;; ---------------------------------------------------------------------------

(def default-family
  "Family form of `default-model`, for callers making an explicit family
   selection. New agents inherit instead of storing this value."
  (or (family-of default-model)
      "accounts/fireworks/models/glm-*"))

(defn- auto-version? [version]
  (or (nil? version) (= version :auto) (= version "auto")))

(defn- newer-version?
  "Whether `candidate` is strictly newer than `preferred`."
  [candidate preferred]
  (pos? (compare (version-key candidate) (version-key preferred))))

(defn- newest-usable-after
  "Newest usable version strictly after `preferred`, within one provider/family.

   This is the only soft-preference fallback path. Its inputs already carry the
   configured provider and family, and every candidate is rechecked through the
   authoritative availability matrix. It therefore cannot reach an older
   version, another family/provider, or the product default."
  [provider family preferred]
  (->> (known-versions-in provider family)
       (filter #(newer-version? % preferred))
       (filter #(available-model? provider (id-for family %)))
       first))

(defn resolve-selection
  "Availability-aware resolution for one stored or incoming selection.

   {:family f :version \"5p2\"} — the human prefers this explicit version.
   {:family f :version :auto}   — newest AVAILABLE version in that exact family.
   {:model id}                  — an explicit preferred version, or an exact
                                  unversioned model.

   Latest stores a family and resolves to its newest served, registered,
   implemented version. An explicit version stays in `:preferred-model` and
   `:candidate`. While usable, it is also `:model`. If it becomes unusable, the
   resolver may set `:model`/`:fallback-model` to the newest usable version that
   is strictly newer in the SAME family and provider. The preference itself is
   never rewritten, so it resumes automatically if it becomes usable again.

   With no such forward candidate, `:model` is nil. No path here consults a
   different family/provider or `default-model`."
  [{:keys [model family version provider]}]
  (if-not (or model family)
    {:selection-kind :unconfigured
     :candidate nil :model nil :provider nil :availability nil
     :preferred? false :fallback? false :available? false}
    (let [explicit-model (when-not family model)
          provider (or provider (infer-provider (or explicit-model family)))
          latest? (boolean (and family (auto-version? version)))
          preferred-family (when-not latest?
                             (or family (family-of explicit-model)))
          preferred-version (when-not latest?
                              (or (when family version)
                                  (version-of explicit-model)))
          preferred? (boolean (and preferred-family preferred-version))
          preferred-model (cond
                            preferred? (or explicit-model
                                           (id-for preferred-family preferred-version))
                            explicit-model explicit-model)
          preferred-availability (when preferred-model
                                   (model-availability provider preferred-model))
          latest-version (when latest? (newest-usable provider family))
          latest-candidate (when latest?
                             (some->> (or latest-version
                                          (newest-supported provider family)
                                          (first (known-versions-in provider family)))
                                      (id-for family)))
          ;; Asked unconditionally for a Latest selection, including when the
          ;; family has no known version at all. `model-availability` answers
          ;; that with `:not-implemented`/`:registry-missing`, which is the same
          ;; answer the picker's own family row gets. Returning nil here instead
          ;; left every display surface without a state to render.
          latest-availability (when latest?
                                (model-availability provider latest-candidate))
          fallback-version (when (and preferred?
                                      (not (:available? preferred-availability)))
                             (newest-usable-after provider preferred-family
                                                  preferred-version))
          fallback-model (some->> fallback-version
                                  (id-for preferred-family))
          fallback-availability (when fallback-model
                                  (model-availability provider fallback-model))
          fallback? (boolean (:available? fallback-availability))
          resolved-model (cond
                           latest? (when (:available? latest-availability)
                                     latest-candidate)
                           (:available? preferred-availability) preferred-model
                           fallback? fallback-model)
          resolved-availability (cond
                                  latest? latest-availability
                                  fallback? fallback-availability
                                  :else preferred-availability)
          candidate (if latest? latest-candidate preferred-model)]
      {:selection-kind (cond
                         latest? :latest
                         preferred? :preferred-version
                         :else :exact-model)
       :candidate candidate
       :model resolved-model
       :provider provider
       :availability resolved-availability
       :resolved-availability resolved-availability
       :preferred? preferred?
       :preferred-model (when preferred? preferred-model)
       :preferred-family (when preferred? preferred-family)
       :preferred-version (when preferred? preferred-version)
       :preferred-availability (when preferred? preferred-availability)
       :fallback? fallback?
       :fallback-model (when fallback? fallback-model)
       :fallback-version (when fallback? fallback-version)
       :fallback-reason (when fallback? :preferred-version-unavailable)
       :available? (boolean resolved-model)})))

(defn resolve-model
  "Concrete executable id for `selection`, or nil when it is unavailable."
  [selection]
  (:model (resolve-selection selection)))

(defn resolve-config
  "Model id for an agent's :actor/config, or nil when it configures no model.

   Honours the family/version form AND a legacy explicit :model — but the
   family form WINS when both are present, so a migrated agent follows its
   family instead of the stale id we are trying to grow out of."
  [{:keys [model-family model-version model]}]
  (resolve-model {:family model-family
                  :version model-version
                  :model (when-not model-family model)}))

(defn family? [s] (str/includes? (str s) "*"))

(defn resolve-string
  "One string, either form. `gpt-*-luna` is a family at its latest version;
   `gpt-5.5` is a preferred version.

   One attribute holds both because a person's model preference should not
   freeze the way an agent's config used to. Picking \"latest\" stores the
   family. Configuration reads can resolve that family again as availability
   changes; a live participant captures one such result when it joins and keeps
   that concrete spec until it leaves and rejoins."
  [s]
  (when (seq (str s))
    (if (family? s)
      (resolve-model {:family s :version :auto})
      (resolve-model {:model s}))))

(defn describe-resolution
  "Desired resolution of an agent's stored config, for display and join-time
   participant construction.

   `:model` is the concrete id this configuration currently resolves to;
   `:configured?` is false when the agent chose nothing and inherits its owner's
   preference. This function does not inspect an already joined participant or
   claim which model that participant has captured."
  [{:keys [model-family model-version model]}]
  (let [auto? (auto-version? model-version)
        resolved (resolve-selection {:family model-family
                                     :version model-version
                                     :model (when-not model-family model)})]
    (merge
     (select-keys resolved
                  [:selection-kind :model :candidate :provider :availability
                   :resolved-availability :available? :preferred?
                   :preferred-model :preferred-family :preferred-version
                   :preferred-availability :fallback? :fallback-model
                   :fallback-version :fallback-reason])
     {:family      model-family
      :version     (when-not auto? model-version)
      :auto?       (boolean (and model-family auto?))
      :configured? (boolean (or model-family model))})))
