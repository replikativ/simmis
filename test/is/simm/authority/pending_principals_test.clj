(ns is.simm.authority.pending-principals-test
  "Principal kinds the P1 plan introduces that do not exist yet. Each test is
   written against the seam it will go through and marked `:kaocha/pending`, so
   kaocha reports it as PENDING on every run (visible, never silently skipped)
   instead of failing. Whoever builds the seam removes the metadata.

   Kept deliberately few: one per missing principal kind.

   - external-agent party with narrowed scopes — waits for P1's
     `:actor/kind :external-agent` and scope-carrying tokens (§2.6, §2.9.1)
   - per-run agent token — waits for P1's per-run tokens bound to agent + room
     + tools, and for agent tokens being refused on the websocket (§2.1, §2.5)
   - Telegram sender — waits for the allowlisted tg-user → party mapping
     through the authenticator seam (§2.4)

   The seams are resolved with `requiring-resolve` at run time, so the names
   below are the INTENDED ones; this namespace compiles today."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]))

(defn- seam
  "The intended seam `sym`, or a failure naming what is missing."
  [sym]
  (or (some-> (try (requiring-resolve sym) (catch Exception _ nil)) deref)
      (throw (ex-info (str "seam not built yet: " sym) {:seam sym}))))

(deftest ^:kaocha/pending an-external-agent-acts-only-within-its-scopes
  ;; Waits for: external-agent parties + scoped agent tokens (§2.6, §2.9.1).
  ;; The check is `scope-permits? ∧ can?`: the agent's party is a member of the
  ;; room (so `can?` alone would say yes) but its token carries only
  ;; :rooms/read, so a wiki write must be refused while a read passes.
  (world/with-world [_ w]
    (let [mint! (seam 'is.simm.model.agent-tokens/mint!)
          authorize (seam 'is.simm.model.access/authorize-op)
          agent-principal (mint! {:owner (:owner w) :scopes #{:rooms/read}
                                  :rooms #{(:room w)}})]
      (is (true? (authorize agent-principal :read {:room (:room w)}))
          "legitimate: the scope it was granted")
      (is (false? (authorize agent-principal :write (:kb-granted-scope w)))
          "attack: membership does not widen a narrowed token")
      (is (false? (access/can? agent-principal :read :admin))
          ":admin is never mintable into an agent token"))))

(deftest ^:kaocha/pending a-per-run-agent-token-is-refused-on-the-websocket
  ;; Waits for: per-run tokens (§2.1) and the ws refusing agent tokens (§2.5).
  (world/with-world [_ w]
    (let [mint-run-token! (seam 'is.simm.model.agent-tokens/mint-run-token!)
          ws-principal (seam 'is.simm.runtimes.auth-config/ws-principal)
          human-token (seam 'is.simm.runtimes.auth-config/mint-access-token!)
          run-token (mint-run-token! {:agent (:team-agent w) :room (:room w)
                                      :tools #{"knowledge_search"}})]
      (is (some? (ws-principal (human-token (:member w))))
          "legitimate: a human web token opens the socket")
      (is (nil? (ws-principal run-token))
          "attack: an agent's run token does not"))))

(deftest ^:kaocha/pending a-telegram-sender-is-mapped-not-trusted
  ;; Waits for: tg-user → party through the authenticator seam (§2.4); an
  ;; empty allowlist must not mean open once hosted.
  (world/with-world [_ w]
    (let [authenticate (seam 'is.simm.runtimes.telegram/authenticate)]
      (is (= (str (:member w))
             (:sub (authenticate {:telegram/user {:id 4711} :allowlist {4711 (:member w)}})))
          "legitimate: an allowlisted sender acts as their party")
      (is (nil? (authenticate {:telegram/user {:id 666} :allowlist {4711 (:member w)}}))
          "attack: an unknown sender gets no principal")
      (is (nil? (authenticate {:telegram/user {:id 666} :allowlist {}}))
          "and an empty allowlist is not an open door"))))
