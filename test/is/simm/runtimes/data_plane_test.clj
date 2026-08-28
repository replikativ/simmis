(ns is.simm.runtimes.data-plane-test
  "`web/data-plane-authorized?` — the join-time gate on konserve-sync
   subscriptions. It decides which store SCOPES a connected peer may
   replicate, so a wrong `true` here does not leak one row: it hands over a
   whole database. Deny-by-default is the property, and every branch that
   returns `true` without consulting `access/can?` is a hole if it is wrong
   about what the topic is.

   Pure in (principal, topic) — no server, no sockets, no stores. The `:else`
   branch delegates to `access/can?`, which has its own coverage in
   `access-test`; here we pin the SHAPE of the dispatch: which topics
   short-circuit, which reach `can?`, and that an unauthenticated principal
   never gets past any of them."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.runtimes.web :as web]
            [is.simm.model.access :as access]))

(def ^:private alice #uuid "11111111-1111-1111-1111-111111111111")
(def ^:private bob   #uuid "22222222-2222-2222-2222-222222222222")

(defn- principal [party-uuid] (when party-uuid {:sub party-uuid}))

(defn- authorized?
  "Call the gate with `can?` stubbed to `can-answer`, so the dispatch is
   observable without a system DB. Records whether `can?` was consulted."
  [princ topic can-answer]
  (let [reached (atom false)]
    (with-redefs [access/can? (fn [& _] (reset! reached true) can-answer)]
      {:allowed? (boolean (web/data-plane-authorized? princ topic))
       :reached-can? @reached})))

;; =============================================================================
;; Anonymous
;; =============================================================================

(deftest anonymous-is-denied-on-every-self-deciding-branch
  (testing "the branches that answer WITHOUT can? deny an absent principal"
    ;; `can?` stubbed TRUE on purpose: if one of these let an anonymous peer
    ;; through on its own authority, this catches it rather than passing by
    ;; accident. The store-scope branch is deliberately not in this list — it
    ;; delegates, and delegation is asserted below.
    (doseq [topic [web/simmis-scope
                   :branching/event
                   :user-rooms/dirty
                   (keyword "notify" (str alice))]]
      (is (false? (:allowed? (authorized? nil topic true)))
          (str "anonymous denied on " topic))
      (is (false? (:allowed? (authorized? {} topic true)))
          (str "a principal with no :sub denied on " topic)))))

(deftest a-store-scope-delegates-anonymity-to-can?
  (testing "the gate does not decide anonymous access for a store scope itself"
    ;; It must REACH can?, which denies anonymous (access-test covers that).
    ;; The failure this pins is the gate short-circuiting a scope to true.
    (let [r (authorized? nil (random-uuid) false)]
      (is (false? (:allowed? r)))
      (is (true? (:reached-can? r)) "consulted can? rather than deciding"))))

;; =============================================================================
;; The short-circuiting branches
;; =============================================================================

(deftest the-shared-app-store-is-open-to-any-authenticated-party
  (testing "simmis-scope carries the app's own furniture, not tenant data"
    (let [r (authorized? (principal alice) web/simmis-scope false)]
      (is (true? (:allowed? r)))
      (is (false? (:reached-can? r)) "short-circuits before can?"))))

(deftest control-topics-are-an-allowlist-not-all-keywords
  (testing "listed invalidation topics are open to any authenticated party"
    (doseq [t [:branching/event :user-rooms/dirty]]
      (is (true? (:allowed? (authorized? (principal alice) t false)))
          (str t " is allowed"))))
  (testing "an UNLISTED keyword topic is not waved through by being a keyword"
    ;; The allowlist exists so a future keyword topic stays deny-by-default
    ;; until vetted. `can?` stubbed false = it fell through to the gate.
    (let [r (authorized? (principal alice) :some/future-topic false)]
      (is (false? (:allowed? r)))
      (is (true? (:reached-can? r)) "falls through to can?, not to true"))))

;; =============================================================================
;; Per-user notify topics — the privacy-critical branch
;; =============================================================================

(deftest a-notify-topic-reaches-only-its-own-party
  (testing "the owner may subscribe to their own mention stream"
    (let [t (keyword "notify" (str alice))
          r (authorized? (principal alice) t false)]
      (is (true? (:allowed? r)))
      (is (false? (:reached-can? r)) "identity decides, not a grant")))

  (testing "ANOTHER party may not — even one with blanket can? authority"
    ;; can? stubbed TRUE: a notify topic must not be reachable through any
    ;; relation, only through being that party. This is the assertion that
    ;; catches the branch being reordered below the `:else`.
    (let [t (keyword "notify" (str alice))
          r (authorized? (principal bob) t true)]
      (is (false? (:allowed? r))
          "bob cannot read alice's private notification stream")))

  (testing "the comparison is on the party id, not on a prefix of it"
    (let [t (keyword "notify" (str alice))
          almost (str (subs (str alice) 0 (dec (count (str alice)))))]
      (is (false? (:allowed? (authorized? {:sub almost} t true)))
          "a truncated id is a different party"))))

(deftest a-run-topic-delegates-to-exact-room-membership
  (let [topic (keyword "runs" (str alice))]
    (testing "room membership grants the private lifecycle stream"
      (let [seen (atom nil)]
        (with-redefs [access/can? (fn [subject action resource]
                                    (reset! seen [subject action resource])
                                    true)]
          (is (true? (web/data-plane-authorized? (principal bob) topic)))
          (is (= [(principal bob) :read {:room alice}] @seen)))))
    (testing "knowing a room UUID is not enough"
      (is (false? (:allowed? (authorized? (principal bob) topic false)))))
    (testing "a malformed runs topic gets no special treatment"
      (let [r (authorized? (principal bob) :runs/not-a-uuid false)]
        (is (false? (:allowed? r)))
        (is (true? (:reached-can? r)))))))

;; =============================================================================
;; Store scopes — the delegating branch
;; =============================================================================

(deftest a-store-scope-is-decided-by-can?
  (let [scope (random-uuid)]
    (testing "granted"
      (let [r (authorized? (principal alice) scope true)]
        (is (true? (:allowed? r)))
        (is (true? (:reached-can? r)))))
    (testing "not granted — knowing the scope uuid is not authorization"
      (let [r (authorized? (principal alice) scope false)]
        (is (false? (:allowed? r)))
        (is (true? (:reached-can? r)))))))

(deftest an-unrecognised-topic-shape-is-denied
  (testing "a string or nil topic reaches can? rather than short-circuiting true"
    (doseq [topic ["a-string" nil 42]]
      (is (false? (:allowed? (authorized? (principal alice) topic false)))
          (str "denied: " (pr-str topic))))))

;; =============================================================================
;; The publish gate — kabel :authorize-publish-fn
;; =============================================================================

(deftest every-inbound-publish-is-refused
  (testing "no topic class, no principal, admits a client write"
    ;; Sync is one-directional by construction: clients send transactions via
    ;; datahike's KabelWriter, and the publisher-side write-hook is attached
    ;; only in konserve-sync's server-side register-store!. So an inbound
    ;; publish is a bug or an attack, whoever sends it.
    (doseq [princ [nil {} {:sub alice} {:sub bob}]
            topic [web/simmis-scope
                   :branching/event
                   :user-rooms/dirty
                   (keyword "notify" (str alice))
                   (keyword "runs" (str alice))
                   (random-uuid)
                   "a-string"
                   nil]]
      (is (false? (boolean (web/data-plane-publish-authorized? princ topic)))
          (str "refused: principal=" (pr-str princ) " topic=" (pr-str topic))))))

(deftest the-publish-gate-does-not-consult-can?
  (testing "refusal is unconditional — a blanket grant does not open it"
    ;; can? stubbed TRUE: if the publish gate ever grows a delegating branch,
    ;; this catches it. Read access must not imply write access on this plane.
    (let [reached (atom false)]
      (with-redefs [access/can? (fn [& _] (reset! reached true) true)]
        (is (false? (boolean (web/data-plane-publish-authorized?
                              {:sub alice} (random-uuid)))))
        (is (false? @reached) "no grant lookup — nothing can authorize a publish")))))

(deftest publish-and-subscribe-are-separate-predicates
  (testing "the same (principal, topic) subscribes yes and publishes no"
    ;; The property kabel 0.3.105 made expressible. Before it one predicate
    ;; answered both, so this pair could not differ.
    (let [topic (random-uuid)
          princ (principal alice)]
      (with-redefs [access/can? (fn [& _] true)]
        (is (true? (boolean (web/data-plane-authorized? princ topic)))
            "may subscribe — the grant permits reading")
        (is (false? (boolean (web/data-plane-publish-authorized? princ topic)))
            "may not publish — nothing permits writing on this plane")))))
