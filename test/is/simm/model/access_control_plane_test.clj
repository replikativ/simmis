(ns is.simm.model.access-control-plane-test
  "The CONTROL plane: `normalize-remote-name`, the `rpc-policy` table, and
   `authorize-remote` — the single gate distributed-scope consults before any
   network-inbound handler runs.

   Until now this had no coverage at all (#71). The data-plane decision (`can?`)
   is exercised in `access-test`; nothing checked the layer that decides WHICH
   question to ask about WHICH resource, which is where a silent typo turns a
   scoped write into an unscoped one — or into a permanent refusal nobody can
   diagnose."
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.model.access :as access]))

(def ^:private normalize @#'access/normalize-remote-name)

;; =============================================================================
;; Name normalization — the keying of the whole table
;; =============================================================================

(deftest normalize-remote-name-reduces-every-registration-shape
  (testing "spindel's defn-spin-remote mangling is stripped, trailing ! kept"
    (is (= "accept-proposal!" (normalize "spin-remote-accept-proposal!-3")))
    (is (= "load-rooms!"      (normalize "is.simm.x/spin-remote-load-rooms!-0")))
    (is (= "load-rooms!"      (normalize 'spin-remote-load-rooms!-12))))
  (testing "block-remote / datahike.kabel prefixes reduce to the bare name"
    (is (= "create-block" (normalize "block-remote/create-block")))
    (is (= "dispatch"     (normalize "datahike.kabel/dispatch"))))
  (testing "an already-bare name is unchanged"
    (is (= "mark-read!" (normalize "mark-read!"))))
  (testing "a name that merely LOOKS mangled is not mis-parsed"
    ;; no trailing -<digits>, so the spin pattern must not match
    (is (= "spin-remote-not-really" (normalize "spin-remote-not-really")))))

;; =============================================================================
;; Deny-by-default
;; =============================================================================

(deftest unpoliced-fn-is-denied
  (testing "a remote fn with no policy entry is refused, not allowed through"
    (is (false? (access/authorize-remote {:sub (str (random-uuid))}
                                         "totally-unknown-fn!" {})))
    (is (false? (access/authorize-remote {:sub (str (random-uuid))}
                                         "spin-remote-never-registered!-1" {})))))

(deftest unauthenticated-principal-is-denied-on-a-policed-fn
  (testing "no :sub means anonymous, and anonymous loses even on :read"
    (is (false? (access/authorize-remote nil "load-rooms!" {:party-id-str (str (random-uuid))})))
    (is (false? (access/authorize-remote {} "load-rooms!" {:party-id-str (str (random-uuid))})))
    (is (false? (access/authorize-remote {:sub nil} "load-rooms!" {})))))

(deftest a-throwing-resource-resolver-denies-and-does-not-escape
  ;; The whole point of the try/catch in `authorize-remote`: a resolver that
  ;; blows up (a uuid reaching d/pull as an entity id, say) must FAIL CLOSED.
  ;; If it escapes instead, distributed-scope reports "Remote invocation error"
  ;; and the caller cannot tell a bug from a refusal — clients that only branch
  ;; on a nil result hang forever.
  (let [calls (atom 0)
        boom (fn [_] (swap! calls inc)
               (throw (ex-info "resolver exploded" {:type :test/boom})))]
    (with-redefs [access/rpc-policy {"kaboom!" {:action :write :resource boom}}]
      (testing "returns false rather than propagating"
        (is (false? (access/authorize-remote {:sub (str (random-uuid))} "kaboom!" {}))))
      (testing "and denial is reached even for an anonymous caller"
        (reset! calls 0)
        (is (false? (access/authorize-remote nil "kaboom!" {}))))
      (testing "the resolver is invoked EXACTLY ONCE per decision"
        ;; The denial log used to call `(resource arg-map)` a second time,
        ;; outside the guard. Measured, not supposed: it did not change the
        ;; verdict (telemere realizes :data on its handler thread) but it threw
        ;; inside the logging path, so `::rpc-denied` lost the `:resource` field
        ;; that tells a policy typo from a genuine refusal.
        (reset! calls 0)
        (access/authorize-remote {:sub (str (random-uuid))} "kaboom!" {})
        (is (= 1 @calls) "resource resolver must not be re-invoked for logging")))))

(deftest a-well-behaved-resolver-is-also-invoked-exactly-once
  (let [calls (atom 0)
        res (fn [_] (swap! calls inc) nil)]   ; nil resource => can? denies
    (with-redefs [access/rpc-policy {"counted!" {:action :read :resource res}}]
      (access/authorize-remote {:sub (str (random-uuid))} "counted!" {})
      (is (= 1 @calls) "resolvers are assumed pure and cheap; call them once"))))

;; =============================================================================
;; Table invariants — the cheap checks that catch a typo before production does
;; =============================================================================

(def ^:private known-actions #{:read :write :merge :admin :grant})

(deftest every-policy-entry-is-well-formed
  (doseq [[fn-name {:keys [action resource]}] access/rpc-policy]
    (testing (str fn-name " has a known action")
      (is (contains? known-actions action)
          (str fn-name " declares unknown action " (pr-str action))))
    (testing (str fn-name " has a callable resource resolver")
      (is (ifn? resource) (str fn-name " :resource is not callable")))))

(deftest every-resolver-survives-a-missing-argument
  ;; A resolver is handed whatever the client sent. A client that omits the key
  ;; (or an arg-key TYPO in the policy itself) must produce a denial, never an
  ;; exception — `can?` denies nil/unresolvable resources by design.
  (doseq [[fn-name {:keys [resource]}] access/rpc-policy]
    (testing (str fn-name " resolver tolerates an empty arg-map")
      (is (nil? (try (resource {}) nil
                     (catch Exception e
                       (str fn-name " threw on empty args: " (.getMessage e)))))))))

(deftest store-preparation-is-read-scoped-to-the-requested-database
  (let [{:keys [action resource]} (get access/rpc-policy "prepare-store!")
        scope (random-uuid)]
    (is (= :read action))
    (is (= (str scope) (resource {:db-scope-str (str scope)})))
    (is (nil? (resource {})) "a missing scope must fail closed in can?")))

(deftest run-controls-are-scoped-to-the-containing-room
  (let [room-id (random-uuid)]
    (doseq [[fn-name action] [["load-room-runs!" :read]
                              ["cancel-room-run!" :write]]]
      (let [policy (get access/rpc-policy fn-name)]
        (is (= action (:action policy)))
        (is (= {:room (str room-id)}
               ((:resource policy) {:room-id-str (str room-id)})))))))

(deftest the-irreversible-verbs-are-where-we-put-them
  ;; Guards against a future edit quietly downgrading these. Landing a branch on
  ;; trunk and deleting a database are the two operations no amount of room
  ;; membership should confer.
  (testing "merging a KB branch onto trunk requires :merge, not :write"
    (is (= :merge (:action (get access/rpc-policy "merge-kb!")))))
  (testing "branching and discarding stay ordinary writes"
    (is (= :write (:action (get access/rpc-policy "branch-kb!"))))
    (is (= :write (:action (get access/rpc-policy "discard-kb-branch!")))))
  (testing "database deletion and agent reconfiguration are admin-only"
    (is (= :admin (:action (get access/rpc-policy "delete-database"))))
    (is (= :admin (:action (get access/rpc-policy "update-agent-config!"))))))
