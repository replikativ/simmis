(ns is.simm.authority.control-plane-matrix-test
  "The control plane's authority matrix: every `rpc-policy` row × every
   principal kind, decided by the REAL `authorize-remote` → `can?` over a real
   system DB (`authority.world`), not a stub.

   `matrix` is the reviewed expectation, keyed exactly like `rpc-policy`.
   `the-matrix-covers-the-policy-table` fails when a row is added without an
   entry (or an entry outlives its row), so a new RPC cannot ship without
   somebody writing down who may call it.

   An entry is a vector of CASES. A case names a TARGET (which resource of the
   world the arguments point at — `target-args`) and `:allow`, the principals
   that SHOULD pass. Where today's answer is wider than `:allow`, the case also
   names `:today` and a `:hole`: the extra principals are asserted through
   `known-hole`, so the case passes until the hole is closed and then demands to
   be flipped. Every principal outside `:today` is asserted refused and every
   principal in `:allow` asserted admitted — each row carries both the attack
   and its legitimate counterpart."
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [is.simm.authority.known-hole :refer [known-hole]]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]))

;; =============================================================================
;; Targets: which resource of the world the arguments name
;; =============================================================================

(defn target-args
  "The capture-var arg map pointing every key a resolver might read at the
   target. Keys a row does not read are ignored by it; a row reading a key this
   omits resolves nil and denies everyone — which the `:allow` side catches."
  [w target]
  (let [s #(str (get w %))]
    (case target
      :none        {}
      :room        {:room-id-str (s :room) :room-uuid-str (s :room)}
      :kb-scope    {:db-scope (:kb-granted-scope w) :db-scope-str (s :kb-granted-scope)
                    :scope-str (s :kb-granted-scope)}
      :kb          {:kb-id-str (s :kb-granted)}
      :proposal    {:proposal-id-str (s :proposal)}
      ;; one open fork of that proposal, written by the member / by the agent
      :member-fork {:proposal-id-str (s :proposal) :scope-str (s :kb-granted-scope)
                    :branch "member-fork"}
      :agent-fork  {:proposal-id-str (s :proposal) :scope-str (s :kb-granted-scope)
                    :branch "agent-fork"}
      ;; a room and the Handbook / the owner's drive in it (detach)
      :room-kb     {:room-id-str (s :room) :kb-id-str (s :kb-granted)
                    :drive-id-str (s :drive-owner)}
      ;; the stranger's room and the OWNER's Handbook / drive (the resource's
      ;; owner withdrawing it from a room they are not in)
      :foreign-room-kb {:room-id-str (s :other-room) :kb-id-str (s :kb-granted)
                        :drive-id-str (s :drive-owner)}
      :mail        {:account-id-str (s :mail)}
      ;; the team room's own MESSAGE store, under the key a KB write names its
      ;; store with: what a raw transaction must never be able to address
      :room-store  {:db-scope (:room-scope w) :db-scope-str (s :room-scope)
                    :scope-str (s :room-scope)}
      ;; the owner's own id, in every key a self/settings row reads: the
      ;; owner passing it is the legitimate call, anyone else passing it is
      ;; impersonation
      :owner-self  {:party-id-str (s :owner) :creator-id-str (s :owner)})))

(def ^:private targets
  #{:none :room :kb-scope :kb :proposal :mail :owner-self :room-store
    :member-fork :agent-fork :room-kb :foreign-room-kb})

(def ^:private AUTH #{:owner :member :non-member :admin})
(def ^:private MEMBERS #{:owner :member})
(def ^:private OWNER #{:owner})
(def ^:private ADMIN #{:admin})

(defn- c
  ([target allow] {:target target :allow allow})
  ([target allow today hole]
   {:target target :allow allow :today today :hole hole}))

(def matrix
  "fn name → [case …]. See the ns doc."
  {;; datahike.kabel's `dispatch`/`create-database`/`delete-database` have no
   ;; row and no entry: unreachable, and refused by the gate if they were
   ;; (raw-paths-test). Rows for `save-schedule!` and
   ;; `get-room-context-stats!` went too — no such fn is registered.
   ;; --- block-remote, on a KB the team room holds :read-write -----------------
   "create-block"            [(c :kb-scope MEMBERS)]
   "create-sibling-after"    [(c :kb-scope MEMBERS)]
   "delete-block"            [(c :kb-scope MEMBERS)]
   "ensure-page"             [(c :kb-scope MEMBERS)]
   "indent-block"            [(c :kb-scope MEMBERS)]
   "outdent-block"           [(c :kb-scope MEMBERS)]
   "move-block-up"           [(c :kb-scope MEMBERS)]
   "move-block-down"         [(c :kb-scope MEMBERS)]
   "rename-page"             [(c :kb-scope MEMBERS)]
   "toggle-block-collapsed"  [(c :kb-scope MEMBERS)]
   "update-block-collapsed"  [(c :kb-scope MEMBERS)]
   "update-block-content"    [(c :kb-scope MEMBERS)]
   "update-block-order"      [(c :kb-scope MEMBERS)]
   "find-page-by-title"      [(c :kb-scope MEMBERS)]
   "prepare-store!"          [(c :kb-scope MEMBERS)]
   "add-type"                [(c :kb-scope MEMBERS)]
   "remove-type"             [(c :kb-scope MEMBERS)]
   "add-property"            [(c :kb-scope MEMBERS)]
   "remove-property"         [(c :kb-scope MEMBERS)]
   "save-property"           [(c :kb-scope MEMBERS)]
   ;; a wiki widget's raw transactions: DATA is a write on the KB; DECLARING an
   ;; attribute is append-only schema global to the KB, so it is the owner's
   ;; (:merge — the read-write grantee may not). Neither reaches a room's own
   ;; store, whoever asks: `{:kb-store …}` answers only for a KB. Attack tests
   ;; with their legitimate twins: widget-writes-test.
   "widget-transact"         [(c :kb-scope MEMBERS) (c :room-store #{})]
   "widget-install-attr"     [(c :kb-scope OWNER) (c :room-store #{})]
   ;; --- branching --------------------------------------------------------------
   "branch-kb!"              [(c :kb-scope MEMBERS)]
   ;; on trunk / a plain branch; an open proposal fork narrows this to its
   ;; author and deciders (handler-holes-test)
   "discard-kb-branch!"      [(c :kb-scope MEMBERS)]
   ;; the grantee may write a fork but not land it
   "merge-kb!"               [(c :kb-scope OWNER)]
   "kb-commit-graph!"        [(c :kb-scope MEMBERS)]
   "kb-diff!"                [(c :kb-scope MEMBERS)]
   "list-kb-branches!"       [(c :kb-scope MEMBERS)]
   ;; --- KB by id ---------------------------------------------------------------
   "create-kb!"              [(c :owner-self OWNER)]
   "load-kb-details!"        [(c :kb MEMBERS)]
   ;; Deleting a KB and handing out direct access to it are :grant-level
   ;; (doc/authority.md: \"handing out authority\" stops at the owner, plus
   ;; the :admin role's authority override). A :read-write grantee used to
   ;; pass all three; `share-kb!` to oneself then made them owner-equivalent.
   "delete-kb!"              [(c :kb #{:owner :admin})]
   "share-kb!"               [(c :kb #{:owner :admin})]
   "unshare-kb!"             [(c :kb #{:owner :admin})]
   ;; --- room ops ---------------------------------------------------------------
   "add-agent-to-room!"      [(c :room MEMBERS)]
   "update-agent-assignment!" [(c :room MEMBERS)]
   "remove-agent-from-room!" [(c :room MEMBERS)]
   ;; Membership IS authority over the room's grants, so changing it is
   ;; :grant — which stops at the owner (doc/authority.md, can?'s B1 rule).
   "add-room-party!"         [(c :room OWNER)]
   "remove-room-party!"      [(c :room OWNER)]
   "attach-drive-to-room!"   [(c :room MEMBERS)]
   ;; detaching: the room's owner, or the resource's owner (or :admin) — a
   ;; plain member may no longer take the room's KBs and drives away
   "detach-drive-from-room!" [(c :room OWNER) (c :room-kb #{:owner :admin})
                              (c :foreign-room-kb #{:owner :non-member :admin})]
   "attach-kb-to-room!"      [(c :room MEMBERS)]
   "detach-kb-from-room!"    [(c :room OWNER) (c :room-kb #{:owner :admin})
                              (c :foreign-room-kb #{:owner :non-member :admin})]
   "delete-drive-node!"      [(c :room MEMBERS)]
   "mkdir-drive-path!"       [(c :room MEMBERS)]
   "transcribe-drive-file!"  [(c :room MEMBERS)]
   "put-drive-file!"         [(c :room MEMBERS)]
   "dispatch-message!"       [(c :room MEMBERS)]
   "ensure-room!"            [(c :room MEMBERS)]
   "run-world-live!"         [(c :room MEMBERS)]
   "load-attempt-files!"     [(c :room MEMBERS)]
   "load-attempt-file!"      [(c :room MEMBERS)]
   "load-workflow-list!"     [(c :room MEMBERS)]
   "load-workflow-source!"   [(c :room MEMBERS)]
   "save-workflow-source!"   [(c :room MEMBERS)]
   "load-memory-folder!"     [(c :room MEMBERS)]
   "prepare-memory-stream!" [(c :room MEMBERS)]
   "load-memory-metadata!"  [(c :room MEMBERS)]
   "load-memory-file!"       [(c :room MEMBERS)]
   "load-memory-properties!" [(c :room MEMBERS)]
   "save-memory-property!"   [(c :room MEMBERS)]
   "preview-memory-declaration!" [(c :room MEMBERS)]
   "save-memory-declaration!" [(c :room MEMBERS)]
   "load-memory-history!"    [(c :room MEMBERS)]
   "load-memory-history-file!" [(c :room MEMBERS)]
   "evaluate-memory-code!"   [(c :room OWNER)]
   "evaluate-memory-repl!"   [(c :room OWNER)]
   "search-memory-files!"    [(c :room MEMBERS)]
   "analyze-memory-text!"    [(c :room MEMBERS)]
   "apply-memory-rich-edit!" [(c :room MEMBERS)]
   "download-memory-file!"   [(c :room MEMBERS)]
   "save-memory-file!"       [(c :room MEMBERS)]
   "save-memory-rich-file!"  [(c :room MEMBERS)]
   "move-memory-file!"       [(c :room MEMBERS)]
   "load-memory-office-part!" [(c :room MEMBERS)]
   "save-memory-office-part!" [(c :room MEMBERS)]
   "submit-workflow-run!"    [(c :room OWNER)]
   "collect-workflow-wiki-output!" [(c :room OWNER)]
   "inspect-wiki-maintenance-setup!" [(c :room OWNER)]
   "install-wiki-maintenance!" [(c :room OWNER)]
   "create-workflow-schedule!" [(c :room OWNER)]
   "set-workflow-schedule-enabled!" [(c :room OWNER)]
   "submit-workflow-benchmark!" [(c :room OWNER)]
   "submit-cli-workflow-benchmark!" [(c :room OWNER)]
   "provision-cli-workflow-allowance!" [(c :room OWNER)]
   "provision-workflow-allowance!" [(c :room OWNER)]
   "load-local-workflow-workers!" [(c :room OWNER)]
   "submit-local-workflow-run!" [(c :room OWNER)]
   "submit-local-workflow-benchmark!" [(c :room OWNER)]
   "provision-local-workflow-allowance!" [(c :room OWNER)]
   ;; the gate admits members; WHICH member may cancel WHICH run is the
   ;; handler's (§2.9.2) — see runs-and-jobs-test
   "cancel-room-run!"        [(c :room MEMBERS)]
   ;; asking whether one may stop a Run is reading the room; the answer is
   ;; per Run and per caller — see runs-and-jobs-test
   "can-cancel-room-run!"    [(c :room MEMBERS)]
   "promote-room-run-world!" [(c :room MEMBERS)]
   ;; Spend is egress (doc/authority.md, problem 4): authorized in advance by
   ;; whoever pays, not raised by anyone who can speak in the room.
   "update-room-budget!"     [(c :room OWNER)]
   "load-room-details!"      [(c :room MEMBERS)]
   "mint-video-token!"       [(c :room MEMBERS)]
   "room-app-status!"        [(c :room MEMBERS)]
   "search-screens!"         [(c :none AUTH)]
   "delete-screenshot!"      [(c :none AUTH)]
   "list-recordings!"        [(c :none AUTH)]
   "delete-recording!"       [(c :none AUTH)]
   "search-pages!"           [(c :none AUTH)]
   "delete-page!"            [(c :none AUTH)]
   "open-screen-grant!"      [(c :room MEMBERS)]
   "close-screen-grant!"     [(c :room MEMBERS)]
   "screen-grant-heartbeat!" [(c :room MEMBERS)]
   "load-room-drive!"        [(c :room MEMBERS)]
   ;; --- self ---------------------------------------------------------------------
   "create-room!"            [(c :owner-self OWNER)]
   "load-rooms!"             [(c :owner-self OWNER)]
   "create-drive!"           [(c :none AUTH)]
   "add-contact!"            [(c :owner-self OWNER)]
   "add-contact-by-handle!"  [(c :owner-self OWNER)]
   "remove-contact!"         [(c :owner-self OWNER)]
   "list-parties!"           [(c :none AUTH)]
   "update-agent-config!"    [(c :none ADMIN)]
   ;; --- sandbox --------------------------------------------------------------
   ;; §2.7: eval in a room is FULL room capability, so owner/:merge level.
   "eval-in-room!"           [(c :room OWNER)]
   ;; answers false rather than refusing, so the UI can hide the control
   "can-eval-in-room!"       [(c :none AUTH)]
   "get-room-state!"         [(c :room MEMBERS)]
   ;; --- proposals ------------------------------------------------------------
   "accept-proposal!"        [(c :proposal MEMBERS)]
   ;; every open fork must be the caller's (author) or decidable (:merge):
   ;; the member wrote one of the two, the owner may merge both
   "dismiss-proposal!"       [(c :proposal OWNER)]
   "accept-fork!"            [(c :proposal MEMBERS)]
   "dismiss-fork!"           [(c :member-fork MEMBERS) (c :agent-fork OWNER)]
   "comment-on-proposal!"    [(c :proposal MEMBERS)]
   "request-changes!"        [(c :proposal MEMBERS)]
   "proposal-status!"        [(c :none AUTH)]
   ;; The audit helper returns nil for invisible or absent proposals, then
   ;; rechecks room access and exact retained ownership before reading data.
   "proposal-retained-audit!" [(c :none AUTH)]
   "proposal-diff!"          [(c :proposal MEMBERS)]
   "proposal-file!"          [(c :proposal MEMBERS)]
   "preview-file-proposal!"  [(c :proposal MEMBERS)]
   "adopt-reviewed-files!"   [(c :proposal MEMBERS)]
   "reconcile-file-proposal!" [(c :proposal MEMBERS)]
   "proposal-checks!"        [(c :proposal MEMBERS)]
   "list-proposals!"         [(c :none AUTH)]
   ;; labels: what the room's people decided on reviewed work — a room read
   "list-labels!"            [(c :room MEMBERS)]
   ;; --- tasks / book / feed --------------------------------------------------
   "list-tasks!"             [(c :none AUTH)]
   "load-position!"          [(c :none AUTH)]
   "load-book!"              [(c :room MEMBERS)]
   "load-account-ledger!"    [(c :room MEMBERS)]
   "load-feed!"              [(c :none AUTH)]
   "set-task-status!"        [(c :kb-scope MEMBERS)]
   ;; --- settings (own) -------------------------------------------------------
   "load-settings!"          [(c :owner-self OWNER)]
   "save-env-var!"           [(c :owner-self OWNER)]
   "delete-env-var!"         [(c :owner-self OWNER)]
   "save-preferred-model!"   [(c :owner-self OWNER)]
   "save-syntax-pref!"       [(c :owner-self OWNER)]
   ;; --- mail -----------------------------------------------------------------
   "load-mail-accounts!"     [(c :none AUTH)]
   "test-mail-connection!"   [(c :none AUTH)]
   "save-mail-account!"      [(c :none AUTH)]
   "sync-mail-account!"      [(c :mail OWNER)]
   "load-mail-folders!"      [(c :mail OWNER)]
   "load-mail-page!"         [(c :mail OWNER)]
   "load-mail-message!"      [(c :mail OWNER)]
   ;; --- admin / misc ---------------------------------------------------------
   "load-admin-data!"        [(c :none ADMIN)]
   "load-schedules!"         [(c :none AUTH)]
   "prepare-workflow-metadata-stream!" [(c :none AUTH)]
   "load-unread-counts!"     [(c :none AUTH)]
   "mark-read!"              [(c :none AUTH)]
   "load-notify-prefs!"      [(c :none AUTH)]
   "set-notify-pref!"        [(c :none AUTH)]
   ;; --- experiments ----------------------------------------------------------
   "load-leaderboard!"       [(c :room MEMBERS)]
   "join-experiment-room!"   [(c :none ADMIN)]})

;; =============================================================================
;; Coverage — the table and the matrix name the same rows
;; =============================================================================

(deftest the-matrix-covers-the-policy-table
  (let [policy (set (keys access/rpc-policy))
        rows (set (keys matrix))]
    (testing "every rpc-policy row has a matrix entry"
      (is (= #{} (set/difference policy rows))
          "a new rpc-policy row needs a reviewed entry in `matrix`: who may call it?"))
    (testing "every matrix entry names a live rpc-policy row"
      (is (= #{} (set/difference rows policy))
          "a matrix entry for a row that no longer exists"))
    (testing "the table is the size we think it is"
      (is (< 100 (count policy))))))

(deftest every-entry-is-well-formed
  (doseq [[fn-name cases] matrix
          {:keys [target allow today hole]} cases]
    (testing fn-name
      (is (contains? targets target) (str fn-name " unknown target " target))
      (is (not (contains? allow :anon)) "nothing is open to an anonymous caller")
      (is (not (contains? (or today #{}) :anon)))
      (when today
        (is (string? hole) "a widened case names its hole")
        (is (set/subset? allow today) "a hole only ever WIDENS the answer")
        (is (seq (set/difference today allow)) "a hole names at least one attacker"))))
  (testing "every row has a legitimate caller in at least one case"
    (doseq [[fn-name cases] matrix]
      (is (some (comp seq :allow) cases)
          (str fn-name " admits nobody — the matrix must show who CAN call it")))))

;; =============================================================================
;; The matrix itself
;; =============================================================================

(defn- decide [w kind fn-name args]
  (boolean (access/authorize-remote (world/principal w kind) fn-name args)))

(deftest every-row-decides-as-the-matrix-says
  (world/with-world [_ w]
    (doseq [[fn-name cases] (sort-by key matrix)
            {:keys [target allow today hole]} cases
            :let [args (target-args w target)
                  today (or today allow)]
            kind world/principal-kinds]
      (testing (str fn-name " on " target " as " kind)
        (cond
          (contains? allow kind)
          (is (true? (decide w kind fn-name args))
              (str fn-name ": legitimate caller " kind " refused on " target))

          (contains? today kind)
          (known-hole hole (str fn-name " admits " (name kind) " on " (name target))
                      (decide w kind fn-name args))

          :else
          (is (false? (decide w kind fn-name args))
              (str fn-name ": " kind " admitted on " target)))))))

(deftest an-id-in-the-arguments-is-not-an-identity
  ;; The impersonation half of every self/settings row, spelled out: the
  ;; member passing THEIR OWN id is admitted, passing the owner's is not.
  (world/with-world [_ w]
    (doseq [[fn-name cases] (sort-by key matrix)
            :when (some #(= :owner-self (:target %)) cases)]
      (testing fn-name
        (is (true? (decide w :member fn-name {:party-id-str (str (:member w))
                                              :creator-id-str (str (:member w))}))
            "the member acting as themselves")
        (is (false? (decide w :member fn-name (target-args w :owner-self)))
            "the member naming the owner's id")))))

(deftest a-row-asked-about-the-wrong-room-refuses
  ;; Cross-tenant: the stranger is a member of their OWN room, and that buys
  ;; nothing in the team room; the member's membership buys nothing there.
  (world/with-world [_ w]
    (doseq [[fn-name cases] (sort-by key matrix)
            :when (some #(= :room (:target %)) cases)]
      (testing fn-name
        (is (false? (decide w :member fn-name {:room-id-str (str (:other-room w))
                                               :room-uuid-str (str (:other-room w))})))
        (is (true? (decide w :non-member fn-name {:room-id-str (str (:other-room w))
                                                  :room-uuid-str (str (:other-room w))}))
            "the stranger in their own room")))))
