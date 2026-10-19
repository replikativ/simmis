(ns is.simm.uis.chat-proposal-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.proposal-card :as proposal-card]))

(defn- model [proposal]
  (proposal-card/action-model {:status :open :proposal proposal}))

(deftest retained-evidence-is-review-only-not-a-lost-capability
  (let [fork {:system-type :file-output :may-merge? true}
        action (model {:forks [fork] :diffs []})]
    (is (= :files (:kind (proposal-card/fork-diff-model fork))))
    (is (= "Review only" (:accept-label action)))
    (is (:accept-disabled? action))
    (is (false? (:dismiss-disabled? action)))
    (is (nil? (:capability-live? fork)))
    (is (:accept-disabled? (model {:forks [{:system-type :kb :may-merge? true} fork] :diffs []})))
    (is (false? (:accept-disabled? (model {:forks [{:system-type :kb :may-merge? true}
                                                   (assoc fork :status :dismissed)] :diffs []}))))))

(deftest retained-file-replies-cannot-overwrite-newer-or-hidden-comparisons
  (let [begin proposal-card/begin-file-request apply-result proposal-card/apply-file-result
        old (begin {} "scope" "wiki/a.md" :old)
        current (begin old "scope" "wiki/a.md" :new)
        body {:path "wiki/a.md" :before nil :after "A"}]
    (is (= current (apply-result current "scope" "wiki/a.md" :old body nil)))
    (is (= :ready (get-in (apply-result current "scope" "wiki/a.md" :new body nil)
                          [:file-bodies ["scope" "wiki/a.md"] :status])))
    (is (= {} (apply-result {} "scope" "wiki/a.md" :new body nil)))
    (is (= :error (get-in (apply-result current "scope" "wiki/a.md" :new (assoc body :path "wiki/b.md") nil)
                          [:file-bodies ["scope" "wiki/a.md"] :status])))
    (is (nil? (get-in (apply-result current "scope" "wiki/a.md" :new body "Denied")
                      [:file-bodies ["scope" "wiki/a.md"] :body])))))

(deftest adopted-world-diffs-count-their-actual-substrate
  (let [present proposal-card/fork-diff-model]
    (is (= {:kind :files :label "Run files" :changed 1}
           (present {:system-type :world :files [{:path "out/result.md" :status :added}]
                     :counts {:files-changed 1 :insertions 2 :deletions 0}})))
    (is (= 2 (:changed (present {:system-type :world :files [{:path "a"} {:path "b"}]}))))
    (is (= {:kind :entities :label "Run data" :changed 3}
           (present {:system-type :world :counts {:entities-touched 3}})))
    (is (zero? (:changed (present {:system-type :world :files [] :counts {:files-changed 0}}))))
    (is (zero? (:changed (present {:system-type :world :counts {:entities-touched 0}}))))
    (is (nil? (:changed (present {:system-type :world}))))
    (is (nil? (:changed (present {:system-type :repo}))))
    (is (= :files (:kind (present {:system-type :review-world :files []}))))
    (is (= 1 (:changed (present {:system-type :book :entries [{:id 1}]}))))))

(deftest proposal-verification-outcome-is-stable-and-truthful
  (is (= {:status :error :error "status unavailable"}
         (proposal-card/verification-outcome nil "status unavailable")))
  (is (= {:status :open}
         (proposal-card/verification-outcome :open nil)))
  (is (= {:status :error :proposal {:error "status unavailable"}}
         (proposal-card/outcome->state
          {:status :error :error "status unavailable"})))
  (let [state (proposal-card/outcome->state {:status :open})
        action (proposal-card/action-model state)]
    (is (= :checking (:status state)))
    (is (= "Proposal is open, but has not reached this list yet."
           (:action-error action)))
    (is (:retry? action))
    (is (:accept-disabled? action))))

(deftest proposal-card-state-is-a-pure-canonical-projection
  (testing "terminal and loading states expose no actions"
    (is (= "Checking…"
           (:status-label (proposal-card/action-model {:status :checking}))))
    (is (:accept-disabled?
         (proposal-card/action-model {:status :accepted}))))
  (testing "the diff must be loaded before accepting"
    (let [m (model {:forks []})]
      (is (= "Checking…" (:accept-label m)))
      (is (:accept-disabled? m))
      (is (false? (:request-disabled? m)))))
  (testing "a failed diff is explicit and retryable, not permanent checking"
    (let [m (model {:forks [] :diff-error "replica unavailable"})]
      (is (= "Review unavailable" (:accept-label m)))
      (is (= "Couldn't load changes — replica unavailable" (:action-error m)))
      (is (:retry? m))
      (is (:accept-disabled? m))))
  (testing "a newer diff failure disables an older retained success defensively"
    (let [m (model {:forks []
                    :diffs [{:scope "kb" :branch "work"}]
                    :diff-error "newer request failed"})]
      (is (= "Review unavailable" (:accept-label m)))
      (is (:retry? m))
      (is (:accept-disabled? m))))
  (testing "a fully available proposal is actionable"
    (let [m (model {:diffs []
                    :forks [{:branch "work" :status nil
                             :may-merge? true :capability-live? true}]})]
      (is (= "Accept" (:accept-label m)))
      (is (false? (:accept-disabled? m)))
      (is (false? (:dismiss-disabled? m)))))
  (testing "whole-proposal authority and capability constraints match the inspector"
    (let [unlandable (model {:diffs []
                             :forks [{:status nil :may-merge? false}]})
          unavailable (model {:diffs []
                              :forks [{:status nil :capability-live? false}]})]
      (is (= "Cannot land all" (:accept-label unlandable)))
      (is (:accept-disabled? unlandable))
      (is (false? (:request-disabled? unlandable)))
      (is (= "Unavailable" (:accept-label unavailable)))
      (is (:dismiss-disabled? unavailable))))
  (testing "a conflict warning arms the explicit second press"
    (let [m (model {:diffs [] :forks [] :accept-warned? true})]
      (is (= "Accept anyway" (:accept-label m)))
      (is (:accept-force? m)))))

(deftest proposal-state-participates-in-keyed-item-equality
  (let [message {:entity/uuid #uuid "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"
                 :message/metadata {:object {:kind :proposal :id "p-1"}}}
        checking (proposal-card/attach-state message {:status :checking})
        accepted (proposal-card/attach-state message {:status :accepted})]
    (is (= (:entity/uuid checking) (:entity/uuid accepted))
        "the durable timeline identity stays stable")
    (is (not= checking accepted)
        "an unchanged message invalidates its keyed VNode when Proposal state changes")
    (is (= "Accepted"
           (:status-label
            (proposal-card/action-model
             (get accepted proposal-card/proposal-state-key)))))))

(deftest proposal-load-errors-are-not-presented-as-authorization-facts
  (let [m (proposal-card/action-model
           {:status :error :proposal {:error "temporary fetch failure"}})]
    (is (= "Load failed" (:status-label m)))
    (is (= "temporary fetch failure" (:action-error m)))
    (is (:retry? m))
    (is (:accept-disabled? m))))

(deftest retained-audit-responses-are-exact-and-do-not-repopulate-replaced-state
  (let [pid "closed" request (random-uuid)
        waiting (proposal-card/begin-audit-request {:proposals []} pid request)
        result {:id pid :status :dismissed :forks [{:system-type :file-output}]}
        ready (proposal-card/apply-audit-result waiting pid request result nil)]
    (is (= :ready (get-in ready [:retained-audits pid :state])))
    (is (= result (get-in ready [:retained-audits pid :proposal])))
    (is (= {:proposals []} (proposal-card/apply-audit-result {:proposals []} pid request result nil)))
    (is (= waiting (proposal-card/apply-audit-result waiting pid (random-uuid) result nil)))
    (is (= :unavailable (get-in (proposal-card/apply-audit-result waiting pid request nil nil)
                                [:retained-audits pid :state])))
    (doseq [invalid [(assoc result :id "other") (assoc result :status :open) (assoc result :forks [])]]
      (is (= :error (get-in (proposal-card/apply-audit-result waiting pid request invalid nil)
                            [:retained-audits pid :state]))))))

(deftest overlapping-diff-results-only-apply-the-latest-request
  (let [a #uuid "00000000-0000-0000-0000-00000000000a"
        b #uuid "00000000-0000-0000-0000-00000000000b"
        initial {:id "proposal"}
        awaiting-b (-> initial
                       (proposal-card/begin-diff-request a)
                       (proposal-card/begin-diff-request b))
        b-failed (proposal-card/apply-diff-result
                  awaiting-b b nil "newer request failed")
        stale-a (proposal-card/apply-diff-result
                 b-failed a {:forks [{:branch "stale"}]} nil)]
    ;; B fails first; late success A cannot replace or make its stale diff
    ;; actionable, and derived fields from any prior success are absent.
    (is (= b-failed stale-a))
    (is (= "newer request failed" (:diff-error stale-a)))
    (is (nil? (:diffs stale-a)))
    ;; If A arrives first it is already stale; B still becomes authoritative.
    (let [stale-a-first (proposal-card/apply-diff-result
                         awaiting-b a {:forks [{:branch "stale"}]} nil)
          b-after (proposal-card/apply-diff-result
                   stale-a-first b nil "newer request failed")]
      (is (= awaiting-b stale-a-first))
      (is (= "newer request failed" (:diff-error b-after)))
      (is (nil? (:diffs b-after))))))
