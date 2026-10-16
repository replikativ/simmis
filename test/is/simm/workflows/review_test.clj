(ns is.simm.workflows.review-test
  "Review escalations become proposals, people's decisions become labels
   (`is.simm.workflows.review`), on real rooms (`review-fixture`) with a
   SCRIPTED reviewer. No model. Who may decide is in
   `is.simm.authority.review-escalations-test`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [dvergr.agent.review :as review]
            [is.simm.ops.proposals :as props]
            [is.simm.workflows.review :as wr]
            [is.simm.workflows.review-fixture :as fx
             :refer [owner local-op texts room-with-world parent-file world-live?
                     wiki-spec scripted reviewed proposals-for escalate]]))

(use-fixtures :once fx/with-home)
(use-fixtures :each fx/in-server-context)

;; =============================================================================
;; Escalation → proposal
;; =============================================================================

(deftest an-escalation-becomes-exactly-one-proposal-with-the-reviewers-notes
  (let [{:keys [room-id room world]} (room-with-world {"docs/a.md" "Source A."} {"wiki/a.md" "# A\n"})
        {rv :review pid :proposal-id} (reviewed room-id world wiki-spec (scripted (atom []) escalate)
                                            :workflow {:id "wiki/maintain" :version "v1"})
        p (props/get-proposal pid)]
    (is (= :escalated (:review/decision rv)))
    (testing "the proposal names the Review, its state and its policy"
      (is (= (:review/id rv) (:proposal/review p)))
      (is (= (:review/state-token rv) (:proposal/review-state p)))
      (is (= (:review/policy rv) (:proposal/review-policy p)))
      (is (= "wiki/maintain" (:proposal/workflow-id p)))
      (is (= room-id (:proposal/room p)))
      (is (= [[:review-world world]] (mapv (juxt :proposal.fork/system-type :proposal.fork/branch)
                                           (:proposal/forks p)))))
    (testing "the reviewer's notes are its first comment, by the reviewer"
      (let [[c :as cs] (:proposal/comments p)]
        (is (= 1 (count cs)))
        (is (= (owner) (:proposal.comment/author c)))
        (is (str/includes? (:proposal.comment/body c) "cites no source"))
        (is (str/includes? (:proposal.comment/body c) "page A may be invented"))))
    (testing "the room's owner is told: the proposal card is posted into the room"
      (is (= :published (:proposal/message-status (props/get-proposal pid)))))
    (testing "idempotent per Review: seen again, nothing new is filed"
      (is (= pid (wr/file-escalation! room-id rv)))
      (is (zero? (wr/sweep!)))
      (wr/await-escalations!)
      (is (= 1 (count (proposals-for (:review/id rv)))))
      (is (= 1 (count (:proposal/comments (props/get-proposal pid))))))
    (testing "nothing has landed"
      (is (nil? (parent-file room "/wiki/a.md")))
      (is (world-live? room world)))))

(deftest a-review-dvergr-runs-on-its-own-is-filed-by-the-store-listener
  ;; no `review-run!`: the Review record alone is the trigger
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n"})
        _ (wr/ensure-listener! room room-id)
        rv (review/review! room (keyword world) wiki-spec :reviewer (scripted (atom []) escalate))]
    (wr/await-escalations!)
    (is (= [(wr/escalation-proposal-id (:review/id rv))] (proposals-for (:review/id rv))))))

(deftest a-failing-check-never-asks-the-reviewer
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n" "schedules.edn" "{:every 1}"})
        calls (atom [])
        {rv :review pid :proposal-id} (reviewed room-id world wiki-spec (scripted calls {:review/verdict :approve}))]
    (is (empty? @calls) "the reviewer is never asked")
    (is (= :escalated (:review/decision rv)))
    (let [body (:proposal.comment/body (first (:proposal/comments (props/get-proposal pid))))]
      (is (str/includes? body "No reviewer was asked"))
      (is (str/includes? body "schedules.edn") "the failed check with its evidence"))
    (is (nil? (parent-file room "/wiki/a.md")))))

;; =============================================================================
;; Deciding
;; =============================================================================

(deftest accepting-merges-the-reviewed-state-and-records-a-label
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n"})
        {rv :review pid :proposal-id} (reviewed room-id world wiki-spec (scripted (atom []) escalate)
                                            :workflow {:id "wiki/maintain" :version "v1"})]
    (is (= :accepted (:status (props/accept-proposal! pid :by (owner) :note "it is faithful"))))
    (is (= "# A\n" (parent-file room "/wiki/a.md")) "the world landed on the room")
    (is (not (world-live? room world)))
    (is (= :accepted (:proposal/status (props/get-proposal pid))))
    (let [[l :as ls] (wr/labels room-id)]
      (is (= 1 (count ls)))
      (is (= {:label/subject [:proposal pid] :label/about :workflow :label/verdict :accept
              :label/by (owner) :label/review (:review/id rv) :label/reasons "it is faithful"
              :label/workflow {:id "wiki/maintain" :version "v1"}
              :label/reviewer-verdict :escalate :label/policy (:review/policy rv)}
             (dissoc l :label/id :label/at))))))

(deftest rejecting-discards-the-world-and-records-a-label
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n"})
        {pid :proposal-id} (reviewed room-id world wiki-spec (scripted (atom []) escalate))]
    (is (= :dismissed (:status (props/dismiss-proposal! pid :by (owner) :note "invented"))))
    (is (not (world-live? room world)) "discarded")
    (is (nil? (parent-file room "/wiki/a.md")))
    (is (= [[:workflow :reject "invented"]]
           (mapv (juxt :label/about :label/verdict :label/reasons) (wr/labels room-id))))))

(deftest a-world-changed-after-review-is-refused-not-merged
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n"})
        {pid :proposal-id} (reviewed room-id world wiki-spec (scripted (atom []) escalate))]
    ;; the world moves after it was reviewed
    (local-op room :room/write {:room world :files (texts {"wiki/b.md" "# B, unreviewed\n"})})
    (let [e (try (props/accept-proposal! pid :by (owner)) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :world-changed (:error (ex-data e))))
      (is (str/includes? (ex-message e) "changed after it was reviewed")))
    (is (nil? (parent-file room "/wiki/a.md")) "nothing merged")
    (is (nil? (parent-file room "/wiki/b.md")))
    (is (= :open (:proposal/status (props/get-proposal pid))) "still the owner's to reject")
    (is (empty? (wr/labels room-id)) "no decision, no label")))

(deftest an-approval-merges-without-a-proposal
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n"})
        {rv :review pid :proposal-id} (reviewed room-id world wiki-spec
                                            (scripted (atom []) {:review/verdict :approve}))]
    (is (= :merged (:review/decision rv)))
    (is (nil? pid))
    (is (empty? (proposals-for (:review/id rv))))
    (is (= "# A\n" (parent-file room "/wiki/a.md")))))

(deftest a-spot-checked-approval-is-a-proposal-whose-decision-labels-the-reviewer
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n"})
        {rv :review pid :proposal-id}
        (reviewed room-id world (assoc wiki-spec :review/spot-check {:rate 1.0 :seed 1})
              (scripted (atom []) {:review/verdict :approve :review/reasons ["faithful"]}))
        p (props/get-proposal pid)]
    (is (:review/spot-check? rv))
    (is (true? (:proposal/review-spot-check? p)))
    (is (str/starts-with? (:proposal/title p) "Spot check"))
    (is (nil? (parent-file room "/wiki/a.md")) "sampled, so not merged yet")
    (testing "agreeing merges it and labels both the work and the reviewer"
      (props/accept-proposal! pid :by (owner))
      (is (= "# A\n" (parent-file room "/wiki/a.md")))
      (is (= #{[:workflow :accept :approve] [:reviewer :accept :approve]}
             (set (map (juxt :label/about :label/verdict :label/reviewer-verdict)
                       (wr/labels room-id)))))
      (is (= 1 (count (wr/labels room-id :about :reviewer)))))))

(deftest a-disagreed-spot-check-labels-the-reviewer-wrong
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n"})
        {pid :proposal-id} (reviewed room-id world (assoc wiki-spec :review/spot-check {:rate 1.0 :seed 1})
                                 (scripted (atom []) {:review/verdict :approve}))]
    (props/dismiss-proposal! pid :by (owner) :note "the reviewer missed an invented number")
    (is (nil? (parent-file room "/wiki/a.md")))
    (is (= [:reject] (mapv :label/verdict (wr/labels room-id :about :reviewer))))))
