(ns is.simm.authority.review-escalations-test
  "Who may decide an escalated review's proposal, through the RPCs a browser
   sends (`world/inbound!`) and through dvergr's ops directly: `:merge` on the
   room, i.e. its owner. Each attack is paired with the legitimate use, on real
   rooms (`is.simm.workflows.review-fixture`)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [is.simm.authority.world :as world]
            [is.simm.model.access :as access]
            [is.simm.ops.dvergr :as sops]
            [is.simm.ops.proposals :as props]
            [is.simm.uis.web.desktop.proposals-remote]
            [is.simm.workflows.review :as wr]
            [is.simm.workflows.review-fixture :as fx
             :refer [member room-with-world parent-file world-live?
                     wiki-spec scripted reviewed escalate]]))

(use-fixtures :once fx/with-home)
(use-fixtures :each fx/in-server-context)

;; =============================================================================
;; Authority
;; =============================================================================

(deftest a-member-cannot-decide-a-review-the-owner-can
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n"})
        {pid :proposal-id} (reviewed room-id world wiki-spec (scripted (atom []) escalate))
        rpc (fn [kind fn-name args] (world/inbound! {:sub (str (get @fx/env kind))} fn-name args))
        args {:proposal-id-str (str pid) :force? false :note nil}]
    (testing "attack: the member accepts through the RPC — refused, the world stays unmerged"
      (let [resp (rpc :member "accept-proposal!" args)]
        (is (:error resp))
        (is (str/includes? (str (:error resp)) "only someone who may land")))
      (is (nil? (parent-file room "/wiki/a.md")))
      (is (world-live? room world))
      (is (= :open (:proposal/status (props/get-proposal pid))))
      (is (empty? (wr/labels room-id))))
    (testing "attack: the member rejects — refused at the gate (not their fork to discard)"
      (is (world/refused? (rpc :member "dismiss-proposal!" {:proposal-id-str (str pid) :note nil})))
      (is (world-live? room world)))
    (testing "attack: the member calls dvergr's merge directly — refused by simmis's authorizer"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not authorized"
                            (sops/invoke (:ctx room) (sops/party-principal (member)) :room/merge
                                         {:room world})))
      (is (nil? (parent-file room "/wiki/a.md"))))
    (testing "the member sees the notes and may read the labels; the card offers no Accept"
      (let [{:keys [result]} (rpc :member "list-proposals!" {:status-kw "open"})
            card (first (filter #(= (str pid) (:id %)) result))]
        (is (some? (:review card)))
        (is (= ["cites no source"] (get-in card [:review :reviewer-reasons])))
        (is (false? (:may-merge? (first (:forks card))))))
      (is (= [] (:result (rpc :member "list-labels!" {:room-id-str (str room-id)})))))
    (testing "legitimate: the owner accepts through the RPC"
      (is (= :accepted (get-in (rpc :owner "accept-proposal!" args) [:result :status])))
      (is (= "# A\n" (parent-file room "/wiki/a.md")))
      (is (= [:accept] (mapv :verdict (:result (rpc :member "list-labels!" {:room-id-str (str room-id)}))))))))

(deftest a-reviewer-without-merge-rights-cannot-auto-merge
  ;; the review merges AS its party; a member holds no :merge on the room, so
  ;; the approval escalates with the refusal and nothing lands
  (let [{:keys [room-id room world]} (room-with-world {} {"wiki/a.md" "# A\n"})
        {rv :review pid :proposal-id} (reviewed room-id world wiki-spec
                                            (scripted (atom []) {:review/verdict :approve})
                                            :reviewer-party (member))]
    (is (= :escalated (:review/decision rv)))
    (is (some #(str/includes? % "not authorized") (:review/reasons rv)))
    (is (nil? (parent-file room "/wiki/a.md")))
    (is (some? pid) "a person decides it instead")))

(deftest labels-are-append-only
  (testing "no RPC writes or removes a label"
    (is (= ["list-labels!"] (filterv #(re-find #"label" %) (keys access/rpc-policy))))
    (is (= :read (get-in access/rpc-policy ["list-labels!" :action])))))
