(ns is.simm.uis.block-sync-test
  "An open wiki editor following the replica (`block-sync`): every block takes
   a change made elsewhere except the one being edited, which keeps its text
   and records a conflict — and a pending prediction of this session's own
   write is never overwritten by an older replica value.

   The rules are pure; the ordering half runs them against a real datahike
   conn and a real `datahike.optimistic` overlay, which is where the editor's
   `data-content` comes from (the effective db: replica + predictions)."
  (:require [clojure.core.async :as a :refer [<!! >!! go]]
            [clojure.test :refer [deftest is testing]]
            [datahike.api :as d]
            [datahike.optimistic :as opt]
            [is.simm.model.schema :as schema]
            [is.simm.model.seed :as seed]
            [is.simm.uis.web.desktop.block-sync :as bs]
            [is.simm.uis.web.desktop.optimistic-write :as ow]))

;; =============================================================================
;; The rules
;; =============================================================================

(def ^:private taken (bs/mounted "<p>a</p>" "<p>a</p>"))

(deftest a-block-nobody-is-editing-takes-the-change
  (is (= [:apply taken] (bs/incoming taken "<p>b</p>" "<p>a</p>" false))))

(deftest the-focused-block-keeps-its-text
  (testing "typing, and the db changes underneath: keep it, record a conflict"
    (is (= [:conflict (assoc taken :conflict "<p>b</p>")]
           (bs/incoming taken "<p>b</p>" "<p>a, typed</p>" true))))
  (testing "focused without typing: still not changed under the cursor"
    (is (= :conflict (first (bs/incoming taken "<p>b</p>" "<p>a</p>" true)))))
  (testing "typed and left without a save (arrow navigation skips it): kept too"
    (is (= :conflict (first (bs/incoming taken "<p>b</p>" "<p>a, typed</p>" false)))))
  (testing "a later change replaces the recorded one"
    (let [[_ s] (bs/incoming taken "<p>b</p>" "<p>a, typed</p>" true)]
      (is (= "<p>c</p>" (:conflict (second (bs/incoming s "<p>c</p>" "<p>a, typed</p>" true))))))))

(deftest nothing-happens-when-nothing-changed
  (testing "the block's db content is what the editor took (other blocks changed)"
    (is (= [:none taken] (bs/incoming taken "<p>a</p>" "<p>a, typed</p>" true))))
  (testing "the db now holds what the editor shows — our own save landed"
    (is (= [:none (bs/mounted "<p>a, typed</p>" "<p>a, typed</p>")]
           (bs/incoming taken "<p>a, typed</p>" "<p>a, typed</p>" true))))
  (testing "a conflict whose change was reverted resolves itself"
    (let [[_ s] (bs/incoming taken "<p>b</p>" "<p>a, typed</p>" true)]
      (is (= [:none taken] (bs/incoming s "<p>a</p>" "<p>a, typed</p>" true))))))

(deftest stored-and-shown-forms-differ
  ;; TipTap normalises what it is given: an agent's plain [[Page]] text shows
  ;; as a reference mark. Comparing the shown HTML to the stored content would
  ;; call every such block typed-in and never let it take a change.
  (let [s (bs/mounted "<p>[[Home]]</p>" "<p><span class=\"page-reference\">[[Home]]</span></p>")]
    (is (not (bs/dirty? s "<p><span class=\"page-reference\">[[Home]]</span></p>")))
    (is (= :apply (first (bs/incoming s "<p>[[Home]] and more</p>"
                                      "<p><span class=\"page-reference\">[[Home]]</span></p>"
                                      false))))))

(deftest leaving-the-block
  (testing "typed: save the local text, over a conflict too"
    (is (= :save (bs/leaving taken "<p>a, typed</p>")))
    (is (= :save (bs/leaving (assoc taken :conflict "<p>b</p>") "<p>a, typed</p>"))))
  (testing "nothing typed, the db changed while focused: take it now"
    (is (= :apply (bs/leaving (assoc taken :conflict "<p>b</p>") "<p>a</p>"))))
  (testing "nothing typed, nothing changed: no write — a click in and out must
            not put the old text back over someone else's"
    (is (= :none (bs/leaving taken "<p>a</p>")))))

;; =============================================================================
;; Predictions against the replica
;; =============================================================================

(def ^:private page-uuid #uuid "eeeeeeee-0000-0000-0000-0000000000a1")
(def ^:private block-uuid #uuid "eeeeeeee-0000-0000-0000-0000000000a2")

(defn- fresh-kb []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (d/transact conn schema/full-schema)
      (seed/ensure-seed-data! conn)
      (d/transact conn [{:entity/uuid page-uuid
                         :instance/of-role [:entity/name "S/Page"]
                         :S.Page/title "Sync"}])
      (d/transact conn [{:entity/uuid block-uuid
                         :instance/of-role [:entity/name "S/Block"]
                         :block/content "<p>a</p>"
                         :block/order "a0"
                         :block/parent [:entity/uuid page-uuid]}])
      conn)))

(defn- content [db] (:block/content (d/entity db [:entity/uuid block-uuid])))

(defn- await-pending
  "Wait for the overlay to hold `n` entries; true when it did within 10s. Driven
   by the overlay's own status events, not by sleeping."
  [overlay n]
  (let [ch (a/chan 64)]
    (opt/listen-status! overlay ::await (fn [e] (a/put! ch e)))
    (let [deadline (a/timeout 10000)]
      (loop []
        (if (= n (count (:entries (:model @(:state overlay)))))
          true
          (let [[_ port] (a/alts!! [ch deadline])]
            (if (= port deadline) false (recur))))))))

(deftest a-pending-prediction-is-not-overwritten-by-an-older-replica-value
  (let [conn (fresh-kb)
        overlay (opt/open conn)
        editor (atom (bs/mounted "<p>a</p>" "<p>a</p>"))
        ;; the editor, fed the effective db's content as the observer feeds it
        follow! (fn [focused?]
                  (let [[action s] (bs/incoming @editor (content (opt/db overlay))
                                                (:shown @editor (:clean @editor)) focused?)]
                    (reset! editor (cond-> s (= :apply action)
                                     (merge (bs/mounted (content (opt/db overlay))
                                                        (content (opt/db overlay))))))
                    action))
        release (a/promise-chan)
        result (ow/predict-remote!
                overlay [{:entity/uuid block-uuid :block/content "<p>mine</p>"}]
                (fn [] (go (a/<! release)
                           (d/transact conn [{:entity/uuid block-uuid :block/content "<p>mine</p>"}])
                           {:block-uuid block-uuid :max-tx (:max-tx @conn)}))
                {})]
    ;; the user typed "mine" and left the block: the editor shows it
    (swap! editor assoc :shown "<p>mine</p>")
    (testing "the prediction shows at once, and the editor agrees with it"
      (is (= "<p>mine</p>" (content (opt/db overlay))))
      (is (= :none (follow! false)))
      (is (= "<p>mine</p>" (:applied @editor))))
    (testing "another session's OLDER write syncs in first: the view keeps the prediction"
      (d/transact conn [{:entity/uuid block-uuid :block/content "<p>theirs, earlier</p>"}])
      (is (= "<p>theirs, earlier</p>" (content @conn)) "the replica has it")
      (is (= "<p>mine</p>" (content (opt/db overlay))) "the effective db does not")
      (is (= :none (follow! false)) "so the editor is not overwritten, and nothing conflicts")
      (is (nil? (:conflict @editor))))
    (testing "our write lands after it: the prediction drops, the content stays ours"
      (>!! release true)
      (is (some? (<!! result)))
      (is (await-pending overlay 0) "nothing left pending")
      (is (= "<p>mine</p>" (content @conn) (content (opt/db overlay))))
      (is (= :none (follow! false))))
    (testing "a NEWER write from elsewhere then reaches the editor"
      (d/transact conn [{:entity/uuid block-uuid :block/content "<p>theirs, later</p>"}])
      (swap! editor dissoc :shown)
      (is (= :apply (follow! false)))
      (is (= "<p>theirs, later</p>" (:applied @editor))))
    (opt/close! overlay)))

(deftest a-refused-prediction-gives-the-block-back
  (let [conn (fresh-kb)
        overlay (opt/open conn)
        failed (a/promise-chan)
        result (ow/predict-remote!
                overlay [{:entity/uuid block-uuid :block/content "<p>refused</p>"}]
                (fn [] (go (ex-info "not authorized" {:type :not-authorized})))
                {:on-failure #(a/put! failed %)})]
    (is (nil? (<!! result)))
    (is (some? (<!! failed)))
    (is (= "<p>a</p>" (content (opt/db overlay))) "the view is the server's again")
    (testing "an editor that had taken the refused text, and was left, takes it back"
      (let [s (bs/mounted "<p>refused</p>" "<p>refused</p>")]
        (is (= :apply (first (bs/incoming s "<p>a</p>" "<p>refused</p>" false))))))
    (opt/close! overlay)))
