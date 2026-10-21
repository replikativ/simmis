(ns is.simm.runtimes.memory-changes-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [is.simm.documents.files :as files]
            [geschichte.repo :as repo]
            [org.replikativ.spindel.engine.context :as context]
            [is.simm.runtimes.memory-changes :as changes]))

(deftest durable-notification-is-private-currently-authorized-and-owned
  (let [room (discourse/make-room {:id (keyword (str "memory-notify-" (random-uuid)))
                                  :store (memory/make)})
        id (random-uuid) party (random-uuid) current (atom room)
        allowed (atom true) publications (atom []) received (promise) checked (atom (promise)) ready (promise)
        owner (changes/open! {:resolve-room (fn [_] @current)
                              :allowed? (fn [_ _]
                                          (let [permitted? @allowed]
                                            (when-not permitted? (deliver @checked (count @publications)))
                                            permitted?))
                              :publish! (fn [p hint] (swap! publications conj [p hint])
                                          (if (realized? ready) (deliver received [p hint])
                                              (deliver ready true)))})]
    (try
      (workspace/ensure-workspace! room)
      (is (= {:watching? true} ((:watch! owner) id room party)))
      ((:watch! owner) id room party)
      (is (= 1 ((:watch-count owner))))
      (is (= true (deref ready 5000 ::timeout)))
      (files/save-file! room {:path "wiki/test.md" :text "# Private" :expected nil})
      (is (= [party {:kind :memory-changed :room-id id}] (deref received 5000 ::timeout)))
      (reset! allowed false)
      (files/save-file! room {:path "other.md" :text "Not sent after revoke" :expected nil})
      ;; An already-authorized hint may finish before the worker observes the
      ;; revocation. Two denied worker checks fence completion of the first
      ;; denied iteration, without assuming globally atomic revoke/publication.
      (let [denied-at (deref @checked 5000 ::timeout)]
        (is (integer? denied-at))
        (reset! checked (promise))
        (files/save-file! room {:path "after-denied.md" :text "Still not authorized" :expected nil})
        (is (= denied-at (deref @checked 5000 ::timeout)))
        (is (= denied-at (count @publications))))
      (is (thrown? Exception ((:watch! owner) id room party)))
      (reset! allowed true)
      (reset! current nil)
      (is (thrown? Exception ((:watch! owner) id room party)))
      ((:unwatch-room! owner) (assoc room :ctx (Object.)))
      (is (= 1 ((:watch-count owner))))
      ((:unwatch-room! owner) room)
      (is (zero? ((:watch-count owner))))
      ;; Transport interest survives a canonical room unload, but concrete
      ;; connection callbacks do not. A later prepare can rebind safely.
      (is (= #{party} (get-in ((:interests owner)) [id :readers])))
      (files/with-workspace room
        #(is (empty? @(:commit-listeners (meta %)))))
      (finally ((:close! owner)) (discourse/close-room! room)))
    (is (thrown? Exception ((:watch! owner) id room party)))))

(deftest speculative-writes-stay-private-and-logical-reset-is-a-durable-change
  (let [room (discourse/make-room {:id (keyword (str "memory-isolation-" (random-uuid)))
                                  :store (memory/make)})
        id (random-uuid) party (random-uuid) ready (promise) restored (promise)
        phase (atom :setup) root-hints (atom 0)
        owner (changes/open! {:resolve-room (fn [_] room) :allowed? (constantly true)
                              :publish! (fn [_ _]
                                          (swap! root-hints inc)
                                          (deliver ready true)
                                          (when (= :reset @phase)
                                            (deliver restored true)))})]
    (try
      (workspace/ensure-workspace! room)
      (let [saved (files/save-file! room {:path "a.md" :text "Original" :expected nil})]
        ((:watch! owner) id room party)
        (is (= true (deref ready 5000 ::timeout)))
        (let [fork (context/fork-context (:ctx room)) child (assoc room :ctx fork)]
          (try
            (is (thrown? Exception ((:watch! owner) id child party)))
            (files/save-file! child {:path "private.md" :text "Fork only" :expected nil})
            (is (nil? (get-in (files/metadata room {:folders [] :paths ["private.md"]}) [:entries "private.md"])))
            (is (= 1 @root-hints) "A fork-only write did not reach canonical listener")
            (finally (context/close-context! fork))))
        (files/save-file! room {:path "a.md" :text "Changed" :expected (:entry saved)})
        (reset! phase :reset)
        (files/with-workspace room #(repo/reset! % (parse-uuid (:commit saved)) {:mode :hard}))
        (is (= true (deref restored 5000 ::timeout)))
        (is (= "Original" (:text (files/read-file room "a.md")))))
      (finally ((:close! owner)) (discourse/close-room! room)))))

(deftest topic-identity-is-exact-and-not-a-blanket-keyword-allowlist
  (let [party (random-uuid)]
    (is (= party (changes/topic-party (changes/topic party))))
    (doseq [value [:memory/not-a-uuid :notify/not-a-uuid :memory nil (str party)]]
      (is (nil? (changes/topic-party value))))))
