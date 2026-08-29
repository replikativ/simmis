(ns is.simm.ops.proposal-publication-test
  (:require [clojure.test :refer [deftest is testing]]
            [datahike.api :as dh]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory-store]
            [is.simm.agents.room-agents :as room-agents]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.proposal-publication :as publication]
            [is.simm.ops.proposals :as proposals]
            [org.replikativ.spindel.core :as spindel]))

(defn- fresh-system-db []
  (let [cfg {:store {:backend :memory :id (random-uuid)}
             :schema-flexibility :write :keep-history? false}]
    (dh/create-database cfg)
    (doto (dh/connect cfg) (dh/transact sdb/schema))))

(defn- proposal-params [id room-id]
  {:id id :title "Review the retained world" :summary "Two systems changed."
   :room room-id
   :forks [{:scope (random-uuid) :branch :review
            :system-type :datahike}]})

(deftest filing-reserves-one-stable-message-identity
  (let [conn (fresh-system-db)
        proposal-id (random-uuid)
        room-id (random-uuid)
        params (proposal-params proposal-id room-id)]
    (with-redefs [sdb/get-conn (constantly conn)
                  publication/publish! (constantly :pending)]
      (proposals/file-proposal! params)
      (let [first-row (proposals/get-proposal proposal-id)
            message-id (:proposal/message-id first-row)]
        (is (uuid? message-id))
        (is (= :pending (:proposal/message-status first-row)))
        ;; Ambiguous proposal commit retry cannot mint a second chat card.
        (proposals/file-proposal! params)
        (is (= message-id
               (:proposal/message-id (proposals/get-proposal proposal-id))))
        (is (= 1 (count (:proposal/forks
                         (proposals/get-proposal proposal-id))))
            "the retry cannot append another anonymous component")
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"already filed with different content"
             (proposals/file-proposal!
              (assoc params :title "A different future"))))))))

(deftest crash-between-room-post-and-system-projection-retries-idempotently
  (let [conn (fresh-system-db)
        proposal-id (random-uuid)
        room-id (random-uuid)
        runtime-id :proposal-publication-test
        ctx (spindel/create-execution-context)
        room (discourse/make-room {:id runtime-id :ctx ctx
                                   :store (memory-store/make)})
        visible-count (atom 0)
        visible (promise)
        _listener (discourse/on-each-message
                   room (fn [_]
                          (swap! visible-count inc)
                          (deliver visible true)))
        fail-once? (atom true)
        mark! proposals/mark-message-published!]
    (with-redefs [sdb/get-conn (constantly conn)
                  publication/publish! (constantly :pending)]
      (proposals/file-proposal! (proposal-params proposal-id room-id)))
    (with-redefs [sdb/get-conn (constantly conn)
                  room-agents/live-room #(when (= room-id %) room)
                  proposals/mark-message-published!
                  (fn [id]
                    (if (compare-and-set! fail-once? true false)
                      (throw (ex-info "simulated crash after durable post" {}))
                      (mark! id)))]
      (testing "the first attempt leaves a durable retry marker"
        (is (= :pending (publication/publish! proposal-id)))
        (is (= :pending
               (:proposal/message-status (proposals/get-proposal proposal-id))))
        (is (= 1 (count (discourse/messages room))))
        (is (true? (deref visible 3000 false)))
        (is (= 1 @visible-count)))
      (testing "retry posts the same envelope and completes the projection"
        (is (= :published (publication/publish! proposal-id)))
        (let [proposal (proposals/get-proposal proposal-id)
              [message] (discourse/messages room)]
          (is (= :published (:proposal/message-status proposal)))
          (is (= (:proposal/message-id proposal) (:id message)))
          (is (= (:id message) (:thread-root-id message)))
          (is (= {:kind :proposal :id proposal-id}
                 (get-in message [:metadata :object])))
          (is (= 1 (count (discourse/messages room)))
              "the immutable room store contains one canonical card")
          (Thread/sleep 100)
          (is (= 1 @visible-count)
              "retry cannot repeat projector or external relay effects"))))))
