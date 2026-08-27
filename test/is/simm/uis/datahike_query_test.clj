(ns is.simm.uis.datahike-query-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.datahike-query :as query]))

(deftest canonical-message-projection
  (let [party-id (random-uuid)
        message-id (random-uuid)
        parent-id (random-uuid)
        blob-id (random-uuid)
        sent-at (java.util.Date.)
        canonical {:id message-id
                   :content "[[Pricing]]"
                   :sent-at sent-at
                   :from (keyword "party" (str party-id))
                   :to :agent/reviewer
                   :in-reply-to parent-id
                   :source-user (str party-id)
                   :reasoning "checked the evidence"
                   :metadata (pr-str {:attachment {:blob-id blob-id
                                                   :mime "audio/ogg"}})}
        legacy {:entity/uuid message-id
                :block/content "[[dh://pricing/page]]"
                :S.Message/author-uuid (random-uuid)
                :S.Message/author-name "stale name"
                :S.Message/is-ai false}
        result (query/merge-message-projections
                canonical legacy {party-id {:name "Mira" :is-ai true}})]
    (testing "canonical identity, actor, time and causality win"
      (is (= message-id (:entity/uuid result)))
      (is (= party-id (:S.Message/author-uuid result)))
      (is (= "Mira" (:S.Message/author-name result)))
      (is (= sent-at (:timeline/ts result)))
      (is (= :agent/reviewer (:message/to result)))
      (is (= parent-id (:message/in-reply-to result))))
    (testing "legacy resolved display text remains compatibility enrichment"
      (is (= "[[dh://pricing/page]]" (:block/content result))))
    (testing "canonical reasoning and attachment metadata are rendered"
      (is (= "checked the evidence" (:S.Message/reasoning result)))
      (is (= (str blob-id) (:S.Message/attachment-blob result)))
      (is (= "audio/ogg" (:S.Message/attachment-mime result)))
      (is (true? (:S.Message/is-ai result))))))

(deftest historical-source-user-fallback
  (let [party-id (random-uuid)
        result (query/merge-message-projections
                {:id (random-uuid)
                 :content "old row"
                 :sent-at (java.util.Date.)
                 :source-user (str party-id)}
                nil
                {party-id {:name "Alice" :is-ai false}})]
    (is (= party-id (:S.Message/author-uuid result)))
    (is (= "Alice" (:S.Message/author-name result)))
    (is (= "old row" (:block/content result)))))
