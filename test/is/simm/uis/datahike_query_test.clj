(ns is.simm.uis.datahike-query-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.datahike-query :as query]))

(deftest typed-message-entity-roundtrip
  (let [message-id (random-uuid)
        parent-id (random-uuid)
        store-ref (random-uuid)
        sent-at (java.util.Date.)
        entity {:message/id message-id
                :message/content "Forecast ready"
                :message/created-at sent-at
                :message/role :assistant
                :message/from :agent/forecaster
                :message/to :party/founder
                :message/in-reply-to parent-id
                :message/reasoning "sampled three scenarios"
                :message/source-user "agent-7"
                :message/source-username "Forecaster"
                :message/source-user-id "provider-7"
                :message/audience [:party/founder :agent/reviewer]
                :message/mention-handles ["founder" "reviewer"]
                :message/metadata-kind :forecast
                :message/context-from :room/planning
                :message/source :agent
                :message/schedule-id "daily-forecast"
                :message/attachment-store-ref store-ref
                :message/attachment-node-id "konserve-node"
                :message/attachment-mime "application/edn"
                :message/attachment-name "forecast.edn"
                :message/attachment-size 2048
                :message/provenance-mode :simulation
                :message/provenance-source :raster
                :message/notification-type :agent/completed
                :message/notification-agent :agent/forecaster
                :message/notification-task "forecast"
                :message/notification-elapsed 1250}
        result (query/canonical-message-entity->message entity)]
    (is (= {:id message-id
            :content "Forecast ready"
            :sent-at sent-at
            :role :assistant
            :from :agent/forecaster
            :to :party/founder
            :in-reply-to parent-id
            :reasoning "sampled three scenarios"
            :source-user "agent-7"
            :metadata {:role :assistant
                       :source-user "agent-7"
                       :source-username "Forecaster"
                       :source-user-id "provider-7"
                       :audience #{:party/founder :agent/reviewer}
                       :mentions #{"founder" "reviewer"}
                       :kind :forecast
                       :from :room/planning
                       :source :agent
                       :schedule-id "daily-forecast"
                       :attachment {:blob-id store-ref
                                    :node-id "konserve-node"
                                    :mime "application/edn"
                                    :name "forecast.edn"
                                    :size 2048}
                       :provenance {:mode :simulation :source :raster}
                       :notification/type :agent/completed
                       :notification/agent :agent/forecaster
                       :notification/task "forecast"
                       :notification/elapsed 1250}}
           result))))

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
                   :metadata {:audience #{:agent/reviewer}
                              :mentions #{"reviewer"}
                              :attachment {:blob-id blob-id
                                           :mime "audio/ogg"}
                              :provenance {:mode :live :source :screen}}}
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
      (is (= #{:agent/reviewer} (:message/audience result)))
      (is (= #{"reviewer"} (:message/mention-handles result)))
      (is (= {:mode :live :source :screen}
             (get-in result [:message/metadata :provenance])))
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
