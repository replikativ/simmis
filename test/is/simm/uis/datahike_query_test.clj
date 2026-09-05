(ns is.simm.uis.datahike-query-test
  (:require [clojure.test :refer [deftest is testing]]
            [is.simm.uis.web.desktop.datahike-query :as query]))

(deftest canonical-message-thread-projection
  (let [root-id (random-uuid)
        reply-id (random-uuid)
        nested-id (random-uuid)
        partial-id (random-uuid)
        missing-id (random-uuid)
        external-parent-id (random-uuid)
        rows [{:entity/uuid root-id
               :timeline/type :message
               :S.Message/author-name "Founder"
               :block/content "Choose the launch market"}
              {:entity/uuid reply-id
               :timeline/type :message
               :message/in-reply-to root-id
               :S.Message/author-name "Researcher"
               :block/content "I compared three regions."}
              {:entity/uuid nested-id
               :timeline/type :message
               :message/in-reply-to reply-id
               :S.Message/author-name "Reviewer"
               :block/content "The evidence favours Vancouver."}
              {:entity/uuid partial-id
               :timeline/type :message
               :message/in-reply-to external-parent-id
               :message/thread-root-id root-id
               :S.Message/author-name "Remote researcher"
               :block/content "The middle of this thread is not loaded."}
              {:entity/uuid missing-id
               :timeline/type :message
               :message/in-reply-to external-parent-id
               :S.Message/author-name "Imported agent"
               :block/content "Parent is outside this bounded result."}
              {:entity/uuid (random-uuid)
               :timeline/type :eval-entry}]
        [root reply nested partial missing tool] (query/annotate-message-threads rows)]
    (testing "known reply chains resolve to one root without reordering"
      (is (= (mapv :entity/uuid rows)
             (mapv :entity/uuid [root reply nested partial missing tool])))
      (is (= root-id (:thread/root-id root)))
      (is (= 3 (:thread/reply-count root)))
      (is (= 1 (:thread/depth reply)))
      (is (= 2 (:thread/depth nested)))
      (is (= {:id root-id
              :author-name "Founder"
              :content "Choose the launch market"}
             (:thread/parent reply))))
    (testing "a persisted root survives an unloaded intermediate parent"
      (is (= root-id (:thread/root-id partial)))
      (is (true? (:thread/root-known? partial)))
      (is (= 1 (:thread/depth partial))))
    (testing "an unloaded parent remains an explicit incomplete relationship"
      (is (= external-parent-id (:thread/root-id missing)))
      (is (false? (:thread/root-known? missing)))
      (is (= 1 (:thread/depth missing)))
      (is (contains? missing :thread/parent))
      (is (nil? (:thread/parent missing))))
    (testing "non-message timeline rows pass through unchanged"
      (is (= (last rows) tool)))))

(deftest focused-thread-projection
  (let [root-id (random-uuid)
        other-id (random-uuid)
        child-id (random-uuid)
        rows (query/annotate-message-threads
              [{:entity/uuid root-id :timeline/type :message}
               {:entity/uuid child-id :timeline/type :message
                :message/in-reply-to root-id
                :message/thread-root-id root-id}
               {:entity/uuid other-id :timeline/type :message}
               {:entity/uuid (random-uuid) :timeline/type :eval-entry}])
        {:keys [root items]} (query/select-message-thread rows root-id)]
    (is (= root-id (:entity/uuid root)))
    (is (= [child-id] (mapv :entity/uuid items)))
    (is (every? #(= :message (:timeline/type %)) items))))

(deftest semantic-tool-activity-is-correlation-only
  (let [message-id #uuid "10000000-0000-0000-0000-000000000001"
        root-id #uuid "10000000-0000-0000-0000-000000000002"
        run-id #uuid "10000000-0000-0000-0000-000000000003"
        activity-id #uuid "10000000-0000-0000-0000-000000000004"
        sent-at (java.util.Date. 1000)
        result (query/canonical-message-entity->timeline-item
                {:message/id message-id
                 :message/content "synthetic tool summary"
                 :message/reasoning "private raw reasoning"
                 :message/created-at sent-at
                 :message/in-reply-to root-id
                 :message/thread-root-id root-id
                 :message/run-id run-id
                 :message/tool-uses [{:tool-use/id "call-1"
                                      :tool-use/name "clojure_eval"
                                      :tool-use/input {:code "(+ 1 2)"}}]
                 :message/activities [{:activity/id activity-id
                                       :activity/run-id run-id
                                       :activity/kind :tool
                                       :activity/verb :invoke
                                       :activity/tool-name "clojure_eval"}]})]
    (is (= {:entity/uuid message-id
            :timeline/type :activity
            :timeline/ts sent-at
            :message/in-reply-to root-id
            :message/thread-root-id root-id
            :message/run-id run-id
            :message/activities [{:activity/id activity-id
                                  :activity/run-id run-id
                                  :activity/kind :tool
                                  :activity/verb :invoke
                                  :activity/tool-name "clojure_eval"}]}
           result))
    (is (not (contains? result :message/tool-uses)))
    (is (not (contains? result :block/content)))
    (is (not (contains? result :S.Message/reasoning)))))

(deftest tool-activity-has-one-surface-per-timeline-scope
  (let [root-id #uuid "20000000-0000-0000-0000-000000000001"
        child-id #uuid "20000000-0000-0000-0000-000000000002"
        activity-id #uuid "20000000-0000-0000-0000-000000000003"
        eval-id #uuid "20000000-0000-0000-0000-000000000004"
        other-root-id #uuid "20000000-0000-0000-0000-000000000005"
        other-activity-id #uuid "20000000-0000-0000-0000-000000000006"
        annotated (query/annotate-message-threads
                   [{:entity/uuid root-id :timeline/type :message}
                    {:entity/uuid child-id
                     :timeline/type :message
                     :message/in-reply-to root-id
                     :message/thread-root-id root-id}
                    {:entity/uuid activity-id
                     :timeline/type :activity
                     :message/thread-root-id root-id
                     :message/activities [{:activity/id activity-id
                                           :activity/kind :tool
                                           :activity/tool-name "clojure_eval"}]}
                    {:entity/uuid eval-id
                     :timeline/type :eval-entry
                     :S.EvalEntry/tool "clojure_eval"
                     :S.EvalEntry/code "(+ 1 2)"
                     :S.EvalEntry/result "3"}
                    {:entity/uuid other-root-id :timeline/type :message}
                    {:entity/uuid other-activity-id
                     :timeline/type :activity
                     :message/thread-root-id other-root-id
                     :message/activities [{:activity/id other-activity-id
                                           :activity/kind :tool
                                           :activity/tool-name "shell"}]}])
        full-room (query/timeline-items-for-scope annotated nil)
        thread (query/timeline-items-for-scope annotated root-id)]
    (testing "the full room keeps the one rich exact evaluation surface"
      (is (= 1 (count (filter #(= :eval-entry (:timeline/type %)) full-room))))
      (is (= [eval-id]
             (mapv :entity/uuid
                   (filter #(= :eval-entry (:timeline/type %)) full-room))))
      (is (not-any? #(= :activity (:timeline/type %)) full-room)))
    (testing "the dedicated thread uses only its canonical correlated activity"
      (is (= [child-id activity-id] (mapv :entity/uuid thread)))
      (is (= [:message :activity] (mapv :timeline/type thread)))
      (is (not-any? :S.EvalEntry/code thread))
      (is (not-any? :S.EvalEntry/result thread)))))

(deftest typed-message-entity-roundtrip
  (let [message-id (random-uuid)
        parent-id (random-uuid)
        run-id (random-uuid)
        object-id (random-uuid)
        activity-id (random-uuid)
        store-ref (random-uuid)
        sent-at (java.util.Date.)
        entity {:message/id message-id
                :message/content "Forecast ready"
                :message/created-at sent-at
                :message/role :assistant
                :message/from :agent/forecaster
                :message/to :party/founder
                :message/in-reply-to parent-id
                :message/thread-root-id parent-id
                :message/run-id run-id
                :message/activities [{:activity/id activity-id
                                      :activity/run-id run-id
                                      :activity/kind :budget
                                      :activity/verb :exhaust
                                      :activity/status :blocked
                                      :activity/critical? true}]
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
                :message/object-kind :proposal
                :message/object-id object-id
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
            :thread-root-id parent-id
            :run-id run-id
            :activities [{:activity/id activity-id
                          :activity/run-id run-id
                          :activity/kind :budget
                          :activity/verb :exhaust
                          :activity/status :blocked
                          :activity/critical? true}]
            :reasoning "sampled three scenarios"
            :source-user "agent-7"
            :metadata {:role :assistant
                       :run-id run-id
                       :activities [{:activity/id activity-id
                                     :activity/run-id run-id
                                     :activity/kind :budget
                                     :activity/verb :exhaust
                                     :activity/status :blocked
                                     :activity/critical? true}]
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
                       :object {:kind :proposal :id object-id}
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
                   :activities [{:activity/id (random-uuid)
                                 :activity/kind :run
                                 :activity/verb :fail
                                 :activity/status :failed}]
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
      (is (= :fail (-> result :message/activities first :activity/verb)))
      (is (= {:mode :live :source :screen}
             (get-in result [:message/metadata :provenance])))
      (is (true? (:S.Message/is-ai result))))))

(deftest canonical-message-pull-is-schema-compatible
  (testing "old room replicas are not asked for an unknown activity attribute"
    (is (not-any? map? (query/canonical-message-pull-pattern false))))
  (testing "upgraded room replicas pull typed activity components"
    (is (= :message/activities
           (-> (query/canonical-message-pull-pattern true) last keys first)))))

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
