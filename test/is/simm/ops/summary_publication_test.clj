(ns is.simm.ops.summary-publication-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.summary-documents :as documents]
            [is.simm.model.room-summaries-test :as report-fixture]
            [is.simm.model.summary-records :as records]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.generated-summary-files :as generated]
            [is.simm.ops.owned-charts-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.ops.room-summary :as summary]
            [is.simm.ops.summary-inputs :as inputs]
            [is.simm.ops.summary-publication :as publication]
            [is.simm.ops.summary-reader :as reader]
            [org.replikativ.spindel.engine.core :as ec]))

(deftest native-result-and-transcript-survive-without-control-request-and-preserve-git-edits
  (@#'fixture/with-fixture
   (fn [{:keys [host conn w room owned]}]
     (let [principal (ops/party-principal (:owner w)) handle (str (:room w))
           original (reads/resolve-host-room! host principal handle)
           bound (reads/resolve-host-read! host principal handle)
           native (summary/native-connection room)]
       (d/transact conn [{:room/id (:room w) :room/content-db-scope (get-in @native [:config :store :id])}])
       (operation/with-app! host
        (fn []
         (binding [ec/*execution-context* (:ctx room)]
         (records/ensure-schema! native)
         (let [inventory (inputs/inventory (:room w) report-fixture/period
                                           {:source-store (get-in @native [:config :store :id])
                                            :source-basis (:max-tx @native)
                                            :rows [{:id (random-uuid) :author (:owner w) :actor-kind :human
                                                    :timestamp (.getTime (:from report-fixture/period))
                                                    :text "Keep the precise original, even if the summary omits it."}]})
               frozen (inputs/freeze inventory #inst "2026-10-31T00:00:00Z" [])
               selection (:selection frozen) request (random-uuid)
               value {:selection selection :report (report-fixture/report selection) :inputs (:inputs frozen)
                      :coverage-id (:coverage-id inventory) :previous nil :generation 1 :target (:target bound)
                      :created-at #inst "2026-10-31T00:00:01Z"
                      :program {:bundle (random-uuid) :model "model-free-fixture" :request request :attempt (random-uuid)
                                :target (records/target-id (:target bound))}}
               record (records/publish! native value)
               prepared {:inventory inventory :frozen frozen :summary-origin {:target (:target bound)}}
               path (documents/path selection)]
           (is (= :published (:status (publication/publish-transcript! host principal handle original prepared))))
           (is (files/entry @owned (documents/transcript-path selection)))
           (is (= :published (:status (publication/publish-file! host principal handle original record :markdown
                                                                 path (documents/markdown value) (:target bound)))))
           (is (= :published (:status (publication/publish-chat! host principal handle original record value))))
           (let [result (reader/result! host principal (:room w) request)]
             (is (= :room-summary (:kind result)))
             (is (= request (:request result)))
             (is (.contains (:text (first (:originals result))) "precise original")))
           (is (= :current (:status (generated/inspect room (:room w) (:target bound) path (files/entry @owned path)))))
           (is (thrown? clojure.lang.ExceptionInfo
                        (generated/editable! room (:room w) (:target bound) path (files/entry @owned path))))
           ;; External Git/file transport can differ from native projection;
           ;; normal editing must never grant authority from frontmatter alone.
           (repo/write! owned path (markdown/source-bytes "External Git edit; preserve this."))
           (is (= :diverged (:status (generated/inspect room (:room w) (:target bound) path (files/entry @owned path)))))
           (is (nil? (generated/inspect room (:room w) (:target bound) "wiki/authored.md" nil)))
           (let [next-value (-> value (assoc :previous (:room.summary/revision record) :generation 2)
                                (assoc-in [:program :request] (random-uuid)))
                 next-record (records/publish! native next-value)
                 result (publication/publish! host principal handle original {:record next-record :payload next-value})]
             (is (= :diverged (get-in result [:markdown :status])))
             (is (= :published (get-in result [:chat :status])))
             (is (= "External Git edit; preserve this." (:text (files/read-file-in @owned path)))))))))))))
