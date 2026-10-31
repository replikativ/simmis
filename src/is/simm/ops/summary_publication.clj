(ns is.simm.ops.summary-publication
  "Independent guarded projections of a native summary. Projection retries
   never execute models; writing without a returned ACK stays inspection-required."
  (:require [datahike.blob :as blob]
            [dvergr.artifact :as artifact]
            [dvergr.discourse :as discourse]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.summary-documents :as documents]
            [is.simm.model.summary-records :as records]
            [is.simm.model.room-summaries :as summaries]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.room-summary :as summary]
            [is.simm.ops.summary-inputs :as inputs]
            [is.simm.ops.work :as work]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [reason]
  (throw (ex-info "Generated summary publication is held" {:type ::held :reason reason})))

(defn- native! [original f]
  (binding [ec/*execution-context* (:ctx (:room original))] (f)))

(defn- check! [host principal handle original bound]
  (let [db (reads/current-host-control-db! host)]
    (reads/validate-host-room-in! host principal handle original db)
    (work/authorize-in! db principal (:room-id original))
    (owned/validate! db principal :write (:room-id original) (:room original) bound)))

(defn- reserve! [host principal handle original bound record kind path value]
  (let [conn (summary/native-connection (:room original))
        target (records/target-id (:target bound))
        revision (:room.summary/revision record)
        content (blob/blob-id (markdown/source-bytes (if (= :chat kind) (pr-str value) value)))
        id (hasch/uuid [:simmis/summary-projection-v1 revision kind target path content])
        existing (native! original #(records/projection-in @conn id))]
    (check! host principal handle original bound)
    (if existing
      existing
      (let [key (records/path-id (:room-id original) target path)
            prior (native! original #(records/path-in @conn key))
            prior-projection (when prior (native! original #(records/projection-in @conn (:summary.path/owner prior))))
            _ (when (and (= :transcript kind) prior
                         (or (not (integer? (:summary.projection/source-basis prior-projection)))
                             (not (integer? (:source-basis record)))
                             (<= (:source-basis record) (:summary.projection/source-basis prior-projection))))
                (refuse! :stale-transcript))
            expected (when-not (= :chat kind) (files/entry (:db bound) path))]
        (when-not (or (nil? prior) (= :published (:summary.path/state prior))) (refuse! :projection-busy))
        (when (and (not= :chat kind)
                   (if prior (not= (:summary.path/content prior) (:content expected)) (some? expected)))
          (refuse! :projection-diverged))
        (let [intent {:kind kind :path path :value value :expected expected :owner (:summary.path/owner prior)}
              entity (cond-> {:summary.projection/id id :summary.projection/revision revision
                      :summary.projection/node (:room.summary/node record) :summary.projection/kind kind
                      :summary.projection/room (:room-id original) :summary.projection/target target
                      :summary.projection/path path :summary.projection/content content
                      :summary.projection/state :reserved}
                       (= :transcript kind) (assoc :summary.projection/source-basis (:source-basis record)))]
          (artifact/prepare-value intent (* 2 1024 1024))
          (native! original
                   #(artifact/publish-value!
                     (artifact/datahike-store conn) intent
                     (fn [ref]
                       (check! host principal handle original bound)
                       (records/reserve-projection! conn (assoc entity :summary.projection/intent ref)))))
          (native! original #(records/projection-in @conn id)))))))

(defn- prepare-delivery! [host principal handle original record kind path value target]
  (operation/with-current!
   host principal handle original
   (fn []
     (let [bound (reads/resolve-host-read! host principal handle)
           conn (summary/native-connection (:room original))
           _ (check! host principal handle original bound)
           _ (native! original #(records/ensure-schema! conn))
           _ (when-not (= (select-keys target records/target-keys)
                          (select-keys (:target bound) records/target-keys))
               (refuse! :target-changed))
           row (reserve! host principal handle original bound record kind path value)]
       (case (:summary.projection/state row)
         :published (if (and (= :transcript kind)
                             (not= (:summary.projection/id row)
                                   (:summary.path/owner
                                    (native! original #(records/path-in @conn
                                                        (records/path-id (:room-id original)
                                                                         (:summary.projection/target row) path))))))
                      {:status :inspection-required :projection row}
                      {:status :published :projection row})
         :writing {:status :inspection-required :projection row}
         :reserved
         (let [intent (:value (native! original #(artifact/get-bounded-value
                                                  (artifact/datahike-store conn) (:summary.projection/intent row)
                                                  (* 2 1024 1024))))
               expected-content (blob/blob-id (markdown/source-bytes (if (= :chat kind) (pr-str value) value)))]
           (when-not (and (= kind (:kind intent)) (= path (:path intent)) (= value (:value intent))
                          (= expected-content (:summary.projection/content row)))
             (refuse! :projection-intent-changed))
           (when (and (not= :chat kind) (not= (:expected intent) (files/entry (:db bound) path)))
             (refuse! :projection-diverged))
           (check! host principal handle original bound)
           (native! original #(records/claim-projection! conn (:summary.projection/id row) (:owner intent)))
           {:status :claimed :projection row :intent intent :bound bound})
         (refuse! :projection-state))))))

(defn- finish! [host principal handle original delivery receipt]
  (operation/with-current!
   host principal handle original
   (fn []
     (let [conn (summary/native-connection (:room original))]
       (check! host principal handle original (:bound delivery))
       (native! original
                #(artifact/publish-value!
                  (artifact/datahike-store conn) receipt
                  (fn [ref]
                    (records/finish-projection! conn (get-in delivery [:projection :summary.projection/id]) ref))))
       {:status :published}))))

(defn- check-delivery! [host principal handle original delivery]
  (check! host principal handle original (:bound delivery))
  (let [conn (summary/native-connection (:room original))
        projection (:projection delivery)
        id (:summary.projection/id projection)
        key (records/path-id (:summary.projection/room projection) (:summary.projection/target projection)
                             (:summary.projection/path projection))]
    (native! original
             #(let [row (records/projection-in @conn id) path (records/path-in @conn key)
                    head (when (#{:markdown :chat} (:summary.projection/kind projection))
                           (records/head-in @conn (:summary.projection/room projection)
                                            (:summary.projection/node projection)))]
                (when-not (and (= :writing (:summary.projection/state row))
                               (= id (:summary.path/owner path)) (= :writing (:summary.path/state path))
                               (or (not (#{:markdown :chat} (:summary.projection/kind projection)))
                                   (= (:summary.projection/revision projection) (:summary.head/revision head))))
                  (refuse! :projection-generation-changed))))))

(defn publish-file!
  [host principal handle original record kind path text target]
  (let [delivery (prepare-delivery! host principal handle original record kind path text target)]
    (if-not (= :claimed (:status delivery))
      (select-keys delivery [:status])
      (operation/with-current!
       host principal handle original
       (fn []
         (let [bound (:bound delivery)
               authorize! #(do (check-delivery! host principal handle original delivery) true)
               receipt (files/save-file-to! (:room original) (:system bound)
                                            {:path path :text text :expected (get-in delivery [:intent :expected])}
                                            {:authorize! authorize!})]
           (finish! host principal handle original delivery receipt)))))))

(defn publish-chat! [host principal handle original record payload]
  (let [revision (:room.summary/revision record)
        id (hasch/uuid [:simmis/room-summary-message-v1 revision])
        envelope {:id id :from :simmis/room-summaries :to :_room-log
                  :content (get-in payload [:report :summary :text])
                  :ts (.getTime ^java.util.Date (:room.summary/created-at record))
                  :in-reply-to nil :thread-root-id id
                  :metadata {:role :system :kind :room-summary :source :simmis/room-summary
                             :notification/type :task-complete
                             :notification/task (:room.summary/request record)
                             :object {:kind :room-summary :id revision}}}
        delivery (prepare-delivery! host principal handle original record :chat
                                    (str "chat/" (:room.summary/node record)) envelope (:target payload))]
    (if-not (= :claimed (:status delivery))
      (select-keys delivery [:status])
      (do
        ;; The short preparation extent ended before posting. Room subscribers
        ;; can fork on visibility without waiting for this same extent to exit.
        (locking (:meta (:room original))
          (check-delivery! host principal handle original delivery)
          (native! original #(discourse/post! (:room original) (discourse/map->Message envelope))))
        (finish! host principal handle original delivery {:message id :revision revision})))))

(defn- delivery [f]
  (try (f)
       (catch Throwable error
         (if (or (.isInterrupted (Thread/currentThread)) (adoption/causal-interruption? error))
           (do (.interrupt (Thread/currentThread)) (throw error))
           {:status (if (= :projection-diverged (:reason (ex-data error))) :diverged :inspection-required)
            :reason (or (:reason (ex-data error)) :publication-held)}))))

(defn publish! [host principal handle original {:keys [record payload]}]
  {:markdown (delivery #(publish-file! host principal handle original record :markdown
                                       (documents/path (:selection payload)) (documents/markdown payload) (:target payload)))
   :chat (delivery #(publish-chat! host principal handle original record payload))})

(defn publish-transcript!
  "A model-free projection over the selected originals. Retained delivery intent
   owns the complete text even if subsequent model admission or execution fails."
  [host principal handle original {:keys [inventory frozen summary-origin]}]
  (let [selection (:selection frozen)
        target (:target summary-origin)
        revision (hasch/uuid [:simmis/daily-transcript-revision-v1
                             (:coverage-id inventory) (:source-basis inventory) (records/target-id target)])
        record {:room.summary/revision revision
                :room.summary/node (summaries/node-id selection)
                :source-basis (:source-basis inventory)}]
    (delivery #(publish-file! host principal handle original record :transcript
                              (documents/transcript-path selection)
                              (inputs/transcript inventory) target))))
