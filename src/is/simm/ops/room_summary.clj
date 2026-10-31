(ns is.simm.ops.room-summary
  "Owned daily selection and certified native publication. No timer, provider
   client, new execution ledger or wiki-processing acknowledgement."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.documents.summary-documents :as documents]
            [is.simm.model.room-summaries :as summaries]
            [is.simm.model.summary-envelope :as envelope]
            [is.simm.model.summary-records :as records]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.conversation-capture :as capture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.ops.owned-workspace :as owned]
            [is.simm.ops.summary-inputs :as inputs]
            [is.simm.ops.worker-files :as worker-files]
            [is.simm.ops.workflow-result :as workflow-result]
            [is.simm.ops.work :as work]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [reason]
  (throw (ex-info "Room summary is unavailable" {:type ::unavailable :reason reason})))

(defn native-connection [room]
  (let [conn (:conn (:store room))]
    (when-not conn (refuse! :unsupported-room-store)) conn))

(defn- check! [host principal handle original bound]
  (let [db (reads/current-host-control-db! host)]
    (work/authorize-in! db principal (:room-id original))
    (reads/validate-host-room-in! host principal handle original db)
    (when bound
      (owned/validate! db principal :write (:room-id original) (:room original) bound))))

(defn prepare!
  "A cheap exact-period source check comes before model/resource preparation.
   Context changes alone cannot trigger inference. No effect or input receipt."
  [host principal handle original period cutoff]
  (operation/with-current!
   host principal handle original
   (fn []
     (check! host principal handle original nil)
     (let [bound (reads/resolve-host-read! host principal handle)
           _ (when-not (identical? (:room original) (:room bound)) (refuse! :room-changed))
           _ (check! host principal handle original bound)
           source (capture/room-source bound period)
           inventory (inputs/inventory (:room-id original) period source)
           conn (native-connection (:room original))
           native-db (binding [ec/*execution-context* (:ctx (:room original))] @conn)
           node (summaries/node-id {:room (:room-id original) :store (:source-store source) :period period})
           head (records/head-in native-db (:room-id original) node)
           previous (records/latest-in native-db (:room-id original) node)]
       (check! host principal handle original bound)
       (cond
         (empty? (:artifacts inventory)) {:status :skip :reason :no-inputs}
         (= (:coverage-id inventory) (:room.summary/coverage-id previous))
         {:status :skip :reason :unchanged}
         :else
         (let [candidates (->> (:rows (records/heads-in native-db (:room-id original) 128))
                               (filter #(and (= :ready (:room.summary/state %))
                                             (not (summaries/before? (:from period) (:room.summary/until %)))))
                               (reduce (fn [out row]
                                         (if (some #(= (:room.summary/node %) (:room.summary/node row)) out)
                                           out (conj out row))) [])
                               (take 2))
               available (- summaries/max-input-bytes
                            (reduce + 0 (map #(alength (envelope/text-bytes (:text %))) (:artifacts inventory))))
               context (:sources
                        (reduce (fn [{:keys [sources remaining] :as selected} row]
                                  (let [item (documents/context-source row (records/payload! conn row))
                                        size (alength (envelope/text-bytes (:text item)))]
                                    (if (> size remaining) selected
                                      {:sources (conj sources item) :remaining (- remaining size)})))
                                {:sources [] :remaining available} candidates))
               frozen (inputs/freeze inventory cutoff context)]
           (check! host principal handle original bound)
           {:status :ready :inventory inventory :frozen frozen
            :summary-origin {:target (:target bound) :target-stamp (:target-stamp bound)
                             :previous (:summary.head/revision head)
                             :generation (inc (or (:summary.head/generation head) 0))}}))))))

(defn verify-resolution!
  "Verify historical occurrence bytes and period against the pinned deployment.
   A read does not reconstruct first-install ACK or grant dispatch authority."
  [target resolution]
  (when-not (and (= 3 (:version resolution)) (= :room-summary (:kind resolution))
                 (= (:target-revision resolution) (hasch/uuid target))
                 (= (:room target) (get-in resolution [:selection :room]))
                 (= (get-in target [:cadence :tz]) (get-in resolution [:selection :period :timezone]))
                 (inputs/declaration (get-in target [:snapshot :files])))
    (refuse! :resolution-changed))
  (envelope/validate! (:selection resolution) (:inputs resolution))
  resolution)

(defn completed!
  "Copy certified result and exact originals into independent native retention.
   Must run within the caller's original Room/App operation after settlement."
  [host principal handle original request-id resolution]
  (operation/with-current!
   host principal handle original
   (fn []
     (let [bound (reads/resolve-host-read! host principal handle)
           _ (check! host principal handle original bound)
           control (:control-conn (::reads/owner host))
           row (requests/request-in (reads/current-host-control-db! host) request-id)
           _ (when-not (and (= request-id (:work.request/id row))
                            (= (:room-id original) (:work.request/room row))
                            (= request-id (requests/request-id (:work.request/by row) (:work.request/room row)
                                                              (:work.request/client-id row)))
                            (= :workflow (:work.request/kind row))
                            (= :submitted (:work.request/state row))
                            (false? (:work.request/return-pending? row)))
               (refuse! :request-changed))
           retained (:value (artifact/get-bounded-value (artifact/datahike-store control)
                                                       (:work.request/payload-ref row) (* 32 1024 1024)))
           _ (when-not (= (:work.request/intent-id row) (hasch/uuid retained)) (refuse! :request-changed))
           input (:input retained)
           _ (when-not (= resolution (:summary/input input)) (refuse! :resolution-changed))
           selection (inputs/validate-inputs! (:source input) (:inputs input))
           _ (when-not (= selection (:selection resolution)) (refuse! :selection-changed))
           _ (when-not (= (select-keys (get-in resolution [:summary-origin :target])
                                       [:repository :store :backend :branch :ref])
                          (select-keys (:target bound) [:repository :store :backend :branch :ref]))
               (refuse! :target-changed))
           certified (or (workflow-result/completed-attempt (:room original) row input)
                         (refuse! :not-certified))
           value (get-in certified [:attempt/evidence :files summaries/output-path])
           bytes (if (string? value) (envelope/text-bytes value) (worker-files/file-bytes! value))
           _ (when (> (alength ^bytes bytes) (* 64 1024)) (refuse! :oversized-result))
           report (summaries/validate-report! selection (envelope/read-value (String. ^bytes bytes "UTF-8") 65536))
           conn (native-connection (:room original))
           payload {:selection selection :report report
                    :inputs (envelope/extract selection (:inputs input))
                    :coverage-id (:coverage-id resolution)
                    :created-at (:work.request/created-at row)
                    :previous (get-in resolution [:summary-origin :previous])
                    :generation (get-in resolution [:summary-origin :generation])
                    :target (select-keys (get-in resolution [:summary-origin :target]) records/target-keys)
                    :program {:bundle (parse-uuid (get-in input [:workflow :bundle]))
                              :model (get-in certified [:attempt/receipt :attempt/model])
                              :target (records/target-id (get-in resolution [:summary-origin :target]))
                              :request request-id :attempt (:attempt/run-id certified)}}]
       (check! host principal handle original bound)
       (binding [ec/*execution-context* (:ctx (:room original))]
         (records/ensure-schema! conn)
         (let [record (records/publish! conn payload)]
           (check! host principal handle original bound)
           ;; Existing running alpha hosts can append this new pointer attr;
           ;; normal cold startup installs it with the work-request schema.
           (when-not (contains? (:schema @control) :work.request/summary-revision)
             (d/transact control (filterv #(= :work.request/summary-revision (:db/ident %)) requests/schema)))
           (requests/attach-summary! control row (:room.summary/revision record))
           {:record record :payload (records/payload! conn record)}))))))
