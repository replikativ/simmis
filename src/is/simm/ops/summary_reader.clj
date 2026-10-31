(ns is.simm.ops.summary-reader
  "Current Room-reader access to native summary revisions and exact retained
   sources. No current-Memory fallback, inference, migration or publication."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.summary-records :as records]
            [is.simm.model.summary-envelope :as envelope]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.ops.room-summary :as summary]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- refuse! [] (throw (ex-info "Native summary evidence is unavailable" {})))

(defn with-room! [host principal handle f]
  (operation/with-app!
   host
   (fn []
     (let [original (reads/resolve-host-read-room! host principal handle)
           room (:room original)]
       (binding [ec/*execution-context* (:execution-ctx host)]
         (preparation/with-host-operation!
          room
          (fn [lease]
            (let [conn (summary/native-connection room)
                  check! #(do (reads/validate-host-read-room! host principal handle original)
                               (preparation/with-host-current! lease (constantly true))
                               (let [control (reads/current-host-control-db! host)
                                     store-id (binding [ec/*execution-context* (:ctx room)]
                                                (get-in @conn [:config :store :id]))]
                                 (when-not (= store-id (:room/content-db-scope (d/entity control [:room/id (:room-id original)])))
                                   (refuse!))))]
              (check!)
              (let [result (binding [ec/*execution-context* (:ctx room)] (f original conn))]
                (check!) result)))))))))

(defn with-request! [host principal room request f]
  (with-room!
   host principal (str room)
   (fn [_ conn]
     (when-not (records/installed? @conn) (refuse!))
     (let [matches (vec (take 2 (d/datoms @conn :avet :room.summary/request request)))
           row (when (= 1 (count matches)) (d/pull @conn records/fields (:e (first matches))))]
       (when-not (= room (:room.summary/room row)) (refuse!))
       (let [payload (records/payload! conn row)
             selection (:selection payload)
             files (mapv (fn [role source]
                           {:input (str "/in/summary/sources/" (:path source)) :path (:path source) :role role
                            :content (:file-content source)
                            :size (alength (envelope/text-bytes (get (:inputs payload) (str "/" (:path source)))) )})
                         (concat (repeat (count (:coverage selection)) :coverage)
                                 (repeat (count (:context selection)) :context))
                         (into (:coverage selection) (:context selection)))
             verified {:payload payload :kind :room-summary :revision (:room.summary/revision row) :request request
                       :selection {:coverage-id (:coverage-id payload)
                                   :context-id (hasch/uuid (:context selection)) :manifest-id (:id selection)}
                       :captured {:manifest {:files files}
                                  :inputs (into {} (map (fn [{:keys [input path]}]
                                                         [input (get (:inputs payload) (str "/" path))])) files)}}
             result (f verified)]
         (when-not (= row (records/revision-in @conn (:room.summary/revision row))) (refuse!))
         result)))))

(defn result! [host principal room request]
  (with-request!
   host principal room request
   (fn [{:keys [revision payload]}]
     (let [{:keys [selection report]} payload]
       {:status :verified :kind :room-summary :room room :request request
        :digest-id revision :digest report :selection selection
        :originals (mapv #(assoc % :text (get (:inputs payload) (str "/" (:path %))))
                         (into (:coverage selection) (:context selection)))}))))
