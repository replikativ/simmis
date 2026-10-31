(ns is.simm.ops.room-summary-settings
  "Daily summary controls select ordinary owned workflow schedules. No new
   timer or funding: source remains editable and execution uses Room allowance."
  (:require [datahike.api :as d]
            [is.simm.documents.files :as files]
            [is.simm.documents.summary-documents :as documents]
            [is.simm.model.access :as access]
            [is.simm.model.room-summary-settings :as settings]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.ops.owned-workflows :as sources]
            [is.simm.ops.owned-workflow-authoring :as authoring]
            [is.simm.ops.summary-inputs :as inputs]
            [is.simm.ops.workflow-readiness :as readiness]
            [is.simm.ops.workflow-schedules :as scheduler]
            [is.simm.ops.workflows :as workflows]
            [is.simm.starter.daily-room-summary :as starter]
            [org.replikativ.spindel.engine.core :as ec]))

(def limits {:budget-microdollars 25000 :max-dispatches 8 :max-model-steps 12
             :timeout-ms 180000 :settlement "discard"})

(defn- refuse! [reason]
  (throw (ex-info "Daily summary settings unavailable" {:type ::unavailable :reason reason})))

(defn read! [host principal handle]
  (operation/with-app!
   host
   (fn []
     (let [original (reads/preflight-host-read! host principal handle)
           {:keys [control-conn]} (::reads/owner host)
           db (reads/current-host-control-db! host)
           id (settings/current-in db (:room-id original))
           row (when id (schedules/activation-in db id))
           target (when row (schedules/managed-target control-conn row))
           _ (when (and id (not= (:room-id original) (:schedule.activation/room row))) (refuse! :activation-changed))
           model-options (readiness/read-on-host! host principal handle)
           result {:models (:models model-options) :default-model (:default-model model-options)
                   :activation id :enabled? (true? (:schedule.activation/enabled? row))
                   :at (or (get-in target [:cadence :at]) "02:00")
                   :timezone (or (get-in target [:cadence :tz]) "UTC")
                   :save-transcript? (= "true" (get-in target [:execution :params "save-transcript"]))
                   :model (get-in target [:execution :model]) :limits limits
                   :reason (:schedule.activation/reason row)
                   :active? (some? (:schedule.activation/active-occurrence row))
                   :can-manage? (and (access/can? db (:party original) :grant {:room (:room-id original)})
                                     (access/can? db (:party original) :merge {:room (:room-id original)}))}]
       (reads/preflight-host-read! host principal handle)
       result))))

(defn- inspect-source! [host principal handle]
  (let [original (reads/resolve-host-room! host principal handle)]
    (operation/with-current!
     host principal handle original
     (fn []
       (let [binding (reads/resolve-host-write! host principal handle)
             snapshot (workflows/snapshot-in (:db binding) starter/name)]
         (reads/validate-host-write! host principal handle binding)
         (assoc binding :snapshot snapshot))))))

(defn- ensure-source! [host principal handle]
  (let [binding (inspect-source! host principal handle)
        snapshot (:snapshot binding)]
    (if (seq (:manifest snapshot))
      (do (when-not (inputs/declaration (:files snapshot)) (refuse! :custom-source-policy)) binding)
      (do (let [original (reads/resolve-host-room! host principal handle)]
            (when-not (identical? (:room binding) (:room original)) (refuse! :room-changed))
            (operation/with-current!
             host principal handle original
             #(authoring/write! host principal handle
                                {:name starter/name
                                 :expected-revision (str (sources/source-id binding starter/name (:manifest snapshot)))
                                 :files (starter/source)})))
          (let [current (inspect-source! host principal handle)]
            (when-not (inputs/declaration (get-in current [:snapshot :files])) (refuse! :source-changed))
            current)))))

(defn- ensure-index! [host principal handle]
  (let [original (reads/resolve-host-room! host principal handle)]
    (operation/with-current!
     host principal handle original
     (fn []
       (let [bound (reads/resolve-host-write! host principal handle)]
         (when-not (files/entry (:db bound) documents/index-path)
           (files/save-file-to! (:room bound) (:system bound)
                                {:path documents/index-path :expected nil
                                 :text (documents/index-markdown (get-in bound [:target :repository]))}
                                {:authorize! #(do (reads/validate-host-write! host principal handle bound) true)})))))))

(defn configure! [host principal handle request]
  (when-not (and (= #{:expected-activation :request-id :enabled? :at :timezone :save-transcript? :model}
                    (set (keys request)))
                 (or (nil? (:expected-activation request)) (uuid? (:expected-activation request)))
                 (uuid? (:request-id request)) (boolean? (:enabled? request))
                 (boolean? (:save-transcript? request))
                 (string? (:at request)) (re-matches #"\d{2}:\d{2}" (:at request))
                 (string? (:timezone request)) (<= 1 (count (:timezone request)) 100)
                 (or (nil? (:model request))
                     (and (string? (:model request)) (<= 1 (count (:model request)) 200))))
    (refuse! :invalid-request))
  (operation/with-app!
   host
   (fn []
     (let [original (reads/preflight-host-read! host principal handle)
           {:keys [control-conn execution-ctx]} (::reads/owner host)
           room (:room-id original) party (:party original)
           expected (:expected-activation request)
           db (reads/current-host-control-db! host)
           old (when expected (schedules/activation-in db expected))]
       (when-not (and (= expected (settings/current-in db room))
                      (access/can? db party :grant {:room room})
                      (access/can? db party :merge {:room room})) (refuse! :settings-changed))
       (binding [ec/*execution-context* execution-ctx]
         (when-not (contains? (:schema @control-conn) :room/summary-activation)
           (d/transact control-conn settings/schema))
         (let [candidate
               (when (:enabled? request)
                 (let [source (ensure-source! host principal handle)
                       _ (ensure-index! host principal handle)
                       snapshot (:snapshot source)
                       inspection (workflows/inspect-snapshot starter/name snapshot :program)
                       _ (when-not (:ok inspection) (refuse! :invalid-workflow))
                       model (or (:model request) (get-in (readiness/read-on-host! host principal handle) [:default-model :id]))
                       _ (when-not model (refuse! :model-unavailable))
                       execution {:name starter/name :expected-bundle (parse-uuid (:bundle inspection))
                                  :expected-owned-source (sources/source-id source starter/name (:manifest snapshot))
                                  :model model :params {"save-transcript" (str (:save-transcript? request))} :limits limits}
                       cadence {:every "day" :tz (:timezone request) :at (:at request)}
                       prior (when old (schedules/managed-target control-conn old))]
                   (if (and (= party (:schedule.activation/by old))
                            (= execution (:execution prior))
                            (= (:cadence prior) (scheduler/cadence! cadence)))
                     (do (when-not (and (= (assoc (scheduler/host-policy) :source :owned-retained-source-v2) (:host-policy prior))
                                        (= (scheduler/model-policy :workflow execution) (:model-policy prior)))
                           (refuse! :schedule-policy-changed))
                         old)
                     (scheduler/create-owned! host principal handle
                                              {:request-id (:request-id request) :kind "workflow" :enabled? false
                                               :cadence cadence :execution execution}))))]
           (reads/preflight-host-read! host principal handle)
           (settings/configure! control-conn party room expected old candidate (:enabled? request))
           (read! host principal handle)))))))
