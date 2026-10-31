(ns is.simm.model.room-summary-settings
  "One Room pointer to an ordinary activation. No second enabled flag, timer,
   budget or execution ledger. Control changes serialize with admission."
  (:require [datahike.api :as d]
            [is.simm.model.access :as access]
            [is.simm.model.workflow-schedules :as schedules]))

(def schema [{:db/ident :room/summary-activation :db/valueType :db.type/uuid
              :db/cardinality :db.cardinality/one}])

(defn current-in [db room]
  (when (contains? (:schema db) :room/summary-activation)
    (:room/summary-activation (d/entity db [:room/id room]))))

(defn- conflict! []
  (throw (ex-info "Daily summary settings changed; reload Room settings."
                  {:type ::conflict})))

(defn- configure-tx [db party room expected old candidate enabled?]
  (when-not (and (access/can? db party :grant {:room room})
                 (access/can? db party :merge {:room room}))
    (throw (ex-info "Room owner and merge authority required" {:type ::denied})))
  (when-not (= expected (current-in db room)) (conflict!))
  (let [prior (when expected (schedules/activation-in db expected))
        next (when candidate (schedules/activation-in db (:schedule.activation/id candidate)))
        same? (= expected (:schedule.activation/id candidate))
        active (when-let [id (:schedule.activation/active-occurrence prior)]
                 (schedules/occurrence-in db id))]
    (when-not (= old prior) (conflict!))
    (when (and candidate
               (not (and (= candidate next) (= room (:schedule.activation/room next))
                         (= party (:schedule.activation/by next))))) (conflict!))
    (when (and enabled? candidate
               (or (:schedule.activation/reason next)
                   (not (schedules/resumable-active?
                         next (when-let [id (:schedule.activation/active-occurrence next)]
                                (schedules/occurrence-in db id))))))
      (conflict!))
    ;; Replacement cannot strand already-admitted work or enable two paid Runs.
    ;; Off only stops future work; a live occurrence keeps its normal lifecycle.
    (when (and candidate (not same?)
               (or (:schedule.activation/enabled? next)
                   (contains? #{:admitting :linked} (:schedule.occurrence/state active))))
      (conflict!))
    (vec (concat
          (when prior
            (schedules/enabled-tx db expected (boolean (and same? enabled?))))
          (when (and candidate (not same?))
            (schedules/enabled-tx db (:schedule.activation/id candidate) enabled?))
          (when candidate
            [{:db/id (:db/id (d/entity db [:room/id room]))
              :room/summary-activation (:schedule.activation/id candidate)}])))))

(defn configure!
  "Caller prepares a paused candidate through ordinary owned schedule admission.
   Transaction rechecks authority, original pointer and inspected control rows."
  [conn party room expected old candidate enabled?]
  (d/transact conn [[:db.fn/call configure-tx party room expected old candidate enabled?]])
  (current-in @conn room))
