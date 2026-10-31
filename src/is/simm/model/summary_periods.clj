(ns is.simm.model.summary-periods
  "Pure calendar boundaries; no clock, scheduler or database access."
  (:import [java.time ZoneId ZonedDateTime]
           [java.util Date]))

(defn closed-day [^Date at timezone]
  (let [zone (ZoneId/of timezone)
        date (.minusDays (.toLocalDate (ZonedDateTime/ofInstant (.toInstant at) zone)) 1)]
    {:kind :day :timezone timezone
     :from (Date/from (.toInstant (.atStartOfDay date zone)))
     :until (Date/from (.toInstant (.atStartOfDay (.plusDays date 1) zone)))}))

(defn day-label [period]
  (str (.toLocalDate (ZonedDateTime/ofInstant (.toInstant ^Date (:from period))
                                             (ZoneId/of (:timezone period))))))
