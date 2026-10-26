(ns is.simm.model.work-request-identity
  "Shared pure request identity; no journal, admission or execution authority."
  (:require [hasch.core :as hasch]))

(defn request-id [party room client-id]
  (hasch/uuid [:simmis/work-request-v1 party room client-id]))
