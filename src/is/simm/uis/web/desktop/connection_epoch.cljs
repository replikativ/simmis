(ns is.simm.uis.web.desktop.connection-epoch
  "Low-level authenticated socket identity, shared without a web/signals cycle.
   A new socket epoch is not evidence that any replica or aggregate is ready."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(def connection (signal runtime nil))

(defn snapshot []
  (binding [ec/*execution-context* runtime] @connection))

(defn current? [epoch]
  (and (some? epoch) (= epoch (snapshot))))
