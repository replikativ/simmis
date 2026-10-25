(ns is.simm.ops.owned-schedule-fixture
  "The existing real shared Room/control fixture plus an actual App lifetime;
   installs no dispatcher, model call, timer, transport or schedule itself."
  (:require [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn with-fixture [order f]
  (@#'fixture/with-fixture
   order
   (fn [data]
     (let [owner (doto (lifetime/create) lifetime/open!)]
       (try
         (f (-> data (assoc :app-owner owner)
                (assoc-in [:host :config :host-lifecycle :with-operation]
                          #(lifetime/with-operation! owner %))))
         (finally (lifetime/fence! owner) (lifetime/join! owner 5000)))))))
