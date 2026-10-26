(ns is.simm.authority.application-fixture
  "A real captured App registration/lifetime for isolated browser boundary tests.
   Operation registration remains the caller's fixture responsibility."
  (:require [is.simm.ops.owned-mcp :as owned-mcp]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn with-application [conn ctx f]
  (let [owner (lifetime/create)
        host (owned-mcp/host conn ctx owner)
        slot (atom {:owner owner :host host})]
    (lifetime/open! owner)
    (try
      (with-redefs-fn {#'application/installation slot}
        #(f {:app-owner owner :app-host host :installation-slot slot}))
      (finally (lifetime/fence! owner) (lifetime/join! owner 5000)))))
