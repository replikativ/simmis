(ns is.simm.uis.web.desktop.admin-remote
  "Spin-remote functions for admin dashboard."
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote] :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            #?(:clj [is.simm.model.system-db :as system-db])
            #?(:clj [is.simm.model.parties :as parties])
            #?(:clj [is.simm.model.billing :as billing])
            #?(:clj [datahike.api :as d])
            #?(:clj [is.simm.uis.web.desktop.remote :as remote])))

#?(:clj
   (defn load-admin-data-server [party-id-str]
     (let [party-id (java.util.UUID/fromString party-id-str)
           profile (parties/get-party party-id)
           _ (when-not (= (:party/role profile) :admin)
               (throw (ex-info "Admin access required" {:type :permission-error})))
           conn (system-db/get-conn)
           ;; Admin view lists all human parties
           humans (->> (d/q '[:find [(pull ?e [:party/id :party/email :party/display-name
                                               :party/role :party/handle :party/created
                                               :actor/created-at :party/last-login]) ...]
                              :where [?e :party/type :human]]
                            @conn)
                       (map (fn [p]
                              ;; the actor core wins, as in `parties/ent->party`
                              (-> (dissoc p :db/id :actor/created-at)
                                  (assoc :party/created (remote/wire-instant
                                                         (or (:actor/created-at p)
                                                             (:party/created p))))
                                  (update :party/last-login remote/wire-instant))))
                       (sort-by :party/email)
                       vec)
           humans-with-budget (mapv (fn [h]
                                      (assoc h :budget
                                             (parties/get-budget (:party/id h))))
                                    humans)
           stats (billing/get-system-stats)]
       {:parties humans-with-budget
        :stats stats})))

(defn-spin-remote load-admin-data!
  [server-id party-id-str]
  (spin-remote server-id [party-id-str]
    (let [pid (identity party-id-str)]
      #?(:clj (load-admin-data-server pid)
         :cljs nil))))
