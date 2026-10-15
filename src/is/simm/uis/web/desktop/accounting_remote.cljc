(ns is.simm.uis.web.desktop.accounting-remote
  "Spin-remotes for the Accounting perspective and a team's book. Thin
   pass-throughs over `is.simm.ops.accounting-report` (the workspace index) and
   `is.simm.ops.book-report` (one book); the party comes from the JWT principal
   and every book is `can?`-checked there. Dates cross the wire as epoch
   milliseconds, room ids as strings."
  (:require [org.replikativ.spindel.distributed.macros :refer [defn-spin-remote]
             :include-macros true]
            [org.replikativ.spindel.distributed.core :as dist]
            #?(:clj [is.simm.ops.accounting-report :as report])
            #?(:clj [is.simm.ops.book-report :as book-report])
            #?(:clj [is.simm.model.access :as access])
            #?(:clj [is.simm.runtimes.context :as ctx])))

(defn-spin-remote load-position!
  [server-id]
  (spin-remote server-id []
    #?(:clj (ctx/with-server-context
              (if-let [party (access/authenticated-party-id)]
                (mapv #(update % :room str) (report/workspace-position party))
                (throw (ex-info "authentication required"
                                {:error :authentication-required}))))
       :cljs nil)))

#?(:clj
   (defn- book-opts
     "`{:from :through :as-of-tx}` as epoch ms -> Dates; absent stays absent."
     [opts]
     (into {} (keep (fn [k] (when-let [v (get opts k)] [k (java.util.Date. (long v))])))
           [:from :through :as-of-tx])))

#?(:clj
   (defn- party! []
     (or (access/authenticated-party-id)
         (throw (ex-info "authentication required" {:error :authentication-required})))))

(defn-spin-remote load-book!
  [server-id room-id-str opts]
  (spin-remote server-id [room-id-str opts]
    (let [rid (identity room-id-str)]
      #?(:clj (ctx/with-server-context
                (book-report/book-report (party!) (java.util.UUID/fromString rid)
                                         (book-opts opts)))
         :cljs nil))))

(defn-spin-remote load-account-ledger!
  [server-id room-id-str path opts]
  (spin-remote server-id [room-id-str path opts]
    (let [rid (identity room-id-str)]
      #?(:clj (ctx/with-server-context
                (book-report/account-ledger (party!) (java.util.UUID/fromString rid)
                                            path (book-opts opts)))
         :cljs nil))))
