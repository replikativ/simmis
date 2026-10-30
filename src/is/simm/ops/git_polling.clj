(ns is.simm.ops.git-polling
  "Finite incoming checks on the original App scheduler. No timer, executor,
   replacement host, push, or recovery of uncertain native effects."
  (:require [datahike.api :as d]
            [geschichte.git.http :as http]
            [is.simm.documents.github-source :as github]
            [is.simm.model.git-policy :as policy]
            [is.simm.model.github-sync :as journal]
            [is.simm.model.github-integrations :as integrations]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.github-incoming :as incoming]
            [is.simm.ops.github-service :as service]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(def ^:private checks-per-pass 4)

(defn- refuse! [] (throw (ex-info "Scheduled Git check is held" {:type ::held})))

(defn- current-policy! [context expected]
  (host/current! context)
  (let [{:keys [connection cursor]} (host/connection! context)
        current (policy/effective @(:conn context) (:room connection) (:github.integration/connection-id cursor))]
    (when-not (and (= (:git.policy/room expected) (:room connection))
                   (= (:git.policy/connection-id expected) (:github.integration/connection-id cursor))
                   (= (:git.policy/revision expected) (:git.policy/revision current))
                   (= (:git.policy/by current) (get-in context [:resolved :party]))
                   (true? (:git.policy/enabled? current))) (refuse!))
    {:connection connection :cursor cursor :policy current}))

(defn- check-in! [context expected now changed!]
  (let [context (incoming/admitted-context context)
        {:keys [connection cursor policy]} (current-policy! context expected)]
    (cond
      (not (policy/due? policy now)) {:status :skipped}
      (journal/active-in @(:conn context) (:room connection)) {:status :busy}
      (policy/pending-proposal @(:conn context) policy) {:status :proposal}
      :else
      (let [refs (http/ls-remote! http/send-http (github/public-url (:source connection))
                                  {:prefixes [(:ref connection)] :import-budget (:git-operation context)
                                   :http-body-idle-timeout-ms 15000})
            matches (vec (take 2 (filter #(= (:ref connection) (:ref %)) refs)))
            oid (:oid (first matches))]
        (when-not (and (= 1 (count matches)) (facts/git-oid? oid)) (refuse!))
        (current-policy! context expected)
        (if (= (:github.integration/oid cursor) oid)
          {:status :unchanged}
          (do (changed!) (service/poll-in! context expected)))))))

(defn- checked! [original expected status]
  (reads/current-host-control-db! original)
  (let [conn (:control-conn (::reads/owner original)) ticket lifetime/*ticket*
        ;; Actual completion time avoids catch-up loops after slow fetches.
        now (java.util.Date.)]
    (d/transact conn
                [[:db.fn/call
                  (fn [db]
                    (when-not (and (lifetime/retained? ticket) (lifetime/ready? (:owner ticket))) (refuse!))
                    (policy/checked-tx db (:git.policy/room expected) (:git.policy/connection-id expected)
                                       (:git.policy/revision expected) now status))]])))

(defn tick-owned!
  "Called only inside the existing scheduler's retained original App pass.
   Inspect at most four due rows and stop after one changed-ref preparation.
   Disabled rows carry no indexed due time. The index supplies durable fairness."
  [original now]
  (when-not (inst? now) (refuse!))
  (binding [ec/*execution-context* (:execution-ctx original)]
    (let [db (reads/current-host-control-db! original)
          rows (mapv #(d/pull db '[*] (:e %))
                     (take checks-per-pass
                           (take-while #(<= (.getTime ^java.util.Date (:v %)) (.getTime ^java.util.Date now))
                                       (d/datoms db :avet :git.policy/next-check))))]
      (loop [remaining rows]
        (when-let [expected (first remaining)]
          (reads/current-host-control-db! original)
          (let [changed? (volatile! false)
                result
                (try
                  (host/in! original (:git.policy/by expected) (:git.policy/room expected) :prepare
                            #(check-in! % expected now (fn [] (vreset! changed? true))))
                  (catch Exception error
                    (when (or (adoption/causal-interruption? error) (.isInterrupted (Thread/currentThread)))
                      (.interrupt (Thread/currentThread)) (throw error))
                    ;; Fencing/ownership expiry is fatal to this old worker.
                    (reads/current-host-control-db! original)
                    {:status (if (= :busy (facts/error-reason error)) :busy :held)}))
                status (case (:status result) (:applied :accepted) :synced :filed :proposal
                             (:unchanged :closed) :unchanged :busy :busy :proposal :proposal :held)]
            (when-not (= :skipped (:status result)) (checked! original expected status))
            (when-not @changed? (recur (next remaining)))))))))
