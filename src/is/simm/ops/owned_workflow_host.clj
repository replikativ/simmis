(ns is.simm.ops.owned-workflow-host
  "Fixed canonical human workflow authorization. Transport identity and tool
   ceilings remain the responsibility of the real browser/MCP boundary."
  (:require [clojure.string :as str]
            [is.simm.model.access :as access]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(def owned-actions
  {:simmis/workspace-bundle :read :simmis/workspace-bundle-chunk :read
   :simmis/benchmark-report-prepare :write :simmis/benchmark-report-publish :write
   :simmis/benchmark-result-retry :write
   :simmis/workflow-list :read :simmis/workflow-source :read
   :simmis/workflow-check :read :simmis/workflow-schedule-list :read
   :simmis/workflow-write :write
   :simmis/workflow-run :merge :simmis/benchmark :merge :simmis/cli-benchmark :merge
   :simmis/workflow-output-collect :merge
   :simmis/workflow-schedule-create :merge :simmis/workflow-schedule-set-enabled :merge
   :simmis/work-status :merge
   :simmis/local-worker-workflow :write :simmis/local-worker-benchmark :write})

(def legacy-room-actions
  ;; Normal browser controls whose existing service still needs a native Room.
  ;; Not part of MCP's owned operation selection or a new admission authority.
  {:simmis/work-allowance :merge :simmis/cli-allowance :merge
   :simmis/local-worker-list :read :simmis/local-worker-allowance :write})

(def ^:private actions (merge owned-actions legacy-room-actions))

(defn- refuse! [] (throw (ex-info "Canonical workflow operation refused" {:type ::refused})))

(defn authorize-in!
  "HOST fixed policy over ONE current immutable control DB. No hydration,
   synthetic resolved Room or retained decision. Does not authenticate MCP."
  [db principal {:keys [op action resource args] :as request}]
  (let [[kind handle] resource required (get actions op)]
    (when-not (and required (= required action) (vector? resource) (= 2 (count resource))
                   (= :room kind) (string? handle) (<= 1 (count handle) 1024)
                   (not (str/blank? handle)) (not (re-find #"[\p{Cntrl}]" handle)))
      (refuse!))
    (let [descriptor (reads/authorize-read! db principal handle)]
      (when-not (access/can? db (:party descriptor) required {:room (:room-id descriptor)})
        (refuse!))
      (when (contains? request :args)
        (when-not (and (map? args)
                       (= (:room-id descriptor)
                          (:room-id (reads/authorize-read! db principal (:room args)))))
          (refuse!))))
    {:allow? true}))

(defn host
  "Captured normal-human HOST for fixed owned workflow operations. This does
   not install a transport, fabricate an MCP identity or hydrate a cold room."
  [conn host-ctx]
  (-> (ops/host host-ctx)
      (assoc :is.simm.ops.owned-reads/owner (reads/host-owner conn host-ctx))
      (assoc-in [:config :auth :authorize]
                (fn [principal {:keys [op] :as request}]
                  (try
                    (binding [ec/*execution-context* host-ctx]
                      (let [db (reads/current-control-db! conn host-ctx)]
                        (if (contains? actions op)
                          (authorize-in! db principal request)
                          (ops/authorize principal request))))
                    (catch InterruptedException _
                      (.interrupt (Thread/currentThread))
                      {:allow? false})
                    (catch Throwable _ {:allow? false}))))))

(defn from-application
  "Build the normal-human policy under an entered original App operation.
   Copy only its exact owner/lifetime, never MCP transport identity or ceilings."
  [application-host]
  (lifetime/check-current!)
  (reads/current-host-control-db! application-host)
  (let [{:keys [control-conn execution-ctx]} (::reads/owner application-host)
        enter (get-in application-host [:config :host-lifecycle :with-operation])]
    (when-not (fn? enter) (refuse!))
    (assoc-in (host control-conn execution-ctx) [:config :host-lifecycle :with-operation] enter)))
