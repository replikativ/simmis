(ns is.simm.ops.coordinator
  "Explicit recursive work admission through the existing work journal and Jobs."
  (:require [is.simm.model.work-requests :as requests]
            [is.simm.ops.workflow-profiles :as profiles]
            [is.simm.ops.coordinator-benchmark :as benchmark]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as room-operation]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.work :as work]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def schema (conj work/task-schema [:benchmark {:optional true} benchmark/schema]))

(defn prepare [input]
  (let [base (profiles/prepare (work/prepare input) {:profile "coordinator"})]
    (-> base
        (assoc :request-kind :coordinator :execution-kind :workflow-coordinator)
        (assoc-in [:basis :submission] input))))

(defn submit! [conn host-ctx principal room-id room input]
  (when-not (m/validate work/task-schema input)
    (throw (ex-info "Invalid bounded task" {:type :is.simm.ops.work/invalid})))
  (binding [ec/*execution-context* host-ctx]
    (let [party (work/authorize! conn principal room-id)
          id (requests/request-id party room-id (:request-id input))]
      (if-let [row (requests/get-request conn id)]
        (do
          (when-not (and (= :coordinator (:work.request/kind row))
                         (= input (get-in (requests/payload conn row) [:input :submission])))
            (throw (ex-info "Request identity was used for different work"
                            {:type :is.simm.model.work-requests/conflict})))
          (work/status conn host-ctx principal room-id id))
        (work/submit-prepared! conn host-ctx principal room-id room input (prepare input))))))

(defn- submit-owned-in!
  "Explicit inspected benchmark offer. Ordinary coordinators retain their
   existing endpoint; this path requires an owned host and operator admission."
  [host principal handle input]
  (when-not (and (m/validate schema input) (:benchmark input) (reads/owned-host? host))
    (throw (ex-info "Invalid owned coordinator benchmark" {:type ::invalid})))
  (let [{:keys [control-conn execution-ctx]} (::reads/owner host)
        original (reads/preflight-host-read! host principal handle)
        room-id (:room-id original)]
    (binding [ec/*execution-context* execution-ctx]
      (let [party (benchmarks/authorize-in! (:control-db original) principal room-id)
            id (requests/request-id party room-id (:request-id input))]
        (if-let [row (requests/get-request control-conn id)]
          (do
            (when-not (and (= :coordinator (:work.request/kind row))
                           (= input (get-in (requests/payload control-conn row) [:input :submission])))
              (throw (ex-info "Request identity was used for different work"
                              {:type :is.simm.model.work-requests/conflict})))
            (work/status control-conn execution-ctx principal room-id id))
          (let [room-binding (reads/resolve-host-room! host principal handle)
                offer (room-operation/with-current!
                        host principal handle room-binding
                        #(benchmark/compile! host principal handle room-binding input))
                ;; Provider preparation and native forks must not inherit a
                ;; source-operation lease, even when creation returns a handle.
                base (prepare (dissoc input :benchmark))
                _ (room-operation/with-current!
                    host principal handle room-binding
                    #(benchmark/current! host principal handle room-binding offer))
                prepared (benchmark/attach control-conn execution-ctx principal room-id
                                           room-binding host base input offer)]
            (work/submit-prepared! control-conn execution-ctx principal room-id
                                   (:room room-binding) input prepared)))))))

(defn submit-owned! [host principal handle input]
  (if-let [with-operation (get-in host [:config :host-lifecycle :with-operation])]
    (with-operation #(submit-owned-in! host principal handle input))
    (throw (ex-info "Coordinator benchmark requires its original App owner" {:type ::invalid}))))
