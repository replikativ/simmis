(ns is.simm.ops.benchmark-result-evidence
  "Read-only correspondence to the exact admitted execution. READY evidence
   supplies checkpoint-substituted definitions without granting execution or
   rebuilding checkers, builders or now-retired native roots."
  (:require [dvergr.agent.experiment :as experiment]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.benchmark-resolutions :as journal]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-case-evidence :as evidence]))

(defn- refuse! []
  (throw (ex-info "Benchmark result evidence unavailable" {:type ::unavailable})))

(defn experiment!
  "Internal bounded reader, not authority. Caller owns its original native
   lifetime and separately checks current access before displaying anything."
  [conn original]
  (let [db @conn
        request (requests/request-in db (:work.request/id original))
        binding (journal/request-binding original)
        _ (when-not (and request (= binding (journal/request-binding request))) (refuse!))
        store (artifact/datahike-store conn)
        retained (:value (artifact/get-bounded-value store (:work.request/payload-ref request) (* 32 1024 1024)))
        _ (when-not (= (:work.request/intent-id original) (hasch/uuid retained)) (refuse!))
        basis (:input retained)]
    (experiment/validate-experiment
     (if-not (contains? basis :benchmark/source)
       (:experiment basis)
       (let [row (journal/resolution db (:work.request/id request))
             _ (when-not (and (= :benchmark (:work.request/kind request))
                              (= :ready (:benchmark.resolution/state row))
                              (= binding (select-keys row (keys binding)))
                              (every? uuid? (map row [:benchmark.resolution/id :benchmark.resolution/claim
                                                      :benchmark.resolution/plan-id :benchmark.resolution/resolved
                                                      :benchmark.resolution/resolved-id]))) (refuse!))
             body (:value (artifact/get-bounded-value store (:benchmark.resolution/resolved row) (* 32 1024 1024)))
             cases (journal/cases db (:benchmark.resolution/id row))]
         (when-not
          (and (= (:benchmark.resolution/resolved-id row) (evidence/execution-id body))
               (= binding (:request body))
               (= {:id (:benchmark.resolution/id row) :claim (:benchmark.resolution/claim row)
                   :plan-id (:benchmark.resolution/plan-id row)} (:resolution body))
               (= 1 (:version body)) (vector? (:cases body)) (<= 1 (count cases) 8)
               (= (count cases) (count (:cases body)))
               (every? true?
                       (map-indexed
                        (fn [ordinal row]
                          (let [record (nth (:cases body) ordinal)]
                            (and (= :retained (:benchmark.case/state row))
                                 (= ordinal (:benchmark.case/ordinal row) (:ordinal record))
                                 (= (:benchmark.case/id row) (:id record))
                                 (= (:benchmark.case/claim row) (:claim record))
                                 (= (:benchmark.case/root row) (:root record))
                                 (= (:benchmark.case/selection-id row) (:evidence-id record))
                                 (= (:benchmark.case/selection row) (:evidence-ref record))))) cases)))
           (refuse!))
         (:experiment body))))))
