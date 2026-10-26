(ns is.simm.ops.benchmark-resolution
  "Fixed HOST reconstruction from retained Request source. No authorization,
   artifact publication, pinning, fixture construction or execution occurs here.
   Caller must authorize before reading retained evaluator source."
  (:require [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-source :as source]
            [is.simm.ops.benchmarks :as benchmarks]))

(defn- refuse! [reason]
  (throw (ex-info "Benchmark reconstruction is held" {:type ::held :reason reason})))

(defn- current! [conn expected]
  (when-not (and expected (= expected (requests/get-request conn (:work.request/id expected)))
                 (= :benchmark (:work.request/kind expected))
                 (= :submitted (:work.request/state expected))
                 (true? (:work.request/return-pending? expected))
                 (every? uuid? (map expected [:work.request/id :work.request/by :work.request/room
                                              :work.request/client-id :work.request/claim
                                              :work.request/intent-id :work.request/job]))
                 (= (:work.request/id expected)
                    (requests/request-id (:work.request/by expected) (:work.request/room expected)
                                         (:work.request/client-id expected))))
    (refuse! :request-changed)))

(defn reconstruct
  "Bounded retained-source read and full regeneration through the existing
   pure compiler. Self-consistent source/Experiment hashes are insufficient:
   every field of the original canonical basis must match. Runtime/model drift
   holds rather than replacing the original team or capabilities. Returning
   these HOST values conveys neither native creation rights nor dispatch."
  [conn request]
  (current! conn request)
  (let [retained (:value (artifact/get-bounded-value
                         (artifact/datahike-store conn) (:work.request/payload-ref request)
                         (* 128 1024 1024)))
        basis (:input retained)
        original (:benchmark/source basis)
        submission (:submission basis)]
    (when-not (and (map? retained) (= #{:kind :input :limits} (set (keys retained)))
                   (= :benchmark (:kind retained))
                   (= (:work.request/intent-id request) (hasch/uuid retained))
                   (= (:work.request/client-id request) (:request-id submission))
                   (= (:limits retained) (update (:limits submission) :settlement keyword)))
      (refuse! :request-integrity))
    (source/reconstruct original)
    (let [prepared (benchmarks/prepare-checkpoint-fixture {:files (:files original)} submission)]
      (when-not (= basis (:basis prepared)) (refuse! :basis-mismatch))
      (let [compiled (source/compile-plan original (dissoc basis :benchmark/source))]
        (current! conn request)
        (assoc compiled :prepared prepared)))))
