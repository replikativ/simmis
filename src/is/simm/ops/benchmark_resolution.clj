(ns is.simm.ops.benchmark-resolution
  "Fixed HOST reconstruction and preparation journal. Publication authorizes
   before retained-source reads and at the writer. No native construction,
   physical pinning, model dispatch or execution occurs here."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.benchmark-resolutions :as journal]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-source :as source]
            [is.simm.ops.benchmarks :as benchmarks]
            [is.simm.ops.owned-workflows :as owned-source]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]))

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

(def ^:private owned-input-schema (conj benchmarks/schema [:expected-owned-source :uuid]))
(def ^:private origin-schema
  [:map {:closed true} [:revision :uuid] [:target-stamp :uuid]
   [:manifest [:vector {:min 1 :max workflows/max-files} :map]]
   [:target [:and publication/target-schema [:fn publication/host-target?]]]])

(defn- provenance! [request basis retained-source]
  (let [submission (:submission basis) origin (:source-origin basis)]
    (when-not (and (= #{:submission :experiment :team :limits :policy :benchmark/source
                        :admission-channel :source-origin} (set (keys basis)))
                   (m/validate owned-input-schema submission)
                   (#{:host :mcp} (:admission-channel basis))
                   (m/validate origin-schema origin)
                   (= (:revision origin) (:expected-owned-source submission)))
      (refuse! :source-provenance))
    (source/validate-manifest! retained-source (:manifest origin))
    (when-not (= (:revision origin)
                 (owned-source/source-id {:room-id (:work.request/room request)
                                          :target-stamp (:target-stamp origin)}
                                         (:name submission) (:manifest origin)))
      (refuse! :source-provenance))
    ;; Channel/target are retained observations, not historical authorization
    ;; certificates. The original facade must prove its source/lifetime handoff.
    origin))

(defn reconstruct
  "Bounded retained-source read and full regeneration through the existing
   pure compiler. Caller must authorize before reading retained evaluator source.
   Self-consistent source/Experiment hashes are insufficient:
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
    (let [origin (provenance! request basis original)
          pure (benchmarks/prepare-checkpoint-fixture {:files (:files original)}
                                                    (dissoc submission :expected-owned-source))
          prepared (-> pure
                       (assoc :submission-channel (:admission-channel basis))
                       (assoc-in [:basis :submission] submission)
                       (assoc-in [:basis :admission-channel] (:admission-channel basis))
                       (assoc-in [:basis :source-origin] origin))]
      (when-not (= basis (:basis prepared)) (refuse! :basis-mismatch))
      (let [compiled (source/compile-plan original (dissoc (:basis pure) :benchmark/source))]
        (current! conn request)
        (assoc compiled :prepared prepared)))))

(defn- authorized! [db principal room-id request]
  (let [party (benchmarks/authorize-in! db principal room-id)]
    (when-not (and (= party (:work.request/by request))
                   (= room-id (:work.request/room request))
                   (= request (requests/request-in db (:work.request/id request))))
      (refuse! :request-changed))))

(defn- preparation-tx [db principal room-id request plan ref tokens]
  (authorized! db principal room-id request)
  (journal/prepare-tx db request plan ref tokens))

(defn- inventory! [row cases plan]
  (let [id (:benchmark.resolution/id row)]
    (when-not
     (and (every? uuid? [id (:benchmark.resolution/claim row)])
          (#{:preparing :ready} (:benchmark.resolution/state row))
          (= (count (:cases plan)) (count cases))
          (= (mapv :ordinal (:cases plan)) (mapv :benchmark.case/ordinal cases))
          (apply distinct? (map :benchmark.case/root cases))
          (apply distinct? (map :benchmark.case/claim cases))
          (every? (fn [c]
                    (and (= id (:benchmark.case/resolution c))
                         (= (journal/case-id id (:benchmark.case/ordinal c)) (:benchmark.case/id c))
                         (every? uuid? [(:benchmark.case/root c) (:benchmark.case/claim c)])
                         (case (:benchmark.case/state c)
                           :planned (and (nil? (:benchmark.case/selection c))
                                         (nil? (:benchmark.case/selection-id c)))
                           (:pinning :retained) (and (some? (:benchmark.case/selection c))
                                                     (uuid? (:benchmark.case/selection-id c)))
                           false))) cases)
          (case (:benchmark.resolution/state row)
            :preparing (and (nil? (:benchmark.resolution/resolved row))
                            (nil? (:benchmark.resolution/resolved-id row)))
            :ready (and (some? (:benchmark.resolution/resolved row))
                        (uuid? (:benchmark.resolution/resolved-id row))
                        (every? #(= :retained (:benchmark.case/state %)) cases))
            false))
      (refuse! :invalid-inventory))))

(defn- read-preparation! [conn principal room-id request plan tokens published-ref]
  (let [db @conn
        _ (authorized! db principal room-id request)
        row (journal/resolution db (:work.request/id request))
        cases (when row (journal/cases db (:benchmark.resolution/id row)))
        binding (journal/request-binding request)]
    (when-not (and row (= binding (select-keys row (keys binding)))
                   (= (journal/plan-id plan) (:benchmark.resolution/plan-id row))
                   (= (:semantic-id plan) (:benchmark.resolution/semantic-id row)))
      (refuse! :resolution-changed))
    (inventory! row cases plan)
    (when-not (= plan (:value (artifact/get-bounded-value
                              (artifact/datahike-store conn) (:benchmark.resolution/plan row)
                              (* 64 1024))))
      (refuse! :plan-integrity))
    (let [new? (= (:id tokens) (:benchmark.resolution/id row))]
      ;; An ACK (including lost-ACK readback) grants creation rights only to
      ;; this exact fresh family. Observing somebody else's family never does.
      (when (and new?
                 (not (and (= (:claim tokens) (:benchmark.resolution/claim row))
                            (= published-ref (:benchmark.resolution/plan row))
                            (= :preparing (:benchmark.resolution/state row))
                            (= (:cases tokens)
                               (mapv #(hash-map :root (:benchmark.case/root %)
                                                :claim (:benchmark.case/claim %)) cases))
                            (every? #(= :planned (:benchmark.case/state %)) cases))))
        (refuse! :creation-rights-changed))
      (let [current @conn]
        (authorized! current principal room-id request)
        (when-not (and (= row (journal/resolution current (:work.request/id request)))
                       (= cases (journal/cases current (:benchmark.resolution/id row))))
          (refuse! :inventory-changed)))
      {:resolution row :cases cases :plan plan :new? new?})))

(defn- interruption? [error]
  (boolean (some #(instance? InterruptedException %)
                 (take-while some? (iterate ex-cause error)))))

(defn publish-preparation!
  "Authorize and reconstruct an exact retained Request, then reserve its
   immutable bounded case inventory. The original caller owns its App/Room
   lifetime. :new? true identifies only this call's journal reservation;
   native work still requires the caller's original App/Room ownership.
   Retries observe progress without repeating native work. An ambiguous write
   ACK is inspected once, never retried. Returned compiler values are HOST-only."
  [conn principal room-id request]
  (let [principal (select-keys principal [:principal/id])
        _ (authorized! @conn principal room-id request)
        {:keys [plan] :as compiled} (reconstruct conn request)
        tokens {:id (random-uuid) :claim (random-uuid)
                :cases (mapv (fn [_] {:root (random-uuid) :claim (random-uuid)}) (:cases plan))}
        published-ref (volatile! nil)
        _ (authorized! @conn principal room-id request)
        publication-error
        (try
          (artifact/publish-value!
           (artifact/datahike-store conn) plan
           (fn [ref]
             ;; Ephemeral ACK handoff only; never part of captured world state.
             (vreset! published-ref ref)
             (authorized! @conn principal room-id request)
             (d/transact conn [[:db.fn/call preparation-tx principal room-id request plan ref tokens]])))
          nil
          (catch Exception error
            (if (interruption? error)
              (do (.interrupt (Thread/currentThread)) (throw error))
              error)))
        observed (try
                   (read-preparation! conn principal room-id request plan tokens @published-ref)
                   (catch Exception error
                     (if (interruption? error)
                       (do (.interrupt (Thread/currentThread)) (throw error))
                       (throw (or publication-error error)))))]
    (assoc observed :prepared (:prepared compiled))))
