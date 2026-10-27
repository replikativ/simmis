(ns is.simm.ops.benchmark-case-evidence
  "Pure case evidence, regenerated from canonical retained inputs. Hashes and
   manifests prove correspondence of data, not physical checkpoint contents,
   authorization, retention or cleanup. Those remain HOST responsibilities."
  (:require [hasch.core :as hasch]
            [is.simm.model.benchmark-resolutions :as journal]
            [is.simm.model.work-request-checkpoints :as checkpoints]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.benchmark-case-manifest :as manifest]
            [is.simm.ops.benchmark-case-program :as program]
            [is.simm.ops.benchmark-source :as source]
            [dvergr.catalog.workspace :as fixtures]))

(defn- refuse! [reason]
  (throw (ex-info "Benchmark case evidence refused" {:type ::invalid :reason reason})))

(defn- template! [c0]
  (when-not (and (map? c0) (= #{:selection :manifest} (set (keys c0))))
    (refuse! :template-shape))
  (checkpoints/validate-selection! (:selection c0))
  (manifest/template-id (:manifest c0)))

(defn case-body
  "Build bounded portable evidence for one ordinal without reading a database.
   Lifecycle states are deliberately excluded: the same body identifies a case
   before pinning and after settlement. Fresh creation rights and current
   authority must be checked separately by the fixed admitting HOST."
  [request resolution plan row retained-source c0 selection]
  (journal/validate-plan! plan)
  (let [binding (journal/request-binding request)
        ordinal (:benchmark.case/ordinal row)
        resolution-id (:benchmark.resolution/id resolution)]
    (when-not (and (every? uuid? (vals binding))
                   (= :benchmark (:work.request/kind request))
                   (= (:work.request/id request)
                      (requests/request-id (:work.request/by request) (:work.request/room request)
                                           (:work.request/client-id request)))
                   (= binding (select-keys resolution (keys binding)))
                   (uuid? resolution-id) (uuid? (:benchmark.resolution/claim resolution))
                   (= (journal/plan-id plan) (:benchmark.resolution/plan-id resolution))
                   (= (:semantic-id plan) (:benchmark.resolution/semantic-id resolution))
                   (= (:source-id plan) (source/source-id retained-source))
                   (integer? ordinal) (<= 0 ordinal) (< ordinal (count (:cases plan)))
                   (= resolution-id (:benchmark.case/resolution row))
                   (= (journal/case-id resolution-id ordinal) (:benchmark.case/id row))
                   (uuid? (:benchmark.case/root row)) (uuid? (:benchmark.case/claim row)))
      (refuse! :case-binding))
    (let [case-plan (nth (:cases plan) ordinal)
          files (source/case-files retained-source (:case case-plan))
          template-id (template! c0)
          _ (checkpoints/validate-selection! selection)
          identity-keys [:registry-system :runtime-system :store :repository :backend]]
      (when-not (= (:manifest-id case-plan)
                   (hasch/uuid [:simmis/benchmark-case-fixtures-v1 files]))
        (refuse! :source-manifest))
      (when-not (= (select-keys (get-in c0 [:selection :target]) identity-keys)
                   (select-keys (:target selection) identity-keys))
        (refuse! :private-workspace-changed))
      {:version 1 :request binding
       :resolution {:id resolution-id :claim (:benchmark.resolution/claim resolution)
                    :plan-id (journal/plan-id plan)}
       :case {:id (:benchmark.case/id row) :claim (:benchmark.case/claim row)
              :root (:benchmark.case/root row) :ordinal ordinal
              :source-case (:case case-plan) :source-manifest-id (:manifest-id case-plan)}
       :program-id (:semantic-id plan)
       :template {:selection (:selection c0) :manifest (:manifest c0) :id template-id}
       :manifest (manifest/after-seeding (:manifest c0) files fixtures/fixture-seed-limits)
       :selection selection})))

(defn validate-case!
  "Regenerate every field, including the complete expected manifest. An outer
   self-consistent hash cannot substitute for the original retained inputs.
   Historical validation does not grant a retry or require a pending Request."
  [body request resolution plan row retained-source c0]
  (when-not (and (map? body)
                 (= body (case-body request resolution plan row retained-source c0 (:selection body))))
    (refuse! :case-integrity))
  body)

(defn content-id [body]
  (hasch/uuid [:simmis/benchmark-case-evidence-v1 body]))

(defn root-owner
  "Diagnostic integrity metadata for a physical pin, never store authority.
   The HOST validates the case body before deriving this immutable owner."
  [body]
  {:simmis/benchmark-case-root (get-in body [:case :root])
   :request (get-in body [:request :benchmark.resolution/request])
   :job (get-in body [:request :benchmark.resolution/job])
   :resolution (get-in body [:resolution :id]) :resolution-claim (get-in body [:resolution :claim])
   :case (get-in body [:case :id]) :case-claim (get-in body [:case :claim])
   :evidence (content-id body)})

(defn resolve-execution
  "Build portable READY evidence and the HOST runner plan from a freshly
   reconstructed canonical program and exact ordered retained case records.
   The fixed coordinator proves physical roots and the builder cleanup barrier
   before publication; this pure constructor cannot prove either. It grants no
   retry, source access, admission, funding or execution."
  [request preparation compiled c0 records]
  (let [{:keys [source plan prepared]} compiled
        resolution (:resolution preparation)
        original (:plan prepared)
        basis (-> (:basis prepared)
                  (select-keys [:submission :experiment :team :limits :policy])
                  (update :submission dissoc :expected-owned-source))
        regenerated (source/compile-plan source basis)
        family-keys [:db/id :benchmark.case/id :benchmark.case/resolution :benchmark.case/ordinal
                     :benchmark.case/claim :benchmark.case/root]]
    (when-not (and (= plan (:plan preparation)) (= plan (:plan regenerated))
                   (= (:experiment original) (:experiment basis)) (= (:team original) (:team basis))
                   (vector? records) (= (count (:cases plan)) (count records))
                   (= (mapv #(select-keys % family-keys) (:cases preparation))
                      (mapv #(select-keys (:case %) family-keys) records)))
      (refuse! :execution-binding))
    (doseq [[ordinal {:keys [case body id ref] :as record}] (map-indexed vector records)]
      (when-not (and (map? record) (= #{:case :body :id :ref} (set (keys record)))
                     (= ordinal (:benchmark.case/ordinal case))
                     (= :retained (:benchmark.case/state case))
                     (uuid? ref) (= ref (:benchmark.case/selection case))
                     (= id (:benchmark.case/selection-id case)) (= id (content-id body)))
        (refuse! :retained-case-binding))
      (validate-case! body request resolution plan case source c0))
    (let [template-id (template! c0)
          bindings (mapv (fn [{:keys [case body]}]
                           {:ordinal (:benchmark.case/ordinal case)
                            :manifest-id (manifest/visible-id (:manifest body)) :selection (:selection body)})
                         records)
          resolved (program/resolve-plan original (:semantic-id plan) template-id bindings)
          environments (get-in resolved [:plan :environments])
          body {:version 1 :request (journal/request-binding request)
                :resolution {:id (:benchmark.resolution/id resolution)
                             :claim (:benchmark.resolution/claim resolution) :plan-id (journal/plan-id plan)}
                :source-id (:source-id plan) :provenance (:provenance resolved) :template c0
                :cases (mapv (fn [{:keys [case body id ref]} environment]
                               {:ordinal (:benchmark.case/ordinal case) :id (:benchmark.case/id case)
                                :claim (:benchmark.case/claim case) :root (:benchmark.case/root case)
                                :evidence-id id :evidence-ref ref
                                :visible-manifest-id (manifest/visible-id (:manifest body))
                                :setup (get-in environment [:environment/world :setup])})
                             records environments)
                :experiment (get-in resolved [:plan :experiment])}]
      {:body body :plan (:plan resolved)})))

(defn execution-id [body]
  (hasch/uuid [:simmis/benchmark-resolved-execution-v1 body]))
