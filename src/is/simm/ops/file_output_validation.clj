(ns is.simm.ops.file-output-validation
  "INTERNAL fixed canonical preflight and exact AFTER snapshot guard. No
   authority, controller activation, transaction, native handles or callbacks."
  (:require [geschichte.query :as query]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.preservation :as preservation]
            [is.simm.documents.profile :as knowledge]
            [is.simm.documents.space :as space]
            [is.simm.documents.view :as view]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.knowledge-outputs :as outputs]
            [malli.core :as m]))

(def profile {:id :simmis/canonical-file-publication-validation :version 1})
(def ^:private physical-keys [:repository :store :backend :branch :ref])
(def ^:private max-preservation-nodes 200000)

(defn- refuse! [reason]
  (throw (ex-info "Canonical file publication validation refused"
                  {:type ::refused :reason reason})))

(defn- target-in [db]
  {:repository (:geschichte.repo/id (query/repository db))
   :store (get-in db [:config :store :id]) :backend (get-in db [:config :store :backend])
   :branch (get-in db [:config :branch]) :ref (query/current-ref db)})

(defn- target! [db target]
  (when-not (and (publication/host-target? target) (m/validate publication/target-schema target)
                 (= (target-in db) (select-keys target physical-keys)))
    (refuse! :target-changed)))

(defn- entry [descriptor]
  {:content (:content descriptor) :size (:bytes descriptor) :mode (:mode descriptor)})

(defn- descriptor? [value]
  (and (map? value) (= #{:content :size :mode} (set (keys value)))
       (uuid? (:content value)) (integer? (:size value))
       (<= 0 (:size value) markdown/max-source-bytes) (= 33188 (:mode value))))

(defn- witness! [witness]
  ;; This is HOST validation data, never a transport capability. Closed shape
  ;; and bounded acquisition prevent absent scope/dependency data becoming an
  ;; accidentally equal empty proof. Final comparisons use freshly computed
  ;; canonical data, never execute anything from this map.
  (let [{:keys [selection typed-scopes constraint-dependencies after]} witness]
    (when-not
     (and (map? witness)
          (= #{:profile :preservation :manifest :target :selection :typed-scopes :constraint-dependencies :after}
             (set (keys witness)))
          (= profile (:profile witness)) (= preservation/profile (:preservation witness))
          (uuid? (:manifest witness))
          (map? selection) (= space/profile (:entity-profile selection))
          (= [""] (:roots selection)) (= [] (:exclude-prefixes selection))
          (map? (:files selection)) (<= (count (:files selection)) 200)
          (every? descriptor? (vals (:files selection)))
          (vector? typed-scopes) (<= 1 (count typed-scopes) 256)
          (every? #(and (map? %) (= #{:declarations :revision} (set (keys %)))
                        (uuid? (:revision %)) (map? (:declarations %))) typed-scopes)
          (map? constraint-dependencies)
          (= #{:codomains :external-targets :unique-values} (set (keys constraint-dependencies)))
          (every? set? (vals constraint-dependencies))
          (<= (reduce + 0 (map count (vals constraint-dependencies))) 50000)
          (map? after) (<= 1 (count after) (:max-files outputs/policy))
          (every? descriptor? (vals after)))
      (refuse! :invalid-validation-witness))
    (doseq [path (keys after)] (outputs/target-path! path))
    witness))

(defn prepare-in
  "Validate retained exact BEFORE/AFTER bodies against ONE fresh canonical
   destination snapshot. Normal selection only, never execution exclusions.
   Existing file baselines must match the manifest; new paths require complete
   absence. The versioned preservation policy is additional to canonical checks.

   Returned immutable HOST data freezes THIS preflight through publication. It
   does not certify an earlier human UI review or unchanged agent admission
   context. No body bytes, DB values, native handles or executable objects are
   retained in the witness. Public Accept needs its own expected review revision."
  [db {:keys [manifest bodies] :as plan}]
  (plans/validate-plan! plan)
  (target! db (:target manifest))
  (let [changes (into (sorted-map)
                      (for [[path pair] bodies]
                        [path {:expected (when (= :present (get-in manifest [:entries path :before :state]))
                                           (entry (get-in manifest [:entries path :before])))
                               :source (markdown/decode (outputs/bytes! (:after pair)))}]))
        preview (view/validate-changes db knowledge/selection-options changes)]
    (when-not (= :valid (:status preview)) (refuse! :invalid-canonical-output))
    (reduce-kv
     (fn [nodes _path {:keys [before after]}]
       (if-not before
         nodes
         (let [before (markdown/parse (markdown/decode (outputs/bytes! before)))
               after (markdown/parse (markdown/decode (outputs/bytes! after)))
               nodes (+ nodes (reduce + 0 (map #(count (tree-seq (comp seq :children) :children
                                                                 {:children (:nodes %)})) [before after])))]
           (when (> nodes max-preservation-nodes) (refuse! :too-much-preservation-work))
           (when-not (= :valid (:status (preservation/check {:before before :after after})))
             (refuse! :metadata-not-preserved))
           nodes)))
     0 bodies)
    (witness!
     {:profile profile :preservation preservation/profile :manifest (:id manifest) :target (:target manifest)
      :selection (get-in preview [:basis :selection])
      :typed-scopes (get-in preview [:basis :typed-scopes])
      :constraint-dependencies (get-in preview [:basis :constraint-dependencies])
      :after (into (sorted-map) (map (fn [[path value]] [path (entry (:after value))])) bodies)})))

(defn assert-after!
  "Validate complete actual AFTER source against the captured preflight, on the
   SAME immutable DB used by native commit preparation. Returns literal true
   only after target, exact touched/untouched source, definitions and constraint
   dependencies match. Source-side data only; never inspect the derived DB as
   if it were a writable destination. A later atomic checkpoint is mandatory.
   Historical receipt recovery must happen before this fresh-work guard."
  [db witness]
  (witness! witness)
  (target! db (:target witness))
  (let [expected (update (:selection witness) :files merge (:after witness))
        actual (view/prepare db knowledge/selection-options)]
    (when-not (and (= :ready (:status actual))
                   (= expected (:selection actual))
                   (= (:typed-scopes witness) (:typed-scopes actual))
                   (= (:constraint-dependencies witness) (:constraint-dependencies actual)))
      (refuse! :validation-basis-changed))
    true))
