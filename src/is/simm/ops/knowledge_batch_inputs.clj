(ns is.simm.ops.knowledge-batch-inputs
  "Inactive bounded batch planning and frozen bytes. The v2 HOST envelope
   retains the existing v1 exact-evidence codec; neither is batch authority.
   No claim, artifact publication, request admission or provider execution."
  (:require [clojure.set :as set]
            [hasch.core :as hasch]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.workflow-policy :as workflow-policy]
            [is.simm.ops.workflows :as workflows]
            [malli.core :as m]))

(def policy-schema
  [:map {:closed true} [:version [:= 1]]
   [:max-trigger-files [:int {:min 1 :max 98}]]
   [:max-trigger-bytes [:int {:min 1 :max 1048576}]]
   [:max-context-files [:int {:min 0 :max 98}]]
   [:max-context-bytes [:int {:min 0 :max 524288}]]
   [:context-paths {:optional true} [:vector {:max 98} [:string {:min 1 :max 512}]]]])

(def default-policy
  {:version 1 :max-trigger-files 32 :max-trigger-bytes 1048576
   :max-context-files 32 :max-context-bytes 524288})

(def reservation-schema
  [:map {:closed true} [:files [:int {:min 0 :max 100}]]
   [:bytes [:int {:min 0 :max 2097152}]]])

(defn- refuse! [reason]
  (throw (ex-info "Knowledge batch inputs are held" {:type ::held :reason reason})))

(defn- utf8-size [text] (alength (.getBytes ^String text "UTF-8")))
(defn- portable [item]
  {:id (:knowledge.item/id item) :order (:knowledge.item/order item)
   :path (:knowledge.item/path item) :content (:knowledge.item/content item)
   :size (:knowledge.item/size item) :mode (:knowledge.item/mode item)})
(defn- entry [item role] (assoc (select-keys item [:path :content :size :mode]) :role role))
(defn- digest [body] (hasch/uuid [:simmis/knowledge-batch-selection-v2 body]))

(defn- pending! [value expected pending]
  (when-not (and (vector? pending) (<= (count pending) streams/max-changes))
    (refuse! :invalid-pending))
  (let [items (mapv portable pending)]
    (when-not (and (= items (vec (sort-by (juxt :order :path) items)))
                   (= (count items) (count (set (map :id items))) (count (set (map :path items)))))
      (refuse! :invalid-pending-order))
    (doseq [[raw item] (map vector pending items)]
      (when-not (and (= (:id value) (:knowledge.item/stream raw) (:knowledge.item/pending-stream raw))
                     (= :pending (:knowledge.item/state raw))
                     (integer? (:order item)) (<= 0 (:order item))
                     (< (:order item) (:knowledge.stream/next-order expected))
                     (integer? (:size item)) (<= 0 (:size item)) (= 33188 (:mode item))
                     (uuid? (:content item))
                     (= (:id item) (streams/item-id (:id value) (:path item) item)))
        (refuse! :invalid-pending-item)))
    items))

(defn selection
  "Metadata only, independent of the canonical 200-document projection.
   RESERVED accounts for ordinary workflow fixture inputs. Context defaults to
   ALL selected context metadata, or explicit context-paths; never silently
   truncate context. Select a prefix of actual pending order, without skipping
   an oversized head item. The caller must verify and retain the cursor pin."
  [db value expected pending folder-policy batch-policy reserved]
  (streams/validate-binding! value expected)
  (when-not (and expected (not (:knowledge.stream/active-batch expected))
                 (m/validate policy-schema batch-policy) (m/validate reservation-schema reserved))
    (refuse! :invalid-batch-selection))
  (let [folder-policy (inputs/normalize-policy folder-policy)
        _ (when-not (= (:policy value) (hasch/uuid folder-policy)) (refuse! :policy-mismatch))
        basis (changes/basis db)
        _ (when-not (= [(:commit basis) (:max-tx basis) (streams/source-id basis)]
                       [(:knowledge.stream/cursor-commit expected) (:knowledge.stream/cursor-tx expected)
                        (:knowledge.stream/source expected)])
            (refuse! :cursor-mismatch))
        items (pending! value expected pending)]
    (if (empty? items)
      {:status :empty :stream (:id value) :basis basis}
      (let [inventory (:files (changes/inventory db folder-policy))
            available (into (sorted-map) (filter #(= :context (:role (val %)))) inventory)
            requested (:context-paths batch-policy)
            _ (when (and requested (not= (count requested) (count (set requested))))
                (refuse! :duplicate-context-path))
            _ (when (and requested (not (set/subset? (set requested) (set (keys available)))))
                (refuse! :context-unavailable))
            context (mapv (fn [[path metadata]] (assoc metadata :path path))
                          (if requested (select-keys available (sort requested)) available))
            context (vec (sort-by :path context))
            context-bytes (reduce + 0 (map :size context))
            _ (when (or (> (count context) (:max-context-files batch-policy))
                        (> context-bytes (:max-context-bytes batch-policy)))
                (refuse! :context-exceeds-budget))
            max-count (min (:max-trigger-files batch-policy)
                           (- (:max-input-files workflow-policy/policy) (:files reserved) (count context) 1))
            _ (when (<= max-count 0) (refuse! :no-trigger-capacity))
            chosen (loop [remaining items result [] bytes 0]
                     (if-let [item (first remaining)]
                       (if (or (= (count result) max-count)
                               (> (:size item) workflows/max-file-bytes)
                               (> (+ bytes (:size item)) (:max-trigger-bytes batch-policy)))
                         (if (empty? result) (refuse! :head-item-exceeds-budget) result)
                         (recur (next remaining) (conj result item) (+ bytes (:size item))))
                       result))
            ;; Validate every selected path/version against the retained source.
            _ (doseq [item chosen]
                (when-not (= (entry item :trigger) (some-> (get inventory (:path item)) (assoc :path (:path item))))
                  (refuse! :pending-source-mismatch)))
            [chosen evidence]
            (loop [chosen chosen]
              (let [evidence (inputs/selection-of-files db folder-policy
                               (into (mapv #(entry % :trigger) chosen) context))
                    bytes (+ (:bytes reserved) (utf8-size (:manifest-text evidence)) context-bytes
                             (reduce + 0 (map :size chosen)))]
                (if (<= bytes (:max-input-bytes workflow-policy/policy))
                  [chosen evidence]
                  (if (= 1 (count chosen)) (refuse! :execution-envelope-exceeds-budget)
                      (recur (pop chosen))))))
            body {:version 2 :stream value :expected expected :basis basis
                  :batch-policy batch-policy :reserved reserved :items chosen
                  :remaining (- (count items) (count chosen)) :evidence (:manifest evidence)}]
        {:status :selected :batch (assoc body :id (digest body))}))))

(defn verify
  "Integrity and frozen-byte validation only. A benchmark or caller can create
   matching data; only the canonical HOST journal can authorize consumption."
  [{:keys [batch captured] :as envelope}]
  (when-not (and (= #{:version :batch :captured} (set (keys envelope))) (= 2 (:version envelope))
                 (= #{:version :stream :expected :basis :batch-policy :reserved :items :remaining :evidence :id}
                    (set (keys batch)))
                 (= 2 (:version batch)) (= (:id batch) (digest (dissoc batch :id)))
                 (m/validate policy-schema (:batch-policy batch))
                 (m/validate reservation-schema (:reserved batch))
                 (map? (:expected batch)) (nil? (get-in batch [:expected :knowledge.stream/active-batch]))
                 (vector? (:items batch)) (<= 1 (count (:items batch)) (get-in batch [:batch-policy :max-trigger-files]))
                 (integer? (:remaining batch)) (<= 0 (:remaining batch) streams/max-changes)
                 (<= (+ (:remaining batch) (count (:items batch))) streams/max-changes))
    (refuse! :invalid-envelope))
  (streams/validate-binding! (:stream batch) (:expected batch))
  (streams/validate-basis! (:basis batch))
  (inputs/verify-capture (get-in batch [:evidence :policy]) captured)
  (when-not (and (= (:evidence batch) (:manifest captured))
                 (= (get-in batch [:stream :policy]) (hasch/uuid (get-in batch [:evidence :policy])))
                 (= (get-in batch [:basis :repository]) (get-in batch [:evidence :repository]))
                 (= (streams/source-id (:basis batch)) (get-in batch [:expected :knowledge.stream/source]))
                 (= ((juxt :commit :max-tx) (:basis batch))
                    ((juxt :knowledge.stream/cursor-commit :knowledge.stream/cursor-tx) (:expected batch)))
                 (= (into (sorted-map) (map (fn [item] [(:path item) (entry item :trigger)])) (:items batch))
                    (into (sorted-map) (for [entry (get-in batch [:evidence :files]) :when (= :trigger (:role entry))]
                                        [(:path entry) (dissoc entry :input)]))))
    (refuse! :frozen-batch-mismatch))
  (let [items (:items batch)]
    (when-not (and (= items (vec (sort-by (juxt :order :path) items)))
                   (= (count items) (count (set (map :id items))) (count (set (map :path items)))))
      (refuse! :invalid-pending-order))
    (doseq [item items]
      (when-not (and (= #{:id :order :path :content :size :mode} (set (keys item)))
                     (integer? (:order item)) (<= 0 (:order item))
                     (< (:order item) (get-in batch [:expected :knowledge.stream/next-order]))
                     (= (:id item) (streams/item-id (get-in batch [:stream :id]) (:path item) item)))
        (refuse! :invalid-item-identity))))
  (let [context (filterv #(= :context (:role %)) (get-in batch [:evidence :files]))
        config (:batch-policy batch) requested (:context-paths config)
        size (reduce + 0 (map utf8-size (vals (:inputs captured))))]
    (when-not (and (<= (count context) (:max-context-files config))
                   (<= (reduce + 0 (map :size context)) (:max-context-bytes config))
                   (<= (reduce + 0 (map :size (:items batch))) (:max-trigger-bytes config))
                   (or (nil? requested) (and (= (count requested) (count (set requested)))
                                            (= (set requested) (set (map :path context)))))
                   (<= (+ (get-in batch [:reserved :files]) (count (:inputs captured)))
                       (:max-input-files workflow-policy/policy))
                   (<= (+ (get-in batch [:reserved :bytes]) size) (:max-input-bytes workflow-policy/policy)))
      (refuse! :frozen-batch-exceeds-budget)))
  envelope)

(defn capture
  "After an acknowledged HOST batch claim, freeze from its owned retained DB.
   This helper itself grants no claim or authority; never recapture on recovery."
  [db {:keys [status batch]}]
  (when-not (= :selected status) (refuse! :not-selected))
  (when-not (= (:basis batch) (changes/basis db)) (refuse! :source-changed))
  (verify {:version 2 :batch batch
           :captured (inputs/capture-files db (get-in batch [:evidence :policy])
                                           (mapv #(dissoc % :input) (get-in batch [:evidence :files])))}))
