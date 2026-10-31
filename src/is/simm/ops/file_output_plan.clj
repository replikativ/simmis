(ns is.simm.ops.file-output-plan
  "Pure immutable file proposal planning. Callers must separately establish
   authorization and certified direct-Attempt/request provenance before filing."
  (:require [datahike.blob :as blob]
            [clojure.string :as str]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.knowledge-publication :as publication]
            [is.simm.model.knowledge-scope :as scope]
            [is.simm.model.file-outputs :as manifests]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-outputs :as outputs]
            [malli.core :as m])
  (:import [java.util Base64]))

(defn- refuse! [reason]
  (throw (ex-info "File proposal cannot be planned" {:type ::refused :reason reason})))

(defn manifest-tag [version]
  (case version 1 :simmis/knowledge-file-manifest-v1 2 :simmis/knowledge-file-manifest-v2))

(defn proposal-tag [version]
  (case version 1 :simmis/knowledge-file-proposal-v1 2 :simmis/knowledge-file-proposal-v2))

(defn validate-manifest!
  "Metadata preflight, before any retained file bodies are loaded."
  [manifest]
  (when-not (and (m/validate manifests/manifest-schema manifest)
                 (not (str/blank? (get-in manifest [:target :ref]))))
    (refuse! :invalid-manifest))
  (when (= 2 (:version manifest)) (scope/normalize! (:scope manifest)))
  (doseq [path (keys (:entries manifest))]
    (if (= 2 (:version manifest)) (scope/target-path! (:scope manifest) path) (outputs/target-path! path)))
  (doseq [side [:before :after]]
    (when (> (reduce + 0 (keep #(get-in % [side :bytes]) (vals (:entries manifest))))
             (:max-total-bytes outputs/policy))
      (refuse! :manifest-too-large)))
  (when-not (= (:id manifest)
               (hasch/uuid [(manifest-tag (:version manifest)) (dissoc manifest :id)]))
    (refuse! :manifest-mismatch))
  manifest)

(defn validate-pair!
  "Verify one exact retained body pair against already validated metadata."
  [{:keys [before after]} pair]
  (when-not (and (map? pair) (= #{:before :after} (set (keys pair))))
    (refuse! :invalid-body-pair))
  (outputs/bytes! (:after pair))
  (when-not (= after (select-keys (:after pair) [:content :bytes :mode]))
    (refuse! :after-body-mismatch))
  (if (= :present (:state before))
    (do (outputs/bytes! (:before pair))
        (when-not (= (dissoc before :state) (select-keys (:before pair) [:content :bytes :mode]))
          (refuse! :before-body-mismatch)))
    (when (some? (:before pair)) (refuse! :unexpected-before-body)))
  pair)

(defn validate-plan!
  "Validate metadata and exact paired bodies before durable publication."
  [{:keys [manifest proposal bodies] :as plan}]
  (validate-manifest! manifest)
  (when-not (and (= #{:manifest :proposal :bodies} (set (keys plan))) (map? bodies)
                 (= (set (keys (:entries manifest))) (set (keys bodies)))
                 (= proposal (hasch/uuid [(proposal-tag (:version manifest)) (:id manifest)])))
    (refuse! :invalid-plan))
  (doseq [[path entry] (:entries manifest)]
    (validate-pair! entry (get bodies path)))
  plan)

(defn- before-body [before retained path]
  (when (:present? before)
      ;; A filtered model manifest is not an inventory, and today's file is
      ;; not a historical baseline. Only the admission-time retained copy can
      ;; supply BEFORE bytes. Excluded/unretained existing targets refuse.
      (let [text (get retained (str "/in/knowledge/sources/" path))]
        (when-not (string? text) (refuse! :historical-body-unavailable))
        (when (or (> (:size before) (:max-file-bytes outputs/policy))
                  (> (count text) (:max-file-bytes outputs/policy)))
          (refuse! :historical-body-too-large))
        (let [bytes (markdown/source-bytes text)]
          (when-not (and (= (:size before) (alength bytes))
                         (= (:content before) (blob/blob-id bytes)))
            (refuse! :historical-body-mismatch))
          (let [descriptor {:content (:content before) :bytes (:size before) :mode (:mode before)
                            :base64 (.encodeToString (Base64/getEncoder) bytes)}]
            (outputs/bytes! descriptor)
            descriptor)))))

(defn plan
  "Produce manifest metadata and independently retainable exact bodies.
   No database reads/writes or live workspace lookups. Omitted outputs never
   imply deletion; absence is established only by the complete host basis."
  [{:keys [request room attempt environment workflow-bundle publication-basis output retained-inputs]}]
  (when-not (and (every? uuid? [request room attempt workflow-bundle])
                 (map? environment) (map? retained-inputs))
    (refuse! :invalid-provenance))
  (publication/validate! publication-basis)
  (outputs/validate! output)
  (when-not (and (= (:version publication-basis) (:version output))
                 (= (:scope publication-basis) (:scope output)))
    (refuse! :output-scope-mismatch))
  (let [before-states (into (sorted-map)
                            (map (fn [path] [path (inputs/publication-before publication-basis path)]))
                            (keys (:files output)))
        ;; Bound historical allocations before encoding even the first body;
        ;; tiny AFTERs can replace much larger historical files.
        _ (when (> (reduce + 0 (keep :size (vals before-states))) (:max-total-bytes outputs/policy))
            (refuse! :too-many-historical-bytes))
        bodies (into (sorted-map)
                     (keep (fn [[path after]]
                             (let [before (before-body (get before-states path) retained-inputs path)]
                               ;; Identical bytes/mode are no change, not a
                               ;; second write or a disguised empty baseline.
                               (when-not (= (select-keys before [:content :bytes :mode])
                                            (select-keys after [:content :bytes :mode]))
                                 [path {:before before :after after}])))) (:files output))
        entries (into (sorted-map)
                      (map (fn [[path {:keys [before after]}]]
                             [path {:before (if before
                                              (assoc (select-keys before [:content :bytes :mode]) :state :present)
                                              {:state :absent})
                                    :after (select-keys after [:content :bytes :mode])}])) bodies)
        manifest (cond-> {:version (:version output) :request request :room room :attempt attempt
                  :environment (hasch/uuid environment) :workflow workflow-bundle
                  :basis (:id publication-basis) :target (:target publication-basis)
                  :output (:id output) :entries entries}
                   (= 2 (:version output)) (assoc :scope (:scope output)))
        id (hasch/uuid [(manifest-tag (:version manifest)) manifest])]
    (validate-plan!
     {:manifest (assoc manifest :id id)
      :proposal (hasch/uuid [(proposal-tag (:version manifest)) id])
      :bodies bodies})))
