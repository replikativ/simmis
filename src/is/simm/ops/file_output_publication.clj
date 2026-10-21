(ns is.simm.ops.file-output-publication
  "PURE retained-file publication projections. Data is not a capability or
   acknowledged claim. No lookup, schema installation, callback or write here.
   Only the canonical native writer/scoped recovery can supply winning receipts."
  (:refer-clojure :exclude [identity])
  (:require [hasch.core :as hasch]
            [is.simm.model.file-adoptions :as adoptions]
            [is.simm.ops.file-output-plan :as plans]
            [is.simm.ops.knowledge-outputs :as outputs]))

(defn- refuse! [reason]
  (throw (ex-info "Retained file publication data was refused" {:type ::refused :reason reason})))

(defn identity
  "Metadata-only complete native publication identity. All valid journal states
   are inspectable, but none grant publication ownership. Full host target
   (including registry/runtime markers) must match the immutable manifest."
  [operation manifest]
  (adoptions/validate! operation)
  (plans/validate-manifest! manifest)
  (when-not (and (seq (:entries manifest))
                 (= (:proposal operation) (hasch/uuid [:simmis/knowledge-file-proposal-v1 (:id manifest)]))
                 (= (:component operation) (:manifest operation) (:id manifest))
                 (= (:room operation) (:room manifest))
                 (= (:target operation) (:target manifest)))
    (refuse! :binding-mismatch))
  (merge {:publication/id (:id operation) :publication/intent (:intent operation)
          :publication/actor (:actor operation) :publication/proposal (:proposal operation)
          :publication/manifest (:id manifest) :publication/attempt (:attempt manifest)}
         (into {} (for [key [:repository :store :backend :branch :ref]]
                    [(keyword "publication" (name key)) (get-in manifest [:target key])]))))

(defn prepare
  "Construct one closed native batch/options value after bounded metadata
   validation and exact body validation. Claimed state is necessary, NOT proof
   of the original invocation's claim ACK. Host authorization/journal callbacks
   must be supplied separately by the eventual fixed writer, never this data."
  [operation {:keys [manifest bodies] :as plan}]
  (let [publication-identity (identity operation manifest)]
    (when-not (= :claimed (:state operation)) (refuse! :not-claimed))
    ;; identity performs bounded metadata/path checks before this first decode.
    (plans/validate-plan! plan)
    {:identity publication-identity
     :files (into (sorted-map)
                  (map (fn [[path pair]] [(str "/" path) {:base64 (get-in pair [:after :base64])}])) bodies)
     :options
     {:expected-worktree
      (into (sorted-map)
            (map (fn [[path {:keys [before]}]]
                   [(str "/" path) (when (= :present (:state before))
                                     {:content (:content before) :size (:bytes before) :mode (:mode before)})]))
            (:entries manifest))
      :file-modes (into (sorted-map)
                        (map (fn [[path entry]] [(str "/" path) (get-in entry [:after :mode])]))
                        (:entries manifest))}}))

(defn- keys! [value required optional]
  (when-not (and (map? value) (every? #(contains? value %) required)
                 (every? (into (set required) optional) (keys value))
                 (or (not (contains? value :db/id)) (integer? (:db/id value))))
    (refuse! :invalid-shape)))

(defn- content-id! [content candidate?]
  (if candidate?
    (do (when-not (and (vector? content) (= 2 (count content))
                       (= :geschichte.content/id (first content)) (uuid? (second content)))
          (refuse! :invalid-candidate-content))
        (second content))
    (do (keys! content #{:geschichte.content/id} #{:db/id})
        (when-not (uuid? (:geschichte.content/id content)) (refuse! :invalid-receipt-content))
        (:geschichte.content/id content))))

(defn- validate-publication! [operation manifest receipt candidate?]
  (let [expected (identity operation manifest)
        required (into (set (keys expected))
                       [:publication/noop? :publication/created-at :publication/entries])]
    (keys! receipt required (cond-> #{:publication/commit :publication/head}
                              (not candidate?) (conj :db/id)))
    (when-not (and (= expected (select-keys receipt (keys expected)))
                   (instance? java.util.Date (:publication/created-at receipt))
                   (boolean? (:publication/noop? receipt))
                   (uuid? (:publication/head receipt))
                   (if (:publication/noop? receipt)
                     (not (contains? receipt :publication/commit))
                     (and (uuid? (:publication/commit receipt))
                          (= (:publication/commit receipt) (:publication/head receipt)))))
      (refuse! :invalid-identity-or-outcome))
    (let [entries (:publication/entries receipt)
          expected-entries (into (sorted-map)
                                 (map (fn [[path entry]] [(str "/" path) (:after entry)])) (:entries manifest))]
      ;; Check counts before traversing nested metadata; bodies are never read.
      (when-not (and (vector? entries) (= (count expected-entries) (count entries))
                   (<= 1 (count entries) (:max-files outputs/policy)))
        (refuse! :invalid-entries))
      (let [actual (mapv (fn [entry]
                           (keys! entry #{:publication.entry/path :publication.entry/state
                                          :publication.entry/content :publication.entry/size :publication.entry/mode}
                                  (if candidate? #{} #{:db/id}))
                           (when-not (and (integer? (:publication.entry/size entry))
                                          (integer? (:publication.entry/mode entry)))
                             (refuse! :invalid-entry-metadata))
                           (when-not (= :present (:publication.entry/state entry)) (refuse! :invalid-entry-state))
                           [(:publication.entry/path entry)
                            {:content (content-id! (:publication.entry/content entry) candidate?)
                             :bytes (:publication.entry/size entry) :mode (:publication.entry/mode entry)}]) entries)]
        (when-not (and (= (count actual) (count (set (map first actual))))
                       (= expected-entries (into {} actual)))
          (refuse! :entry-mismatch))))
    (when (and (= :accepted (:state operation))
               (not= (select-keys operation [:commit :head :noop?])
                     (into {} (for [key [:commit :head :noop?]
                                    :let [attr (keyword "publication" (name key))]
                                    :when (contains? receipt attr)]
                                [key (get receipt attr)]))))
      (refuse! :accepted-outcome-mismatch))
    receipt))

(defn validate-candidate!
  "Validate exact PRE-DISPATCH data, with content lookup refs. This is NOT a
   committed receipt, current authority or permission to enter applying state."
  [operation manifest candidate]
  (validate-publication! operation manifest candidate true))

(defn validate-receipt!
  "Validate an ORIGINAL scoped winning receipt, with pulled content maps. Host
   must obtain it from canonical writer/scoped recovery after authorization.
   Never substitutes current HEAD/files; later edits/checkouts do not invalidate
   historical evidence. No-op requires UUID HEAD: this manifest is nonempty and
   publishes present AFTER entries, so an empty-repository no-op is impossible."
  [operation manifest receipt]
  (validate-publication! operation manifest receipt false))
