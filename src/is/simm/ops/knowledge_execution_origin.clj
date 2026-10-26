(ns is.simm.ops.knowledge-execution-origin
  "Explicit source opt-in to HOST-selected checkpoint execution. The sidecar
   chooses a policy, never a store, commit, native handle or permission."
  (:require [clojure.edn :as edn]
            [is.simm.model.work-request-checkpoints :as checkpoints]
            [is.simm.ops.knowledge-batch-inputs :as batches]
            [malli.core :as m])
  (:import [java.io PushbackReader StringReader]))

(def policy
  {:version 1 :workspace :retained-batch-checkpoint :retention :request-evidence-v1})

(def ^:private schema
  [:map {:closed true} [:version [:= 1]]
   [:workspace [:= :retained-batch-checkpoint]] [:retention [:= :request-evidence-v1]]])

(defn- refuse! [reason]
  (throw (ex-info "Knowledge execution origin is unavailable" {:type ::held :reason reason})))

(defn declaration
  "Read one closed, bounded EDN policy from ordinary pinned workflow source.
   Absence preserves the existing copied-context behavior. A checkpoint policy
   requires explicit empty context selection; trigger-copy budgets stay intact."
  [source]
  (when (contains? source "execution-origin.edn")
    (let [text (get source "execution-origin.edn")]
      (when-not (and (string? text) (<= (count text) 4096)) (refuse! :invalid-declaration))
      (let [value (try
                    (with-open [reader (PushbackReader. (StringReader. text))]
                      (let [eof (Object.)
                            options {:eof eof :readers {} :default (fn [& _] (refuse! :invalid-declaration))}
                            value (edn/read options reader)]
                        (when-not (identical? eof (edn/read options reader)) (refuse! :invalid-declaration))
                        value))
                    (catch StackOverflowError _ (refuse! :invalid-declaration))
                    (catch Exception _ (refuse! :invalid-declaration)))]
        (when-not (m/validate schema value) (refuse! :invalid-declaration))
        (let [batching (batches/declaration source)]
          (when-not (= {:context-paths [] :max-context-files 0 :max-context-bytes 0}
                       (select-keys (:policy batching) [:context-paths :max-context-files :max-context-bytes]))
            (refuse! :copied-context-policy-mismatch)))
        value))))

(defn require-copied-context!
  "Entry points without exact checkpoint admission must refuse the policy,
   never silently evaluate it against live or differently staged context."
  [source]
  (when (declaration source) (refuse! :checkpoint-execution-unsupported))
  nil)

(defn validate-execution!
  "Checkpoint wiki changes are reviewed through retained file-output Proposals,
   never a whole historical Run-world merge. Refuse an incompatible settlement;
   do not silently rewrite a caller's intent. Pure preflight before discovery."
  [source limits]
  (when-let [declared (declaration source)]
    (when-not (= :discard (keyword (:settlement limits)))
      (refuse! :checkpoint-requires-discard))
    declared))

(defn selection
  "Derive inert identity from an already verified canonical batch proof, never
   the current Memory or caller-selected CID. Fixed admission must revalidate
   this binding against the retained Request/batch before native pin IO."
  [source proof]
  (when-let [declared (declaration source)]
    (let [basis (get-in proof [:selection :basis])]
      (checkpoints/validate-selection!
       {:version 1 :policy (:retention declared)
        :origin {:version 1 :kind :datahike :commit (:commit basis)
                 :source (select-keys basis [:store :repository])}
        :basis basis :target (get-in proof [:source-origin :target])}))))
