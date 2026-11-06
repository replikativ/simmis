(ns is.simm.ops.coordinator-sandbox
  "An opt-in coordinator over existing scoped Dvergr APIs, not a richer worker."
  (:require [dvergr.agent.run :as run]
            [dvergr.agent.roster :as roster]
            [dvergr.room.registry :as registry]
            [dvergr.room.store :as store]
            [dvergr.runtime.ctx :as runtime]
            [dvergr.sandbox.extensions :as extensions]
            [dvergr.sandbox.ns.doc :as doc]
            [is.simm.ops.results :as results]
            [is.simm.ops.work-sandbox :as worker]
            [org.replikativ.spindel.engine.core :as ec]
            [sci.core :as sci]))

(def id :simmis/coordinator)
(def delegation-policy
  {:tools #{:clojure_eval :read_file :write_file :edit_file :await_child}
   :sandboxes #{#{worker/id} #{id}}
   :providers #{:openai :anthropic :fireworks :codex-subscription}
   :max-attempts 8 :max-parallelism 2})

(def completion-ref
  ;; This explicit grant is NOT permission to read arbitrary verifier evidence.
  {:verifier/id :simmis/work-completion :verifier/version 1
   :verifier/basis "simmis-work-completion-v1"})

(def policy {:version 4 :delegation delegation-policy :file-verifier completion-ref
             :surface :scoped-recursive-files :codecs #{:base64-encode :base64-decode}
             :no-shared-registries true})

(def ^:private retained-agent-functions
  '#{roster make-agent revise-agent lookup ref list select environment environment-ref
     dataset dataset-ref experiment experiment-ref room-id hire! run-experiment! invoke-experiment!
     offered delegation-policy current-run-id observe inspect attempt-output cancel! balance split
     spend-by-role run-id result-spin owned-result-spin})

(defn- refused! [message]
  (throw (ex-info message {:type ::denied})))

(defn- read-captured [execution-ctx observe current-run-id safe-verifiers project id opts]
  (when-not (uuid? id) (refused! "Attempt id must be a UUID"))
  (let [ctx (runtime/selected-context execution-ctx)]
    (binding [ec/*execution-context* ctx]
      ;; Reuse the injected, scoped observer; never accept a room from the agent.
      (let [r (observe id)
            room (when r (registry/lookup (:run/room r)))
            attempt (when (and room (satisfies? store/PAttemptStore (:store room)))
                      (store/-load-attempt (:store room) (store/conversation-id room) id))
            consumer (current-run-id)]
        (when-not (and (uuid? consumer) attempt
                       (contains? safe-verifiers (get-in attempt [:attempt/environment :environment/verifier])))
          (refused! "No captured output offered for this scoped certified Attempt"))
        (let [value (project {:id (str id) :run-id (str (:attempt/run-id attempt))
                              :content-id (str (:attempt/content-id attempt))
                              :evidence (:attempt/evidence attempt)} opts)]
          (when-not (= consumer id) (run/record-cause! consumer id))
          value)))))

(defn install!
  "Keep the normal worker restrictions, restoring only existing scoped agent
   closures. Requires the exact host policy; cannot be used as an unbounded profile."
  [ctx execution-ctx]
  (when-not (= delegation-policy (sci/eval-string* ctx "(dvergr.agent/delegation-policy)"))
    (refused! "Coordinator sandbox requires its host-owned delegation policy"))
  (let [functions (into {} (map (fn [name]
                                 ;; Read values directly. ns-publics may return
                                 ;; functions, not dereferenceable Vars.
                                  [name (sci/eval-string* ctx (str "dvergr.agent/" name))]))
                        retained-agent-functions)
        observe (get functions 'observe)
        current-run-id (get functions 'current-run-id)
        codecs (into {} (map (fn [name] [name (sci/eval-string* ctx (str "dvergr.codec/" name))]))
                     '[base64-encode base64-decode])
        offers (vec (filter #(= :simmis/admitted-benchmark (:invocation/id %))
                            (:invocations ((get functions 'offered)))))
        reviews (vec (filter #(= :simmis/admitted-benchmark-review (:invocation/id %))
                             (:invocations ((get functions 'offered)))))
        safe-verifiers (into #{completion-ref} (keep #(get-in % [:invocation/basis :file-verifier])) (concat offers reviews))
        invoke (get functions 'invoke-experiment!)]
    (worker/install! ctx execution-ctx)
    (sci/add-namespace! ctx 'dvergr.agent functions)
    ;; Captured files are bounded base64 chunks. Restore only these two pure
    ;; codecs, not the denied host/API namespace surface or arbitrary classes.
    (sci/add-namespace! ctx 'dvergr.codec codecs)
    (sci/add-namespace!
     ctx 'simmis.benchmarks
     (doc/with-docs
       {'offered (fn [] offers)
        'reviews-offered (fn [] reviews)
        'start-review! (fn start-review!
                         ([opts]
                          (when-not (= 1 (count reviews)) (refused! "Select one exact admitted development review"))
                          (start-review! (first reviews) opts))
                         ([ref opts]
                          (when-not (some #{ref} reviews) (refused! "Development review is not offered to this Run"))
                          (invoke ref (roster/make-roster) (assoc opts :result-mode :receipt))))
        'start! (fn start!
                  ([team opts]
                   (when-not (= 1 (count offers)) (refused! "Select one exact admitted benchmark offer"))
                   (start! (first offers) team opts))
                  ([ref team opts]
                   (when-not (some #{ref} offers) (refused! "Benchmark is not offered to this Run"))
                   (invoke ref team (assoc opts :result-mode :receipt))))
        'run! (fn run!
                ([team opts]
                 (when-not (= 1 (count offers)) (refused! "Select one exact admitted benchmark offer"))
                 (run! (first offers) team opts))
                ([ref team opts]
                 (when-not (some #{ref} offers) (refused! "Benchmark is not offered to this Run"))
                 (invoke ref team opts)))}
       {'offered ['([]) "Exact host-admitted benchmark refs. Empty means no scoring authority; authored checkers need separate inspected admission."]
        'reviews-offered ['([]) "Explicit gold-aware exposed-development review offers. Empty means no reference disclosure was admitted. Never use for held-out tuning."]
        'start-review! ['([opts] [ref opts]) "Start one pinned reviewer child experiment. opts: :subject {:job uuid :scorecard uuid :attempt uuid} from this Run's exact settled benchmark child, :resources explicit grant; optional :parallelism 1. The operator pins reviewer source/model/prompt; callers cannot replace its team. Next use await_child with the returned Job UUID. Structural checks validate format/binding/quotes/advisory status, not semantic truth. Reference/rubric changes still need human adjudication."]
        'start! ['([team opts] [ref team opts]) "Start one exact offered benchmark and return a Job receipt. opts: :resources aggregate grant, optional :parallelism 1. Next use the await_child model tool with its Job UUID; do not dereference or poll from SCI. If the start reply is lost, use await_child without job to recover receipts, following next-offset; never restart as recovery."]
        'run! ['([team opts] [ref team opts]) "Benchmark one candidate prompt/model revision per call using the offer's declared candidate profile against one fixed admitted dataset/checker in clean fixture worlds. opts: :resources required aggregate grant, :parallelism optional 1. Returns native owned result Spin with Scorecard. Revise candidates, inspect results and repeat within your remaining balance."]}))
    (sci/add-namespace!
     ctx 'simmis.results
     (doc/with-docs
       {'attempt-files (fn attempt-files
                         ([id] (attempt-files id {}))
                         ([id opts] (read-captured execution-ctx observe current-run-id safe-verifiers results/captured-files id opts)))
        'attempt-file (fn [id opts] (read-captured execution-ctx observe current-run-id safe-verifiers results/captured-file id opts))}
       {'attempt-files ['([id] [id opts])
                        "List canonical immutable files from a descendant offered Attempt; takes a UUID and optional {:offset n}. No private verifier evidence or live world access."
                        [:function [:=> [:cat :uuid] :map] [:=> [:cat :uuid results/files-options-schema] :map]]]
        'attempt-file ['([id opts])
                       "Read a descendant offered Attempt file as bounded base64 bytes. opts: :path required, :offset/:length optional; follow :next-offset. Records causal consumption."
                       [:=> [:cat :uuid results/file-options-schema] :map]]})))
  ctx)

(defn restrict-team [team]
  (worker/restrict-team (roster/make-roster))
  (extensions/register! id {:version (:version policy) :basis policy
                            :install 'is.simm.ops.coordinator-sandbox/install!})
  (reduce (fn [team agent]
            (roster/revise-agent team (:agent/id agent)
                                 {:sandbox #{id} :tools (:tools delegation-policy)}))
          team (roster/agents team)))
