(ns is.simm.ops.knowledge-binding-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.room.store.memory :as memory]
            [dvergr.substrate.geschichte :as geschichte]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.knowledge-outputs :as outputs]
            [is.simm.ops.knowledge-workspace :as binding]
            [is.simm.ops.workflow-runs :as workflows]
            [muschel.fs :as fs]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.protocols :as protocols]))

(defn- with-repositories [f]
  (let [room (discourse/make-room {:id (keyword (str "knowledge-binding-" (random-uuid)))
                                   :store (memory/make)})
        opened (atom [])
        open! (fn [] (let [conn (workspace/open-memory-workspace! room)]
                       (swap! opened conj conn)
                       conn))]
    (try
      (binding [ec/*execution-context* (:ctx room)]
        (let [attached (open!)
              owned (open!)
              identity-for (fn [conn]
                             {:registry-system (random-uuid)
                              :runtime-system
                              (some (fn [system]
                                      (when (identical? conn (geschichte/gy-connection system))
                                        (str (protocols/system-id system))))
                                    (filter #(= :geschichte (protocols/system-type %))
                                            (vals (ygg/registered-systems))))})
              target (fn [conn] (:target (inputs/publication-basis @conn (identity-for conn))))]
          (f room {:owned owned :attached attached :target (target owned)
                   :attached-target (target attached)})))
      (finally
        (try (discourse/close-room! room)
             (finally (doseq [conn @opened] (d/release conn))))))))

(defn- base-evaluator [capture]
  (evaluation/make-evaluator
   {:id :fixture/worker-completion :version 3 :basis {:worker :unchanged}
    :capture capture
    ;; Deliberately drops every unknown field: the wrapper must retain proof.
    :observe (fn [context] (select-keys (:execution/evidence context) [:worker]))
    :verify (fn [_ _] {:checks {:completed? true :worker? true} :reward 0.75
                       :metrics {:worker-measured true}})}))

(deftest publication-binding-preserves-worker-policy-and-canonicalizes-before-claim
  (with-repositories
    (fn [_ {:keys [owned target]}]
      (let [captured (inputs/capture @owned inputs/default-policy)
            basis (inputs/publication-basis @owned (select-keys target [:registry-system :runtime-system]))
            setup (evaluation/make-world-setup
                   {:id :fixture/worker-setup :version 4 :basis {:frozen :bytes}
                    :prepare (fn [_] (throw (AssertionError. "Binding executed setup")))})
            evaluator (base-evaluator (fn [_] (throw (AssertionError. "Binding captured live state"))))
            ref (evaluation/evaluator-ref evaluator)
            env (environment/make-environment
                 {:id :simmis/local-worker :version 3 :task {:frozen :task}
                  :verifier {:id (:verifier/id ref) :version (:verifier/version ref) :basis (:verifier/basis ref)}
                  :limits {:timeout-ms 12345 :on-timeout :verdict}
                  :world {:isolation :ctx :settlement :discard :effects #{} :resources {:worker 1M}
                          :protocol {:protocol/id :fixture/worker :protocol/version 2}
                          :setup (evaluation/world-setup-ref setup)}
                  :metadata {:workflow {:input-selection {:execution-id (:execution-id captured)}}
                             :worker-cell :preserved}})
            prepared {:environment env :evaluator evaluator :world-setup setup :protocol :same-capability
                      :basis {:environment env :frozen :retained-worker-bytes
                              :workflow {:input-selection {:execution-id (:execution-id captured)}}}}
            bound (workflows/bind-publication prepared {:captured captured :publication-basis basis})
            new-env (:environment bound)]
        (is (identical? prepared (workflows/bind-publication prepared {:captured nil :publication-basis nil})))
        (is (= new-env (get-in bound [:basis :environment])))
        (is (= basis (get-in bound [:basis :knowledge/publication-basis])))
        (is (not= (:environment/content-id env) (:environment/content-id new-env)))
        (is (= (select-keys env [:environment/id :environment/version :environment/task
                                 :environment/limits :environment/metadata])
               (select-keys new-env [:environment/id :environment/version :environment/task
                                     :environment/limits :environment/metadata])))
        (is (= (dissoc (:environment/world env) :setup) (dissoc (:environment/world new-env) :setup)))
        (is (= :same-capability (:protocol bound)))
        (is (= :retained-worker-bytes (get-in bound [:basis :frozen])))
        (is (= env (:environment prepared) (get-in prepared [:basis :environment])))
        (is (= (evaluation/evaluator-ref evaluator)
               (get-in new-env [:environment/verifier :verifier/basis :base])))
        (is (= (evaluation/world-setup-ref setup)
               (get-in new-env [:environment/world :setup :setup/basis :base])))
        (is (= target (get-in new-env [:environment/verifier :verifier/basis :target])
               (get-in new-env [:environment/world :setup :setup/basis :target])))
        (is (= new-env (environment/validate-environment new-env)))))))

(deftest actual-selected-clone-proof-preserves-observation-and-parent-isolation
  (with-repositories
    (fn [room {:keys [owned attached target attached-target]}]
      (is (= (:runtime-system attached-target)
             (str (protocols/system-id (geschichte/current-system)))))
      (let [child (ygg/fork!) world (assoc room :ctx (:child-ctx child))
            base (evaluation/make-world-setup
                  {:id :fixture/prepare :prepare (fn [_]
                                                   (fs/write-string! (geschichte/filesystem) "/out/bound.txt" "child only" false)
                                                   {:unchanged :setup-result})})
            setup (binding/with-publication-target base target)
            evaluator (outputs/with-publication-target
                        (base-evaluator
                         (fn [_]
                           {:worker {:selected (geschichte/selected-system-id)
                                     :branch (get-in @(:conn (geschichte/current-workspace)) [:config :branch])
                                     :text (fs/read-file (geschichte/filesystem) "/out/bound.txt")}})) target)]
        (try
          ;; Ambient context is deliberately the parent, not the child. The
          ;; wrappers themselves must bind every delegated callback correctly.
          (is (identical? (:ctx room) ec/*execution-context*))
          (is (= {:unchanged :setup-result} ((:prepare setup) {:room world})))
          (let [proof (binding/target-proof! world target)
                evidence ((:capture evaluator) {:world/room world})
                observed ((:observe evaluator) {:execution/evidence evidence})
                verdict ((:verify evaluator) nil observed)]
            (is (= (binding/expected-proof target) proof (:knowledge-workspace observed)))
            (is (= proof (edn/read-string (pr-str proof))))
            (is (= (:runtime-system target) (get-in observed [:worker :selected])))
            (is (= "child only" (get-in observed [:worker :text])))
            (is (= {:completed? true :worker? true :knowledge-workspace? true} (:checks verdict)))
            (is (= 0.75 (:reward verdict)))
            (is (= {:worker-measured true} (:metrics verdict)))
            (is (not= (get-in @owned [:config :branch]) (get-in observed [:worker :branch])))
            (let [bad ((:verify evaluator) nil (assoc observed :knowledge-workspace nil))]
              (is (false? (get-in bad [:checks :knowledge-workspace?])))
              (is (zero? (:reward bad)))))
          (is (identical? (:ctx room) ec/*execution-context*))
          (ygg/with-fork child
            (let [nested (ygg/fork!)]
              (try
                (ygg/with-fork nested
                  (is (= (:runtime-system target) (geschichte/selected-system-id))))
                (finally (ygg/discard-fork! nested)))))
          (is (nil? (geschichte/selected-system-id)))
          (is (= (:runtime-system attached-target)
                 (str (protocols/system-id (geschichte/current-system)))))
          (doseq [conn [owned attached]]
            (is (not (fs/exists? (geschichte/filesystem {:conn conn :repository {:conn conn :config (:config @conn)}})
                                 "/out/bound.txt"))))
          (finally (ygg/discard-fork! child)))))))

(deftest unselected-or-retargeted-capture-cannot-certify-a-coincidentally-correct-default
  (with-repositories
    (fn [room {:keys [target attached-target]}]
      (let [calls (atom 0)
            base (base-evaluator (fn [_] (swap! calls inc) {:worker :captured}))
            unselected (outputs/with-publication-target base attached-target)]
        (is (thrown? clojure.lang.ExceptionInfo ((:capture unselected) {:world/room room})))
        (is (zero? @calls))
        (geschichte/select-workspace! (:runtime-system target))
        (let [changed (outputs/with-publication-target
                        (base-evaluator (fn [_]
                                          (swap! calls inc)
                                          (geschichte/select-workspace! (:runtime-system attached-target))
                                          {:worker :changed})) target)]
          (is (thrown? clojure.lang.ExceptionInfo ((:capture changed) {:world/room room})))
          (is (= 1 @calls)))))))

(deftest setup-checks-physical-identity-before-base-and-selection-after-base
  (with-repositories
    (fn [room {:keys [target attached-target]}]
      (let [calls (atom 0)
            before (binding/with-publication-target
                     (evaluation/make-world-setup {:id :fixture/before :prepare (fn [_] (swap! calls inc))})
                     (assoc target :store (random-uuid)))]
        (is (thrown? clojure.lang.ExceptionInfo ((:prepare before) {:room room})))
        (is (zero? @calls))
        (let [after (binding/with-publication-target
                      (evaluation/make-world-setup
                       {:id :fixture/after :prepare (fn [_]
                                                      (swap! calls inc)
                                                      (geschichte/select-workspace! (:runtime-system attached-target)))}) target)]
          (is (thrown? clojure.lang.ExceptionInfo ((:prepare after) {:room room})))
          (is (= 1 @calls)))))))
