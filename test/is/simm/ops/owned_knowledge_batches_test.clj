(ns is.simm.ops.owned-knowledge-batches-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.gc-roots :as roots]
            [dvergr.artifact :as artifact]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.repo :as repo]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.workflow-schedules :as schedules]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.knowledge-batch-inputs :as inputs]
            [is.simm.ops.knowledge-inputs :as evidence]
            [is.simm.ops.owned-knowledge-discovery :as discovery]
            [is.simm.ops.owned-knowledge-discovery-test :as fixture]))

(defn- with-fixture [f]
  (@#'fixture/with-fixture
   (fn [{:keys [conn owned] :as data}]
     (let [epoch (random-uuid)]
       (d/transact conn (concat batches/schema scheduled/schema schedules/schema))
       (@#'fixture/write! owned "inbox/second.md" "Another source")
       (@#'fixture/write! owned "workflows/wiki/input-batching.edn"
                        (pr-str {:version 2 :epoch epoch :policy (assoc inputs/default-policy :max-trigger-files 1)}))
       (f (assoc data :batch-input (assoc (@#'fixture/request data) :epoch epoch)))))))

(defn- prepare [{:keys [host w batch-input]}]
  (discovery/prepare-batch! host (ops/party-principal (:owner w)) (str (:room w)) batch-input))

(deftest host-freezes-an-exact-prefix-and-releases-its-source-lease-before-return
  (with-fixture
    (fn [{:keys [conn room owned] :as data}]
      (let [result (prepare data)
            row (batches/batch @conn (:batch result))]
        (is (= :ready (:status result) (:knowledge.batch/state row)))
        (is (= result (edn/read-string (pr-str result))) "Only resource-independent data leaves the source extent")
        (is (= 1 (:remaining result)))
        (is (= (:captured result)
               (is.simm.ops.knowledge-inputs/from-inputs
                 {:version 1 :trigger-roots ["inbox"] :context-roots ["wiki"] :exclude-prefixes []}
                 (get-in result [:captured :inputs]))))
        (is (empty? (get @(:meta room) preparation/leases-key)))
        (is (= 1 (count (streams/pending-items @conn (:stream result)))))
        (@#'fixture/write! owned "inbox/later.md" "Evidence after the batch was frozen")
        (@#'fixture/write! owned "wiki/later.md" "Wiki occupancy after the frozen source")
        (is (not (contains? (get-in result [:publication-basis :files]) "wiki/later.md")))
        (let [retained (:value (artifact/get-bounded-value (artifact/datahike-store conn)
                                    (:knowledge.batch/frozen row) inputs/max-frozen-bytes))]
          (is (= (:publication-basis result) (:publication-basis retained))))
        (with-redefs [inputs/capture (fn [& _] (throw (AssertionError. "Active batch must not recapture")))]
          (is (= {:status :held :reason :active-batch :stream (:stream result)
                  :batch (:batch result) :batch-state :ready} (prepare data))))))))

(deftest no-opt-in-means-no-discovery-effects
  (@#'fixture/with-fixture
   (fn [{:keys [host w conn] :as data}]
     (let [request (@#'fixture/request data)]
       (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "No implicit activation")))]
         (is (= :batching-not-enabled-for-this-epoch
                (try (discovery/prepare-batch! host (ops/party-principal (:owner w)) (str (:room w)) request) nil
                     (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))))
       (is (empty? (d/datoms @conn :avet :knowledge.pin/id)))))))

(deftest a-different-epoch-never-implicitly-revisits-the-backlog
  (with-fixture
    (fn [{:keys [conn] :as data}]
      (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "No implicit new epoch")))]
        (is (= :batching-not-enabled-for-this-epoch
               (try (prepare (assoc-in data [:batch-input :epoch] (random-uuid))) nil
                    (catch clojure.lang.ExceptionInfo error (:reason (ex-data error)))))))
      (is (empty? (d/datoms @conn :avet :knowledge.pin/id)))
      (is (empty? (d/datoms @conn :avet :knowledge.batch/id))))))

(deftest retained-publication-occupancy-includes-unselected-existing-wiki-files
  (with-fixture
    (fn [{:keys [owned batch-input] :as data}]
      (@#'fixture/write! owned "wiki/private/notes.md" "Excluded context")
      (@#'fixture/write! owned "wiki/config.edn" "{:meaning :existing-non-markdown}")
      (@#'fixture/write! owned "workflows/wiki/input-selection.edn"
                       (pr-str {:kind :knowledge :policy {:version 1 :trigger-roots ["inbox"]
                                                         :context-roots ["wiki"] :exclude-prefixes ["wiki/private"]}}))
      (let [data (assoc data :batch-input (assoc (@#'fixture/request data) :epoch (:epoch batch-input)))
            result (prepare data) paths (set (keys (get-in result [:publication-basis :files])))
            contexts (set (for [entry (get-in result [:captured :manifest :files]) :when (= :context (:role entry))]
                            (:path entry)))]
        (is (contains? paths "wiki/private/notes.md"))
        (is (contains? paths "wiki/config.edn"))
        (is (= #{"wiki/file.md"} contexts))))))

(deftest deterministic-publication-occupancy-failure-does-not-claim-items
  (with-fixture
    (fn [{:keys [conn] :as data}]
      (with-redefs [evidence/publication-basis (fn [& _] (throw (ex-info "Occupancy exceeds bounds" {})))
                    inputs/capture (fn [& _] (throw (AssertionError. "No source copy")))]
        (is (thrown-with-msg? Exception #"Occupancy exceeds bounds" (prepare data))))
      (is (empty? (d/datoms @conn :avet :knowledge.batch/id)))
      (is (empty? (d/datoms @conn :avet :knowledge.item/state :claimed))))))

(deftest lost-claim-or-freeze-ack-never-recatures-on-a-new-invocation
  (doseq [boundary [:claim :freeze]]
    (with-fixture
      (fn [{:keys [conn] :as data}]
        (let [publish artifact/publish-value!]
          (with-redefs [artifact/publish-value!
                        (fn [store value callback]
                          (let [result (publish store value callback)]
                            (when (case boundary :claim (:batch-policy value) :freeze (:captured value))
                              (throw (ex-info "Injected batch ACK loss" {})))
                            result))]
            (is (thrown-with-msg? Exception #"Injected batch ACK loss" (prepare data))))
          (with-redefs [inputs/capture (fn [& _] (throw (AssertionError. "No recapture after unknown ACK")))]
            (let [result (prepare data)]
              (is (= :held (:status result)))
              (is (= (case boundary :claim :capturing :freeze :ready) (:batch-state result)))
              (is (= (:batch result) (:knowledge.batch/id (batches/batch @conn (:batch result))))))))))))

(deftest permission-revocation-after-claim-prevents-source-copying
  (with-fixture
    (fn [{:keys [conn w] :as data}]
      (let [publish artifact/publish-value!]
        (with-redefs [artifact/publish-value!
                      (fn [store value callback]
                        (let [result (publish store value callback)]
                          (when (:batch-policy value)
                            (d/transact conn [{:room/id (:room w) :room/owner [:party/id (:member w)]}]))
                          result))
                      inputs/capture (fn [& _] (throw (AssertionError. "Revoked source read")))]
          (is (thrown? Exception (prepare data))))
        (is (= [:capturing] (mapv #(-> (d/pull @conn '[*] (:e %)) :knowledge.batch/state)
                                  (d/datoms @conn :avet :knowledge.batch/id))))))))

(deftest empty-evidence-skips-batch-claim-and-source-copying
  (with-fixture
    (fn [{:keys [owned conn] :as data}]
      (repo/remove! owned "inbox/meeting.md")
      (repo/remove! owned "inbox/second.md")
      (repo/stage-all! owned)
      (repo/commit! owned {:message "Empty source test"})
      (with-redefs [inputs/capture (fn [& _] (throw (AssertionError. "No source bytes for empty work")))]
        (is (= :empty (:status (prepare data)))))
      (is (empty? (d/datoms @conn :avet :knowledge.batch/id))))))
