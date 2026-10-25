(ns is.simm.ops.knowledge-retirement-gc-probe
  "Explicit three-process model/native file-store proof; not an ordinary test or
   persistent HOST reconstruction. Only this probe's disposable stores are GC'd.
   Run prepare, release, verify in three JVMs with the same manifest path."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [datahike.gc-roots :as roots]
            [dvergr.artifact :as artifact]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-batch-recovery :as recovery]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.knowledge-stream-retirement :as retirement]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-batch-inputs :as inputs]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.knowledge-checkpoint-roots :as checkpoints]
            [is.simm.ops.knowledge-inputs :as evidence]
            [konserve.core :as k]
            [superv.async :refer [<?? S]]))

(def ^:private marker :simmis/knowledge-retirement-gc-probe-v1)

(defn- config [root name]
  {:store {:backend :file :id (random-uuid) :path (str (io/file root name))}
   :writer {:backend :self :writer-ownership :exclusive}
   :schema-flexibility :write :keep-history? false :commit-graph? true
   :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0})

(defn- save! [path state] (spit path (pr-str state) :encoding "UTF-8"))

(defn- load-state! [path]
  (let [state (edn/read-string (slurp path :encoding "UTF-8"))
        parent (.getCanonicalFile (.getParentFile (.getAbsoluteFile (io/file path))))]
    (assert (= marker (:kind state)))
    (doseq [[key name] [[:source-config "source"] [:control-config "control"]]]
      (let [cfg (get state key)]
        (assert (= :file (get-in cfg [:store :backend])))
        (assert (uuid? (get-in cfg [:store :id])))
        (assert (= (.getCanonicalPath (io/file parent name))
                   (.getCanonicalPath (io/file (get-in cfg [:store :path])))))))
    state))

(defn- orphan! [source]
  (let [bytes (markdown/source-bytes (str "Positive unreachable-blob control " (random-uuid)))
        id (blob/blob-id bytes)]
    (k/bassoc (:store @source) id bytes {:sync? true})
    (assert (k/exists? (:store @source) id {:sync? true}))
    id))

(defn- prepare! [path]
  (assert (not (.exists (io/file path))) "Use a fresh disposable manifest")
  (let [root (.getCanonicalFile (.getParentFile (.getAbsoluteFile (io/file path))))
        _ (.mkdirs root)
        sc (config root "source") cc (config root "control")]
    (doseq [name ["source" "control"]]
      (assert (not (.exists (io/file root name))) "Probe refuses existing store directories"))
    (doseq [cfg [sc cc]] (d/create-database cfg))
    (let [source (d/connect sc) control (d/connect cc)]
      (try
        (repo/init! source {:name "Disposable retirement GC proof"})
        (d/transact control (vec (concat streams/schema pins/schema batches/schema recovery/schema
                                          requests/schema retirement/schema
                                          scheduled/schema)))
        (let [text (str "Unique old evidence " (random-uuid))]
          ;; No Git commit retains this fixture's old logical content. We later
          ;; prune its unreferenced content entity to make collection observable.
          (repo/write! source "inbox/evidence.md" (markdown/source-bytes text))
          (repo/write! source "wiki/context.md" (markdown/source-bytes "Retained context"))
          (let [inventory (changes/inventory @source evidence/default-policy)
                basis (:basis inventory)
                origin {:target (merge (select-keys basis [:repository :store :backend :branch :ref])
                                       {:registry-system (random-uuid) :runtime-system "disposable-gc-probe"})
                        :target-stamp (random-uuid) :workflow "wiki" :source-revision (random-uuid)}
                value (streams/descriptor {:by (random-uuid) :room (random-uuid) :target (pins/target-id (:target origin))
                                           :policy (hasch/uuid evidence/default-policy) :program (random-uuid) :epoch (random-uuid)})
                prepared (pins/prepare! control value nil basis origin)
                _ (assert (:new? prepared))
                verified (checkpoints/create! control @source prepared)
                _ (d/transact control [[:db.fn/call pins/mark-pinned-tx (:intent verified)]])
                intent (pins/intent @control (get-in prepared [:intent :knowledge.pin/id]))
                plan {:before nil :after basis
                      :changes (mapv (fn [[path metadata]] {:path path :kind :added :before nil :after metadata}) (:files inventory))}]
            (artifact/publish-value! (artifact/datahike-store control) plan
              #(d/transact control [[:db.fn/call pins/discovery-tx intent value nil plan origin %]]))
            (let [committed (pins/intent @control (:knowledge.pin/id intent))]
              (checkpoints/release-previous! control @source committed)
              (d/transact control [[:db.fn/call pins/finish-tx committed]]))
            (let [stream (streams/stream @control (:id value))
                  selected (inputs/selection @source value stream (streams/pending-items @control (:id value))
                             evidence/default-policy inputs/default-policy {:files 0 :bytes 0})
                  selection (:batch selected) claim (random-uuid)]
              (assert (= :selected (:status selected)))
              (artifact/publish-value! (artifact/datahike-store control) selection
                #(d/transact control [[:db.fn/call batches/claim-tx selection % claim]]))
              (let [row (batches/batch @control (:id selection))
                    frozen (inputs/capture @source selected (select-keys (:target origin) [:registry-system :runtime-system]))
                    frozen-id (hasch/uuid frozen)]
                (artifact/publish-value! (artifact/datahike-store control) frozen
                  #(d/transact control [[:db.fn/call batches/freeze-tx row % frozen-id]]))
                (let [ready (batches/batch @control (:id selection))
                      expected (recovery/expected {:row ready :selection selection} nil)
                      decision (recovery/decision {:batch (:id selection) :room (:room value) :by (:by value)
                                                   :client (random-uuid) :at (java.util.Date.) :mode :skip
                                                   :reason "Resolve disposable batch before clean retirement"})
                      entry (files/entry @source "inbox/evidence.md")
                      payload (:geschichte.content/payload (d/pull @source '[*] [:geschichte.content/id (:content entry)]))]
                  (assert (uuid? payload))
                  (d/transact control [[:db.fn/call recovery/abandon-tx expected decision]])
                  (repo/remove! source "inbox/evidence.md")
                  ;; Fixture-only pruning, NOT a new production Geschichte GC
                  ;; policy. The old native checkpoint still owns these bytes.
                  (d/transact source [[:db.fn/retractEntity [:geschichte.content/id (:content entry)]]])
                  (repo/write! source "inbox/current.md" (markdown/source-bytes "Different current evidence"))
                  (let [state {:kind marker :source-config sc :control-config cc :descriptor value
                               :root (:knowledge.pin/id intent) :checkpoint (:commit basis) :payload payload :text text
                               :batch-origin {:batch (:id selection) :selection (:id selection) :frozen frozen-id}
                               :client (random-uuid) :reason "Retire disposable completed stream"
                               :orphan-a (orphan! source)}]
                    (save! path state)
                    {:status :prepared :manifest path :models 0 :scope :model-native-file-store}))))))
        (finally (d/release source) (d/release control))))))

(defn- release! [path]
  (let [{:keys [source-config control-config descriptor root checkpoint payload text orphan-a] :as state} (load-state! path)
        source (d/connect source-config) control (d/connect control-config)]
    (try
      (let [swept (<?? S (d/gc-storage source (java.util.Date.)))
            old (d/commit-as-db source checkpoint)
            entry (files/entry old "inbox/evidence.md")]
        (assert (seq swept))
        (assert (not (k/exists? (:store @source) orphan-a {:sync? true})))
        (assert old "Canonical cursor root must retain checkpoint across process exit")
        (assert (= text (String. ^bytes (content/read-by-id (atom old) (:content entry)) "UTF-8")))
        (assert (k/exists? (:store @source) payload {:sync? true}))
        (let [stream (streams/stream @control (:id descriptor)) pin (pins/intent @control root)
              expected (retirement/expected stream pin (pins/payload control pin))
              value (retirement/decision {:stream (:id descriptor) :root root :by (:by descriptor)
                                          :client (:client state) :reason (:reason state) :at (java.util.Date.)})
              native roots/release! lost? (atom false)]
          (d/transact control [[:db.fn/call retirement/begin-tx expected value]])
          (assert (try
                    (with-redefs [roots/release!
                                  (fn [& args]
                                    (let [result (apply native args)]
                                      (when (and (= root (second args)) (compare-and-set! lost? false true))
                                        (throw (ex-info "Injected native release ACK loss" {:probe/ack-loss true})))
                                      result))]
                      ;; Explicit no-auth boundary: this probe proves native/model
                      ;; GC, while separate HOST tests exercise real authority.
                      (checkpoints/release-retiring! control @source expected value (fn [] true)))
                    false (catch clojure.lang.ExceptionInfo e (true? (:probe/ack-loss (ex-data e))))))
          (assert @lost?)
          (assert (= :retiring (:knowledge.stream/state (streams/stream @control (:id descriptor)))))
          (assert (not (contains? (roots/roots (:store @source) {:sync? true}) root)))
          (save! path (assoc state :decision value :orphan-b (orphan! source) :phase-b {:swept (count swept) :lost-release-ack true}))
          {:status :retiring :lost-release-ack true :scope :model-native-file-store :models 0}))
      (finally (d/release source) (d/release control)))))

(defn- verify! [path]
  (let [{:keys [source-config control-config descriptor root checkpoint payload decision batch-origin orphan-b] :as state} (load-state! path)
        _ (assert (true? (get-in state [:phase-b :lost-release-ack])))
        source (d/connect source-config) control (d/connect control-config)]
    (try
      (let [swept (<?? S (d/gc-storage source (java.util.Date.)))
            stream (streams/stream @control (:id descriptor)) pin (pins/intent @control root)
            expected (retirement/expected stream pin (pins/payload control pin))]
        (assert (seq swept))
        (assert (not (k/exists? (:store @source) orphan-b {:sync? true})))
        (assert (nil? (d/commit-as-db source checkpoint)) "Old source checkpoint must actually be collected")
        (assert (not (k/exists? (:store @source) payload {:sync? true})) "Unique old payload must actually be collected")
        (with-redefs [d/commit-as-db (fn [& _] (throw (AssertionError. "Recovery loaded collected checkpoint")))
                      roots/pin! (fn [& _] (throw (AssertionError. "Recovery recreated root")))
                      roots/release! (fn [& _] (throw (AssertionError. "Recovery released another root")))]
          (checkpoints/release-retiring! control @source expected decision (fn [] true)))
        (d/transact control [[:db.fn/call retirement/finish-tx expected decision]])
        (assert (= :retired (:knowledge.stream/state (streams/stream @control (:id descriptor)))))
        (<?? S (d/gc-storage control (java.util.Date.)))
        (let [proof ((requiring-resolve 'is.simm.ops.knowledge-batch-proof/retained!) control (:by descriptor) (:room descriptor) batch-origin)
              before (d/commit-id @control)]
          (assert (= (:text state) (get-in proof [:frozen :captured :inputs "/in/knowledge/sources/inbox/evidence.md"])))
          (assert (= [] (retirement/begin-tx @control expected decision)))
          (assert (= [] (retirement/finish-tx @control expected decision)))
          (assert (= before (d/commit-id @control)))
          (let [result {:status :verified :scope :model-native-file-store :fresh-processes 3 :models 0
                        :source-checkpoint-collected true :source-payload-collected true :root-recreated false
                        :lost-release-ack-recovered true :frozen-proof-after-control-gc true :swept-source-keys (count swept)}]
            (save! (str path ".verified.edn") result)
            result)))
      (finally (d/release source) (d/release control)))))

(defn -main [phase path]
  (try
    (when-not (and path (#{"prepare" "release" "verify"} phase))
      (throw (ex-info "Usage: knowledge-retirement-gc-probe prepare|release|verify MANIFEST" {})))
    (prn (case phase "prepare" (prepare! path) "release" (release! path) "verify" (verify! path)))
    (shutdown-agents)
    (System/exit 0)
    (catch Throwable error
      (.printStackTrace error)
      (shutdown-agents)
      (System/exit 1))))
