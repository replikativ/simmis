(ns is.simm.starter.conversation-digest-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.blob :as blob]
            [dvergr.catalog.room :as catalog]
            [is.simm.model.conversation-digest :as digest]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.room-evidence :as evidence]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.starter.conversation-digest :as starter]))

(defn fixture []
  (let [room #uuid "60cff6c4-11e1-4403-9930-6cf280c60c04"
        store #uuid "0858bdea-8ef4-4054-815a-dbe15d26e370"
        artifact (evidence/artifact room store
                                   {:id #uuid "913cb079-fd63-4e3a-838f-ff469fb1767a"
                                    :timestamp 1793400000000 :role :assistant
                                    :text "Synthetic assistant proposal: review the draft. This is not a human-approved decision."})
        artifact (assoc artifact :authority :assistant
                        :file-content (blob/blob-id (.getBytes ^String (:text artifact) "UTF-8")))
        selection (digest/selection [artifact]) revision (:source-revision artifact)
        value {:version 1 :selection-id (:id selection)
               :summary {:text "An assistant proposal remains unapproved." :sources [revision]}
               :coverage [{:source-revision revision :status :selected :note "Read the original assistant proposal."}]
               :sections {:decisions []
                          :proposals [{:text "Assistant proposed reviewing the draft." :sources [revision]}]
                          :observations []
                          :uncertainty [{:text "Human approval is not recorded." :sources [revision]}]
                          :open-work []}}
        gold {:version 1 :required [(str (:id selection)) (str revision) "Assistant proposed" "Human approval is not recorded"]
              :forbidden ["The human approved the purchase"]}]
    {:artifact artifact :selection selection :digest (digest/validate! selection value) :gold gold}))

(defn example-source []
  (let [{:keys [artifact selection digest gold]} (fixture)
        source (starter/source)
        definition (assoc-in (edn/read-string (get source "workflow.edn")) [:params :evidence-manifest] (pr-str selection))]
    (assoc source "workflow.edn" (pr-str definition)
           "fixture-pack.edn" (pr-str {:kind :dvergr/fixture-pack :version 1
                                       :files {(str "/" (:path artifact)) (:text artifact)}})
           "gold.edn" (pr-str gold)
           "calibration.edn" (pr-str {:reference {"/out/digest.edn" (pr-str digest)}
                                      :damaged {:missing {:files {} :loses [:expected-output?]}
                                                :invented-approval
                                                {:files {"/out/digest.edn" (str (pr-str digest) " The human approved the purchase")}
                                                 :loses [:forbidden?]}}}))))

(defn write-example! [directory]
  (let [files (example-source)]
    (doseq [[path text] files]
      (let [file (io/file directory path)] (io/make-parents file) (spit file text)))
    {:files (count files)}))

(deftest generic-digest-workflow-pins-explicit-selection-with-no-knowledge-sidecar
  (let [source (starter/source) bundle (catalog/bundle starter/name source)
        {:keys [selection]} (fixture)
        prepared (runs/prepare-source {:files source}
                                      {:name starter/name :expected-bundle (:id bundle)
                                       :params {"evidence-manifest" (pr-str selection)}
                                       :inputs {(str "/" (:path (:artifact (fixture)))) (:text (:artifact (fixture)))}})]
    (is (nil? (inputs/declaration source)))
    (is (not (contains? source "input-batching.edn")))
    (is (not (contains? (:workflow prepared) :input-selection)))
    (is (str/includes? (:task prepared) (pr-str selection)))
    (is (str/includes? (:task prepared) "retained room fork"))
    (is (str/includes? (get source "fixtures/workflow-context/conversation-digest.txt") "Omission never changes"))))

(deftest optional-digest-checker-and-model-free-example
  (let [source (example-source) bundle (catalog/bundle starter/name source)
        {:keys [digest gold]} (fixture)
        reference {"/out/digest.edn" (pr-str digest)}
        verdict #(checkers/run-checker (get source "checker.clj")
                                      {:files % :gold gold :params {} :fetched {} :judgements {}})]
    (is (= 1.0 (:reward (verdict reference))))
    (is (false? (get-in (verdict {}) [:checks :expected-output?])))
    (is (false? (get-in (verdict {"/out/digest.edn" "The human approved the purchase"}) [:checks :forbidden?])))
    (is (:ok? (checkers/calibrate bundle)))
    (is (map? (:inputs (runs/prepare-source {:files source} {:name starter/name :expected-bundle (:id bundle)}))))))

(deftest host-verifies-frozen-bytes-before-the-worker-inspects-their-meaning
  (let [source (starter/source) bundle (catalog/bundle starter/name source)
        {:keys [artifact selection]} (fixture)
        path (str "/" (:path artifact))
        input {:name starter/name :expected-bundle (:id bundle)
               :params {"evidence-manifest" (pr-str selection)}
               :inputs {path (:text artifact)}}
        prepared (runs/prepare-source {:files source} input)
        task (:task prepared)
        context (get source "fixtures/workflow-context/conversation-digest.txt")]
    (is (= (:text artifact) (get (:inputs prepared) path)))
    (is (thrown? Exception
                 (runs/prepare-source {:files source} (update-in input [:inputs path] str " changed")))
        "Frozen byte verification runs before any worker can receive the task")
    (is (str/includes? task "HOST already verified the exact frozen file-content bytes before admission"))
    (is (not (str/includes? task "verify its expected source-revision")))
    (is (str/includes? context "Those values need not match"))
    (doseq [operation ["(slurp" "(babashka.fs/create-dirs" "(spit"]]
      (is (str/includes? context operation)))))

(deftest checked-in-digest-example-matches-the-shipped-source-and-fixture-generator
  (doseq [[path expected] (example-source)]
    (is (= expected (slurp (io/file "examples/workflows/conversation-digest-eval-v1" path))) path)))
