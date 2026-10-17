(ns is.simm.ops.worker-executor-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.catalog.workspace :as catalog]
            [dvergr.discourse :as discourse]
            [dvergr.model.providers :as providers]
            [dvergr.room.registry :as registry]
            [dvergr.rooms.forks :as forks]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.ops.worker-executor :as executor]
            [is.simm.ops.worker-files :as files]
            [is.simm.ops.worker-inference :as inference]
            [is.simm.ops.worker-inference-test :as fixture]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Base64]))

(defn descriptor [values mode]
  (let [bytes (byte-array (map unchecked-byte values))]
    {:base64 (.encodeToString (Base64/getEncoder) bytes) :bytes (alength bytes)
     :sha256 (files/sha256 bytes) :mode mode}))

(def binary (descriptor [0 255 128 10 42] 0x81ED))
(def input-files {"docs/input.bin" binary "docs/remove.txt" (descriptor [104 105] 0x81A4)})

(defn frozen
  ([] (frozen input-files))
  ([selected]
   (let [input {:task "Work on the selected files and put new deliverables under /out/."
                :config fixture/config :files selected
                :limits {:timeout-ms 60000 :settlement "review"} :policy files/policy}]
     (assoc input :input-id (hasch/uuid [:simmis/worker-input-v1 input])))))

(defn with-inference [infer f]
  (let [broker (inference/broker fixture/config infer)
        provider (inference/forward-provider fixture/config #(fixture/wire (broker (fixture/wire %))))]
    (with-redefs [providers/ensure-initialized! (constantly nil)
                  providers/providers (atom {:codex-subscription provider})]
      (f))))

(deftest invalid-inputs-refuse-before-guest-allocation
  (doseq [bad [(assoc (frozen) :input-id (random-uuid))
               (assoc (frozen) :unexpected true)
               (assoc-in (frozen) [:files "docs/input.bin" :sha256] (apply str (repeat 64 "0")))
               (assoc-in (frozen) [:files "docs/input.bin"] nil)
               (assoc-in (frozen) [:policy :lease-ttl-ms] 999999)]]
    (let [allocated (atom false)]
      (with-redefs [discourse/make-room (fn [& _] (reset! allocated true) (throw (Exception. "Must not allocate")))]
        (is (thrown? clojure.lang.ExceptionInfo (executor/execute! bad))))
      (is (false? @allocated)))))

(deftest actual-guest-loop-preserves-binary-modes-deletes-and-settles-before-export
  (let [calls (atom 0)
        parent (atom nil)
        parent-before-settlement (atom nil)
        original catalog/ensure-workspace!]
    (with-redefs [catalog/ensure-workspace! (fn [room] (reset! parent room) (original room))]
      (let [result
            (with-inference
              (fn [_ _]
                (case (swap! calls inc)
                  1 (assoc fixture/answer :content "" :stop-reason :tool-use
                           :tool-calls [{:id "copy" :name "clojure_eval"
                                         :input {:code "(do (babashka.fs/create-dirs \"/out\") (babashka.fs/copy \"/docs/input.bin\" \"/out/result.bin\") (babashka.process/shell \"chmod +x /out/result.bin\") (babashka.fs/delete \"/docs/remove.txt\"))"}}])
                  2 (do
                      (reset! parent-before-settlement
                              (files/freeze @parent "Before settlement" fixture/config
                                            (vec (keys input-files)) (:limits (frozen))))
                      (assoc fixture/answer :content "Prepared the binary deliverable and removed the selected file."))))
              #(executor/execute! (frozen)))]
        (is (= 2 @calls))
        (is (= input-files (:files @parent-before-settlement)) "tool writes remained in the guest Run fork")
        (is (= {:files {"out/result.bin" binary "docs/remove.txt" nil}
                :summary "Prepared the binary deliverable and removed the selected file." :exit-code 0}
               result))))))

(deftest failed-guest-run-does-not-export-or-merge-partial-tool-writes
  (let [calls (atom 0)
        partial-observed? (atom false)
        parent (atom nil)
        at-close (atom nil)
        original catalog/ensure-workspace!
        close discourse/close-room!]
    (with-redefs [catalog/ensure-workspace! (fn [room] (reset! parent room) (original room))
                  discourse/close-room! (fn [room]
                                          (when (identical? room @parent)
                                            (binding [ec/*execution-context* (:ctx room)]
                                              (reset! at-close (set (keys (repo/worktree (:conn (workspace/current-workspace))))))))
                                          (close room))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (with-inference
                     (fn [messages _]
                       (if (= 1 (swap! calls inc))
                         (assoc fixture/answer :content "" :stop-reason :tool-use
                                :tool-calls [{:id "partial" :name "clojure_eval"
                                              :input {:code "(do (spit \"/out/partial.txt\" \"must not escape\") (slurp \"/out/partial.txt\"))"}}])
                         (do (reset! partial-observed?
                                     (boolean (some #(and (= "function_call_output" (:type %))
                                                          (re-find #"must not escape" (:output %))) messages)))
                             (throw (ex-info "model connection failed" {})))))
                     #(executor/execute! (frozen)))))
      (is (= 2 @calls))
      (is (true? @partial-observed?))
      (is (= (set (keys input-files)) @at-close)))))

(defn run-code [input code]
  (let [calls (atom 0)]
    (with-inference
      (fn [_ _]
        (if (= 1 (swap! calls inc))
          (assoc fixture/answer :content "" :stop-reason :tool-use
                 :tool-calls [{:id "edit" :name "clojure_eval" :input {:code code}}])
          fixture/answer))
      #(executor/execute! input))))

(deftest empty-inputs-mode-only-edits-and-nondeliverable-scratch
  (is (= {:files {"out/result.txt" (descriptor [111 107] 0x81A4)} :summary "Done" :exit-code 0}
         (run-code (frozen {}) "(spit \"/out/result.txt\" \"ok\")")))
  (is (= {:files {"docs/input.bin" (assoc binary :mode 0x81A4)} :summary "Done" :exit-code 0}
         (run-code (frozen) "(babashka.process/shell \"chmod 644 /docs/input.bin\")")))
  (is (= {:files {} :summary "Done" :exit-code 0}
         (run-code (frozen) "(spit \"/scratch.txt\" \"not a selected file or deliverable\")"))))

(deftest oversized-deliverable-refuses-the-entire-result
  (is (thrown? clojure.lang.ExceptionInfo
               (run-code (frozen {}) "(spit \"/out/large.txt\" (apply str (repeat 1048577 \"x\")))"))))

(deftest model-success-with-failed-settlement-never-exports
  (let [guarded (atom 0)
        close discourse/close-room!]
    (with-redefs [forks/workspace-merge-guard (fn [& _] (fn [_] (swap! guarded inc)
                                                          (throw (ex-info "Merge refused" {}))))
                  discourse/close-room!
                  (fn [room]
                    ;; This test runs in a shared JVM, not a disposable process.
                    ;; Discard this guest's retained review fork before closing.
                    (binding [ec/*execution-context* (:ctx room)]
                      (doseq [child (registry/children (:id room)) :when (discourse/fork-handle child)]
                        (discourse/discard child)))
                    (close room))]
      (is (thrown? clojure.lang.ExceptionInfo
                   (run-code (frozen {}) "(spit \"/out/result.txt\" \"not settled\")")))
      (is (= 1 @guarded)))))
