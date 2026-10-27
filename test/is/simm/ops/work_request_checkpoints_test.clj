(ns is.simm.ops.work-request-checkpoints-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [datahike.blob :as blob]
            [dvergr.artifact :as artifact]
            [datahike.gc-roots :as roots]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as files]
            [hasch.core :as hasch]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.knowledge-pins-test :as pin-fixtures]
            [is.simm.model.knowledge-pins :as pins]
            [is.simm.model.work-request-checkpoints :as model]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.knowledge-changes :as changes]
            [is.simm.ops.knowledge-batch-proof :as batch-proof]
            [is.simm.ops.work-request-checkpoints :as checkpoints]
            [konserve.core :as k]
            [superv.async :refer [<?? S]]))

(defn- fixture
  ([f] (fixture {:backend :memory :id (random-uuid)} f))
  ([source-store f]
   (let [common {:schema-flexibility :write :keep-history? false :commit-graph? true
                 :max-string-length 0 :max-bytes-length 0 :max-tuple-string-length 0}
         cc (assoc common :store {:backend :memory :id (random-uuid)})
         sc (assoc common :store source-store :writer {:backend :self :writer-ownership :exclusive})]
     (d/create-database cc)
     (d/create-database sc)
     (let [control (d/connect cc) source (d/connect sc)]
       (try
         (d/transact control (vec (concat requests/schema model/schema pins/schema)))
         (repo/init! source {:name "Disposable Request checkpoints"})
         (repo/write! source "wiki/context.md" (markdown/source-bytes "C1 retained context"))
         (let [basis (changes/basis @source)
               selection {:version 1 :policy :request-evidence-v1
                          :origin {:version 1 :kind :datahike :commit (:commit basis)
                                   :source (select-keys basis [:store :repository])}
                          :basis basis :target (:target (pin-fixtures/origin basis))}
               publication-body {:version 1 :root "wiki" :target (:target selection) :files {} :directories []}
               publication (assoc publication-body :id (hasch/uuid [:simmis/knowledge-publication-basis-v1 publication-body]))
               input {:party (random-uuid) :room (random-uuid) :client-id (random-uuid)
                      :kind :workflow :intent {:knowledge/checkpoint selection
                                               :knowledge/publication-basis publication
                                               :workflow {:input-selection {:execution-id (random-uuid)}}}}
               request (:request (requests/claim! control input))
               submitted (requests/record-job! control (:work.request/id request)
                                               (:work.request/claim request) (random-uuid))]
           (f control source submitted input))
         (finally (d/release control) (d/release source)
                  (d/delete-database cc) (d/delete-database sc)))))))

(defn- reason [f]
  (try (f) nil (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))

(deftest request-pin-is-independent-of-cursor-and-survives-job-settlement
  (fixture
   (fn [control source request _]
     (let [prepared (model/prepare! control request)
           id (:work.request/id request)
           root (get-in prepared [:intent :work.checkpoint/id])
           verified (checkpoints/create! control @source prepared)]
       (is (:new? prepared))
       (is (= root (get-in verified [:intent :work.checkpoint/id])))
       (is (= (:basis verified) (changes/basis @source)))
       (checkpoints/acknowledge! control @source id)
       (requests/mark-returned! control id)
       (repo/write! source "wiki/context.md" (markdown/source-bytes "C2 current context"))
       (let [again (checkpoints/verify! control @source id)]
         (is (= :retained (get-in again [:intent :work.checkpoint/state])))
         (is (not= (:commit (:basis again)) (:commit (changes/basis @source))))
         (is (contains? (k/get (:store @source) roots/registry-key nil {:sync? true}) root)))
       (is (= 0 (count (d/q '[:find ?e :where [?e :knowledge.pin/id _]] @control))))))))

(deftest concurrent-preparation-has-one-native-creation-winner
  (fixture
   (fn [control source request _]
     (let [gate (promise)
           done (java.util.concurrent.CountDownLatch. 4)
           contenders (mapv (fn [_] (future
                                      (try @gate (model/prepare! control request)
                                           (finally (.countDown done))))) (range 4))]
       (try
         (deliver gate true)
         (let [results (mapv #(deref % 10000 ::timeout) contenders)]
           (is (not-any? #{::timeout} results))
           (is (= 1 (count (filter :new? results))))
           (is (= 1 (count (set (map #(get-in % [:intent :work.checkpoint/id]) results)))))
           (let [winner (first (filter :new? results))
                 loser (first (remove :new? results))]
             (is (= :not-creation-winner (reason #(checkpoints/create! control @source loser))))
             (checkpoints/create! control @source winner)
             (is (= :root-already-present (reason #(checkpoints/create! control @source winner))))))
         (finally
           (deliver gate true)
           ;; Join physical completion before the fixture closes its stores,
           ;; even when a result timed out or another contender failed.
           (.await done)))))))

(deftest lost-control-ack-uses-only-the-original-fresh-token
  (fixture
   (fn [control _ request _]
     (let [native d/transact
           result (with-redefs [d/transact (fn [& args] (apply native args)
                                             (throw (ex-info "Injected control ACK loss" {})))]
                    (model/prepare! control request))]
       (is (:new? result))
       (let [retry (model/prepare! control request)]
         (is (false? (:new? retry)))
         (is (= (:intent result) (:intent retry))))))))

(deftest interrupted-control-ack-never-grants-native-creation
  (doseq [wrapped? [false true]]
    (fixture
     (fn [control _ request _]
       (let [publish artifact/publish-value!
             interruption (InterruptedException. "Interrupted after publication")
             error (if wrapped? (ex-info "Wrapped interruption" {} interruption) interruption)
             caught (try
                      (with-redefs [artifact/publish-value!
                                    (fn [& args] (apply publish args) (throw error))]
                        (model/prepare! control request))
                      (catch Exception e e))
             interrupted? (Thread/interrupted)]
         (is (identical? error caught))
         (is interrupted?)
         ;; Clear the interrupt before storage cleanup. The durable intent
         ;; remains visible, but a retry cannot claim fresh pin authority.
         (is (some? (model/intent @control (:work.request/id request))))
         (is (false? (:new? (model/prepare! control request)))))))))

(deftest interrupted-control-readback-never-grants-native-creation
  (doseq [wrapped? [false true]]
    (fixture
     (fn [control _ request _]
       (let [publish artifact/publish-value!
             intent model/intent
             committed? (atom false)
             interruption (InterruptedException. "Interrupted control readback")
             error (if wrapped? (ex-info "Wrapped readback interruption" {} interruption) interruption)
             caught (try
                      (with-redefs [artifact/publish-value!
                                    (fn [& args]
                                      (apply publish args)
                                      (reset! committed? true)
                                      (throw (ex-info "Lost publication ACK" {})))
                                    model/intent
                                    (fn [& args]
                                      (if @committed? (throw error) (apply intent args)))]
                        (model/prepare! control request))
                      (catch Exception e e))
             interrupted? (Thread/interrupted)]
         (is (identical? error caught))
         (is interrupted?)
         (is (false? (:new? (model/prepare! control request)))))))))

(deftest lost-pin-ack-and-partial-publication-never-replay-the-native-write
  (fixture
   (fn [control source request _]
     (let [prepared (model/prepare! control request)
           root (get-in prepared [:intent :work.checkpoint/id])
           native roots/pin! calls (atom 0)]
       (with-redefs [roots/pin! (fn [& args] (swap! calls inc) (apply native args)
                                  (throw (ex-info "Injected pin ACK loss" {})))]
         (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Injected pin ACK loss"
                               (checkpoints/create! control @source prepared))))
       (is (= 1 @calls))
       (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "Never recreate in recovery")))]
         (is (= :retained (get-in (checkpoints/acknowledge! control @source (:work.request/id request))
                                  [:intent :work.checkpoint/state])))
         (roots/release! @source root {:sync? true})
         (is (= :pin-unavailable (reason #(checkpoints/verify! control @source (:work.request/id request)))))
         (is (= :not-creation-winner
                (reason #(checkpoints/create! control @source (model/prepare! control request))))))))))

(deftest changed-request-cannot-create-a-pin
  (fixture
   (fn [control source request _]
     (let [prepared (model/prepare! control request)]
       (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "No invalid native creation")))]
         (d/transact control [[:db/add (:db/id request) :work.request/job (random-uuid)]])
         (is (= :payload-mismatch (reason #(checkpoints/create! control @source prepared)))))))))

(deftest mismatched-publication-target-is-refused-before-pin-io
  (fixture
   (fn [control _ _ input]
     (let [publication (-> (get-in input [:intent :knowledge/publication-basis])
                           (dissoc :id) (assoc-in [:target :runtime-system] "other-memory"))
           publication (assoc publication :id (hasch/uuid [:simmis/knowledge-publication-basis-v1 publication]))
           input (-> input (assoc :client-id (random-uuid))
                     (assoc-in [:intent :knowledge/publication-basis] publication))
           claimed (:request (requests/claim! control input))
           request (requests/record-job! control (:work.request/id claimed)
                                         (:work.request/claim claimed) (random-uuid))]
       (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "No mismatched-source pin")))]
         (is (= :publication-target-mismatch (reason #(model/prepare! control request)))))))))

(defn- synthetic-batch-request!
  "A deliberately incomplete batch fixture; canonical batch proof is tested
   at the native boundary, never bypassed in production."
  [control input]
  (let [batch (random-uuid)
        input (-> input (assoc :client-id (random-uuid))
                  (assoc-in [:intent :knowledge/batch] {:batch batch :selection batch :frozen (random-uuid)}))
        {:keys [intent digest] :as prepared} (requests/prepare-claim input)
        {:keys [row claim]} (requests/claim-row prepared)]
    (artifact/publish-value! (artifact/datahike-store control) intent
                             #(d/transact control [[:db.fn/call requests/claim-tx (assoc row :work.request/payload-ref %) digest]]))
    (requests/record-job! control (:id prepared) claim (random-uuid))))

(deftest mismatched-retained-batch-is-refused-before-native-creation
  (fixture
   (fn [control source _ input]
     (let [request (synthetic-batch-request! control input)
           prepared (model/prepare! control request)]
       (with-redefs [batch-proof/request! (fn [& _] {:selection {:basis {}}})
                     roots/pin! (fn [& _] (throw (AssertionError. "No mismatched-batch pin")))]
         (is (= :batch-checkpoint-mismatch (reason #(checkpoints/create! control @source prepared)))))))))

(deftest missing-batch-markers-cannot-skip-the-retained-proof
  (doseq [marker [:work.request/knowledge-batch :work.request/knowledge-frozen]]
    (fixture
     (fn [control source _ input]
       (let [request (synthetic-batch-request! control input)
             prepared (model/prepare! control request)]
         (d/transact control [[:db/retract (:db/id request) marker (get request marker)]])
         (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "No partial-linkage pin")))]
           (is (= :batch-linkage-mismatch (reason #(checkpoints/create! control @source prepared))))))))))

(deftest partial-native-record-or-registry-is-never-overwritten
  (doseq [damage [:record-only :registry-only]]
    (fixture
     (fn [control source request _]
       (let [prepared (model/prepare! control request)
             root (get-in prepared [:intent :work.checkpoint/id])]
         (checkpoints/create! control @source prepared)
         (case damage
           :record-only (roots/release! @source root {:sync? true})
           :registry-only (k/dissoc (:store @source) (roots/record-key root) {:sync? true}))
         (with-redefs [roots/pin! (fn [& _] (throw (AssertionError. "No partial-outcome overwrite")))]
           (is (= :pin-unavailable (reason #(checkpoints/verify! control @source (:work.request/id request)))))
           (is (= :root-already-present (reason #(checkpoints/create! control @source prepared))))))))))

(deftest selection-is-closed-and-exact-checkpoint-is-part-of-request-identity
  (fixture
   (fn [control _ _ input]
     (let [selection (get-in input [:intent :knowledge/checkpoint])]
       (doseq [bad [(assoc selection :extra true)
                    (assoc-in selection [:origin :commit] (random-uuid))
                    (assoc-in selection [:target :store] (random-uuid))
                    (assoc selection :policy :unknown)]]
         (is (thrown? clojure.lang.ExceptionInfo (model/validate-selection! bad))))
       (is (thrown? clojure.lang.ExceptionInfo
                    (requests/claim! control (assoc-in input [:intent :knowledge/checkpoint :origin :commit]
                                                       (random-uuid)))))))))

(deftest gc-retains-request-checkpoint-and-its-blob-after-source-churn
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "simmis-request-pin-gc-"
                                                              (make-array java.nio.file.attribute.FileAttribute 0)))]
    (try
      (fixture {:backend :file :id (random-uuid) :path (.getPath (io/file dir "source"))}
               (fn [control source request _]
                 (let [prepared (model/prepare! control request)
                       id (:work.request/id request)
                       verified (checkpoints/create! control @source prepared)
                       entry (files/entry (:db verified) "wiki/context.md")
                       bytes (markdown/source-bytes (str "Unreachable positive GC control " (random-uuid)))
                       orphan (blob/blob-id bytes)]
                   (checkpoints/acknowledge! control @source id)
       ;; This disposable repo has no logical commits referencing the old
       ;; content. Remove its live entity so only the C1 pin retains its blob.
                   (repo/write! source "wiki/context.md" (markdown/source-bytes "C2 current context"))
                   (d/transact source [[:db.fn/retractEntity [:geschichte.content/id (:content entry)]]])
                   (k/bassoc (:store @source) orphan bytes {:sync? true})
                   (is (k/exists? (:store @source) orphan {:sync? true}))
                   (let [swept (<?? S (d/gc-storage source (java.util.Date.)))
                         retained (checkpoints/verify! control @source id)]
                     (is (seq swept))
                     (is (not (k/exists? (:store @source) orphan {:sync? true})))
                     (is (= "C1 retained context"
                            (String. ^bytes (content/read-by-id (atom (:db retained)) (:content entry)) "UTF-8")))
                     (is (not= (:commit (:basis retained)) (:commit (changes/basis @source))))))))
      (finally
        (doseq [file (reverse (file-seq dir))] (io/delete-file file true))))))
