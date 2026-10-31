(ns is.simm.ops.knowledge-coverage-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.agent.evaluation :as evaluation]
            [is.simm.ops.knowledge-coverage :as coverage]))

(def trigger #uuid "00000000-0000-4000-8000-000000000001")
(def source-a #uuid "00000000-0000-4000-8000-000000000002")
(def source-b #uuid "00000000-0000-4000-8000-000000000003")
(def selection {:version 1 :trigger-id trigger
                :items [{:input "/in/knowledge/sources/inbox/message.md" :content source-a}
                        {:input "/in/knowledge/sources/captures/company.md" :content source-b}]})
(def report {:version 1 :trigger-id trigger
             :items [(merge (first (:items selection))
                            {:status :incorporated :targets ["wiki/Acme.md"]
                             :note "The exact statement supports the renewal date."})
                     (merge (second (:items selection))
                            {:status :incorporated :targets ["wiki/Acme.md"]
                             :note "The website observation supports the company description, not a client relationship."})]})
(defn target? [path] (= "wiki/Acme.md" path))

(deftest digest-omissions-cannot-remove-original-coverage-obligations
  ;; Neither a digest nor its topic list is an input to this contract.
  (is (= :ready (:status (coverage/inspect selection report #{"wiki/Acme.md"} target?))))
  (doseq [bad [(update report :items pop)
               (assoc-in report [:items 1] (first (:items report)))
               (assoc-in report [:items 1 :content] source-a)
               (assoc-in report [:items 1 :input] "/in/knowledge/sources/foreign.md")]]
    (is (thrown? Exception (coverage/inspect selection bad #{"wiki/Acme.md"} target?)))))

(deftest unresolved-is-held-even-when-other-evidence-produces-useful-output
  (let [partial (assoc-in report [:items 1]
                          (merge (second (:items selection))
                                 {:status :deferred :targets [] :note "Original image unavailable."}))]
    (is (= :held (:status (coverage/inspect selection partial #{"wiki/Acme.md"} target?)))))
  (let [no-change (update report :items
                          #(mapv (fn [item] (assoc item :status :no-change :targets [])) %))]
    (is (= :ready (:status (coverage/inspect selection no-change #{} target?))))
    (is (thrown? Exception (coverage/inspect selection no-change #{"wiki/Acme.md"} target?)))))

(deftest reports-cannot-invent-output-or-quietly-omit-output-attribution
  (is (thrown? Exception (coverage/inspect selection report #{} target?)))
  (is (thrown? Exception
               (coverage/inspect selection report #{"wiki/Acme.md" "wiki/Unrelated.md"} (constantly true))))
  (is (thrown? Exception
               (coverage/inspect selection (assoc-in report [:items 0 :targets] ["../outside.md"])
                                 #{"../outside.md"} target?))))

(deftest already-represented-needs-a-real-retained-target
  (let [represented (update report :items
                            #(mapv (fn [item] (assoc item :status :already-represented)) %))
        scoped {:version 2 :output-root "out/knowledge" :destination-root "wiki" :exclude-prefixes []}
        known (coverage/with-existing-targets selection
                                              {:files {"wiki/Acme.md" {:mode 33188}
                                                       "wiki/not-regular.md" {:mode 40960}}} scoped)]
    (is (thrown? Exception (coverage/inspect selection represented #{} target?)))
    (is (= :ready (:status (coverage/inspect known represented #{} target?))))
    (is (= #{"wiki/Acme.md"} (:existing-targets known)))))

(deftest parser-and-opt-in-are-inert-and-closed
  (is (nil? (coverage/declaration {})))
  (is (= coverage/declaration-value
         (coverage/declaration {"coverage-policy.edn" (pr-str coverage/declaration-value)})))
  (doseq [text ["#=(spit \"bad\" \"bad\")" "{} {}" "#foreign {}" ""
                (apply str (repeat (inc coverage/max-report-bytes) "x"))]]
    (is (thrown? Exception (coverage/read-value text))))
  (is (= selection (coverage/expected {:trigger-id trigger
                                     :files (mapv #(assoc % :role :trigger) (:items selection))}))))

(deftest held-coverage-retains-output-but-cannot-be-filed-as-a-no-op
  (let [output-scope {:version 2 :output-root "out/knowledge" :destination-root "wiki"
                      :exclude-prefixes []}
        complete {:files {coverage/output-path (pr-str report)}
                  :knowledge-output {:files {"wiki/Acme.md" {}}}}
        base (evaluation/make-evaluator
              {:id :fixture/completion :version 1 :basis :fixture
               :capture (constantly complete)
               :observe (fn [{:keys [execution/evidence]}] evidence)
               :verify (fn [_ _] {:checks {:completed? true} :reward 1.0})})
        wrapped (coverage/with-capture base selection output-scope)
        evidence ((:capture wrapped) {})
        observed ((:observe wrapped) {:execution/evidence evidence})]
    (is (= 1.0 (:reward ((:verify wrapped) {} observed))))
    (is (= :ready (:status (coverage/require-ready! selection output-scope evidence))))
    (let [incomplete (dissoc (:files complete) coverage/output-path)
          held-base (assoc base :capture (constantly (assoc complete :files incomplete)))
          held (coverage/with-capture held-base selection output-scope)
          retained ((:capture held) {})
          observation ((:observe held) {:execution/evidence retained})]
      (is (= (:knowledge-output complete) (:knowledge-output retained)))
      (is (= 0.0 (:reward ((:verify held) {} observation))))
      (is (false? (get-in ((:verify held) {} observation) [:checks :original-evidence-covered?])))
      (is (thrown? Exception (coverage/require-ready! selection output-scope retained))))))
