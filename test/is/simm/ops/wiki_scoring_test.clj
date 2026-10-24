(ns is.simm.ops.wiki-scoring-test
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.blob :as blob]
            [hasch.core :as hasch]
            [dvergr.agent.evaluation :as evaluation]
            [is.simm.ops.checkers :as checkers]
            [is.simm.ops.knowledge-inputs :as inputs]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.workflows :as workflows]
            [is.simm.starter.wiki-maintenance :as starter]
            [dvergr.catalog.room :as catalog]))

(def repository #uuid "0418db64-3755-45f1-b522-891941563963")
(def page "1cce4d07-3c56-4678-b348-1af7eb2d49ed")
(def other-page "71ae5b64-4173-48d5-a3ad-17b3021cadc3")
(def block-id "9ec3553a-8879-4056-8e48-28593b9e2815")
(def source-path "/in/knowledge/sources/inbox/note.md")
(def source "The product price is an unvalidated hypothesis.")
(defn block [id body] (str "<!-- simmis:block " id " -->\n\n" body "\n\n<!-- /simmis:block -->\n"))
(def original (str "---\nsimmis-id: " page "\nrevenue: 0\nactive: false\n---\n" (block block-id "# Company")))
(def addition (str "\nThe product price is an unvalidated hypothesis.\n\n[Note](" source-path ")\n"))
(def changed (str original addition))
(def fresh (str "---\nsimmis-id: " other-page "\n---\n# Product\n" addition))

(defn frozen [sources]
  (let [entries (mapv (fn [[path text]]
                        {:path path :input (str "/in/knowledge/sources/" path)
                         :role (if (str/starts-with? path "wiki/") :context :trigger)
                         :content (blob/blob-id (.getBytes ^String text "UTF-8"))
                         :size (alength (.getBytes ^String text "UTF-8")) :mode 33188}) (sort-by key sources))
        by-role (fn [role] (into (sorted-map) (for [e entries :when (= role (:role e))]
                                                              [(:path e) (select-keys e [:content :size :mode])])))
        trigger (hasch/uuid {:version 1 :repository repository :policy inputs/default-policy :files (by-role :trigger)})
        execution (hasch/uuid {:trigger trigger :context (by-role :context)})
        manifest {:version 1 :repository repository :policy inputs/default-policy
                  :trigger-id trigger :execution-id execution :files entries}]
    (into {"/in/knowledge/manifest.edn" (pr-str manifest)}
          (map (fn [e] [(:input e) (get sources (:path e))])) entries)))

(defn bundle []
  {:name "wiki-v2" :id (random-uuid)
   :definition {:params {:simmis/checker-profile :wiki-structure-v1}}
   :checker (slurp (io/resource "is/simm/workflows/maintain-wiki/v2/checker.clj"))
   :fixtures (frozen {"wiki/Company.md" original "inbox/note.md" source})
   :cases {"supported" {:gold {:version 2 :no-change? false :required ["unvalidated hypothesis"]
                               :preserved {"wiki/Company.md" ["# Company"]}
                               :forbidden ["five paying customers"] :citations [source-path]
                               :quotes [{:source source-path :text source}] :sources {source-path source}}}
           "no-change" {:fixtures (frozen {"wiki/Company.md" changed "inbox/note.md" source})
                        :gold {:version 2 :no-change? true :required [] :preserved {}
                                :forbidden [] :citations [] :quotes []}}}})

(defn verdict [b case files]
  (checkers/score b inputs/default-policy {:environment/metadata {:case case}} files checkers/policy))

(deftest valid-old-page-new-page-and-no-change-all-pass
  (doseq [[case files] [["supported" {"/out/wiki/Company.md" changed}]
                       ["supported" {"/out/wiki/Product.md" fresh}]
                       ["no-change" {}]]]
    (is (every? true? (vals (:checks (verdict (bundle) case files)))))))

(deftest existing-metadata-and-block-structure-are-preserved-but-insertion-is-allowed
  (let [b (bundle)]
    (is (false? (get-in (verdict b "supported" {"/out/wiki/Company.md" (str/replace changed "active: false" "active: true")})
                       [:checks :preserved-structure?])))
    (is (false? (get-in (verdict b "supported" {"/out/wiki/Company.md" (str/replace changed block-id other-page)})
                       [:checks :preserved-structure?])))
    (is (true? (get-in (verdict b "supported" {"/out/wiki/Company.md" (str changed (block other-page "New section"))})
                      [:checks :preserved-structure?])))
    (is (false? (get-in (verdict b "supported" {"/out/wiki/Product.md" (str/replace fresh other-page page)})
                       [:checks :unique-identities?])))
    (is (false? (get-in (verdict b "supported"
                               {"/out/wiki/Company.md" (str/replace changed "# Company" "# Renamed")
                                "/out/wiki/Product.md" (str fresh "\n# Company")})
                       [:checks :preserved-content?])))))

(deftest unsupported-claims-and-citations-in-code-do-not-pass
  (let [b (bundle)]
    (is (false? (get-in (verdict b "supported" {"/out/wiki/Product.md" (str fresh "\nfive paying customers")})
                       [:checks :forbidden-content?])))
    (is (false? (get-in (verdict b "supported" {"/out/wiki/Product.md" (str/replace fresh (str "[Note](" source-path ")")
                                                                                             (str "`[Note](" source-path ")`"))})
                       [:checks :citations?])))
    (is (false? (get-in (verdict b "supported" {"/out/wiki/Product.md" (str fresh "\n[Invented](/in/knowledge/sources/inbox/missing.md)")})
                       [:checks :citations?])))
    (let [fake "A quotation not present in frozen evidence."
          stale (-> b
                    (assoc-in [:cases "supported" :gold :sources source-path] fake)
                    (assoc-in [:cases "supported" :gold :quotes] [{:source source-path :text fake}]))]
      (is (false? (get-in (verdict stale "supported" {"/out/wiki/Product.md" (str fresh "\n" fake)})
                         [:checks :quotes?]))))))

(deftest baseline-is-verified-from-selected-fixtures-and-never-gold
  (let [b (bundle)]
    (is (thrown? clojure.lang.ExceptionInfo (verdict b "missing" {})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (verdict (assoc-in b [:fixtures source-path] "tampered") "supported" {})))
    (is (thrown? clojure.lang.ExceptionInfo
                 (verdict (assoc b :fixtures (frozen {"wiki/Company.md" "---\nsimmis-id: bad\n---\n"})) "supported" {})))
    (let [replacement (str/replace original "active: false" "active: true")
          selected (assoc-in b [:cases "supported" :fixtures]
                             (frozen {"wiki/Company.md" replacement "inbox/note.md" source}))]
      (is (false? (get-in (verdict selected "supported" {"/out/wiki/Company.md" changed})
                         [:checks :preserved-structure?]))))))

(deftest host-observation-is-inside-the-sticky-scoring-account
  (let [calls (atom 0)]
    (is (thrown? clojure.lang.ExceptionInfo
                 (checkers/run-checker "(ns checker) (defn check [_] {:checks {:ok true} :reward 1})" {}
                                       (assoc checkers/policy
                                              :allocation-check #(when (> (swap! calls inc) 1) (throw (ex-info "limit" {})))
                                              :prepare-input (fn [guard! input] (guard!) input)))))
    (is (= 2 @calls))))

(deftest verifier-identity-pins-profile-and-selection-without-changing-v1
  (let [b (bundle) ref #(evaluation/evaluator-ref (checkers/evaluator %1 %2))]
    (is (not= (ref b inputs/default-policy) (ref b (assoc inputs/default-policy :exclude-prefixes ["wiki/private"]))))
    (let [old (update-in b [:definition :params] dissoc :simmis/checker-profile)]
      (is (= (ref old nil) (ref old inputs/default-policy))))
    (doseq [invalid [:unknown nil false]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (checkers/evaluator (assoc-in b [:definition :params :simmis/checker-profile] invalid)))))))

(deftest malformed-or-missing-case-gold-does-not-fall-back-to-root-gold
  (let [b (bundle)]
    (doseq [gold [nil false {:version 2 :no-change? true}
                 (assoc (get-in b [:cases "supported" :gold]) :unknown true)]]
      (is (thrown? clojure.lang.ExceptionInfo
                   (verdict (-> b (assoc :gold (get-in b [:cases "supported" :gold]))
                                (assoc-in [:cases "supported" :gold] gold)) "supported" {}))))))

(defn calibrated-bundle []
  (assoc (bundle) :calibration
         {:version 2
          :references {:update {:case "supported" :files {"/out/wiki/Company.md" changed}}
                       :new-page {:case "supported" :files {"/out/wiki/Product.md" fresh}}
                       :no-op {:case "no-change" :files {}}}
          :damaged {:metadata {:case "supported" :files {"/out/wiki/Company.md" (str/replace changed "revenue: 0" "revenue: 1")}
                               :loses [:preserved-structure?]}
                    :claim {:case "supported" :files {"/out/wiki/Product.md" (str fresh "\nfive paying customers")}
                            :loses [:forbidden-content?]}
                    :missing {:case "supported" :files {} :loses [:expected-change? :required-content? :citations?]}}}))

(deftest shipped-default-is-v2-with-explicit-profile-and-unchanged-v1-source
  (let [current (starter/source)
        old (starter/source 1)]
    (is (= 2 starter/version))
    (is (= (set (keys old)) (set (keys current))))
    (is (= :wiki-structure-v1 (get-in (catalog/bundle starter/name current) [:definition :params :simmis/checker-profile])))
    (is (nil? (get-in (catalog/bundle starter/name old) [:definition :params :simmis/checker-profile])))
    (is (not= (:id (catalog/bundle starter/name current)) (:id (catalog/bundle starter/name old))))))

(deftest ordinary-v2-workflows-do-not-execute-checkers-or-template-the-profile
  (let [program-source (starter/source)
        snapshot {:files program-source}
        input {:name starter/name :expected-bundle (:id (catalog/bundle starter/name program-source))
               :inputs (frozen {"wiki/Company.md" original "inbox/note.md" source})}
        prepared (with-redefs [checkers/run-checker (fn [& _] (throw (AssertionError. "Ordinary workflows never grade")))]
                   (runs/prepare-source snapshot input))]
    (is (= {} (get-in prepared [:workflow :params])))
    (is (= :wiki-structure-v1 (get-in prepared [:bundle :definition :params :simmis/checker-profile])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (runs/prepare-source snapshot (assoc input :params {"simmis/checker-profile" "other"}))))))

(deftest bad-selected-baseline-is-refused-before-base-setup
  (let [b (assoc (bundle) :fixtures (frozen {"wiki/Company.md" "---\nsimmis-id: bad\n---\n"}))
        called? (atom false)]
    (with-redefs [catalog/world-setup
                  (fn [_] (evaluation/make-world-setup
                           {:id ::base :version 1 :basis {} :prepare (fn [_] (reset! called? true) {})}))]
      (let [setup (checkers/world-setup b inputs/default-policy)]
        (is (thrown? clojure.lang.ExceptionInfo
                     ((:prepare setup) {:environment {:environment/metadata {:case "supported"}}})))
        (is (false? @called?))))))

(deftest calibration-and-actual-verification-use-identical-structural-scoring
  (let [b (calibrated-bundle)
        report (checkers/calibrate b checkers/calibration-policy inputs/default-policy)
        evaluator (checkers/evaluator b inputs/default-policy)]
    (is (:ok? report))
    (doseq [[k {:keys [case files]}] (get-in b [:calibration :references])]
      (is (every? true? (vals (get-in report [:references k :checks]))))
      (is (= (assoc-in (get-in report [:references k]) [:checks :completed?] true)
             ((:verify evaluator) {:environment/metadata {:case case}} {:files files :run-status :completed}))))
    (doseq [[k {:keys [case files]}] (get-in b [:calibration :damaged])]
      (is (get-in report [:damaged k :ok?]))
      (is (= (assoc-in (get-in report [:damaged k :verdict]) [:checks :completed?] true)
             ((:verify evaluator) {:environment/metadata {:case case}} {:files files :run-status :completed}))))))

(deftest invalid-multi-reference-calibration-is-refused-before-execution
  (let [b (calibrated-bundle)]
    (doseq [bad [(assoc-in b [:calibration :references :update :case] "missing")
                 (assoc-in b [:calibration :references :update :unexpected] true)
                 (assoc-in b [:calibration :damaged :metadata :loses] [:x :x])
                 (assoc-in b [:calibration :references] {})
                 (assoc-in b [:calibration :references :update :files "/out/../escape"] "bad")
                 (assoc-in b [:calibration :references :update :files "/out/big"]
                           (apply str (repeat (inc (:max-bytes checkers/structural-calibration-policy)) "x")))]]
      (with-redefs [checkers/run-checker (fn [& _] (throw (AssertionError. "Validation must be pure")))]
        (is (thrown? clojure.lang.ExceptionInfo
                     (checkers/calibrate bad checkers/calibration-policy inputs/default-policy)))))))

(deftest multi-reference-calibration-shares-one-account-and-actual-answer-count
  (let [b (calibrated-bundle) seen (atom []) account (fn [])]
    (with-redefs [dvergr.sandbox/make-resource-limits
                  (fn [& options] (is (= [:max-bytes (* 6 4096)] (vec options))) account)
                  checkers/run-checker
                  (fn [_ _ limits]
                    (swap! seen conj limits)
                    {:checks {:ok? true} :reward 1})]
      (checkers/calibrate b (assoc checkers/calibration-policy :max-allocated-bytes 4096) inputs/default-policy))
    (is (= 6 (count @seen)))
    (is (every? #(identical? account (:allocation-check %)) @seen))
    (is (apply = (map :deadline-nanos @seen)))
    (is (every? fn? (map :prepare-input @seen)))))

(deftest reusable-development-example-calibrates-the-exact-shipped-program
  (let [root (io/file "examples/workflows/wiki-maintenance-eval-v2")
        source (into {} (for [file (file-seq root) :when (.isFile file)]
                          [(str (.relativize (.toPath root) (.toPath file))) (slurp file)]))
        bundle (catalog/bundle starter/name source)]
    (is (= (starter/source) (select-keys source (keys (starter/source)))))
    (is (:ok (workflows/inspect-snapshot starter/name {:files source})))
    (is (:ok? (checkers/calibrate bundle checkers/calibration-policy (inputs/declaration source))))
    (is (= #{"supported" "no-change"} (set (keys (:cases bundle)))))
    (doseq [case (keys (:cases bundle))]
      (let [prepared (runs/prepare-source {:files source}
                                          {:name starter/name :expected-bundle (:id bundle) :case case})]
        (is (= (if (= case "supported") original changed)
               (get-in prepared [:inputs "/in/knowledge/sources/wiki/Company.md"])))))))
