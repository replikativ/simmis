(ns is.simm.ops.benchmark-reports-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.agent.experiment :as experiment]
            [geschichte.repo :as repo]
            [is.simm.documents.files :as documents]
            [is.simm.documents.markdown :as markdown]
            [is.simm.ops.benchmark-reports :as reports]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.owned-charts-test :as fixture]
            [is.simm.ops.owned-files :as files]
            [is.simm.ops.owned-saved-views-test :as receipts]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [org.replikativ.spindel.engine.core :as ec]))

(defn- with-fixture [f]
  (@#'fixture/with-fixture
   (fn [{:keys [conn w room] :as context}]
     (let [id (@#'receipts/seed-scorecard! room)
           scope (binding [ec/*execution-context* (:ctx room)]
                   (get-in @(:conn (:store room)) [:config :store :id]))]
       (d/transact conn [{:room/id (:room w) :room/content-db-scope scope}])
       (f (assoc context :id id :handle (str (:room w)) :principal (ops/party-principal (:member w))))))))

(defn- request [prepared]
  {:scorecard (get-in prepared [:source :scorecard])
   :expected-target-stamp (:target-stamp prepared) :expected-plan (:plan prepared)})

(deftest explicit-publication-is-read-only-until-published-and-reconciles-exact-bytes
  (with-fixture
    (fn [{:keys [host principal handle id owned attached]}]
      (let [before @owned other @attached
            prepared (reports/prepare! host principal handle {:scorecard id})
            sent (request prepared)]
        (is (= before @owned) "Preparation does not publish")
        (is (= prepared (reports/prepare! host principal handle {:scorecard id})))
        (is (not-any? #(contains? prepared %) [:db :room :conn :system :lease :control-db]))
        (let [published (reports/publish! host principal handle sent)
              after @owned retry (reports/publish! host principal handle sent)]
          (is (:complete? published))
          (is (= [:created :created] (mapv :status [(:report published) (:index published)])))
          (is (= [:existing :existing] (mapv :status [(:report retry) (:index retry)])))
          (is (= after @owned) "Acknowledgement recovery does not write another revision")
          (is (= (get-in prepared [:report :text])
                 (:text (documents/read-file-in @owned (get-in prepared [:report :path]))))))
        (is (= other @attached) "The selected attachment is never a fallback target")
        (repo/write! owned (get-in prepared [:report :path]) (markdown/source-bytes "# Human correction\n"))
        (is (thrown? clojure.lang.ExceptionInfo (reports/publish! host principal handle sent)))
        (is (= "# Human correction\n"
               (:text (documents/read-file-in @owned (get-in prepared [:report :path])))))))))

(deftest authored-index-is-preserved-and-partial-publication-can-be-recovered
  (with-fixture
    (fn [{:keys [host principal handle id owned]}]
      (let [prepared (reports/prepare! host principal handle {:scorecard id})
            sent (request prepared) save! files/save!]
        (with-redefs [files/save! (fn [& args]
                                   (if (= "reports/index.md" (:path (last args)))
                                     (throw (ex-info "Uncertain second write" {}))
                                     (apply save! args)))]
          (is (thrown? clojure.lang.ExceptionInfo (reports/publish! host principal handle sent))))
        (is (= (get-in prepared [:report :text])
               (:text (documents/read-file-in @owned (get-in prepared [:report :path])))))
        (repo/write! owned "reports/index.md" (markdown/source-bytes "# My report index\n"))
        (let [before @owned result (reports/publish! host principal handle sent)]
          (is (false? (:complete? result)))
          (is (= :existing (get-in result [:report :status])))
          (is (= :preserved (get-in result [:index :status])))
          (is (= before @owned))
          (is (= "# My report index\n" (:text (documents/read-file-in @owned "reports/index.md")))))))))

(deftest malformed-requests-revocation-and-changed-target-refuse-before-receipt-bodies
  (with-fixture
    (fn [{:keys [host principal handle id owned conn w]}]
      (let [prepared (reports/prepare! host principal handle {:scorecard id}) sent (request prepared)
            before @owned]
        (with-redefs [experiment/scorecard-bounded
                      (fn [& _] (throw (AssertionError. "Rejected request reached native receipt body")))]
          (doseq [invalid [(assoc sent :extra :argument) (dissoc sent :expected-plan)
                           (assoc sent :scorecard "not-a-native-id")
                           (assoc sent :expected-target-stamp (random-uuid))]]
            (is (thrown? clojure.lang.ExceptionInfo (reports/publish! host principal handle invalid))))
          (is (thrown? clojure.lang.ExceptionInfo
                       (reports/prepare! host (ops/party-principal (:non-member w)) handle {:scorecard id})))
          (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
          (is (thrown? clojure.lang.ExceptionInfo (reports/publish! host principal handle sent))))
        (is (= before @owned))))))

(deftest caller-digest-and-absent-destination-precondition-prevent-replacement
  (with-fixture
    (fn [{:keys [host principal handle id owned]}]
      (let [prepared (reports/prepare! host principal handle {:scorecard id}) sent (request prepared)
            before @owned save! files/save! path (get-in prepared [:report :path])]
        (is (thrown? clojure.lang.ExceptionInfo
                     (reports/publish! host principal handle (assoc sent :expected-plan (random-uuid)))))
        (is (= before @owned))
        (with-redefs [files/save! (fn [& args]
                                   (repo/write! owned path (markdown/source-bytes "# Concurrent author\n"))
                                   (apply save! args))]
          (is (thrown? clojure.lang.ExceptionInfo (reports/publish! host principal handle sent))))
        (is (= "# Concurrent author\n" (:text (documents/read-file-in @owned path))))))))

(deftest lost-acknowledgement-reconciles-the-original-committed-report
  (with-fixture
    (fn [{:keys [host principal handle id owned]}]
      (let [prepared (reports/prepare! host principal handle {:scorecard id}) sent (request prepared)
            path (get-in prepared [:report :path]) save! files/save! writes (atom [])]
        (with-redefs [files/save! (fn [& args]
                                   (let [destination (:path (last args)) result (apply save! args)]
                                     (swap! writes conj destination)
                                     (when (= path destination) (throw (ex-info "Committed; acknowledgement lost" {})))
                                     result))]
          (is (thrown? clojure.lang.ExceptionInfo (reports/publish! host principal handle sent)))
          (is (= (get-in prepared [:report :text]) (:text (documents/read-file-in @owned path))))
          (let [retry (reports/publish! host principal handle sent)]
            (is (:complete? retry))
            (is (= :existing (get-in retry [:report :status])))
            (is (= [path "reports/index.md"] @writes))))))))

(deftest app-fence-during-a-native-read-prevents-new-publication
  (with-fixture
    (fn [{:keys [host principal handle id owner owned]}]
      (let [prepared (reports/prepare! host principal handle {:scorecard id}) before @owned
            load! experiment/scorecard-bounded saves (atom 0)]
        (with-redefs [experiment/scorecard-bounded
                      (fn [& args]
                        (let [result (apply load! args)]
                          ;; The admitted caller blocks while another thread
                          ;; fences its App; a helper may swallow interruption.
                          (is (nil? (deref (future (lifetime/fence! owner)) 5000 ::timeout)))
                          (.interrupt (Thread/currentThread))
                          (Thread/interrupted)
                          result))
                      files/save! (fn [& _] (swap! saves inc))]
          (is (thrown? clojure.lang.ExceptionInfo (reports/publish! host principal handle (request prepared)))))
        (is (zero? @saves))
        (is (= before @owned))))))

(deftest receipt-store-remapping-during-read-prevents-publication
  (with-fixture
    (fn [{:keys [host principal handle id conn w owned]}]
      (let [prepared (reports/prepare! host principal handle {:scorecard id}) before @owned
            load! experiment/scorecard-bounded saves (atom 0)]
        (with-redefs [experiment/scorecard-bounded
                      (fn [& args]
                        (let [result (apply load! args)]
                          (d/transact conn [{:room/id (:room w) :room/content-db-scope (random-uuid)}])
                          result))
                      files/save! (fn [& _] (swap! saves inc))]
          (is (thrown? clojure.lang.ExceptionInfo (reports/publish! host principal handle (request prepared)))))
        (is (zero? @saves))
        (is (= before @owned))))))
