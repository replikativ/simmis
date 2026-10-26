(ns is.simm.documents.scorecard-views-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.saved-view-spec-test :as declarations]
            [is.simm.documents.saved-view-values :as values]
            [is.simm.documents.scorecard-views :as views]))

(def descriptor
  (assoc declarations/scorecard-table
         :columns [{:kind :candidate} {:kind :pass-rate} {:kind :pass-rate-interval}
                   {:kind :cost} {:kind :notional-cost} {:kind :cost-known}]))
(def context {:room #uuid "cf455d8f-2cf9-4948-8043-628a59f85415"
              :store #uuid "cfb81aac-7988-49da-8d8a-482da501341a"})
(def experiment-id #uuid "9490d218-621e-42d5-a80c-48fe13b16021")
(def definition
  {:experiment/content-id experiment-id :experiment/candidates [{:candidate/id :work/luna}]
   :experiment/repetitions 1
   :experiment/dataset {:dataset/content-id #uuid "dcb093f9-9a03-4b37-8b93-2d9053a07f6b"
                        :dataset/environments [{}]}
   :experiment/metadata {:private "__host_private_credentials__"
                         :simmis/workflow {:name "research" :bundle (random-uuid)
                                          :title "__host_private_credentials__"}}})
(def candidate
  {:candidate "work/luna" :attempts 1 :passed 1 :pass-rate 1.0 :pass-rate-interval [0.1 0.9]
   :reward-mean 1.0 :reward-interval nil
   :spend {:microdollars 123456 :notional-microdollars 400000
           :priced? true :notional-priced? true :usage-reported? true
           :by-model {"__host_private_credentials__" {}}}
   :microdollars-per-pass 123456 :notional-microdollars-per-pass 400000
   :receipt "__host_private_credentials__"})
(def summary
  {:id (str (get-in descriptor [:source :scorecard]))
   :experiment {:content-id (str experiment-id) :environments 1 :repetitions 1}
   :cells 1 :leaderboard [candidate] :private "__host_private_credentials__"})

(deftest projection-is-readonly-exactly-identified-and-uses-native-values
  (let [result (views/prepare descriptor context summary definition)
        row (first (:rows result)) cells (:cells row)]
    (is (= :ready (:status result)))
    (is (:matrix-complete? result))
    (is (:complete? result))
    (is (= (assoc context :kind :scorecard :scorecard (get-in descriptor [:source :scorecard])) (:source result)))
    (is (= [(get-in descriptor [:source :scorecard]) "work/luna"] (:id row)))
    (is (not (contains? row :path)))
    (is (not (contains? row :edit)))
    (is (= ["work/luna" "100%" "10–90%" "$0.123456" "$0.4" "Yes"] (mapv values/scalar-text cells)))
    (is (= {:priced? true :notional-priced? true :usage-reported? true} (:pricing row)))
    (is (not (str/includes? (pr-str result) "__host_private_credentials__")))
    (is (= result (views/prepare descriptor context summary definition)))))

(deftest unknown-cost-is-not-a-zero-and-does-not-erase-known-list-price
  (let [input (-> summary (assoc-in [:leaderboard 0 :spend :priced?] false)
                         (assoc-in [:leaderboard 0 :spend :microdollars] 0))
        result (views/prepare descriptor context input definition)
        row (first (:rows result))]
    (is (= :null (get-in row [:cells 3 :kind])))
    (is (= "$0.4" (values/scalar-text (get-in row [:cells 4]))))
    (is (false? (get-in row [:pricing :priced?])))))

(deftest displayed-prefix-is-distinct-from-the-certified-matrix
  (let [definition (assoc definition :experiment/candidates [{:candidate/id :work/luna} {:candidate/id :work/other}])
        summary (-> summary (assoc :cells 2)
                            (update :leaderboard conj (assoc candidate :candidate "work/other")))
        result (views/prepare (assoc descriptor :limit 1) context summary definition)]
    (is (= 2 (:total result)))
    (is (= 1 (count (:rows result))))
    (is (:matrix-complete? result))
    (is (false? (:complete? result)))
    (is (false? (get-in result [:display :complete?])))))

(deftest native-fractional-notional-costs-keep-their-value-and-bound-decimal-expansion
  (doseq [[amount expected] [[123456.5M "$0.1234565"] [1/3 "≈$0.000000333333"]]]
    (let [input (assoc-in summary [:leaderboard 0 :spend :notional-microdollars] amount)
          result (views/prepare descriptor context input definition)]
      (is (= expected (values/scalar-text (get-in result [:rows 0 :cells 4]))))))
  (doseq [amount [1E-1000000M -1 Double/POSITIVE_INFINITY]]
    (is (thrown? clojure.lang.ExceptionInfo
                 (views/prepare descriptor context
                                (assoc-in summary [:leaderboard 0 :spend :notional-microdollars] amount)
                                definition)))))

(deftest wrong-identities-unsafe-values-and-extra-context-are-refused
  (doseq [[ctx input] [[(assoc context :conn :native-handle) summary]
                        [context (assoc summary :id (str (random-uuid)))]
                        [context (assoc-in summary [:leaderboard 0 :candidate] (apply str (repeat 257 "x")))]
                        [context (assoc-in summary [:leaderboard 0 :pass-rate] Double/NaN)]
                        [context (assoc-in summary [:leaderboard 0 :spend :priced?] "private")]]]
    (is (thrown? clojure.lang.ExceptionInfo (views/prepare descriptor ctx input definition)))))

(deftest oversized-presentation-is-held-instead-of-partially-changing-statistics
  (with-redefs [views/max-presentation-bytes 100]
    (let [result (views/prepare descriptor context summary definition)]
      (is (= :unavailable (:status result)))
      (is (= :display-limit (:reason result)))
      (is (nil? (:rows result))))))
