(ns is.simm.documents.summary-views-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.documents.saved-view-spec :as spec]
            [is.simm.documents.summary-views :as views]
            [is.simm.model.room-summaries-test :as fixtures]))

(def descriptor
  {:version 1 :id (random-uuid) :source {:kind :room-summaries :room :current}
   :presentation :table :columns [{:kind :period} {:kind :summary} {:kind :status}
                                  {:kind :originals} {:kind :model} {:kind :publication}]
   :limit 1})

(deftest summary-source-is-fixed-bounded-and-read-only
  (doseq [bad [(assoc-in descriptor [:source :room] (random-uuid))
               (assoc descriptor :limit 33)
               (assoc descriptor :columns [{:kind :summary :edit true}])]]
    (is (thrown? clojure.lang.ExceptionInfo (spec/validate bad))))
  (let [selection (fixtures/selection 1)
        revision (random-uuid)
        row {:record {:room.summary/revision revision :room.summary/node (random-uuid)
                      :room.summary/model "test-model" :room.summary/state :ready}
             :payload {:selection selection :report (fixtures/report selection)}
             :path "summaries/conversations/day.md" :publication :pending}
        listing {:rows [row] :total 2 :complete? true}
        result (views/prepare descriptor {:room fixtures/room :store fixtures/store} listing)]
    (is (= :room-summaries (get-in result [:source :kind])) )
    (is (= revision (get-in result [:rows 0 :id]) (get-in result [:rows 0 :summary])))
    (is (= "pending" (get-in result [:rows 0 :cells 5 :text])))
    (is (false? (:complete? result)))
    (is (not (contains? (first (:rows result)) :edit)))
    (is (= result (views/prepare descriptor {:room fixtures/room :store fixtures/store} listing)))))
