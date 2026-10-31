(ns is.simm.model.room-summaries-test
  (:require [clojure.test :refer [deftest is testing]]
            [hasch.core :as hasch]
            [is.simm.model.conversation-digest :as digest]
            [is.simm.model.room-summaries :as summaries]))

(def room #uuid "6d119f42-d0b7-4c67-a452-9da4088ac087")
(def store #uuid "cbe41021-e1eb-43da-b616-9586db7141d5")
(def period {:kind :day :timezone "Europe/Berlin"
             :from #inst "2026-10-29T23:00:00Z" :until #inst "2026-10-30T23:00:00Z"})

(defn source [n]
  {:source-id (hasch/uuid [:message n]) :source-revision (hasch/uuid [:revision n])
   :file-content (hasch/uuid [:bytes n]) :path (str "originals/" n ".md")
   :kind :conversation :authority :human})

(defn selection
  ([n] (selection n []))
  ([n context]
   (summaries/selection {:room room :store store :period period
                         :cutoff #inst "2026-10-31T00:00:00Z"
                         :inventory :current-revisions :complete? true
                         :coverage (mapv source (range n)) :context context})))

(defn report [selected]
  {:version 2 :selection-id (:id selected)
   :summary {:text "A proposal was revised; agreement remains unconfirmed."
             :sources [(-> selected :coverage first :source-revision)]}
   :sections (zipmap digest/section-keys (repeat []))
   :coverage (mapv #(hash-map :source-revision (:source-revision %) :status :selected
                             :note "Read the exact original; preserved its uncertainty.")
                   (:coverage selected))})

(deftest busy-periods-use-a-file-manifest-without-broadening-manual-v1
  (let [selected (selection 64)]
    (is (> (count (pr-str selected)) digest/max-selection-characters))
    (is (= 64 (count (:coverage (summaries/validate-selection! selected)))))
    (is (summaries/ready? selected (report selected)))
    (is (thrown? clojure.lang.ExceptionInfo (selection 65)))
    (is (= 8 digest/max-sources) "Historical manual digest limits stay unchanged")))

(deftest context-is-frozen-separately-and-cannot-acknowledge-originals
  (let [context (assoc (source 90) :kind :summary :through (:from period))
        selected (selection 2 [context])
        value (assoc-in (report selected) [:sections :open-work]
                        [{:text "The earlier unresolved question remains context."
                          :sources [(:source-revision context)]}])]
    (is (= value (summaries/validate-report! selected value)))
    (is (thrown? clojure.lang.ExceptionInfo
                 (summaries/validate-report!
                  selected (update value :coverage conj {:source-revision (:source-revision context)
                                                        :status :selected :note "Old context is not new activity."}))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (selection 2 [(assoc context :through (:until period))])))
    (is (thrown? clojure.lang.ExceptionInfo
                 (selection 2 [(dissoc context :through)])))))

(deftest every-new-original-needs-one-disposition-and-valid-citations
  (let [selected (selection 2) value (report selected)]
    (doseq [[label damaged]
            [["missing original" (update value :coverage pop)]
             ["duplicate original" (update value :coverage #(conj % (first %)))]
             ["invented source" (assoc-in value [:summary :sources] [(random-uuid)])]
             ["uncited summary" (assoc-in value [:summary :sources] [])]
             ["HTML" (assoc-in value [:summary :text] "<script>bad()</script>")]
             ["changed basis" (assoc value :selection-id (random-uuid))]]]
      (testing label
        (is (thrown? clojure.lang.ExceptionInfo (summaries/validate-report! selected damaged)))))))

(deftest held-and-idle-periods-never-look-complete
  (let [selected (selection 1) value (report selected)
        partial (summaries/selection (assoc selected :complete? false))]
    (is (false? (summaries/ready? partial (assoc value :selection-id (:id partial)))))
    (is (false? (summaries/ready? selected (-> value (assoc-in [:coverage 0 :status] :deferred)
                                            (assoc-in [:summary :sources] [])))))
    (let [context (assoc (source 90) :kind :summary :through (:from period))
          idle (selection 0 [context])
          value {:version 2 :selection-id (:id idle)
                 :summary {:text "Earlier context alone creates no activity."
                           :sources [(:source-revision context)]}
                 :coverage [] :sections (zipmap digest/section-keys (repeat []))}]
      (is (false? (summaries/ready? idle value))))))

(deftest logical-node-stays-stable-while-source-and-execution-revisions-change
  (let [a (selection 1) b (selection 2)
        program {:bundle (random-uuid) :model "local-test" :request (random-uuid) :attempt (random-uuid)}]
    (is (= (summaries/node-id a) (summaries/node-id b)))
    (is (not= (:id a) (:id b)))
    (is (not= (summaries/revision-id a (report a) program)
              (summaries/revision-id a (report a) (assoc program :model "another-model"))))
    (is (not= (summaries/node-id a)
              (summaries/node-id (assoc a :room (random-uuid)))))
    (is (thrown? clojure.lang.ExceptionInfo
                 (summaries/selection (assoc a :cutoff (:from period)))))))
