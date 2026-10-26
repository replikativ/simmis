(ns is.simm.documents.saved-view-positions-test
  (:require [is.simm.documents.saved-view-positions :as positions]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
            #?(:clj [is.simm.documents.blocks :as blocks])
            #?(:clj [is.simm.documents.markdown :as markdown])
            #?(:clj [is.simm.documents.saved-view-fences :as fences])
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(def id-a #uuid "075aa061-e6e8-44ec-80cc-49de22c149eb")
(def id-b #uuid "2b22d4eb-7450-44ec-a964-45a649062288")
(def fence-a {:kind :FencedCodeBlock :language "simmis-view" :literal "identical text" :from 10 :to 30})
(def fence-b (assoc fence-a :from 40 :to 60))
(def entry-a {:from 10 :to 30 :view id-a :status :ready})
(def entry-b {:from 40 :to 60 :view id-b :status :ready})

(defn slot [node] (assoc (select-keys node [:from :to :kind]) :model (rich/model node)))
(def analysis {:nodes [fence-a fence-b] :saved-views [entry-a entry-b]})

(deftest exact-occurrences-do-not-borrow-identical-code-text
  (let [slots (mapv slot (:nodes analysis))]
    (is (= {[0 0] entry-a [1 0] entry-b} (positions/positions analysis slots)))
    (is (= {[1 0] entry-b}
           (positions/positions analysis (assoc-in slots [0 :from] 11)))))
  (is (= {[1 0] entry-b}
         (positions/positions analysis (assoc-in (mapv slot (:nodes analysis)) [0 :model :content 0 :content 0 :text] "edited")))))

(deftest duplicate-invalid-and-excess-entries-stay-source-only
  (let [slots (mapv slot (:nodes analysis))]
    (is (empty? (positions/positions (assoc-in analysis [:saved-views 1 :view] id-a) slots)))
    (is (= {[1 0] entry-b} (positions/positions (assoc-in analysis [:saved-views 0 :status] :invalid) slots)))
    (is (= {[0 0] entry-a}
           (positions/positions (assoc analysis :saved-views [entry-a {:status :invalid} {:status :invalid} {:status :invalid} entry-b]) slots)))))

(deftest nested-list-correspondence-retains-its-exact-model-path
  (let [paragraph {:kind :Paragraph :from 1 :to 5 :children [{:kind :Text :text "Before"}]}
        list {:kind :BulletList :from 0 :to 35
              :children [{:kind :BulletListItem :from 0 :to 35 :children [paragraph fence-a]}]}
        source {:nodes [list] :saved-views [entry-a]}]
    (is (= {[0 0 0 1] entry-a} (positions/positions source [(slot list)])))
    (is (empty? (positions/positions source [(assoc-in (slot list) [:model :content 0 :content] [])]))))
  (let [opaque {:kind :BlockQuote :from 0 :to 35 :children [fence-a]}]
    (is (empty? (positions/positions {:nodes [opaque] :saved-views [entry-a]} [(slot opaque)])))))

#?(:clj
   (deftest actual-parser-and-fence-analysis-agree-with-read-model-paths
     (let [descriptor {:version 1 :id id-a :source {:kind :pages :roots [""] :types []}
                       :presentation :table :columns [{:kind :title}]}
           source (str "Before\r\n\r\n```simmis-view\r\n" (pr-str descriptor) "\r\n```\r\n\r\nAfter\r\n")
           parsed (markdown/parse source)
           analysis (assoc parsed :saved-views (fences/analyze (:nodes parsed)))
           mapped (positions/positions analysis (mapv slot (:nodes parsed)))
           entry (first (vals mapped))]
       (is (= 1 (count mapped)))
       (is (= [1 0] (first (keys mapped))))
       (is (= id-a (:view entry)))
       (is (= (first (:saved-views analysis)) entry))
       (is (= source (str (subs source 0 (:from entry)) (subs source (:from entry) (:to entry)) (subs source (:to entry)))))
       (is (empty? (:diagnostics parsed))))))


(deftest rich-region-paths-retain-wrappers-and-preceding-model-child-counts
  (let [paragraph {:type "paragraph" :content [{:type "text" :text "Extra"}]}
        slots [(slot fence-a) {:from 31 :to 39 :kind :Paragraph
                              :model {:type "doc" :content [paragraph paragraph]}} (slot fence-b)]
        content (vec (mapcat #(get-in % [:model :content]) slots))
        projection {:model {:type "doc" :content [{:type "memoryBlock" :attrs {:key "parent"}
                                                   :content [{:type "memoryRegion" :attrs {:key "region"} :content content}]}]}
                    :regions {"region" {:slots slots :model {:type "doc" :content content}}}}
        source (assoc analysis :semantic {:status :ready})]
    (is (= {[0 0 0] entry-a [0 0 3] entry-b} (positions/rich-positions source projection)))
    (is (empty? (positions/rich-positions analysis projection)))
    (is (empty? (positions/rich-positions source (assoc-in projection [:model :content 0 :content 0 :content 0 :content 0 :text] "edited"))))))

#?(:clj
   (deftest actual-parser-maps-nested-saved-fences-into-the-continuous-rich-document
     (let [descriptor {:version 1 :id id-a :source {:kind :pages :roots [""] :types []}
                       :presentation :table :columns [{:kind :title}]}
           source (str "Before\n\n- Item\n\n  ```simmis-view\n  " (pr-str descriptor) "\n  ```\n\nAfter\n")
           parsed (markdown/parse source)
           analysis (assoc parsed :semantic (blocks/project id-b parsed)
                                  :saved-views (fences/analyze (:nodes parsed))
                                  :slots (mapv slot (:nodes parsed)))
           projection (document/project source analysis)
           mapped (positions/rich-positions analysis projection)
           [path entry] (first mapped)]
       (is (= 1 (count mapped)))
       (is (= id-a (:view entry)))
       (is (= "codeBlock" (:type (reduce #(get-in %1 [:content %2]) (:model projection) path))))
       (is (= source (document/apply-model source projection (:model projection)))))))
