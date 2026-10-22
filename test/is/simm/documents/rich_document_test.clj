(ns is.simm.documents.rich-document-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [is.simm.documents.markdown :as markdown]
            [is.simm.documents.blocks :as blocks]
            [is.simm.documents.rich-text :as rich]
            [is.simm.documents.rich-document :as document]
            [is.simm.documents.rich :as host]
            [is.simm.uis.web.desktop.memory-editor :as editor]))

(def repo #uuid "00000000-0000-4000-8000-000000000001")
(def parent-id "00000000-0000-4000-8000-000000000002")
(def child-id "00000000-0000-4000-8000-000000000003")
(defn paragraph [s] {:type "paragraph" :content [{:type "text" :text s}]})
(defn analysis [source]
  (let [parsed (markdown/parse source)]
    (assoc parsed :semantic (blocks/project repo parsed)
           :slots (mapv #(assoc (select-keys % [:from :to :kind]) :model (rich/model %)) (:nodes parsed)))))
(defn nodes [model] (tree-seq #(seq (:content %)) :content model))
(defn change-region [model key f]
  (letfn [(walk [n]
            (if (= key (get-in n [:attrs :key])) (f n)
                (cond-> n (:content n) (update :content #(mapv walk %)))))] (walk model)))
(defn check-host [source projection model]
  (let [result (document/apply-model source projection model)]
    (is (= result (host/apply-edits repo "wiki/note.md" source (markdown/revision source)
                                  (document/edits projection model))))
    result))

(deftest semantic-containers-retain-nesting-and-source-only-content
  (let [source (str "---\r\ncustom: retained\r\n---\r\n\r\n"
                    "<!-- simmis:block " parent-id " -->\r\n\r\n"
                    "First\r\n\r\nSecond\r\n\r\n"
                    "<!-- simmis:block " child-id " -->\r\n\r\n"
                    "[[Opaque]]\r\n\r\nChild\r\n\r\n<!-- /simmis:block -->\r\n\r\n"
                    "Last\r\n\r\n<!-- /simmis:block -->\r\n")
        projection (document/project source (analysis source)) model (:model projection)
        containers (filter #(= "memoryBlock" (:type %)) (nodes model))
        key (get-in (first (:content (first containers))) [:attrs :key])
        edited (change-region model key #(assoc-in % [:content 0 :content 0 :text] "Changed"))]
    (is (= [parent-id child-id] (mapv #(get-in % [:attrs :blockId]) containers)))
    (is (= 3 (count (:regions projection))))
    (is (= [] (document/edits projection model)))
    (is (= source (document/apply-model source projection model)))
    (is (= (str/replace source "First" "Changed") (check-host source projection edited)))
    (is (= (document/topology model) (document/topology edited)))
    (doseq [bad [(assoc model :content [])
                 (assoc-in model [:content 0 :attrs :blockId] "forged")
                 (update-in model [:content 0 :content] #(vec (reverse %)))]]
      (is (thrown? clojure.lang.ExceptionInfo (document/edits projection bad))))))

(deftest split-join-and-insertion-within-a-semantic-block-retain-its-id
  (let [source (str "<!-- simmis:block " parent-id " -->\n\nAlpha\n\nBeta\n\n<!-- /simmis:block -->\n")
        projection (document/project source (analysis source)) model (:model projection)
        key (first (keys (:regions projection)))]
    (doseq [content [[(paragraph "Al") (paragraph "pha") (paragraph "Beta")]
                     [(paragraph "AlphaBeta")]
                     [(paragraph "Before") (paragraph "Alpha") (paragraph "Beta")]
                     [(paragraph "Alpha") (paragraph "Middle") (paragraph "Beta")]
                     [(paragraph "Alpha") (paragraph "Beta") (paragraph "After")]]]
      (let [edited (change-region model key #(assoc % :content content))
            result (check-host source projection edited)
            projected (blocks/project repo (markdown/parse result))]
        (is (= :ready (:status projected)))
        (is (= parent-id (str (get-in projected [:blocks 0 :id]))))
        (is (= 1 (count (:blocks projected))))))))

(deftest untouched-siblings-keep-their-original-spelling
  (let [source "First\r\n\r\n__Unusual bold spelling__\r\n\r\nLast\r\n"
        projection (document/project source (analysis source)) model (:model projection)
        key (first (keys (:regions projection)))
        edited (change-region model key #(assoc-in % [:content 0 :content 0 :text] "Revised"))]
    (is (= (str/replace source "First" "Revised") (check-host source projection edited))))
  (doseq [content [[(paragraph "Revised") {:type "paragraph" :content [{:type "text" :text "Middle" :marks [{:type "bold"}]}]}
                    (paragraph "Final")]
                   [(paragraph "Split") (paragraph "First")
                    {:type "paragraph" :content [{:type "text" :text "Middle" :marks [{:type "bold"}]}]}
                    (paragraph "Final")]]]
    (let [source "First\n\n__Middle__\n\nLast\n"
          projection (document/project source (analysis source)) key (first (keys (:regions projection)))
          edited (change-region (:model projection) key #(assoc % :content content))
          result (check-host source projection edited)]
      (is (str/includes? result "__Middle__"))
      (is (= (rich/normalize {:type "doc" :content content})
             (rich/normalize {:type "doc" :content (vec (mapcat #(get-in % [:model :content]) (:slots (analysis result))))}))))))

(deftest host-refuses-crossing-markers-and-opaque-syntax-even-with-forged-ranges
  (doseq [source [(str "Alpha\n\n<!-- simmis:block " parent-id " -->\n\nBeta\n\n<!-- /simmis:block -->\n")
                  "Alpha\n\n[[Opaque]]\n\nBeta\n"]]
    (let [parsed (markdown/parse source)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (host/apply-edits repo "wiki/note.md" source (:revision parsed)
                                     [{:from (:from (first (:nodes parsed))) :to (:to (last (:nodes parsed)))
                                       :model {:type "doc" :content [(paragraph "Overwritten")]}}]))))))

(deftest document-drafts-retain-context-owned-model-and-stale-base-protection
  (let [source "Alpha\n\nBeta\n" a (analysis source)
        d (editor/draft {:kind :markdown :text source :analysis a :entry :opened})
        base (:rich-base d) model (get-in base [:document :model])
        key (first (keys (get-in base [:document :regions])))
        edited (change-region model key #(assoc % :content [(paragraph "AlphaBeta")]))
        next (editor/document-changed d (:id base) edited {:from 3 :to 3})]
    (is (= "AlphaBeta\n" (:text next)))
    (is (= source (:text d)))
    (is (= edited (get-in next [:rich-base :document-model])))
    (is (= next (editor/document-changed next :stale model nil)))
    (is (= (:text next) (host/apply-edits repo "wiki/note.md" source (:revision a) (editor/document-edits next))))))

(deftest empty-files-and-empty-identified-blocks-can-be-edited-without-inventing-an-id
  (doseq [source ["" "---\ncustom: yes\n---\n"
                  (str "<!-- simmis:block " parent-id " -->\r\n\r\n<!-- /simmis:block -->\r\n")]]
    (let [projection (document/project source (analysis source))
          model (:model projection) key (first (keys (:regions projection)))
          edited (change-region model key #(assoc % :content [(paragraph "New content")]))
          result (check-host source projection edited)]
      (is (= source (document/apply-model source projection model)))
      (is (str/includes? result "New content"))
      (is (= :ready (:status (blocks/project repo (markdown/parse result)))))))
  (is (thrown? clojure.lang.ExceptionInfo
               (host/apply-edits repo "wiki/note.md" "Alpha\n" (markdown/revision "Alpha\n")
                                 [{:from 2 :to 2 :model {:type "doc" :content [(paragraph "Forged")]}}]))))
