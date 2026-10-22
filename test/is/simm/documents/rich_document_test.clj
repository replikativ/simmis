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

(deftest local-source-follows-shifted-rich-spans-and-protects-identities
  (let [source (str "<!-- simmis:block " parent-id " -->\n\nAlpha\n\n<!-- /simmis:block -->\n\n"
                    "<!-- simmis:block " child-id " -->\n\nBeta\n\n<!-- /simmis:block -->\n")
        d (editor/draft {:kind :markdown :text source :analysis (analysis source) :entry :opened})
        base (:rich-base d) projection (:document base)
        regions (sort-by :from (vals (:regions projection)))
        first-key (get-in (:model projection) [:content 0 :content 0 :attrs :key])
        second-key (get-in (:model projection) [:content 1 :content 0 :attrs :key])
        model (change-region (:model projection) first-key #(assoc % :content [(paragraph "Longer first paragraph")]))
        typed (editor/document-changed d (:id base) model nil)
        opened (editor/open-local-source typed second-key)
        local (:local-source opened)]
    (is (= "Beta\n" (:text local)))
    (is (= (str/index-of (:text typed) "Beta") (:from local)))
    (is (not (str/includes? (:text local) "simmis:block")))
    (let [edited (assoc-in opened [:local-source :text] "New **content**\n")
          candidate (editor/local-source-candidate edited)
          applied (editor/local-source-applied edited (:local-source edited) candidate (analysis candidate))]
      (is (= (str/replace (:text typed) "Beta" "New **content**") (:text applied)))
      (is (nil? (:local-source applied)))
      (is (some? (get-in applied [:rich-base :document])))
      (is (= [parent-id child-id] (mapv #(str (:id %)) (get-in applied [:analysis :semantic :blocks])))))
    (let [edited (assoc-in opened [:local-source :text] "<!-- /simmis:block -->\n")
          candidate (editor/local-source-candidate edited)]
      (is (thrown? clojure.lang.ExceptionInfo
                   (editor/local-source-applied edited (:local-source edited) candidate (analysis candidate)))))
    (is (thrown? clojure.lang.ExceptionInfo (editor/local-source-candidate (assoc opened :text "Concurrent source edit"))))))

(deftest local-source-buffers-retain-their-state-across-incoming-file-observations
  (let [source "Alpha\n" file {:kind :markdown :text source :analysis (analysis source) :entry :opened}
        d (editor/draft file) key (first (keys (get-in d [:rich-base :document :regions])))
        opened (editor/open-local-source d key)
        edited (assoc-in opened [:local-source :text] "Local draft\n")
        incoming (assoc file :text "External version\n" :entry :new)
        observed (editor/loaded edited incoming)]
    (is (not (editor/dirty? opened)))
    (is (editor/dirty? edited))
    (is (= source (:text observed)))
    (is (= "Local draft\n" (get-in observed [:local-source :text])))
    (is (= incoming (:incoming observed)))
    (is (= (:local-source opened) (:local-source (editor/loaded opened file))))))

(deftest live-source-reprojection-keeps-editor-identity-and-rebases-exact-span
  (let [source (str "<!-- simmis:block " parent-id " -->\n\nAlpha\n\n<!-- /simmis:block -->\n")
        d (editor/draft {:kind :markdown :text source :analysis (analysis source) :entry :opened})
        key (first (keys (get-in d [:rich-base :document :regions])))
        opened (editor/open-local-source d key)
        edited (assoc-in opened [:local-source :text] "Longer **text**\n")
        candidate (editor/local-source-candidate edited)
        projected (editor/local-source-projected edited (:local-source edited) candidate (analysis candidate))
        next-edit (assoc-in projected [:local-source :text] "Second edit\n")
        next-source (editor/local-source-candidate next-edit)]
    (is (= (get-in opened [:local-source :id]) (get-in projected [:local-source :id])))
    (is (= candidate (get-in projected [:local-source :basis-text])))
    (is (= candidate (:text projected)))
    (is (= (str/replace source "Alpha" "Second edit") next-source))
    (is (= parent-id (str (get-in projected [:analysis :semantic :blocks 0 :id]))))))
