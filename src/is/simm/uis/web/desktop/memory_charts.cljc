(ns is.simm.uis.web.desktop.memory-charts
  "Pure saved-chart selection and response fences, held by the owning Memory
   execution context. No requests, global state, native handles or authority."
  (:require [is.simm.documents.chart-contract :as contract]))

(defn saved-analysis
  "Use only analysis bound to the CURRENT saved content identity. Markdown's
   source revision and Geschichte's content ID both hash the same UTF-8 bytes.
   Autosave can advance entry/text before an optional analysis reply arrives."
  [draft]
  (let [source (:source draft) id (get-in source [:entry :content])]
    (when (uuid? id)
      (cond
        (= id (get-in source [:analysis :revision])) (:analysis source)
        (and (string? (:text source)) (= (:text source) (:analysis-text draft))
             (= id (get-in draft [:analysis :revision]))) (:analysis draft)))))

(defn analysis-marker [state path]
  [(:connection state) (:target-stamp state)
   (get-in state [:drafts path :source :entry]) (get-in state [:drafts path :generation])])

(defn blocks [draft]
  (let [source (:source draft)
        analysis (saved-analysis draft)
        declarations (get-in analysis [:frontmatter :properties "simmis" "blocks"])]
    (when (and (:entry source) (= :markdown (:kind source))
               (= :explicit (:identity-status analysis)) (empty? (:diagnostics analysis)))
      (vec (take contract/max-mounted
                 (for [block (mapcat #(tree-seq (comp seq :children) :children %)
                                      (get-in analysis [:semantic :blocks]))
                       :when (and (= :explicit (:identity-status block))
                                  (some #{(str contract/type-id)} (get-in declarations [(str (:id block)) "types"])))]
                   (:id block)))))))

(defn marker [state path block]
  [(:connection state) (:target-stamp state) (:read-epoch state) (:definition-basis state)
   (get-in state [:drafts path :generation])
   (get-in state [:drafts path :source :entry])
   (:revision (saved-analysis (get-in state [:drafts path]))) block (:stream-marker state)])

(defn request [state path block]
  {:path path :block block :expected (get-in state [:drafts path :source :entry])
   :revision (:revision (saved-analysis (get-in state [:drafts path])))
   :expected-target-stamp (:target-stamp state)})

(defn reply-current? [state path block sent reply]
  (and (= sent (marker state path block))
       (= :ready (:status reply)) (= path (:path reply)) (= block (:block reply))
       (= (:target-stamp state) (:target-stamp reply))
       (= (get-in state [:drafts path :source :entry]) (:expected reply))
       (= (:revision (saved-analysis (get-in state [:drafts path]))) (:revision reply))
       (= contract/profile (get-in reply [:feature :profile]))
       (uuid? (get-in reply [:feature :digest])) (uuid? (:scope-revision reply))
       (string? (:spec-path reply)) (map? (:spec-entry reply))
       (= contract/data-profile (get-in reply [:chart :profile]))))

(defn visible [state path]
  (let [draft (get-in state [:drafts path])]
    (when (and (:target-verified? state) (not (:target-stale? draft))
               (= (:target-stamp state) (get-in draft [:source :target-stamp]))
               ;; Source editing may change declarations/identity; never attach
               ;; an old saved chart to a newly authored unsaved declaration.
               (not= :source (:mode draft)) (nil? (:local-source draft)))
      (into {} (for [block (blocks draft)
                     :let [{:keys [reply marker]} (get-in state [:charts [path block]])]
                     ;; A metadata-interest refresh with the SAME definition
                     ;; basis is not a source change. Keep accepted data visible
                     ;; while the new read is admitted; reply acceptance remains
                     ;; strict. A real source/definition change still hides it.
                     ;; Ordinary continuous prose edits cannot change saved
                     ;; declarations. Keep the existing projection during typing;
                     ;; new async replies still use the strict generation fence.
                     :when (and (vector? marker)
                                (reply-current? state path block
                                                (assoc marker 2 (:read-epoch state) 4 (:generation draft)) reply))]
                 [(str block) (:chart reply)])))))
