(ns is.simm.uis.web.desktop.memory-cells
  "Pure, context-owned saved projections. No authorization or native handles."
  (:require [is.simm.documents.code-cells :as cells]
            [is.simm.documents.chart-contract :as contract]
            [is.simm.documents.rich-document :as document]
            [is.simm.uis.web.desktop.memory-charts :as charts]))

(defn marker [draft]
  (let [source (:source draft)]
    [(:repository-id source) (:path source) (:entry source)
     (:revision (charts/saved-analysis draft))]))

(defn ensure-projection [draft]
  (let [analysis (charts/saved-analysis draft) source (:source draft) token (marker draft)]
    (cond
      (not (and (= :markdown (:kind source)) (string? (:text source))
                (= :ready (get-in analysis [:semantic :status])) (<= (count (:slots analysis)) 200)))
      (dissoc draft :saved-cells)
      (= token (get-in draft [:saved-cells :marker])) draft
      :else (assoc draft :saved-cells
                   {:marker token :projection (if (= (:text source) (get-in draft [:rich-base :text]))
                                                (get-in draft [:rich-base :document])
                                                (document/project (:text source) analysis))}))))

(defn visible [state path]
  (let [draft (get-in state [:drafts path]) source (:source draft)
        cache (:saved-cells draft) projection (:projection cache)
        native (or (get-in draft [:rich-base :document-model]) (get-in draft [:rich-base :document :model]))
        clean? (and (= (:text draft) (:text source)) (nil? (:local-source draft))
                    (not (:target-stale? draft)) (not (:remote-deleted? draft))
                    (nil? (:incoming draft)) (nil? (:incoming-entry draft)))
        receipts (vec (filter #(and (= :saved-markdown-cell (get-in % [:source :kind]))
                                    (= (:repository-id source) (get-in % [:source :repository-id]))
                                    (= path (get-in % [:source :path]))
                                    (= (:entry source) (get-in % [:source :entry]))
                                    (= (:target-stamp state) (get-in % [:source :target-stamp])))
                              (get-in state [:repl :history])))
        newest (first receipts)
        coherent? (and projection native (= (:marker cache) (marker draft))
                      (cells/equivalent-projection? (:model projection) native))]
    (when (and clean? coherent? (:target-verified? state)
               (= (:target-stamp state) (:target-stamp source)))
      {:model (:model projection) :cells (:cells projection)
       :namespace (get-in state [:repl :namespace] "user")
       :enabled? (not (false? (get-in state [:sandbox-permissions :evaluate?])))
       :running (vec (keep (fn [[slot loading?]]
                            (when (and loading? (vector? slot) (= :cell (first slot)) (= path (second slot))
                                       (= (:revision (charts/saved-analysis draft)) (nth slot 2 nil)))
                              (nth slot 3))) (:loading state)))
       :presentation-id (when (and (:success newest) (not (:partial? newest)) (:presentation newest)) (:id newest))
       :receipts receipts})))

(defn chart-budget [state path]
  (let [current (when (= :rich (get-in state [:drafts path :mode])) (visible state path))
        receipt (first (:receipts current))]
    (if (and (:presentation-id current) (= :chart (get-in receipt [:presentation :kind])))
      (dec contract/max-mounted)
      contract/max-mounted)))

(defn request [state path key]
  (when-let [cell (first (filter #(= key (:key %)) (:cells (visible state path))))]
    (when (:runnable? cell)
      {:path path :repository-id (get-in state [:drafts path :source :repository-id])
       :expected (get-in state [:drafts path :source :entry])
       :revision (:revision (charts/saved-analysis (get-in state [:drafts path])))
       :from (:from cell) :to (:to cell) :block (some-> (:block-id cell) parse-uuid)
       :namespace (get-in state [:repl :namespace] "user")
       :expected-target-stamp (:target-stamp state)})))

(defn receipt-status [state receipt]
  (let [source (:source receipt) draft (get-in state [:drafts (:path source)])
        current (:source draft)]
    (cond
      (or (not (:target-verified? state)) (nil? current) (:target-stale? draft)) :unverified
      (or (not= (:target-stamp source) (:target-stamp state))
          (not= (:repository-id source) (:repository-id current))
          (:remote-deleted? draft) (:incoming draft) (:incoming-entry draft)
          (not= (:entry source) (:entry current))) :saved-changed
      (not= (:text draft) (:text current)) :draft-changed
      :else :saved-current)))
