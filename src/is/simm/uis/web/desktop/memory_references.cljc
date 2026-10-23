(ns is.simm.uis.web.desktop.memory-references
  "Saved block addresses and context-owned navigation intent. No effects."
  (:require [is.simm.uis.web.desktop.memory-charts :as charts]
            [clojure.string :as str]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-links :as links]))

(defn selected-tab [tab path preserve-reference?]
  (-> tab
      (cond-> (not preserve-reference?) (update :data dissoc :reference))
      (assoc :title (last (str/split path #"/")))
      (assoc-in [:data :path] path)
      (cond-> (and preserve-reference? (get-in tab [:data :reference]))
        (assoc-in [:data :reference :path-hint] path))))

(defn explicit-blocks [draft]
  (let [analysis (charts/saved-analysis draft)]
    (when (and (= :ready (get-in analysis [:semantic :status]))
               (empty? (:diagnostics analysis)))
      (filter #(= :explicit (:identity-status %))
              (mapcat #(tree-seq (comp seq :children) :children %) (get-in analysis [:semantic :blocks]))))))

(defn block-links [room path draft]
  (let [repository (get-in draft [:source :repository-id])]
    (when (uuid? repository)
      (into {} (for [block (explicit-blocks draft)
                     :let [route (links/route {:kind :memory-block :room (str room) :repository (str repository)
                                              :block (str (:id block)) :path path})]
                     :when route]
                 [(str (:id block)) route])))))

(defn marker [state intent]
  [(:connection state) (:target-stamp state) (:target-epoch state) intent])

(defn completed? [state intent]
  (let [reference (:block-reference state)]
    (and (:applied? reference) (= intent (:intent reference))
         (= (:target-stamp state) (second (:request-marker reference))))))

(defn jump [state path]
  (let [{:keys [intent reply id applied? request-marker]} (:block-reference state)
        draft (get-in state [:drafts path])
        block (first (filter #(= (:block-id intent) (:id %)) (explicit-blocks draft)))]
    (when (and id (not applied?) (:target-verified? state)
               (= request-marker (marker state intent)) (= :resolved (:status reply))
               (= path (:selected state) (:path reply))
               (= (:repository-id intent) (get-in draft [:source :repository-id]))
               (= (:target-stamp state) (get-in draft [:source :target-stamp]))
               (= (:entry reply) (get-in draft [:source :entry]))
               (not (:target-stale? draft)) (not (:remote-deleted? draft))
               (not (:incoming draft)) (not (:incoming-entry draft))
               (not (editor/dirty? draft)) block)
      {:id (str id) :block-id (str (:id block)) :from (:from block) :to (:to block)})))
