(ns is.simm.uis.web.desktop.retained-source
  "Pure progress for one exact retained-source read, owned by its tab."
  (:require [is.simm.uis.web.desktop.captured-result :as captured]))

(def identity-keys [:proposal-id :component-id :path])

(defn current-tab [columns tab-id identity]
  (let [matches (for [column columns tab (:tabs column) :when (= tab-id (:id tab))]
                  {:col-id (:id column) :tab tab})
        match (when (and tab-id (= 1 (count matches))) (first matches))]
    (when (and (= :proposal-source (get-in match [:tab :type]))
               (= identity (select-keys (get-in match [:tab :data]) identity-keys)))
      match)))

(defn step [state]
  (case (:status state)
    :listing {:kind :list}
    :reading {:kind :chunk :offset (count (:bytes state))}
    nil))

(defn accept-list [identity state result]
  (let [listing (:data result)
        entry (first (filter #(= (:path identity) (:path %)) (:files listing)))]
    (if (and (= :ok (:status result))
             (captured/source-list-valid? (:proposal-id identity) (:component-id identity) listing)
             entry)
      (assoc state :status :reading :listing listing :entry entry :bytes [])
      (assoc state :status :unavailable))))

(defn accept-chunk [state result decoded]
  (if (and (= :ok (:status result)) (vector? decoded)
           (every? #(and (integer? %) (<= 0 % 255)) decoded)
           (captured/source-chunk-valid? (:listing state) (:entry state) (count (:bytes state))
                                         (:data result) (count decoded)))
    (-> state (update :bytes into decoded)
        (assoc :status (if (get-in result [:data :next-offset]) :reading :received)))
    (-> state (assoc :status :unavailable) (dissoc :bytes))))

(defn finish [state text]
  (if (= :received (:status state))
    (cond-> (-> state (dissoc :bytes) (assoc :status (if (string? text) :ready :unavailable)))
      (string? text) (assoc :text text))
    state))
