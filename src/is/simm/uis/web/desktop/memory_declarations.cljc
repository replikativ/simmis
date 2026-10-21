(ns is.simm.uis.web.desktop.memory-declarations
  "Pure declaration-draft transitions. Callers keep these values inside their
   Spindel execution-context state; no runtime handles or authority live here."
  (:require [is.simm.uis.web.desktop.memory-editor :as editor]))

(defn draft [model]
  {:baseline (editor/property-baseline model) :types (:assigned-types model [])
   :additions [] :inputs {} :generation 0})

(defn current? [draft model]
  (= (:baseline draft) (editor/property-baseline model)))

(defn drafts? [state path]
  (boolean (some (fn [[[p _] value]] (and (= p path) (some? value))) (:declaration-drafts state))))

(defn busy? [state path]
  (boolean (some #(get-in state [:loading [% path]]) [:save :property-save :office-save :move])))

(defn move-blocked? [state path]
  (boolean (or (busy? state path) (drafts? state path) (editor/property-drafts? state path)
               (editor/dirty? (get-in state [:drafts path])))))

(defn openable? [state path model]
  (boolean (and model (not= :unavailable (:status model))
                (get-in state [:drafts path :source :entry])
                (= (:expected model) (get-in state [:drafts path :source :entry]))
                (not (move-blocked? state path)))))

(defn discard [state path target]
  (if (or (busy? state path) (nil? (get-in state [:declaration-drafts [path target]]))) state
      (update state :declaration-drafts dissoc [path target])))

(defn set-types [draft ids]
  (if (or (nil? draft) (= ids (:types draft))) draft
      (-> draft (assoc :types ids) (update :generation inc) (dissoc :preview))))

(defn add-binding [draft field alias]
  (if (or (nil? draft) (some #(= alias (:alias %)) (:additions draft))
          (some #(= (:id field) (:property %)) (:additions draft)))
    draft
    (-> draft
        (update :additions conj {:alias alias :property (:id field) :declaring-type (:declaring-type field)})
        (assoc-in [:inputs (:id field)] {:null true})
        (update :generation inc)
        (dissoc :preview))))

(defn remove-binding [draft id]
  (let [next (filterv #(not= id (:property %)) (:additions draft))]
    (if (or (nil? draft) (= next (:additions draft))) draft
        (-> draft (assoc :additions next) (update :inputs dissoc id)
            (update :generation inc) (dissoc :preview)))))

(defn preview-request [draft]
  (merge (select-keys (:baseline draft) [:path :target])
         {:types (:types draft) :additions (:additions draft)
          :expected-target-stamp (get-in draft [:baseline :target-stamp])}))

(defn previewed [draft generation reply]
  (if (not= generation (:generation draft)) draft
      (if (current? draft reply)
        (-> draft (assoc :preview reply :type-choices (:type-choices reply)) (dissoc :conflict?))
        ;; Preserve the user's proposed types/values; a fresh read is not
        ;; permission to silently rebase a declaration draft onto new source.
        (assoc draft :conflict? true))))

(defn ready? [draft]
  (let [preview (:preview draft)
        fields (:fields preview)
        new-fields (into {} (map (juxt :id identity)) (filter :new? fields))]
    (boolean
     (and preview (not (:conflict? draft)) (current? draft preview)
          ;; Required unbound fields must be explicitly bound in this batch.
          (not-any? #(and (:required? %)
                          (or (:needs-binding? %)
                              (and (not (:new? %)) (not (:present? %))))) fields)
          (every? (fn [{:keys [property]}]
                    (let [field (get new-fields property) input (get-in draft [:inputs property])]
                      (and (:editable? field) (map? input)
                           (or (not (:required? field)) (not (:null input)))))) (:additions draft))))))

(defn command [draft]
  (when (ready? draft)
    (let [preview (:preview draft)]
      (-> (select-keys preview [:path :target :expected :revision :scope-revision :proposed-scope-revision])
          (assoc :expected-target-stamp (get-in draft [:baseline :target-stamp])
                 :types (:types draft)
                 :additions (mapv #(assoc % :input (get-in draft [:inputs (:property %)])) (:additions draft)))))))
