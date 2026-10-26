(ns is.simm.uis.web.desktop.memory-page-views
  "Pure saved-view correspondence and reply fences in context-owned state.
   Displayed results remain explicit snapshots, never editable capabilities."
  (:require [is.simm.documents.page-view-contract :as contract]
            [is.simm.uis.web.desktop.memory-charts :as charts]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-declarations :as declarations]
            [is.simm.uis.web.desktop.memory-metadata-view :as metadata-view]))

(defn entries [state path]
  (vec (take contract/max-mounted
             (:page-views (charts/saved-analysis (get-in state [:drafts path]))))))

(defn entry [state path view]
  (first (filter #(and (= view (:view %)) (= :ready (:status %))) (entries state path))))

(defn marker [state path view]
  (let [entry (entry state path view)]
    [(:connection state) (:target-stamp state) (:target-epoch state)
     (:read-epoch state) (:definition-basis state) (:stream-marker state)
     (get-in state [:drafts path :source :entry]) (get-in state [:drafts path :generation])
     (select-keys entry [:from :to :descriptor])]))

(defn owned? [state path]
  (let [draft (get-in state [:drafts path])]
    (and (:target-verified? state) (uuid? (:target-stamp state)) (not (:target-stale? draft))
         (= (:target-stamp state) (get-in draft [:source :target-stamp]))
         (not (:local-source draft)))))

(defn request [state path view]
  (when-let [entry (when (owned? state path) (entry state path view))]
    (when-not (editor/dirty? (get-in state [:drafts path]))
      (merge (select-keys entry [:from :to :view])
             {:path path :expected (get-in state [:drafts path :source :entry])
              :revision (:revision (charts/saved-analysis (get-in state [:drafts path])))
              :expected-target-stamp (:target-stamp state)}))))

(defn compatible?
  "Prior results can remain visible during ordinary refresh or prose saves.
   Changed view meaning/target or loss of verification hides them immediately."
  [state path view reply]
  (let [entry (entry state path view)]
    (and (owned? state path) entry
         (= contract/profile (:profile reply))
         (= view (:view reply)) (= path (:path reply))
         (= (:target-stamp state) (:target-stamp reply))
         (= (:descriptor entry) (:descriptor reply))
         (contains? #{:ready :unavailable} (:status reply)))))

(defn reply-current? [state path view sent reply]
  (let [request (request state path view)]
    (and request (= sent (marker state path view))
         (compatible? state path view reply)
         (= (select-keys request [:path :expected :revision :from :to :view])
            (select-keys reply [:path :expected :revision :from :to :view]))
         (or (= :unavailable (:status reply))
             (and (uuid? (:basis reply)) (vector? (:rows reply))
                  (<= (count (:rows reply)) contract/max-rows)
                  (vector? (:columns reply)) (<= (count (:columns reply)) contract/max-columns))))))

(defn visible [state path view]
  (let [reply (get-in state [:page-views [path view] :reply])]
    (when (compatible? state path view reply) reply)))

(defn row-identity-current? [state row]
  (let [draft (get-in state [:drafts (:path row)])
        analysis (charts/saved-analysis draft)]
    (and (owned? state (:path row)) (= :markdown (get-in draft [:source :kind]))
         (= (:id row) (:document-id analysis)))))

(defn retire-unused-rows
  "Release only clean buffers enrolled by saved views. Walking a table must
   not accumulate every visited file in the 64-path metadata demand. Existing
   ordinary buffers, active forms, pending IO and every draft remain owned."
  [state]
  (let [active (into #{(:selected state)}
                     (keep (fn [[key row]] (when (and (= (:selected state) (first key))
                                                      (get-in state [:page-view-open key])) (:path row))))
                     (:page-view-edits state))
        pending? (fn [path] (some (fn [[slot value]] (and value (vector? slot) (= path (second slot)))) (:loading state)))
        paths (remove (fn [path]
                        (or (contains? active path) (pending? path)
                            (editor/dirty? (get-in state [:drafts path]))
                            (get-in state [:drafts path :local-source])
                            (editor/property-drafts? state path) (declarations/drafts? state path)))
                      (:page-view-enrolled state))]
    (reduce
     (fn [state path]
       (let [slots (into #{} (keep (fn [[slot _]] (when (and (vector? slot) (= path (second slot))) slot)))
                         (:request-contexts state))
             scope-maps [:property-models :property-attempts :property-drafts :declaration-drafts :reference-search]
             edit-keys (keep (fn [[key row]] (when (= path (:path row)) key)) (:page-view-edits state))]
         (-> (reduce (fn [state key]
                       (update state key #(into {} (remove (fn [[scope _]] (= path (first scope)))) %)))
                     state scope-maps)
             (metadata-view/retire-reads slots)
             (update :drafts dissoc path) (update :observed-entries dissoc path)
             (update :file-intents dissoc path) (update :page-view-enrolled disj path)
             ;; Returning to an open form must obtain a fresh reply to
             ;; re-establish its retired clean selection. Dirty selections stay.
             (update :page-view-attempts #(apply dissoc % edit-keys))
             (update :page-view-edits #(apply dissoc % edit-keys))
             (update :page-view-row-attempts #(apply dissoc % edit-keys))
             (update :errors #(into {} (remove (fn [[slot _]] (and (vector? slot) (= path (second slot))))) %)))))
     state paths)))
