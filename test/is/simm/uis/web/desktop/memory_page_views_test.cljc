(ns is.simm.uis.web.desktop.memory-page-views-test
  (:require [is.simm.documents.page-view-contract :as contract]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-page-views :as views]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(def path "views/companies.md")
(def view-id #uuid "b5140b8e-f610-4774-bcf0-f4ae853f1c71")
(def stamp #uuid "7282de0a-7648-4926-a8b2-4f587fb247bc")
(def content #uuid "a513c74c-078a-449b-813c-9b835d270602")
(def descriptor {:version 1 :id view-id :source {:kind :pages :roots ["companies"] :types []}
                 :presentation :table :columns [{:kind :title}] :limit 50
                 :order [{:kind :title :direction :asc}]})
(def file {:kind :markdown :text "saved" :entry {:content content :size 5 :mode 33188}
           :target-stamp stamp :analysis {:revision content :page-views [{:from 10 :to 100 :view view-id
                                                                        :status :ready :descriptor descriptor}]}})
(def state {:connection :connection :target-stamp stamp :target-verified? true :target-epoch 1 :read-epoch 1
            :drafts {path {:source file :text "saved" :generation 0}}})

(defn- reply [state]
  (merge (select-keys (views/request state path view-id) [:path :expected :revision :from :to :view])
         {:profile contract/profile :descriptor descriptor :target-stamp stamp :status :ready
          :basis #uuid "88838c60-6a85-4c1a-b8f7-c62df5f728df" :columns [{:kind :title :label "Page"}] :rows []}))

(deftest results-bind-the-exact-saved-source-and-request-generation
  (let [sent (views/marker state path view-id) result (reply state)]
    (is (views/reply-current? state path view-id sent result))
    (doseq [changed [(assoc state :connection :replacement)
                     (assoc state :target-epoch 2) (assoc state :read-epoch 2)
                     (assoc state :definition-basis :changed)
                     (assoc-in state [:drafts path :generation] 1)]]
      (is (not (views/reply-current? changed path view-id sent result))))
    (doseq [bad [(assoc result :view (random-uuid)) (assoc result :revision (random-uuid))
                 (assoc result :from 11) (assoc result :target-stamp (random-uuid))
                 (assoc result :descriptor (assoc descriptor :source {:kind :pages :roots [""]}))]]
      (is (not (views/reply-current? state path view-id sent bad))))))

(deftest ordinary-refresh-keeps-prior-data-but-changed-meaning-or-authority-hides-it
  (let [saved (assoc-in state [:page-views [path view-id]] {:reply (reply state)})
        prose-draft (assoc-in saved [:drafts path :text] "Unsaved prose")]
    (is (views/visible (assoc saved :read-epoch 2) path view-id))
    (is (views/visible prose-draft path view-id))
    (is (nil? (views/request prose-draft path view-id)) "do not dispatch a new saved query while its file is dirty")
    (is (nil? (views/visible (assoc saved :target-verified? false) path view-id)))
    (is (nil? (views/visible (assoc saved :target-stamp (random-uuid)) path view-id)))
    (is (nil? (views/visible (assoc-in saved [:drafts path :local-source] {:text "editing the definition"}) path view-id)))
    (is (nil? (views/visible (assoc-in saved [:drafts path :source :analysis :page-views 0 :descriptor :limit] 5)
                             path view-id)))
    (is (nil? (views/visible (update saved :page-views dissoc [path view-id]) path view-id))
        "a failed read can retire its old data conservatively")))

(deftest row-enrollment-checks-portable-page-identity-in-the-current-memory
  (let [id (random-uuid) row {:id id :path "companies/a.md"}
        enrolled (assoc-in state [:drafts (:path row) :source]
                           (assoc-in file [:analysis :document-id] id))]
    (is (views/row-identity-current? enrolled row))
    (is (not (views/row-identity-current? enrolled (assoc row :id (random-uuid)))))
    (is (not (views/row-identity-current? (assoc enrolled :target-stamp (random-uuid)) row)))
    (is (not (views/row-identity-current? (assoc enrolled :target-verified? false) row)))
    (is (not (views/row-identity-current? (assoc-in enrolled [:drafts (:path row) :source :entry :content]
                                                  (random-uuid)) row)))
    (is (not (views/row-identity-current? (update enrolled :drafts dissoc (:path row)) row)))))

(deftest background-row-interests-and-clean-buffer-retirement-stay-bounded
  (let [row-path "companies/a.md" old-path "companies/old.md" other-path "notes/ordinary.md"
        key [path view-id]
        draft {:source (assoc file :path row-path) :text "saved"}
        state (-> state (assoc :selected path :page-view-open {key true}
                               :page-view-edits {key {:id (random-uuid) :path row-path}}
                               :page-view-enrolled #{row-path old-path})
                  (assoc-in [:drafts row-path] draft) (assoc-in [:drafts old-path] draft)
                  (assoc-in [:drafts other-path] draft))
        retired (views/retire-unused-rows state)]
    (is (some #{row-path} (:paths (editor/metadata-request (update state :drafts dissoc row-path)))))
    (is (contains? (:drafts retired) row-path) "active form remains enrolled")
    (is (not (contains? (:drafts retired) old-path)))
    (is (contains? (:drafts retired) other-path) "ordinary cached files are not view-owned")
    (is (contains? (:drafts state) old-path) "the original context value is unchanged")
    (doseq [protected [(assoc-in state [:drafts old-path :text] "retained source")
                       (assoc-in state [:property-drafts [old-path {:kind :page}]] {:field {:input {:text "retained"}}})
                       (assoc-in state [:declaration-drafts [old-path {:kind :page}]] {:types []})
                       (assoc-in state [:loading [:file old-path]] true)]]
      (is (contains? (:drafts (views/retire-unused-rows protected)) old-path)))))

(deftest inactive-report-forms-and-many-clean-row-visits-do-not-accumulate-demands
  (let [old-path "companies/old.md" old-key ["views/previous.md" (random-uuid)]
        draft {:source file :text "saved"}
        base (assoc state :selected path :page-view-open {old-key true}
                          :page-view-edits {old-key {:id (random-uuid) :path old-path}}
                          :page-view-attempts {old-key :already-read}
                          :page-view-enrolled #{old-path}
                          :drafts (assoc (:drafts state) old-path draft))
        retired (views/retire-unused-rows base)]
    (is (not (contains? (:drafts retired) old-path)))
    (is (nil? (get-in retired [:page-view-edits old-key])))
    (is (nil? (get-in retired [:page-view-attempts old-key]))
        "Returning to an open report re-reads before auto-selecting its form")
    (is (not (some #{old-path} (:paths (editor/metadata-request retired)))))
    (let [dirty (assoc-in base [:drafts old-path :text] "retained work")
          retained (views/retire-unused-rows dirty)]
      (is (= "retained work" (get-in retained [:drafts old-path :text])))
      (is (= (:page-view-edits dirty) (:page-view-edits retained)))
      (is (= (:page-view-attempts dirty) (:page-view-attempts retained))
          "Retained dirty forms keep their selection and request state"))
    (let [key [path view-id]
          states (reductions
                  (fn [s n]
                    (let [row-path (str "companies/" n ".md")]
                      (views/retire-unused-rows
                       (-> s (assoc-in [:page-view-open key] true)
                           (assoc-in [:page-view-edits key] {:id (random-uuid) :path row-path})
                           (assoc-in [:drafts row-path] draft)
                           (update :page-view-enrolled (fnil conj #{}) row-path)))))
                  (assoc state :selected path) (range 80))]
      (is (every? #(<= (count (:paths (editor/metadata-request %))) 2) states))
      (is (= 1 (count (:page-view-enrolled (last states))))))))
