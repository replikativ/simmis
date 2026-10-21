(ns is.simm.uis.web.desktop.memory-declarations-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.uis.web.desktop.memory-declarations :as declarations]))

(def type-id #uuid "11111111-1111-4111-8111-111111111111")
(def field-id #uuid "22222222-2222-4222-8222-222222222222")
(def model {:path "wiki/a.md" :target {:kind :page} :assigned-types []
            :expected {:content #uuid "33333333-3333-4333-8333-333333333333"}
            :revision #uuid "44444444-4444-4444-8444-444444444444"
            :scope-revision #uuid "55555555-5555-4555-8555-555555555555"
            :target-stamp #uuid "66666666-6666-4666-8666-666666666666"})
(def field {:id field-id :declaring-type type-id :required? true :new? true :editable? true})
(def reply (assoc model :proposed-scope-revision #uuid "77777777-7777-4777-8777-777777777777" :fields [field]))

(deftest drafts-are-pure-forkable-values-and-preserve-explicit-false-zero
  (let [original (declarations/draft model)
        typed (declarations/set-types original [type-id])
        bound (declarations/add-binding typed field "active")
        previewed (declarations/previewed bound (:generation bound) reply)]
    (is (= [] (:types original)))
    (is (= [type-id] (:types typed)))
    (is (= bound (declarations/add-binding bound field "duplicate-property")))
    (is (false? (declarations/ready? previewed)))
    (is (nil? (declarations/command previewed)))
    (doseq [text ["false" "0" "9007199254740993" ""]]
      (let [draft (assoc-in previewed [:inputs field-id] {:text text})
            command (declarations/command draft)]
        (is (declarations/ready? draft))
        (is (= {:text text} (get-in command [:additions 0 :input])))
        (is (= (:target-stamp model) (:expected-target-stamp command)))
        (is (not (contains? command :fields)))))
    (let [removed (declarations/remove-binding bound field-id)]
      (is (empty? (:additions removed)))
      (is (empty? (:inputs removed)))
      (is (> (:generation removed) (:generation bound))))))

(deftest stale-previews-never-rebase-or-overwrite-drafts
  (let [draft (-> (declarations/draft model) (declarations/set-types [type-id]))
        newer (declarations/add-binding draft field "revenue")
        stale-source (assoc reply :revision (random-uuid))
        conflicted (declarations/previewed newer (:generation newer) stale-source)]
    (is (= newer (declarations/previewed newer (:generation draft) reply)))
    (is (:conflict? conflicted))
    (is (= (:baseline newer) (:baseline conflicted)))
    (is (= (:additions newer) (:additions conflicted)))
    (is (nil? (declarations/command conflicted)))))

(deftest callbacks-for-retired-drafts-do-not-create-new-state
  (is (nil? (declarations/set-types nil [type-id])))
  (is (nil? (declarations/add-binding nil field "revenue")))
  (is (nil? (declarations/remove-binding nil field-id)))
  (is (nil? (declarations/previewed nil 0 reply))))

(deftest move-and-declaration-admission-exclude-each-other-in-either-order
  (let [path (:path model) target (:target model)
        state {:drafts {path {:text "saved" :source {:text "saved" :entry (:expected model)}}}}
        open (assoc-in state [:declaration-drafts [path target]] (declarations/draft model))]
    (is (declarations/openable? state path model))
    (doseq [operation [:move :save :property-save :office-save]]
      (is (false? (declarations/openable? (assoc-in state [:loading [operation path]] true) path model))))
    (is (declarations/move-blocked? open path))
    (is (false? (declarations/openable? open path model)))
    ;; Changing the visible property scope does not remove or transfer a draft.
    (let [switched (assoc-in open [:property-targets path] {:kind :block :id field-id})
          cancelled (declarations/discard switched path target)]
      (is (declarations/drafts? switched path))
      (is (= (declarations/draft model) (get-in switched [:declaration-drafts [path target]])))
      (is (false? (declarations/drafts? cancelled path)))
      (is (false? (declarations/move-blocked? cancelled path))))))

(deftest queued-discard-cannot-forget-an-in-flight-assignment
  (let [path (:path model) target (:target model)
        state {:declaration-drafts {[path target] (declarations/draft model)}}]
    (doseq [operation [:move :save :property-save :office-save]]
      (let [busy (assoc-in state [:loading [operation path]] true)]
        (is (= busy (declarations/discard busy path target)))))
    (is (false? (declarations/drafts? (declarations/discard state path target) path)))))

(deftest missing-required-or-readonly-inputs-never-gain-client-authority
  (let [base (-> (declarations/draft model) (declarations/set-types [type-id])
                 (declarations/add-binding field "revenue") (assoc-in [:inputs field-id] {:text "0"}))]
    (doseq [fields [[(assoc field :editable? false :reason :owner-managed-property)]
                    [(dissoc field :new?)]
                    [field {:id (random-uuid) :required? true :needs-binding? true}]]]
      (let [draft (declarations/previewed base (:generation base) (assoc reply :fields fields))]
        (is (false? (declarations/ready? draft)))
        (is (nil? (declarations/command draft)))))))
