(ns is.simm.uis.web.desktop.memory-edit-owners-test
  (:require [is.simm.uis.web.desktop.memory-edit-owners :as owners]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(def path "companies/example.md")
(def session #uuid "95951573-b231-434b-a6e8-7b8fbecb3ad0")
(def memory #uuid "7cf1d647-8122-4002-881b-c935d00e201a")
(def first-key [session "room-a" :tab :first])
(def second-key [session "room-a" :tab :second])
(def clean {:target-stamp memory :drafts {path {:source {:text "saved" :entry {:content (random-uuid)}} :text "saved"}}})

(deftest other-unsaved-work-is-protected-without-blocking-unrelated-memories
  (let [dirty (assoc-in clean [:drafts path :text] "retained draft")
        states {first-key clean second-key dirty}]
    (is (owners/blocked? states first-key path))
    (is (= [:source-draft] (mapv :reason (owners/conflicts states first-key path))))
    (is (false? (owners/blocked? {first-key dirty} first-key path)) "own drafts use the existing per-file checks")
    (is (false? (owners/blocked? (assoc-in states [second-key :target-stamp] (random-uuid)) first-key path)))
    (is (false? (owners/blocked? {first-key clean [session "room-b" :tab :second] dirty} first-key path)))
    (is (false? (owners/blocked? {first-key clean [(random-uuid) "room-a" :tab :second] dirty} first-key path)))
    (is (false? (owners/blocked? states first-key "companies/other.md")))
    (is (false? (owners/blocked? (dissoc states second-key) first-key path)))
    (is (= "retained draft" (get-in states [second-key :drafts path :text])))))

(deftest property-type-and-pending-write-states-share-the-same-guard
  (doseq [[other reason]
          [[(assoc clean :property-drafts {[path {:kind :page}] {:field {:text "typed"}}}) :property-draft]
           [(assoc clean :declaration-drafts {[path {:kind :page}] {:types []}}) :type-draft]
           [(assoc clean :loading {[:property-save path] true}) :pending-write]
           [(assoc clean :loading {[:save path] true}) :pending-write]
           [(assoc clean :loading {[:move path] true}) :pending-write]]]
    (is (= [reason] (mapv :reason (owners/conflicts {first-key clean second-key other} first-key path)))))
  (is (false? (owners/blocked? {first-key clean second-key (assoc clean :loading {[:file path] true})} first-key path))))

(deftest observations-do-not-entangle-forked-buffer-values
  (let [root {first-key clean second-key clean}
        child (assoc-in root [second-key :drafts path :text] "child-only change")]
    (is (owners/blocked? child first-key path))
    (is (false? (owners/blocked? root first-key path)))
    (is (= "saved" (get-in root [second-key :drafts path :text])))))
