(ns is.simm.documents.saved-view-snapshot-test
  (:require [clojure.string :as str]
            [is.simm.documents.saved-view-contract :as contract]
            [is.simm.documents.saved-view-snapshot :as snapshot]
            [is.simm.documents.links :as links]
            #?(:clj [is.simm.documents.markdown :as markdown])
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])))

(def context {:id #uuid "19d55f77-d4a6-4790-b737-e11e1358b99c"
              :room #uuid "82d2b578-25f8-44d3-98ba-2769b5cb3f40"
              :target #uuid "1fd7da0a-451f-4de4-939a-41e81c0a7c98"
              :created-at "2026-10-26T05:30:00.000Z"})
(def reply
  {:profile contract/profile :status :ready :view #uuid "be4dfe5a-a4e2-4cce-8304-a18140fa85ae"
   :basis #uuid "940b606c-0c6f-4bff-85a6-66837357adfb" :revision #uuid "35b97810-b6f1-4ead-8f69-5c0d36c0c449"
   :target-stamp (:target context) :path "reports/benchmark.md" :from 12 :to 100
   :expected {:content #uuid "ff325edf-949d-4c47-88fb-389e2be34d1f" :size 100 :mode 33188}
   :descriptor {:version 1 :source {:kind :scorecard :room :current
                                   :scorecard #uuid "1d14d88e-dd1b-4052-b739-398798ae1d54"}}
   :source {:kind :scorecard :room (:room context) :store #uuid "1281b9d7-1a62-42f7-9986-75ffd7cb546f"
            :scorecard #uuid "1d14d88e-dd1b-4052-b739-398798ae1d54"}
   :complete? false :display {:complete? false} :total 2 :matrix-complete? true
   :matrix {:cells 4 :environments 2 :repetitions 1}
   :columns [{:kind :candidate :label "Candidate"} {:kind :cost :label "Cost"} {:kind :attempts :label "Attempts"}]
   :rows [{:id [#uuid "1d14d88e-dd1b-4052-b739-398798ae1d54" "control"] :candidate "control"
           :cells [{:kind :text :text "control" :complete? true} {:kind :null} {:kind :number :text "0"}]}]})

(deftest snapshot-preserves-visible-values-and-does-not-become-a-live-query
  (let [output (snapshot/markdown reply context)]
    (is (str/includes? output "| control | — | 0 |"))
    (is (str/includes? output "1 of 2 rows"))
    (is (str/includes? output "Source selection complete: no. Display previews complete: no."))
    (is (str/includes? output "A dash means unknown"))
    (is (str/includes? output (str (:basis reply))))
    (is (str/includes? output "\"matrix-complete?\":true"))
    (is (not (str/includes? output "```simmis-view")))
    (is (= output (snapshot/markdown reply context)))))

(deftest table-text-remains-inert-and-cannot-add-cells-or-blocks
  (let [literal "<script>x</script>|[click](javascript:alert(1))\n# heading"
        source (assoc-in reply [:rows 0 :cells 0] {:kind :text :text literal :complete? true})
        output (snapshot/markdown source context)]
    (is (str/includes? output "&lt;script&gt;"))
    (is (str/includes? output "\\|\\[click\\]"))
    (is (str/includes? output "&#10;\\# heading"))
    (is (not (str/includes? output "<script>")))
    (is (not (str/includes? output "\n# heading")))))

(deftest capture-export-retains-the-exact-original-media-identity
  (let [media {:kind :media :text "Original recording" :path "captures/session/original.webm"
               :content #uuid "12e54580-f572-4f41-94f6-ac2a84966b74"}
        source (-> reply (assoc-in [:descriptor :source :kind] :archived-captures)
                   (assoc :columns [{:kind :recording :label "Recording"}]
                          :rows [{:id [(:id context) "captures/note.md"] :cells [media]}]))
        output (snapshot/markdown source context)
        url (links/media-route (merge media (select-keys context [:room :target])))]
    (is (str/includes? output (str "[Original recording](" url ")")))
    (is (str/includes? output "Media bytes are not embedded"))
    (is (str/includes? output "alignment is estimated"))
    (is (str/includes? output (str (:content media))))))

(deftest refused-exports-do-not-silently-drop-information
  (doseq [bad [(assoc reply :status :unavailable) (assoc reply :basis nil)
               (assoc reply :target-stamp (random-uuid)) (assoc reply :total 0)
               (assoc reply :rows (vec (repeat 51 (first (:rows reply)))))
               (assoc-in reply [:rows 0 :cells] [])
               (assoc-in reply [:rows 0 :cells 0] {:kind :media :text "Lost original" :path "../outside"})
               (assoc reply :rows (vec (repeat 51 {:cells []})))
               (assoc-in reply [:provenance :unsupported] (fn [] :never-call))]]
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (snapshot/markdown bad context))))
  (let [large {:kind :text :text (apply str (repeat 16384 "x")) :complete? true}
        source (assoc reply :total 50 :columns [{:label "Value"}]
                      :rows (vec (repeat 50 {:cells [large]})))]
    (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error) (snapshot/markdown source context)))))

#?(:clj
   (deftest ordinary-markdown-reader-preserves-snapshot-metadata-and-table
     (let [output (snapshot/markdown reply context) parsed (markdown/parse output)
           props (get-in parsed [:frontmatter :properties])
           metadata (get props "simmis-snapshot")]
       (is (= (str (:id context)) (get props "simmis-id")))
       (is (= (str (:basis reply)) (get metadata "basis")))
       (is (= false (get metadata "complete?")))
       (is (= true (get metadata "matrix-complete?")))
       (is (= (str (get-in reply [:expected :content])) (get-in metadata ["expected" "content"])))
       (is (= "control" (get (first (get metadata "rows")) "candidate")))
       (is (= 1 (count (filter #(= :TableBlock (:kind %)) (:nodes parsed)))))
       (is (empty? (:diagnostics parsed))))))
