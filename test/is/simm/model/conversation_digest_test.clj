(ns is.simm.model.conversation-digest-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.model.conversation-digest :as digest]))

(defn source-ref [authority]
  {:message-id (random-uuid) :source-revision (random-uuid) :content-id (random-uuid)
   :path (str "inbox/conversations/" (random-uuid) ".md") :authority authority})

(defn reference [selection]
  (let [revision (get-in selection [:sources 0 :source-revision])]
    {:version 1 :selection-id (:id selection)
     :summary {:text "A suggestion remains unapproved." :sources [revision]}
     :coverage (mapv (fn [source] {:source-revision (:source-revision source) :status :selected :note "Read original."})
                     (:sources selection))
     :sections {:decisions [] :proposals [{:text "Assistant proposed a review." :sources [revision]}]
                :observations [] :uncertainty [] :open-work []}}))

(deftest selection-consumes-room-artifacts-and-generic-capture-refs
  (let [message (source-ref :human)
        capture {:source-id (random-uuid) :source-revision (random-uuid) :file-content (random-uuid)
                 :path "captures/observation.md" :source-kind :capture :authority :capture}
        selection (digest/selection [message capture])]
    (is (= selection (digest/selection [capture message])))
    (is (= #{:conversation :capture} (set (map :source-kind (:sources selection)))))
    (is (= #{(:message-id message) (:source-id capture)} (set (map :source-id (:sources selection)))))
    (is (thrown? Exception (digest/selection [message message])))
    (is (thrown? Exception (digest/selection [(assoc message :path "../secrets.md")]))))
  (is (thrown? Exception (digest/selection [])))
  (is (thrown? Exception (digest/selection (vec (repeatedly 9 #(source-ref :unknown)))))))

(deftest exact-membership-and-plain-structured-citations
  (let [selection (digest/selection [(source-ref :assistant)])
        value (reference selection)]
    (is (= value (digest/validate! selection value)))
    (is (uuid? (digest/digest-id selection value)))
    (doseq [bad [(assoc value :selection-id (random-uuid))
                 (assoc value :coverage [])
                 (update value :coverage conj (first (:coverage value)))
                 (assoc-in value [:coverage 0 :source-revision] (random-uuid))
                 (assoc-in value [:summary :sources] [(random-uuid)])
                 (assoc-in value [:coverage 0 :status] :omitted)
                 (assoc-in value [:sections :proposals 0 :sources] [])
                 (assoc-in value [:summary :text] "<script>alert(1)</script>")
                 (assoc value :html "<p>Unsupported</p>")]]
      (is (thrown? Exception (digest/validate! selection bad))))))

(deftest omission-and-deferral-are-explicit-and-do-not-mutate-originals
  (let [artifact (source-ref :human) original artifact
        selection (digest/selection [artifact])]
    (doseq [status [:omitted :deferred]]
      (let [value (-> (reference selection)
                      (assoc :summary {:text "No source is represented in this digest." :sources []})
                      (assoc :sections (zipmap digest/section-keys (repeat [])))
                      (assoc-in [:coverage 0 :status] status)
                      (assoc-in [:coverage 0 :note] (if (= :omitted status) "Read; no useful navigation detail."
                                                      "Original could not be inspected.")))]
        (is (= value (digest/validate! selection value)))
        (is (= original artifact))
        (is (= selection (digest/selection [artifact])))))))
