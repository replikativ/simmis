(ns is.simm.ops.room-digest-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [datahike.blob :as blob]
            [dvergr.catalog.room :as catalog]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.conversation-digest :as report]
            [is.simm.ops.digest-inputs :as inputs]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.room-digest :as digest]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.conversation-capture-test :as fixture]
            [is.simm.starter.conversation-digest :as starter]))

(defn retained-fixture
  ([] (retained-fixture {"/inbox/conversations/original.md" "An original, not a digest.\r\n"}))
  ([originals]
   (let [paths (mapv #(subs % 1) (keys originals))
         selection (digest/selection-of-inputs paths originals)
         source (starter/source)
         bundle (:id (catalog/bundle starter/name source))
         submission {:name starter/name :expected-bundle bundle
                     :params {"evidence-manifest" (pr-str selection)} :inputs originals}
         prepared (runs/prepare-source {:files source} submission)
         retained {:kind :workflow :limits {} :input (assoc (select-keys prepared [:source :workflow :inputs])
                                                          :submission submission
                                                          :environment {:environment/id :simmis/saved-workflow :environment/version 1
                                                                        :environment/task (:task prepared)
                                                                        :environment/metadata {:workflow (:workflow prepared)}})}
         by (random-uuid) room (random-uuid) client (random-uuid) request (requests/request-id by room client)
         row {:work.request/id request :work.request/by by :work.request/client-id client
              :work.request/room room :work.request/kind :workflow :work.request/intent-id (hasch/uuid retained)}]
     {:room room :request request :row row :retained retained :selection selection :originals originals})))

(deftest identical-document-bytes-retain-distinct-path-revisions-and-result-citations
  (let [text "The same original text.\r\n"
        originals {"/inbox/a.md" text "/inbox/b.md" text}
        {:keys [room request row retained selection]} (retained-fixture originals)
        sources (:sources selection)
        revisions (mapv :source-revision sources)
        verified (digest/verify-retained! room request row (edn/read-string (pr-str retained)))
        value {:version 1 :selection-id (:id selection)
               :summary {:text "Both originals report the same observation." :sources revisions}
               :coverage (mapv (fn [revision] {:source-revision revision :status :selected :note "Read this original."}) revisions)
               :sections {:decisions [] :proposals []
                          :observations [{:text "The observation appears in two documents." :sources revisions}]
                          :uncertainty [] :open-work []}}
        roundtrip (edn/read-string (pr-str value))
        changed (digest/selection-of-inputs ["inbox/a.md" "inbox/b.md"]
                                            (assoc originals "/inbox/a.md" (str text "Changed.")))]
    (is (= 2 (count sources)))
    (is (= 1 (count (set (map :file-content sources)))) "Equal bytes retain their shared content identity")
    (is (= 2 (count (set revisions))) "Coverage and citations distinguish document paths")
    (is (= selection (:selection verified)))
    (is (= originals (into {} (map (fn [{:keys [path text]}] [(str "/" path) text]) (:originals verified)))))
    (is (= value (report/validate! (:selection verified) roundtrip)))
    (is (= (report/digest-id selection value) (report/digest-id (:selection verified) roundtrip)))
    (is (= (mapv :source-id sources) (mapv :source-id (:sources changed))))
    (is (not= (:source-revision (first sources)) (get-in changed [:sources 0 :source-revision])))
    (is (= (:source-revision (second sources)) (get-in changed [:sources 1 :source-revision])))))

(deftest retained-reader-checks-canonical-request-and-exact-frozen-originals
  (let [{:keys [room request row retained selection]} (retained-fixture)]
    (is (= selection (:selection (digest/verify-retained! room request row retained))))
    (doseq [bad-row [(assoc row :work.request/room (random-uuid))
                     (assoc row :work.request/client-id (random-uuid))
                     (assoc row :work.request/intent-id (random-uuid))]]
      (is (thrown? Exception (digest/verify-retained! room request bad-row retained))))
    (let [damaged (update-in retained [:input :inputs "/inbox/conversations/original.md"] str " changed")
          changed-row (assoc row :work.request/intent-id (hasch/uuid damaged))]
      (is (thrown? Exception (digest/verify-retained! room request changed-row damaged))))))

(deftest preview-reads-only-explicit-already-retained-default-memory-originals
  (@#'fixture/with-fixture
   (fn [{:keys [host w owned attached]}]
     (repo/write! owned "inbox/original.md" (.getBytes "Exact retained original\r\n" "UTF-8"))
     (let [before @owned other @attached
           p (digest/preview! host (ops/party-principal (:owner w)) (str (:room w)) {:paths ["inbox/original.md"]})]
       (is (= :preview (:status p)))
       (is (= before @owned))
       (is (= other @attached))
       (is (= :unknown (get-in p [:selection :sources 0 :authority])))
       (is (= (blob/blob-id (.getBytes "Exact retained original\r\n" "UTF-8"))
              (get-in p [:selection :sources 0 :file-content])))
       (is (= (:selection p) (inputs/validate-inputs! (starter/source) (:params p) (:inputs p))))
       (is (thrown? Exception (digest/preview! host (ops/party-principal (:owner w)) (str (:room w)) {:paths ["inbox/missing.md"]})))))))
