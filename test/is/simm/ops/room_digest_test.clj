(ns is.simm.ops.room-digest-test
  (:require [clojure.edn :as edn]
            [clojure.test :refer [deftest is]]
            [datahike.blob :as blob]
            [dvergr.agent.attempt :as attempt]
            [dvergr.agent.environment :as environment]
            [dvergr.agent.episode :as episode]
            [dvergr.agent.evaluation :as evaluation]
            [dvergr.agent.roster :as roster]
            [dvergr.agent.run :as run]
            [dvergr.catalog.room :as catalog]
            [dvergr.room.store :as store]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.conversation-digest :as report]
            [is.simm.ops.digest-inputs :as inputs]
            [is.simm.ops.dvergr :as ops]
            [is.simm.ops.room-digest :as digest]
            [is.simm.ops.work :as work]
            [is.simm.ops.workflow-runs :as runs]
            [is.simm.ops.conversation-capture-test :as fixture]
            [is.simm.starter.conversation-digest :as starter]))

(deftest completed-reader-certifies-the-full-retained-agent-definition
  (let [party (random-uuid) request (random-uuid) job-id (random-uuid) candidate-id (random-uuid)
        room-id :digest-certification-fixture room {:id room-id :ctx nil}
        agent (-> (roster/make-roster)
                  (roster/make-agent {:id :candidate :program {:kind :echo}})
                  (roster/agent :candidate))
        verifier (evaluation/evaluator-ref work/completion)
        definition (environment/make-environment
                    {:id :simmis/saved-workflow :task "Summarize retained originals."
                     :verifier {:id (:verifier/id verifier) :version (:verifier/version verifier)
                                :basis (:verifier/basis verifier)}
                     :world {:isolation :ctx :settlement :discard}})
        row {:work.request/id request :work.request/by party :work.request/job job-id}
        input {:candidate agent :environment definition}
        job {:run/id job-id :run/kind :workflow :run/room room-id
             :run/actor (keyword "party" (str party)) :run/status :completed :run/ended-at (java.util.Date.)}
        candidate {:run/id candidate-id :run/parent job-id :run/kind :agent-task :run/room room-id
                   :run/actor (:agent/id agent) :run/status :completed :run/ended-at (java.util.Date.)
                   :run/agent-version (:agent/version agent) :run/program-kind :echo
                   :run/interpreter-version 5 :run/agent-def-hash (hasch/uuid agent)}
        certify (fn [definition-agent extra-metrics checks]
                  (let [evidence {:result "done"
                                  :trace {:runs [(select-keys candidate [:run/id :run/parent :run/actor :run/status :run/agent-def-hash])]}
                                  :files {"/out/digest.edn" "{}"}}
                        receipt (environment/make-attempt-receipt
                                 definition {:run-id candidate-id :provider :dvergr :model "echo" :status :completed
                                             :started-at 1000 :elapsed-ms 25
                                             :metrics (merge {:program-kind :echo :agent-version (:agent/version definition-agent)
                                                              :agent-def-hash (hasch/uuid definition-agent) :interpreter-version 5
                                                              :requester-party party :request-id request :execution-kind :saved-workflow
                                                              :verifier-trust (evaluation/evaluator-tier work/completion)} extra-metrics)
                                             :checks checks :reward 1.0 :result (:result evidence) :trace (:trace evidence)})]
                    (attempt/make-attempt definition definition-agent receipt evidence :discard)))
        certified (certify agent {} {:completed? true})
        read! (fn [record]
                (with-redefs [run/run (fn [_ id] (get {job-id job candidate-id candidate} id))
                              run/runs (fn [& _] [job candidate])
                              store/conversation-id (constantly room-id)
                              episode/attempt (fn [& _] record)]
                  (@#'digest/completed-attempt! room row input)))]
    (is (map? (:attempt/agent certified)))
    (is (= certified (read! certified)) "A native full AgentDef is distinct from its actor keyword")
    (doseq [bad [(certify (update agent :agent/version inc) {} {:completed? true})
                 (certify agent {:request-id (random-uuid)} {:completed? true})
                 (certify agent {:requester-party (random-uuid)} {:completed? true})
                 (certify agent {:execution-kind :benchmark} {:completed? true})
                 (certify agent {} {:completed? false})]]
      (is (thrown? clojure.lang.ExceptionInfo (read! bad))
          "Canonical but foreign AgentDef, invocation metrics or completion checks are refused"))))

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
