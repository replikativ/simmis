(ns is.simm.authority.memory-remote-test
  "Actual inbound policy/handlers over real two-repository Rooms, OTHER selected.
   Only file-backed default provisioning uses the existing memory fixture seam."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.rooms.files :as native]
            [dvergr.substrate.geschichte :as workspace]
            [is.simm.agents.room-agents :as rooms]
            [is.simm.authority.world :as world]
            [is.simm.distributed-scope :as ds]
            [is.simm.documents.files :as files]
            [is.simm.documents.docx-test :as docx-fixtures]
            [is.simm.media.office :as zip]
            [is.simm.model.access :as access]
            [is.simm.ops.owned-workspace-test :as fixture]
            [is.simm.runtimes.context :as context]
            [is.simm.runtimes.memory-changes :as changes]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [org.replikativ.spindel.engine.impl.simple :as simple])
  (:import [java.util Base64]))

(defn- with-fixture [owned-first? f]
  (let [configs (atom []) chat-connection (atom nil) completed? (atom false)]
    (try
      (let [result (@#'fixture/with-fixture
                    owned-first?
                    (fn [{:keys [conn owned attached w room] :as data}]
                      (let [chat (:conn (:store room))]
                        (reset! chat-connection chat)
                        (reset! configs (mapv #(-> % deref :config) [conn owned attached chat])))
                      (let [hydrations (atom 0)
                            inbound (fn [kind name request]
                                      (world/inbound! (world/principal w kind) name
                                                      (cond-> {:room-id-str (str (:room w))}
                                                        (some? request) (assoc :request request))))]
                        (with-redefs [context/server-context (:ctx room)
                                      rooms/live-room (fn [id]
                                                        (swap! hydrations inc)
                                                        (when (= id (:room w)) room))]
                          (f (assoc data :inbound inbound :hydrations hydrations))))))]
        (reset! completed? true)
        result)
      (finally
        ;; Only NORMAL shared-fixture return proves its physical Room teardown
        ;; and releases acknowledged. Preserve stores after callback/teardown
        ;; exceptions for diagnosis, rather than inferring quiescence.
        (when @completed?
          (when-let [chat @chat-connection] (d/release chat))
          (doseq [cfg @configs]
            (when-not (and (= :memory (get-in cfg [:store :backend]))
                           (uuid? (get-in cfg [:store :id])))
              (throw (ex-info "Refuse non-fixture cleanup" {})))
            (d/delete-database cfg)))))))

(defn- discover [inbound]
  (let [reply (:result (inbound :member "load-memory-metadata!" {:folders [""] :paths ["wiki/file.md"]}))]
    (is (= 2 (:protocol-version reply)))
    (is (uuid? (:target-stamp reply)))
    (:target-stamp reply)))

(defn- error-info [reply]
  (when (:error reply) (edn/read-string (:error reply))))

(deftest actual-stamped-inbound-reads-and-writes-use-only-owned-default
  (doseq [owned-first? [true false]]
    (with-fixture
      owned-first?
      (fn [{:keys [room owned attached own-descriptor inbound]}]
        (let [xml "<document><!-- keep -->Hello</document>"
              container (zip/write-parts {"word/document.xml" (.getBytes xml "UTF-8")})]
          (native/write-to! room (:system own-descriptor)
                            {"/src/core.clj" {:text "(throw (ex-info \"Reading must not execute\" {}))"}
                             "/report.docx" {:base64 (.encodeToString (Base64/getEncoder) container)}} {})
          (let [stamp (discover inbound) scope {:expected-target-stamp stamp}
                call (fn [name args] (:result (inbound :member name (merge scope args))))
                other @attached selected (workspace/selected-system-id)
                opened (call "load-memory-file!" {:path "wiki/file.md"})
                office (call "load-memory-file!" {:path "report.docx"})
                history (call "load-memory-history!" {:path "wiki/file.md"})
                analysis (call "analyze-memory-text!" {:path "wiki/file.md" :text (:text opened)})
                slot (first (:slots analysis))]
            (is (= "# Owned\n" (:text opened)))
            (is (= :source-only (:execution-policy (call "load-memory-file!" {:path "src/core.clj"}))))
            (is (= ["src" "wiki" "report.docx"] (mapv :name (:entries (call "load-memory-folder!" {:folder ""})))))
            (is (= ["src/core.clj"] (mapv :path (:entries (call "search-memory-files!" {:query "core"})))))
            (is (= "# Owned\n" (:text (call "load-memory-history-file!"
                                            {:path "wiki/file.md" :commit (:commit (first (:versions history)))}))))
            (is (= "# Owned\n" (String. (.decode (Base64/getDecoder)
                                                 ^String (:base64 (call "download-memory-file!"
                                                                        {:path "wiki/file.md" :expected (:entry opened)}))) "UTF-8")))
            (is (= stamp (:target-stamp opened) (:target-stamp history) (:target-stamp analysis)))
            (is (= (:text opened) (:text (call "apply-memory-rich-edit!"
                                               (merge {:path "wiki/file.md" :text (:text opened) :revision (:revision analysis)}
                                                      (select-keys slot [:from :to :model]))))))
            (is (:saved? (call "save-memory-rich-file!"
                               {:path "wiki/file.md" :expected (:entry opened) :source (:text opened)
                                :revision (:revision analysis) :edits [] :text (:text opened)})))
            (is (= xml (:text (call "load-memory-office-part!" {:path "report.docx" :expected (:entry office) :part nil}))))
            (is (:saved? (call "save-memory-office-part!"
                               {:path "report.docx" :expected (:entry office) :part "word/document.xml" :text xml})))
            (let [created (call "save-memory-file!" {:path "wiki/new.md" :expected nil :text "Created"})
                  moved (call "move-memory-file!" {:path "wiki/new.md" :target "wiki/moved.md" :expected (:entry created)})]
              (is (true? (:saved? created)))
              (is (true? (:moved? moved)))
              (is (= stamp (:target-stamp created) (:target-stamp moved)))
              (is (= "Created" (:text (files/read-file-in @owned "wiki/moved.md"))))
              (is (nil? (files/entry @owned "wiki/new.md"))))
            (is (= other @attached))
            (is (= selected (workspace/selected-system-id)))))))))

(deftest actual-inbound-word-correction-is-a-stamped-owned-write
  (with-fixture
    false
    (fn [{:keys [room owned attached own-descriptor inbound]}]
      (let [container (zip/write-parts (docx-fixtures/package-parts "<w:p><w:r><w:t>Original</w:t></w:r></w:p>"))]
        (native/write-to! room (:system own-descriptor)
                          {"/report.docx" {:base64 (.encodeToString (Base64/getEncoder) container)}} {})
        (let [stamp (discover inbound) other @attached
              file (:result (inbound :member "load-memory-file!" {:path "report.docx" :expected-target-stamp stamp}))
              opened (:result (inbound :member "load-memory-office-part!"
                                       {:path "report.docx" :expected (:entry file) :part nil :expected-target-stamp stamp}))
              token (first (vals (get-in opened [:reading :corrections :tokens])))
              request {:path "report.docx" :expected (:entry file) :token token :replacement "Corrected" :expected-target-stamp stamp}
              saved (:result (inbound :member "correct-memory-office-text!" request))]
          (is (uuid? token))
          (is (true? (:saved? saved)))
          (let [read (:result (inbound :member "load-memory-office-part!"
                                     {:path "report.docx" :expected (:entry saved) :part nil :expected-target-stamp stamp}))]
            (is (= "Corrected" (get-in read [:reading :blocks 0 :runs 0 :text]))))
          (is (:error (inbound :member "correct-memory-office-text!" request)) "The old revision cannot be reused")
          (is (= other @attached)))))))

(deftest inbound-and-direct-worker-authorization-precede-room-hydration
  (with-fixture
    true
    (fn [{:keys [w inbound hydrations]}]
      (doseq [kind [:non-member :admin :anon]
              [name args] [["load-memory-metadata!" {:folders [""] :paths []}]
                           ["prepare-memory-stream!" nil]
                           ["preview-memory-declaration!" {:path "wiki/file.md" :target {:kind :page}
                                                            :types [] :additions [] :expected-target-stamp (random-uuid)}]
                           ["save-memory-declaration!" {:path "wiki/file.md" :expected-target-stamp (random-uuid)}]
                           ["load-memory-file!" {:path "wiki/file.md" :expected-target-stamp (random-uuid)}]
                           ["save-memory-file!" {:path "bad.md" :expected nil :text "bad" :expected-target-stamp (random-uuid)}]
                           ["correct-memory-office-text!" {:path "report.docx" :expected nil :token (random-uuid)
                                                           :replacement "bad" :expected-target-stamp (random-uuid)}]]]
        (is (world/refused? (inbound kind name args))))
      (is (zero? @hydrations))
      ;; This ordering unit isolates the actual drain predicate, not a conveyed
      ;; dynamic spin-id. Even an authorized caller must not hydrate on-drain.
      (let [failure (with-redefs [simple/on-drain-thread? (constantly true)]
                      (try (remote/invoke-memory (:owner w) (str (:room w)) :metadata {:folders [] :paths []})
                           nil
                           (catch clojure.lang.ExceptionInfo error error)))]
        (is (= :validation-error (:type (ex-data failure))))
        (is (zero? @hydrations)))
      (doseq [party [(:non-member w) (:admin w) nil]]
        (is (thrown? Exception (remote/invoke-memory party (str (:room w)) :metadata {:folders [] :paths []}))))
      (is (zero? @hydrations))
      (with-redefs [access/require! (fn [& _] (throw (AssertionError. "Worker used ambient identity")))]
        (binding [ds/*principal* (world/principal w :non-member)]
          (is (= 2 (:protocol-version (remote/invoke-memory (:owner w) (str (:room w)) :metadata {:folders [] :paths []}))))
          (is (thrown? Exception (remote/invoke-memory (:non-member w) (str (:room w)) :metadata {:folders [] :paths []}))))))))

(deftest replacement-reports-only-a-safe-wire-target-conflict
  (with-fixture
    false
    (fn [{:keys [conn w inbound owned attached-descriptor]}]
      (let [stamp (discover inbound) before @owned]
        (d/transact conn [{:room/id (:room w) :room/default-repository [:system/id (:registry-system attached-descriptor)]}])
        (let [failure (inbound :member "load-memory-file!" {:path "wiki/file.md" :expected-target-stamp stamp})
              info (error-info failure)]
          (is (= :memory/target-changed (:type info)))
          (is (= #{:error :type :correlation-id} (set (keys info))))
          (is (not (str/includes? (:error failure) (str stamp))))
          (is (not (str/includes? (:error failure) "workspace-config"))))
        (is (= :memory/target-changed
               (:type (error-info (inbound :member "save-memory-file!"
                                           {:path "bad.md" :expected nil :base64 "not base64!"
                                            :expected-target-stamp stamp})))))
        (let [fresh (discover inbound)]
          (is (not= stamp fresh))
          (is (= "# Attachment\n" (get-in (inbound :member "load-memory-file!"
                                                   {:path "wiki/file.md" :expected-target-stamp fresh}) [:result :text]))))
        (is (= before @owned))))))

(deftest current-write-revocation-and-invalid-shapes-never-publish
  (with-fixture
    true
    (fn [{:keys [conn w inbound owned attached]}]
      (let [stamp (discover inbound) before @owned other @attached writer native/write-to!]
        (doseq [request [{:path "bad.md" :expected nil :text "no stamp"}
                         {:path "bad.md" :expected nil :text "forged actor" :expected-target-stamp stamp :actor (:owner w)}]]
          (is (:error (inbound :member "save-memory-file!" request))))
        (doseq [[name args] [["load-memory-history!" {:path "wiki/file.md"}]
                             ["load-memory-file!" {:path "wiki/file.md"}]
                             ["load-memory-folder!" {:folder ""}]
                             ["search-memory-files!" {:query "file"}]]]
          (is (:error (inbound :member name args))))
        (let [entry (files/entry @owned "wiki/file.md")
              failure (inbound :member "save-memory-file!"
                               {:path "wiki/file.md" :expected (assoc entry :content (random-uuid))
                                :text "Stale file, not a stale target" :expected-target-stamp stamp})]
          (is (:error failure))
          (is (not= :memory/target-changed (:type (error-info failure)))))
        (with-redefs [native/write-to!
                      (fn [& args]
                        (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
                        (apply writer args))]
          (let [failure (inbound :member "save-memory-file!"
                                 {:path "revoked.md" :expected nil :text "Must not appear" :expected-target-stamp stamp})]
            (is (:error failure))
            (is (not= :memory/target-changed (:type (error-info failure))))))
        (is (= before @owned))
        (is (= other @attached))
        (is (world/refused? (inbound :member "load-memory-file!" {:path "wiki/file.md" :expected-target-stamp stamp})))))))

(deftest generic-room-sci-retains-its-permissions-and-closed-request-schema
  (with-fixture
    true
    (fn [{:keys [inbound]}]
      (let [code {:path "src/core.clj" :text "(+ 20 22)" :scope :file :from 0 :to 0}
            repl {:syntax :superficie :text "20 + 22" :namespace "user"}]
        (doseq [[name args] [["evaluate-memory-code!" code] ["evaluate-memory-repl!" repl]]]
          (is (= "42" (get-in (inbound :owner name args) [:result :value])))
          (doseq [kind [:member :non-member :admin :anon]]
            (is (world/refused? (inbound kind name args))))
          (is (:error (inbound :owner name (assoc args :party (random-uuid))))))))))

(deftest watch-discovery-uses-the-real-owned-service-with-private-transport-seam
  (with-fixture
    true
    (fn [{:keys [conn w room inbound]}]
      (let [owner (changes/open-owned! conn (:ctx room) (fn [& _]))
            registrations (atom [])]
        (try
          (with-redefs [changes/register-topic! (fn [_ party] (swap! registrations conj party))
                        changes/watch-owned! (fn [id r party] ((:watch! owner) id r party))]
            (let [reply (:result (inbound :member "prepare-memory-stream!" nil))]
              ;; The ACK confirms intent only. Repository discovery belongs to
              ;; the single worker, not a concurrent transport reconciliation.
              (is (= {:watching? true :permissions {:write? true :evaluate? false}} reply))
              (is (uuid? (discover inbound)))
              (is (= [(:member w)] @registrations))))
          (finally ((:close! owner))))))))
