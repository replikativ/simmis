(ns is.simm.agents.office-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [dvergr.catalog.workspace :as workspace]
            [dvergr.discourse :as discourse]
            [dvergr.effects :as effects]
            [dvergr.room.store.memory :as memory]
            [dvergr.runtime.ctx :as runtime-ctx]
            [dvergr.substrate.geschichte :as geschichte]
            [geschichte.repo :as repo]
            [is.simm.agents.office :as adapter]
            [is.simm.media.office :as office]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [sci.core :as sci]
            [yggdrasil.protocols :as systems]))

(defn- container [text]
  (office/write-parts
   {"word/document.xml" (.getBytes (str "<w:document xmlns:w=\"urn:test\"><w:p><w:r><w:t>"
                                        text "</w:t></w:r></w:p></w:document>") "UTF-8")}))

(defn- with-room [f]
  (let [room (discourse/make-room {:id (keyword (str "office-capability-" (random-uuid))) :store (memory/make)})]
    (try
      (workspace/ensure-workspace! room)
      (binding [ec/*execution-context* (:ctx room)] (f room))
      (finally (discourse/close-room! room)))))

(defn- install! [room policy]
  (let [cap (random-uuid) owner (:ctx room)
        sid (systems/system-id (geschichte/current-system))]
    (runtime-ctx/install-sandbox-binding!
     owner cap {:agent-id :mcp/office-test :room-runtime-id (:id room)
                :simmis/source-memory {:room-id (random-uuid) :agent-id :mcp/office-test :system-id sid}})
    (with-meta (adapter/bindings owner cap nil
                                 (merge {:authorize! (constantly true) :authorize-write! (constantly true)
                                         :proposal-workspace (constantly nil)} policy))
      {:capability-id cap})))

(deftest saved-office-capability-reads-the-selected-source-world
  (with-room
    (fn [room]
      (repo/write! (:conn (geschichte/current-workspace)) "report.docx" (container "parent"))
      (let [api (install! room {})
            sci (sci/init {:namespaces {'office api}})
            _ (sci/eval-string* sci "(def saved-open office/open)")
            child (ygg/fork!)]
        (try
          (binding [ec/*execution-context* (:child-ctx child)]
            (repo/write! (:conn (geschichte/current-workspace)) "report.docx" (container "child"))
            (let [h (sci/eval-string* sci "(saved-open \"/report.docx\")")]
              (is (string? (:handle h)))
              (is (str/includes? (:text ((api 'text) h)) "child"))
              (is (= :read-only (:error ((api 'save-as!) h "/copy.docx"))))))
          (let [h ((api 'open) "report.docx")]
            (is (str/includes? (:text ((api 'text) h)) "parent")))
          (finally (ygg/discard-fork! child)))))))

(deftest legacy-drive-and-publication-refuse-without-resolving-shared-adapters
  (with-room
    (fn [room]
      (repo/write! (:conn (geschichte/current-workspace)) "report.docx" (container "original"))
      (let [resolved (atom 0) writes (atom 0)
            api (install! room {:legacy-filesystem (fn [_]
                                                     (swap! resolved inc)
                                                     {:write-file! (fn [& _] (swap! writes inc))})})
            h ((api 'open) "report.docx")
            child (ygg/fork!)]
        (try
          (is (string? (:handle h)))
          (binding [ec/*execution-context* (:child-ctx child)]
            (is (str/includes? (:text ((api 'text) h)) "original") "captured data may be staged")
            (is (= 1 (:replaced ((api 'replace-text) h "original" "child"))))
            (is (= :unsupported-operation (:error ((api 'open) "/drive/report.docx"))))
            (is (= :read-only (:error ((api 'save-as!) h "/drive/copy.docx")))))
          (is (= 0 @resolved) "child never resolves the root filesystem")
          (is (= 0 @writes))
          (is (= :unsupported-operation (:error ((api 'open) "/drive/report.docx"))))
          (with-redefs [office/write-parts (fn [_] (throw (AssertionError. "Publication must refuse before ZIP construction")))]
            (is (= :read-only (:error ((api 'save-as!) h "/drive/copy.docx")))))
          (is (= 0 @resolved))
          (is (= 0 @writes))
          (finally (ygg/discard-fork! child)))))))

(deftest revocation-refuses-before-handle-access-or-filesystem-resolution
  (with-room
    (fn [room]
      (let [allowed? (atom true) resolved (atom 0)
            api (install! room {:authorize! (fn [_]
                                              (when-not @allowed? (throw (ex-info "Revoked" {:error :not-authorized}))))
                                :legacy-filesystem (fn [_] (swap! resolved inc) {})})]
        (reset! allowed? false)
        (is (= :not-authorized (:error ((api 'text) "unknown"))))
        (is (= :not-authorized (:error ((api 'open) "/drive/a.docx"))))
        (is (= 0 @resolved))
        (is (not (contains? ((api 'parts) "unknown") :open)))))))

(deftest proposal-connection-and-descendant-source-selection-stay-distinct
  (with-room
    (fn [room]
      (repo/write! (:conn (geschichte/current-workspace)) "report.docx" (container "parent"))
      (let [proposal (ygg/fork!)
            proposal-ws (binding [ec/*execution-context* (:child-ctx proposal)] (geschichte/current-workspace))
            _ (repo/write! (:conn proposal-ws) "report.docx" (container "proposal"))
            consultations (atom 0)
            api (install! room {:proposal-workspace (fn [_] (swap! consultations inc) proposal-ws)})
            child (ygg/fork!)]
        (try
          (let [h ((api 'open) "report.docx")]
            (is (str/includes? (:text-preview h) "proposal")))
          (is (= 1 @consultations))
          (binding [ec/*execution-context* (:child-ctx child)]
            (let [h ((api 'open) "report.docx")]
              (is (str/includes? (:text-preview h) "parent"))))
          (is (= 1 @consultations) "child does not consult the owner's proposal")
          (finally (ygg/discard-fork! child) (ygg/discard-fork! proposal)))))))

(deftest replayed-io-still-creates-and-updates-real-local-handles
  (with-room
    (fn [room]
      (repo/write! (:conn (geschichte/current-workspace)) "report.docx" (container "original"))
      (let [api (install! room {}) recording (effects/recording!)
            run! (fn []
                   (let [h ((api 'open) "report.docx")]
                     (is (string? (:handle h)))
                     (is (= 1 (:replaced ((api 'replace-text) h "original" "edited"))))
                     (is (str/includes? (:text ((api 'text) h)) "edited"))
                     (is (= :read-only (:error ((api 'save-as!) h "/drive/copy.docx"))))
                     (is (:closed ((api 'close) h)))
                     (:handle h)))]
        (effects/install-world! (:ctx room) [[:record {:id recording}]])
        (let [before (run!) entries (effects/recorded recording)
              replay (effects/replay! entries)]
          (is (every? #(or (= true (:value %)) (map? (:value %)) (string? (:value %))) entries)
              "receipts contain data, not host byte arrays or live handles")
          (effects/install-world! (:ctx room) [[:replay {:id replay}]])
          (is (not= before (run!)) "replay creates a new actual component handle"))))))

(deftest source-identity-rebind-and-missing-systems-refuse-retained-handles
  (with-room
    (fn [room]
      (repo/write! (:conn (geschichte/current-workspace)) "report.docx" (container "private"))
      (let [api (install! room {}) h ((api 'open) "report.docx")
            cap (:capability-id (meta api))
            child (ygg/fork! {:systems :none})]
        (try
          (binding [ec/*execution-context* (:child-ctx child)]
            (is (= :unavailable (:error ((api 'text) h)))))
          (runtime-ctx/update-sandbox-binding! (:ctx room) cap assoc-in [:simmis/source-memory :scope] (random-uuid))
          (is (= :unknown-handle (:error ((api 'text) h))))
          (finally (ygg/discard-fork! child)))))))

(deftest unrelated-world-cannot-use-a-retained-office-capability
  (with-room
    (fn [_]
      (repo/write! (:conn (geschichte/current-workspace)) "report.docx" (container "private"))
      (let [a (ygg/fork!) b (ygg/fork!)
            api (binding [ec/*execution-context* (:child-ctx a)] (install! {:ctx (:child-ctx a) :id :test} {}))
            h (binding [ec/*execution-context* (:child-ctx a)] ((api 'open) "report.docx"))]
        (try
          (binding [ec/*execution-context* (:child-ctx b)]
            (is (= :wrong-world (:error ((api 'text) h))))
            (is (not (contains? ((api 'parts) h) :open))))
          (finally (ygg/discard-fork! a) (ygg/discard-fork! b)))))))

(deftest malformed-replayed-binary-results-refuse-before-parsing
  (with-room
    (fn [room]
      (repo/write! (:conn (geschichte/current-workspace)) "report.docx" (container "private"))
      (let [api (install! room {}) perform effects/perform!]
        (doseq [value [42 "not-an-office-receipt" "simmis-office-base64-v1:!!!"]]
          (with-redefs [effects/perform! (fn [boundary effect executor]
                                           (if (= :fs/read (:effect effect)) value
                                               (perform boundary effect executor)))]
            (is (= :invalid-receipt (:error ((api 'open) "report.docx"))))))))))
