(ns is.simm.ops.owned-mcp-transport-test
  "Actual MCP dispatch after the authenticated-principal boundary, using the
   inactive Simmis host and real EACL/two native repos. No listener or JWT claim."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.catalog.room :as catalog-room]
            [dvergr.mcp.json-rpc :as rpc]
            [dvergr.mcp.server :as server]
            [dvergr.mcp.surface :as surface]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [is.simm.ops.owned-mcp-test :as fixture]
            [jsonista.core :as json]))

(def ^:private versions
  ["2024-11-05" "2025-03-26" "2025-06-18" "2025-11-25" "2026-07-28"])

(defn- session [version principal]
  (let [context (server/session-context (fn [_]) (surface/selection {:profile :admin}) principal)]
    (when-not (= "2026-07-28" version)
      (let [initialized (rpc/handle-message context {:jsonrpc "2.0" :id 1 :method "initialize"
                                                     :params {:protocolVersion version :capabilities {}
                                                              :clientInfo {:name "simmis-owned-test" :version "1"}}})]
        (is (= version (get-in initialized [:result :protocolVersion])) (pr-str initialized)))
      (rpc/handle-message context {:jsonrpc "2.0" :method "notifications/initialized"}))
    (fn [method params]
      (let [params (cond-> params
                     (= "2026-07-28" version)
                     (assoc :_meta {:io.modelcontextprotocol/protocolVersion version
                                    :io.modelcontextprotocol/clientInfo {:name "simmis-owned-test" :version "1"}
                                    :io.modelcontextprotocol/clientCapabilities {}}))]
        (json/read-value
         (json/write-value-as-string
          (rpc/handle-message context {:jsonrpc "2.0" :id 2 :method method :params params})
          (json/object-mapper {:encode-key-fn #(if (keyword? %) (subs (str %) 1) (str %))}))
         json/keyword-keys-object-mapper)))))

(defn- tool [send name args]
  (send "tools/call" {:name name :arguments args}))

(defn- data [reply]
  (is (nil? (:error reply)) (pr-str reply))
  (is (not (get-in reply [:result :isError])) (pr-str reply))
  (json/read-value (get-in reply [:result :content 0 :text]) json/keyword-keys-object-mapper))

(defn- durable-state [conn]
  (let [db @conn]
    {:commit (d/commit-id db) :config (:config db) :worktree (query/worktree db)}))

(deftest tools-and-resources-read-the-same-owned-memory-in-every-protocol
  (doseq [owned-first? [true false]]
    (@#'fixture/with-fixture
     owned-first?
     (fn [{:keys [conn w host attached hydrations]}]
       (let [principal (@#'fixture/principal w :member)
             id (str (:room w)) slug (:room/slug (d/entity @conn [:room/id (:room w)]))
             selection (workspace/selected-system-id) before (durable-state attached)]
         (with-redefs [server/current-daemon (constantly host)]
           (doseq [version versions handle [id slug]]
             (let [send (session version principal)
                   opened (data (tool send "room_read" {:room handle :path "wiki/file.md"}))
                   stamp (:target-stamp opened)
                   listing (data (tool send "room_files" {:room handle :path "/wiki"}))
                   file (send "resources/read" {:uri (str "room://" handle "/files/wiki/file.md")})
                   directory (send "resources/read" {:uri (str "room://" handle "/files/wiki")})
                   derived (send "resources/read" {:uri (str "room://" handle "/files")})]
               (is (= "# Owned\n" (:text opened) (get-in file [:result :contents 0 :text])))
               (is (some? (parse-uuid stamp)))
               (is (= stamp (:target-stamp listing)
                      (get-in file [:result :contents 0 :_meta :dvergr/target-stamp])
                      (get-in directory [:result :contents 0 :_meta :dvergr/target-stamp])))
               (is (= ["file.md"] (mapv :name (:entries listing))))
               (is (= ["file.md"]
                      (mapv :name (:entries (json/read-value (get-in directory [:result :contents 0 :text])
                                                             json/keyword-keys-object-mapper)))))
               (is (= stamp (:target-stamp
                             (json/read-value (get-in derived [:result :contents 0 :text])
                                              json/keyword-keys-object-mapper))))
               (is (= "# Owned\n" (:text (data (tool send "room_read"
                                                     {:room handle :path "wiki/file.md"
                                                      :expected-target-stamp stamp})))))))
           (is (= (* 6 2 (count versions)) (count @hydrations)))
           (is (every? #(= (:room w) %) @hydrations))
           (is (= selection (workspace/selected-system-id)))
           (is (= before (durable-state attached)))))))))

(deftest an-open-mcp-session-does-not-cache-room-authority
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w host hydrations]}]
     (with-redefs [server/current-daemon (constantly host)]
       (let [sessions (mapv #(session % (@#'fixture/principal w :member)) ["2025-11-25" "2026-07-28"])
             room (str (:room w))]
         (doseq [send sessions]
           (is (= "# Owned\n" (:text (data (tool send "room_read" {:room room :path "wiki/file.md"}))))))
         (let [before (count @hydrations)]
           (doseq [send sessions]
             (let [reply (tool send "room_write" {:room room :files {"/wiki/file.md" {:text "Forbidden"}}})]
               (is (or (:error reply) (get-in reply [:result :isError])))))
           (is (= before (count @hydrations)))
           (d/transact conn [[:db/retract [:room/id (:room w)] :room/parties [:party/id (:member w)]]])
           (doseq [send sessions
                   reply [(tool send "room_read" {:room room :path "wiki/file.md"})
                          (send "resources/read" {:uri (str "room://" room "/files/wiki/file.md")})]]
             (is (or (:error reply) (get-in reply [:result :isError]))))
           (is (= before (count @hydrations)))))))))

(deftest configured-catalog-and-file-tools-use-the-same-owned-source
  (doseq [owned-first? [true false]]
    (@#'fixture/with-fixture
     owned-first?
     (fn [{:keys [conn w host owned attached hydrations]}]
       (let [source {"workflow.edn" "{:title \"Owned insight\" :task \"Think\"}"
                     "checker.clj" "(throw (ex-info \"CHECKER MUST NOT EXECUTE\" {}))"
                     "fixtures/input.md" "Owned evidence"}
             handle (str (:room w))
             slug (:room/slug (d/entity @conn [:room/id (:room w)]))
             expected (str (:id (catalog-room/bundle "insight" source)))]
         (doseq [[connection files] [[owned source]
                                     [attached (assoc source "fixtures/input.md" "Attachment evidence")]]
                 [path text] files]
           (repo/write! connection (str "workflows/insight/" path) (.getBytes ^String text "UTF-8")))
         (let [selection (workspace/selected-system-id) other (durable-state attached)]
           (with-redefs [server/current-daemon (constantly host)
                         catalog-room/tier (constantly :ad-hoc)
                         catalog-room/run-checker (fn [& _] (throw (AssertionError. "Checker executed")))]
             (doseq [original [handle slug]]
               (let [send (session "2026-07-28" (@#'fixture/principal w :member))
                     rows (data (tool send "catalog_list" {:room original}))
                     local (filterv #(= (str original "/insight") (:id %)) rows)
                     opened (data (tool send "room_read"
                                        {:room original :path "workflows/insight/fixtures/input.md"}))]
                 (is (= 1 (count local)))
                 (is (= expected (:bundle (first local))))
                 (is (= "Owned insight" (:title (first local))))
                 (is (= "Owned evidence" (:text opened)))
                 (is (= (:target-stamp opened) (:target-stamp (first local))))))
             (is (= [(:room w) (:room w) (:room w) (:room w)] @hydrations))
             (is (= selection (workspace/selected-system-id)))
             (is (= other (durable-state attached))))))))))
