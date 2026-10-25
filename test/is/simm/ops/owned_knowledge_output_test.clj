(ns is.simm.ops.owned-knowledge-output-test
  "Actual control DB/Room/application authority for existing output discovery.
   Synthetic request headers are metadata fixtures, never fabricated Attempts."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.mcp.server :as server]
            [dvergr.mcp.surface :as surface]
            [dvergr.mcp.json-rpc :as rpc]
            [dvergr.ops :as dops]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.ops.knowledge-output-service :as service]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.owned-knowledge-output :as output]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-mcp :as mcp-host]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.work-api :as work-api]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- with-fixture [f]
  (@#'fixture/with-fixture
   true
   (fn [{:keys [conn w room] :as data}]
     (let [owner (doto (lifetime/create) lifetime/open!)
           host (mcp-host/host conn (:ctx room) owner)
           request (random-uuid) job (random-uuid)
           principal {:principal/kind :human :principal/id (str "party/" (:owner w))}
           row {:work.request/id request :work.request/by (:owner w) :work.request/room (:room w)
                :work.request/kind :workflow :work.request/state :submitted :work.request/job job
                :work.request/publication-basis-id (random-uuid)}]
       (try
         (d/transact conn [row])
         (with-redefs-fn {#'application/installation (atom {:host host :owner owner})}
           #(f (assoc data :host host :owner owner :principal principal :request request :job job
                      :row (assoc row :db/id (d/q '[:find ?e . :in $ ?id :where [?e :work.request/id ?id]] @conn request))
                      :inspect (fn [] ((service/prepare-inspect-job (:owner w) (:room w) job))))))
         (finally (lifetime/fence! owner) (lifetime/join! owner 30000)))))))

(deftest canonical-root-job-discovers-only-its-authors-matching-workflow-request
  (with-fixture
    (fn [{:keys [conn w request job row inspect hydrations]}]
      (is (= {:status :eligible :request (str request) :job (str job)} (inspect)))
      (is (empty? @hydrations) "Request descriptors do not hydrate a Room or read source")
      (doseq [change [{:work.request/by (:member w)} {:work.request/room (:other-room w)}
                      {:work.request/kind :benchmark} {:work.request/state :admitting}]]
        (d/transact conn [(merge row change)])
        (is (= {:status :none} (inspect)))
        (is (= {:status :unavailable} ((service/prepare-collect (:owner w) (:room w) request)))))
      (d/transact conn [row])
      (d/transact conn [[:db/retract [:work.request/id request] :work.request/publication-basis-id
                        (:work.request/publication-basis-id row)]])
      (is (= {:status :none} (inspect)))
      (is (empty? @hydrations)))))

(deftest a-foreign-duplicate-job-header-does-not-hide-behind-author-filtering
  (with-fixture
    (fn [{:keys [conn w row inspect hydrations]}]
      (d/transact conn [(assoc (dissoc row :db/id) :work.request/id (random-uuid) :work.request/by (:member w))])
      (is (= {:status :none} (inspect)))
      (is (empty? @hydrations)))))

(deftest missing-or-foreign-identities-never-reach-hydration-or-filing
  (with-fixture
    (fn [{:keys [host owner w request job hydrations]}]
      (let [calls (atom 0)]
        (with-redefs [publish/file-completed-request-unannounced! (fn [& _] (swap! calls inc))]
          (doseq [party [(:member w) (:non-member w) (:admin w) (:team-agent w) nil]]
            (is (= {:status :none} ((service/prepare-inspect-job party (:room w) job))))
            (is (= {:status :unavailable} ((service/prepare-collect party (:room w) request)))))
          (is (zero? @calls)) (is (empty? @hydrations))
          (is (= {:status :none}
                 (lifetime/with-operation! owner
                   #(output/inspect-job! host {:principal/kind :human :principal/id (str "party/" (:owner w))}
                                         (:room w) (random-uuid))))))))))

(deftest queued-output-actions-cannot-borrow-a-replacement-application
  (with-fixture
    (fn [{:keys [host owner w request job hydrations]}]
      (let [inspect (service/prepare-inspect-job (:owner w) (:room w) job)
            collect (service/prepare-collect (:owner w) (:room w) request)
            replacement (doto (lifetime/create) lifetime/open!) calls (atom 0)]
        (try
          (lifetime/fence! owner)
          (with-redefs-fn {#'application/installation (atom {:host host :owner replacement})
                          #'publish/file-completed-request-unannounced! (fn [& _] (swap! calls inc))}
            #(do (is (= {:status :none} (inspect)))
                 (is (= {:status :unavailable} (collect)))
                 (is (zero? @calls)) (is (empty? @hydrations))))
          (finally (lifetime/fence! replacement) (lifetime/join! replacement 30000)))))))

(deftest current-revocation-after-the-first-snapshot-refuses-the-descriptor
  (with-fixture
    (fn [{:keys [conn w inspect hydrations]}]
      (let [original reads/current-host-control-db! reads (atom 0)]
        (with-redefs [reads/current-host-control-db!
                      (fn [host]
                        ;; capture-operation validates its original owner once;
                        ;; the core then takes its first and final snapshots.
                        (when (= 3 (swap! reads inc))
                          (d/transact conn [[:db/retract [:room/id (:room w)] :room/owner [:party/id (:owner w)]]]))
                        (original host))]
          (is (= {:status :none} (inspect))))
        (is (empty? @hydrations))))))

(deftest the-real-mcp-tool-keeps-its-closed-input-and-native-owner-lifetimes
  (with-fixture
    (fn [{:keys [host w room request hydrations]}]
      (let [previous (dops/operations) previous-host @@#'server/embedded-host
            calls (atom 0) proposal (random-uuid)
            principal (fn [party] {:principal/kind :human :principal/id (str "party/" party)
                                   :principal/mcp? true :principal/scopes #{}
                                   :principal/tools #{"simmis_workflow-output-collect"}})
            call (fn [party args]
                   (let [ctx (server/session-context (fn [_]) (surface/selection {:profile :admin}) (principal party))]
                     (rpc/handle-message ctx {:jsonrpc "2.0" :id 1 :method "initialize"
                                              :params {:protocolVersion "2025-06-18" :capabilities {}
                                                       :clientInfo {:name "test" :version "1"}}})
                     (rpc/handle-message ctx {:jsonrpc "2.0" :method "notifications/initialized"})
                     (rpc/handle-message ctx {:jsonrpc "2.0" :id 2 :method "tools/call"
                                              :params {:name "simmis_workflow-output-collect" :arguments args}})))
            args {:room (str (:room w)) :id (str request)}]
        (try
          (work-api/install!) (server/set-host! host)
          (with-redefs [publish/file-completed-request-unannounced!
                        (fn [_ _ _ _ actual]
                          (lifetime/check-current!)
                          (is (= request actual))
                          (swap! calls inc)
                          (is (seq (get @(:meta room) preparation/leases-key)))
                          {:kind :proposed :request request :proposal proposal})
                        publish/announce-filed-original!
                        (fn [_ _ original value]
                          (lifetime/check-current!)
                          (is (identical? room (:room original)))
                          (is (empty? (get @(:meta room) preparation/leases-key)))
                          value)]
            (let [result (call (:owner w) args)]
              (is (not (:error result)) (pr-str result))
              (is (not (get-in result [:result :isError])) (pr-str result))
              (is (= 1 @calls)))
            (reset! hydrations [])
            (doseq [party [(:member w) (:non-member w) (:admin w)]]
              (let [result (call party args)]
                (is (or (:error result) (get-in result [:result :isError])))))
            (let [result (call (:owner w) (assoc args :attempt (str (random-uuid))))]
              (is (or (:error result) (get-in result [:result :isError]))))
            (is (= 1 @calls)) (is (empty? @hydrations)))
          (finally
            (doseq [op (keys work-api/specifications)]
              (if-let [spec (get previous op)] (dops/register-op! op spec) (dops/unregister-op! op)))
            (server/sync-op-tools!) (server/set-host! previous-host)))))))
