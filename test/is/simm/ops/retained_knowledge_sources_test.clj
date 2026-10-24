(ns is.simm.ops.retained-knowledge-sources-test
  "Real immutable request/Proposal/artifacts; isolated application lifecycle adapter."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [geschichte.repo :as repo]
            [is.simm.authority.world :as world]
            [is.simm.documents.markdown :as markdown]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.knowledge-publish-test :as fixture]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.retained-knowledge-sources :as sources]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.context :as runtime-context]
            [is.simm.uis.web.desktop.proposals-remote :as remote]
            [konserve.core :as k]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec])
  (:import [java.util Base64]))

(defn- with-sources [f]
  (@#'fixture/with-fixture
   {:retain-source-origin? true :evidence-text "A🦋 proposal\r\n"}
   (fn [{:keys [conn party room-id principal room request] :as state}]
     (d/transact conn [{:db/id [:party/id party] :actor/kind :human}
                       {:db/id [:room/id room-id] :room/slug (str "sources-" room-id)}])
     (let [filing (publish/file-completed-request! conn principal room-id room request)
           owner-ctx (context/create-execution-context)
           host {:execution-ctx owner-ctx
                 ::reads/owner {:control-conn conn :execution-ctx owner-ctx}}
           active (atom false) operations (atom 0)
           invoke (fn [f]
                    (swap! operations inc)
                    (reset! active true)
                    (try (f host) (finally (reset! active false))))]
       (try
         (with-redefs [runtime-context/server-context owner-ctx
                       application/with-operation! invoke
                       application/capture-operation (fn [] invoke)]
           (f (assoc state :proposal (:proposal filing) :component (:manifest filing)
                     :active active :operations operations :host host :owner-ctx owner-ctx)))
         (finally (context/close-context! owner-ctx)))))))

(deftest inbound-source-rpcs-use-the-authenticated-room-reader
  (with-sources
    (fn [{:keys [conn party room-id proposal component]}]
      (let [args {:proposal-id-str (str proposal) :component-id-str (str component)}
            principal {:sub (str party)}
            listing #(world/inbound! % "list-proposal-sources!" args)
            chunk #(world/inbound! principal "read-proposal-source!" (assoc args :options %))
            opts {:path "/in/knowledge/sources/inbox/evidence.md" :length 1}]
        (is (world/refused? (listing nil)))
        (is (world/refused? (listing {:sub (str (random-uuid))})))
        (is (= :ok (get-in (listing principal) [:result :status])))
        (is (= :ok (get-in (chunk opts) [:result :status])))
        (is (= :unavailable (get-in (chunk (assoc opts :artifact (random-uuid))) [:result :status])))
        (is (= :unavailable
               (get-in (world/inbound! principal "list-proposal-sources!"
                                       (assoc args :component-id-str "invalid")) [:result :status])))
        (d/transact conn [[:db/retract [:room/id room-id] :room/owner [:party/id party]]
                          [:db/retract [:room/id room-id] :room/parties [:party/id party]]])
        (is (world/refused? (chunk (assoc opts :offset 1))))))))

(deftest retained-citations-do-not-follow-live-files-or-proposal-resolution
  (with-sources
    (fn [{:keys [conn workspace party request proposal component active operations]}]
      (let [original (sources/list! party proposal component)
            listing (:data original)
            path "/in/knowledge/sources/inbox/evidence.md"
            size (:size (first (filter #(= path (:path %)) (:files listing))))]
        (is (= :ok (:status original)))
        (is (= (str request) (:request listing)))
        (is (= #{path "/in/knowledge/sources/wiki/existing.md"}
               (set (map :path (:files listing)))))
        (repo/write! workspace "inbox/evidence.md" (markdown/source-bytes "Different current file"))
        (d/transact conn [{:db/id [:proposal/id proposal] :proposal/status :dismissed}])
        (is (= original (sources/list! party proposal component)))
        (let [chunks (mapv #(sources/read! party proposal component {:path path :offset % :length 3})
                           (range 0 size 3))
              bytes (byte-array (mapcat #(seq (.decode (Base64/getDecoder) ^String (get-in % [:data :base64])))
                                        chunks))]
          (is (every? #(= :ok (:status %)) chunks))
          (is (= "A🦋 proposal\r\n" (markdown/decode bytes)))
          (is (true? (get-in (last chunks) [:data :eof?])))
          (is (every? #(= (str request) (get-in % [:data :request])) chunks)))
        (is (false? @active))
        (is (> @operations 2))))))

(deftest denial-and-invalid-chunks-never-reach-artifact-storage
  (with-sources
    (fn [{:keys [party proposal component]}]
      (with-redefs [artifact/get-bounded-value
                    (fn [& _]
                      (is false "Denied reader reached artifact storage")
                      (throw (AssertionError. "Denied reader reached artifact storage")))
                    artifact/get-value
                    (fn [& _]
                      (is false "Used unbounded artifact reader")
                      (throw (AssertionError. "Used unbounded artifact reader")))]
        (doseq [reader [nil (random-uuid)]]
          (is (= {:status :unavailable} (sources/list! reader proposal component))))
        (is (= {:status :unavailable} (sources/list! party (random-uuid) component)))
        (doseq [opts [{:path "x" :offset -1} {:path "x" :length 16385}
                      {:path "x" :length 0} {:path "x" :artifact (random-uuid)}]]
          (is (= {:status :unavailable} (sources/read! party proposal component opts))))))))

(deftest only-listed-frozen-sources-and-exact-canonical-request-are-readable
  (with-sources
    (fn [{:keys [conn party request proposal component active]}]
      (let [bounded artifact/get-bounded-value calls (atom [])]
        (with-redefs [artifact/get-value
                      (fn [& _]
                        (is false "Used unbounded artifact reader")
                        (throw (AssertionError. "Used unbounded artifact reader")))
                      artifact/get-bounded-value
                      (fn [& args]
                        (is (true? @active) "Host lifetime includes the physical artifact read")
                        (swap! calls conj (last args))
                        (apply bounded args))]
          (is (= :ok (:status (sources/list! party proposal component))))
          (doseq [path ["/in/knowledge/manifest.edn" "/workflows/memory/task.md"
                        "inbox/evidence.md" "/out/wiki/existing.md"]]
            (is (= {:status :unavailable} (sources/read! party proposal component {:path path}))))
          (is (every? #(= sources/max-payload-bytes %) @calls))
          (let [row (requests/get-request conn request)]
            (d/transact conn [{:db/id (:db/id row) :work.request/intent-id (random-uuid)}])
            (is (= {:status :unavailable} (sources/list! party proposal component)))))))))

(deftest replacing-global-connection-cannot-redirect-an-admitted-source-read
  (with-sources
    (fn [{:keys [conn workspace party proposal component owner-ctx]}]
      (let [current reads/current-host-control-db!
            bounded artifact/get-bounded-value replaced? (atom false)
            ambient (context/create-execution-context)]
        (try
          (with-redefs [system-db/get-conn #(if @replaced? workspace conn)
                        reads/current-host-control-db!
                        (fn [host]
                          (let [db (current host)] (reset! replaced? true) db))
                        artifact/get-bounded-value
                        (fn [store & args]
                          (is (identical? conn (:conn store)) "Never adopt the replacement connection")
                          (is (identical? owner-ctx ec/*execution-context*) "Never use ambient fork ownership")
                          (apply bounded store args))]
            (binding [ec/*execution-context* ambient]
              (is (= {:status :unavailable} (sources/list! party proposal component))
                  "Replacement during artifact access retires the original host before publication")
              (is (= {:status :unavailable} (sources/list! party proposal component))
                  "The next call refuses the stale original owner before artifact access")))
          (finally (context/close-context! ambient)))))))

(deftest each-chunk-rechecks-current-room-permission
  (with-sources
    (fn [{:keys [conn party room-id proposal component]}]
      (let [opts {:path "/in/knowledge/sources/inbox/evidence.md" :length 1}]
        (is (= :ok (:status (sources/read! party proposal component opts))))
        (d/transact conn [[:db/retract [:room/id room-id] :room/owner [:party/id party]]
                          [:db/retract [:room/id room-id] :room/parties [:party/id party]]])
        (with-redefs [artifact/get-bounded-value
                      (fn [& _]
                        (is false "Revoked reader reached artifact storage")
                        (throw (AssertionError. "Denied")))]
          (is (= {:status :unavailable} (sources/read! party proposal component (assoc opts :offset 1)))))))))

(deftest actual-byte-cap-and-corrupt-artifact-fail-with-no-unbounded-fallback
  (with-sources
    (fn [{:keys [conn party request proposal component]}]
      (with-redefs [artifact/get-value
                    (fn [& _]
                      (is false "Used unbounded fallback")
                      (throw (AssertionError. "Unbounded")))]
        (with-redefs [sources/max-payload-bytes 1]
          (is (= {:status :unavailable} (sources/list! party proposal component))))
        (let [ref (:work.request/payload-ref (requests/get-request conn request))]
          ;; Exact private fixture artifact, never a live user object.
          (k/bassoc (:store @conn) ref (markdown/source-bytes "corrupt") {:sync? true})
          (is (= {:status :unavailable} (sources/list! party proposal component))))))))

(deftest wrapped-interruption-keeps-the-native-thread-interrupted
  (with-sources
    (fn [{:keys [party proposal component]}]
      (Thread/interrupted)
      (try
        (with-redefs [artifact/get-bounded-value
                      (fn [& _] (throw (ex-info "wrapped" {} (InterruptedException. "fixture"))))]
          (let [result (sources/list! party proposal component)
                interrupted? (.isInterrupted (Thread/currentThread))]
            (is (= {:status :unavailable} result))
            (is interrupted?)))
        (finally (Thread/interrupted))))))

(deftest queued-rpcs-use-the-original-captured-app-without-another-global-lookup
  (with-sources
   (fn [{:keys [party proposal component host]}]
     (let [body (remote/prepare-source-operation party :list (str proposal) (str component) nil)]
       (with-redefs [application/with-operation!
                     (fn [& _] (throw (AssertionError. "Queued read reacquired the global App")))]
         (is (= :ok (:status (body)))))
       (let [opts {:path "/in/knowledge/sources/inbox/evidence.md" :length 1}
             body (remote/prepare-source-operation party :read (str proposal) (str component) opts)]
         (with-redefs [application/with-operation!
                       (fn [& _] (throw (AssertionError. "Queued chunk reacquired the global App")))]
           (is (= :ok (:status (body))))))))))

(deftest permission-revoked-during-artifact-read-prevents-publication
  (with-sources
   (fn [{:keys [conn party room-id proposal component]}]
     (let [read artifact/get-bounded-value]
       (with-redefs [artifact/get-bounded-value
                     (fn [& args]
                       (let [result (apply read args)]
                         (d/transact conn [[:db/retract [:room/id room-id] :room/owner [:party/id party]]
                                           [:db/retract [:room/id room-id] :room/parties [:party/id party]]])
                         result))]
         (is (= {:status :unavailable} (sources/list! party proposal component))))))))
