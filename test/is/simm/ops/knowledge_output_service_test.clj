(ns is.simm.ops.knowledge-output-service-test
  "Mocked ordering/DTO probes; native ownership and certification have separate tests."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.knowledge-filing :as filing]
            [is.simm.ops.knowledge-publish :as publish]
            [is.simm.ops.knowledge-output-service :as service]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- exercise [options f]
  (let [party (random-uuid) room-id (random-uuid) request (random-uuid) proposal (random-uuid)
        conn (atom {}) room (Object.) ctx (Object.)
        host {:execution-ctx ctx ::reads/owner {:control-conn conn :execution-ctx ctx}}
        calls (atom []) app? (atom false) room? (atom false)
        note! #(swap! calls conj %)
        row (merge {:work.request/id request :work.request/by party :work.request/room room-id
                    :work.request/kind :workflow :work.request/state :submitted
                    :work.request/job (random-uuid) :work.request/publication-basis-id (random-uuid)}
                   (:row options))]
    (with-redefs [application/with-operation!
                  (fn [body]
                    (note! :application)
                    (when (:stopped? options) (throw (ex-info "Stopped" {})))
                    (reset! app? true)
                    (try (body host) (finally (reset! app? false) (note! :application-exit))))
                  lifetime/check-current! (fn [] (is @app?))
                  system-db/get-conn (fn [] (is @app?) conn)
                  reads/current-host-control-db! (fn [h] (is @app?) (is (identical? host h)) @conn)
                  reads/authorize-read! (fn [_ principal handle]
                                          (is @app?) (note! :human)
                                          (is (= {:principal/kind :human :principal/id (str "party/" party)} principal))
                                          (is (= (str room-id) handle)))
                  filing/authorize! (fn [& _]
                                      (note! :authority)
                                      (when (or (:denied? options) (and @room? (:revoked-after-hydration? options)))
                                        (throw (ex-info "Denied" {})))
                                      party)
                  d/pull (fn [_ _ lookup] (note! :request) (is (= [:work.request/id request] lookup)) row)
                  reads/resolve-host-room! (fn [& _] (note! :hydrate) (is @app?) {:room room})
                  preparation/with-host-operation!
                  (fn [r body]
                    (note! :room) (is (identical? room r)) (reset! room? true)
                    (try (body :lease) (finally (reset! room? false) (note! :room-exit))))
                  preparation/with-host-current! (fn [_ body] (is @room?) (body))
                  reads/validate-host-room! (fn [& _] (note! :current-room) (is @room?))
                  publish/file-completed-request!
                  (fn [actual principal actual-room r id]
                    (note! :file) (is @app?) (is @room?)
                    (is (identical? conn actual)) (is (identical? room r))
                    (is (= room-id actual-room)) (is (= request id))
                    (is (= (str "party/" party) (:principal/id principal)))
                    (when (:failure? options) (throw (ex-info "Private native diagnostic" {:body :secret})))
                    (merge {:request request :kind :proposed :proposal proposal
                            :token :secret :body :secret :room room}
                           (:result options)))]
      (f {:party party :room room-id :request request :proposal proposal :calls calls
          :collect! #(service/collect! party room-id request)}))))

(deftest only-existing-owned-workflow-can-enter-native-filing
  (exercise {}
            (fn [{:keys [collect! request proposal calls]}]
              (is (= {:status :proposed :request (str request) :proposal (str proposal)} (collect!)))
              (is (= [:application :human :authority :request :hydrate :room
                      :human :authority :request :current-room :file :room-exit :application-exit]
                     @calls)))))

(deftest no-op-is-explicit-and-never-invents-a-proposal
  (exercise {:result {:kind :no-op :proposal nil}}
            (fn [{:keys [collect! request]}]
              (is (= {:status :no-op :request (str request)} (collect!))))))

(deftest malformed-foreign-unfinished-or-benchmark-request-cannot-hydrate
  (doseq [row [{:work.request/by (random-uuid)} {:work.request/room (random-uuid)}
               {:work.request/kind :benchmark} {:work.request/state :admitting}
               {:work.request/job nil} {:work.request/publication-basis-id nil}]]
    (exercise {:row row}
              (fn [{:keys [collect! calls]}]
                (is (= {:status :unavailable} (collect!)))
                (is (not-any? #{:hydrate :room :file} @calls)))))
  (is (= {:status :unavailable} (service/collect! nil nil nil))))

(deftest stopped-or-revoked-authority-is-not-an-output-reader
  (doseq [options [{:stopped? true} {:denied? true}]]
    (exercise options
              (fn [{:keys [collect! calls]}]
                (is (= {:status :unavailable} (collect!)))
                (is (not-any? #{:request :hydrate :file} @calls))))))

(deftest certification-failure-and-invalid-results-do-not-leak-or-authorize-reexecution
  (doseq [options [{:failure? true} {:result {:request (random-uuid)}}
                   {:result {:kind :proposed :proposal "not-an-id"}} {:result {:kind :pending}}]]
    (exercise options
              (fn [{:keys [collect! calls]}]
                (is (= {:status :unavailable} (collect!)))
                (is (= [:room-exit :application-exit] (take-last 2 @calls)))))))

(deftest revocation-after-hydration-refuses-before-attempt-or-output-reads
  (exercise {:revoked-after-hydration? true}
            (fn [{:keys [collect! calls]}]
              (is (= {:status :unavailable} (collect!)))
              (is (some #{:room} @calls))
              (is (not-any? #{:file} @calls))
              (is (= [:room-exit :application-exit] (take-last 2 @calls))))))
