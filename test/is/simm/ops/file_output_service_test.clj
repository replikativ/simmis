(ns is.simm.ops.file-output-service-test
  "Mocked service ordering/DTO tests; not native lifetime or authority evidence."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.file-output-adoption :as adoption]
            [is.simm.ops.file-output-controller :as controller]
            [is.simm.ops.file-output-service :as service]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.runtimes.application-ops :as application]))

(defn- with-service [options f]
  (let [party (random-uuid) proposal (random-uuid) room-id (random-uuid)
        revision (random-uuid) operation (random-uuid) room (Object.) conn (atom {})
        host {:execution-ctx (Object.)} app-held (atom false) room-held (atom false)
        calls (atom []) note! #(swap! calls conj %)
        action! (fn [action]
                  (is @app-held) (is @room-held) (note! action)
                  (when-let [error (:error options)] (throw error))
                  {:status (if (= action :preview) :reviewed :accepted)
                   :proposal proposal :revision revision
                   ;; Actual fresh finalization returns the journal map; it is
                   ;; not itself safe DTO data. Recovery returns operation-id.
                   :operation {:id operation :token (random-uuid) :actor party}
                   :token (random-uuid) :witness {:secret true} :conn conn})]
    (with-redefs [application/with-operation!
                  (fn [body]
                    (note! :app-enter)
                    (when (:stopped? options) (throw (ex-info "Stopped" {})))
                    (reset! app-held true)
                    (try (body host) (finally (reset! app-held false) (note! :app-exit))))
                  system-db/get-conn (fn [] (is @app-held) conn)
                  reads/current-host-control-db! (fn [actual] (is @app-held) (is (identical? host actual)) @conn)
                  d/pull (fn [_ _ lookup]
                           (is @app-held) (is (= [:proposal/id proposal] lookup))
                           (note! :metadata) {:proposal/room room-id})
                  reads/authorize-read! (fn [_ principal handle]
                                          (is @app-held)
                                          (is (= {:principal/kind :human :principal/id (str "party/" party)} principal))
                                          (is (= (str room-id) handle)) (note! :authorize-human))
                  adoption/snapshot (fn [& _]
                                      (is @app-held) (note! :closed-header)
                                      (when (or (:denied? options) (:accepted? options))
                                        (throw (ex-info "Closed or denied" {})))
                                      {})
                  adoption/reconciliation-snapshot (fn [& _] (is @app-held) (note! :historical-header) {})
                  reads/resolve-host-room! (fn [& _] (is @app-held) (note! :hydrate) {:room room})
                  reads/validate-host-room! (fn [& _] (is @room-held) (note! :validate-room))
                  preparation/with-host-operation! (fn [actual body]
                                                     (is @app-held) (is (identical? room actual))
                                                     (note! :room-enter) (reset! room-held true)
                                                     (try (body :lease)
                                                          (finally (reset! room-held false) (note! :room-exit))))
                  preparation/with-host-current! (fn [_ body] (is @room-held) (body))
                  controller/preview-reviewed! (fn [& _] (action! :preview))
                  controller/adopt-reviewed! (fn [_ _ _ _ _ actual-revision]
                                               (is (= revision actual-revision)) (action! :adopt))
                  adoption/recover! (fn [& _] (action! :reconcile))]
      (f {:party party :proposal proposal :revision revision :operation operation :calls calls
          :preview! #(service/preview! party proposal)
          :adopt! #(service/adopt! party proposal %)
          :reconcile! #(service/reconcile! party proposal)}))))

(deftest authority-and-shape-before-hydration-and-native-ownership-before-action
  (with-service {}
    (fn [{:keys [preview! calls proposal revision operation]}]
      (is (= {:status :reviewed :proposal (str proposal) :revision (str revision)
              :operation (str operation)} (preview!)))
      (is (= [:app-enter :metadata :authorize-human :closed-header :hydrate
              :room-enter :metadata :authorize-human :closed-header :validate-room
              :preview :room-exit :app-exit] @calls)))))

(deftest denied-or-mixed-proposal-cannot-hydrate
  (with-service {:denied? true}
    (fn [{:keys [preview! calls]}]
      (is (= {:status :unavailable} (preview!)))
      (is (not-any? #{:hydrate :room-enter :preview} @calls)))))

(deftest stopped-application-is-not-even-a-metadata-reader
  (with-service {:stopped? true}
    (fn [{:keys [preview! calls]}]
      (is (= {:status :unavailable} (preview!)))
      (is (= [:app-enter] @calls)))))

(deftest required-review-and-explicit-adoption
  (with-service {}
    (fn [{:keys [adopt! calls revision]}]
      (doseq [invalid [nil false "arbitrary"]]
        (is (= {:status :unavailable} (adopt! invalid))))
      (is (empty? @calls))
      (is (= :accepted (:status (adopt! revision))))
      (is (= 1 (count (filter #{:adopt} @calls)))))))

(deftest reconciliation-uses-historical-not-open-only-authorization
  (with-service {:accepted? true}
    (fn [{:keys [reconcile! calls]}]
      (is (= :accepted (:status (reconcile!))))
      (is (= 2 (count (filter #{:historical-header} @calls))))
      (is (not-any? #{:closed-header :preview :adopt} @calls)))))

(deftest errors-are-closed-dtos-and-uncertainty-is-not-replay-permission
  (let [operation (random-uuid)]
    (doseq [[error expected]
            [[(ex-info "Secret" {:type ::controller/unavailable :reason :review-changed :secret "body"})
              {:status :review-changed}]
             [(ex-info "Secret" {:type ::adoption/claim-unacknowledged :operation operation :token (random-uuid)})
              {:status :pending :operation (str operation)}]
             [(ex-info "Untrusted arbitrary publication" {:publication {:applied? true} :secret "body"})
              {:status :unavailable}]]]
      (with-service {:error error}
        (fn [{:keys [preview! calls]}]
          (is (= expected (preview!)))
          (is (= [:room-exit :app-exit] (take-last 2 @calls))))))))
