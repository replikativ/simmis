(ns is.simm.uis.web.desktop.room-digest-remote-test
  (:require [clojure.test :refer [deftest is]]
            [is.simm.model.work-requests :as requests]
            [is.simm.ops.room-digest :as digest]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.uis.web.desktop.room-digest-remote :as remote]))

(deftest queued-digest-work-keeps-original-application-and-human
  (let [party (random-uuid) room (str (random-uuid)) calls (atom []) captured (Object.)]
    (with-redefs [application/capture-operation (fn [] (fn [f] (f captured)))
                  digest/preview! (fn [host principal handle request]
                                    (swap! calls conj [host principal handle request]) {:status :preview})]
      (let [body (remote/prepare-operation party room :preview {:paths ["inbox/a.md"]})]
        (is (empty? @calls))
        (with-redefs [application/capture-operation (fn [] (throw (AssertionError. "Adopted replacement application")))]
          (is (= {:status :preview} (body))))
        (is (= [[captured {:principal/kind :human :principal/id (str "party/" party)
                          :principal/actor (keyword "party" (str party)) :principal/scopes #{}}
                  room {:paths ["inbox/a.md"]}]] @calls))))))

(deftest queued-launch-preserves-the-authenticated-human-actor
  (let [party (random-uuid) room (random-uuid) client (random-uuid)
        request (requests/request-id party room client) captured (Object.)
        submitted (atom nil)]
    (with-redefs [application/capture-operation (fn [] (fn [f] (f captured)))
                  digest/launch! (fn [host principal handle input]
                                   (reset! submitted [host principal handle input])
                                   {:id request})]
      (let [body (remote/prepare-operation party (str room) :launch {:request-id client})]
        (is (nil? @submitted) "Admission waits for the captured worker body")
        (is (= {:status :submitted :request request} (body)))
        (let [[host principal handle input] @submitted]
          (is (identical? captured host))
          (is (= (str room) handle))
          (is (= {:request-id client} input))
          (is (= :human (:principal/kind principal)))
          (is (= (str "party/" party) (:principal/id principal)))
          (is (= (keyword "party" (str party)) (:principal/actor principal))
              "Dvergr hire! needs the authenticated participant actor as :from")
          (is (= #{} (:principal/scopes principal))))))))

(deftest uncertain-launch-cannot-manufacture-safe-replay
  (let [party (random-uuid) room (random-uuid) client (random-uuid)]
    (with-redefs [application/capture-operation (fn [] (fn [f] (f {})))
                  digest/launch! (fn [& _] (throw (ex-info "Helper says retry" {:status :retry :type :safe}))) ]
      (is (= {:status :inspection-required :request (requests/request-id party room client)}
             ((remote/prepare-operation party (str room) :launch {:request-id client})))))))

(deftest uncertain-publication-never-exposes-a-retry-instruction
  (let [party (random-uuid) room (str (random-uuid))]
    (with-redefs [application/capture-operation (fn [] (fn [f] (f {})))
                  is.simm.ops.digest-notification/publish!
                  (fn [& _] (throw (ex-info "Lost ACK" {:status :retry})))]
      (is (= {:status :inspection-required}
             ((remote/prepare-operation party room :publish {:request-id (random-uuid) :digest-id (random-uuid)})))))))
