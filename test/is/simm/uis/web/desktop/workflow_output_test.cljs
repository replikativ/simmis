(ns is.simm.uis.web.desktop.workflow-output-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [is.simm.uis.web.desktop.workflow-output :as output]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn- with-client [f]
  (binding [ec/*execution-context* runtime]
    (let [session (atom {:id (random-uuid)}) room (str (random-uuid)) job (str (random-uuid))
          request (str (random-uuid)) proposal (str (random-uuid))
          calls (atom {}) opened (atom [])
          socket (signal runtime {:socket (random-uuid)})
          states (signal runtime {}) roster (signal runtime {:rooms [{:room/id room}]})
          hints (signal runtime {:control-dirty 0})
          deferred (fn [kind]
                     (fn [& args]
                       (fn [ok error] (swap! calls update kind (fnil conj []) {:args args :ok ok :error error}))))]
      (with-redefs [web/current-session #(deref session)
                    web/current-session? #(= (:id %) (:id @session))
                    connection/connection socket sig/workflow-output-reviews states
                    sig/workflow-editor-state (signal runtime {}) sig/user-rooms roster sync/state hints
                    remote/inspect-run-wiki-output! (deferred :inspect)
                    remote/collect-workflow-wiki-output! (deferred :collect)
                    refs/open! (fn ([reference] (swap! opened conj reference))
                                 ([reference _options] (swap! opened conj reference)))]
        (let [a (output/controller room {:id job :kind :workflow :status :completed})
              eligible {:status :eligible :request request :job job}]
          (f {:a a :room room :job job :request request :proposal proposal :eligible eligible
              :calls calls :opened opened :socket socket :hints hints :roster roster :session session
              :last-call #(last (get @calls %))}))))))

(deftest a-fresh-root-inspector-collects-the-canonical-request-without-a-submission-form
  (with-client
    (fn [{:keys [a request proposal eligible calls opened last-call]}]
      ((:ensure a)) ((:ensure a))
      (is (= 1 (count (:inspect @calls))) "Rendering does not repeat one current observation")
      ((:ok (last-call :inspect)) eligible)
      (is ((:current? a)))
      (is (empty? @sig/workflow-editor-state) "No fake authoring submission is installed")
      ((:collect a))
      (is (= request (last (:args (last-call :collect)))))
      (let [reply (:ok (last-call :collect))]
        (reply {:status :proposed :request request :proposal proposal})
        (reply {:status :proposed :request request :proposal (str (random-uuid))})
        (is (= proposal (get-in ((:state a)) [:collection :proposal]))))
      ((:open a))
      (is (= [{:kind :proposal :id proposal}] @opened)))))

(deftest metadata-coalesces-and-does-not-erase-an-acknowledged-publication
  (with-client
    (fn [{:keys [a request proposal eligible calls hints opened last-call]}]
      ((:ensure a))
      (let [old (:ok (last-call :inspect))]
        (swap! hints assoc :control-dirty 1)
        ((:ensure a))
        (is (= 1 (count (:inspect @calls))))
        (old eligible)
        (is (nil? (:inspection ((:state a)))))
        ((:ensure a))
        (is (= 2 (count (:inspect @calls)))))
      ((:ok (last-call :inspect)) eligible)
      ((:collect a))
      (swap! hints assoc :control-dirty 2)
      ((:ensure a))
      ((:ok (last-call :collect)) {:status :proposed :request request :proposal proposal})
      (is (= proposal (get-in ((:state a)) [:collection :proposal])))
      ((:open a))
      (is (empty? @opened) "A stale descriptor is not refreshed by a write receipt")
      ((:ok (last-call :inspect)) eligible)
      ((:open a))
      (is (= [{:kind :proposal :id proposal}] @opened)))))

(deftest reconnect-drops-native-ownership-without-replaying-collection
  (with-client
    (fn [{:keys [a request proposal eligible calls socket last-call]}]
      ((:ensure a)) ((:ok (last-call :inspect)) eligible) ((:collect a))
      (let [old (:ok (last-call :collect))]
        (reset! socket {:socket (random-uuid)})
        ((:ensure a))
        (is (not ((:current? a))) "Reconnect must await a fresh inspection acknowledgement")
        ((:collect a)) ((:open a))
        (is (= 1 (count (:collect @calls))) "A prior descriptor cannot admit another collection")
        (old {:status :proposed :request request :proposal proposal})
        (is (nil? (:collection ((:state a)))))
        (is (= 1 (count (:collect @calls))) "Only a new explicit review may retry a mutation")
        (is (nil? (get-in ((:state a)) [:observations :collect :active])))))))

(deftest account-and-roster-changes-fence-actions-before-render-resumes
  (with-client
    (fn [{:keys [a eligible calls roster last-call]}]
      ((:ensure a)) ((:ok (last-call :inspect)) eligible)
      (reset! roster {:rooms []})
      ((:collect a))
      (is (empty? (:collect @calls)))
      (is (not ((:current? a))))))
  (with-client
    (fn [{:keys [a request proposal eligible session last-call]}]
      ((:ensure a)) ((:ok (last-call :inspect)) eligible) ((:collect a))
      (let [before ((:state a)) reply (:ok (last-call :collect))]
        (reset! session {:id (random-uuid)})
        (reply {:status :proposed :request request :proposal proposal})
        (is (= before ((:state a))))))))

(deftest malformed-descriptors-and-receipts-never-open-a-proposal
  (with-client
    (fn [{:keys [a eligible opened last-call]}]
      ((:ensure a))
      ((:ok (last-call :inspect)) (assoc eligible :job (str (random-uuid))))
      (is (nil? (:inspection ((:state a)))))
      (is (get-in ((:state a)) [:errors :inspect]))
      ((:retry a)) ((:ensure a)) ((:ok (last-call :inspect)) eligible)
      ((:collect a))
      ((:ok (last-call :collect)) {:status :proposed :request (str (random-uuid)) :proposal (str (random-uuid))})
      (is (nil? (:collection ((:state a)))))
      ((:open a))
      (is (empty? @opened)))))
