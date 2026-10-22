(ns is.simm.ops.wiki-setup-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.runtime.preparation :as preparation]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-authoring :as authoring]
            [is.simm.ops.wiki-setup :as setup]
            [is.simm.ops.workflows :as workflows]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.starter.install-wiki :as installer]
            [is.simm.starter.wiki-maintenance :as starter]))

(defn- exercise [options f]
  (let [party (random-uuid) room-id (random-uuid) host {:execution-ctx (Object.)}
        room (Object.) calls (atom []) app? (atom false) held? (atom false)
        note! #(swap! calls conj %)
        authorize! (fn [h p handle]
                     (is @app?) (is (= host (dissoc h ::reads/install-room-binding)))
                     (is (= {:principal/kind :human :principal/id (str "party/" party)} p))
                     (is (= (str room-id) handle)))]
    (with-redefs [application/with-operation!
                  (fn [body]
                    (note! :application)
                    (when (:stopped? options) (throw (ex-info "Stopped" {})))
                    (reset! app? true)
                    (try (body host) (finally (reset! app? false) (note! :app-exit))))
                  reads/preflight-host-install!
                  (fn [& args]
                    (note! :authorize) (apply authorize! args)
                    (when (:denied? options) (throw (ex-info "Denied" {}))))
                  reads/resolve-host-install-room!
                  (fn [& args] (note! :hydrate) (apply authorize! args) {:room room})
                  preparation/with-host-operation!
                  (fn [r body]
                    (is (identical? room r)) (note! :room) (reset! held? true)
                    (try (body :lease) (finally (reset! held? false) (note! :room-exit))))
                  preparation/with-host-current! (fn [_ body] (is @held?) (body))
                  reads/validate-host-install-room!
                  (fn [h p handle _]
                    (note! :validate) (is @held?) (authorize! h p handle)
                    (when (:revoked? options) (throw (ex-info "Revoked" {}))))
                  reads/resolve-host-install!
                  (fn [& args] (note! :target) (is @held?) (apply authorize! args)
                    (is (identical? room (get-in (first args) [::reads/install-room-binding :room])))
                    (when (:legacy? options) (throw (ex-info "Legacy" {})))
                    {:db :canonical})
                  workflows/snapshot-in
                  (fn [db name] (is @held?) (is (= :canonical db)) (is (= starter/name name))
                    (:snapshot options {:files {} :manifest []}))
                  starter/source (constantly {"workflow.edn" "{}"})
                  installer/install!
                  (fn [& args]
                    (note! :install) (is @held?) (apply authorize! args)
                    (is (identical? room (get-in (first args) [::reads/install-room-binding :room])))
                    (when (:pending? options) (throw (ex-info "Unknown" {:type ::authoring/pending})))
                    (merge {:status :installed :private :must-not-escape} (:result options)))]
      (f {:inspect #(setup/inspect! party room-id) :install #(setup/install! party room-id) :calls calls}))))

(deftest inspection-is-a-current-source-observation-under-exact-lifetimes
  (doseq [[snapshot status]
          [[{:files {} :manifest []} :missing]
           [{:files {"workflow.edn" "{}"} :manifest [{:path "workflow.edn" :mode 33188}]} :installed]
           [{:files {"workflow.edn" "custom"} :manifest [{:path "workflow.edn" :mode 33188}]} :customized]]]
    (exercise {:snapshot snapshot}
              (fn [{:keys [inspect calls]}]
                (is (= {:status status :name starter/name :activation :unchanged :observation :current-source}
                       (inspect)))
                (is (= [:application :authorize :hydrate :room :validate :target :room-exit :app-exit] @calls))))))

(deftest explicit-install-returns-only-its-closed-ack
  (doseq [status [:installed :already-installed :customized]]
    (exercise {:result {:status status}}
              (fn [{:keys [install calls]}]
                (is (= {:status (if (= :customized status) :customized :installed)
                        :name starter/name :activation :unchanged :observation :acknowledged}
                       (install)))
                (is (= [:application :authorize :hydrate :room :validate :install :room-exit :app-exit] @calls))))))

(deftest refusal-precedes-hydration-or-target-access
  (doseq [options [{:stopped? true} {:denied? true} {:revoked? true} {:legacy? true}]]
    (exercise options
              (fn [{:keys [inspect calls]}]
                (is (= {:status :unavailable} (inspect)))
                (is (not-any? #{:install} @calls))
                (when (or (:stopped? options) (:denied? options))
                  (is (not-any? #{:hydrate :target} @calls))))))
  (is (= {:status :unavailable} (setup/inspect! nil nil))))

(deftest uncertain-install-is-pending-with-no-internal-retry
  (exercise {:pending? true}
            (fn [{:keys [install calls]}]
              (is (= {:status :pending} (install)))
              (is (= 1 (count (filter #{:install} @calls))))
              (is (= [:room-exit :app-exit] (take-last 2 @calls))))))
