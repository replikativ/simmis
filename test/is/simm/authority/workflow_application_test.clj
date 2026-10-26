(ns is.simm.authority.workflow-application-test
  (:require [clojure.test :refer [deftest is]]
            [dvergr.mcp.server :as server]
            [dvergr.ops :as dops]
            [geschichte.repo :as repo]
            [is.simm.authority.application-fixture :as application-fixture]
            [is.simm.model.access :as access]
            [is.simm.ops.dvergr :as principals]
            [is.simm.ops.owned-mcp :as owned-mcp]
            [is.simm.ops.owned-mcp-test :as fixture]
            [is.simm.ops.owned-workflow-host :as workflow-host]
            [is.simm.ops.owned-workflows :as workflows]
            [is.simm.ops.work-api :as api]
            [is.simm.ops.workflows-test :as source]
            [is.simm.runtimes.application-ops :as application]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [org.replikativ.spindel.blocking :as blocking]
            [org.replikativ.spindel.engine.impl.simple :as simple]))

(defn- with-app [f]
  (@#'fixture/with-fixture true
   (fn [{:keys [conn room owned] :as data}]
     (doseq [[path text] source/fixture]
       (repo/write! owned (str "workflows/echo/" path) (.getBytes ^String text "UTF-8")))
     (application-fixture/with-application
      conn (:ctx room)
      (fn [app]
        (let [previous (dops/operations)]
          (try (api/install!) (f (merge data app))
               (finally
                 (doseq [op (keys api/specifications)]
                   (if-let [spec (get previous op)] (dops/register-op! op spec) (dops/unregister-op! op)))
                 (server/sync-op-tools!)))))))))

(deftest browser-worker-retains-actual-thread-and-original-human-without-mcp-identity
  (with-app
    (fn [{:keys [w app-owner]}]
      (let [caller (atom (:owner w)) queued (atom nil) seen (atom [])
            caller-thread (Thread/currentThread) list! workflows/list!]
        (with-redefs [access/authenticated-party-id #(deref caller)
                      blocking/blocking (fn [f] (reset! queued f) :queued)
                      workflows/list!
                      (fn [host principal handle args]
                        (lifetime/check-current!)
                        (swap! seen conj {:thread (Thread/currentThread) :ticket-thread (:thread lifetime/*ticket*)
                                          :owner (:owner lifetime/*ticket*) :principal principal})
                        (list! host principal handle args))]
          (is (= :queued
                 (with-redefs [simple/on-drain-thread? (constantly true)]
                   (@#'remote/workflow-on-worker (str (:room w)) :simmis/workflow-list {}))))
          (is (empty? @seen))
          (reset! caller (:non-member w))
          (let [reply (deref (future (@queued)) 15000 :timeout)
                observed (first @seen)]
            (is (= ["echo"] (:names reply)))
            (is (= 1 (count @seen)))
            (is (not (identical? caller-thread (:thread observed))))
            (is (identical? (:thread observed) (:ticket-thread observed)))
            (is (identical? app-owner (:owner observed)))
            (is (= (principals/party-principal (:owner w)) (:principal observed)))
            (is (nil? (get-in observed [:principal :principal/mcp?])))))))))

(deftest queued-browser-work-cannot-adopt-replacement-app-even-with-the-same-database
  (with-app
    (fn [{:keys [conn room w app-owner installation-slot]}]
      (let [queued (atom nil) calls (atom 0) list! workflows/list! replacement (lifetime/create)]
        (lifetime/open! replacement)
        (try
          (with-redefs [access/authenticated-party-id (constantly (:owner w))
                        blocking/blocking (fn [f] (reset! queued f) :queued)
                        workflows/list! (fn [& args] (swap! calls inc) (apply list! args))]
            (is (= :queued
                   (with-redefs [simple/on-drain-thread? (constantly true)]
                     (@#'remote/workflow-on-worker (str (:room w)) :simmis/workflow-list {}))))
            (lifetime/fence! app-owner)
            (reset! installation-slot {:owner replacement :host (owned-mcp/host conn (:ctx room) replacement)})
            (is (= :not-ready
                   (deref (future (try (@queued) :unexpected
                                       (catch clojure.lang.ExceptionInfo error (:reason (ex-data error))))) 15000 :timeout)))
            (is (zero? @calls))
            (is (= ["echo"] (:names (remote/invoke-workflow (:owner w) (str (:room w)) :simmis/workflow-list {}))))
            (is (= 1 @calls)))
          (finally (lifetime/fence! replacement) (lifetime/join! replacement 5000)))))))

(deftest normal-human-host-does-not-borrow-mcp-transport-authorization
  (with-app
    (fn [{:keys [w app-host]}]
      (let [principal (principals/party-principal (:owner w))
            request {:room (str (:room w))}]
        (is (thrown? Exception (workflow-host/from-application app-host)))
        ((application/capture-operation)
         (fn [original]
           (let [host (workflow-host/from-application original)]
             (is (thrown? Exception (dops/authorize! original principal :simmis/workflow-list request)))
             (is (= :allowed (try (dops/authorize! host principal :simmis/workflow-list request) :allowed
                                 (catch Exception _ :denied))))
             (is (identical? (get-in original [:config :host-lifecycle :with-operation])
                             (get-in host [:config :host-lifecycle :with-operation]))))))))))
