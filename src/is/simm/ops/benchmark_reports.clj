(ns is.simm.ops.benchmark-reports
  "Explicit HOST report preparation/publication. Caller-carried recovery only;
   no execution, background publication, selected workspace or durable intent."
  (:require [datahike.api :as d]
            [dvergr.agent.experiment :as experiment]
            [dvergr.ops :as native]
            [dvergr.room.store.datahike]
            [dvergr.runtime.preparation :as preparation]
            [hasch.core :as hasch]
            [is.simm.documents.benchmark-reports :as reports]
            [is.simm.model.system-db :as system-db]
            [is.simm.ops.owned-files :as files]
            [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-room-operation :as operation]
            [is.simm.runtimes.operation-lifetime :as lifetime]
            [malli.core :as m]
            [org.replikativ.spindel.engine.core :as ec]))

(def prepare-schema [:map {:closed true} [:scorecard :uuid]])
(def publish-schema
  [:map {:closed true} [:scorecard :uuid] [:expected-target-stamp :uuid] [:expected-plan :uuid]])

(defn- refuse! [reason]
  (throw (ex-info "Benchmark report unavailable" {:type ::refused :reason reason})))

(defn- receipt-context [room room-id]
  (let [store (:store room)
        _ (when-not (instance? dvergr.room.store.datahike.DatahikeStore store)
            (refuse! :unsupported-receipt-store))
        store-id (binding [ec/*execution-context* (:ctx room)]
                   (get-in @(:conn store) [:config :store :id]))]
    (when-not (uuid? store-id) (refuse! :invalid-receipt-store))
    {:room room-id :store store-id}))

(defn- validate-receipt-context! [control-db context]
  (when-not (= (:store context)
               (:room/content-db-scope (d/entity control-db [:room/id (:room context)])))
    (refuse! :receipt-store-changed)))

(defn- plan [room room-id control-db scorecard-id stamp]
  (let [context (receipt-context room room-id)]
    (validate-receipt-context! control-db context)
    (let [scorecard (binding [ec/*execution-context* (:ctx room)]
                      (experiment/scorecard-bounded room scorecard-id))]
      (when-not scorecard (refuse! :scorecard-not-found))
      (let [source (reports/prepare context (native/scorecard-summary scorecard) (:scorecard/experiment scorecard))]
        (assoc source :target-stamp stamp
               :plan (hasch/uuid [::plan reports/version stamp source]))))))

(defn- check-source! [host principal handle original prepared]
  (let [context (select-keys (:source prepared) [:room :store])]
    (when-not (= context (receipt-context (:room original) (:room context)))
      (refuse! :receipt-store-changed))
    ;; Store observations precede this current control snapshot, just as for
    ;; the existing owned saved-view reader. A prior ACL cannot span body IO.
    (let [control-db (reads/current-host-control-db! host)]
      (reads/validate-host-write-room-in! host principal handle original control-db)
      (validate-receipt-context! control-db context))))

(defn- with-source! [host principal handle request schema body]
  ;; Validate before acquiring any native Room or reading receipt/file bodies.
  (when-not (m/validate schema request) (refuse! :invalid-request))
  (operation/with-app!
   host
   (fn []
     (let [original (reads/resolve-host-write-room! host principal handle)
           room (:room original)]
       (binding [ec/*execution-context* (:execution-ctx host)]
         (preparation/with-host-operation!
          room
          (fn [lease]
            (let [check! #(do (lifetime/check-current!)
                             (reads/validate-host-write-room! host principal handle original)
                             (lifetime/check-current!)
                             (preparation/with-host-current! lease (constantly true)))
                  _ (check!)
                  bound (reads/resolve-host-write! host principal handle)
                  _ (when-not (identical? room (:room bound)) (refuse! :room-changed))
                  stamp (:target-stamp bound)
                  _ (when (and (contains? request :expected-target-stamp)
                               (not= stamp (:expected-target-stamp request)))
                      (refuse! :target-changed))
                  prepared (plan room (:room-id bound) (reads/current-host-control-db! host)
                                 (:scorecard request) stamp)
                  check-source! #(do (check!) (check-source! host principal handle original prepared) (check!))]
              (check!)
              (reads/validate-host-write! host principal handle bound)
              (check-source!)
              (let [result (body bound prepared check-source!)]
                (reads/validate-host-write! host principal handle bound)
                (check-source!)
                result)))))))))

(defn prepare!
  "Prepare exact source and digest for inspection before explicit publication.
   The caller retains this original digest and target for uncertain retries."
  [host principal handle request]
  (with-source! host principal handle request prepare-schema (fn [_ prepared _] prepared)))

(defn- publish-file! [principal bound prepared check! {:keys [path text]} index?]
  (check!)
  (let [conn (system-db/get-conn) room (:room bound) room-id (:room-id bound)
        stamp (:target-stamp prepared)
        metadata (files/metadata! conn principal room-id room {:folders [] :paths [path]})]
    (when-not (= stamp (:target-stamp metadata)) (refuse! :target-changed))
    (if (get-in metadata [:entries path])
      (let [existing (files/read! conn principal room-id room {:path path :expected-target-stamp stamp})]
        (cond
          (= text (:text existing)) {:path path :status :existing}
          index? {:path path :status :preserved}
          :else (refuse! :authored-report-conflict)))
      ;; An intervening creation fails the ordinary writer's absent-entry CAS.
      ;; A retry reconciles exact bytes; this call never overwrites the winner.
      (do
        (check!)
        (files/save! conn principal room-id room
                     {:path path :text text :expected nil :expected-target-stamp stamp})
        {:path path :status :created}))))

(defn publish!
  "Publish ONLY the prepared source in its original owned target. Two ordinary
   file writes, report first; no atomic bundle or server restart recovery.
   A different authored index is preserved and explicitly reported. Lost ACKs
   require the SAME target/digest; changed formatting refuses before a write."
  [host principal handle {:keys [expected-plan] :as request}]
  (with-source!
   host principal handle request publish-schema
   (fn [bound prepared check!]
     (when-not (= expected-plan (:plan prepared)) (refuse! :plan-changed))
     (let [report (publish-file! principal bound prepared check! (:report prepared) false)
           index (publish-file! principal bound prepared check! (:index prepared) true)]
       {:source (:source prepared) :target-stamp (:target-stamp prepared) :plan expected-plan
        :report report :index index :complete? (not= :preserved (:status index))}))))
