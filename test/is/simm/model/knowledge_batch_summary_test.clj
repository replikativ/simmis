(ns is.simm.model.knowledge-batch-summary-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [is.simm.model.knowledge-batches :as batches]
            [is.simm.model.knowledge-schedule-batches :as scheduled]
            [is.simm.model.knowledge-streams :as streams]
            [is.simm.model.work-requests :as requests]
            [is.simm.model.workflow-schedules :as schedules]))

(defn- fixture [phase f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}
        party (random-uuid) room (random-uuid) aid (random-uuid) oid (random-uuid)
        bid (random-uuid) sid (random-uuid) target (random-uuid) revision (random-uuid)
        rid (requests/request-id party room oid) intent (random-uuid) frozen (random-uuid)
        linked? (not= :ready phase)
        a {:schedule.activation/id aid :schedule.activation/by party :schedule.activation/room room
           :schedule.activation/revision revision :schedule.activation/target-ref (random-uuid)}
        o (cond-> {:schedule.occurrence/id oid :schedule.occurrence/activation aid
                   :schedule.occurrence/revision revision :schedule.occurrence/knowledge-batch bid
                   :schedule.occurrence/batch-ref (random-uuid) :schedule.occurrence/batch-revision (random-uuid)}
            linked? (assoc :schedule.occurrence/request rid))
        b (cond-> {:knowledge.batch/id bid :knowledge.batch/stream sid :knowledge.batch/target target
                   :knowledge.batch/by party :knowledge.batch/room room :knowledge.batch/state phase
                   :knowledge.batch/scheduled-occurrence oid :knowledge.batch/selection-id (random-uuid)
                   :knowledge.batch/frozen-id frozen
                   :knowledge.batch/selection (random-uuid) :knowledge.batch/frozen (random-uuid)}
            linked? (assoc :knowledge.batch/request rid :knowledge.batch/request-intent intent))
        s (cond-> {:knowledge.stream/id sid :knowledge.stream/by party :knowledge.stream/room room
                   :knowledge.stream/target target}
            (not= :closed phase) (assoc :knowledge.stream/active-batch bid))
        r {:work.request/id rid :work.request/client-id oid :work.request/by party :work.request/room room
           :work.request/kind :workflow :work.request/state :submitted :work.request/intent-id intent
           :work.request/knowledge-batch bid :work.request/knowledge-frozen frozen}]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn (concat schedules/schema batches/schema streams/schema requests/schema scheduled/schema))
        (d/transact conn (cond-> [a o b s] linked? (conj r)))
        (f {:conn conn :a a :o o :b b :s s :r r :target target})
        (finally (d/release conn) (d/delete-database cfg))))))

(deftest bounded-phases-are-observations-without-artifact-reads
  (doseq [phase [:ready :submitted :review :closed]]
    (fixture phase
      (fn [{:keys [conn a o b target]}]
        (with-redefs [artifact/get-bounded-value (fn [& _] (throw (AssertionError. "No artifact hydration")))
                      artifact/get-value (fn [& _] (throw (AssertionError. "No artifact hydration")))]
          (is (= {:status :observed :id (str (:knowledge.batch/id b)) :phase phase}
                 (scheduled/occurrence-summary @conn a o target))))))))

(deftest half-links-and-foreign-identities-never-expose-a-batch-id
  (doseq [[attribute value] [[:knowledge.batch/scheduled-occurrence (random-uuid)]
                           [:knowledge.batch/by (random-uuid)] [:knowledge.batch/room (random-uuid)]
                           [:knowledge.batch/target (random-uuid)] [:knowledge.batch/request (random-uuid)]
                           [:knowledge.batch/request-intent (random-uuid)] [:knowledge.batch/frozen-id (random-uuid)]
                           [:work.request/client-id (random-uuid)] [:work.request/by (random-uuid)]
                           [:work.request/room (random-uuid)] [:work.request/knowledge-batch (random-uuid)]
                           [:work.request/knowledge-frozen (random-uuid)] [:work.request/state :admitting]]]
    (fixture :submitted
      (fn [{:keys [conn a o b r target]}]
        (let [id (if (= "work.request" (namespace attribute)) [:work.request/id (:work.request/id r)]
                     [:knowledge.batch/id (:knowledge.batch/id b)])]
          (d/transact conn [{:db/id id attribute value}])
          (is (= {:status :unavailable} (scheduled/occurrence-summary @conn a o target)))))))
  (fixture :ready
    (fn [{:keys [conn a o b target]}]
      (d/transact conn [[:db/retract [:knowledge.batch/id (:knowledge.batch/id b)]
                        :knowledge.batch/scheduled-occurrence (:schedule.occurrence/id o)]])
      (is (= {:status :unavailable} (scheduled/occurrence-summary @conn a o target)))
      (is (= {:status :unavailable} (scheduled/occurrence-summary @conn a (dissoc o :schedule.occurrence/batch-ref) target)))
      (is (nil? (scheduled/occurrence-summary @conn a
                  (apply dissoc o [:schedule.occurrence/knowledge-batch :schedule.occurrence/batch-ref
                                  :schedule.occurrence/batch-revision]) target))))))

(deftest one-snapshot-observation-survives-later-changes-without-borrowing-a-new-active-batch
  (fixture :closed
    (fn [{:keys [conn a o b s target]}]
      (let [db @conn expected {:status :observed :id (str (:knowledge.batch/id b)) :phase :closed}]
        (d/transact conn [{:db/id [:knowledge.stream/id (:knowledge.stream/id s)] :knowledge.stream/active-batch (random-uuid)}])
        (is (= expected (scheduled/occurrence-summary @conn a o target)))
        (d/transact conn [{:db/id [:schedule.activation/id (:schedule.activation/id a)] :schedule.activation/revision (random-uuid)}])
        (is (= expected (scheduled/occurrence-summary db a o target)))
        (is (= {:status :unavailable} (scheduled/occurrence-summary @conn a o target)))))))


(deftest missing-occurrence-markers-cannot-conceal-the-reverse-batch-link
  (fixture :ready
    (fn [{:keys [conn a o target]}]
      (let [fields [:schedule.occurrence/knowledge-batch :schedule.occurrence/batch-ref
                    :schedule.occurrence/batch-revision]]
        (d/transact conn (mapv (fn [k] [:db/retract [:schedule.occurrence/id (:schedule.occurrence/id o)] k (get o k)]) fields))
        (is (= {:status :unavailable}
               (scheduled/occurrence-summary @conn a (apply dissoc o fields) target)))))))

(deftest missing-frozen-artifact-reference-is-unavailable-without-hydration
  (doseq [field [:knowledge.batch/selection :knowledge.batch/frozen]]
    (fixture :ready
      (fn [{:keys [conn a o b target]}]
        (d/transact conn [[:db/retract [:knowledge.batch/id (:knowledge.batch/id b)] field (get b field)]])
        (is (= {:status :unavailable} (scheduled/occurrence-summary @conn a o target)))))))


(deftest reverse-request-links-and-legacy-inputs-cannot-be-hidden
  (fixture :submitted
    (fn [{:keys [conn a o b target]}]
      (let [bid (:knowledge.batch/id b) oid (:schedule.occurrence/id o)]
        (d/transact conn [[:db/retract [:knowledge.batch/id bid] :knowledge.batch/request (:knowledge.batch/request b)]
                          [:db/retract [:knowledge.batch/id bid] :knowledge.batch/request-intent (:knowledge.batch/request-intent b)]
                          [:db/retract [:schedule.occurrence/id oid] :schedule.occurrence/request (:schedule.occurrence/request o)]
                          {:db/id [:knowledge.batch/id bid] :knowledge.batch/state :ready}])
        (is (= {:status :unavailable}
               (scheduled/occurrence-summary @conn a (dissoc o :schedule.occurrence/request) target))))))
  (fixture :submitted
    (fn [{:keys [conn a o b target]}]
      (d/transact conn [{:work.request/id (random-uuid) :work.request/knowledge-batch (:knowledge.batch/id b)}])
      (is (= {:status :unavailable} (scheduled/occurrence-summary @conn a o target)))))
  (fixture :ready
    (fn [{:keys [conn a o target]}]
      (let [id (random-uuid)]
        (d/transact conn [{:db/id [:schedule.occurrence/id (:schedule.occurrence/id o)] :schedule.occurrence/input-id id}])
        (is (= {:status :unavailable}
               (scheduled/occurrence-summary @conn a (assoc o :schedule.occurrence/input-id id) target)))))))
