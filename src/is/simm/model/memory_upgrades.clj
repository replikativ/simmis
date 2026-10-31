(ns is.simm.model.memory-upgrades
  "Explicit operator upgrade receipts. Pure control-DB data, never authority."
  (:require [datahike.api :as d]))

(def schema
  (into [{:db/ident :memory.upgrade/id :db/valueType :db.type/uuid
          :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}]
        (for [[ident type] [[:room/knowledge-upgrade :db.type/uuid]
                           [:memory.upgrade/room :db.type/uuid]
                           [:memory.upgrade/system :db.type/uuid]
                           [:memory.upgrade/repository :db.type/uuid]
                           [:memory.upgrade/store :db.type/uuid]
                           [:memory.upgrade/by :db.type/uuid]
                           [:memory.upgrade/at :db.type/instant]
                           [:memory.upgrade/basis :db.type/uuid]
                           [:memory.upgrade/control-basis :db.type/uuid]
                           [:memory.upgrade/source :db.type/uuid]
                           [:memory.upgrade/definition-revision :db.type/uuid]
                           [:memory.upgrade/plan :db.type/uuid]
                           [:memory.upgrade/system-scope :db.type/string]
                           [:memory.upgrade/state :db.type/keyword]]]
          {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})))

(defn in [db id]
  (when (and id (contains? (d/schema db) :memory.upgrade/id))
    (when-let [eid (d/q '[:find ?e . :in $ ?id :where [?e :memory.upgrade/id ?id]] db id)]
      (d/pull db '[*] eid))))

(defn request-of [row]
  {:id (:memory.upgrade/id row) :actor (:memory.upgrade/by row)
   :room (:memory.upgrade/room row) :at (:memory.upgrade/at row)
   :target {:system (:memory.upgrade/system row) :scope (:memory.upgrade/system-scope row)
            :repository (:memory.upgrade/repository row) :store (:memory.upgrade/store row)}})

(defn published-for? [db room system]
  (when (contains? (d/schema db) :room/knowledge-upgrade)
    (let [pointer (:room/knowledge-upgrade (d/entity db [:room/id room]))
          receipt (in db pointer)
          target (d/entity db [:system/id system])]
      (and (uuid? pointer) (= :published (:memory.upgrade/state receipt))
           (= room (:memory.upgrade/room receipt))
           (= system (:memory.upgrade/system receipt))
           (= (:system/scope target) (:memory.upgrade/system-scope receipt))
           (uuid? (:memory.upgrade/repository receipt))
           (uuid? (:memory.upgrade/store receipt))))))
