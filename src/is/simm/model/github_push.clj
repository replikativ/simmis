(ns is.simm.model.github-push
  "Pure journal fragments for explicit outbound Git publication. Shares the
   pending-room slot with incoming sync, but never its transition protocol.
   These fragments grant no native creation, network or recovery authority."
  (:require [is.simm.model.github-sync :as sync]))

(def max-pack-bytes (* 16 1024 1024))

(def schema
  (mapv (fn [[ident type]] {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one})
        [[:github.push/retention :db.type/store-ref] [:github.push/retention-id :db.type/uuid]
         [:github.push/ready :db.type/store-ref] [:github.push/ready-id :db.type/uuid]
         [:github.push/pack :db.type/store-ref] [:github.push/pack-size :db.type/long]
         [:github.push/intent :db.type/store-ref] [:github.push/intent-id :db.type/uuid]
         [:github.push/outcome :db.type/store-ref] [:github.push/outcome-id :db.type/uuid]
         [:github.push/closure :db.type/store-ref] [:github.push/closure-id :db.type/uuid]]))

(defn- refuse! [reason]
  (throw (ex-info "Git push journal refused" {:type ::refused :reason reason})))

(defn prepare-tx [db binding ref body-id]
  (sync/prepare-tx db binding ref body-id :push))

(def ^:private next-states
  {:creating #{:retaining :closed}
   :retaining #{:ready :closed}
   :ready #{:sending :closed}
   :sending #{:pushed :rejected :observed}
   :pushed #{:closed} :rejected #{:closed} :observed #{:closed}
   :closed #{}})

(def ^:private slots
  {:retaining :retention :ready :ready :sending :intent
   :pushed :outcome :rejected :outcome :observed :outcome :closed :closure})

(defn transition-tx
  "Whole-row CAS. Only the original positively admitted Sending call may send;
   unknown Sending cannot close or return to Ready. Observed is a NEW verified
   upstream fact, never a claim that the historical push succeeded. The fixed
   HOST must separately validate all native/remote evidence and current grants."
  ([db expected state ref body-id] (transition-tx db expected state ref body-id nil))
  ([db expected state ref body-id pack]
   (let [row (sync/sync-in db (:github.sync/id expected))
         from (:github.sync/state row) slot (slots state)
         ref-key (keyword "github.push" (name (or slot :invalid)))
         id-key (keyword "github.push" (str (name (or slot :invalid)) "-id"))
         receipt {ref-key ref id-key body-id}]
     (when-not (and row (= row expected) (= :push (sync/kind row))) (refuse! :different-intent))
     (when-not (and slot (every? uuid? [ref body-id])) (refuse! :invalid-receipt))
     (when-not (or (= state from) (contains? (next-states from) state)) (refuse! :invalid-transition))
     (when-not (= (:github.sync/room row) (:github.sync/active-room row))
       (when-not (= :closed from state) (refuse! :inactive-intent)))
     (when (and (contains? row ref-key) (not= receipt (select-keys row (keys receipt))))
       (refuse! :different-receipt))
     (when-not (if (= :ready state)
                 (and (= #{:github.push/pack :github.push/pack-size} (set (keys pack)))
                      (uuid? (:github.push/pack pack))
                      (integer? (:github.push/pack-size pack))
                      (<= 1 (:github.push/pack-size pack) max-pack-bytes)
                      (every? uuid? [(:github.push/retention row) (:github.push/retention-id row)])
                      (or (nil? (:github.push/pack row)) (= pack (select-keys row (keys pack)))))
                 (nil? pack))
       (refuse! :invalid-pack))
     (if (= state from) []
         (cond-> [(merge {:db/id (:db/id row) :github.sync/state state} receipt pack)]
           (= :closed state) (conj [:db/retract (:db/id row) :github.sync/active-room (:github.sync/active-room row)]))))))
