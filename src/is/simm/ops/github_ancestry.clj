(ns is.simm.ops.github-ancestry
  "Finite reachability over verified native Git commit metadata. This reads one
   immutable DB, never payloads, and refuses incomplete or cyclic evidence."
  (:require [datahike.api :as d]
            [geschichte.budget :as budget]))

(defn- refuse! [reason]
  (throw (ex-info "Git ancestry cannot be verified" {:type ::held :reason reason})))

(defn proven-ancestor?
  "True means a concrete parent path was found. False requires complete explored
   history; a truncated frontier is not evidence of unrelated history. Caller
   owns snapshot retention, source authenticity and current authorization."
  [db ancestor descendant operation]
  (budget/require-limits! operation [:commit-visits :graph-edges])
  (when-not (every? uuid? [ancestor descendant]) (refuse! :invalid-history))
  (loop [pending [[descendant false]] active #{} finished #{}]
    (budget/check! operation)
    (if-let [[id exit?] (peek pending)]
      (cond
        exit? (recur (pop pending) (disj active id) (conj finished id))
        (contains? active id) (refuse! :cyclic-history)
        (contains? finished id) (recur (pop pending) active finished)
        :else
        (do
          (budget/charge! operation :commit-visits 1)
          (let [row (d/entity db [:geschichte.commit/id id])]
            (when-not (:db/id row) (refuse! :history-unavailable))
            (if (= id ancestor)
              true
              (let [edges (vec (take 17 (d/datoms db :eavt (:db/id row) :geschichte.commit/parents)))]
                (when (> (count edges) 16) (refuse! :history-limit))
                (budget/charge! operation :graph-edges (count edges))
                ;; Imported frontier stubs carry only an ID/OID. An ordinary
                ;; imported root has parsed commit time even without a snapshot.
                (when (and (empty? edges) (nil? (:geschichte.commit/snapshot row))
                           (not (inst? (:geschichte.commit/time row))))
                  (refuse! :history-unavailable))
                (let [parents (mapv #(get (d/entity db (:v %)) :geschichte.commit/id) edges)]
                  (when-not (every? uuid? parents) (refuse! :history-unavailable))
                  (recur (into (conj (pop pending) [id true]) (map #(vector % false) parents))
                         (conj active id) finished)))))))
      false)))
