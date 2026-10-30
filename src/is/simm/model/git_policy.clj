(ns is.simm.model.git-policy
  "Typed remote policy and bounded polling observations. Policy is outside
   authored files; it grants no transport, Room, interpreter or publication
   authority. Review and polling have separate revision identities."
  (:require [datahike.api :as d]
            [hasch.core :as hasch]))

(def schema
  (mapv (fn [[ident type]]
          (cond-> {:db/ident ident :db/valueType type :db/cardinality :db.cardinality/one}
            (= :git.policy/room ident) (assoc :db/unique :db.unique/value)))
        [[:git.policy/room :db.type/uuid] [:git.policy/connection-id :db.type/uuid]
         [:git.policy/by :db.type/uuid] [:git.policy/revision :db.type/uuid]
         [:git.policy/review-revision :db.type/uuid] [:git.policy/review-mode :db.type/keyword]
         [:git.policy/enabled? :db.type/boolean] [:git.policy/interval-seconds :db.type/long]
         [:git.policy/next-check :db.type/instant] [:git.policy/last-check :db.type/instant]
         [:git.policy/status :db.type/keyword] [:git.policy/failures :db.type/long]
         [:git.policy/pending-proposal :db.type/uuid]]))

(defn- refuse! [reason]
  (throw (ex-info "Git remote policy refused" {:type ::refused :reason reason})))

(defn current-in [db room]
  (when-not (uuid? room) (refuse! :invalid-room))
  (when-let [entity (:db/id (d/entity db [:git.policy/room room]))]
    (d/pull db '[*] entity)))

(defn- settings! [{:keys [enabled? interval-seconds review-mode] :as settings}]
  (when-not (and (= #{:enabled? :interval-seconds :review-mode} (set (keys settings)))
                 (boolean? enabled?) (integer? interval-seconds) (<= 60 interval-seconds 86400)
                 (#{:fast-forward :review-all} review-mode))
    (refuse! :invalid-settings))
  settings)

(defn effective
  "Defaults are inert: no timer or transport is enabled by reading them. Their
   stable identity binds a missing policy to this exact Room/connection."
  [db room connection-id]
  (when-not (every? uuid? [room connection-id]) (refuse! :invalid-identity))
  (if-let [row (current-in db room)]
    (do
      (when-not (and (= connection-id (:git.policy/connection-id row))
                     (every? uuid? (map row [:git.policy/by :git.policy/revision :git.policy/review-revision])))
        (refuse! :connection-changed))
      (settings! {:enabled? (:git.policy/enabled? row) :interval-seconds (:git.policy/interval-seconds row)
                  :review-mode (:git.policy/review-mode row)})
      row)
    (let [revision (hasch/uuid [:simmis/git-policy-defaults 1 room connection-id])]
      {:git.policy/room room :git.policy/connection-id connection-id
       :git.policy/revision revision :git.policy/review-revision revision
       :git.policy/enabled? false :git.policy/interval-seconds 60 :git.policy/review-mode :fast-forward})))

(defn review-token [policy]
  (select-keys policy [:git.policy/room :git.policy/connection-id :git.policy/review-mode :git.policy/review-revision]))

(defn review-current! [db expected]
  (when-not (and (= #{:git.policy/room :git.policy/connection-id :git.policy/review-mode :git.policy/review-revision}
                    (set (keys expected)))
                 (= expected (review-token (effective db (:git.policy/room expected) (:git.policy/connection-id expected)))))
    (refuse! :review-policy-changed))
  expected)

(defn configure-tx
  "HOST writer fragment, requiring current human ownership and exact connection
   separately. Polling settings do not invalidate manual review tokens. An ABA
   review-mode change gets a fresh token even when returning to the old mode."
  [db room connection-id expected-revision revision by settings now]
  (settings! settings)
  (let [current (effective db room connection-id)]
    (when-not (and (every? uuid? [expected-revision revision by]) (inst? now)
                   (= expected-revision (:git.policy/revision current)) (not= revision expected-revision))
      (refuse! :policy-changed))
    [(cond-> {:git.policy/room room :git.policy/connection-id connection-id :git.policy/by by
              :git.policy/revision revision
              :git.policy/review-revision (if (= (:review-mode settings) (:git.policy/review-mode current))
                                            (:git.policy/review-revision current) revision)
              :git.policy/review-mode (:review-mode settings) :git.policy/enabled? (:enabled? settings)
              :git.policy/interval-seconds (long (:interval-seconds settings))
              :git.policy/next-check now :git.policy/failures 0 :git.policy/status :idle}
       (:db/id current) (assoc :db/id (:db/id current)))]))

(defn pending-proposal
  "A single bounded hint into ordinary Proposal state. Missing/mismatched
   evidence holds; only a positively terminal Proposal releases this guard."
  [db policy]
  (when-let [id (:git.policy/pending-proposal policy)]
    (let [proposal (d/entity db [:proposal/id id])]
      (when-not (and (uuid? id) (= (:git.policy/room policy) (:proposal/room proposal))
                     (uuid? (:proposal/github-key proposal)))
        (refuse! :pending-proposal-unavailable))
      (case (:proposal/status proposal)
        :open id
        (:accepted :dismissed) nil
        (refuse! :pending-proposal-unavailable)))))

(defn pending-tx
  "Compose with positive Proposal filing, never use a marker as authority.
   A default row created here keeps polling disabled and its default epochs."
  [db room connection-id by proposal-id]
  (let [current (effective db room connection-id)
        pending (pending-proposal db current)]
    (when-not (and (every? uuid? [by proposal-id]) (or (nil? pending) (= proposal-id pending)))
      (refuse! :incoming-proposal-pending))
    [(cond-> (merge (select-keys current [:git.policy/room :git.policy/connection-id :git.policy/revision
                                        :git.policy/review-revision :git.policy/review-mode :git.policy/enabled?
                                        :git.policy/interval-seconds])
                   {:git.policy/pending-proposal proposal-id})
       (:db/id current) (assoc :db/id (:db/id current))
       (not (:db/id current)) (assoc :git.policy/by by :git.policy/status :idle :git.policy/failures 0))]))

(defn due? [policy now]
  (and (true? (:git.policy/enabled? policy)) (inst? now)
       (inst? (:git.policy/next-check policy))
       (<= (.getTime ^java.util.Date (:git.policy/next-check policy)) (.getTime ^java.util.Date now))))

(defn checked-tx
  "Record one current-version observation. Old versions/times are no-ops and
   cannot overwrite new configuration. Backoff is bounded; busy is not failure.
   This telemetry is not an execution receipt or permission to repeat an effect."
  [db room connection-id revision now status]
  (when-not (and (uuid? revision) (inst? now) (#{:unchanged :synced :proposal :busy :held :failed} status))
    (refuse! :invalid-observation))
  (let [current (effective db room connection-id)
        previous (:git.policy/last-check current)]
    (if (or (not= revision (:git.policy/revision current)) (not (:db/id current))
            (and (inst? previous) (<= (.getTime ^java.util.Date now) (.getTime ^java.util.Date previous))))
      []
      (let [interval (:git.policy/interval-seconds current)
            failures (if (#{:held :failed} status) (min 6 (inc (or (:git.policy/failures current) 0))) 0)
            delay-seconds (max interval (min 3600 (* interval (bit-shift-left 1 failures))))]
        [{:db/id (:db/id current) :git.policy/last-check now :git.policy/status status
          :git.policy/failures (long failures)
          :git.policy/next-check (java.util.Date. (+ (.getTime ^java.util.Date now) (* 1000 delay-seconds)))}]))))
