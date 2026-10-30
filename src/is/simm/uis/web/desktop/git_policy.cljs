(ns is.simm.uis.web.desktop.git-policy
  "Context-owned, revision-fenced Memory sync settings."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.github-sync :as git]
            [is.simm.uis.web.desktop.github-remote :as remote]
            [is.simm.runtimes.web :as web]))

(defn- uuid-string? [s] (and (string? s) (= s (some-> s parse-uuid str))))

(defn action-basis [memory state]
  {:action (git/action-basis state memory (connection/snapshot))
   :observed (:policy-observed state)
   :policy (select-keys (:policy-reply state) [:connection-id :revision])})

(defn valid-settings? [settings]
  (and (= #{:enabled? :interval-seconds :review-mode} (set (keys settings)))
       (boolean? (:enabled? settings)) (integer? (:interval-seconds settings))
       (<= 60 (:interval-seconds settings) 86400) (zero? (mod (:interval-seconds settings) 60))
       (#{:fast-forward :review-all} (:review-mode settings))))

(defn- valid? [result stamp]
  (and (= #{:connection-id :target-stamp :revision :enabled? :interval-seconds :review-mode :status :last-check :next-check}
          (set (keys result)))
       (= (str stamp) (:target-stamp result))
       (every? uuid-string? (map result [:connection-id :target-stamp :revision]))
       (valid-settings? (select-keys result [:enabled? :interval-seconds :review-mode]))
       (or (nil? (:status result)) (#{:idle :busy :held :unchanged :synced :proposal :failed} (:status result)))
       (every? #(or (nil? %) (and (string? %) (<= (count %) 64))) (map result [:last-check :next-check]))))

(defn controller [actions memory-state]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        state (:state actions) key (:owner actions) room (second key)
        live? #(and (web/current-session? session) ((:enabled? actions))
                    (identical? (ec/home-context ctx) (ec/home-context runtime))
                    (or (nil? ec/*execution-context*)
                        (identical? (ec/home-context ec/*execution-context*) (ec/home-context ctx))))
        scope #(binding [ec/*execution-context* ctx]
                 [(connection/snapshot) (:target-stamp (memory-state))
                  (select-keys (:github-source (memory-state)) [:repository :ref])
                  (get-in (memory-state) [:github-source :git-policy :revision])])
        basis #(binding [ec/*execution-context* ctx] (action-basis (memory-state) (state)))
        update! (fn [& args] (binding [ec/*execution-context* ctx]
                               (when (web/current-session? session) (apply swap! sig/github-sync-state update key args))))]
    (letfn [(request! [offer operation settings]
              (when (and (live?) (= offer (basis)) (not (:policy-loading? (state))))
                (let [[_ stamp :as observed] (scope) ticket (random-uuid)
                      current (:policy-reply (state))
                      owns? #(and (web/current-session? session) (= ticket (:policy-ticket (state)))
                                  (not= ticket (:policy-finished (state))))
                      fail (fn [_] (when (owns?)
                                     (update! assoc :policy-loading? false :policy-finished ticket :policy-observed observed
                                              :policy-error "Sync settings could not be confirmed. Reload settings before changing them again.")))
                      done (fn [result]
                             (when (owns?)
                               (if (and (= observed (scope)) (valid? result stamp))
                                 (update! assoc :policy-loading? false :policy-finished ticket :policy-observed observed
                                          :policy-reply result :policy-error nil)
                                 (fail nil))))
                      request (if (= :status operation) {:expected-target-stamp (str stamp)}
                                  {:connection-id (:connection-id current) :revision (:revision current) :settings settings})]
                  (when (or (= :status operation) (and current (= observed (:policy-observed (state)))))
                    (update! assoc :policy-loading? true :policy-ticket ticket :policy-requested observed :policy-error nil)
                    (try (binding [ec/*execution-context* ctx ec/*spin-id* nil]
                           ((remote/git-policy-operation! web/server-id room operation request) done fail))
                         (catch :default _ (fail nil)))))))
            (ensure! []
              (when (live?)
                (when (and (:policy-loading? (state)) (not= (:policy-requested (state)) (scope)))
                  (let [same-target? (= (take 3 (:policy-requested (state))) (take 3 (scope)))]
                    (update! #(cond-> (assoc % :policy-loading? false :policy-ticket (random-uuid) :policy-observed nil)
                                (not same-target?) (assoc :policy-reply nil)))))
                (when (and (not (:policy-loading? (state))) (not= (:policy-observed (state)) (scope)))
                  (request! (basis) :status nil))))]
      {:ensure ensure! :reload #(request! % :status nil)
       :save (fn [offer settings]
               (when (and (live?) (= offer (basis)))
                 (if (valid-settings? settings) (request! offer :save settings)
                     (update! assoc :policy-error "Use a whole-number interval between 1 and 1,440 minutes."))))})))
