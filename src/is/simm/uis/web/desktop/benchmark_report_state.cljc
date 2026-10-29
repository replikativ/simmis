(ns is.simm.uis.web.desktop.benchmark-report-state
  "Pure client correspondence for a prepared report; never write authority.")

(defn intent [room scorecard prepared]
  (let [source (:source prepared)]
    (when (and (= #{:room :store :scorecard} (set (keys source)))
               (every? uuid? (vals source))
               (= (str room) (str (:room source))) (= scorecard (str (:scorecard source)))
               (uuid? (:plan prepared)) (uuid? (:target-stamp prepared)))
      {:source source :scorecard (:scorecard source) :expected-plan (:plan prepared)
       :expected-target-stamp (:target-stamp prepared)})))

(defn receipt-current? [room intent receipt]
  (and (= (:expected-plan intent) (:plan receipt))
       (= (:expected-target-stamp intent) (:target-stamp receipt))
       (= (str room) (str (get-in receipt [:source :room])))
       (= (:source intent) (:source receipt))
       (= (str "reports/benchmarks/" (:scorecard intent) ".md") (get-in receipt [:report :path]))
       (#{:created :existing} (get-in receipt [:report :status]))
       (= "reports/index.md" (get-in receipt [:index :path]))
       (#{:created :existing :preserved} (get-in receipt [:index :status]))
       (= (not= :preserved (get-in receipt [:index :status])) (:complete? receipt))))

(defn retire-stale [state epoch]
  (when state (into {}
                    (map (fn [[scorecard entry]]
                           [scorecard (if (and (:loading? entry) (not= epoch (:epoch entry)))
                                        (-> entry (assoc :loading? false :token nil
                                                         :error "Connection changed. Retry publication in its original Memory."))
                                        entry)])) state)))
