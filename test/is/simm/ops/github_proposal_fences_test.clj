(ns is.simm.ops.github-proposal-fences-test
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [is.simm.model.github-proposals :as git]
            [is.simm.model.system-db :as sdb]
            [is.simm.ops.proposals :as proposals]))

(defn- fixture [marker f]
  (let [cfg {:store {:backend :memory :id (random-uuid)} :schema-flexibility :write}
        id (random-uuid) by (random-uuid) room (random-uuid) scope (random-uuid)]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (d/transact conn sdb/schema)
        (let [filing (proposals/prepare-filing
                      {:id id :title "Incoming Git changes" :author by :room room
                       :attrs (when marker {marker (if (= :proposal/github-decision-state marker) :applying (random-uuid))})
                       :forks [{:scope scope :authority-scope room :branch "github-review-local"
                                :system-type :repo :author by :base-commit (str (random-uuid))
                                :head-commit (str (random-uuid))}]})]
          (d/transact conn [[:db.fn/call proposals/filing-tx-data filing]])
          (with-redefs [sdb/get-conn (constantly conn)]
            (f conn id by scope)))
        (finally (d/release conn) (d/delete-database cfg))))))

(defn- result [f]
  (try (f) :unexpected-success
       (catch Throwable e
         (loop [e e]
           (cond (= :git-settlement-unavailable (:error (ex-data e))) :guarded
                 (ex-cause e) (recur (ex-cause e))
                 :else {:unexpected-error (ex-message e)})))))

(deftest every-partial-marker-fences-every-generic-decision-before-native-effects
  (doseq [marker git/markers]
    (fixture marker
             (fn [conn id by scope]
               (let [before (proposals/get-proposal id) effects (atom 0)
                     forbidden (fn [& _] (swap! effects inc) (throw (AssertionError. "Native decision attempted")))]
                 (with-redefs-fn
                   {#'proposals/fork-conflicts forbidden
                    #'proposals/land-fork! forbidden
                    #'proposals/discard-fork-branch! forbidden
                    #'proposals/add-comment! forbidden
                    #'proposals/file-decision! forbidden}
                   (fn []
                     (doseq [action [#(proposals/accept-proposal! id :by by)
                                     #(proposals/dismiss-proposal! id :by by)
                                     #(proposals/accept-fork! id scope "github-review-local" :by by)
                                     #(proposals/dismiss-fork! id scope "github-review-local" :by by)
                                     #(proposals/request-changes! id {:by by :body "Revise"})
                                     #(proposals/reconcile-status! id)
                                     #(proposals/record-decision! before :accepted "forged" by)
                                     #(proposals/decide-in! conn id by {:verb :dismiss-proposal} {})]]
                       (is (= :guarded (result action)) (str marker)))))
                 (is (= before (proposals/get-proposal id)))
                 (is (zero? @effects)))))))

(deftest a-stale-unmarked-map-cannot-bypass-the-writer-guard
  (fixture nil
           (fn [conn id by _]
             (let [stale (proposals/get-proposal id)]
               (d/transact conn [{:proposal/id id :proposal/github-provenance-id (random-uuid)}])
               (is (= :guarded (result #(proposals/record-decision! stale :accepted "stale map" by))))
               (is (= :open (:proposal/status (proposals/get-proposal id))))))))

(deftest generic-card-review-does-not-certify-live-branches-or-dispatch-a-model
  (doseq [marker git/markers]
    (let [p {:proposal/id (random-uuid) marker (random-uuid)}
          forbidden (fn [& _] (throw (AssertionError. "Live branch review attempted")))]
      (with-redefs-fn {#'proposals/fork-heads forbidden
                       #'proposals/compute-proposal-review forbidden}
        (fn []
          (is (= {:tier :review-only :conflicts [] :settlement-supported? false
                  :target-compatibility :not-checked :backend :git}
                 (proposals/proposal-review p)))
          (is (nil? (proposals/proposal-summary p []))))))))
