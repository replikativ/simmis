(ns is.simm.savepoint-contract-test
  "Provider-free characterization of the PINNED savepoint substrate scope.
   Dirty-state divergence is a documented limitation, not a portability law.
   Parent isolation, unlike that characterization, remains a hard assertion."
  (:require [clojure.test :refer [deftest is]]
            [datahike.api :as d]
            [dvergr.substrate.geschichte :as workspace]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.yggdrasil :as ygg]
            [yggdrasil.adapters.geschichte :as gy]
            [yggdrasil.protocols :as p]))

(def ^:private system-id "savepoint-contract-repository")
(def ^:private path "state.clj")

(defn- text [db entry]
  (when entry (String. ^bytes (repo/read-entry (atom db) entry) "UTF-8")))

(defn- observe-db [db]
  (let [work (get (query/worktree db) path)
        stage (get (query/stage db) path)]
    {:work work :stage stage :work-text (text db work) :stage-text (text db stage)}))

(defn- parent-view [conn]
  (let [db @conn]
    {:config (:config db) :physical-commit (d/commit-id db)
     :ref (query/current-ref db) :refs (query/refs db)
     :workspace (observe-db db)}))

(defn- current-observation []
  (observe-db @(gy/connection (get (ygg/registered-systems) system-id))))

(defn- with-repository [f]
  (let [cfg (assoc workspace/repository-options :store {:backend :memory :id (random-uuid)})
        root (context/create-execution-context)
        forks (atom [])]
    (d/create-database cfg)
    (let [conn (d/connect cfg)]
      (try
        (repo/init! conn {:name "savepoint contract diagnostic"})
        (binding [ec/*execution-context* root]
          (ygg/register! (gy/create conn {:system-name system-id}))
          (repo/write! conn path (.getBytes "A" "UTF-8") {:mode 33188})
          (repo/stage-all! conn)
          (repo/commit! conn {:message "A"})
          (f {:conn conn :root root :forks forks}))
        (finally
          ;; Dedicated process-termination diagnostic: no Spindel computations,
          ;; provider or tool tasks were started. Native DB connections are
          ;; closed through Datahike release below. Attempt EVERY real native
          ;; settlement, retaining refusal as a separate hard assertion. Raw
          ;; context/store teardown below does NOT fabricate a settled handle.
          (doseq [{:keys [fork]} (reverse @forks)]
            (let [error (try (binding [ec/*execution-context* root]
                               (ygg/discard-fork! fork {:sync? true})) nil
                             (catch Throwable error error))]
              (is (nil? error)
                  (str "Native discard must acknowledge ownership return: "
                       (when error {:class (str (class error))
                                    :message (ex-message error) :data (ex-data error)})))))
          (doseq [{:keys [fork]} (reverse @forks)]
            (context/stop-context! (:child-ctx fork)))
          (context/close-context! root)
          (doseq [c (reduce (fn [cs c] (if (some #(identical? c %) cs) cs (conj cs c)))
                            [conn] (map :conn @forks))]
            (d/release c))
          (assert (and (= :memory (get-in cfg [:store :backend]))
                       (uuid? (get-in cfg [:store :id]))))
          (d/delete-database cfg))))))

(defn- dirty! [conn]
  (repo/write! conn path (.getBytes "B" "UTF-8") {:mode 33188})
  (repo/stage! conn [path])
  (repo/write! conn path (.getBytes "B2" "UTF-8") {:mode 33261}))

(defn- advance! [conn]
  (repo/write! conn path (.getBytes "C" "UTF-8") {:mode 33188})
  (repo/stage-all! conn)
  (repo/commit! conn {:message "C"}))

(defn- owned-fork! [forks opts]
  (let [fork (ygg/fork! (assoc opts :sync? true))
        conn (ygg/with-fork fork
               (gy/connection (get (ygg/registered-systems) system-id)))]
    (swap! forks conj {:fork fork :conn conn})
    fork))

(deftest logical-head-capture-excludes-dirty-state-but-frozen-fork-retains-it
  (with-repository
    (fn [{:keys [conn forks]}]
      (let [system (get (ygg/registered-systems) system-id)
            committed (p/snapshot-id system)
            committed-db @conn]
        (dirty! conn)
        (let [captured @conn
              dirty (observe-db captured)
              frozen (owned-fork! forks {:mode :frozen})]
          (is (= committed (p/snapshot-id system)))
          (is (not= (d/commit-id committed-db) (d/commit-id captured)))
          (is (= ["B2" "B" 33261 33188]
                 [(:work-text dirty) (:stage-text dirty)
                  (get-in dirty [:work :mode]) (get-in dirty [:stage :mode])]))
          (advance! conn)
          (let [parent-before (parent-view conn)
                historical (owned-fork! forks {:mode :frozen :snapshots {system-id committed}})]
            (is (= dirty (observe-db captured)) "The immutable physical DB still names dirty state")
            (is (= dirty (ygg/with-fork frozen (current-observation))))
            (is (= "A" (:work-text (ygg/with-fork historical (current-observation))))
                "KNOWN LIMITATION: historical logical HEAD restores committed A, not dirty B2")
            (is (= parent-before (parent-view conn))
                "Historical snapshot restoration must not alter the live parent")))))))
