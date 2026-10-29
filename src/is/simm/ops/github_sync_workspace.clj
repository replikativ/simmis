(ns is.simm.ops.github-sync-workspace
  "Fixed HOST native workspace extent. A retained Git branch is not a live
   connection. Unknown acquisitions/releases remain with the original Room's
   existing cleanup owner; they are never force-released or retried blindly."
  (:require [datahike.api :as d]
            [datahike.versioning :as versioning]
            [dvergr.runtime.preparation :as preparation]
            [geschichte.query :as query]
            [geschichte.workspace :as workspace]
            [is.simm.ops.github-sync-data :as facts]
            [is.simm.runtimes.operation-lifetime :as lifetime]))

(defn- release! [owner]
  (case (:phase @owner)
    :connected (let [conn (:conn @owner)]
                 (swap! owner assoc :phase :releasing)
                 (d/release conn)
                 (reset! owner {:phase :released}))
    :released nil
    ;; Native allocation/acquisition/release has no safe automatic replay.
    (facts/refuse! :native-cleanup-unconfirmed)))

(defn retained-checkpoint!
  "Read-only cleanup proof for an acknowledged private branch after the caller
   acquired its exclusive Room slot. No source-body reads or native allocation.
   Missing/interrupted history holds; branch-name existence alone is not proof."
  [parent branch ancestor]
  (let [base @parent snapshot (versioning/branch-as-db parent branch)
        repository (:geschichte.repo/id (query/repository base))]
    (when-not (and snapshot (uuid? ancestor)
                   (= branch (get-in snapshot [:config :branch]))
                   (= (dissoc (:config base) :branch) (dissoc (:config snapshot) :branch))
                   (= repository (:geschichte.repo/id (query/repository snapshot))))
      (facts/refuse! :private-branch-changed))
    (let [tip (d/commit-id snapshot)]
      (loop [pending [snapshot] seen #{}]
        (when (.isInterrupted (Thread/currentThread)) (throw (InterruptedException. "Git cleanup interrupted")))
        (when (or (empty? pending) (> (+ (count seen) (count pending)) 4096))
          (facts/refuse! :checkpoint-history-unavailable))
        (let [db (peek pending) pending (pop pending) id (d/commit-id db)]
          (if (= ancestor id)
            tip
            (let [parents (versioning/parent-commit-ids db)]
              (when-not (and (uuid? id) (coll? parents) (<= (count parents) 16) (every? uuid? parents))
                (facts/refuse! :checkpoint-history-unavailable))
              (let [unseen (remove (conj seen id) parents)
                    loaded (mapv #(or (versioning/commit-as-db parent %) (facts/refuse! :checkpoint-history-unavailable)) unseen)]
                (recur (into pending loaded) (conj seen id))))))))))

(defn retire!
  "Remove only the exact retained private root after the fixed caller recorded
   :closing. Unknown removal keeps the original physical Room operation slot;
   absence is accepted only on this recorded cleanup path, never construction."
  [parent lease branch checkpoint]
  (when-not (and (keyword? branch) (= "geschichte.workspace" (namespace branch))
                 (re-matches #"github-sync-[0-9a-f-]{36}" (name branch)) (uuid? checkpoint))
    (facts/refuse! :invalid-native-request))
  (when (contains? (versioning/branches parent) branch)
    (let [snapshot (versioning/branch-as-db parent branch)]
      (when-not (and snapshot (= checkpoint (d/commit-id snapshot)))
        (facts/refuse! :private-branch-changed)))
    (let [owner (atom {:phase :retiring :parent parent :branch branch :checkpoint checkpoint})]
      (try
        (versioning/delete-branch! parent branch)
        (when (contains? (versioning/branches parent) branch) (facts/refuse! :cleanup-unconfirmed))
        (reset! owner {:phase :released})
        (catch Throwable error
          (preparation/retain-host-cleanup! lease {branch owner} release!)
          (throw error)))))
  (when (contains? (versioning/branches parent) branch) (facts/refuse! :cleanup-unconfirmed))
  nil)

(defn with-connection!
  "Use a previously journaled private branch under its original Room lease.
   create? is internal: only the winning physical prepare call may pass true.
   F is synchronous HOST implementation, never a request-selected callback.
   A successful return acknowledges release while retaining the branch."
  ([parent lease branch checkpoint create? f]
   (with-connection! parent lease branch checkpoint create? (constantly nil) f))
  ([parent lease branch checkpoint create? allocated! f]
   (when-not (and (keyword? branch) (= "geschichte.workspace" (namespace branch))
                  (re-matches #"github-sync-[0-9a-f-]{36}" (name branch))
                  (uuid? checkpoint) (boolean? create?) (fn? allocated!) (fn? f))
     (facts/refuse! :invalid-native-request))
   (lifetime/check-current!)
   (let [parent-db @parent
         origin {:version 1 :kind :datahike :commit checkpoint
                 :source {:store (get-in parent-db [:config :store :id])
                          :repository (:geschichte.repo/id (query/repository parent-db))}}
         source (workspace/resolve-checkpoint parent origin)
         owner (atom {:phase :unacquired})
         acknowledged (volatile! false)
         last-checkpoint (volatile! checkpoint)
         result (try
                  (if create?
                    (do
                      (when (contains? (versioning/branches parent) branch) (facts/refuse! :private-branch-exists))
                     ;; No callback/native connect before the allocation ACK.
                      (reset! owner {:phase :allocating :parent parent :branch branch :origin origin})
                      (workspace/fork-at-checkpoint! parent branch origin source)
                      (reset! owner {:phase :forked :parent parent :branch branch :origin origin})
                      (vreset! acknowledged true)
                      (allocated!))
                    (let [stored (versioning/branch-as-db parent branch)]
                      (when-not (and stored (= checkpoint (d/commit-id stored)))
                        (facts/refuse! :private-branch-changed))
                      (vreset! acknowledged true)))
                  {:value
                   (lifetime/call-detached
                    (fn [restore!]
                      (reset! owner {:phase :acquiring})
                      (let [conn (d/connect (assoc (:config parent-db) :branch branch))]
                        (reset! owner {:phase :connected :conn conn})
                        (try
                          (workspace/verify-checkpoint-workspace! source @conn branch)
                          (restore! #(f conn))
                          (finally
                            (vreset! last-checkpoint nil)
                            (vreset! last-checkpoint (d/commit-id @conn)))))))}
                  (catch Throwable error {:error error})
                  (finally
                    (try
                      (when-not (= :unacquired (:phase @owner)) (release! owner))
                      (catch Throwable error
                       ;; Preserve this exact acquisition and exclusive slot.
                       ;; Unknown native outcomes are not automatically retried.
                        (preparation/retain-host-cleanup! lease {branch owner} release!)
                        (throw error)))))]
     (if-let [error (:error result)]
       (if @acknowledged
         (throw (ex-info "Private Git operation failed after native return"
                         {:type ::body-failed :branch branch :checkpoint @last-checkpoint :released? true} error))
         (throw error))
       (:value result)))))
