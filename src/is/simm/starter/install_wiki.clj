(ns is.simm.starter.install-wiki
  "Explicit inactive installation. Never discovers rooms, funds work, enables a
   schedule, upgrades a customized bundle or retries an uncertain write."
  (:require [is.simm.ops.owned-reads :as reads]
            [is.simm.ops.owned-workflow-authoring :as authoring]
            [is.simm.ops.owned-workflows :as owned]
            [is.simm.ops.workflows :as workflows]
            [is.simm.starter.wiki-maintenance :as starter]))

(defn decision
  "Pure comparison of a captured whole subtree, including paths and modes.
   An executable bit, extra file, deletion or textual edit is a customization."
  [snapshot shipped]
  (cond
    (empty? (:manifest snapshot)) :install
    (and (= shipped (:files snapshot))
         (= (set (keys shipped)) (set (map :path (:manifest snapshot))))
         (every? #(= 33188 (:mode %)) (:manifest snapshot))) :already-installed
    :else :customized))

(defn install!
  "Install shipped bytes only into an inspected empty named subtree, using its
   target-bound revision and native whole-subtree CAS. Exact repeats are reads;
   custom source is preserved. Pending/failure propagates without replay.
   Returned activation means this operation never changes any schedule/model."
  [host principal original-room-handle]
  (let [binding (reads/resolve-host-install! host principal original-room-handle)
        snapshot (workflows/snapshot-in (:db binding) starter/name)
        revision (str (owned/source-id binding starter/name (:manifest snapshot)))
        shipped (starter/source)
        action (decision snapshot shipped)
        base {:name starter/name :shipped-version starter/version :activation :unchanged}]
    (case action
      :install
      (let [result (authoring/install! host principal original-room-handle
                                       {:name starter/name :expected-revision revision :files shipped})]
        (assoc base :status :installed :revision (get-in result [:source :revision])))
      :already-installed (assoc base :status action :revision revision)
      :customized (assoc base :status action :revision revision))))
