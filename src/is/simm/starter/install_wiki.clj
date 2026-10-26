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

(defn classify
  "Match a complete subtree against immutable shipped versions. Matching an
   older bundle is an upgrade offer, never permission to replace it implicitly."
  [snapshot]
  (let [base {:shipped-version starter/version}]
    (if (and (empty? (:manifest snapshot)) (empty? (:files snapshot)))
      (assoc base :status :missing)
      (if-let [version (some #(when (= :already-installed (decision snapshot (starter/source %))) %)
                             (range starter/version 0 -1))]
        (assoc base :status (if (= version starter/version) :installed :upgrade-available)
                    :installed-version version)
        (assoc base :status :customized)))))

(defn inspect!
  "Inspect source and its target-bound whole-subtree revision. Does not write,
   install, upgrade, select a model or change any activation."
  [host principal original-room-handle]
  (let [binding (reads/resolve-host-install! host principal original-room-handle)
        snapshot (workflows/snapshot-in (:db binding) starter/name)
        facts (assoc (classify snapshot) :name starter/name :activation :unchanged
                     :revision (str (owned/source-id binding starter/name (:manifest snapshot))))]
    (reads/validate-host-install! host principal original-room-handle binding)
    facts))

(defn upgrade!
  "Explicit upgrade of one inspected pristine older shipped subtree. Native
   whole-subtree CAS catches later races. Changed/custom source is preserved.
   Never changes an existing deployment, allowance, model or processing cursor."
  [host principal original-room-handle request]
  (when-not (and (map? request) (= #{:expected-revision :from-version} (set (keys request)))
                 (integer? (:from-version request)) (<= 1 (:from-version request) (dec starter/version))
                 (string? (:expected-revision request))
                 (re-matches #"[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}" (:expected-revision request)))
    (throw (ex-info "Invalid wiki starter upgrade" {:type ::invalid})))
  (let [binding (reads/resolve-host-install! host principal original-room-handle)
        snapshot (workflows/snapshot-in (:db binding) starter/name)
        revision (str (owned/source-id binding starter/name (:manifest snapshot)))
        facts (assoc (classify snapshot) :name starter/name :activation :unchanged :revision revision)]
    (reads/validate-host-install! host principal original-room-handle binding)
    (if-not (and (= revision (:expected-revision request)) (= :upgrade-available (:status facts))
                 (= (:from-version request) (:installed-version facts)))
      (assoc facts :status :changed)
      (let [result (authoring/install! host principal original-room-handle
                                       {:name starter/name :expected-revision revision :files (starter/source)})]
        (assoc facts :status :upgraded :installed-version starter/version
                     :revision (get-in result [:source :revision]))))))

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
