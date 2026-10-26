(ns is.simm.ops.knowledge-changes
  "HOST metadata discovery over authorized, retained published snapshots.
   This is a net-state comparison, not an event log or processing receipt.
   Callers own authorization and snapshot retention. No cursor writes, pins,
   content reads, execution, resource grants or provider access happen here."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [datahike.api :as d]
            [datahike.db.interface :as dbi]
            [datahike.experimental.diff :as diff]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.files :as files]
            [is.simm.ops.knowledge-inputs :as inputs])
  (:import [datahike.db DB]))

(def limits
  ;; Both index counts are checked BEFORE diff allocates its result. This
  ;; bounds datom counts, not arbitrary value sizes, peak RSS or tree-node IO.
  ;; Larger rooms need bounded upstream traversal or explicit reconciliation.
  {:max-snapshot-datoms 100000 :max-inventory-entries 20000
   :max-changed-files 20000 :max-ancestor-commits 4096})

(defn- refuse! [reason]
  (throw (ex-info "Incremental knowledge discovery is unavailable"
                  {:type ::refused :reason reason})))

(defn- snapshot! [db]
  (when-not (and (instance? DB db)
                 (= :datahike.index/persistent-set (get-in db [:config :index])))
    (refuse! :unsupported-snapshot))
  (when (> (count db) (:max-snapshot-datoms limits)) (refuse! :snapshot-too-large))
  (let [id (d/commit-id db)
        published (when (uuid? id) (d/commit-as-db db id))
        config-keys [:store :branch :index :attribute-refs? :crypto-hash?]]
    (when-not published (refuse! :published-snapshot-unavailable))
    (when (> (count published) (:max-snapshot-datoms limits)) (refuse! :snapshot-too-large))
    ;; d/with can preserve the old commit id while changing the database. A
    ;; durable pin of that id would then protect DIFFERENT data. Verify the
    ;; exact published state before using the id as a discovery checkpoint.
    ;; Structural equality here also skips shared subtrees; no whole-tree seq.
    (when-not (and (= (select-keys db [:max-tx :max-eid :hash :schema])
                       (select-keys published [:max-tx :max-eid :hash :schema]))
                   (= (select-keys (:config db) config-keys)
                      (select-keys (:config published) config-keys))
                   (every? empty? (vals (diff/diff published db))))
      (refuse! :unpublished-snapshot)))
  db)

(defn source-identity
  "HOST physical metadata only, without enumerating the source. This is not a
   published checkpoint, authorization or lease. Retention cleanup may inspect
   an authorized current head larger than the discovery cardinality limit."
  [db]
  (when-not (and (instance? DB db)
                 (= :datahike.index/persistent-set (get-in db [:config :index])))
    (refuse! :unsupported-snapshot))
  (let [value {:repository (:geschichte.repo/id (query/repository db))
               :store (get-in db [:config :store :id])
               :backend (get-in db [:config :store :backend])
               :branch (get-in db [:config :branch]) :ref (query/current-ref db)
               :configuration (hasch/uuid (select-keys (:config db)
                                                       [:store :branch :index :attribute-refs? :crypto-hash?]))}]
    (when-not (and (every? uuid? (map value [:repository :store]))
                   (keyword? (:backend value)) (keyword? (:branch value))
                   (string? (:ref value)) (not (str/blank? (:ref value))))
      (refuse! :invalid-basis))
    value))

(defn basis
  "Verified published checkpoint and bounded cardinality, without file bodies.
   A descriptor is not a pin, capability or ancestry proof."
  [db]
  (snapshot! db)
  (let [commit (d/commit-id db)]
    (when-not (and (uuid? commit) (integer? (:max-tx db))) (refuse! :invalid-basis))
    (assoc (source-identity db) :commit commit :max-tx (:max-tx db) :datoms (count db))))

(defn- role [policy path]
  (when (and (string? path) (re-find #"(?i)\.(md|markdown)$" path)
             (not-any? #(str/starts-with? path (str % "/")) (:exclude-prefixes policy)))
    (cond (some #(str/starts-with? path (str % "/")) (:trigger-roots policy)) :trigger
          (some #(str/starts-with? path (str % "/")) (:context-roots policy)) :context)))

(def ^:private metadata-pattern
  '[:geschichte.work/path :geschichte.work/size :geschichte.work/mode
    {:geschichte.work/content [:geschichte.content/id]}])

(defn- entry [db policy eid]
  (let [row (d/pull db metadata-pattern eid) path (:geschichte.work/path row)]
    (when-let [kind (role policy path)]
      (files/path! path)
      (let [value {:path path :role kind :content (get-in row [:geschichte.work/content :geschichte.content/id])
                   :size (:geschichte.work/size row) :mode (:geschichte.work/mode row)}]
        (when-not (and (uuid? (:content value)) (integer? (:size value)) (<= 0 (:size value))
                       (= repo/default-mode (:mode value))
                       (= (:size value)
                          (:geschichte.content/size (d/entity db [:geschichte.content/id (:content value)]))))
          (refuse! :unsupported-file))
        value))))

(defn inventory
  "Bounded initial Markdown metadata inventory, independent of the canonical
   200-body projection and 99-input execution limit. Binary/code files count
   toward scan bounds but are never selected or loaded. No implicit truncation."
  [db policy]
  (let [policy (inputs/normalize-policy policy) snapshot (basis db)
        rows (vec (take (inc (:max-inventory-entries limits))
                        (d/datoms db :avet :geschichte.work/path)))]
    (when (> (count rows) (:max-inventory-entries limits)) (refuse! :inventory-too-large))
    {:basis snapshot :policy policy :order :path
     :files (into (sorted-map)
                  (keep (fn [datom] (when-let [value (entry db policy (:e datom))]
                                      [(:path value) (dissoc value :path)]))) rows)}))

(defn- ancestor! [before after snapshot]
  ;; A branch name and increasing tx counter do not prove lineage. Walk only
  ;; published commit metadata, with an explicit budget. A missing intermediate
  ;; after GC holds for reconciliation; it must never mean an empty delta.
  (let [wanted (d/commit-id before)]
    (loop [pending [snapshot] seen #{}]
      (if-let [current (peek pending)]
        (let [pending (pop pending) id (:commit current)]
          (cond
            (= id wanted) true
            (contains? seen id) (recur pending seen)
            (>= (count seen) (:max-ancestor-commits limits)) (refuse! :ancestry-limit)
            :else
            (let [db (if (= id (d/commit-id after)) after (d/commit-as-db after id))]
              (when-not db (refuse! :ancestry-unavailable))
              (let [parents (vec (take (inc (:max-ancestor-commits limits)) (d/parent-commit-ids db)))
                    remaining (into (set (map :commit pending)) parents)]
                (when (or (> (count parents) (:max-ancestor-commits limits))
                          (> (count remaining) (:max-ancestor-commits limits)))
                  (refuse! :ancestry-limit))
                (when-not (every? uuid? parents) (refuse! :invalid-ancestry))
                (if (contains? remaining wanted)
                  true
                  (recur (mapv #(hash-map :commit %) (sort-by str (remove seen remaining)))
                         (conj seen id)))))))
        (refuse! :not-descendant)))))

(defn between
  "Metadata-only net file changes between retained snapshots of ONE source.
   Renames are removal+addition; deletion never means delete a wiki page.
   Resolve both sides: removed datoms carry their ASSERTION tx, not deletion
   time. Output order is lexical path order, not asserted event chronology.
   Unsupported/reset/missing baselines hold before any source body is read."
  [before after policy]
  (let [policy (inputs/normalize-policy policy) old (basis before) new (basis after)
        identity-keys [:repository :store :backend :branch :ref]
        encoding-keys [:index :attribute-refs? :crypto-hash?]]
    (when-not (and (= (select-keys old identity-keys) (select-keys new identity-keys))
                   (= (get-in before [:config :store]) (get-in after [:config :store]))
                   (= (select-keys (:config before) encoding-keys)
                      (select-keys (:config after) encoding-keys)))
      (refuse! :source-changed))
    (when (> (:max-tx old) (:max-tx new)) (refuse! :cursor-reversed))
    (ancestor! before after new)
    (let [delta (diff/diff before after)
          attributes #{:geschichte.work/path :geschichte.work/content :geschichte.work/size :geschichte.work/mode}
          affected (fn [db datoms]
                     (into #{} (keep #(when (contains? attributes (dbi/ident-for db (:a %) :error-on-missing)) (:e %))) datoms))
          ids (set/union (affected before (:removed delta)) (affected after (:added delta)))
          pairs (mapv (fn [id] [(entry before policy id) (entry after policy id)]) (sort ids))
          old-files (into (sorted-map) (keep (fn [[value _]] (when value [(:path value) (dissoc value :path)]))) pairs)
          new-files (into (sorted-map) (keep (fn [[_ value]] (when value [(:path value) (dissoc value :path)]))) pairs)
          changes (into [] (keep (fn [path]
                                   (let [a (get old-files path) b (get new-files path)]
                                     (when (not= a b)
                                       {:path path :kind (cond (nil? a) :added (nil? b) :removed :else :modified)
                                        :before a :after b}))))
                        (sort (set/union (set (keys old-files)) (set (keys new-files)))))]
      (when (> (count changes) (:max-changed-files limits)) (refuse! :too-many-changed-files))
      {:before old :after new :policy policy :order :path :changes changes})))
