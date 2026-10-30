(ns is.simm.ops.git-history
  "Read-only evidence for automatic Git integration. Logical commit metadata
   is not authority: ancestry comes from hash-verified raw Git objects, and a
   local OID is usable only with the frozen, byte-verified local tree."
  (:require [clojure.string :as str]
            [geschichte.budget :as budget]
            [geschichte.content :as content]
            [geschichte.git.object :as object]
            [geschichte.git.store :as store]))

(defn- refuse! [reason]
  (throw (ex-info "Git history evidence is unavailable" {:type ::held :reason reason})))

(defn- oid? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{40}" value))))

(defn- commit! [objects oid operation]
  (when-not (oid? oid) (refuse! :invalid-history))
  (budget/charge! operation :commit-visits 1)
  (let [row (store/read-object objects oid {:import-budget operation})
        payload (:payload row)]
    (when-not (and (= :commit (:geschichte.git.object/type row)) payload
                   (= oid (object/object-id :commit payload)))
      (refuse! :history-unavailable))
    (let [parsed (object/parse-commit payload)
          parents (:parents parsed)]
      (when-not (and (= 1 (count (filter #(= "tree" (first %)) (:headers parsed))))
                     (oid? (:tree parsed)) (<= (count parents) 16) (every? oid? parents))
        (refuse! :invalid-history))
      (budget/charge! operation :graph-edges (count parents))
      parsed)))

(defn proven-ancestor?
  "A concrete path in bounded raw Git history, never logical UUID parent edges.
   Missing/truncated objects refuse; false requires a fully explored graph.
   Caller supplies an immutable pack catalog and owns its retention."
  [db ancestor descendant operation]
  (budget/require-limits! operation [:commit-visits :graph-edges :object-bytes])
  (when-not (every? oid? [ancestor descendant]) (refuse! :invalid-history))
  (store/with-store
   (atom db)
   (fn [objects]
     (loop [pending [descendant] seen #{}]
       (budget/check! operation)
       (if-let [id (peek pending)]
         (if (contains? seen id)
           (recur (pop pending) seen)
           (let [parsed (commit! objects id operation)]
             (if (= id ancestor) true
                 (recur (into (pop pending) (:parents parsed)) (conj seen id)))))
         false)))))

(defn- directory-paths [files]
  (reduce (fn [dirs path]
            (loop [dirs dirs path path]
              (let [index (.lastIndexOf ^String path "/")]
                (if (pos? index)
                  (let [parent (subs path 0 index)] (recur (conj dirs parent) parent))
                  (conj dirs "/")))))
          #{"/"} (keys files)))

(defn- tree-oid [entries operation depth]
  (budget/check! operation)
  ;; Paths have already passed the bounded canonical manifest validator.
  (when (> depth 64) (refuse! :tree-limit))
  (let [groups (group-by #(first (:segments %)) entries)
        tree (mapv
              (fn [[name children]]
                (budget/charge! operation :tree-entries 1)
                (if (= 1 (count (:segments (first children))))
                  (do (when-not (= 1 (count children)) (refuse! :invalid-tree))
                      (assoc (select-keys (first children) [:mode :oid]) :name name))
                  {:name name :mode "40000"
                   :oid (tree-oid (mapv #(update % :segments subvec 1) children) operation (inc depth))}))
              groups)
        payload (object/tree-payload tree)]
    (budget/bound! operation :object-bytes (alength ^bytes payload))
    (object/object-id :tree payload)))

(defn assert-local-tree!
  "Bind a claimed local commit OID to the complete PRE-IMPORT local filesystem.
   Reconstruct content with native finite readers and verify its hashes. Git
   cannot represent extra empty directories; hold those rather than lose them.
   pack-db may include a newly fetched pack; source-db and manifest must precede
   checkout/materialization, so imported metadata cannot redefine local files."
  [pack-db source-db manifest local-oid operation]
  (when-not (= (set (:directories manifest)) (directory-paths (:files manifest)))
    (refuse! :unrepresented-directories))
  (let [snapshot (atom source-db)
        entries (mapv
                 (fn [[path {:keys [content size mode]}]]
                   (budget/check! operation)
                   (let [payload (content/read-by-id snapshot content {:merge-budget operation})]
                     (when-not (and payload (= size (alength ^bytes payload)) (#{33188 33261} mode))
                       (refuse! :invalid-tree))
                     {:segments (vec (str/split (subs path 1) #"/"))
                      :mode (if (= 33261 mode) "100755" "100644")
                      :oid (object/object-id :blob payload)}))
                 (:files manifest))
        actual (tree-oid entries operation 0)
        expected (store/with-store (atom pack-db) #(:tree (commit! % local-oid operation)))]
    (when-not (= expected actual) (refuse! :local-history-mismatch))
    true))
