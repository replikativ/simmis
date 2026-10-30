(ns is.simm.ops.github-push-roots
  "Physical retention for a prepared outbound Git head. Fixed HOST code owns
   authority and lifetime; durable intent precedes both root creations."
  (:require [datahike.api :as d]
            [dvergr.artifact :as artifact]
            [hasch.core :as hasch]
            [is.simm.model.github-sync :as journal]
            [is.simm.ops.checkpoint-roots :as physical]
            [is.simm.ops.github-host :as host]
            [is.simm.ops.knowledge-changes :as changes]))

(defn- refuse! [reason]
  (throw (ex-info "Git push retention is unavailable" {:type ::held :reason reason})))

(defn body-id [body] (hasch/uuid [:simmis/github-push-retention-v1 body]))

(defn describe
  "Describe the exact logical head snapshot AND post-mapping source checkpoint.
   These are different roots; neither descriptor creates a native pin."
  [source row head oid]
  (when-not (and (= :push (journal/kind row)) (uuid? head)
                 (string? oid) (re-matches #"[0-9a-f]{40}" oid)) (refuse! :invalid-head))
  (let [commit (d/pull source [:geschichte.commit/id :geschichte.commit/git-oid :geschichte.commit/snapshot]
                       [:geschichte.commit/id head])
        snapshot (when-let [id (:geschichte.commit/snapshot commit)] (d/commit-as-db source id))]
    (when-not (and snapshot (= oid (:geschichte.commit/git-oid commit))
                   (= (dissoc (:config source) :branch) (dissoc (:config snapshot) :branch)))
      (refuse! :head-snapshot-unavailable))
    {:version 1 :sync (:github.sync/id row) :claim (:github.sync/claim row)
     :by (:github.sync/by row) :room (:github.sync/room row) :head head :git-oid oid
     :roots {:sent {:id (random-uuid) :basis (changes/basis snapshot)}
             :source {:id (random-uuid) :basis (changes/basis source)}}}))

(defn- snapshot! [source basis]
  (let [snapshot (d/commit-as-db (:store source) (:commit basis))]
    (when-not (and snapshot (= (dissoc (:config source) :branch) (dissoc (:config snapshot) :branch)))
      (refuse! :source-changed))
    (physical/load-snapshot! snapshot basis)))

(defn- binding! [context row body]
  (host/current! context)
  (when-not (and (= :push (journal/kind row))
                 (= row (journal/sync-in @(:conn context) (:github.sync/id row)))
                 (= 1 (:version body))
                 (= (select-keys body [:sync :claim :by :room])
                    {:sync (:github.sync/id row) :claim (:github.sync/claim row)
                     :by (:github.sync/by row) :room (:github.sync/room row)})
                 (= #{:sent :source} (set (keys (:roots body))))
                 (every? uuid? (map #(get-in body [:roots % :id]) [:sent :source]))
                 (not= (get-in body [:roots :sent :id]) (get-in body [:roots :source :id]))
                 (= (body-id body) (:github.push/retention-id row))
                 (= body (:value (artifact/get-bounded-value (artifact/datahike-store (:conn context))
                                                             (:github.push/retention row) 65536))))
    (refuse! :intent-changed))
  (let [source (:db (host/current! context))
        audit (snapshot! source (get-in body [:roots :source :basis]))
        commit (d/pull audit [:geschichte.commit/id :geschichte.commit/git-oid :geschichte.commit/snapshot]
                       [:geschichte.commit/id (:head body)])]
    (when-not (and (= (:head body) (:geschichte.commit/id commit))
                   (= (:git-oid body) (:geschichte.commit/git-oid commit))
                   (= (:geschichte.commit/snapshot commit) (get-in body [:roots :sent :basis :commit])))
      (refuse! :head-changed))
    source))

(defn- owner [row body role]
  {:simmis/github-push (:github.sync/id row) :claim (:github.sync/claim row)
   :by (:github.sync/by row) :room (:github.sync/room row) :role role
   :root (get-in body [:roots role :id]) :retention-id (body-id body)})

(defn verify!
  "Recovery verifies these same roots; it never creates or replaces them."
  [context row body]
  (let [source (binding! context row body)]
    (doseq [role [:sent :source]
            :let [{:keys [id basis]} (get-in body [:roots role]) snapshot (snapshot! source basis)]]
      (physical/verify! snapshot basis id (owner row body role)))
    (host/current! context)
    body))

(defn create-original!
  "Only the original synchronous intent transaction's positive winner creates.
   The report is native caller-owned state, never a persisted or wire argument.
   Partial or unknown pin creation is recovered ONLY through verify!."
  [context row body report]
  (when-not (and report (= :retaining (:github.sync/state row))
                 (= :creating (:github.sync/state (journal/sync-in (:db-before report) (:github.sync/id row))))
                 (= row (journal/sync-in (:db-after report) (:github.sync/id row))))
    (refuse! :not-original-retention))
  (let [source (binding! context row body)]
    (doseq [role [:sent :source]
            :let [{:keys [id basis]} (get-in body [:roots role]) snapshot (snapshot! source basis)]]
      (host/current! context)
      (physical/create! snapshot basis id (owner row body role)))
    (verify! context row body)))
