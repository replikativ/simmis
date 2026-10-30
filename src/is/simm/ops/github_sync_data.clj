(ns is.simm.ops.github-sync-data
  "Bounded immutable Git-visible facts for HOST preparation and comparison.
   No transport, native construction, publication or authority."
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.query :as query]
            [geschichte.repo :as repo]
            [hasch.core :as hasch]
            [is.simm.documents.github-source :as github]
            [is.simm.ops.benchmark-case-manifest :as manifests]))

(def max-artifact-bytes (* 2 1024 1024))
(def max-observation-bytes (* 256 1024))

(defn git-oid? [value]
  (and (string? value) (boolean (re-matches #"[0-9a-f]{40}" value))))

(defn refuse! [reason]
  (throw (ex-info "GitHub sync is held" {:type ::held :reason reason})))

(defn error-reason
  "Closed public diagnostics only; never forward exception text or data."
  [error]
  (or (some (fn [e]
              (let [{:keys [type reason] :as data} (ex-data e)]
                (or (case type
                      :geschichte.workspace/stale-advance :target-changed
                      :geschichte.git.budget/exceeded :limit
                      :geschichte.git.budget/deadline-exceeded :timeout
                      :geschichte.git.budget/cancelled :cancelled
                      :geschichte.git.client/unsupported-local-projection :local-history
                      :is.simm.ops.owned-reads/refused :access-unverified
                      :is.simm.model.github-integrations/refused
                      (when (#{:cursor-changed :unverified-base} reason) :integration-unverified)
                      nil)
                    (case (:pull/status data) :diverged :diverged :local-changes :dirty :missing-branch :source-changed nil)
                    (when (#{::held :is.simm.ops.github-import/held :is.simm.ops.owned-workspace/refused
                             :is.simm.ops.github-incoming/held :is.simm.ops.github-roots/held
                             :is.simm.ops.github-native/held} type)
                      (case reason
                        (:conflicts :document-validation :markdown-removal-requires-review :incoming-reserved
                         :integration-unverified :retention-unavailable :unsupported-native-runtime) reason
                        (:dirty :pending-merge) :dirty
                        :busy :busy
                        (:metadata-limit :proof-too-large) :limit
                        (:target-changed :invalid-mapping) :target-changed
                        (:source-settings-changed :source-unavailable) :source-changed
                        :local-commits :local-history
                        nil)))))
            (take 16 (take-while some? (iterate ex-cause error))))
      :unconfirmed))

(defn branch [id claim]
  (when-not (every? uuid? [id claim]) (refuse! :invalid-identity))
  (keyword "geschichte.workspace" (str "github-sync-" (hasch/uuid [:simmis/github-sync-branch-v1 id claim]))))

(defn body-id [kind body]
  (when (> (count (pr-str body)) max-artifact-bytes) (refuse! :proof-too-large))
  (when (> (alength (.getBytes ^String (pr-str body) "UTF-8")) max-artifact-bytes)
    (refuse! :proof-too-large))
  (hasch/uuid [:simmis/github-sync-artifact-v1 kind body]))

(defn- bounded-index! [db attribute maximum]
  (when (> (count (take (inc maximum) (d/datoms db :avet attribute))) maximum)
    (refuse! :metadata-limit)))

(defn origin!
  "Validate retained HOST origin; it cannot be inferred from authored files."
  [{:keys [repository ref import-head registry-system] :as origin}]
  (when-not (and (map? origin) (= #{:repository :ref :import-head :registry-system} (set (keys origin)))
                 (= repository (github/repository! repository))
                 (string? ref) (<= 12 (count ref) 512) (str/starts-with? ref "refs/heads/")
                 (not (re-find #"[\\\p{Cntrl}]" ref))
                 (every? uuid? [import-head registry-system]))
    (refuse! :source-settings-changed))
  origin)

(defn capture
  "One immutable snapshot, with metadata bounds before index projections.
   Regular/executable files and explicit empty directories retain exact facts."
  [db origin]
  (origin! origin)
  (doseq [[attribute maximum] [[:geschichte.repo/id 1]
                               [:geschichte.stage/path (:max-files manifests/policy)]
                               [:geschichte.config/key 16] [:geschichte.ref/name 64]]]
    (bounded-index! db attribute maximum))
  (let [repository-id (:v (first (d/datoms db :avet :geschichte.repo/id)))
        _ (when-not (uuid? repository-id) (refuse! :source-unavailable))
        manifest (manifests/inventory db)
        ref (:ref origin) name (subs ref 11)
        expected-config {"remote.origin.url" (github/public-url {:kind :github :repository (:repository origin)})
                         "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*"
                         (str "branch." name ".remote") "origin"
                         (str "branch." name ".merge") ref}
        configuration (query/configuration db)
        repository (d/pull db [:geschichte.repo/id :geschichte.repo/name :geschichte.repo/head
                               :geschichte.repo/merge-parent :document.repository/profile]
                           [:geschichte.repo/id repository-id])
        repository (dissoc repository :db/id)
        refs (query/refs db)
        head (get refs ref)
        status (repo/status (atom db))
        checkpoint (d/commit-id db)]
    (when-not (and (= ref (:geschichte.repo/head repository)) (= configuration expected-config)
                   (or (nil? (:geschichte.repo/name repository))
                       (and (string? (:geschichte.repo/name repository))
                            (<= (count (:geschichte.repo/name repository)) 512))))
      (refuse! :source-settings-changed))
    (when (:geschichte.repo/merge-parent repository) (refuse! :pending-merge))
    (when-not (and (:clean? status) (empty? (:untracked status))) (refuse! :dirty))
    (when-not (every? uuid? [head checkpoint]) (refuse! :source-unavailable))
    (let [result {:checkpoint checkpoint
                  :git-oid (:geschichte.commit/git-oid (query/commit db head))
                  :visible {:repository repository :configuration configuration :refs refs :stage (query/stage db)
                            :explicit-directories (vec (sort (map :v (d/datoms db :avet :geschichte.work-dir/path))))
                            :manifest manifest :ref ref :head head}}
          printed (pr-str result)]
      ;; Two observations plus the path diff must fit the shared artifact cap.
      ;; Apply this SAME limit before publishing an imported Memory as well.
      (when (or (> (count printed) max-observation-bytes)
                (> (alength (.getBytes ^String printed "UTF-8")) max-observation-bytes))
        (refuse! :metadata-limit))
      result)))

(defn changes [before after]
  (let [before (get-in before [:visible :manifest :files])
        after (get-in after [:visible :manifest :files])]
    (->> (set/union (set (keys before)) (set (keys after))) sort
         (keep (fn [path]
                 (let [a (get before path) b (get after path)]
                   (when-not (= a b)
                     (cond-> {:path (subs path 1) :kind (cond (nil? a) :added (nil? b) :deleted :else :modified)}
                       a (assoc :before a) b (assoc :after b)))))) vec)))

(defn same-visible? [a b] (= (:visible a) (:visible b)))

(defn validate-desired!
  "Native advance changes one checked-out ref and committed file facts only."
  [before after]
  (let [ref (get-in before [:visible :ref])
        old (:visible before) new (:visible after)]
    (when-not (and (= ref (:ref new))
                   (= (:configuration old) (:configuration new))
                   (= (:repository old) (:repository new))
                   (= (:refs new) (assoc (:refs old) ref (:head new)))
                   (= (:explicit-directories old) (:explicit-directories new)))
      (refuse! :unsupported-preparation))
    (when-not (and (git-oid? (:git-oid before)) (git-oid? (:git-oid after)))
      (refuse! :local-commits))
    after))
