(ns is.simm.model.workspace-intent
  "Closed human workspace creation requests and durable outcome observation.
   An observation never retries native preparation or publishes membership."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [hasch.core :as hasch]
            [is.simm.documents.github-source :as github]
            [is.simm.documents.space :as space]
            [is.simm.model.access :as access]))

(defn- refuse! [reason]
  (throw (ex-info "Workspace creation refused" {:type ::refused :reason reason})))

(defn request!
  "Normalize wire data before queuing or looking up any native resource."
  [{:keys [id name members] :as request}]
  (let [canonical-uuid (fn [value]
                         (when (and (string? value) (= value (some-> value parse-uuid str)))
                           (parse-uuid value)))]
    (when-not (and (map? request)
                   (or (= #{:id :name :members} (set (keys request)))
                       (= #{:id :name :members :source} (set (keys request))))
                   (canonical-uuid id) (string? name) (<= 1 (count name) 512)
                   (not (str/blank? name)) (vector? members) (<= (count members) 255)
                   (every? canonical-uuid members))
      (refuse! :invalid-request))
    (cond-> {:id (canonical-uuid id) :name (str/trim name)
             :members (vec (sort-by str (set (map canonical-uuid members))))}
      (contains? request :source) (assoc :source (github/source! (:source request))))))

(defn human! [snapshot creator]
  (let [party (when (uuid? creator) (d/entity snapshot [:party/id creator]))]
    (when-not (and (:db/id party) (= :human (or (:actor/kind party) (:party/type party))))
      (refuse! :human-required)))
  creator)

(defn authorize!
  "Current pure publication check, also repeated before native preparation."
  [snapshot creator request]
  (human! snapshot creator)
  (when-not (every? #(access/enlistable? snapshot creator %) (:members request))
    (refuse! :member-not-enlistable))
  request)

(defn slug [creator request]
  (when-not (and (uuid? creator) (uuid? (:id request))) (refuse! :invalid-identity))
  (str "workspace-" (hasch/uuid [:simmis/workspace-creation 1 creator (:id request)])))

(defn basis [creator request]
  (hasch/uuid {:version 1 :creator creator :request request}))

(defn receipt [creator request]
  {:room/creation-intent (:id request) :room/creation-basis (basis creator request)})

(defn record-source-tx
  "Fixed HOST writer for a newly pending GitHub import, before any network IO.
   Neither creates a native Room nor publishes membership. Exact replay only."
  [snapshot creator request]
  (authorize! snapshot creator request)
  (let [source (github/source! (:source request))
        row (d/pull snapshot '[:db/id :room/id :room/name :room/type :room/slug
                              :room/provisioning-state :room/provisioning-version
                              :room/created-by :room/content-db-scope :room/parties :room/archived-at
                              :room/creation-intent :room/creation-basis :room/github-source
                              {:room/owner [:party/id]}
                              {:room/default-repository [:system/repository-profile]}]
                    [:room/slug (slug creator request)])
        expected (assoc (receipt creator request) :room/github-source (:repository source))
        actual (select-keys row (keys expected))]
    (when-not (and (uuid? (:room/id row)) (= creator (get-in row [:room/owner :party/id]))
                   (= (:name request) (:room/name row)) (= :project (:room/type row))
                   (= :pending (:room/provisioning-state row)) (= 1 (:room/provisioning-version row))
                   (nil? (:room/archived-at row))
                   (= space/profile (get-in row [:room/default-repository :system/repository-profile]))
                   (not-any? #(contains? row %) [:room/created-by :room/content-db-scope :room/parties])
                   (or (empty? actual) (= expected actual)))
      (refuse! :source-intent-changed))
    (if (= expected actual) [] [(assoc expected :db/id (:db/id row))])))

(defn observe
  "Read an exact owner's one-shot intent. Not-observed is uncertain, NOT
   permission to resend: the original worker may not have recorded intent yet."
  [snapshot creator request]
  (human! snapshot creator)
  (let [eid (d/q '[:find ?e . :in $ ?slug :where [?e :room/slug ?slug]] snapshot (slug creator request))
        row (when eid (d/pull snapshot '[:room/id :room/name :room/type :room/provisioning-state
                              :room/provisioning-version :room/content-db-scope :room/created-by
                              :room/creation-intent :room/creation-basis :room/archived-at :room/github-source
                              {:room/owner [:party/id]}
                              {:room/default-repository [:system/repository-profile]}]
                              eid))]
    (cond
      (nil? (:room/id row)) {:status :not-observed}
      (or (not= creator (get-in row [:room/owner :party/id]))
          (not= :project (:room/type row)) (:room/archived-at row)
          (not= space/profile (get-in row [:room/default-repository :system/repository-profile]))
          (not= 1 (:room/provisioning-version row))) {:status :unavailable}
      (or (:room/creation-intent row) (:room/creation-basis row))
      (cond
        (or (not= (receipt creator request) (select-keys row [:room/creation-intent :room/creation-basis]))
            (not= (get-in request [:source :repository]) (:room/github-source row)))
        {:status :conflict}
        (= :pending (:room/provisioning-state row)) {:status :preparing}
        (and (= :ready (:room/provisioning-state row))
             (nil? (:room/content-db-scope row)) (nil? (:room/created-by row)))
        {:status :prepared-not-published}
        (and (= :ready (:room/provisioning-state row)) (= creator (:room/created-by row))
             (uuid? (:room/content-db-scope row))
             (access/can? snapshot creator :read {:room (:room/id row)}))
        {:status :created :room {:room/id (str (:room/id row)) :room/name (:room/name row) :room/type :project}}
        :else {:status :unavailable})
      (not= (:name request) (:room/name row)) {:status :conflict}
      (= :pending (:room/provisioning-state row)) {:status :preparing}
      (and (= :ready (:room/provisioning-state row))
           (nil? (:room/content-db-scope row)) (nil? (:room/created-by row)))
      {:status :prepared-not-published}
      :else {:status :unavailable})))
