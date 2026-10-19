(ns is.simm.documents.repository
  "Experimental colocated document/projection storage over Geschichte.

   NOT registered as a live system, RPC or sandbox tool. Callers must own and
   authorize the supplied connection. Proves atomic source/projection writes;
   does not yet replace KB entities or provide stable legacy block identities.
   All offsets/nodes are a disposable projection, never a second source."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [geschichte.content :as content]
            [geschichte.repo :as repo]
            [is.simm.documents.markdown :as markdown]))

(def schema
  (into [{:db/ident :document/id :db/valueType :db.type/uuid
          :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
         {:db/ident :document/path :db/valueType :db.type/string
          :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
         {:db/ident :document/source-content :db/valueType :db.type/ref
          :db/cardinality :db.cardinality/one}
         {:db/ident :document/nodes :db/valueType :db.type/ref
          :db/cardinality :db.cardinality/many :db/isComponent true}
         {:db/ident :document.node/children :db/valueType :db.type/ref
          :db/cardinality :db.cardinality/many :db/isComponent true}]
        (for [[attr type] [[:document/revision :db.type/uuid]
                           [:document/projection-version :db.type/long]
                           [:document/valid? :db.type/boolean]
                           [:document.node/kind :db.type/keyword]
                           [:document.node/heading-level :db.type/long]
                           [:document.node/ordinal :db.type/long]
                           [:document.node/checked? :db.type/boolean]
                           [:document.node/table-header? :db.type/boolean]
                           [:document.node/table-alignment :db.type/keyword]
                           [:document.node/from :db.type/long]
                           [:document.node/to :db.type/long]]]
          {:db/ident attr :db/valueType type :db/cardinality :db.cardinality/one})))

(defn initialize!
  "Install into a NEW disposable/explicitly provisioned knowledge repository."
  [conn]
  (repo/init! conn {:name "Knowledge documents"})
  (let [installed (set (d/q '[:find [?a ...] :where [_ :db/ident ?a]] @conn))
        missing (remove #(contains? installed (:db/ident %)) schema)]
    (when (seq missing) (d/transact conn (vec missing))))
  conn)

(defn- invalid! [message]
  (throw (ex-info message {:type ::invalid})))

(defn- path! [path]
  ;; No implicit normalization: the identity checked here must be the exact
  ;; Geschichte worktree key checked inside the transaction.
  (when-not (and (string? path) (<= 1 (count path) 512)
                 (not (str/includes? path "\\"))
                 (not (re-find #"[\p{Cntrl}]" path))
                 (every? #(and (not (str/blank? %)) (not (#{"." ".." ".git" ".geschichte"} %)))
                         (str/split path #"/" -1)))
    (invalid! "Invalid document path"))
  path)

(defn- row [db id]
  (when-let [eid (d/q '[:find ?e . :in $ ?id :where [?e :document/id ?id]] db id)]
    (d/pull db '[* {:document/source-content [:geschichte.content/id]}] eid)))

(defn- stamp [document]
  (when document
    {:revision (:document/revision document) :path (:document/path document)
     :content (get-in document [:document/source-content :geschichte.content/id])}))

(defn- work-content [db path]
  (when-let [eid (d/q '[:find ?e . :in $ ?path :where
                        [?e :geschichte.work/path ?path]] db path)]
    (get-in (d/pull db '[{:geschichte.work/content [:geschichte.content/id]}] eid)
            [:geschichte.work/content :geschichte.content/id])))

(defn- check-current! [db id expected target]
  (let [current (row db id)
        prior-path (:document/path current)
        other (d/q '[:find ?id . :in $ ?path :where
                     [?e :document/path ?path] [?e :document/id ?id]] db target)]
    (when-not (and (= expected (stamp current))
                   (or (nil? current)
                       (= (:content expected) (work-content db prior-path)))
                   (or (nil? other) (= other id))
                   (or (= prior-path target) (nil? (work-content db target))))
      (throw (ex-info "Document or destination changed; reload before writing"
                      {:type ::conflict})))
    current))

(defn- node-tx [ordinal {:keys [kind from to children heading-level checked? table-header? table-alignment]}]
  (cond-> {:document.node/kind kind :document.node/from from :document.node/to to
           :document.node/ordinal (long ordinal)}
    heading-level (assoc :document.node/heading-level (long heading-level))
    (some? checked?) (assoc :document.node/checked? checked?)
    (some? table-header?) (assoc :document.node/table-header? table-header?)
    table-alignment (assoc :document.node/table-alignment table-alignment)
    (seq children) (assoc :document.node/children (mapv node-tx (range) children))))

(defn publish-tx
  "Pure Datahike transaction function. CAS and projection publish atomically."
  [db id expected path content-id projection]
  (let [current (check-current! db id expected path)]
    (vec
     (concat
      (for [node (:document/nodes current)] [:db/retractEntity (:db/id node)])
      [{:geschichte.work/path path :geschichte.work/content [:geschichte.content/id content-id]
        :geschichte.work/size (:size projection)
        :geschichte.work/mode (long repo/default-mode)}
       (cond-> {:document/id id :document/path path
                :document/source-content [:geschichte.content/id content-id]
                :document/revision (:revision projection) :document/projection-version 2
                :document/valid? (:valid? projection)}
         (seq (:nodes projection)) (assoc :document/nodes (:nodes projection)))]))))

(defn read-document
  "Read one immutable DB view. An external file write invalidates the view
   explicitly rather than returning old projected knowledge as current."
  [conn id]
  (let [db @conn]
    (when-let [document (row db id)]
      (let [{:keys [path content]} (stamp document)]
        (when-not (= content (work-content db path))
          (throw (ex-info "Document projection does not match its source"
                          {:type ::stale-projection})))
        (let [source (markdown/decode (content/read-by-id (atom db) content))
              parsed (markdown/parse source)]
          (when-not (= (:document/revision document) (:revision parsed))
            (throw (ex-info "Document revision does not match its source"
                            {:type ::corrupt})))
          (assoc parsed :id id :path path :basis (stamp document)))))))

(defn- publish! [conn id expected path parsed]
  (let [bytes (markdown/source-bytes (:source parsed))]
    (content/transact-content!
     conn bytes (:content expected)
     (fn [content-id]
       ;; Transaction failures can be logged by the writer. Pass source-free
       ;; projection data, never document text/frontmatter, into the tx function.
       [[:db.fn/call publish-tx id expected path content-id
         {:revision (:revision parsed) :size (long (alength bytes))
          :valid? (empty? (:diagnostics parsed)) :nodes (mapv node-tx (range) (:nodes parsed))}]]))
    {:id id :path path :revision (:revision parsed)}))

(defn create! [conn {:keys [id path source]}]
  (when-not (uuid? id) (invalid! "A document requires an explicit UUID"))
  (path! path)
  (publish! conn id nil path (markdown/parse source)))

(defn edit! [conn id expected-revision edits]
  (let [document (or (read-document conn id) (invalid! "No such document"))
        parsed (markdown/patch (:source document) expected-revision edits)]
    (if (= (:revision document) (:revision parsed))
      ;; No-op views/edits must not create transactions or rewrite the file.
      {:id id :path (:path document) :revision (:revision document) :unchanged? true}
      (publish! conn id (:basis document) (:path document) parsed))))

(defn move-tx [db id expected target]
  (let [current (check-current! db id expected target)
        old (:document/path current)]
    (when-not current (invalid! "No such document"))
    (if (= old target)
      []
      [[:db/retractEntity [:geschichte.work/path old]]
     {:geschichte.work/path target
      :geschichte.work/content [:geschichte.content/id (:content expected)]
      :geschichte.work/size (:geschichte.work/size
                             (d/entity db [:geschichte.work/path old]))
      :geschichte.work/mode (long repo/default-mode)}
       {:document/id id :document/path target}])))

(defn move! [conn id expected-basis target]
  (path! target)
  (d/transact conn [[:db.fn/call move-tx id expected-basis target]])
  {:id id :path target :revision (:revision expected-basis)})
