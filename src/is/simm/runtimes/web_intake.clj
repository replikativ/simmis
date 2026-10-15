(ns is.simm.runtimes.web-intake
  "Web-page intake — the browser extension's captured DOM, per USER
   (doc/archive/web-intake-design.md).

   A Chrome/Brave extension (../dvergr/resources/extension) passively grabs the
   DOM of pages the user browses and POSTs them to `/pages`, authenticated with
   the user's JWT. Each capture is stored in the OWNER's page archive — raw HTML
   as a CAS blob, the visible text fulltext-indexed, url/title/host/meta as
   queryable facts. Agents mine it through the `web/*` SCI vocabulary (data, not
   a tool); intake agents (Phase 2) extract contacts, events, topics and product
   issues into KBs.

   Personal by construction: browsing is more sensitive than a deliberate screen
   share, so a user's pages feed only their OWN agents (their personal room),
   never a shared room, unless explicitly shared later.

   Security posture (Phase 1): JWT-authenticated (captures are assignable to a
   user, and unauthenticated ones are refused); TLS in transit; DOM content is
   NEVER logged. At-rest encryption of this store arrives with konserve AEAD,
   covering pages/screens/recordings together — Phase 1 is plaintext on disk,
   like the screenshots, and we do not pretend otherwise."
  (:require [is.simm.runtimes.instance :as instance]
            [is.simm.model.parties :as parties]
            [is.simm.model.blobs :as blobs]
            [clojure.string :as str]
            [datahike.api :as d]
            [taoensso.telemere :as log]))

;; =============================================================================
;; Limits
;; =============================================================================

;; A single captured DOM. Most pages are well under this; a runaway page is
;; truncated rather than allowed to bloat the store.
(def ^:const max-html-bytes (* 8 1024 1024))
;; innerText is already capped client-side (~50k); mirror it server-side.
(def ^:const max-text-chars 60000)
;; Keep the newest N pages per user; older ones age out (blobs are the bulk).
(def ^:const max-pages-per-user 5000)

;; =============================================================================
;; Per-user pages store (metadata + fulltext over the visible text)
;; =============================================================================

(def ^:private pages-schema
  [{:db/ident :page/id :db/valueType :db.type/uuid
    :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
   {:db/ident :page/at :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :page/url :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :page/title :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :page/text :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :page/host :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one :db/index true}
   {:db/ident :page/blob-id :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
   {:db/ident :page/meta :db/valueType :db.type/string :db/cardinality :db.cardinality/one}])

(def pages-fulltext-ident :pages/fulltext)

(defonce ^:private conns (atom {}))

(defn- pages-cfg [party-uuid]
  {:store {:backend :file
           :path (instance/data-path "simmis-pages" (str party-uuid))
           ;; STABLE id (the screens-cfg lesson): a random id per connect fails
           ;; to reconnect to an existing store after a restart.
           :id (java.util.UUID/nameUUIDFromBytes
                 (.getBytes (str "simmis-pages/" party-uuid) "UTF-8"))}
   :schema-flexibility :write
   :keep-history? false
   :crypto-hash? true})

(defn ensure-conn
  "Connection to a USER's pages DB, creating DB + schema + fulltext index on
   first use."
  [party-uuid]
  (or (get @conns party-uuid)
      (locking conns
        (or (get @conns party-uuid)
            (let [cfg (pages-cfg party-uuid)
                  _ (when-not (d/database-exists? cfg) (d/create-database cfg))
                  conn (d/connect cfg)]
              (d/transact conn pages-schema)
              (when-not (:db.secondary/type (d/entity @conn pages-fulltext-ident))
                (try ((requiring-resolve 'dvergr.search.secondary/declare-index!)
                      conn pages-fulltext-ident [:page/text]
                      (instance/data-path "simmis-pages" (str party-uuid "-ft")))
                     (catch Throwable t
                       (log/log! {:level :warn :id ::fulltext-declare-failed
                                  :data {:party party-uuid :error (ex-message t)}}))))
              (swap! conns assoc party-uuid conn)
              conn)))))

(defn- host-of [url]
  (try (.getHost (java.net.URI. url)) (catch Throwable _ nil)))

(defn- prune! [party-uuid]
  ;; keep the newest max-pages-per-user; drop the rest (metadata only — blobs
  ;; are content-addressed and GC'd separately).
  (let [conn (ensure-conn party-uuid)
        ids (->> (d/q '[:find ?e ?at :where [?e :page/at ?at]] @conn)
                 (sort-by second >)
                 (drop max-pages-per-user)
                 (map first))]
    (when (seq ids)
      (d/transact conn (mapv (fn [e] [:db/retractEntity e]) ids))
      (log/log! {:level :info :id ::pages-pruned
                 :data {:party party-uuid :dropped (count ids)}}))))

(defn handle-page!
  "Store a captured page for `party-uuid` (the authenticated user). `page` is the
   extension payload {:url :title :html :text :meta}. Returns a ring response.
   DOM content is never logged."
  [party-uuid {:keys [url title html text meta]}]
  (cond
    (nil? party-uuid) {:status 401 :body "unauthenticated"}
    (str/blank? (str url)) {:status 400 :body "missing url"}
    :else
    (try
      (let [now (System/currentTimeMillis)
            html-str (let [s (str html)]
                       (if (> (count s) max-html-bytes) (subs s 0 max-html-bytes) s))
            blob (blobs/store! (.getBytes html-str "UTF-8") "text/html")
            text* (let [s (str text)]
                    (if (> (count s) max-text-chars) (subs s 0 max-text-chars) s))]
        (d/transact (ensure-conn party-uuid)
                    [{:page/id (java.util.UUID/randomUUID)
                      :page/at now
                      :page/url (str url)
                      :page/title (str (or title ""))
                      :page/text text*
                      :page/host (or (host-of (str url)) "")
                      :page/blob-id (:blob/id blob)
                      :page/meta (pr-str (or meta {}))}])
        (future (try (prune! party-uuid) (catch Throwable _ nil)))
        (log/log! {:level :info :id ::page-captured
                   :data {:party party-uuid :host (host-of (str url))
                          :bytes (count html-str)}})   ; host only — never the content
        {:status 200 :body "captured"})
      (catch Throwable t
        (log/log! {:level :error :id ::page-failed
                   :data {:party party-uuid :error (str t)}})
        {:status 500 :body "capture failed"}))))

;; =============================================================================
;; Reads — the owner's own archive
;; =============================================================================

(defn- ->page [e]
  {:id (str (:page/id e)) :at (:page/at e) :url (:page/url e)
   :title (:page/title e) :host (:page/host e) :text (:page/text e)
   :blob-id (:page/blob-id e)})

(defn recent [party-uuid n]
  (let [db @(ensure-conn party-uuid)]
    (->> (d/q '[:find [?e ...] :where [?e :page/at _]] db)
         (map #(d/entity db %))
         (sort-by :page/at >)
         (take n)
         (mapv ->page))))

(defn search [party-uuid query n]
  (if (str/blank? (str query))
    (recent party-uuid n)
    (let [db @(ensure-conn party-uuid)]
      (->> ((requiring-resolve 'dvergr.search.secondary/search)
            db pages-fulltext-ident (str query))
           (take n)
           (mapv (fn [[eid score]] (assoc (->page (d/entity db eid)) :score score)))))))

(defn since [party-uuid ms]
  (let [db @(ensure-conn party-uuid)]
    (->> (d/q '[:find [?e ...] :in $ ?t :where [?e :page/at ?at] [(>= ?at ?t)]] db ms)
         (map #(d/entity db %))
         (sort-by :page/at >)
         (mapv ->page))))

(defn hosts [party-uuid]
  (let [db @(ensure-conn party-uuid)]
    (->> (d/q '[:find ?host (count ?e) :where [?e :page/host ?host]] db)
         (map (fn [[h c]] {:host h :count c}))
         (sort-by :count >)
         vec)))

(defn page-text
  "The full stored text of one of the owner's pages by id."
  [party-uuid page-id-str]
  (let [db @(ensure-conn party-uuid)
        id (try (parse-uuid page-id-str) (catch Throwable _ nil))
        e (when id (d/entity db [:page/id id]))]
    (when e (:page/text e))))

(defn delete-page! [party-uuid page-id-str]
  (let [conn (ensure-conn party-uuid)
        id (try (parse-uuid page-id-str) (catch Throwable _ nil))
        eids (when id (d/q '[:find [?e ...] :in $ ?id :where [?e :page/id ?id]] @conn id))]
    (when (seq eids)
      (d/transact conn (mapv (fn [e] [:db/retractEntity e]) eids)))
    (log/log! {:level :info :id ::page-deleted :data {:party party-uuid :page id}})
    {:deleted (count (or eids []))}))

;; =============================================================================
;; The agent's window — the `web/*` SCI vocabulary (owner-scoped, DATA)
;; =============================================================================

(defn sci-namespace
  "Build the `web/*` map for `sci/add-namespace!`, bound to the OWNER `party`.
   Everything returns plain data the agent can filter/aggregate; it is the
   owner's OWN browsing archive (personal — installed only in personal rooms)."
  [party]
  {'recent (fn ([] (recent party 20)) ([n] (recent party n)))
   'search (fn ([q] (search party q 20)) ([q n] (search party q n)))
   'page   (fn [id] (page-text party id))
   'since  (fn [ms] (since party ms))
   'hosts  (fn [] (hosts party))})

(def prompt-block
  (str "\n\nYOUR WEB CAPTURES: pages you browse (via the simmis browser"
       " extension) are archived to YOU and readable as DATA through `web/*`"
       " in clojure_eval (personal — only in this, your own room):\n"
       "- `(web/recent)` / `(web/recent n)` — recent captured pages:"
       " [{:url :title :at :host :text}]\n"
       "- `(web/search \"query\")` — fulltext over your captured pages\n"
       "- `(web/page id)` — the full text of one page\n"
       "- `(web/hosts)` — sites you've been reading + counts\n"
       "Program over them freely — e.g. pull LinkedIn profiles, find events,"
       " track what people are discussing. Empty when nothing has been captured."))
