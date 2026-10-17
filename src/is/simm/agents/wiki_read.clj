(ns is.simm.agents.wiki-read
  "Wiki query/formatting vocabulary over caller-resolved immutable snapshots.
   Authority and world selection belong to the resolver, not these queries."
  (:require [clojure.string :as str]
            [datahike.api :as d]
            [is.simm.model.knowledge-bases :as kbs]
            [is.simm.model.references :as refs]))

(defn- targets! [resolve-targets!]
  (let [targets (resolve-targets!)]
    (when-not (and (vector? targets)
                   (every? #(and (map? %) (string? (:name %)) (some? (:db %))) targets))
      (throw (ex-info "Wiki resolver must supply explicit database snapshots" {:type ::invalid-targets})))
    targets))

(defn- incoming [targets title]
  (->> targets
       (mapcat (fn [{:keys [name db]}]
                 (map (fn [source] {:kb name :title source})
                      (d/q '[:find [?src-title ...] :in $ ?t :where
                             [?p :S.Page/title ?t] [?b :block/references ?p]
                             [?b :block/parent ?src] [?src :S.Page/title ?src-title]]
                           db title))))
       distinct vec))

(defn bindings
  "Pure read vocabulary. Each public call invokes resolve-targets! exactly
   once; it must return a vector of {:name string :db immutable-db}. [] is a
   legitimate authorized empty selection, nil is not. Resolver failures are
   never replaced by an empty result or a connection to trunk.

   Saved functions retain the resolver, not target DB/connection values. The
   caller must resolve current authority and ambient world on every use. No
   DB handles, mutation, summarization or model calls enter this result map."
  [resolve-targets!]
  {'pages
   (fn []
     (mapv (fn [{:keys [name db]}]
             {:kb name :pages (->> (d/q '[:find ?title :where [_ :S.Page/title ?title]] db)
                                   (map first) sort vec)})
           (targets! resolve-targets!)))

   'read-page
   (fn [title]
     (some (fn [{:keys [name db]}]
             (when-let [eid (when (some? title)
                              (d/q '[:find ?e . :in $ ?t :where [?e :S.Page/title ?t]] db title))]
               (let [blocks (d/q '[:find ?c ?o :keys content order :in $ ?p
                                   :where [?b :block/parent ?p] [?b :block/content ?c] [?b :block/order ?o]]
                                 db eid)]
                 {:kb name :title title
                  :blocks (->> (sort-by :order blocks) (mapv (comp refs/strip-html :content)))})))
           (targets! resolve-targets!)))

   'search
   (fn [query]
     (let [targets (targets! resolve-targets!)
           secondary (try (requiring-resolve 'dvergr.search.secondary/search)
                          (catch Throwable _ nil))
           hits (when secondary
                  (->> targets
                       (mapcat (fn [{:keys [name db]}]
                                 (for [[eid score] (secondary db kbs/wiki-fulltext-ident query {:limit 12})
                                       :let [e (d/pull db [:S.Page/title :block/content
                                                           {:block/parent [:S.Page/title]}] eid)
                                             title (or (:S.Page/title e) (get-in e [:block/parent :S.Page/title]))]
                                       :when title]
                                   {:kb name :title title :score score
                                    :snippet (when-let [content (:block/content e)]
                                               (let [text (str/replace content #"<[^>]+>" "")]
                                                 (subs text 0 (min 140 (count text)))))})))
                       (sort-by :score >) (take 12) vec))]
       (if (seq hits)
         hits
         (let [pattern (re-pattern (str "(?i)" query))]
           (->> targets
                (mapcat (fn [{:keys [name db]}]
                          (->> (d/q '[:find ?title :where [_ :S.Page/title ?title]] db)
                               (map first) (filter #(re-find pattern %))
                               (map (fn [title] {:kb name :title title})))))
                (sort-by :title) vec)))))

   'backlinks
   (fn [title]
     (incoming (targets! resolve-targets!) title))

   'neighborhood
   (fn [title]
     (let [targets (targets! resolve-targets!)
           outgoing (->> targets
                         (mapcat (fn [{:keys [name db]}]
                                   (map (fn [target] {:kb name :title target})
                                        (d/q '[:find [?tt ...] :in $ ?t :where
                                               [?p :S.Page/title ?t] [?b :block/parent ?p]
                                               [?b :block/references ?tp] [?tp :S.Page/title ?tt]]
                                             db title))))
                         distinct vec)]
       {:page title :links-to outgoing :linked-from (incoming targets title)}))})
