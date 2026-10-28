(ns is.simm.agents.memory-links
  "Links over the same selected filesystem as reads and edits. No page creation,
   title guessing, or authority-bearing links. Unpublished worlds are explicit."
  (:require [clojure.string :as str]
            [geschichte.query :as query]
            [is.simm.documents.files :as files]
            [is.simm.documents.block-reference :as blocks]
            [is.simm.documents.links :as links]))

(defn bindings [with-workspace! owner]
  {'link
   (fn link
     ([path] (link path {}))
     ([path {:keys [label block] :as options}]
      (files/path! path)
      (when-not (and (map? options) (every? #{:label :block} (keys options))
                     (or (nil? block) (uuid? block)))
        (throw (ex-info "Link options are an optional label and explicit block UUID" {:type ::invalid})))
      (let [label (or label (last (str/split path #"/")))]
        (when-not (and (string? label) (<= 1 (count label) 200) (not (re-find #"\p{Cntrl}" label)))
          (throw (ex-info "Link label must be bounded plain text" {:type ::invalid})))
        (with-workspace!
          (fn [{:keys [conn target proposal selected-context]}]
            (let [db @conn entry (files/entry db path)]
              (cond
                (nil? entry) {:status :missing :path path}
                (or proposal (not= (:fork-id owner) (:fork-id selected-context)))
                {:status :unpublished :path path :reason (if proposal :proposal-workspace :isolated-world)}
                :else
                (let [repository (:geschichte.repo/id (query/repository db))
                      resolved (when block (blocks/resolve-in db {:repository-id repository :block-id block :path-hint path}))
                      _ (when (and block (not (and (= :resolved (:status resolved)) (= path (:path resolved)))))
                          (throw (ex-info "The block is missing or ambiguous in this repository"
                                          {:type ::missing-block :path path :status (:status resolved)})))
                      reference (cond-> {:kind :memory-file :room (str (:room-id target)) :path path}
                                  block (assoc :kind :memory-block :repository (str repository) :block (str block)))
                      href (or (links/route reference)
                               (throw (ex-info "This source cannot be linked from the room" {:type ::unaddressable})))
                      escaped (str/replace label #"[\\\[\]]" #(str "\\" %))]
                  {:status :ready :path path :href href :reference reference
                   :publication :shared-worktree :markdown (str "[" escaped "](" href ")")}))))))))})
