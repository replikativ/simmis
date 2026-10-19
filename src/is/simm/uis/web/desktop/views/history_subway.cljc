(ns is.simm.uis.web.desktop.views.history-subway
  "History subway (S4): the KB's yggdrasil commit graph as a clickable
   backbone in the wiki context panel. Today the history is linear (one
   :db branch, one commit per transaction), so this renders as a vertical
   rail of recent commits grouped by day; clicking a commit jumps the
   GlobalCut to that commit's timestamp (the slider + subway share one
   ref). Fork lanes light up once proposals create branches — the
   :branch badge + rail colour are already keyed on it.

   doc/proposals-and-time-travel.md §S4."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [clojure.core.async :refer [go] :include-macros true])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [org.replikativ.spindel.incremental.interval :as iv])
            #?(:cljs [org.replikativ.spindel.effects.track :refer [track]])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.branching-remote :as br])
            #?(:cljs [is.simm.uis.web.desktop.aggregate :as aggregate])
            #?(:cljs [is.simm.uis.web.desktop.connection-epoch :as epoch])
            #?(:cljs [is.simm.runtimes.web :as web])))

;; How many recent commits to show before "Show older".
(def ^:private DEFAULT-LIMIT 25)

#?(:cljs (defonce ^:private loading (atom {})))
#?(:cljs (defonce ^:private gates (atom {})))
#?(:cljs (defonce ^:private show-limit (atom {})))  ; scope-str → limit

#?(:cljs
   (defn load-commit-graph!
     "Fire-and-forget load of the KB's commit graph into
      sig/commit-graph-data. Idempotent per scope unless :force?."
     [db-scope & {:keys [force?]}]
     (let [sk (str db-scope) connection (epoch/snapshot)]
       (when (and connection db-scope (or force? (not= connection (get @loading sk))))
         (swap! loading assoc sk connection)
         (let [gate (get (swap! gates #(if (contains? % sk) % (assoc % sk (aggregate/gate)))) sk)]
           (aggregate/run!
            gate
            (fn [done current?]
              ;; Dispatch outside a render spin, but keep callback ownership
              ;; in the aggregate gate rather than an extra pending channel.
              (go
                (try
                  (binding [rtc/*execution-context* runtime]
                    (let [s (br/kb-commit-graph! web/server-id sk)]
                      (s (fn [value]
                           (try
                             (when (current?)
                               (binding [rtc/*execution-context* runtime]
                                 (swap! sig/commit-graph-data assoc sk value)))
                             (finally (done))))
                         (fn [error]
                           (try (when (current?) (js/console.error "[subway] load error:" error))
                                (finally (done)))))))
                  (catch :default error
                    (when (current?) (js/console.error "[subway] load error:" error))
                    (done)))))))))))

#?(:cljs
   (defn invalidate!
     "Re-fetch `db-scope`'s graph, but only if it has been fetched before.

      `loading` has to stay a latch — the footer spin calls `load-commit-graph!`
      in its body, so releasing it on completion would turn every re-render into
      an RPC. That made the graph load exactly once per scope per page load, so
      a commit made while the app was open never appeared. Invalidating on the
      write event instead keeps the latch honest AND the graph fresh.

      Scoped to already-loaded graphs so a write to a KB nobody is looking at
      does not pull its history over the wire."
     [db-scope]
     (let [sk (str db-scope)]
       (when (contains? @loading sk)
         (load-commit-graph! db-scope :force? true)))))

#?(:cljs
   (defn reload-loaded!
     "Catch up requested graphs for this session, including hung first loads."
     []
     (doseq [[scope connection] @loading
             :when (= (:session-id connection) (:session-id (epoch/snapshot)))]
       (load-commit-graph! scope :force? true))))

(defn- day-key [^js d]
  #?(:cljs (str (.getFullYear d) "-" (inc (.getMonth d)) "-" (.getDate d)) :clj ""))

(defn- day-label [^js d]
  #?(:cljs (let [now (js/Date.)
                 same? (fn [a b] (and (= (.getFullYear a) (.getFullYear b))
                                      (= (.getMonth a) (.getMonth b))
                                      (= (.getDate a) (.getDate b))))
                 yday (js/Date. (- (.getTime now) 86400000))]
             (cond (same? d now) "Today"
                   (same? d yday) "Yesterday"
                   :else
                   ;; Intl rejects an explicit null option value — to drop the
                   ;; year the key must be ABSENT, not nil.
                   (let [opts #js {:month "short" :day "numeric"}]
                     (when (not= (.getFullYear d) (.getFullYear now))
                       (set! (.-year opts) "numeric"))
                     (.toLocaleDateString d "en-US" opts))))
     :clj ""))

(defn- time-label [^js d]
  #?(:cljs (.toLocaleTimeString d "en-US" #js {:hour "numeric" :minute "2-digit"}) :clj ""))

(defn history-subway
  "Context-footer section (PLAIN fn — render-context-content is not a spin,
   so this can't be one). Reactivity comes from render-column, which tracks
   sig/global-ref + sig/commit-graph-data and re-runs the footer on change;
   here we just read their current values (fork-safe deref under runtime)."
  [db-scope]
  #?(:cljs
     (binding [rtc/*execution-context* runtime]
       (let [gref @sig/global-ref
             _ (load-commit-graph! db-scope)
             data @sig/commit-graph-data
             graph (get data (str db-scope))
             limit (get @show-limit (str db-scope) DEFAULT-LIMIT)
             cut-ms (some-> (:as-of gref) (.getTime))
             all (->> (:nodes graph)
                      (keep (fn [[id node]]
                              (when-let [ts (get-in node [:meta :timestamp])]
                                {:id id :ts ts :ms (.getTime ts)
                                 :branch (get-in node [:meta :branch])})))
                      (sort-by :ms >))
             total (count all)
             commits (vec (take limit all))
             ;; active = latest commit at or before the cut
             active-id (when cut-ms
                         (:id (first (filter #(<= (:ms %) cut-ms) all))))
             ;; group the shown commits by day (already desc)
             groups (partition-by #(day-key (:ts %)) commits)]
         ;; :key scopes the ADDRESSES of every descendant (keyed-child-address
         ;; cascades through the element macro). Without it, two subways for
         ;; different db-scopes rendered in one pass — page switch while time
         ;; travel is active — collide on every element: same source-loc, same
         ;; parent chain, same slots. Measured as ::addr-collision on
         ;; `subway-more` ("263 more" vs "288 more" claiming one addr), which
         ;; then strands the losing subtree elementless (::commit-unbound-addr
         ;; warned on one frozen div forever after).
         (el/div {:key (str db-scope) :class "history-subway"}
           (el/div {:class "context-section-title"}
             (str "History" (when (pos? total) (str " (" total ")"))))
           (el/div {:class "subway-rail"}
             ;; Now node — clears the cut
             (el/div {:class (str "subway-node subway-now"
                                  (when-not cut-ms " subway-active"))
                      :on-click (fn [_] (binding [rtc/*execution-context* runtime]
                                          (reset! sig/global-ref nil)))}
               (el/span {:class "subway-dot"})
               (el/span {:class "subway-node-label"} "Now — live"))
             (if (empty? all)
               (el/div {:class "context-item context-item--empty"}
                 (vc/icon "git-commit") "No history yet")
               (for [grp groups]
                 (el/div {:key (day-key (:ts (first grp))) :class "subway-day"}
                   (el/div {:class "subway-day-label"} (day-label (:ts (first grp))))
                   (for [c grp]
                     (el/div {:key (:id c)
                              :class (str "subway-node"
                                          (when (= (:id c) active-id) " subway-active")
                                          (when (not= :db (:branch c)) " subway-fork"))
                              :title (:id c)
                              :on-click (fn [_] (binding [rtc/*execution-context* runtime]
                                                  (reset! sig/global-ref {:as-of (:ts c)})))}
                       (el/span {:class "subway-dot"})
                       (el/span {:class "subway-node-time"} (time-label (:ts c)))
                       (when (not= :db (:branch c))
                         (el/span {:class "subway-branch-badge"} (name (:branch c))))
                       (el/span {:class "subway-node-id"} (subs (:id c) 0 7)))))))
             (when (> total limit)
               (el/div {:class "subway-more"
                        :on-click (fn [_] (binding [rtc/*execution-context* runtime]
                                            (swap! show-limit assoc (str db-scope)
                                                   (+ limit 25))))}
                 (str "Show older (" (- total limit) " more)")))))))
     :clj nil))
