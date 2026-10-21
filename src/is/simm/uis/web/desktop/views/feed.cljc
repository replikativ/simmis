(ns is.simm.uis.web.desktop.views.feed
  "Feed — what is happening, from the system and from every room.

   The rows are heterogeneous on purpose: a deployment announcement and a page
   an agent wrote overnight are both \"what happened\", and splitting them would
   make the answer to \"anything new?\" take two looks. Each row says where it
   came from (`is.simm.ops.feed` assembles them).

   Distinct from Timelines' Changed panel, which shares data but answers a
   different question: that one is provenance at a chosen reference, this one
   is attention at now."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [clojure.core.async :refer [go <! put! promise-chan] :include-macros true])
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.feed-remote :as fr])
            #?(:cljs [is.simm.uis.web.desktop.refs :as refs])
            #?(:cljs [is.simm.uis.web.desktop.aggregate :as agg])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.incremental.interval]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]]))
  #?(:cljs (:require [org.replikativ.spindel.incremental.interval :as iv]
                     [org.replikativ.spindel.effects.track :refer [track]])))

#?(:cljs (defonce ^:private gate (agg/gate)))
#?(:cljs (defn refresh-loaded! [] (agg/refresh! gate)))

#?(:cljs
   (defn load-feed!
     "Fetch the feed into `sig/feed-data`, coalesced by the aggregate gate.

      No `loading?` flag of its own: the gate is the ONE guard (see
      `uis.desktop.aggregate` for the bug two guards caused), and the
      invalidation layer calls this without knowing anything about timing."
     []
     (agg/run!
      gate
      (fn [done current?]
        (go
          (let [ch (promise-chan)]
            (binding [rtc/*execution-context* runtime]
              (let [s (fr/load-feed! web/server-id)]
                (s (fn [r] (put! ch {:ok r})) (fn [e] (put! ch {:err e})))))
            (let [{:keys [ok err]} (<! ch)]
              (when (current?)
               (binding [rtc/*execution-context* runtime]
                (reset! sig/feed-data
                        (if err
                          {:items [] :error (or (rem/error-text err)
                                                (ex-message err) (str err))}
                          {:items (vec ok) :loaded-at (js/Date.now)}))))
              ;; exactly once, on both paths — a gate that never reopens is an
              ;; aggregate that never refreshes again
              (done))))))))

(def ^:private kind-icon
  {:limits "gauge" :beta "sparkles" :news "newspaper"
   :mention "at-sign" :proposal-filed "git-pull-request"
   :proposal-resolved "git-merge" :page-new "file-plus"})

#?(:cljs
   (defn- when-str [at]
     (when at
       (let [ms (- (js/Date.now) (.getTime (js/Date. at)))
             mins (js/Math.round (/ ms 60000))
             hrs (js/Math.round (/ ms 3600000))
             days (js/Math.round (/ ms 86400000))]
         (cond (< mins 1) "now"
               (< mins 60) (str mins "m")
               (< hrs 24) (str hrs "h")
               (< days 7) (str days "d")
               :else (.toLocaleDateString (js/Date. at) "en-US"
                                          #js {:month "short" :day "numeric"}))))))

#?(:cljs
   (defn- row
     "Actor and place on top, headline, then the CONTENT.

      The time sits at the right of the head line rather than under the
      headline, because the space below a headline is the most valuable on the
      row and metadata does not earn it — an excerpt does. You decide whether
      to open something from what it says, not from when it happened."
     [{:keys [id source kind title body actor where at] :as item}]
     (el/div {:key id :class (str "feed-row feed-row--" (name source)
                                 (when (:ref item) " feed-row--linked"))
              :on-click (fn [e]
                          (when-let [r (:ref item)]
                            (refs/open! r {:new-column? (or (.-metaKey e) (.-ctrlKey e))})))}
       (el/span {:class "feed-icon"} (vc/icon (get kind-icon kind "circle")))
       (el/div {:class "feed-main"}
         (el/div {:class "feed-head"}
           ;; Falling back to the SOURCE, not to "simmis": a proposal filed
           ;; before authorship was recorded has no actor, and attributing it
           ;; to the system would be a small lie in the most visible position
           ;; on the row.
           (el/span {:class (str "feed-actor feed-actor--" (name source))}
                    (or actor (case source
                                :proposal "a proposal"
                                :mention "someone"
                                :page "a wiki"
                                "simmis")))
           (when (and where (not= where actor))
             (el/span {:class "feed-where"} where))
           (el/span {:class "feed-when"} (when-str at)))
         (el/div {:class "feed-title"} title)
         (when body (el/div {:class "feed-body"} body)))))) 

(defn feed-view []
  #?(:cljs
     (spin
       (let [data (iv/get-new (track sig/feed-data))
             _ (when (nil? data) (load-feed!))
             {:keys [items error]} data]
         (el/div {:class "feed-view"}
           ;; No Refresh button: `perspectives-sync` invalidates this signal
           ;; when the server says a KB changed. A button asking the user to
           ;; find out whether the screen is lying is not a feature.
           (el/div {:class "feed-header"}
             (el/h2 {} "Feed"))
           (cond
             error (el/div {:class "proposal-error"} (str "Couldn't load the feed — " error))
             (nil? data) (el/div {:class "feed-empty"} "Loading…")
             (empty? items) (el/div {:class "feed-empty"}
                              "Nothing yet. Announcements land here, and so does
                               what your agents write while you are away.")
             ;; ifor-each, not `for`: the list length varies between renders,
             ;; and a plain `for` gave every sibling the same source-loc addr —
             ;; spindel logged ::addr-collision for each row and two vnodes
             ;; claimed one address per pass.
             :else (el/div {:class "feed-list"}
                     (ifor-each :id items (fn [i] (row i))))))))
     :clj nil))
