(ns is.simm.uis.web.desktop.views.tasks
  "Tasks — everything that is ready to do, from every source.

   The list is deliberately heterogeneous: an `S/Task` page in a wiki, a
   landable ForkSet, and a dvergr dispatch are all things you have to do, and
   splitting them into three screens would make the answer to \"what is on my
   plate\" require three lookups. Each row says where it came from; the
   aggregate is built server-side by `is.simm.ops.tasks`."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.views.core :as vc]
            [is.simm.uis.web.desktop.signals :as sig]
            #?(:cljs [org.replikativ.spindel.engine.core :as rtc])
            #?(:cljs [is.simm.uis.web.desktop.runtime :refer [runtime]])
            #?(:cljs [is.simm.uis.web.desktop.remote :as rem])
            #?(:cljs [is.simm.uis.web.desktop.tasks-remote :as tr])
            #?(:cljs [is.simm.uis.web.desktop.refs :as refs])
            #?(:cljs [is.simm.uis.web.desktop.aggregate :as agg])
            #?(:cljs [is.simm.runtimes.web :as web]))
  #?(:cljs (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]
                            [org.replikativ.spindel.incremental.interval]
                            [org.replikativ.spindel.dom.foreach :refer [ifor-each]]))
  #?(:cljs (:require [org.replikativ.spindel.incremental.interval :as iv]
                     [org.replikativ.spindel.effects.track :refer [track]])))

;; =============================================================================
;; Loading + actions
;; =============================================================================

#?(:cljs (defonce ^:private gate (agg/gate)))

#?(:cljs
   (defn load-tasks!
     "Fetch the task list, coalesced by the aggregate gate — see
      `uis.desktop.aggregate`."
     ([] (load-tasks! false))
     ([include-done?]
      (agg/run!
       gate
       (fn [done]
         (rem/spin!
          #(tr/list-tasks! web/server-id include-done?)
          (fn [ok err]
            (binding [rtc/*execution-context* runtime]
              ;; A refusal has to reach the screen. An empty list and a denied
              ;; request must not render the same, or a broken principal looks
              ;; like a clear plate.
              (reset! sig/tasks-data
                      (if err
                        {:tasks [] :error (or (rem/error-text err)
                                              (ex-message err) (str err))
                         :include-done? include-done?}
                        {:tasks (vec ok) :include-done? include-done?
                         :loaded-at (js/Date.now)})))
            (done)))))))) 

#?(:cljs
   (defn- set-status! [{:keys [scope page]} status]
     (rem/spin!
      #(tr/set-task-status! web/server-id scope page status)
      (fn [_ err]
        (if err
          (js/console.error "[tasks] status change failed:" err)
          ;; reload rather than patch locally: a status change can move the row
          ;; out of the list entirely (done), and the server owns that decision
          (load-tasks! (:include-done? @sig/tasks-data)))))))

;; =============================================================================
;; Rows
;; =============================================================================

(def ^:private source-label
  {:page "wiki" :forkset "proposal" :dispatch "request"})

(def ^:private source-icon
  {:page "file-text" :forkset "git-pull-request" :dispatch "inbox"})

(defn- due-text
  "Relative when it is close, absolute when it is not — \"in 3 days\" is what a
   person acts on, \"2026-09-14\" is what they plan around."
  [due]
  #?(:cljs
     (when due
       (let [ms (- (.getTime (js/Date. due)) (js/Date.now))
             days (js/Math.round (/ ms 86400000))]
         (cond
           (< ms 0) (str "overdue by " (js/Math.abs days) "d")
           (= 0 days) "today"
           (<= days 7) (str "in " days "d")
           :else (.toLocaleDateString (js/Date. due)))))
     :clj nil))

(defn- task-row [{:keys [id source title status priority due kb-name tier
                         auto-mergeable?] :as t}]
  #?(:cljs
     (let [overdue? (and due (< (- (.getTime (js/Date. due)) (js/Date.now)) 0))]
       (el/div {:key id :class (str "task-row task-row--" (name source)
                                   (when (:ref t) " task-row--linked"))
                ;; The title opens what the task IS — a wiki page, the room a
                ;; request came from. The checkbox below stops propagation so
                ;; completing a task does not also navigate away from it.
                :on-click (fn [e]
                            (when-let [r (:ref t)]
                              (refs/open! r {:new-column? (or (.-metaKey e) (.-ctrlKey e))})))}
         ;; Only a page task can have its status set here. A ForkSet is
         ;; progressed by accepting it, and a dispatch by dvergr's lifecycle —
         ;; offering a checkbox that silently does nothing would be worse than
         ;; not offering one.
         (if (= :page source)
           (el/button {:class (str "task-check" (when (= "done" status) " task-check--done"))
                       :title (if (= "done" status) "Reopen" "Mark done")
                       :on-click (fn [e]
                                   (.stopPropagation e)
                                   (set-status! t (if (= "done" status) "open" "done")))}
                      (vc/icon (if (= "done" status) "check-circle-2" "circle")))
           (el/span {:class "task-check task-check--na"
                     :title (str "Progressed where it lives (" (source-label source) ")")}
                    (vc/icon (source-icon source))))
         (el/div {:class "task-main"}
           (el/div {:class "task-title"} title)
           (el/div {:class "task-meta"}
             (el/span {:class (str "task-source task-source--" (name source))}
                      (source-label source))
             (when kb-name (el/span {:class "task-kb"} kb-name))
             (when (and priority (not= "medium" priority))
               (el/span {:class (str "task-priority task-priority--" priority)} priority))
             (when (and (= :forkset source) auto-mergeable?)
               (el/span {:class "task-flag"} "merges cleanly"))
             (when (and (= :forkset source) (= :reviewable tier))
               (el/span {:class "task-flag"} "needs review"))
             (when-let [d (due-text due)]
               (el/span {:class (str "task-due" (when overdue? " task-due--overdue"))} d))
             (when (not= "open" status)
               (el/span {:class (str "task-status task-status--" status)} status))))))
     :clj nil))

;; =============================================================================
;; View
;; =============================================================================

(defn tasks-view []
  #?(:cljs
     (spin
       (let [data (iv/get-new (track sig/tasks-data))
             _ (when (nil? data) (load-tasks!))
             {:keys [tasks error include-done?]} data]
         (el/div {:class "tasks-view"}
           (el/div {:class "tasks-header"}
             (el/h2 {} "Tasks")
             (el/div {:class "tasks-header-actions"}
               ;; "Show done" stays — it is a FILTER, not a refresh.
               ;; `perspectives-sync` keeps the list current, so there is
               ;; nothing here for the user to trigger.
               (el/button {:class (str "btn btn-ghost btn-sm"
                                       (when include-done? " btn-active"))
                           :on-click (fn [_] (load-tasks! (not include-done?)))}
                          (if include-done? "Hide done" "Show done"))))
           (cond
             error (el/div {:class "proposal-error"}
                     (str "Couldn't load tasks — " error))
             (nil? data) (el/div {:class "tasks-empty"} "Loading…")
             (empty? tasks) (el/div {:class "tasks-empty"}
                              "Nothing to do. Tasks come from wiki pages typed
                               Task, proposals that are ready to land, and
                               requests other people or agents send you.")
             ;; ifor-each — see views.feed for why a plain `for` collides
             :else (el/div {:class "tasks-list"}
                     (ifor-each :id tasks (fn [t] (task-row t))))))))
     :clj nil))
