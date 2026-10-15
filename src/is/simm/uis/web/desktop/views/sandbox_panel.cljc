(ns is.simm.uis.web.desktop.views.sandbox-panel
  "Sandbox state inspector panel for chat rooms.

   Shows three sections:
   - Namespace: user-defined vars with types and source code
   - Repositories: git repos tracked in the room context
   - Databases: room DB and attached KBs with entity counts

   Rendered as a collapsible <details> panel at the bottom of the chat.
   The room's owner also gets a quick REPL input for direct eval without
   sending a chat message (`:can-eval?` from the server)."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.dom.foreign]
            [is.simm.uis.web.desktop.views.core :as vc]
            [clojure.string :as str]
            #?(:cljs [is.simm.uis.web.desktop.sandbox-remote :as sandbox-remote])
            #?(:cljs [is.simm.uis.web.desktop.markdown :as md])
)
  #?(:cljs (:require-macros [org.replikativ.spindel.dom.elements :as el]
                            [org.replikativ.spindel.dom.foreign :refer [foreign-node]])))

;; =============================================================================
;; Highlighted code block via foreign-node
;; =============================================================================

(defn highlighted-code
  "Render a syntax-highlighted Clojure code block."
  [code extra-class]
  #?(:cljs
     (foreign-node
       {:class (str "sandbox-code" (when extra-class (str " " extra-class)))
        :on-mount (fn [container]
                    (let [code-el (js/document.createElement "code")]
                      (set! (.-className code-el) "hljs language-clojure")
                      (set! (.-innerHTML code-el)
                            (or (md/highlight-code (or code "")) (or code "")))
                      (.appendChild container code-el)))
        :on-unmount (fn [container]
                      (when container
                        (set! (.-innerHTML container) "")))})
     :clj
     (el/pre {:class (str "sandbox-code" (when extra-class (str " " extra-class)))}
       (or code ""))))

;; =============================================================================
;; Sub-sections
;; =============================================================================

(defn- type-badge [type-kw]
  (let [[label css] (case type-kw
                      :function   ["fn"  "badge-fn"]
                      :value      ["val" "badge-val"]
                      :unreadable ["?"   "badge-unknown"]
                      ["?" "badge-unknown"])]
    (el/span {:class (str "sandbox-badge " css)} label)))

(defn namespace-section
  "Namespace vars section — type badge, name, and collapsible source code."
  [vars history]
  (let [code-by-name (into {} (map (juxt :name :code) history))]
    (el/div {:class "sandbox-section"}
      (el/div {:class "sandbox-section-title"}
        (vc/icon "box" {:class "sandbox-section-icon"})
        (el/span {} (str "Namespace (" (count vars) " vars)")))
      (if (seq vars)
        (el/div {:class "sandbox-vars-list"}
          (map (fn [{:keys [name type preview]}]
                 (el/details {:key name :class "sandbox-var-item"}
                   (el/summary {:class "sandbox-var-summary"}
                     (type-badge type)
                     (el/span {:class "sandbox-var-name"} name)
                     (el/span {:class "sandbox-var-preview"} preview))
                   (if-let [code (get code-by-name name)]
                     (highlighted-code code nil)
                     (el/pre {:class "sandbox-code"} preview))))
               vars))
        (el/div {:class "sandbox-empty"} "No vars defined yet.")))))

(defn repos-section
  "Git repositories section."
  [repos]
  (el/div {:class "sandbox-section"}
    (el/div {:class "sandbox-section-title"}
      (vc/icon "git-branch" {:class "sandbox-section-icon"})
      (el/span {} (str "Repositories (" (count repos) ")")))
    (if (seq repos)
      (el/div {}
        (map (fn [{:keys [name path branch modified untracked recent-commits]}]
               (el/div {:key path :class "sandbox-repo-item"}
                 (el/div {:class "sandbox-repo-header"}
                   (el/span {:class "sandbox-repo-name"} (or name path))
                   (el/span {:class "sandbox-repo-branch"}
                     (vc/icon "git-branch" {:class "sandbox-repo-branch-icon"})
                     branch))
                 (el/div {:class "sandbox-repo-status"}
                   (when (pos? modified)
                     (el/span {:class "sandbox-repo-modified"} (str modified " modified")))
                   (when (pos? untracked)
                     (el/span {:class "sandbox-repo-untracked"} (str untracked " untracked"))))
                 (when (seq recent-commits)
                   (el/div {:class "sandbox-repo-commits"}
                     (map (fn [{:keys [sha msg]}]
                            (el/div {:key sha :class "sandbox-commit"}
                              (el/span {:class "sandbox-commit-sha"} (subs sha 0 7))
                              (el/span {:class "sandbox-commit-msg"} msg)))
                          (take 3 recent-commits))))))
             repos))
      (el/div {:class "sandbox-empty"} "No repositories tracked."))))

(defn databases-section
  "Databases section — room DB and KB connections."
  [databases]
  (el/div {:class "sandbox-section"}
    (el/div {:class "sandbox-section-title"}
      (vc/icon "database" {:class "sandbox-section-icon"})
      (el/span {} (str "Databases (" (count databases) ")")))
    (if (seq databases)
      (el/div {:class "sandbox-db-list"}
        (map (fn [{:keys [name type entity-count]}]
               (el/div {:key name :class "sandbox-db-item"}
                 (el/span {:class (str "sandbox-badge "
                                       (if (= type :kb) "badge-kb" "badge-room"))}
                   (if (= type :kb) "KB" "room"))
                 (el/span {:class "sandbox-db-name"} name)
                 (el/span {:class "sandbox-db-count"} (str entity-count " entities"))))
             databases))
      (el/div {:class "sandbox-empty"} "No databases connected."))))

;; =============================================================================
;; The panel — foreign-node to retain state across re-renders
;; =============================================================================

(defn sandbox-panel
  "Full sandbox state panel for a room.
   Fetches state lazily when first opened. Retains state across re-renders.
   Params:
   - room-uuid: UUID of the room whose sandbox to inspect"
  [room-uuid]
  #?(:cljs
     (foreign-node
       {:class "sandbox-panel-host"
        :on-mount
        (fn [host]
          ;; Build the <details> DOM and attach event handlers
          (let [details  (doto (js/document.createElement "details")
                           (-> .-className (set! "sandbox-panel")))
                summary  (doto (js/document.createElement "summary")
                           (-> .-className (set! "sandbox-panel-summary")))
                icon-el  (doto (js/document.createElement "i")
                           (-> .-className (set! "lucide-terminal sandbox-panel-icon"))
                           (.setAttribute "data-lucide" "terminal"))
                title-el (doto (js/document.createElement "span")
                           (-> .-className (set! "sandbox-panel-title"))
                           (-> .-textContent (set! "Sandbox")))
                counts-el (doto (js/document.createElement "span")
                            (-> .-className (set! "sandbox-panel-counts")))
                refresh-btn (doto (js/document.createElement "button")
                              (-> .-className (set! "sandbox-refresh-btn"))
                              (-> .-title (set! "Refresh sandbox state")))
                refresh-icon (doto (js/document.createElement "i")
                               (-> .-className (set! "lucide-refresh-cw sandbox-refresh-icon"))
                               (.setAttribute "data-lucide" "refresh-cw"))
                body-el (doto (js/document.createElement "div")
                          (-> .-className (set! "sandbox-body"))
                          (-> .-style .-display (set! "none")))
                ;; State
                state-atom   (atom nil)
                loading-atom (atom false)]

            (.appendChild refresh-btn refresh-icon)
            (.appendChild summary icon-el)
            (.appendChild summary title-el)
            (.appendChild summary counts-el)
            (.appendChild summary refresh-btn)
            (.appendChild details summary)
            (.appendChild details body-el)
            (.appendChild host details)
            ;; Render Lucide icons in the newly created elements
            (when (exists? js/lucide)
              (js/lucide.createIcons))

            (letfn [(render-state! [state]
                      (reset! state-atom state)
                      (reset! loading-atom false)
                      ;; Update counts badge
                      (set! (.-textContent counts-el)
                            (str (count (:namespace-vars state)) " vars"
                                 (when (seq (:repos state))
                                   (str ", " (count (:repos state)) " repos"))))
                      ;; Clear + rebuild body and ensure visible
                      (set! (.-innerHTML body-el) "")
                      (set! (.-style.display body-el) "block")
                      ;; Namespace vars
                      (let [vars    (:namespace-vars state)
                            history (:eval-history state)
                            code-by-name (into {} (map (fn [h] [(:name h) (:code h)]) history))
                            ns-sec  (js/document.createElement "div")]
                        (set! (.-className ns-sec) "sandbox-section")
                        (let [ns-title (doto (js/document.createElement "div")
                                         (-> .-className (set! "sandbox-section-title"))
                                         (-> .-textContent (set! (str "Namespace (" (count vars) " vars)"))))]
                          (.appendChild ns-sec ns-title))
                        (if (seq vars)
                          (doseq [{:keys [name type preview]} vars]
                            (let [item    (doto (js/document.createElement "details")
                                            (-> .-className (set! "sandbox-var-item")))
                                  summ    (doto (js/document.createElement "summary")
                                            (-> .-className (set! "sandbox-var-summary")))
                                  badge   (doto (js/document.createElement "span")
                                            (-> .-className
                                                (set! (str "sandbox-badge "
                                                           (case type
                                                             :function "badge-fn"
                                                             :value "badge-val"
                                                             "badge-unknown"))))
                                            (-> .-textContent
                                                (set! (case type :function "fn" :value "val" "?"))))
                                  nm-el   (doto (js/document.createElement "span")
                                            (-> .-className (set! "sandbox-var-name"))
                                            (-> .-textContent (set! name)))
                                  prev-el (doto (js/document.createElement "span")
                                            (-> .-className (set! "sandbox-var-preview"))
                                            (-> .-textContent (set! preview)))]
                              (.appendChild summ badge)
                              (.appendChild summ nm-el)
                              (.appendChild summ prev-el)
                              (.appendChild item summ)
                              ;; Code block
                              (let [code-pre (js/document.createElement "div")
                                    code-el  (js/document.createElement "code")
                                    src      (or (get code-by-name name) preview)]
                                (set! (.-className code-pre) "sandbox-code")
                                (set! (.-className code-el) "hljs language-clojure")
                                (set! (.-innerHTML code-el)
                                      (or (md/highlight-code src) src))
                                (.appendChild code-pre code-el)
                                (.appendChild item code-pre))
                              (.appendChild ns-sec item)))
                          (let [empty-el (doto (js/document.createElement "div")
                                           (-> .-className (set! "sandbox-empty"))
                                           (-> .-textContent (set! "No vars defined yet.")))]
                            (.appendChild ns-sec empty-el)))
                        (.appendChild body-el ns-sec))
                      ;; Repos
                      (let [repos   (:repos state)
                            repo-sec (js/document.createElement "div")]
                        (set! (.-className repo-sec) "sandbox-section")
                        (let [repo-title (doto (js/document.createElement "div")
                                           (-> .-className (set! "sandbox-section-title"))
                                           (-> .-textContent (set! (str "Repositories (" (count repos) ")"))))]
                          (.appendChild repo-sec repo-title))
                        (doseq [{:keys [name path branch modified untracked recent-commits]} repos]
                          (let [item (js/document.createElement "div")]
                            (set! (.-className item) "sandbox-repo-item")
                            (let [hdr (doto (js/document.createElement "div")
                                        (-> .-className (set! "sandbox-repo-header")))
                                  nm  (doto (js/document.createElement "span")
                                        (-> .-className (set! "sandbox-repo-name"))
                                        (-> .-textContent (set! (or name path))))
                                  br  (doto (js/document.createElement "span")
                                        (-> .-className (set! "sandbox-repo-branch"))
                                        (-> .-textContent (set! (str "⎇ " branch))))]
                              (.appendChild hdr nm)
                              (.appendChild hdr br)
                              (.appendChild item hdr))
                            (when (or (pos? modified) (pos? untracked))
                              (let [st (js/document.createElement "div")]
                                (set! (.-className st) "sandbox-repo-status")
                                (when (pos? modified)
                                  (.appendChild st
                                    (doto (js/document.createElement "span")
                                      (-> .-className (set! "sandbox-repo-modified"))
                                      (-> .-textContent (set! (str modified " modified"))))))
                                (when (pos? untracked)
                                  (.appendChild st
                                    (doto (js/document.createElement "span")
                                      (-> .-className (set! "sandbox-repo-untracked"))
                                      (-> .-textContent (set! (str untracked " untracked"))))))
                                (.appendChild item st)))
                            (doseq [{:keys [sha msg]} (take 3 recent-commits)]
                              (let [commit (js/document.createElement "div")
                                    sha-el (doto (js/document.createElement "span")
                                             (-> .-className (set! "sandbox-commit-sha"))
                                             (-> .-textContent (set! (subs sha 0 7))))
                                    msg-el (doto (js/document.createElement "span")
                                             (-> .-className (set! "sandbox-commit-msg"))
                                             (-> .-textContent (set! msg)))]
                                (set! (.-className commit) "sandbox-commit")
                                (.appendChild commit sha-el)
                                (.appendChild commit msg-el)
                                (.appendChild item commit)))
                            (.appendChild repo-sec item)))
                        (.appendChild body-el repo-sec))
                      ;; Databases
                      (let [dbs     (:databases state)
                            db-sec  (js/document.createElement "div")]
                        (set! (.-className db-sec) "sandbox-section")
                        (let [db-title (doto (js/document.createElement "div")
                                         (-> .-className (set! "sandbox-section-title"))
                                         (-> .-textContent (set! (str "Databases (" (count dbs) ")"))))]
                          (.appendChild db-sec db-title))
                        (doseq [{:keys [name type entity-count]} dbs]
                          (let [row    (js/document.createElement "div")
                                badge  (doto (js/document.createElement "span")
                                         (-> .-className
                                             (set! (str "sandbox-badge "
                                                        (if (= type :kb) "badge-kb" "badge-room"))))
                                         (-> .-textContent (set! (if (= type :kb) "KB" "room"))))
                                nm-el  (doto (js/document.createElement "span")
                                         (-> .-className (set! "sandbox-db-name"))
                                         (-> .-textContent (set! name)))
                                cnt-el (doto (js/document.createElement "span")
                                         (-> .-className (set! "sandbox-db-count"))
                                         (-> .-textContent (set! (str entity-count " entities"))))]
                            (set! (.-className row) "sandbox-db-item")
                            (.appendChild row badge)
                            (.appendChild row nm-el)
                            (.appendChild row cnt-el)
                            (.appendChild db-sec row)))
                        (.appendChild body-el db-sec))
                      ;; Quick REPL — only for who may evaluate here (the room's
                      ;; owner); the server says so in `:can-eval?`
                      (when (:can-eval? state)
                        (let [repl-container (js/document.createElement "div")]
                          (set! (.-className repl-container) "sandbox-repl")
                          (let [hdr      (doto (js/document.createElement "div")
                                           (-> .-className (set! "sandbox-repl-header")))
                                lbl      (doto (js/document.createElement "span")
                                           (-> .-className (set! "sandbox-repl-label"))
                                           (-> .-textContent (set! "Quick eval (Ctrl+Enter)")))
                                textarea (doto (js/document.createElement "textarea")
                                           (-> .-className (set! "sandbox-repl-input"))
                                           (-> .-placeholder (set! "(+ 1 2)"))
                                           (-> .-rows (set! 2)))
                                result   (doto (js/document.createElement "div")
                                           (-> .-className (set! "sandbox-repl-result"))
                                           (-> .-style .-display (set! "none")))]
                            (.appendChild hdr lbl)
                            (.appendChild repl-container hdr)
                            (.appendChild repl-container textarea)
                            (.appendChild repl-container result)
                            (set! (.-onkeydown textarea)
                                  (fn [e]
                                    (when (and (= (.-key e) "Enter") (.-ctrlKey e))
                                      (.preventDefault e)
                                      (let [code (.-value textarea)]
                                        (when (seq code)
                                          (set! (.-textContent lbl) "Evaluating…")
                                          (let [spin (sandbox-remote/eval-in-room-remote!
                                                       room-uuid code)]
                                            (spin
                                              (fn [r]
                                                (let [code-el (js/document.createElement "code")
                                                      css (if (:success r)
                                                            "hljs language-clojure sandbox-result--ok"
                                                            "hljs language-clojure sandbox-result--err")]
                                                  (set! (.-className code-el) css)
                                                  (set! (.-innerHTML code-el)
                                                        (or (md/highlight-code (:result r)) (:result r)))
                                                  (set! (.-innerHTML result) "")
                                                  (.appendChild result code-el)
                                                  (set! (.-style.display result) "block")
                                                  (set! (.-textContent lbl) "Quick eval (Ctrl+Enter)")))
                                              (fn [err]
                                                (set! (.-textContent result) (str err))
                                                (set! (.-style.display result) "block")
                                                (set! (.-textContent lbl) "Quick eval (Ctrl+Enter)"))))))))))
                          (.appendChild body-el repl-container))))

                    (fetch-state! []
                      (when-not @loading-atom
                        (reset! loading-atom true)
                        (set! (.-style.display body-el) "none")
                        (let [spin (sandbox-remote/get-room-state-remote! room-uuid)]
                          (spin render-state!
                                (fn [err]
                                  (js/console.error "[sandbox-panel] fetch error" err)
                                  (reset! loading-atom false))))))]

              ;; Toggle: fetch on first open
              (set! (.-ontoggle details)
                    (fn []
                      (if (.-open details)
                        (do
                          (set! (.-style.display body-el) "block")
                          (when (nil? @state-atom)
                            (fetch-state!)))
                        (set! (.-style.display body-el) "none"))))

              ;; Refresh button
              (set! (.-onclick refresh-btn)
                    (fn [e]
                      (.stopPropagation e)
                      (fetch-state!))))))

        :on-unmount (fn [host]
                      (when host
                        (set! (.-innerHTML host) "")))})
     :clj
     (el/div {:class "sandbox-panel-host"} "Sandbox panel (browser only)")))
