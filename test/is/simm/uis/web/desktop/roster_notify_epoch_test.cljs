(ns is.simm.uis.web.desktop.roster-notify-epoch-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [clojure.core.async :refer [go <! put! chan] :include-macros true]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.connection-epoch :as epoch]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.chat-remote :as cr]
            [is.simm.uis.web.desktop.remote :as remote]
            [is.simm.uis.web.desktop.user-rooms-sync :as roster]
            [is.simm.uis.web.desktop.message-notify-sync :as notify]
            [is.simm.uis.web.desktop.branching-sync :as branching]
            [is.simm.uis.web.desktop.branching-remote :as branching-remote]
            [is.simm.uis.web.desktop.topic-subscription :as subscription]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(deftest authoritative-branch-refresh-removes-offline-deletions
  (binding [ec/*execution-context* runtime]
    (let [scope (random-uuid) sk (str scope) callbacks (atom [])]
      (with-redefs [sig/user-rooms (signal runtime {:knowledge-bases [{:kb/db-scope scope}]})
                    sig/kb-branches (signal runtime {sk {:db {:status :open} :old {:status :open}}})
                    branching-remote/list-kb-branches! (fn [& _] (fn [ok _] (swap! callbacks conj ok)))]
        (reset! epoch/connection {:session-id (random-uuid) :epoch (random-uuid)})
        (branching/refresh-kb-branches! scope)
        (branching/refresh-kb-branches! scope)
        (is (= 1 (count @callbacks)) "Invalidation during a snapshot queues one subsequent pass")
        ((first @callbacks) [:db :old])
        (is (= 2 (count @callbacks)))
        ((second @callbacks) [:db])
        (is (= #{:db} (set (keys (get @sig/kb-branches sk)))))
        ((first @callbacks) [:db :old])
        (is (= #{:db} (set (keys (get @sig/kb-branches sk)))))))))

(deftest roster-invalidations-coalesce-and-manual-refresh-keeps-the-reconciler
  (async done
         (go
           (let [party (random-uuid) connection {:session-id (random-uuid) :epoch (random-uuid) :party party}
                 callbacks (chan 4) options (atom nil) reconciled (atom []) publications (atom []) key (random-uuid)]
             (try
               (with-redefs [sig/user-rooms (signal runtime nil) sig/layout-columns (signal runtime [])
                             subscription/ensure! (fn [_ opts] (reset! options opts))
                             web/current-session (constantly {:client :peer})
                             cr/load-rooms! (fn [& _] (fn [ok error] (put! callbacks {:ok ok :error error})))]
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection connection)
                   (add-watch sig/user-rooms key (fn [_ _ _ value]
                                                   (swap! publications conj value)
                                                   (is (= value (last @reconciled)) "Authorization precedes synchronous roster consumers")))
                   (roster/subscribe! party #(swap! reconciled conj %)))
                 ((:on-ready @options))
                 (let [first-read (<! callbacks)]
                   ((:on-message @options) {:party-ids #{party}})
                   ((:ok first-read) {:rooms [] :knowledge-bases []})
                   (let [second-read (<! callbacks) fresh {:rooms [] :knowledge-bases [{:kb/db-scope (random-uuid)}]}]
                     ((:ok second-read) fresh)
                     (binding [ec/*execution-context* runtime] (is (= fresh @sig/user-rooms)))
                     (roster/refresh-user-rooms! party)
                     (let [manual (<! callbacks)] ((:ok manual) {:rooms [] :knowledge-bases []}))
                     (is (= 3 (count @reconciled)))
                     (is (= @reconciled @publications))))
                 (binding [ec/*execution-context* runtime] (remove-watch sig/user-rooms key)))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest visible-chat-advances-the-read-cursor-before-refreshing-unread
  (async done
         (go
           (let [party (random-uuid) active (str (random-uuid)) background (str (random-uuid))
                 connection {:session-id (random-uuid) :epoch (random-uuid) :party party}
                 options (atom nil) marks (chan 3) unread (chan 3) unread-starts (atom 0)
                 prior-visibility (js/Object.getOwnPropertyDescriptor js/document "hidden")]
             (try
               ;; Visibility is a controlled fixture, not a requirement that
               ;; the human keep this test tab selected during an async run.
               (js/Object.defineProperty js/document "hidden" #js {:configurable true :value false})
               (with-redefs [sig/layout-columns (signal runtime [{:active-tab :chat
                                                                  :tabs [{:id :chat :type :chat :data {:room-id active}}]}])
                             sig/unread-counts (signal runtime {})
                             subscription/ensure! (fn [_ opts] (reset! options opts))
                             web/current-session (constantly {:client :peer})
                             cr/mark-read! (fn [& _] (fn [ok error] (put! marks {:ok ok :error error})))
                             cr/load-unread-counts! (fn [& _]
                                                      (swap! unread-starts inc)
                                                      (fn [ok error] (put! unread {:ok ok :error error})))]
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection connection)
                   (notify/subscribe! party))
                 ((:on-message @options) {:kind :message :room-id active :popup? false})
                 (let [mark (<! marks)]
                   (is (zero? @unread-starts) "Do not race unread snapshot ahead of the durable read cursor")
                   ((:ok mark) true)
                   (let [snapshot (<! unread)]
                     ((:ok snapshot) {active 3 background 1})
                     (binding [ec/*execution-context* runtime]
                       (is (nil? (get @sig/unread-counts active)))
                       (is (= 1 (get @sig/unread-counts background))))))
                 ((:on-message @options) {:kind :message :room-id background :popup? false})
                 (let [snapshot (<! unread)]
                   (binding [ec/*execution-context* runtime] (is (= 2 (get @sig/unread-counts background))))
                   ((:ok snapshot) {background 2})
                   (binding [ec/*execution-context* runtime] (is (= {background 2} @sig/unread-counts)))))
               (catch :default error (is false (str error)))
               (finally
                 (if prior-visibility
                   (js/Object.defineProperty js/document "hidden" prior-visibility)
                   (js-delete js/document "hidden"))
                 (done)))))))

(deftest stale-preference-rollback-cannot-overwrite-a-newer-choice
  (async done
         (go
           (let [room (str (random-uuid)) connection {:session-id (random-uuid) :epoch (random-uuid)}
                 requests (chan 3) reports (atom [])]
             (try
               (with-redefs [sig/notify-prefs (signal runtime {})
                             cr/set-notify-pref! (fn [& _] (fn [ok error] (put! requests {:ok ok :error error})))
                             remote/on-error (fn [_] #(swap! reports conj %))]
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection connection)
                   (notify/set-notify-pref! room :all)
                   (notify/set-notify-pref! room :none))
                 (let [old (<! requests) latest (<! requests)]
                   ((:error old) (ex-info "obsolete failure" {}))
                   (binding [ec/*execution-context* runtime] (is (= :none (get @sig/notify-prefs room))))
                   (is (empty? @reports))
                   (binding [ec/*execution-context* runtime] (reset! epoch/connection (assoc connection :epoch (random-uuid))))
                   ((:error latest) (ex-info "prior epoch failure" {}))
                   (binding [ec/*execution-context* runtime] (is (= :none (get @sig/notify-prefs room))))
                   (is (empty? @reports))))
               (catch :default error (is false (str error)))
               (finally (done)))))))
