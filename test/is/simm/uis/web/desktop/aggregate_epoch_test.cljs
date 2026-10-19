(ns is.simm.uis.web.desktop.aggregate-epoch-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [clojure.core.async :refer [go <! put! promise-chan] :include-macros true]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.connection-epoch :as epoch]
            [is.simm.uis.web.desktop.aggregate :as aggregate]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.remote :as remote]
            [is.simm.uis.web.desktop.feed-remote :as fr]
            [is.simm.uis.web.desktop.tasks-remote :as tr]
            [is.simm.uis.web.desktop.accounting-remote :as ar]
            [is.simm.uis.web.desktop.proposals-remote :as pr]
            [is.simm.uis.web.desktop.experiments-remote :as er]
            [is.simm.uis.web.desktop.views.feed :as feed]
            [is.simm.uis.web.desktop.views.tasks :as tasks]
            [is.simm.uis.web.desktop.views.accounting :as accounting]
            [is.simm.uis.web.desktop.views.proposals :as proposals]
            [is.simm.uis.web.desktop.views.leaderboard :as leaderboard])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(deftest all-five-callers-can-replace-a-hung-request-without-stale-writes
  (async done
    (let [calls (atom []) first-ready (promise-chan) second-ready (promise-chan)
          first-landed (promise-chan) second-landed (promise-chan)
          capture (fn [key]
                    (fn [& _]
                      (fn [ok _]
                        (let [n (count (swap! calls conj [key ok]))]
                          (when (= n 5) (put! first-ready true))
                          (when (= n 10) (put! second-ready true))))))
          start (fn []
                  (feed/load-feed!) (tasks/load-tasks! true)
                  (accounting/load-position!) (proposals/load-proposals!)
                  (leaderboard/load-leaderboard! "epoch-test" :fingerprint))]
      (go
        (try
          ;; Keep replacements installed across the channel waits, not merely
          ;; around dispatch: the actual remote constructors run in go blocks.
          (with-redefs [sig/feed-data (signal runtime nil)
                        sig/tasks-data (signal runtime nil)
                        sig/accounting-data (signal runtime nil)
                        sig/proposals-data (signal runtime nil)
                        sig/leaderboards (signal runtime {})
                        fr/load-feed! (capture :feed)
                        tr/list-tasks! (capture :tasks)
                        ar/load-position! (capture :accounting)
                        pr/list-proposals! (capture :proposals)
                        er/load-leaderboard! (capture :leaderboard)]
            (binding [ec/*execution-context* runtime]
              (reset! epoch/connection {:epoch (random-uuid)}) (start))
            (<! first-ready)
            (binding [ec/*execution-context* runtime]
              (reset! epoch/connection {:epoch (random-uuid)}) (start))
            (<! second-ready)
            (doseq [[_ ok] (take 5 @calls)] (ok []))
            ;; These sentinels are queued after the callbacks they witness.
            ;; No elapsed-time assumption is needed to observe their effects.
            (go (put! first-landed true))
            (<! first-landed)
            (binding [ec/*execution-context* runtime]
              (is (every? nil? [@sig/feed-data @sig/tasks-data @sig/accounting-data @sig/proposals-data]))
              (is (= {} @sig/leaderboards)))
            (doseq [[_ ok] (drop 5 @calls)] (ok []))
            (go (put! second-landed true))
            (<! second-landed)
            (binding [ec/*execution-context* runtime]
              (is (= [] (:items @sig/feed-data)))
              (is (= true (:include-done? @sig/tasks-data)))
              (is (= [] (:books @sig/accounting-data)))
              (is (= [] (:proposals @sig/proposals-data)))
              (is (= :fingerprint (get-in @sig/leaderboards ["epoch-test" :attempts])))))
          (catch :default e (is false (str e)))
          (finally (done)))))))

(deftest proposal-child-requests-keep-their-own-epoch-after-parent-completes
  (binding [ec/*execution-context* runtime]
    (let [callbacks (atom {}) checks (atom 0)
          fresh! #(reset! epoch/connection {:epoch (random-uuid)})
          diffs {:forks [{:system-type :repo}]}]
      (with-redefs [sig/proposals-data (signal runtime nil)
                    aggregate/run! (fn
                                     ([_ fetch!] (fetch! (fn []) (constantly true)))
                                     ([_ _ _ fetch!] (fetch! (fn []) (constantly true))))
                    remote/spin! (fn
                                   ([make-spin cb] (swap! callbacks assoc (make-spin) cb))
                                   ([make-spin cb _] (swap! callbacks assoc (make-spin) cb)))
                    pr/list-proposals! (fn [& _] :list)
                    pr/proposal-diff! (fn [& _] :diff)
                    pr/proposal-checks! (fn [& _] (swap! checks inc) :checks)]
        (fresh!)
        (proposals/load-proposals!)
        ((:list @callbacks) [{:id "proposal"}] nil)
        (fresh!)
        ((:diff @callbacks) diffs nil)
        (is (nil? (:diffs (first (:proposals @sig/proposals-data)))))
        (is (zero? @checks))
        (proposals/load-proposals!)
        ((:list @callbacks) [{:id "proposal"}] nil)
        ((:diff @callbacks) diffs nil)
        (is (= 1 @checks))
        (fresh!)
        ((:checks @callbacks) [:stale-check] nil)
        (is (nil? (:checks (first (:proposals @sig/proposals-data)))))))))
