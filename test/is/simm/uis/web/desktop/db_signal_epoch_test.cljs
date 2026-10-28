(ns is.simm.uis.web.desktop.db-signal-epoch-test
  (:require [cljs.test :refer-macros [deftest is async]]
            [clojure.core.async :refer [go <! put! chan promise-chan alts! timeout] :include-macros true]
            [datahike.versioning :as versioning]
            [datahike.kabel.connector :as connector]
            [datahike.optimistic :as optimistic]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.connection-epoch :as epoch]
            [is.simm.uis.web.desktop.db-signal :as db]
            [is.simm.uis.web.desktop.chat-remote :as chat]
            [is.simm.uis.web.desktop.replica-io :as io])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn- delivered [value] (doto (promise-chan) (put! value)))

(deftest reentrant-session-retirement-cannot-resurrect-the-old-db-summary
  (binding [ec/*execution-context* runtime]
    (let [conn (atom {}) overlay (js-obj) key (random-uuid)]
      (with-redefs [db/local-conn (atom conn) db/local-overlay (atom overlay)
                    db/local-db (signal runtime nil) db/local-db-signal (signal runtime nil)
                    db/room-states (signal runtime {}) db/room-connecting (atom {})
                    db/kb-conns (atom {}) db/kb-connecting (atom {}) db/view-db-signals (atom {})
                    db/replica-sync (signal runtime {})
                    io/retire-all! (constantly []) io/close-overlay! (fn [_])
                    optimistic/db (constantly {:max-tx 999})]
        (try
          (add-watch db/local-db key (fn [_ _ _ value] (when value (db/clear-session!))))
          (db/refresh-db!)
          (is (nil? @db/local-conn))
          (is (nil? @db/local-db))
          (is (nil? @db/local-db-signal))
          (finally (remove-watch db/local-db key)))))))

(deftest initial-bootstrap-follows-a-new-authenticated-epoch
  (async done
         (go
           (let [session (random-uuid) a {:session-id session :epoch (random-uuid)}
                 b (assoc a :epoch (random-uuid)) requests (chan 2)
                 conn-a (atom {}) conn-b (atom {})]
             (try
               (with-redefs [db/local-conn (atom nil) db/local-overlay (atom nil)
                             db/local-db (signal runtime nil) db/local-db-signal (signal runtime nil)
                             db/replica-sync (signal runtime {})
                             io/open! (fn [_ opts]
                                        (let [result (promise-chan)]
                                          (put! requests (assoc opts :result result)) result))
                             io/overlay! (fn [handle] (:conn handle))
                             optimistic/listen! (fn [& _]) optimistic/listen-status! (fn [& _])
                             optimistic/db (fn [_] {:max-tx 1})]
                 (binding [ec/*execution-context* runtime] (reset! epoch/connection a))
                 (let [boot (db/init-reactive-db! :peer) first-request (<! requests)]
                   (binding [ec/*execution-context* runtime] (reset! epoch/connection b))
                   (let [second-request (<! requests)]
                     ((:publish! first-request) {:conn conn-a} (:valid? first-request))
                     (is (nil? @db/local-conn) "Late initial constructor cannot publish")
                     (put! (:result first-request) {:status :ready :resource {:conn conn-a}})
                     ((:publish! second-request) {:conn conn-b} (:valid? second-request))
                     (put! (:result second-request) {:status :ready :resource {:conn conn-b}})
                     (is (= :ready (<! boot)))
                     (is (identical? conn-b @db/local-conn))
                     (binding [ec/*execution-context* runtime]
                       (is (= b (get-in @db/replica-sync [[:system] :connection])))
                       (is (= :ready (get-in @db/replica-sync [[:system] :status])))))))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest revoked-pending-scope-cannot-publish-or-reopen-from-stale-ui
  (async done
         (go
           (let [scope (random-uuid) sk (str scope) connection {:session-id (random-uuid) :epoch (random-uuid)}
                 request (promise-chan) result (promise-chan) retired (atom []) opens (atom 0)]
             (try
               (with-redefs [db/local-conn (atom nil) db/room-states (signal runtime {})
                             db/room-connecting (atom {}) db/kb-conns (atom {}) db/kb-overlays (atom {})
                             db/kb-connecting (atom {}) db/replica-sync (signal runtime {})
                             io/open! (fn [_ opts] (swap! opens inc) (put! request opts) result)
                             io/retire! (fn [cfg] (swap! retired conj (get-in cfg [:store :id])) (delivered true))]
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection connection)
                   (db/resync-open! connection {:rooms [] :knowledge-bases [{:kb/db-scope scope}]} :peer)
                   (db/connect-kb! scope :peer))
                 (let [opts (<! request)]
                   (binding [ec/*execution-context* runtime]
                     (db/resync-open! connection {:rooms [] :knowledge-bases [{:kb/db-scope scope}]} :peer)
                     (is (= 1 @opens) "A current-epoch pending constructor is not restarted")
                     (db/resync-open! connection {:rooms [] :knowledge-bases []} :peer)
                     (is (false? ((:valid? opts))))
                     ((:publish! opts) {:conn (atom {})} (:valid? opts))
                     (db/connect-kb! scope :peer)
                     (is (nil? (db/get-kb-conn scope)))
                     (is (nil? (get @db/kb-connecting sk))))
                   (put! result {:status :ready :resource {:conn (atom {})}})
                   (is (= [scope] @retired))
                   (is (= 1 @opens))))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest same-peer-catch-up-preserves-connection-and-optimistic-overlay
  (async done
         (go
           (let [peer (js-obj) conn (atom {:writer {:local-peer peer}}) overlay (js-obj)
                 connection {:session-id (random-uuid) :epoch (random-uuid)}
                 started (promise-chan) fresh-head (promise-chan) ready (promise-chan) key (random-uuid)]
             (try
               (with-redefs [db/local-conn (atom conn) db/local-overlay (atom overlay)
                             db/room-states (signal runtime {}) db/room-connecting (atom {})
                             db/kb-conns (atom {}) db/kb-connecting (atom {}) db/replica-sync (signal runtime {})
                             connector/resubscribe! (fn
                                                      ([_] (delivered nil))
                                                      ([actual opts]
                                                       (is (identical? conn actual))
                                                       (is (= (:epoch connection) (:epoch opts)))
                                                       (put! started true) fresh-head))]
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection connection)
                   (add-watch db/replica-sync key
                              (fn [_ _ _ value]
                                (when (= :ready (get-in value [[:system] :status])) (put! ready true))))
                   (db/resync-open! connection {:rooms [] :knowledge-bases []} peer))
                 (<! started)
                 (binding [ec/*execution-context* runtime]
                   (is (= :pending (get-in @db/replica-sync [[:system] :status]))))
                 (is (identical? conn @db/local-conn))
                 (is (identical? overlay @db/local-overlay))
                 (put! fresh-head {:status :ready})
                 (<! ready)
                 (is (identical? conn @db/local-conn))
                 (is (identical? overlay @db/local-overlay))
                 (binding [ec/*execution-context* runtime] (remove-watch db/replica-sync key)))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest old-ready-publication-cannot-invalidate-the-next-epochs-projection
  (async done
         (go
           (let [scope (random-uuid) sk (str scope) peer (js-obj)
                 conn (atom {:writer {:local-peer peer}})
                 a {:session-id (random-uuid) :epoch (random-uuid)} b (assoc a :epoch (random-uuid))
                 ref {:forkset :proposal :branches {sk "proposal"}}
                 started (promise-chan) fresh-head (promise-chan) published (promise-chan)
                 projection-started (promise-chan) projection-result (promise-chan) projected (promise-chan)
                 selected (atom nil) key (random-uuid) projection-key (random-uuid)]
             (try
               (with-redefs [db/local-conn (atom nil) db/room-states (signal runtime {})
                             db/room-connecting (atom {}) db/kb-conns (atom {sk conn})
                             db/kb-connecting (atom {}) db/view-db-signals (atom {}) db/replica-sync (signal runtime {})
                             chat/prepare-store! (fn [& _] (fn [ok _] (ok true)))
                             connector/resubscribe! (fn ([_] (delivered nil))
                                                      ([_ _] (put! started true) fresh-head))
                             versioning/branch-as-db (fn ([_ _] (delivered nil))
                                                       ([_ _ _] (put! projection-started true) projection-result))]
                 (binding [ec/*execution-context* runtime]
                   (reset! epoch/connection a)
                   (add-watch db/replica-sync key
                              (fn [_ _ _ value]
                                (when (= :ready (get-in value [[:kb sk] :status]))
                                  (reset! epoch/connection b)
                                  (let [s (db/ensure-view-db-signal! scope ref)]
                                    (reset! selected s)
                                    (reset! s {:keep :new-epoch})
                                    (add-watch s projection-key (fn [_ _ _ value]
                                                                  (when (= {:fresh :projection} value) (put! projected true)))))
                                  (put! published true))))
                   (db/resync-open! a {:rooms [] :knowledge-bases [{:kb/db-scope scope}]} peer))
                 (<! started)
                 (put! fresh-head {:status :ready})
                 (<! published)
                 (<! projection-started)
                 (binding [ec/*execution-context* runtime] (is (= {:keep :new-epoch} @@selected)))
                 (put! projection-result {:fresh :projection})
                 (<! projected)
                 (binding [ec/*execution-context* runtime]
                   (is (= {:fresh :projection} @@selected))
                   (remove-watch @selected projection-key)
                   (remove-watch db/replica-sync key)))
               (catch :default error (is false (str error)))
               (finally (done)))))))

(deftest eviction-clears-held-projections-and-late-reads-cannot-refill-them
  (async done
         (go
           (let [scope (random-uuid) sk (str scope) connection {:session-id (random-uuid) :epoch (random-uuid)}
                 ref {:forkset :proposal :branches {sk "proposal"}}
                 reads (chan 2) result-a (promise-chan) result-b (promise-chan) calls (atom 0)]
             (try
               (with-redefs [db/kb-conns (atom {sk (atom {})}) db/view-db-signals (atom {})
                             versioning/branch-as-db (fn
                                                       ([_ _] (delivered nil))
                                                       ([_ _ _]
                                                        (let [n (swap! calls inc)]
                                                          (put! reads n) (if (= n 1) result-a result-b))))]
                 (binding [ec/*execution-context* runtime] (reset! epoch/connection connection))
                 (let [old (binding [ec/*execution-context* runtime] (db/ensure-view-db-signal! scope ref))]
                   (<! reads)
                   (binding [ec/*execution-context* runtime]
                     (reset! old {:private :old})
                     (db/evict-view-db-for-scope! scope)
                     (is (nil? @old) "An already-held projection loses revoked data immediately"))
                   (let [replacement (binding [ec/*execution-context* runtime] (db/ensure-view-db-signal! scope ref))]
                     (<! reads)
                     (is (not (identical? old replacement)))
                     (put! result-a {:private :stale})
                     (put! result-b {:private :fresh})
                ;; A signal watch is the completion event, not a sleep.
                     (let [landed (promise-chan) key (random-uuid)]
                       (binding [ec/*execution-context* runtime]
                         (add-watch replacement key (fn [_ _ _ value] (when value (put! landed true))))
                         (when @replacement (put! landed true)))
                       (<! landed)
                       (binding [ec/*execution-context* runtime]
                         (remove-watch replacement key)
                         (is (nil? @old))
                         (is (= {:private :fresh} @replacement)))))))
               (catch :default error (is false (str error)))
               (finally (done)))))))


(deftest a-view-requested-before-roster-opens-once-when-authorized
  (async done
    (go
      (let [scope (random-uuid) unused (random-uuid)
            connection {:session-id (random-uuid) :epoch (random-uuid)}
            request (promise-chan) result (promise-chan) opens (atom [])]
        (try
          (with-redefs [db/local-conn (atom nil) db/room-states (signal runtime {})
                        db/room-connecting (atom {}) db/kb-conns (atom {}) db/kb-overlays (atom {})
                        db/kb-db-signals (atom {}) db/kb-connecting (atom {})
                        db/replica-sync (signal runtime {})
                        io/open! (fn [cfg opts] (swap! opens conj (get-in cfg [:store :id]))
                                   (put! request opts) result)]
            (binding [ec/*execution-context* runtime]
              (reset! epoch/connection connection)
              ;; Exactly the cold direct-page sequence: its signal exists,
              ;; but current authorization has not arrived yet.
              (db/ensure-kb-db-signal! scope)
              (db/connect-kb! scope :peer)
              (is (empty? @opens))
              (is (nil? (db/get-kb-conn scope)))
              (db/resync-open! connection {:rooms [] :knowledge-bases [{:kb/db-scope scope} {:kb/db-scope unused}]} :peer))
            (let [opts (<! request)]
              (is (= [scope] @opens) "Only the requested KB opens")
              (is (true? ((:valid? opts))))
              (binding [ec/*execution-context* runtime]
                (db/resync-open! connection {:rooms [] :knowledge-bases [{:kb/db-scope scope} {:kb/db-scope unused}]} :peer)
                (db/connect-kb! scope :peer))
              (is (= [scope] @opens) "Roster/render repeats do not restart a pending open")
              (let [finished (promise-chan) watch-key (random-uuid)]
                (add-watch db/replica-sync watch-key
                           (fn [_ _ _ state]
                             (when (= :error (get-in state [[:kb (str scope)] :status]))
                               (put! finished true))))
                (try
                  (put! result {:status :superseded})
                  (let [[_ port] (alts! [finished (timeout 2000)])]
                    (is (identical? finished port) "The open continuation completed before fixtures retire"))
                  (finally (remove-watch db/replica-sync watch-key))))))
          (catch :default error (is false (str error)))
          (finally (done)))))))
