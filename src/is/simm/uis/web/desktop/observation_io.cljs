(ns is.simm.uis.web.desktop.observation-io
  "Context-owned observation I/O. Immutable fetch frames live in caller signals;
   native callbacks never become application state or admission authority."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.observation :as observation]
            [is.simm.uis.web.desktop.remote :as remote]
            [is.simm.runtimes.web :as web]))

(defn controller [ctx session state update!]
  (let [epoch #(when-let [socket (connection/snapshot)]
                 (when (web/current-session? session)
                   [socket (:fork-id (ec/home-context ctx))]))]
    (fn observe! [slot generation invoke consume failure & [after]]
      (binding [ec/*execution-context* ctx]
        (let [path [:observations slot]
              old (get-in (state) path)
              ;; Compute dependencies again at callback time. A newer signal
              ;; may be committed before the rendering Spin has resumed.
              [next frame] (observation/offer old (epoch) (generation) (random-uuid))
              finish! (fn [reply error]
                        (binding [ec/*execution-context* ctx]
                          (when (and (= (:epoch frame) (epoch))
                                     (observation/owns? (get-in (state) path) frame))
                            (let [publish? (and (= (:generation frame) (generation))
                                                (observation/current? (get-in (state) path) frame))]
                              (update!
                               (fn [s]
                                 (let [s (update-in s path observation/finish frame publish?)]
                                   (if-not publish? s
                                           (if error
                                             (failure s (remote/error-text error))
                                             (try (assoc-in (consume s reply) [:observation-accepted slot] (:token frame))
                                                  (catch :default e (failure s (remote/error-text e)))))))))
                              (when (and after (= (:token frame) (get-in (state) [:observation-accepted slot])))
                                (after reply))))))]
          (when (not= old next) (update! assoc-in path next))
          (when frame
            (try
              ;; It must survive invalidation of the render which requested it.
              (binding [ec/*spin-id* nil]
                ((invoke) #(if-let [error (remote/result-failure %)]
                             (finish! nil error) (finish! % nil))
                          #(finish! nil %)))
              (catch :default error (finish! nil error)))))))))
