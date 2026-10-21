(ns is.simm.uis.web.desktop.memory-sync
  "One private hint subscription per authenticated socket. Hints never contain
   files; owning views reconcile authorized metadata after readiness/reconnect."
  (:require [is.simm.uis.web.desktop.remote :as remote]
            [is.simm.uis.web.desktop.topic-subscription :as subscription]
            [is.simm.runtimes.web :as web]))

(def ^:private owner (subscription/owner))
(def state (:state owner))

(defn- options
  [connection]
  {:connection connection :peer (:client (web/current-session))
   :topic (keyword "memory" (str (:party connection))) :initial {:dirty {}}
   :on-message (fn [{:keys [kind room-id]}]
                 (when (= kind :memory-changed)
                   (swap! state update-in [:dirty (str room-id)] (fnil inc 0))))})

(defn ensure!
  "Call only after prepare-memory-stream! registered the party's private topic.
   Readiness means the handshake callback, not merely subscribe request sent."
  [connection]
  (subscription/ensure! owner (options connection)))

(defn retry!
  "Explicit error retry, using the existing owner and exact-strategy disposal."
  [connection]
  (subscription/retry! owner (options connection)))

(defn problem [stream]
  (when (= :error (:status stream))
    (str "Live Memory updates unavailable: " (remote/error-text (:error stream)))))
