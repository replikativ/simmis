(ns is.simm.uis.web.desktop.room-digest-actions
  (:require [hasch.core :as hasch]
            [is.simm.uis.web.desktop.room-digest :as model]
            [is.simm.uis.web.desktop.room-digest-remote :as remote]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.runtimes.web :as web]
            [org.replikativ.spindel.engine.core :as ec]))

(defn state-key
  ([account room] [(str account) (str room) "room-digest"])
  ([account room message] [(str account) (str room) "room-digest-notification" (str message)]))

(defn controller [{:keys [room-id message-id col-id tab-id thread-root-id frame notification]}]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        key (if notification (state-key (:account frame) room-id message-id) (state-key (:account frame) room-id))
        state #(get @sig/conversation-captures key)
        own? #(and (some? (ec/home-context ctx)) (identical? (ec/home-context ctx) (ec/home-context runtime))
                   (web/current-session? session) (= (:session frame) (:id session))
                   (= (:account frame) (:id @sig/current-user)))
        visible? #(let [col (some (fn [c] (when (= col-id (:id c)) c)) @sig/layout-columns)
                        tab (some (fn [t] (when (= tab-id (:id t)) t)) (:tabs col))]
                    (and (= tab-id (:active-tab col)) (= (str room-id) (str (get-in tab [:data :room-id])))
                         (= (if thread-root-id :chat-thread :chat) (:type tab))
                         (= (some-> thread-root-id str) (some-> (get-in tab [:data :thread-root-id]) str))))
        current? #(and (own?) (visible?) (= (:connection frame) (connection/snapshot)))
        update! (fn [f & args] (when (own?) (apply swap! sig/conversation-captures update key f args)))
        invoke! (fn [operation request]
                  (binding [ec/*execution-context* ctx]
                    (when (current?)
                      (let [token (random-uuid)]
                        (when (and notification (= :result operation))
                          (update! assoc :request (:request notification) :expected-digest-id (:digest-id notification)))
                        (update! model/begin (:connection frame) token operation message-id)
                        (when (model/current? (state) (:connection frame) token)
                          (when (= :launch operation)
                            (update! assoc :request (hasch/uuid [:simmis/work-request-v1
                                                                (uuid (str (:account frame)))
                                                                (uuid (str room-id)) (:request-id request)])))
                          (let [settle (fn [reply]
                                         (binding [ec/*execution-context* ctx]
                                           (when (own?)
                                             (if (current?) (update! model/receive (:connection frame) token reply)
                                                 (when (model/current? (state) (:connection frame) token)
                                                   (update! model/hide))))))
                                call (case operation :preview remote/preview-room-digest! :install remote/install-room-digest!
                                           :launch remote/launch-room-digest! :result remote/read-room-digest! :publish remote/publish-room-digest!)]
                            (try (binding [ec/*spin-id* nil]
                                   ((call web/server-id (str room-id) request) settle #(settle nil)))
                                 (catch :default _ (settle nil)))))))))]
    {:preview #(invoke! :preview {:paths %})
     :install #(when (js/confirm "Install the optional digest workflow into its empty source folder? This starts no work and grants no allowance.")
                 (invoke! :install {:expected-target-stamp (get-in (state) [:preview :target-stamp])}))
     :launch (fn [model]
               (when (js/confirm "Run this optional digest with the selected model? Maximum 25,000 microdollars, 4 dispatches, 8 steps and 180 seconds. Uses existing room allowance; no funding or schedule changes.")
                 (let [p (:preview (state))]
                   (invoke! :launch {:paths (mapv :path (get-in p [:selection :sources]))
                                     :selection-id (get-in p [:selection :id]) :expected-target-stamp (:target-stamp p)
                                     :expected-source-revision (:source-revision p) :request-id (random-uuid) :model model}))))
     :result #(when-let [request (or (:request notification) (:request (state)))] (invoke! :result {:request-id request}))
     :publish #(when-let [r (:result (state))]
                 (invoke! :publish {:request-id (:request r) :digest-id (:digest-id r)}))
     :model #(binding [ec/*execution-context* ctx] (when (current?) (update! assoc :model %)))
     :hide #(binding [ec/*execution-context* ctx] (update! model/hide))}))
