(ns is.simm.uis.web.desktop.conversation-capture-actions
  "Explicit capture commands, owned by the rendered account/room/message view."
  (:require [is.simm.uis.web.desktop.conversation-capture :as model]
            [is.simm.uis.web.desktop.conversation-capture-remote :as remote]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.runtimes.web :as web]
            [org.replikativ.spindel.engine.core :as ec]))

(defn state-key [account room message] [(str account) (str room) (str message)])

(defn controller [{:keys [room-id room-name message-id col-id tab-id thread-root-id frame room-evidence?]}]
  (let [ctx (or ec/*execution-context* runtime)
        session (web/current-session)
        key (state-key (:account frame) room-id (if room-evidence? :room-evidence message-id))
        ;; Spins may extend the execution environment without forking its
        ;; home. Compare native home ownership, not the wrapper map identity.
        own? #(and (some? (ec/home-context ctx))
                   (identical? (ec/home-context ctx) (ec/home-context runtime))
                   (web/current-session? session)
                   (= (:session frame) (:id session))
                   (= (:account frame) (:id @sig/current-user)))
        visible? #(let [col (some (fn [c] (when (= col-id (:id c)) c)) @sig/layout-columns)
                        tab (some (fn [t] (when (= tab-id (:id t)) t)) (:tabs col))]
                    (and (= tab-id (:active-tab col))
                         (= (if thread-root-id :chat-thread :chat) (:type tab))
                         (= (str room-id) (str (get-in tab [:data :room-id])))
                         (= (some-> thread-root-id str)
                            (some-> (get-in tab [:data :thread-root-id]) str))))
        state #(get @sig/conversation-captures key)
        update! (fn [f & args]
                  (when (own?) (apply swap! sig/conversation-captures update key f args)))
        current-view? #(and (visible?) (= (:connection frame) (connection/snapshot)))
        settle! (fn [kind token reply]
                  (binding [ec/*execution-context* ctx]
                    (when (own?)
                      (if-not (current-view?)
                        (when (model/current-request? (state) (:connection frame) token
                                                      (if (= kind :preview) :preview-loading :publishing))
                          (update! model/hide))
                        (update! (if (= kind :preview)
                                   (if room-evidence? model/receive-room-preview model/receive-preview)
                                   (if room-evidence? model/receive-room-publish model/receive-publish))
                                 (:connection frame) token reply)))))
        send! (fn [kind token invoke]
                (try
                  (binding [ec/*spin-id* nil]
                    ((invoke) #(settle! kind token %) #(settle! kind token nil)))
                  (catch :default _ (settle! kind token nil))))]
    {:preview
     (fn []
       (binding [ec/*execution-context* ctx]
         (when (and (own?) (current-view?))
           (let [token (random-uuid)]
             (update! (if room-evidence? model/begin-room-preview model/begin-preview) (:connection frame) token [message-id])
             (when (model/current-request? (state) (:connection frame) token :preview-loading)
               (send! :preview token
                      #((if room-evidence? remote/preview-room-evidence! remote/preview-conversation-capture!)
                        web/server-id (str room-id) (if room-evidence? {} {:message-ids [message-id]}))))))))
     :publish
     (fn []
       (binding [ec/*execution-context* ctx]
         (when (and (own?) (current-view?))
           (let [token (random-uuid)]
             (update! (if room-evidence? model/begin-room-publish model/begin-publish) (:connection frame) token)
             (when (model/current-request? (state) (:connection frame) token :publishing)
               (let [p (:preview (state))]
                 (send! :publish token
                        #((if room-evidence? remote/publish-room-evidence! remote/publish-conversation-capture!)
                          web/server-id (str room-id)
                          (cond-> {:capture-id (:capture-id p) :expected-target-stamp (:target-stamp p)}
                            (not room-evidence?) (assoc :message-ids (:message-ids p)))))))))))
     :hide #(binding [ec/*execution-context* ctx] (update! model/hide))
     :open
     (fn []
       (binding [ec/*execution-context* ctx]
         (when (and (own?) (current-view?))
           (let [s (model/reconcile (state) (connection/snapshot))
                 p (or (:saved s) (:preview s))]
             (when (and p (#{:saved :inspection-required} (:phase s)))
               ;; Independent authorized inspection, never a reconstructed ACK.
               (navigation/open-file! room-id room-name (if room-evidence? (get-in s [:preview :artifacts 0 :path]) (:path p)) col-id (:target-stamp p)))))))}))
