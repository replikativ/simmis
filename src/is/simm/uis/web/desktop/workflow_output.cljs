(ns is.simm.uis.web.desktop.workflow-output
  "Request-scoped output review independent of an authoring form or submission tab."
  (:require [org.replikativ.spindel.dom.elements :as el]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.effects.track :refer [track]]
            [org.replikativ.spindel.incremental.interval :as iv]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.observation-io :as io]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.workflows-remote :as remote]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(defn state-key [room job]
  [(:id (web/current-session)) (str room) (str job)])

(defn controller [room run]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        job (str (:id run)) key (state-key room job)
        state #(binding [ec/*execution-context* ctx] (get @sig/workflow-output-reviews key))
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (web/current-session? session)
                      (apply swap! sig/workflow-output-reviews update key f args))))
        observe! (io/controller ctx session state update!)
        generation #(binding [ec/*execution-context* ctx]
                      [(:status run) (:retry (state)) @sig/user-rooms
                       (:control-dirty @sync/state) (get-in @sync/state [:dirty (str room)])])
        current? #(binding [ec/*execution-context* ctx]
                    (and (web/current-session? session) (connection/snapshot)
                         (= (:inspection-basis (state)) (generation))
                         (= (:owner (state)) [(connection/snapshot) (:fork-id (ec/home-context ctx))])))]
    {:state state :current? current?
     :ensure (fn []
               (let [owner [(connection/snapshot) (:fork-id (ec/home-context ctx))]]
                 (when-not (= owner (:owner (state)))
                   ;; Retire old transport ownership, preserving historical
                   ;; receipts. Never replay a mutating collection on reconnect.
                   (update! #(-> % (assoc :owner owner)
                                 (dissoc :inspection :inspection-basis)
                                 (update :observations dissoc :collect))))
                 (observe! :inspect generation
                           #(remote/inspect-run-wiki-output! web/server-id (str room) job)
                           (fn [state reply]
                             (when-not (or (= {:status :none} reply)
                                           (and (= :eligible (:status reply)) (= job (:job reply))
                                                (string? (:request reply)) (parse-uuid (:request reply))))
                               (throw (js/Error. "Invalid workflow request descriptor")))
                             (-> state (assoc :inspection reply :inspection-basis (generation))
                                 (update :errors dissoc :inspect)))
                           (fn [state error] (-> state (dissoc :inspection :inspection-basis)
                                                (assoc-in [:errors :inspect] error))))))
     :retry #(update! update :retry (fnil inc 0))
     :collect (fn []
                (when (and (current?) (= :eligible (get-in (state) [:inspection :status]))
                           (nil? (get-in (state) [:observations :collect :active])))
                  (let [request (get-in (state) [:inspection :request]) nonce (random-uuid)]
                    ;; Metadata changes caused by filing must not erase its
                    ;; acknowledged receipt. Session/socket/world fences remain.
                    (observe! :collect (constantly [request nonce])
                              #(remote/collect-workflow-wiki-output! web/server-id (str room) request)
                              (fn [state reply]
                                (when-not (or (= {:status :unavailable} reply)
                                              (and (= request (:request reply))
                                                   (or (= :no-op (:status reply))
                                                       (and (= :proposed (:status reply)) (string? (:proposal reply))
                                                            (parse-uuid (:proposal reply))))))
                                  (throw (js/Error. "Invalid workflow output receipt")))
                                (-> state (assoc :collection reply) (update :errors dissoc :collect)))
                              #(assoc-in %1 [:errors :collect] %2)))))
     :open (fn []
             (binding [ec/*execution-context* ctx]
               (when (and (current?) (= :proposed (get-in (state) [:collection :status]))
                          (= (get-in (state) [:inspection :request]) (get-in (state) [:collection :request])))
                 (refs/open! {:kind :proposal :id (get-in (state) [:collection :proposal])}))))}))

(defn view [room run]
  (spin
   (let [states-i (track sig/workflow-output-reviews)
         socket-i (track connection/connection)
         stream-i (track sync/state)
         roster-i (track sig/user-rooms)
         states (iv/get-new states-i)
         _socket (iv/get-new socket-i) _stream (iv/get-new stream-i) _roster (iv/get-new roster-i)
         actions (controller room run) state (get states (state-key room (:id run)))
         eligible? (and ((:current? actions)) (= :eligible (get-in state [:inspection :status])))]
     ((:ensure actions))
     (when (or eligible? (get-in state [:errors :inspect]))
       (el/section {:class "run-section workflow-output-review"}
                   (el/h3 {} "Knowledge output")
                   (if-not eligible?
                     (el/div {} (el/p {:role "status"} "Output status is unavailable. Check room access and try again.")
                             (el/button {:on-click (fn [_] ((:retry actions)))} "Try again"))
                     (el/div {}
                             (el/p {} "Collect this completed workflow's checked wiki output into a proposal. This never reruns inference or accepts files.")
                             (el/button {:disabled (or (not= :completed (:status run))
                                                       (some? (get-in state [:observations :collect :active])))
                                         :on-click (fn [_] ((:collect actions)))}
                                        (if (get-in state [:observations :collect :active]) "Collecting wiki output…" "Review wiki output"))
                             (when-let [error (get-in state [:errors :collect])] (el/p {:role "status"} error))
                             (case (get-in state [:collection :status])
                               :proposed (el/button {:on-click (fn [_] ((:open actions)))} "Open wiki proposal")
                               :no-op (el/p {:role "status"} "No wiki changes were proposed.")
                               :unavailable (el/p {:role "status"} "Output is not available for review. Retrying collection never reruns the workflow.")
                               nil))))))))
