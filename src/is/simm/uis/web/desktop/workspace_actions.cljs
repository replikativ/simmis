(ns is.simm.uis.web.desktop.workspace-actions
  "Context-owned one-shot human creation. Native dispatch requires the live UI;
   frozen intents and historical outcomes remain independent in copied worlds."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.observation-io :as observation-io]
            [is.simm.uis.web.desktop.workspace-editor :as editor]
            [is.simm.uis.web.desktop.workspace-remote :as remote]
            [is.simm.uis.web.desktop.chat-remote :as chat]
            [is.simm.uis.web.desktop.user-rooms-sync :as roster]
            [is.simm.uis.web.desktop.remote :as rpc]
            [is.simm.runtimes.web :as web]))

(defn state-key [data] [(:id (web/current-session)) (:tab-id data)])

(defn- location [columns data]
  (let [matches (for [column columns tab (:tabs column)
                     :when (= (:tab-id data) (:id tab))]
                  {:column (:id column) :tab tab :active? (= (:active-tab column) (:id tab))})
        match (when (= 1 (count matches)) (first matches))]
    (when (= (:tab-type data) (get-in match [:tab :type])) match)))

(defn controller [data]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session) key (state-key data)
        current? #(and (:id session) (web/current-session? session))
        state #(binding [ec/*execution-context* ctx] (get @sig/workspace-creation-state key))
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (current?) (apply swap! sig/workspace-creation-state update key f args))))
        live? #(and (current?) (identical? (ec/home-context ctx) (ec/home-context runtime)))
        observe! (observation-io/controller ctx session state update!)]
    (letfn [(invoke! [operation]
              (binding [ec/*execution-context* ctx]
                (let [submission (:submission (state)) token (random-uuid)
                      owns? #(and (current?) (= submission (:submission (state)))
                                  (= token (get-in (state) [:requests operation]))
                                  (or (not= :observe operation)
                                      (= (get-in (state) [:request-connections operation]) (connection/snapshot)))
                                  (not= token (get-in (state) [:finished operation])))
                      failure (fn [error]
                                (when (owns?)
                                  (update! #(cond-> (-> % (assoc-in [:finished operation] token)
                                                        (assoc-in [:loading operation] false))
                                              (not (editor/created? (:outcome %)))
                                              (assoc :phase :unconfirmed :error (rpc/error-text error))))))]
                  (when (and submission (current?) (connection/snapshot)
                             (not (get-in (state) [:loading operation])))
                    (update! #(-> % (assoc-in [:requests operation] token)
                                  (assoc-in [:request-connections operation] (connection/snapshot))
                                  (assoc-in [:loading operation] true)))
                    (try
                      (binding [ec/*spin-id* nil]
                        (((case operation :create remote/create-workspace! :observe remote/observe-workspace-creation!)
                          web/server-id submission)
                         (fn [reply]
                           (binding [ec/*execution-context* ctx]
                             (when (owns?)
                               (update! #(-> (editor/observed % submission reply)
                                             (assoc-in [:finished operation] token)
                                             (assoc-in [:loading operation] false)))
                               (when (and (live?) (editor/created? (:outcome (state)))
                                          (not (:roster-requested? (state))))
                                 (update! assoc :roster-requested? true)
                                 (when-let [party (:id @sig/current-user)]
                                   (roster/refresh-user-rooms! party))))))
                         #(binding [ec/*execution-context* ctx] (failure %))))
                      (catch :default error (failure error)))))))
            (create! []
              (binding [ec/*execution-context* ctx]
                (cond
                  (:submission (state)) nil
                  (not (live?)) (update! assoc :error "Create workspaces from the live interface.")
                  (nil? (location @sig/layout-columns data)) nil
                  (nil? (connection/snapshot)) (update! assoc :error "Reconnect before creating a workspace.")
                  (not (editor/valid? (state))) (update! assoc :error "Enter a name of up to 512 characters.")
                  :else (do (update! editor/submit (random-uuid) (connection/snapshot))
                            (invoke! :create)))))
            (ensure! []
              (binding [ec/*execution-context* ctx]
                (when (and (current?) (:submission (state)) (= :submitting (:phase (state)))
                           (not= (:submitted-on (state)) (connection/snapshot)))
                  (update! #(-> % (assoc :phase :unconfirmed)
                                (assoc-in [:loading :create] false))))
                (when (and (current?) (get-in (state) [:loading :observe])
                           (not= (get-in (state) [:request-connections :observe]) (connection/snapshot)))
                  ;; Only the read can be retried explicitly. Retire its native
                  ;; token so a delayed old socket reply cannot become current.
                  (update! #(-> % (assoc-in [:loading :observe] false)
                                (update :requests dissoc :observe)
                                (update :finished dissoc :observe))))
                (when (and (current?) (not= :memory (:landing data)))
                  (observe! :people #(:people-retry (state) 0)
                            #(chat/list-parties! web/server-id :human)
                            (fn [s people]
                              (when-not (and (vector? people) (<= (count people) 4096))
                                (throw (ex-info "The people list is unavailable; invite people from room settings later." {})))
                              (-> s (assoc :people people) (dissoc :people-error)))
                            #(assoc %1 :people-error %2)))
                (open! true)))
            (open! [& [automatic?]]
              (binding [ec/*execution-context* ctx]
                (when (and (live?) (editor/created? (:outcome (state))))
                  (when-let [{:keys [column active?]} (location @sig/layout-columns data)]
                    ;; Automatic navigation belongs to the original foreground
                    ;; form and connection. A historical ACK survives reconnect
                    ;; for explicit recovery, but cannot move the user's focus.
                    (when (or (not automatic?)
                              (and active? (= column (sig/get-active-column-id @sig/layout-columns))
                                   (= (:submitted-on (state)) (connection/snapshot))))
                      (let [created (get-in (state) [:outcome :room])
                            room (some #(when (= (:room/id created) (str (:room/id %))) %) (:rooms @sig/user-rooms))]
                        (when room
                          (let [memory? (= :memory (:landing data))]
                            (sig/open-tab! (if memory? :memory :chat)
                                           (cond-> {:room-id (str (:room/id room)) :room-name (:room/name room)}
                                             memory? (assoc :path "wiki/Welcome.md")
                                             (not memory?) (assoc :db-scope (str (:room/content-db-scope room))))
                                           {:col-id column :new-tab? true
                                            :title (if memory? "Welcome.md" (:room/name room))})
                            (sig/close-tab! column (:tab-id data))))))))))]
      {:owner key :state state :ensure ensure! :create create! :open open!
       :check #(invoke! :observe)
       :set-name #(when-not (:submission (state)) (update! assoc :name %))
       :toggle-member #(when-not (:submission (state))
                         (update! update :members (fn [members]
                                                    (let [members (set members)]
                                                      (if (contains? members %) (disj members %) (conj members %))))))})))
