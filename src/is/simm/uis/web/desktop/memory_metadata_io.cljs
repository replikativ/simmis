(ns is.simm.uis.web.desktop.memory-metadata-io
  "One context-owned read coordinator per authenticated Room. Native callbacks
   publish immutable metadata only; view reconciliation belongs to the renderer."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-metadata :as metadata]
            [is.simm.uis.web.desktop.memory-metadata-view :as view]
            [is.simm.uis.web.desktop.memory-owner :as owner]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.observation-io :as observation-io]
            [is.simm.uis.web.desktop.remote :as rpc]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.runtimes.web :as web]))

(defn controller [ctx session room requesting-owner state-source]
  (let [room (str room) room-key [(:id session) room]
        current? #(and (:id session) (web/current-session? session))
        available? #(let [rooms @sig/user-rooms]
                      (or (nil? rooms)
                          (some (fn [entry] (= room (str (:room/id entry)))) (:rooms rooms))))
        state #(get @sig/memory-room-metadata room-key)
        update! (fn [f & args]
                  (when (current?)
                    (apply swap! sig/memory-room-metadata update room-key f args)))
        epoch #(when-let [socket (connection/snapshot)]
                 (when (current?) [socket (:fork-id (ec/home-context ctx))]))
        admissions
        (fn []
          (let [columns @sig/layout-columns
                stream @sync/state
                stream-marker [(:connection stream) (:status stream) (get-in stream [:dirty room] 0)]
                states (merge @sig/memory-editor-state @sig/memory-navigation-state @state-source)
                tabs (into {}
                           (for [column columns tab (:tabs column)
                                 :when (and (= :memory (:type tab))
                                            (= room (str (get-in tab [:data :room-id])))
                                            (owner/current-tab columns room (:id tab)))]
                             [(owner/key (:id session) (assoc (:data tab) :tab-id (:id tab)))
                              (:data tab)]))
                keys (into (set (keys tabs))
                           (keep (fn [[key s]]
                                   (when (and (= (:id session) (first key)) (= room (second key))
                                              (not= :tab (nth key 2 nil))
                                              (or (:metadata-interest? s)
                                                  (:root-expanded? s))
                                              (or (not= :navigation (nth key 2 nil))
                                                  (:root-expanded? s))) key))) states)]
            (into {}
                  (for [key keys
                        :let [s (get states key)
                              s (if (:selected s) s (assoc s :selected (:path (get tabs key))))
                              s (if (:stream-marker s) s (assoc s :stream-marker stream-marker))]]
                    [key (view/admission (connection/snapshot) s)]))))
        ;; An unsupported demand is refused by its own controller. It must
        ;; never prevent a valid sibling from observing the same Room.
        valid-admissions #(into {} (filter (fn [[_ frame]]
                                            (try (metadata/normalize-request (:request frame)) true
                                                 (catch :default _ false)))) (admissions))
        plans #(metadata/plans (valid-admissions)
                               [(:invalidation (state) 0)
                                (select-keys @sync/state [:connection :status :control-dirty])
                                (get-in @sync/state [:dirty room] 0) @sig/user-rooms])
        observe! (observation-io/controller ctx session state update!)
        publish (fn [s plan reply error]
                  (when (and reply (not (and (= 2 (:protocol-version reply))
                                             (uuid? (:target-stamp reply))
                                             (metadata/covered? reply (:request plan)))))
                    (throw (ex-info "Memory returned incomplete metadata; reload or retry." {})))
                  (let [sequence (inc (:sequence s 0))
                        envelope (assoc plan :epoch (epoch) :sequence sequence
                                             :reply reply :error error)
                        ;; A newly observed target retires other batches without
                        ;; pretending their independently read snapshots are atomic.
                        envelopes (if reply
                                    (into {} (filter (fn [[_ e]]
                                                       (= (:target-stamp reply)
                                                          (get-in e [:reply :target-stamp]))))
                                          (:envelopes s))
                                    (:envelopes s))]
                    (cond-> (assoc s :sequence sequence
                                   :envelopes (assoc envelopes (:request plan) envelope))
                      error (assoc :failure envelope)
                      reply (assoc :target {:epoch (epoch) :stamp (:target-stamp reply)})
                      reply (dissoc :failure))))]
    {:available? #(binding [ec/*execution-context* ctx] (available?))
     :included? #(binding [ec/*execution-context* ctx]
                   (contains? (valid-admissions) requesting-owner))
     :target #(binding [ec/*execution-context* ctx]
                (when (and (available?) (= (epoch) (get-in (state) [:target :epoch])))
                  (get-in (state) [:target :stamp])))
     :invalidate (fn [] (binding [ec/*execution-context* ctx]
                          (update! #(-> % (update :invalidation (fnil inc 0))
                                        (dissoc :envelopes :failure)))))
     :observation (fn []
                    (binding [ec/*execution-context* ctx]
                      (when (available?)
                        (or (when (some #(metadata/envelope-current? (:failure (state)) % (epoch)) (plans))
                              (:failure (state)))
                            (metadata/observation-for (state) (plans) (epoch) requesting-owner)))))
     :ensure (fn []
               (binding [ec/*execution-context* ctx]
                 (when (and (current?) (not (available?))
                            (some #(contains? (state) %) [:envelopes :failure :target]))
                   (update! dissoc :envelopes :failure :target))
                 (when (and (current?) (epoch) (available?))
                   (let [current-plans (plans)
                         wanted (set (map :request current-plans))]
                     (when (some #(not (contains? wanted %)) (keys (:envelopes (state))))
                       (update! update :envelopes select-keys wanted))
                     (when-let [plan (metadata/next-plan (state) current-plans (epoch))]
                       (observe! :metadata
                                 #(when (available?) (some (fn [current]
                                          (when (= (:request plan) (:request current))
                                            (:generation current))) (plans)))
                                 (fn []
                                   (fn [ok _]
                                     (let [failed (fn [error]
                                                    (ok {::failure {:message (rpc/error-text error)
                                                                   :type (:type (rpc/error-info error))}}))]
                                       (try
                                         ((remote/load-memory-metadata! web/server-id room (:request plan))
                                          #(if-let [error (rpc/result-failure %)] (failed error) (ok %))
                                          failed)
                                         (catch :default error (failed error))))))
                                 #(if-let [error (::failure %2)]
                                    (publish %1 plan nil error)
                                    (publish %1 plan %2 nil))
                                 #(publish %1 plan nil {:message %2})))))))}))
