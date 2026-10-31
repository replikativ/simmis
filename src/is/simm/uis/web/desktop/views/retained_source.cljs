(ns is.simm.uis.web.desktop.views.retained-source
  "One read-only frozen source; no editing, evaluation or adoption authority."
  (:require [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.effects.track :refer [track]]
            [org.replikativ.spindel.incremental.interval :as iv]
            [org.replikativ.spindel.dom.elements :as el]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.retained-source :as source]
            [is.simm.uis.web.desktop.observation-io :as io]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.proposals-remote :as remote]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.spin.cps :refer [spin]]))

(defn- chunk-bytes [result]
  (try
    (let [encoded (get-in result [:data :base64])]
      (when (and (string? encoded) (<= (count encoded) 21848))
        (let [binary (js/atob encoded)]
          (when (= encoded (js/btoa binary))
            (mapv #(.charCodeAt binary %) (range (count binary)))))))
    (catch :default _ nil)))

(defn- decode [bytes]
  (try (.decode (js/TextDecoder. "utf-8" #js {:fatal true}) (js/Uint8Array. (clj->js bytes)))
       (catch :default _ nil)))

(defn controller [data]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        identity (select-keys data source/identity-keys) key [(:id session) (:tab-id data)]
        tab #(source/current-tab @sig/layout-columns (:tab-id data) identity)
        current? #(and (web/current-session? session) (tab))
        state #(binding [ec/*execution-context* ctx] (get @sig/retained-source-state key))
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (current?) (apply swap! sig/retained-source-state update key f args))))
        marker #(binding [ec/*execution-context* ctx]
                  (when (and (current?) (connection/snapshot))
                    [identity (connection/snapshot) @sig/user-rooms (:control-dirty @sync/state)]))
        generation #(vector (marker) (:retry (state)) (source/step (state)))
        observe! (io/controller ctx session state update!)
        visible #(when (= (marker) (:marker (state))) (state))]
    {:state visible
     :retry #(update! (fn [s] {:retry (inc (or (:retry s) 0))}))
     :open-proposal (fn []
                      (binding [ec/*execution-context* ctx]
                        (when-let [location (when (current?) (tab))]
                          (refs/open! {:kind :proposal :id (:proposal-id identity)}
                                      {:col-id (:col-id location) :new-tab? true}))))
     :ensure
     (fn []
       (when-let [next-marker (marker)]
         (when-not (= next-marker (:marker (state)))
           (update! (fn [s] {:marker next-marker :status :listing :retry (:retry s)})))
         (when-let [step (source/step (visible))]
           (observe!
            :source generation
            #(case (:kind step)
               :list (if (:request-id identity)
                       (remote/list-request-sources! web/server-id (:room-id identity) (:request-id identity))
                       (remote/list-proposal-sources! web/server-id (:proposal-id identity) (:component-id identity)))
               :chunk (if (:request-id identity)
                        (remote/read-request-source! web/server-id (:room-id identity) (:request-id identity)
                                                     {:path (:path identity) :offset (:offset step) :length 16384})
                        (remote/read-proposal-source! web/server-id (:proposal-id identity) (:component-id identity)
                                                     {:path (:path identity) :offset (:offset step) :length 16384})))
            (fn [s result]
              (let [next (case (:kind step)
                           :list (source/accept-list identity s result)
                           :chunk (source/accept-chunk s result (chunk-bytes result)))]
                (if (= :received (:status next)) (source/finish next (decode (:bytes next))) next)))
            (fn [s _] (-> s (assoc :status :unavailable) (dissoc :bytes :text)))))))}))

(defn view [data]
  (spin
   (let [_state (iv/get-new (track sig/retained-source-state))
         _layout (iv/get-new (track sig/layout-columns))
         _connection (iv/get-new (track connection/connection))
         _roster (iv/get-new (track sig/user-rooms))
         _sync (iv/get-new (track sync/state))
         actions (controller data) state ((:state actions))]
     ((:ensure actions))
     (el/section {:class "memory-panel" :aria-label "Frozen workflow source"}
                 (el/div {:class "memory-file-header"}
                         (el/strong {} (:path data))
                         (when (:proposal-id data)
                           (el/button {:on-click (fn [_] ((:open-proposal actions)))} "Open proposal")))
                 (el/p {:class "memory-note"} "Frozen evidence used by this workflow. Read only; independent of today's Memory.")
                 (when (:request-id data)
                   (el/p {:class "memory-note"} (str "Request " (:request-id data))))
                 (case (:status state)
                   :ready (el/div {}
                                  (el/p {:class "memory-note"}
                                        (str (get-in state [:entry :source-path]) " · content " (get-in state [:entry :content-id])))
                                  (el/pre {:class "proposal-retained-source"} (:text state)))
                   :unavailable (el/div {:role "status"}
                                        (el/p {} "This frozen source is unavailable or is not listed in the retained manifest.")
                                        (el/button {:on-click (fn [_] ((:retry actions)))} "Try again"))
                   (el/p {:class "memory-note" :role "status"} "Loading frozen source…"))))))
