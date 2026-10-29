(ns is.simm.uis.memory-report-navigation-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :as runtime]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.runtimes.web :as web]))

(defn- with-state [f]
  (let [ctx (context/fork-context runtime/runtime) session {:id (random-uuid)} epoch (atom :first)
        room (str (random-uuid)) data {:room-id room :room-name "Room" :tab-id "chat"}
        calls (atom {:metadata [] :open [] :folder []})]
    (try
      (with-redefs [web/current-session (constantly session) web/current-session? #(= % session)
                    connection/snapshot #(deref epoch) connection/current? #(= % @epoch)
                    remote/load-memory-metadata! (fn [_ room request]
                                                   (let [construction ec/*execution-context*]
                                                     (fn [ok error]
                                                       (swap! calls update :metadata conj {:room room :request request :ok ok :error error
                                                                                           :construction construction :context ec/*execution-context* :spin ec/*spin-id*}))))
                    navigation/open-file! (fn [room title path & [column stamp]]
                                            (swap! calls update :open conj [room title path column stamp]))
                    navigation/focus-folder! (fn [room folder] (swap! calls update :folder conj [room folder]))]
        (binding [ec/*execution-context* ctx]
          (reset! sig/layout-columns [{:id "left" :active-tab "chat" :tabs [{:id "chat" :type :chat :data data}]}])
          (f {:ctx ctx :data data :epoch epoch :calls calls :open! (navigation/room-folder-action data "reports")})))
      (finally (context/close-context! ctx)))))

(deftest explicit-report-navigation-prefers-an-existing-index-in-the-original-column
  (with-state
    (fn [{:keys [ctx data calls open!]}]
      (let [stamp (random-uuid)]
        (binding [ec/*execution-context* runtime/runtime ec/*spin-id* :foreign] (open!))
        (is (= {:folders [] :paths ["reports/index.md"]} (:request (first (:metadata @calls)))))
        (is (identical? ctx (:construction (first (:metadata @calls)))))
        (is (identical? ctx (:context (first (:metadata @calls)))))
        (is (nil? (:spin (first (:metadata @calls)))))
        (swap! sig/layout-columns assoc-in [0 :id] "right")
        (binding [ec/*execution-context* runtime/runtime]
          ((:ok (first (:metadata @calls))) {:target-stamp stamp :entries {"reports/index.md" {:content (random-uuid)}}}))
        (is (= [[(:room-id data) "Room" "reports/index.md" "right" stamp]] (:open @calls)))
        (is (empty? (:folder @calls)))))))

(deftest no-index-focuses-the-folder-and-a-late-reply-cannot-retarget-another-tab
  (with-state
    (fn [{:keys [data calls open! epoch]}]
      (open!)
      ((:ok (first (:metadata @calls))) {:target-stamp (random-uuid) :entries {"reports/index.md" nil}})
      (is (= [[(:room-id data) "reports"]] (:folder @calls)))
      (is (empty? (:open @calls)))
      (open!)
      (swap! sig/layout-columns assoc-in [0 :active-tab] "other")
      ((:ok (second (:metadata @calls))) {:target-stamp (random-uuid) :entries {"reports/index.md" {:content (random-uuid)}}})
      (is (empty? (:open @calls)))
      (is (= 1 (count (:folder @calls))))
      (open!)
      (is (= 2 (count (:metadata @calls))) "Inactive original tab does not start navigation")
      (swap! sig/layout-columns assoc-in [0 :active-tab] "chat")
      (open!)
      (reset! epoch :second)
      ((:ok (nth (:metadata @calls) 2)) {:target-stamp (random-uuid) :entries {"reports/index.md" {:content (random-uuid)}}})
      (is (empty? (:open @calls)))
      (is (= 1 (count (:folder @calls)))))))

(deftest closed-and-repurposed-tabs-ignore-pending-folder-navigation
  (doseq [replace [(fn [_] [])
                   #(assoc-in % [0 :tabs 0 :data :room-id] "other-room")
                   #(assoc-in % [0 :tabs 0 :type] :leaderboard)]]
    (with-state
      (fn [{:keys [calls open!]}]
        (open!)
        (swap! sig/layout-columns replace)
        ((:ok (first (:metadata @calls))) {:target-stamp (random-uuid) :entries {"reports/index.md" {:content (random-uuid)}}})
        (is (empty? (:open @calls)))
        (is (empty? (:folder @calls)))))))
