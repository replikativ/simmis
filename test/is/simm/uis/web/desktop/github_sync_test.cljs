(ns is.simm.uis.web.desktop.github-sync-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.dom.browser :as browser]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-actions :as memory]
            [is.simm.uis.web.desktop.github-sync :as sync]
            [is.simm.uis.web.desktop.github-remote :as remote]
            [is.simm.uis.web.desktop.views.github-sync :as view]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn- act! [a operation]
  ((get a operation) ((:basis a))))

(defn- with-client [f]
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} room (str (random-uuid)) calls (atom [])
          m (signal runtime {:target-stamp (random-uuid) :permissions {:evaluate? true}
                             :github-source {:kind :github :repository "owner/repository"
                                             :ref "refs/heads/main" :available? true}})
          send (fn [operation request]
                 (fn [ok error] (swap! calls conj {:operation operation :request request :ok ok :error error})))
          ready {:status :ready :ready-id (str (random-uuid)) :can-discard? true
                 :total 1 :changes [{:path "<script>alert(1)</script>.md" :kind :added}]}]
      (with-redefs [web/current-session (constantly session) web/current-session? #(= session %)
                    sig/github-sync-state (signal runtime {})
                    sig/user-rooms (signal runtime {:rooms [{:room/id room}]})
                    sig/memory-connection (signal runtime {:epoch (random-uuid)})
                    connection/snapshot #(binding [ec/*execution-context* runtime] @sig/memory-connection)
                    memory/verified? (fn [s _] (not (:unverified? s)))
                    remote/sync-github-memory! (fn [_ _ operation request] (send operation request))
                    remote/load-github-memory-sync! (fn [_ _ request] (send :observe request))
                    remote/retire-github-memory-sync! (fn [_ _ request] (send :retire request))]
        (let [a (sync/controller {:room-id room} #(deref m))]
          (f {:a a :state (:state a) :memory m :calls calls :ready ready :room room}))))))

(deftest frozen-review-has-one-apply-and-unknown-ack-is-never-resent
  (with-client
    (fn [{:keys [a state calls ready]}]
      (act! a :sync) (act! a :sync)
      (is (= [:prepare] (mapv :operation @calls)))
      (let [intent (:request (first @calls))]
        ((:ok (first @calls)) ready)
        (is ((:can-apply? a)))
        (act! a :apply) (act! a :apply)
        (is (= [:prepare :apply] (mapv :operation @calls)))
        (is (= {:client-id (:client-id intent) :ready-id (:ready-id ready)} (:request (second @calls))))
        ((:error (second @calls)) (js/Error. "Lost ACK"))
        (act! a :apply) (act! a :sync)
        (is (= 2 (count @calls)))
        (act! a :check)
        (is (= :observe (:operation (last @calls))))
        (is (= {:client-id (:client-id intent)} (:request (last @calls))))
        ((:ok (last @calls)) {:status :unconfirmed})
        (act! a :recover)
        (is (= :recover (:operation (last @calls))))
        ((:ok (last @calls)) {:status :satisfied-observed})
        (is (false? (:open? (state))))))))

(deftest reconnect-revocation-and-copied-contexts-cannot-confirm-stale-reviews
  (with-client
    (fn [{:keys [a state calls ready memory room]}]
      (act! a :sync)
      ((:ok (first @calls)) ready)
      (reset! sig/memory-connection {:epoch (random-uuid)})
      (act! a :apply)
      (is (= 1 (count @calls)))
      ((:ensure a))
      (is (= :observe (:operation (last @calls))))
      ((:ok (last @calls)) ready)
      (is ((:can-apply? a)))
      (swap! memory assoc :unverified? true)
      (act! a :apply)
      (is (= 2 (count @calls)))
      (swap! memory dissoc :unverified?)
      (let [child (context/fork-context runtime)]
        (try
          (let [parent-offer ((:basis a))]
            (binding [ec/*execution-context* child]
              ((:apply a) parent-offer) ((:recover a) parent-offer) ((:sync a) parent-offer))
            (is (= 2 (count @calls)) "An existing parent controller cannot borrow its owner from a child"))
          (let [forked (binding [ec/*execution-context* child] (sync/controller {:room-id room} #(deref memory)))]
            (act! forked :apply) (act! forked :sync) (act! forked :recover)
            (is (= 2 (count @calls)))
            (is (= :ready (get-in (state) [:reply :status]))))
          (finally (context/close-context! child)))))))

(deftest mounted-review-renders-paths-as-text-and-detached-buttons-are-inert
  (with-client
    (fn [{:keys [a state calls ready memory]}]
      (act! a :sync) ((:ok (first @calls)) ready)
      (let [container (.createElement js/document "div") stop (atom nil)]
        (.appendChild (.-body js/document) container)
        (try
          (reset! stop (browser/mount! container (view/view @memory a (state))))
          (is (.includes (.-textContent container) "<script>alert(1)</script>.md"))
          (is (nil? (.querySelector container "script")))
          (let [button (some #(when (= "Apply reviewed update" (.-textContent %)) %) (array-seq (.querySelectorAll container "button")))]
            (is (some? button))
            (.remove button)
            (.click button)
            (is (= 1 (count @calls))))
          (finally
            (when @stop (browser/unmount! @stop))
            (.remove container)))))))

(deftest known-local-history-refusal-explains-what-is-kept-and-closes-only-the-attempt
  (with-client
    (fn [{:keys [a state calls memory]}]
      (act! a :sync)
      ((:ok (first @calls)) {:status :held :reason :local-history :can-discard? true})
      (let [container (.createElement js/document "div") stop (atom nil)]
        (.appendChild (.-body js/document) container)
        (try
          (reset! stop (browser/mount! container (view/view @memory a (state))))
          (is (.includes (.-textContent container) "Your edits are saved in this Memory."))
          (is (.includes (.-textContent container) "nothing was pushed"))
          (is (not (.includes (.-textContent container) "This sync needs inspection")))
          (let [close (some #(when (= "Close attempt (keep edits)" (.-textContent %)) %)
                            (array-seq (.querySelectorAll container "button")))]
            (is (some? close))
            (.click close)
            (is (= [:prepare :discard] (mapv :operation @calls)))
            (is (= (get-in (first @calls) [:request :client-id])
                   (get-in (second @calls) [:request :client-id]))))
          (finally
            (when @stop (browser/unmount! @stop))
            (.remove container)))))))

(deftest connected-old-review-buttons-cannot-approve-or-discard-a-later-intent
  (with-client
    (fn [{:keys [a state calls ready memory]}]
      (act! a :sync) ((:ok (first @calls)) ready)
      (let [container (.createElement js/document "div") stop (atom nil)]
        (.appendChild (.-body js/document) container)
        (try
          (reset! stop (browser/mount! container (view/view @memory a (state))))
          (let [buttons (array-seq (.querySelectorAll container "button"))
                apply-button (some #(when (= "Apply reviewed update" (.-textContent %)) %) buttons)
                discard-button (some #(when (= "Discard private preparation" (.-textContent %)) %) buttons)]
            (act! a :apply) ((:ok (last @calls)) {:status :applied})
            (act! a :sync) ((:ok (last @calls)) (assoc ready :ready-id (str (random-uuid))))
            (let [before (count @calls)]
              (is (.-isConnected apply-button))
              (.click apply-button) (.click discard-button)
              (is (= before (count @calls)))
              (is (= :ready (get-in (state) [:reply :status])))))
          (finally (when @stop (browser/unmount! @stop)) (.remove container)))))))

(deftest stale-connected-retirement-button-does-not-select-a-new-sync
  (with-client
    (fn [{:keys [a state calls memory]}]
      (swap! memory assoc-in [:github-source :can-retire?] true)
      (swap! memory assoc-in [:github-source :sync-id] (str (random-uuid)))
      (let [container (.createElement js/document "div") stop (atom nil)]
        (.appendChild (.-body js/document) container)
        (try
          (reset! stop (browser/mount! container (view/view @memory a (state))))
          (let [button (some #(when (= "Discard previous owner's prepared sync" (.-textContent %)) %)
                             (array-seq (.querySelectorAll container "button")))]
            (is (some? button))
            (swap! memory assoc-in [:github-source :sync-id] (str (random-uuid)))
            (.click button)
            (is (empty? @calls)))
          (finally (when @stop (browser/unmount! @stop)) (.remove container)))))))

(deftest malformed-async-replies-settle-the-same-intent-without-stranding-loading
  (with-client
    (fn [{:keys [a state calls ready]}]
      (act! a :sync)
      (let [intent (:intent (state))]
        ((:ok (first @calls)) {:status :ready :changes [] :total 0})
        (is (false? (:loading? (state))))
        (is (= :unconfirmed (get-in (state) [:reply :status])))
        (is (= intent (:intent (state))))
        (act! a :check)
        ((:ok (last @calls)) (assoc ready :changes (mapv #(hash-map :path (str % ".md") :kind :modified) (range 2048)) :total 2048))
        (is (= :ready (get-in (state) [:reply :status])))
        (is (false? (:loading? (state))))))))

(deftest retired-old-metadata-cannot-overwrite-a-new-own-review
  (with-client
    (fn [{:keys [a state calls ready memory]}]
      (let [old-id (str (random-uuid))]
        (swap! memory update :github-source assoc :can-retire? true :sync-active? true :sync-id old-id)
        (act! a :retire)
        ((:ok (last @calls)) {:status :closed :sync-id old-id})
        (is (= old-id (:retired-sync-id (state))))
        (act! a :sync)
        ((:ok (last @calls)) ready)
        (let [before (count @calls) intent (:intent (state))]
          (act! a :retire)
          (is (= before (count @calls)))
          (is (= intent (:intent (state))))
          (is (= :ready (get-in (state) [:reply :status]))))))))
