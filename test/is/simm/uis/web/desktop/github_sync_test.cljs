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
            [is.simm.uis.web.desktop.github-credentials :as credentials]
            [is.simm.uis.web.desktop.personal-connections :as personal]
            [is.simm.uis.web.desktop.git-policy :as policy]
            [is.simm.uis.web.desktop.github-remote :as remote]
            [is.simm.uis.web.desktop.refs :as refs]
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
                    sig/personal-connections-state (signal runtime {})
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
      (is (= [:prepare-policy] (mapv :operation @calls)))
      (let [intent (:request (first @calls))]
        ((:ok (first @calls)) ready)
        (is ((:can-apply? a)))
        (act! a :apply) (act! a :apply)
        (is (= [:prepare-policy :apply] (mapv :operation @calls)))
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
          (reset! stop (browser/mount! container (view/review-pull @memory a (state))))
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
          (reset! stop (browser/mount! container (view/review-pull @memory a (state))))
          (is (.includes (.-textContent container) "Your edits are saved in this Memory."))
          (is (.includes (.-textContent container) "nothing was pushed"))
          (is (not (.includes (.-textContent container) "This sync needs inspection")))
          (let [close (some #(when (= "Close attempt (keep edits)" (.-textContent %)) %)
                            (array-seq (.querySelectorAll container "button")))]
            (is (some? close))
            (.click close)
            (is (= [:prepare-policy :discard] (mapv :operation @calls)))
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
          (reset! stop (browser/mount! container (view/review-pull @memory a (state))))
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
          (reset! stop (browser/mount! container (view/retirement-control @memory a (state))))
          (let [button (some #(when (= "Close previous owner's Git preparation" (.-textContent %)) %)
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

(deftest filed-replies-require-a-proposal-id-before-navigation
  (doseq [reply [{:status :filed} {:status :filed :proposal-id nil}
                 {:status :filed :proposal-id "invalid"}]]
    (with-client
      (fn [{:keys [a state calls]}]
        (let [opened (atom [])]
          (with-redefs [refs/open! #(swap! opened conj %)]
            (act! a :sync)
            (let [intent (:intent (state))]
              ((:ok (first @calls)) reply)
              (is (false? (:loading? (state))))
              (is (= :unconfirmed (get-in (state) [:reply :status])))
              (is (= intent (:intent (state))))
              (is (empty? @opened)))))))))

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

(deftest outbound-review-is-separate-and-an-uncertain-send-is-observed-without-resending
  (with-client
    (fn [{:keys [room memory calls ready state]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))]
        (act! a :sync)
        (is (= [:prepare-push] (mapv :operation @calls)))
        (is (nil? (:intent (state))) "Incoming controller has no outbound intent")
        ((:ok (last @calls)) (assoc ready :operation-kind :push))
        (act! a :apply)
        ((:error (last @calls)) (js/Error. "Lost send response"))
        (is (= [:prepare-push :apply :observe] (mapv :operation @calls)))
        (act! a :apply) (act! a :sync)
        (is (= 3 (count @calls)))
        ((:ok (last @calls)) {:status :pushed :operation-kind :push})
        (is (= [:prepare-push :apply :observe :recover] (mapv :operation @calls)))
        ((:ok (last @calls)) {:status :closed :operation-kind :push :outcome :pushed})
        (is (= :closed (get-in ((:state a)) [:reply :status])))
        (act! a :sync)
        (is (= :prepare-push (:operation (last @calls))) "A completed publish permits a new edit/push cycle")))))

(deftest automatic-push-verification-requires-a-same-journal-observation-and-stops-on-failure
  (with-client
    (fn [{:keys [room memory calls ready]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))
            id (str (random-uuid))]
        (act! a :sync)
        (let [client (get-in (first @calls) [:request :client-id])
              sending {:status :unconfirmed :operation-kind :push :client-id client :sync-id id}]
          ((:ok (last @calls)) (assoc ready :operation-kind :push :client-id client :sync-id id))
          (act! a :apply)
          ((:error (last @calls)) (js/Error. "Lost ACK"))
          ((:ok (last @calls)) {:status :unconfirmed :operation-kind :push})
          (is (= [:prepare-push :apply :observe] (mapv :operation @calls)) "Generic uncertainty cannot authorize remote verification")
          (act! a :check-status)
          ((:ok (last @calls)) sending)
          (is (= :recover (:operation (last @calls))))
          (is (= {:client-id client} (:request (last @calls))))
          ((:error (last @calls)) (js/Error. "Verification unavailable"))
          (let [count-before (count @calls)]
            ((:ensure a)) ((:ensure a))
            (is (= count-before (count @calls)) "Renders cannot retry failed recovery"))
          (act! a :check-status)
          ((:ok (last @calls)) sending)
          (is (= :recover (:operation (last @calls))) "An explicit click may request one new verification")
          ((:ok (last @calls)) {:status :closed :operation-kind :push :client-id client :sync-id id :outcome :upstream-observed})
          (is (false? (:open? ((:state a)))))
          (is (= 1 (count (filter #(= :apply (:operation %)) @calls)))))))))

(deftest automatic-recovery-is-never-authorized-by-a-different-journal
  (with-client
    (fn [{:keys [room memory calls ready]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory)) id (str (random-uuid))]
        (act! a :sync)
        (let [client (get-in (first @calls) [:request :client-id])]
          ((:ok (last @calls)) (assoc ready :operation-kind :push :client-id client :sync-id id))
          (act! a :apply)
          ((:error (last @calls)) (js/Error. "Lost ACK"))
          ((:ok (last @calls)) {:status :unconfirmed :operation-kind :push :client-id client :sync-id (str (random-uuid))})
          (is (= [:prepare-push :apply :observe] (mapv :operation @calls)))
          (is (= :unconfirmed (get-in ((:state a)) [:reply :status]))))))))

(deftest copied-or-stale-callbacks-cannot-start-automatic-push-verification
  (doseq [change [:epoch :target :source :fork]]
    (with-client
      (fn [{:keys [room memory calls ready]}]
        (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))]
          (act! a :sync) ((:ok (last @calls)) (assoc ready :operation-kind :push))
          (act! a :apply)
          (let [failed (:error (last @calls))]
            (case change
              :epoch (reset! sig/memory-connection {:epoch (random-uuid)})
              :target (swap! memory assoc :target-stamp (random-uuid))
              :source (swap! memory assoc-in [:github-source :ref] "refs/heads/other")
              :fork nil)
            (if (= :fork change)
              (let [child (context/fork-context runtime)]
                (try (binding [ec/*execution-context* child] (failed (js/Error. "Copied callback")))
                     (finally (context/close-context! child))))
              (failed (js/Error. "Stale callback")))
            (is (= 2 (count @calls)) (str change " cannot start a new read chain"))))))))

(deftest pending-outbound-intent-is-adopted-only-by-the-outbound-controller
  (with-client
    (fn [{:keys [a room memory calls state]}]
      (let [client (str (random-uuid))
            push (sync/controller {:room-id room :github-direction :push} #(deref memory))]
        (swap! memory update :github-source assoc :sync-kind :push :sync-active? true
               :sync-client-id client :sync-id (str (random-uuid)))
        ((:ensure a))
        (is (nil? (:intent (state))))
        ((:ensure push))
        (is (= client (get-in ((:state push)) [:intent :client-id])))
        (is (= [:observe] (mapv :operation @calls)))))))

(deftest replicated-background-completion-reobserves-the-same-journal-without-replaying
  (with-client
    (fn [{:keys [a memory calls state]}]
      (let [client (str (random-uuid))]
        (swap! memory update :github-source assoc :sync-kind :incoming-policy :sync-active? true :sync-client-id client)
        ((:ensure a))
        ((:ok (last @calls)) {:status :held})
        ((:ensure a))
        (is (= 1 (count @calls)) "An unchanged render cannot retry")
        (swap! memory update :github-source dissoc :sync-active? :sync-kind :sync-client-id)
        ((:ensure a))
        (is (= [:observe :observe] (mapv :operation @calls)))
        (is (= {:client-id client} (:request (last @calls))))
        (is (= :held (get-in (state) [:reply :status])) "Metadata cannot establish success")
        ((:ok (last @calls)) {:status :applied :cleanup-pending? false})
        ((:ensure a))
        (is (= :applied (get-in (state) [:reply :status])))
        (is (= 2 (count @calls)))))))

(deftest completed-intents-yield-to-new-background-intents-but-unknown-intents-do-not
  (with-client
    (fn [{:keys [a memory calls state]}]
      (act! a :sync)
      ((:ok (last @calls)) {:status :applied :cleanup-pending? false})
      (doseq [client [(str (random-uuid)) (str (random-uuid))]]
        (swap! memory update :github-source assoc :sync-kind :incoming-policy :sync-active? true :sync-client-id client)
        ((:ensure a))
        (is (= :observe (:operation (last @calls))))
        (is (= {:client-id client} (:request (last @calls))))
        (is (nil? (:reply (state))))
        ((:ok (last @calls)) {:status :applied :cleanup-pending? false}))
      (swap! memory update :github-source dissoc :sync-active? :sync-kind :sync-client-id)
      (act! a :sync)
      ((:error (last @calls)) (js/Error. "Unknown outcome"))
      (let [original (get-in (state) [:intent :client-id])]
        (swap! memory update :github-source assoc :sync-client-id (str (random-uuid)))
        ((:ensure a))
        (is (= {:client-id original} (:request (last @calls))) "Unknown work retains its own journal")
        ((:ok (last @calls)) {:status :unconfirmed})
        (is (= original (get-in (state) [:intent :client-id])))))))

(deftest credential-controller-retains-metadata-but-never-the-token
  (with-client
    (fn [{:keys [room memory]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))
            c (credentials/controller a #(deref memory)) calls (atom [])
            token {:id (str (random-uuid)) :revision (str (random-uuid))} connection-id (str (random-uuid))]
        (with-redefs [remote/github-credential-operation!
                      (fn [_ _ operation request]
                        (fn [ok error] (swap! calls conj {:operation operation :request request :ok ok :error error})))]
          ((:ensure c))
          (is (= [:select] (mapv :operation @calls)))
          ((:ok (last @calls)) {:account-id nil :accounts [] :configured? false :revision nil :connection-id connection-id
                                :target-stamp (str (:target-stamp @memory))
                                :repository "owner/repository" :ref "refs/heads/main"})
          ((:bind c) ((:basis c)) token)
          (is (= [:select :bind] (mapv :operation @calls)))
          (is (= (:id token) (get-in (last @calls) [:request :account-id])))
          (is (not (contains? (:request (last @calls)) :token)))
          ((:error (last @calls)) (js/Error. "Do not forward provider text"))
          ((:ensure c))
          (is (= 2 (count @calls)) "A failed save is not retried by rendering")
          (is (not (.includes (pr-str @sig/github-sync-state) "provider text"))))))))

(deftest reconnect-and-source-changes-retire-credential-writes-and-only-reload-status
  (with-client
    (fn [{:keys [room memory]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))
            c (credentials/controller a #(deref memory)) calls (atom [])
            response (fn [] {:account-id nil :accounts [] :configured? false :revision nil :connection-id (str (random-uuid))
                             :target-stamp (str (:target-stamp @memory))
                             :repository (get-in @memory [:github-source :repository]) :ref "refs/heads/main"})]
        (with-redefs [remote/github-credential-operation!
                      (fn [_ _ operation request]
                        (fn [ok error] (swap! calls conj {:operation operation :request request :ok ok :error error})))]
          ((:ensure c))
          (let [old (first @calls)]
            (reset! sig/memory-connection {:epoch (random-uuid)})
            ((:ensure c))
            (is (= [:select :select] (mapv :operation @calls)))
            ((:ok old) (response))
            (is (:credential-loading? ((:state a))))
            ((:ok (last @calls)) (response)))
          ((:bind c) ((:basis c)) {:id (str (random-uuid)) :revision (str (random-uuid))})
          (let [save (last @calls)]
            (swap! memory assoc-in [:github-source :repository] "owner/another")
            ((:ensure c))
            (is (= [:select :select :bind :select] (mapv :operation @calls)))
            (is (= #{:expected-target-stamp} (set (keys (:request (last @calls))))))
            ((:error save) (js/Error. "Lost old save"))
            (is (:credential-loading? ((:state a))))
            ((:ok (last @calls)) (response))
            (is (false? (:credential-loading? ((:state a)))))))))))

(deftest stale-credential-controls-cannot-borrow-a-newly-observed-revision
  (with-client
    (fn [{:keys [room memory]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))
            c (credentials/controller a #(deref memory)) calls (atom [])
            response {:account-id (str (random-uuid)) :accounts [] :configured? true :revision (str (random-uuid)) :connection-id (str (random-uuid))
                      :target-stamp (str (:target-stamp @memory))
                      :repository "owner/repository" :ref "refs/heads/main"}]
        (with-redefs [remote/github-credential-operation!
                      (fn [_ _ operation request]
                        (fn [ok error] (swap! calls conj {:operation operation :request request :ok ok :error error})))]
          ((:ensure c))
          ((:ok (last @calls)) response)
          (let [old-offer ((:basis c))]
            ((:reload c) old-offer)
            ((:ok (last @calls)) (assoc response :revision (str (random-uuid))))
            ((:revoke c) old-offer)
            ((:bind c) old-offer {:id (str (random-uuid)) :revision (str (random-uuid))})
            (is (= [:select :select] (mapv :operation @calls)))
            ((:revoke c) ((:basis c)))
            (is (= :revoke (:operation (last @calls))))))))))

(deftest page-toolbar-is-remote-neutral-and-access-setup-opens-settings
  (with-client
    (fn [{:keys [a state room memory calls]}]
      (let [push (sync/controller {:room-id room :github-direction :push} #(deref memory))
            container (.createElement js/document "div") stop (atom nil) settings (atom 0)]
        (.appendChild (.-body js/document) container)
        (try
          (reset! stop (browser/mount! container
                                       (view/toolbar (assoc-in @memory [:github-source :last-sync]
                                                               {:at "2026-10-30T18:00:00Z" :git-oid (apply str (repeat 40 "a"))})
                                                     a (state) push ((:state push)) nil #(swap! settings inc))))
          (is (= ["Pull" "Push"] (mapv #(.-textContent %) (array-seq (.querySelectorAll container "button")))))
          (is (.includes (.-textContent container) "Last sync"))
          (is (nil? (.querySelector container "input")))
          (is (not (.includes (.-textContent container) "GitHub")))
          (.click (aget (.querySelectorAll container "button") 1))
          (is (= 1 @settings))
          (is (empty? @calls))
          (finally (when @stop (browser/unmount! @stop)) (.remove container)))))))

(deftest provider-access-is-contained-in-the-memory-settings-dialog
  (with-client
    (fn [{:keys [a state memory]}]
      (let [container (.createElement js/document "div") stop (atom nil) closed (atom false)
            credentials {:basis (constantly {}) :reload (fn [_]) :bind (fn [& _]) :revoke (fn [_])}]
        (.appendChild (.-body js/document) container)
        (try
          (reset! stop (browser/mount! container
                                       (view/settings-view {:room-name "Test Memory"} (assoc @memory :settings-open? true)
                                                           {:settings-open #(reset! closed (not %))}
                                                           a (state) {:credential-reply {:configured? false}} credentials)))
          (is (some? (.querySelector container "[role=dialog][aria-label='Memory settings']")))
          (is (nil? (.querySelector container "input[type=password]")))
          (is (.includes (.-textContent container) "Repository access"))
          (.click (.querySelector container "button[aria-label='Close Memory settings']"))
          (is @closed)
          (finally (when @stop (browser/unmount! @stop)) (.remove container)))))))

(deftest an-old-render-snapshot-cannot-borrow-a-new-review-token
  (doseq [direction [:incoming :push]]
    (with-client
      (fn [{:keys [a room memory calls ready]}]
        (let [a (if (= :push direction) (sync/controller {:room-id room :github-direction :push} #(deref memory)) a)]
          (act! a :sync) ((:ok (last @calls)) ready)
          (let [old-state ((:state a)) old-memory @memory]
            (act! a :discard) ((:ok (last @calls)) {:status :closed})
            (act! a :sync) ((:ok (last @calls)) (assoc ready :ready-id (str (random-uuid))))
            (let [container (.createElement js/document "div") stop (atom nil) before (count @calls)]
              (.appendChild (.-body js/document) container)
              (try
                (reset! stop (browser/mount! container ((if (= :push direction) view/review-push view/review-pull) old-memory a old-state)))
                (let [label (if (= :push direction) "Publish changes" "Apply reviewed update")
                      button (some #(when (= label (.-textContent %)) %) (array-seq (.querySelectorAll container "button")))]
                  (is (some? button))
                  (.click button)
                  (is (= before (count @calls))))
                (finally (when @stop (browser/unmount! @stop)) (.remove container))))))))))

(deftest an-old-credential-render-cannot-borrow-the-current-revision
  (with-client
    (fn [{:keys [room memory]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))
            c (credentials/controller a #(deref memory)) calls (atom [])
            reply {:account-id (str (random-uuid)) :accounts []
                   :configured? true :revision (str (random-uuid)) :connection-id (str (random-uuid))
                   :target-stamp (str (:target-stamp @memory)) :repository "owner/repository" :ref "refs/heads/main"}]
        (with-redefs [remote/github-credential-operation!
                      (fn [_ _ operation request]
                        (fn [ok error] (swap! calls conj {:operation operation :request request :ok ok :error error})))]
          ((:ensure c)) ((:ok (last @calls)) reply)
          (let [old-state ((:state a)) container (.createElement js/document "div") stop (atom nil)]
            ((:reload c) ((:basis c)))
            ((:ok (last @calls)) (assoc reply :revision (str (random-uuid))))
            (.appendChild (.-body js/document) container)
            (try
              (reset! stop (browser/mount! container (view/credential-view @memory old-state c)))
              (let [unlink (some #(when (= "Unlink this Memory" (.-textContent %)) %) (array-seq (.querySelectorAll container "button")))]
                (is (some? unlink)) (.click unlink)
                (is (= [:select :select] (mapv :operation @calls)))
                (is (nil? (.querySelector container "input[type=password]"))))
              (finally (when @stop (browser/unmount! @stop)) (.remove container)))))))))

(deftest personal-vault-writes-are-never-admitted-from-copied-contexts
  (with-client
    (fn [_]
      (let [calls (atom []) parent (personal/controller) offer ((:basis parent))
            child (context/fork-context runtime) id (str (random-uuid)) rev (str (random-uuid))]
        (try
          (with-redefs [remote/personal-connection-operation! (fn [& args] (swap! calls conj args) (fn [& _]))]
            (binding [ec/*execution-context* child]
              ((:ensure parent)) ((:save parent) offer id nil "GitHub" "transient") ((:revoke parent) offer id rev))
            (let [forked (binding [ec/*execution-context* child] (personal/controller))]
              ((:ensure forked)) ((:save forked) ((:basis forked)) id nil "GitHub" "transient")
              ((:revoke forked) ((:basis forked)) id rev))
            (is (empty? @calls)))
          (finally (context/close-context! child)))))))

(deftest lost-personal-save-ack-only-allows-metadata-observation
  (with-client
    (fn [_]
      (let [calls (atom []) a (personal/controller) token "transient_secret_not_signal"]
        (with-redefs [remote/personal-connection-operation!
                      (fn [_ operation request]
                        (fn [ok error] (swap! calls conj {:operation operation :request request :ok ok :error error})))]
          ((:ensure a)) ((:ok (last @calls)) [])
          ((:save a) ((:basis a)) (str (random-uuid)) nil "GitHub" token)
          (is (= token (get-in (last @calls) [:request :token])))
          (is (not (.includes (pr-str @sig/personal-connections-state) token)))
          ((:error (last @calls)) (js/Error. "Lost ACK"))
          ((:ensure a))
          (is (= [:list :save] (mapv :operation @calls)))
          ((:reload a) ((:basis a)))
          (is (= [:list :save :list] (mapv :operation @calls)))
          (is (= {} (:request (last @calls)))))))))

(deftest sync-policy-controls-freeze-revisions-and-never-resend-a-lost-save
  (with-client
    (fn [{:keys [a state memory]}]
      (let [p (policy/controller a #(deref memory)) calls (atom [])
            reply {:connection-id (str (random-uuid)) :target-stamp (str (:target-stamp @memory)) :revision (str (random-uuid))
                   :review-mode :fast-forward :enabled? false :interval-seconds 60 :status nil :last-check nil :next-check nil}]
        (with-redefs [remote/git-policy-operation! (fn [_ _ operation request]
                                                     (fn [ok error] (swap! calls conj {:operation operation :request request :ok ok :error error})))]
          ((:ensure p)) ((:ok (last @calls)) reply)
          (let [old (policy/action-basis @memory (state))]
            ((:reload p) old) ((:ok (last @calls)) (assoc reply :revision (str (random-uuid))))
            ((:save p) old {:review-mode :review-all :enabled? false :interval-seconds 60})
            (is (= [:status :status] (mapv :operation @calls)))
            ((:save p) (policy/action-basis @memory (state)) {:review-mode :review-all :enabled? false :interval-seconds 60})
            ((:error (last @calls)) (js/Error. "Lost ACK"))
            ((:ensure p))
            (is (= [:status :status :save] (mapv :operation @calls)))
            ((:save p) (policy/action-basis @memory (state)) {:review-mode :review-all :enabled? false :interval-seconds 61})
            (is (= 3 (count @calls)) "Invalid minute intervals do not issue an RPC")))))))

(deftest sync-policy-controls-do-not-admit-fork-created-or-copied-actions
  (with-client
    (fn [{:keys [a memory room]}]
      (let [p (policy/controller a #(deref memory)) child (context/fork-context runtime) calls (atom [])]
        (try
          (with-redefs [remote/git-policy-operation! (fn [& args] (swap! calls conj args) (fn [& _]))]
            (binding [ec/*execution-context* child]
              ((:ensure p)) ((:reload p) (policy/action-basis @memory ((:state a)))))
            (let [forked (binding [ec/*execution-context* child]
                           (policy/controller (sync/controller {:room-id room} #(deref memory)) #(deref memory)))]
              ((:ensure forked)))
            (is (empty? @calls)))
          (finally (context/close-context! child)))))))

(deftest refreshed-sync-telemetry-is-not-masked-by-an-older-memory-projection
  (let [old {:status :idle :last-check "2026-10-30T10:00:00Z"}
        fresh {:status :synced :last-check "2026-10-30T10:01:00Z"}]
    (is (= fresh (view/latest-policy-observation old fresh)))
    (is (= fresh (view/latest-policy-observation fresh old)))
    (is (= old (view/latest-policy-observation old nil)))))

(deftest known-incoming-outcomes-finish-cleanup-once-without-repeating-integration
  (with-client
    (fn [{:keys [a calls]}]
      (act! a :sync)
      ((:ok (first @calls)) {:status :applied :cleanup-pending? true})
      (is (= [:prepare-policy :recover] (mapv :operation @calls)))
      ;; Even a surprising pending response cannot form an automatic loop.
      ((:ok (last @calls)) {:status :applied :cleanup-pending? true})
      (is (= 2 (count @calls)))
      (act! a :sync)
      ((:ok (last @calls)) {:status :unconfirmed})
      (is (= 3 (count @calls)) "Unknown Applying never triggers cleanup or another integration"))))

(deftest publish-review-has-two-clear-actions-and-dismissal-sends-nothing
  (with-client
    (fn [{:keys [room memory calls ready]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))
            container (.createElement js/document "div") stop (atom nil)]
        (act! a :sync) ((:ok (last @calls)) (assoc ready :operation-kind :push))
        (.appendChild (.-body js/document) container)
        (try
          (reset! stop (browser/mount! container (view/review-push @memory a ((:state a)))))
          (is (= ["×" "Publish changes" "Cancel"]
                 (mapv #(.-textContent %) (array-seq (.querySelectorAll container "button")))))
          (let [intent (:intent ((:state a)))]
            (.click (.querySelector container "[aria-label='Dismiss push review']"))
            (is (= 1 (count @calls)))
            (is (= intent (:intent ((:state a)))))
            (is (false? (:open? ((:state a))))))
          (finally (when @stop (browser/unmount! @stop)) (.remove container)))))))

(deftest uncertain-publish-offers-one-status-action-with-technical-controls-under-details
  (with-client
    (fn [{:keys [room memory calls ready]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))
            container (.createElement js/document "div") stop (atom nil)]
        (act! a :sync) ((:ok (last @calls)) (assoc ready :operation-kind :push))
        (act! a :apply) ((:error (last @calls)) (js/Error. "Lost ACK"))
        ((:error (last @calls)) (js/Error. "Status unavailable"))
        (.appendChild (.-body js/document) container)
        (try
          (reset! stop (browser/mount! container (view/review-push @memory a ((:state a)))))
          (let [buttons (array-seq (.querySelectorAll container "button"))
                check (some #(when (= "Check push status" (.-textContent %)) %) buttons)
                inspect (some #(when (= "Inspect local attempt" (.-textContent %)) %) buttons)]
            (is (some? check))
            (is (nil? (.closest check "details")))
            (is (some? (.closest inspect "details")))
            (is (nil? (some #(when (= "Verify remote head" (.-textContent %)) %) buttons)))
            (.click check)
            (is (= :observe (:operation (last @calls))))
            (is (= 1 (count (filter #(= :apply (:operation %)) @calls)))))
          (finally (when @stop (browser/unmount! @stop)) (.remove container)))))))

(deftest remote-presence-and-an-unchanged-remote-have-distinct-publication-labels
  (with-client
    (fn [{:keys [a room memory]}]
      (let [push (sync/controller {:room-id room :github-direction :push} #(deref memory))]
        (doseq [[outcome label] [[:pushed "Published"] [:upstream-observed "Published · verified"]
                                 [:upstream-unchanged "Remote unchanged · edits kept locally"]]]
          (let [container (.createElement js/document "div") stop (atom nil)]
            (.appendChild (.-body js/document) container)
            (try
              (reset! stop (browser/mount! container (view/toolbar @memory a ((:state a)) push
                                                                   {:reply {:status :closed :outcome outcome}} :push (fn []))))
              (is (.includes (.-textContent container) label))
              (finally (when @stop (browser/unmount! @stop)) (.remove container)))))))))

(deftest lost-publication-ack-is-verified-once-automatically-and-the-next-push-is-independent
  (with-client
    (fn [{:keys [room memory calls ready]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory))]
        (dotimes [_ 2]
          (act! a :sync)
          (let [client (get-in (last @calls) [:request :client-id]) id (str (random-uuid))
                before (count @calls)]
            ((:ok (last @calls)) (assoc ready :operation-kind :push :client-id client :sync-id id))
            (act! a :apply)
            ((:error (last @calls)) (js/Error. "Lost publication acknowledgment"))
            (is (= :observe (:operation (last @calls))))
            ((:ok (last @calls)) {:status :unconfirmed :operation-kind :push :client-id client :sync-id id})
            (is (= :recover (:operation (last @calls))))
            ((:ok (last @calls)) {:status :closed :operation-kind :push :client-id client :sync-id id :outcome :upstream-observed})
            (is (= :closed (get-in ((:state a)) [:reply :status])))
            (is (= (+ before 3) (count @calls)))
            ((:ensure a))
            (is (= (+ before 3) (count @calls)))))
        (is (= 2 (count (filter #(= :apply (:operation %)) @calls))))))))

(deftest reconnect-and-local-inspection-cannot-authorize-remote-push-verification
  (with-client
    (fn [{:keys [room memory calls ready]}]
      (let [a (sync/controller {:room-id room :github-direction :push} #(deref memory)) id (str (random-uuid))]
        (act! a :sync)
        (let [client (get-in (last @calls) [:request :client-id])
              sending {:status :unconfirmed :operation-kind :push :client-id client :sync-id id}]
          ((:ok (last @calls)) (assoc ready :operation-kind :push :client-id client :sync-id id))
          (act! a :apply)
          ((:error (last @calls)) (js/Error. "Lost ACK"))
          (reset! sig/memory-connection {:epoch (random-uuid)})
          ((:ok (last @calls)) sending)
          (is (= 3 (count @calls)) "A response from the previous epoch cannot verify remotely")
          ((:ensure a))
          ((:ok (last @calls)) sending)
          (is (= 4 (count @calls)) "Lifecycle observation stays local")
          (act! a :check)
          ((:ok (last @calls)) sending)
          (is (= 5 (count @calls)) "Details inspection stays local")
          (act! a :check-status)
          ((:ok (last @calls)) sending)
          (is (= :recover (:operation (last @calls))) "An explicit status check grants one new scoped verification"))))))
