(ns is.simm.uis.web.desktop.retained-source-view-test
  (:require [cljs.test :refer-macros [deftest is]]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.connection-epoch :as connection]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.uis.web.desktop.proposals-remote :as remote]
            [is.simm.uis.web.desktop.views.retained-source :as view]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn- with-client [f]
  (binding [ec/*execution-context* runtime]
    (let [proposal (str (random-uuid)) component (str (random-uuid))
          a {:proposal-id proposal :component-id component :path "/in/knowledge/sources/inbox/A.md" :tab-id :a}
          b (assoc a :path "/in/knowledge/sources/inbox/B.md" :tab-id :b)
          tabs (signal runtime [{:id :left :tabs [{:id :a :type :proposal-source :data (dissoc a :tab-id)}
                                                 {:id :b :type :proposal-source :data (dissoc b :tab-id)}]}])
          socket (signal runtime {:socket (random-uuid)}) session {:id (random-uuid)}
          calls (atom []) opened (atom [])
          deferred (fn [kind] (fn [& args] (fn [ok error] (swap! calls conj {:kind kind :args args :ok ok :error error}))))
          listing {:proposal proposal :component component :request (str (random-uuid))
                   :selection (zipmap [:trigger-id :execution-id :manifest-id] (repeatedly 3 #(str (random-uuid))))
                   :files [{:path (:path a) :source-path "inbox/A.md" :role :trigger :size 4 :content-id component}
                           {:path (:path b) :source-path "inbox/B.md" :role :context :size 1 :content-id proposal}]}]
      (with-redefs [sig/retained-source-state (signal runtime {}) sig/layout-columns tabs
                    sig/user-rooms (signal runtime {:rooms []}) sync/state (signal runtime {})
                    connection/connection socket web/current-session (constantly session)
                    web/current-session? #(= (:id %) (:id session))
                    remote/list-proposal-sources! (deferred :list) remote/read-proposal-source! (deferred :chunk)
                    refs/open! (fn ([ref] (swap! opened conj [ref {}]))
                                 ([ref options] (swap! opened conj [ref options])))]
        (f {:a (view/controller a) :b (view/controller b) :calls calls :listing listing :tabs tabs :socket socket
            :opened opened :last-call #(last @calls)
            :chunk (fn [entry offset bytes next-offset]
                     {:status :ok :data (merge (select-keys listing [:proposal :component :request :selection])
                                              (select-keys entry [:path :size :content-id])
                                              {:offset offset :length (count bytes) :encoding "base64"
                                               :base64 (js/btoa (apply str (map js/String.fromCharCode bytes)))}
                                              (when next-offset {:next-offset next-offset}))})})))))

(deftest two-source-tabs-assemble-their-own-exact-byte-streams
  (with-client
   (fn [{:keys [a b calls listing last-call chunk]}]
     ((:ensure a)) ((:ensure a))
     (let [a-list (:ok (last-call))]
       ((:ensure b))
       (is (= 2 (count @calls)))
       ((:ok (last-call)) {:status :ok :data listing})
       (a-list {:status :ok :data listing}))
     ((:ensure a))
     ((:ok (last-call)) (chunk (first (:files listing)) 0 [240 159] 2))
     ((:ensure a))
     ((:ok (last-call)) (chunk (first (:files listing)) 2 [166 139] nil))
     (is (= "🦋" (:text ((:state a)))))
     ((:ensure b))
     ((:ok (last-call)) (chunk (second (:files listing)) 0 [66] nil))
     (is (= "B" (:text ((:state b)))))
     (is (= "🦋" (:text ((:state a)))))
     ((:ensure a)) ((:ensure b))
     (is (= 5 (count @calls))))))

(deftest retired-tabs-and-reconnects-do-not-publish-old-observations
  (with-client
   (fn [{:keys [a calls listing last-call tabs socket opened]}]
     ((:ensure a))
     (let [old (:ok (last-call))]
       (reset! socket {:socket (random-uuid)})
       ((:ensure a))
       (is (= 2 (count @calls)))
       (old {:status :ok :data listing})
       (is (= :listing (:status ((:state a)))))
       ((:ok (last-call)) {:status :ok :data listing}))
     (let [tab (first (:tabs (first @tabs)))]
       (reset! tabs [{:id :right :tabs [tab]}])
       ((:open-proposal a))
       (is (= :right (get-in (first @opened) [1 :col-id])))
       ((:ensure a))
       (let [old (:ok (last-call)) before (count @calls)]
         (reset! tabs [])
         (old {:status :unavailable})
         ((:ensure a)) ((:open-proposal a))
         (is (nil? ((:state a))))
         (is (= before (count @calls)))
         (is (= 1 (count @opened))))))))

(deftest actual-tab-closing-releases-only-its-read-cache
  (with-client
   (fn [{:keys [a b calls listing last-call tabs]}]
     ((:ensure a))
     ((:ok (last-call)) {:status :ok :data listing})
     ((:ensure a))
     (let [old (:ok (last-call))
           tab-a (first (:tabs (first @tabs)))
           tab-b (second (:tabs (first @tabs)))]
       ((:ensure b))
       (reset! tabs [{:id :right :tabs [tab-a tab-b] :active-tab :a}])
       (with-redefs [sig/notify-focus-change! (fn [& _]) sig/refresh-active-nav-keys! (fn [])
                     sig/ensure-layout! (fn [])]
         (sig/close-tab! :right :a)
         (is (= 1 (count @sig/retained-source-state)))
         (is (= :listing (:status ((:state b)))))
         (old {:status :unavailable})
         (is (= 1 (count @sig/retained-source-state)) "Late response cannot recreate a closed cache")
         (sig/close-tab-of-type! :proposal-source)
         (is (empty? @sig/retained-source-state)))))))

(deftest closing-a-source-in-a-fork-preserves-the-parent-tab-and-cache
  (with-client
   (fn [{:keys [a listing last-call]}]
     ((:ensure a))
     ((:ok (last-call)) {:status :ok :data listing})
     (let [before @sig/retained-source-state child (context/fork-context runtime)]
       (try
         (with-redefs [sig/notify-focus-change! (fn [& _]) sig/refresh-active-nav-keys! (fn [])
                       sig/ensure-layout! (fn [])]
           (binding [ec/*execution-context* child]
             (sig/close-tab! :left :a)
             (is (empty? @sig/retained-source-state))))
         (is (= before @sig/retained-source-state))
         (is (= :reading (:status ((:state a)))))
         (finally (context/stop-context! child)))))))
