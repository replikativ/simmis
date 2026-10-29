(ns is.simm.uis.web.desktop.memory-actions-test
  (:require [cljs.test :refer-macros [deftest is] :refer [use-fixtures]]
            [clojure.string :as str]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [org.replikativ.spindel.engine.impl.simple :as simple]
            [org.replikativ.spindel.dom.browser :as browser]
            [org.replikativ.spindel.dom.render :as render]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.memory-actions :as actions]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-owner :as owner]
            [is.simm.uis.web.desktop.memory-metadata-io :as metadata-io]
            [is.simm.uis.web.desktop.memory-cells :as cells]
            [is.simm.uis.web.desktop.memory-saved-views :as page-views]
            [is.simm.uis.web.desktop.memory-saved-views-test :as page-view-fixture]
            [is.simm.uis.web.desktop.saved-view-remote :as saved-view-remote]
            [is.simm.documents.saved-view-contract :as saved-view-contract]
            [is.simm.documents.saved-view-snapshot-test :as snapshot-fixture]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.memory-reference-remote :as reference-remote]
            [is.simm.uis.web.desktop.refs :as refs]
            [is.simm.documents.links :as links]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.connection-epoch :as connection-epoch]
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.uis.web.desktop.views.nav :as nav]
            [is.simm.uis.web.desktop.db-signal :as db-signal]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn- wire [stamp reply]
  (cond-> (assoc reply :target-stamp stamp :protocol-version 2)
    (contains? reply :folders) (update :folders #(merge {"" {:entries []}} %))))

(defn- verified [connection stamp]
  {:connection connection :verified-connection connection :watch-connection connection
   :target-stamp stamp :target-verified? true :target-epoch 0 :read-epoch 0
   :permissions {:write? true :evaluate? true} :sandbox-permissions {:evaluate? true}})

(def ^:dynamic *controllers* nil)

(defn- pump!
  "Run production consumption as rendering would, with no callback subscribers."
  []
  (swap! *controllers* #(into {} (filter (fn [[owner _]] (= (:id (web/current-session)) (first owner)))) %))
  (loop [remaining 20]
    (let [before [@sig/memory-room-metadata
                  (mapv (fn [a] ((:state a))) (vals @*controllers*))]]
      (doseq [a (vals @*controllers*)] ((:consume-metadata a)))
      (when (not= before [@sig/memory-room-metadata
                         (mapv (fn [a] ((:state a))) (vals @*controllers*))])
        (when (zero? remaining) (throw (ex-info "Metadata consumption did not settle" {})))
        (recur (dec remaining))))))

(defn- publish-stream! [a connection stream]
  (reset! sync/state stream)
  ((:ensure-sync a) connection))

(defn- reply! [callback value]
  (callback value)
  (pump!))

(use-fixtures :each
  (fn [test-fn]
    (let [make-controller actions/controller]
      (binding [ec/*execution-context* runtime *controllers* (atom {})]
        (with-redefs [sig/memory-room-metadata (signal runtime {})
                      sig/memory-editor-state (signal runtime {})
                      sig/memory-navigation-state (signal runtime {})
                      sig/layout-columns (signal runtime [])
                      sig/user-rooms (signal runtime nil)
                      sync/state (signal runtime nil)
                      connection-epoch/snapshot #(binding [ec/*execution-context* runtime]
                                                   @sig/memory-connection)
                      actions/controller (fn [data]
                                           (let [a (make-controller data)]
                                             (swap! *controllers* assoc (:owner a) a) a))]
          (test-fn))))))

(deftest resumed-render-actions-use-the-current-stream-without-rewinding-it
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          states (signal runtime {}) calls (atom 0)
          stream {:connection connection :status :pending :dirty {"test-room" 2}}]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    remote/load-memory-metadata! (fn [& _] (fn [_ _] (swap! calls inc)))]
        (let [data {:room-id "test-room" :col-id :test :state-signal states}
              ;; Both renders are created before the stream is available. Their
              ;; actions must consult the live signal when they later resume.
              page (actions/controller data) footer (actions/controller data)
              state #(get @states (:owner page))]
          (swap! states assoc (:owner page) (verified connection stamp))
          (reset! sync/state stream)
          ((:ensure-sync page) connection)
          (is (= [connection :pending 2] (:stream-marker (state))))
          (let [settled @states metadata @sig/memory-room-metadata commits (atom [])]
            (add-watch states ::replayed-render (fn [& _] (swap! commits conj :editor)))
            (add-watch sig/memory-room-metadata ::replayed-render (fn [& _] (swap! commits conj :metadata)))
            (try
              (dotimes [_ 10]
                ((:ensure-sync footer) connection)
                ((:ensure-sync page) connection))
              (is (= settled @states) "Repeated render actions leave the current marker stable")
              (is (= metadata @sig/memory-room-metadata) "No alternating metadata observations")
              (is (empty? @commits) "Stable actions emit no signal commits, even equal-value ones")
              (is (= 1 @calls))
              (finally
                (remove-watch states ::replayed-render)
                (remove-watch sig/memory-room-metadata ::replayed-render))))
          (reset! sync/state (assoc-in stream [:dirty "test-room"] 3))
          ((:ensure-sync footer) connection)
          (is (= [connection :pending 3] (:stream-marker (state)))))))))

(deftest reopening-a-fresh-folder-is-local-and-closed-caches-remain-bounded
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          states (signal runtime {}) calls (atom []) metadata (atom [])]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    remote/load-memory-folder! (fn [_ _ request]
                                                 (swap! calls conj (:folder request))
                                                 (fn [ok _] (ok (wire stamp {:entries []}))))
                    remote/load-memory-metadata! (fn [_ _ request]
                                                   (swap! metadata conj request)
                                                   (fn [ok _] (ok (wire stamp {:folders {} :entries {}}))))]
        (let [a (actions/controller {:room-id "test-room" :col-id :navigation :state-signal states})
              state #(get @states (:owner a))]
          (swap! states assoc (:owner a) (assoc (verified connection stamp) :root-expanded? true
                                               :folders {"" {:entries []}}))
          ((:toggle a) "wiki") ((:toggle a) "wiki") ((:toggle a) "wiki")
          (is (= ["wiki"] @calls))
          (is (= [] (get-in (state) [:folders "wiki" :entries])))
          ((:toggle a) "wiki")
          ((:toggle-root a)) ((:toggle-root a))
          (is (= ["wiki"] @calls) "Neither root collapse nor cached reopen needs a folder read")
          (doseq [i (range 100)]
            (let [path (str "folder-" i)] ((:toggle a) path) ((:toggle a) path)))
          (is (<= (count (:folders (state))) 64))
          ((:discover a))
          (is (= [""] (:folders (last @metadata))))
          (is (nil? (get-in (state) [:errors :metadata]))))))))

(deftest a-late-folder-response-cannot-repopulate-a-cache-after-a-room-change
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          states (signal runtime {}) folder (atom nil) metadata (atom nil)
          stream {:connection connection :status :ready :dirty {"test-room" 1}}]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    remote/load-memory-folder! (fn [& _] (fn [ok _] (reset! folder ok)))
                    remote/load-memory-metadata! (fn [& _] (fn [ok _] (reset! metadata ok)))]
        (let [a (actions/controller {:room-id "test-room" :col-id :navigation :state-signal states})
              state #(get @states (:owner a))]
          (swap! states assoc (:owner a)
                 (assoc (verified connection stamp) :root-expanded? true
                        :stream-marker [connection :ready 0]
                        :folders {"" {:entries []} "closed" {:entries [:old]}}))
          ((:folder a) "slow")
          (publish-stream! a connection stream)
          (is (nil? (get-in (state) [:folders "closed"])))
          (reply! @folder (wire stamp {:entries [:stale]}))
          (is (nil? (get-in (state) [:folders "slow"])))
          (reply! @metadata (wire stamp {:folders {"" {:entries []} "slow" {:entries [:current]}} :entries {}}))
          (is (= [:current] (get-in (state) [:folders "slow" :entries]))))))))

(deftest a-superseded-metadata-snapshot-cannot-refill-a-closed-cache
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          states (signal runtime {}) metadata (atom nil)
          stream {:connection connection :status :ready :dirty {}}]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    remote/load-memory-metadata! (fn [& _] (fn [ok _] (reset! metadata ok)))]
        (let [a (actions/controller {:room-id "test-room" :col-id :navigation :state-signal states})
              state #(get @states (:owner a))]
          (swap! states assoc (:owner a)
                 (assoc (verified connection stamp) :root-expanded? true :expanded {"wiki" true}
                        :folders {"" {:entries [{:path "wiki" :kind :folder}]} "wiki" {:entries []}}))
          (publish-stream! a connection stream)
          (let [before @metadata]
            ((:toggle a) "wiki")
            (publish-stream! a connection (assoc-in stream [:dirty "test-room"] 1))
            (is (nil? (get-in (state) [:folders "wiki"])))
            (before (wire stamp {:folders {"wiki" {:entries [:stale]}} :entries {}}))
            (is (nil? (get-in (state) [:folders "wiki"])))))))))

(deftest reference-resolution-refreshes-a-current-file-and-retains-dirty-drafts
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          states (signal runtime {}) requests (atom []) file-reads (atom [])
          intent {:repository-id (random-uuid) :block-id (random-uuid)} path "wiki/Moved.md"
          old (wire stamp {:path path :kind :text :entry {:content :old} :text "Saved"})
          data {:room-id (str (random-uuid)) :col-id :reference-test :state-signal states :reference intent}]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    reference-remote/resolve-memory-block! (fn [_ _ request]
                                                            (fn [ok _] (swap! requests conj {:request request :ok ok})))
                    remote/load-memory-file! (fn [_ _ request]
                                               (fn [ok _] (swap! file-reads conj {:request request :ok ok})))]
        (let [a (actions/controller data) key (:owner a) state #(get @states key)]
          (swap! states assoc key (assoc (verified connection stamp) :drafts {path (assoc (editor/draft old) :text "Unsaved typing")}))
          ((:ensure-path a)) ((:ensure-path a))
          (is (= 1 (count @requests)))
          (is (= (assoc intent :expected-target-stamp stamp) (:request (first @requests))))
          (reply! (:ok (first @requests)) (wire stamp {:status :resolved :path path :entry {:content :current}}))
          (is (= path (:selected (state))))
          (is (= 1 (count @file-reads)) "Even an already open path receives a current read")
          (reply! (:ok (first @file-reads)) (wire stamp {:path path :kind :text :entry {:content :current} :text "Current source"}))
          (is (= "Unsaved typing" (get-in (state) [:drafts path :text])))
          (is (= "Current source" (get-in (state) [:drafts path :incoming :text])))
          (is (nil? ((:jump a) path)))
          ((:ensure-path a))
          (is (= 1 (count @requests)) "No unbounded retry from rendering"))))))

(deftest passive-refresh-and-reordered-reads-never-navigate-or-resurrect-a-file
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          states (signal runtime {}) replies (atom {})
          defer (fn [slot] (fn [ok _] (swap! replies assoc slot ok)))
          data {:room-id "test-room" :col-id :test :tab-id :file :state-signal states}
          a-file (wire stamp {:path "a.clj" :entry {:content :a} :text "A"})
          b-file (wire stamp {:path "b.clj" :entry {:content :b} :text "B"})]
      (with-redefs [web/current-session (constantly session)
                    sig/memory-connection (signal runtime connection)
                    web/current-session? (constantly true)
                    sync/ensure! (fn [_])
                    remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {})))
                    remote/load-memory-metadata! (fn [& _] (defer :metadata))
                    remote/load-memory-folder! (fn [& _] (defer :folder))
                    remote/load-memory-file! (fn [_ _ request] (defer [:file (:path request)]))]
        (let [a (actions/controller data) key (:owner a) stream {:connection connection :status :ready :dirty {}}
              state #(get @states key)]
          (reset! sig/layout-columns [{:id :test :tabs [{:id :file :type :memory :data {:room-id "test-room"}}]}])
          (swap! states assoc key (assoc (verified connection stamp)
                                         :selected "a.clj" :folders {"" {:entries []}}
                                         :drafts {"a.clj" (editor/draft a-file) "b.clj" (editor/draft b-file)}))
          ((:folder a) "slow")
          (let [old-folder (:folder @replies)]
            (publish-stream! a connection stream)
            (publish-stream! a connection stream)
            (reply! (:metadata @replies) (wire stamp {:folders {"" {:entries [:new]} "slow" {:entries [:current]}}
                                               :entries {"a.clj" (:entry a-file) "b.clj" {:content :new-b}} :revision :v1}))
            (is (= "a.clj" (:selected (state))) "Background B refresh does not select B")
            (old-folder (wire stamp {:entries [:stale]}))
            (is (= [:current] (get-in (state) [:folders "slow" :entries])))
            ((:reload a) "a.clj")
            (let [old-file (get @replies [:file "a.clj"])]
              (publish-stream! a connection (assoc-in stream [:dirty "test-room"] 1))
              (reply! (:metadata @replies) (wire stamp {:folders {"" {:entries []} "slow" {:entries []}}
                                                 :entries {"a.clj" nil "b.clj" (:entry b-file)} :revision :v2}))
              (old-file a-file)
              (is (:remote-deleted? (get-in (state) [:drafts "a.clj"])))
              (is (= "A" (get-in (state) [:drafts "a.clj" :text])))
              (is (= "a.clj" (:selected (state)))))))))))

(deftest invalidation-during-a-read-is-reconciled-on-completion-not-by-a-timer
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          states (signal runtime {}) pending (atom nil) calls (atom 0)]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    sync/ensure! (fn [_])
                    remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {})))
                    remote/load-memory-metadata! (fn [& _] (fn [ok _] (swap! calls inc) (reset! pending ok)))]
        (let [a (actions/controller {:room-id "test-room" :col-id :test :state-signal states})
              stream {:connection connection :status :ready :dirty {}}
              later (assoc-in stream [:dirty "test-room"] 1)]
          (publish-stream! a connection stream)
          (publish-stream! a connection stream)
          (publish-stream! a connection later)
          (is (= 1 @calls))
          (@pending (wire stamp {:folders {} :entries {} :revision :one}))
          (publish-stream! a connection later)
          (is (= 2 @calls))
          (@pending (wire stamp {:folders {} :entries {} :revision :two}))
          (publish-stream! a connection later)
          (is (= 2 @calls)))))))

(deftest intent-ack-and-transport-readiness-catch-up-on-either-side-of-the-worker-hint
  (doseq [ordering [:hint-before-ready :ready-before-hint]]
    (binding [ec/*execution-context* runtime]
      (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
            states (signal runtime {}) pending (atom nil) calls (atom 0) watches (atom 0)]
        (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                      sig/memory-connection (signal runtime connection)
                      sync/ensure! (fn [_])
                      remote/prepare-memory-stream! (fn [& _] (fn [ok _] (swap! watches inc) (ok {:watching? true})))
                      remote/load-memory-metadata! (fn [& _] (fn [ok _] (swap! calls inc) (reset! pending ok)))]
          (let [a (actions/controller {:room-id "test-room" :col-id ordering :state-signal states})
                connecting {:connection connection :status :connecting :dirty {}}
                ready (assoc connecting :status :ready)
                first-event (if (= ordering :hint-before-ready) (assoc-in connecting [:dirty "test-room"] 1) ready)
                settled (assoc-in ready [:dirty "test-room"] 1)]
            (publish-stream! a connection connecting)
            (is (= 1 @calls))
            (publish-stream! a connection first-event)
            (is (= 1 @calls) "Do not race two metadata reads while the first is in flight")
            (@pending (wire stamp {:folders {} :entries {} :revision :before}))
            (publish-stream! a connection first-event)
            (is (= 2 @calls) "The changed stream marker catches up after the old read completes")
            (@pending (wire stamp {:folders {} :entries {} :revision :first-event}))
            (publish-stream! a connection settled)
            (is (= 3 @calls) "Readiness or the later initial hint forces the final catch-up")
            (@pending (wire stamp {:folders {} :entries {} :revision :current}))
            (publish-stream! a connection settled)
            (is (= 3 @calls))
            (is (= 2 @watches) "Initial target discovery intentionally renews the Room interest once")))))))

(deftest initial-file-reply-before-newer-metadata-does-not-strand-an-old-buffer
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          states (signal runtime {}) replies (atom {}) loads (atom 0)]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    sync/ensure! (fn [_])
                    remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {})))
                    remote/load-memory-metadata! (fn [& _] (fn [ok _] (swap! replies assoc :metadata ok)))
                    remote/load-memory-file! (fn [& _] (fn [ok _] (swap! loads inc) (swap! replies assoc :file ok)))]
        (let [a (actions/controller {:room-id "test-room" :col-id :test :state-signal states})
              stream {:connection connection :status :ready :dirty {}}]
          ;; Seed an already discovered v2 target. The body is still initial
          ;; relative to the NEWER metadata request below, not an unbound read.
          (swap! states assoc (:owner a) (verified connection stamp))
          ((:open a) "a.clj")
          (let [initial (:file @replies)]
            (publish-stream! a connection stream)
            (publish-stream! a connection stream)
            (initial (wire stamp {:path "a.clj" :entry {:content :old} :text "old"}))
            (reply! (:metadata @replies) (wire stamp {:folders {} :entries {"a.clj" {:content :new}} :revision :new}))
            (is (= 2 @loads) "The newer metadata must trigger a fresh body read")
            ((:file @replies) (wire stamp {:path "a.clj" :entry {:content :new} :text "new"}))
            (is (= "new" (get-in @states [(:owner a) :drafts "a.clj" :text])))))))))

(deftest a-file-started-after-the-metadata-snapshot-is-not-stranded
  (doseq [body-first? [false true]]
    (binding [ec/*execution-context* runtime]
      (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
            states (signal runtime {}) replies (atom {}) loads (atom [])
            file (wire stamp {:path "uploaded.clj" :entry {:content :uploaded} :text "Uploaded"})]
        (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                      sig/memory-connection (signal runtime connection)
                      sync/ensure! (fn [_])
                      remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {})))
                      remote/load-memory-metadata!
                      (fn [_ _ request]
                        (when-not (:metadata @replies)
                          (is (not (some #{"uploaded.clj"} (:paths request)))))
                        (fn [ok _] (swap! replies assoc :metadata ok)))
                      remote/load-memory-file!
                      (fn [& _] (fn [ok _] (swap! loads conj ok)))]
          (let [a (actions/controller {:room-id "test-room" :col-id :test :state-signal states})
                stream {:connection connection :status :ready :dirty {}}
                state #(get @states (:owner a))]
            ;; Keep the browser-proved ordering exactly: verified target, M
            ;; snapshots no uploaded path, THEN that path's body starts.
            (swap! states assoc (:owner a) (verified connection stamp))
            (publish-stream! a connection stream)
            (publish-stream! a connection stream)
            ;; Like upload completion: this path did not exist when metadata
            ;; captured its request. No selected/deep-link rerender retries it.
            ((:open a) "uploaded.clj")
            ((:select-path a) "other.clj")
            (let [old-body (first @loads)]
              (when body-first? (reply! old-body file))
              (reply! (:metadata @replies) (wire stamp {:folders {} :entries {} :revision :snapshot-before-upload}))
              ;; A changed demand retires that old batch. Consume the new
              ;; complete observation, then check the same body-read race.
              (reply! (:metadata @replies)
                      (wire stamp {:folders {} :entries {"uploaded.clj" (:entry file) "other.clj" nil}
                                   :revision :snapshot-after-upload}))
              (when-not body-first?
                (is (= 2 (count @loads)) "Metadata must replace the invalidated, omitted body request")
                (reply! old-body (assoc file :text "Stale response"))
                (is (nil? (get-in (state) [:drafts "uploaded.clj"])))
                (is (true? (get-in (state) [:loading [:file "uploaded.clj"]]))
                    "The old callback must not finish its replacement request")
                (reply! (last @loads) file))
              (is (= "Uploaded" (get-in (state) [:drafts "uploaded.clj" :text])))
              (is (= "other.clj" (:selected (state))) "Background retries must not navigate")
              (is (false? (get-in (state) [:loading [:file "uploaded.clj"]]))))))))))

(defn- with-client [f]
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          states (signal runtime {}) calls (atom {}) retries (atom [])
          data {:room-id "test-room" :col-id :column :tab-id :one :state-signal states}
          deferred (fn [kind]
                     (fn [_ _ request]
                       (fn [ok error]
                         (swap! calls update kind (fnil conj []) {:request request
                                                                  :ok (if (= kind :metadata)
                                                                        (fn [reply]
                                                                          (ok (-> reply
                                                                                  (update :folders #(merge (zipmap (:folders request) (repeat {:entries []})) %))
                                                                                  (update :entries #(merge (zipmap (:paths request) (repeat nil)) %))))) ok)
                                                                  :error error}))))]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    sig/memory-editor-state states
                    sig/memory-navigation-state (signal runtime {})
                    sig/layout-columns (signal runtime [{:id :column :active-tab :one
                                                         :tabs [{:id :one :type :memory
                                                                 :data {:room-id "test-room" :path "x.clj"}}]}])
                    sync/ensure! (fn [_]) sync/retry! #(swap! retries conj %)
                    remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {:permissions {:evaluate? true}})))
                    remote/load-memory-metadata! (deferred :metadata)
                    remote/load-memory-folder! (deferred :folder) remote/load-memory-file! (deferred :file)
                    remote/search-memory-files! (deferred :search) remote/analyze-memory-text! (deferred :analyze)
                    remote/load-memory-history! (deferred :history) remote/load-memory-history-file! (deferred :history-file)
                    reference-remote/load-memory-backlinks! (deferred :backlinks)
                    remote/download-memory-file! (deferred :download) remote/load-memory-office-part! (deferred :office)
                    remote/save-memory-file! (deferred :save) remote/save-memory-rich-file! (deferred :rich-save)
                    remote/load-memory-properties! (deferred :properties) remote/save-memory-property! (deferred :property-save)
                    remote/evaluate-memory-repl! (deferred :repl)
                    remote/evaluate-memory-cell! (deferred :cell)
                    remote/save-memory-office-part! (deferred :office-save)
                    remote/correct-memory-office-text! (deferred :office-correct) remote/move-memory-file! (deferred :move)]
        (let [a (actions/controller data) state #(get @states (:owner a))
              file (wire stamp {:path "x.clj" :entry {:content :same :size 8 :mode 33188} :text "Original"})]
          (f {:a a :data data :states states :state state :connection connection :stamp stamp :file file
              :calls calls :retries retries
              :seed! #(swap! states assoc (:owner a)
                             (assoc (verified connection stamp) :selected "x.clj" :drafts {"x.clj" (editor/draft file)}))
              :last-call #(last (get @calls %))
              :metadata! (fn [target]
                           ((:refresh a))
                           (reply! (:ok (last (:metadata @calls)))
                            (wire target {:folders {"" {:entries []}} :entries {"x.clj" (:entry file)}
                                          :revision (random-uuid) :permissions {:write? true :evaluate? true}})))}))))))

(defn- with-backlink-client [f]
  (with-client
    (fn [{:keys [data states stamp connection] :as fixture}]
      (let [room "39bb0b7b-938d-46d7-8a96-5e2f1061c5a5"
            path "wiki/Target.md" repository (random-uuid)
            data (assoc data :room-id room)
            a (actions/controller data)
            state #(get @states (:owner a))
            file (wire stamp {:path path :repository-id repository :kind :markdown
                              :entry {:content (random-uuid) :mode 33188 :size 8} :text "# Target"})
            response (fn [results] (wire stamp {:status :ready :complete? true :results results
                                                :repository-id repository :source-basis (random-uuid)
                                                :target {:kind :file :path path}}))]
        (reset! sig/user-rooms {:rooms [{:room/id room :room/name "Knowledge"}]})
        (reset! sig/layout-columns [{:id :column :active-tab :one
                                    :tabs [{:id :one :type :memory :data {:room-id room :path path}}]}])
        (swap! states assoc (:owner a) (assoc (verified connection stamp)
                                             :selected path :read-epoch 0 :definition-basis (random-uuid)
                                             :drafts {path (editor/draft file)}))
        (f (assoc fixture :a a :data data :state state :room room :path path
                          :repository repository :response response))))))

(deftest a-resumed-render-cannot-rewind-the-current-file-address
  (with-client
    (fn [{:keys [data states state seed!]}]
      (seed!)
      (let [old (actions/controller (assoc data :path "x.clj"))]
        ((:ensure-path old))
        ((:open old) "reports/result.md")
        (let [current (actions/controller (assoc data :path "reports/result.md"))]
          ;; An earlier incremental frame can resume after the tab retargets.
          ;; Both frames must observe the one current layout address.
          (dotimes [_ 3]
            ((:ensure-path old))
            (is (= "reports/result.md" (:selected (state))))
            ((:ensure-path current)))
          (is (= "reports/result.md" (:selected (state))))
          (is (= "reports/result.md" (:opened-path (state))))
          (is (= "reports/result.md" (get-in (first @sig/layout-columns) [:tabs 0 :data :path])))
          (let [before @states]
            (swap! sig/layout-columns assoc-in [0 :tabs] [])
            ((:ensure-path old))
            (is (= before @states) "Closed frames cannot reopen their old path")))))))

(deftest reference-resolution-cannot-reopen-a-destination-after-file-navigation
  (with-client
    (fn [{:keys [data state seed! stamp]}]
      (seed!)
      (let [intent {:repository-id (random-uuid) :block-id (random-uuid)}
            data (assoc data :path "x.clj" :reference intent)
            pending (atom nil)]
        (swap! sig/layout-columns assoc-in [0 :tabs 0 :data] data)
        (with-redefs [reference-remote/resolve-memory-block!
                      (fn [& _] (fn [ok _] (reset! pending ok)))]
          (let [old (actions/controller data)]
            ((:ensure-path old))
            (is (some? @pending))
            ((:open old) "reports/result.md")
            (reply! @pending (wire stamp {:status :resolved :path "wiki/Old.md" :entry {:content :old}}))
            ((:ensure-path old))
            (is (= "reports/result.md" (:selected (state))))
            (is (= "reports/result.md" (get-in (first @sig/layout-columns) [:tabs 0 :data :path])))
            (is (nil? (:block-reference (state))))))))))

(deftest backlink-observations-reopen-locally-and-follow-metadata-without-a-refresh-button
  (with-backlink-client
    (fn [{:keys [a data states state calls last-call response]}]
      ((:ensure-backlinks a))
      (is (empty? (:backlinks @calls)))
      ((:backlinks-open a) true) ((:ensure-backlinks a))
      (is (= 1 (count (:backlinks @calls))))
      (reply! (:ok (last-call :backlinks)) (response []))
      (is (= :ready (get-in ((:backlinks a)) [:reply :status])))
      ((:backlinks-open a) false) ((:backlinks-open a) true) ((:ensure-backlinks a))
      (is (= 1 (count (:backlinks @calls))))
      (swap! states assoc-in [(:owner a) :definition-basis] (random-uuid))
      (is (nil? (:reply ((:backlinks a)))))
      ((:ensure-backlinks a))
      (is (= 2 (count (:backlinks @calls))))
      (reply! (:ok (last-call :backlinks)) (response []))
      (is (= :ready (get-in ((:backlinks a)) [:reply :status]))))))

(deftest a-new-read-epoch-can-replace-a-pending-backlink-read-and-reject-its-late-reply
  (with-backlink-client
    (fn [{:keys [a states calls last-call response]}]
      ((:backlinks-open a) true) ((:ensure-backlinks a))
      (let [old (last-call :backlinks)]
        (swap! states update-in [(:owner a) :read-epoch] inc)
        ((:ensure-backlinks a))
        (is (= 2 (count (:backlinks @calls))) "An old loading flag cannot stall the new marker")
        (reply! (:ok old) (response []))
        (is (nil? (:reply ((:backlinks a)))))
        (let [new (response [])]
          (reply! (:ok (last-call :backlinks)) new)
          (is (= (:source-basis new) (get-in ((:backlinks a)) [:reply :source-basis]))))))))

(deftest backlink-navigation-follows-tab-moves-and-rejects-a-retired-tab
  (with-backlink-client
    (fn [{:keys [a room stamp calls last-call response]}]
      ((:backlinks-open a) true) ((:ensure-backlinks a))
      (let [opened (atom []) result {:path "wiki/Source.md" :url (links/route {:kind :memory-file :room room :path "wiki/Source.md"})}]
        (reply! (:ok (last-call :backlinks)) (response [result]))
        (swap! sig/layout-columns assoc-in [0 :id] :moved)
        (with-redefs [refs/open! (fn ([ref] (swap! opened conj [ref {}]))
                                 ([ref opts] (swap! opened conj [ref opts])))]
          ((:open-backlink a) result)
          (is (= :moved (get-in @opened [0 1 :col-id])))
          (is (= "wiki/Source.md" (get-in @opened [0 0 :path])))
          (swap! sig/layout-columns assoc-in [0 :tabs 0 :type] :home)
          ((:open-backlink a) result)
          (is (= 1 (count @opened)))
          (is (nil? (:reply ((:backlinks a)))))
          ((:ensure-backlinks a))
          (is (= 1 (count (:backlinks @calls)))))))))

(defn- memory-tab-layout []
  [{:id :column :active-tab :one
    :tabs [{:id :one :type :memory :data {:room-id "test-room" :path "x.clj"}}
           {:id :keep :type :home :data {}}]}
   {:id :other :active-tab :other-home :tabs [{:id :other-home :type :home :data {}}]}])

(defn- move-memory-tab! [new-column?]
  (if new-column?
    (sig/create-column-with-tab! :column :one :end)
    (sig/move-tab! :column :one :other)))

(deftest moving-a-memory-tab-keeps-its-draft-selection-and-original-save-callback
  (doseq [new-column? [false true]]
    (with-client
      (fn [{:keys [a data states state seed! stamp last-call calls]}]
        (with-redefs [sig/active-column-id (signal runtime :column)
                      sig/active-nav-keys (signal runtime #{})]
          (reset! sig/layout-columns (memory-tab-layout))
          (seed!)
          ((:edit a) "x.clj" editor/changed "Submitted" {:anchor 2 :head 4})
          (let [original (last-call :save) before (get @states (:owner a))]
            (move-memory-tab! new-column?)
            (let [location (owner/current-tab @sig/layout-columns "test-room" :one)
                  moved (actions/controller (assoc data :col-id (:col-id location)))]
              (is (some? location))
              (is (not= :column (:col-id location)))
              (is (= (:owner a) (:owner moved)))
              (is (= before (get @states (:owner moved))))
              (is (= 1 (count @states)))
              ((:edit moved) "x.clj" editor/changed "Later typing" {:anchor 5 :head 8})
              (is (= 1 (count (:save @calls))))
              (reply! (:ok original) (wire stamp {:entry {:content :submitted}}))
              (is (= "Later typing" (get-in (state) [:drafts "x.clj" :text])))
              (is (= {:anchor 5 :head 8} (get-in (state) [:drafts "x.clj" :selection])))
              (is (= "Submitted" (get-in (state) [:drafts "x.clj" :source :text])))
              (is (= 2 (count (:save @calls))))
              (is (= "Later typing" (get-in (last-call :save) [:request :text])))
              (reply! (:ok (last-call :save)) (wire stamp {:entry {:content :later}}))
              (is (not (editor/dirty? (get-in (state) [:drafts "x.clj"])))))))))))

(deftest a-file-move-ack-retargets-the-tabs-current-column
  (doseq [new-column? [false true]]
    (with-client
      (fn [{:keys [a seed! stamp file state last-call]}]
        (with-redefs [sig/active-column-id (signal runtime :column)
                      sig/active-nav-keys (signal runtime #{})]
          (reset! sig/layout-columns (memory-tab-layout))
          (seed!)
          ((:move a) "x.clj" "renamed.clj")
          (move-memory-tab! new-column?)
          (let [location (owner/current-tab @sig/layout-columns "test-room" :one)]
            (reply! (:ok (last-call :move)) (wire stamp {:path "renamed.clj" :entry (:entry file)}))
            (is (= "renamed.clj" (:selected (state))))
            (is (= "renamed.clj" (get-in (owner/current-tab @sig/layout-columns "test-room" :one)
                                          [:tab :data :path])))
            (is (= (:col-id location) (:col-id (owner/current-tab @sig/layout-columns "test-room" :one))))
            (is (= "Original" (get-in (state) [:drafts "renamed.clj" :text])))
            (is (nil? (get-in (state) [:drafts "x.clj"])))))))))

(deftest late-move-acks-do-not-retarget-closed-repurposed-or-ambiguous-tabs
  (doseq [scenario [:closed :other-type :other-room :duplicate]]
    (with-client
      (fn [{:keys [a seed! stamp file state last-call]}]
        (with-redefs [sig/active-column-id (signal runtime :column)
                      sig/active-nav-keys (signal runtime #{})]
          (reset! sig/layout-columns (memory-tab-layout))
          (seed!)
          ((:move a) "x.clj" "renamed.clj")
          (swap! sig/layout-columns
                 (fn [columns]
                   (case scenario
                     :closed (update-in columns [0 :tabs] #(filterv (fn [tab] (not= :one (:id tab))) %))
                     :other-type (assoc-in columns [0 :tabs 0 :type] :chat)
                     :other-room (assoc-in columns [0 :tabs 0 :data :room-id] "another-room")
                     :duplicate (update-in columns [1 :tabs] conj {:id :one :type :home :data {}}))))
          (let [before @sig/layout-columns]
            (reply! (:ok (last-call :move)) (wire stamp {:path "renamed.clj" :entry (:entry file)}))
            (is (= before @sig/layout-columns))
            (is (= "renamed.clj" (:selected (state))) "Retain the old owner's receipt without navigating an unrelated tab")))))))

(deftest a-retargeted-file-view-retains-the-original-pending-cell-receipt-once
  (with-client
    (fn [{:keys [a seed! metadata! last-call state stamp]}]
      (seed!)
      (let [path "wiki/Cells.md" revision (random-uuid) next-stamp (random-uuid)
            request {:path path :revision revision :namespace "user"}
            receipt {:id (random-uuid) :target :human-sci :success true :value "42"
                     :input-syntax :clojure :namespace "user" :requested-namespace "user"
                     :source {:kind :saved-markdown-cell :path path :revision revision :target-stamp stamp}}]
        ;; The pure projection/request contract has its own source mapping tests.
        ;; Here exercise the real controller's retained native callback ownership.
        (with-redefs [cells/request (fn [& _] request)]
          ((:run-cell a) path "cell:0:22"))
        (let [reply (:ok (last-call :cell))]
          (is (fn? reply))
          (metadata! next-stamp)
          (is (= next-stamp (:target-stamp (state))))
          (reply receipt)
          (is (= [receipt] (get-in (state) [:repl :history])))
          (is (= :unverified (cells/receipt-status (state) receipt)))
          (is (false? (get-in (state) [:loading [:cell path revision "cell:0:22"]])))
          (reply receipt)
          (is (= [receipt] (get-in (state) [:repl :history])) "A duplicate callback cannot append twice")
          (is (= next-stamp (:target-stamp (state)))))))))

(deftest moves-retire-old-path-reads-and-drain-current-metadata
  (doseq [metadata-first? [true false] old-exists? [true false]]
    (with-client
      (fn [{:keys [a states state stamp file seed! calls last-call]}]
        (seed!)
        (swap! states update (:owner a) assoc :history-preview {"x.clj" {:text "Old history"}})
        ((:discover a))
        (let [old-metadata (last-call :metadata)]
          ((:reload a) "x.clj")
          ((:history a) "x.clj")
          (reply! (:ok (last-call :history)) (wire stamp {:versions []}))
          (let [metadata (wire stamp {:folders {} :entries {"x.clj" (when old-exists? (:entry file))}
                                      :revision :before-move :permissions {:write? true :evaluate? true}})]
            ((:move a) "x.clj" "moved.clj")
            (when metadata-first? (reply! (:ok old-metadata) metadata))
            (let [old-bodies (filterv #(= "x.clj" (get-in % [:request :path])) (:file @calls))
                  old-histories (:history @calls)]
            (reply! (:ok (last-call :move)) (wire stamp {:path "moved.clj" :entry (:entry file)}))
            (when-not metadata-first? (reply! (:ok old-metadata) metadata))
            (doseq [body old-bodies]
              (reply! (:error body) (ex-info "Old path has been removed" {}))
              (reply! (:ok body) (assoc file :text "Late original body")))
            (doseq [history old-histories]
              (reply! (:ok history) (wire stamp {:versions [{:old-history true}]})))
            (let [current (last (filter #(some #{"moved.clj"} (get-in % [:request :paths])) (:metadata @calls)))]
              (is (some? current) "Superseded discovery drains a current request")
              (is (not (some #{"x.clj"} (get-in current [:request :paths]))))
              (reply! (:ok current) (wire stamp {:folders {} :entries {"moved.clj" (:entry file)}
                                         :revision :after-move :permissions {:write? true :evaluate? true}})))
            (reply! (:ok (last-call :file)) (assoc file :path "moved.clj"))
            (is (= "moved.clj" (:selected (state))))
            (is (= "Original" (get-in (state) [:drafts "moved.clj" :text])))
            (is (nil? (get-in (state) [:drafts "x.clj"])))
            (is (nil? (get-in (state) [:errors [:file "x.clj"]])))
            (is (nil? (get-in (state) [:history "x.clj"])))
            (is (nil? (get-in (state) [:history-preview "x.clj"])))
            (is (nil? (get-in (state) [:observed-entries "x.clj"])))
            (is (= (count old-bodies) (count (filter #(= "x.clj" (get-in % [:request :path])) (:file @calls))))
                "No new read of the retired path after acknowledgement")
            (is (= (count old-histories) (count (:history @calls)))))))))))

(deftest typing-during-move-follows-the-file-and-resumes-autosave
  (doseq [old-entry [nil {:content :old-path-changed}]]
  (with-client
    (fn [{:keys [a state stamp file seed! calls last-call]}]
      (seed!)
      ((:discover a))
      (let [metadata (last-call :metadata)]
      ((:move a) "x.clj" "moved.clj")
      ((:edit a) "x.clj" editor/changed "Typed during move" {:anchor 17 :head 17})
      (is (empty? (:save @calls)) "Do not save the path while its move is in flight")
      (reply! (:ok metadata) (wire stamp {:folders {} :entries {"x.clj" old-entry}
                                  :revision :old-path :permissions {:write? true :evaluate? true}}))
      (reply! (:ok (last-call :move)) (wire stamp {:path "moved.clj" :entry (:entry file)}))
      (is (= "Typed during move" (get-in (state) [:drafts "moved.clj" :text])))
      (is (nil? (get-in (state) [:drafts "x.clj"])))
      (is (nil? (get-in (state) [:autosave "x.clj"])))
      (is (= "moved.clj" (get-in (last-call :save) [:request :path])))
      (is (= "Typed during move" (get-in (last-call :save) [:request :text])))
      (is (not (get-in (state) [:drafts "moved.clj" :remote-deleted?])))
      (is (nil? (get-in (state) [:drafts "moved.clj" :incoming-entry])))
      (reply! (:ok (last-call :save)) (wire stamp {:path "moved.clj" :entry {:content :typed}}))
      (is (not (editor/dirty? (get-in (state) [:drafts "moved.clj"])))))))))

(deftest office-preview-refresh-retains-dirty-xml-and-fences-stale-revisions
  (with-client
    (fn [{:keys [a states state stamp file seed! last-call]}]
      (seed!)
      (let [xml {:part "word/document.xml" :text "Human XML" :saved-text "Saved XML" :selection {:anchor 2 :head 5}}
            next-entry {:content :next}]
        (swap! states assoc-in [(:owner a) :drafts "x.clj" :office] xml)
        ((:office-open a) "x.clj" nil true)
        (let [old (last-call :office)]
          (swap! states assoc-in [(:owner a) :drafts "x.clj" :incoming-entry] next-entry)
          ((:office-open a) "x.clj" nil true)
          (let [current (last-call :office)]
            (reply! (:ok old) (wire stamp {:entry (:entry file) :reading {:status :ready :notice "old"}}))
            (is (true? (get-in (state) [:loading [:office-reading "x.clj"]])))
            (is (nil? (get-in (state) [:drafts "x.clj" :office :reading])))
            (reply! (:ok current) (wire stamp {:entry next-entry :text "Server XML" :part "another.xml"
                                       :reading {:status :ready :notice "current"}}))))
        (is (= xml (select-keys (get-in (state) [:drafts "x.clj" :office]) (keys xml))))
        (is (= (:entry file) (get-in (state) [:drafts "x.clj" :source :entry])))
        (is (= next-entry (get-in (state) [:drafts "x.clj" :office :reading-entry])))
        ((:office-open a) "x.clj" nil true)
        (let [pending (last-call :office)]
          (swap! states assoc-in [(:owner a) :drafts "x.clj" :remote-deleted?] true)
          (reply! (:ok pending) (wire stamp {:entry next-entry :reading {:status :ready :notice "deleted"}})))
        (is (= "current" (get-in (state) [:drafts "x.clj" :office :reading :notice])))
        (is (false? (get-in (state) [:loading [:office-reading "x.clj"]])))))))

(deftest metadata-retires-and-replaces-pending-office-reads
  (with-client
    (fn [{:keys [a state stamp file seed! last-call metadata! calls]}]
      (seed!)
      ((:office-open a) "x.clj" nil)
      (let [old (last-call :office)]
        (metadata! stamp)
        (is (= 2 (count (:office @calls))))
        (reply! (:ok old) (wire stamp {:entry (:entry file) :text "Late XML"}))
        (is (true? (get-in (state) [:loading [:office "x.clj"]])))
        (is (nil? (get-in (state) [:drafts "x.clj" :office])))
        (reply! (:ok (last-call :office)) (wire stamp {:entry (:entry file) :text "Current XML" :reading {:status :ready}}))
        (is (= "Current XML" (get-in (state) [:drafts "x.clj" :office :text])))
        (is (false? (get-in (state) [:loading [:office "x.clj"]])))))))

(deftest office-save-ack-refreshes-preview-without-losing-newer-xml-typing
  (with-client
    (fn [{:keys [a states state stamp seed! last-call]}]
      (seed!)
      (swap! states assoc-in [(:owner a) :drafts "x.clj" :office]
             {:part "word/document.xml" :text "First XML edit" :saved-text "Baseline XML"})
      ((:office-save a) "x.clj")
      ((:change a) "x.clj" update :office assoc :text "Second XML edit" :selection {:anchor 3 :head 7})
      (let [entry {:content :saved-first}]
        (reply! (:ok (last-call :office-save)) (wire stamp {:entry entry :commit :saved}))
        (is (= entry (get-in (last-call :office) [:request :expected])))
        (reply! (:ok (last-call :office)) (wire stamp {:entry entry :text "First XML edit" :reading {:status :ready}}))
        (is (= "Second XML edit" (get-in (state) [:drafts "x.clj" :office :text])))
        (is (= "First XML edit" (get-in (state) [:drafts "x.clj" :office :saved-text])))
        (is (= {:anchor 3 :head 7} (get-in (state) [:drafts "x.clj" :office :selection])))
        (is (= entry (get-in (state) [:drafts "x.clj" :office :reading-entry])))
        (is (editor/dirty? (get-in (state) [:drafts "x.clj"])))))))

(deftest correction-ack-reloads-saved-xml-and-retains-concurrent-correction-typing
  (doseq [newer? [false true]]
    (with-client
      (fn [{:keys [a states state stamp file seed! last-call]}]
        (seed!)
        (let [token (random-uuid) correction {:path "x.clj" :token token :text "Correction" :original "Original" :entry (:entry file)}
              base [:drafts "x.clj"]]
          (swap! states update (:owner a)
                 #(-> % (assoc-in (conj base :office-correction) correction)
                      (assoc-in (conj base :office) {:text "Saved XML" :saved-text "Saved XML"
                                                   :reading-entry (:entry file)
                                                   :reading {:corrections {:tokens {4 token}}}})))
          ((:office-correct a) "x.clj")
          (is (= "Correction" (get-in (last-call :office-correct) [:request :replacement])))
          (when newer? ((:change a) "x.clj" assoc-in [:office-correction :text] "Later typing"))
          (let [entry {:content :corrected}]
            (reply! (:ok (last-call :office-correct)) (wire stamp {:entry entry :commit :corrected}))
            (if newer?
              (do (is (= "Later typing" (get-in (state) (conj base :office-correction :text))))
                  (is (= (:entry file) (get-in (state) (conj base :source :entry))))
                  (is (= entry (get-in (state) (conj base :earlier-save-receipt :entry)))))
              (do (is (nil? (get-in (state) (conj base :office-correction))))
                  (is (nil? (get-in (state) (conj base :office))))))
          (is (some? (last-call :file)) "Actual file reload follows publication")))))))

(deftest office-writes-exclude-each-other-in-both-dispatch-orders
  (doseq [first-write [:correction :xml]]
    (with-client
      (fn [{:keys [a states stamp file seed! calls]}]
        (seed!)
        (let [token (random-uuid) base [(:owner a) :drafts "x.clj"]]
          (swap! states assoc-in (conj base :office-correction)
                 {:path "x.clj" :token token :entry (:entry file) :original "Original" :text "Edited"})
          (swap! states assoc-in (conj base :office)
                 {:text "XML" :saved-text "XML" :reading-entry (:entry file) :part "word/document.xml"
                  :reading {:corrections {:tokens {4 token}}}})
          (if (= :correction first-write)
            (do ((:office-correct a) "x.clj") ((:office-save a) "x.clj")
                (is (= 1 (count (:office-correct @calls))))
                (is (empty? (:office-save @calls))))
            (do ((:office-save a) "x.clj") ((:office-correct a) "x.clj")
                (is (= 1 (count (:office-save @calls))))
                (is (empty? (:office-correct @calls))))))))))

(deftest discovery-and-manual-refresh-do-not-wait-for-live-stream-readiness
  (with-client
    (fn [{:keys [a state connection stamp calls last-call]}]
      ((:open a) "x.clj")
      (is (= 1 (count (:metadata @calls))))
      (is (empty? (:file @calls)) "No unbound body read before discovery")
      (publish-stream! a connection {:connection connection :status :error})
      (reply! (:ok (last-call :metadata)) (wire stamp {:folders {} :entries {"x.clj" {:content :x}} :permissions {:write? true}}))
      (is (not (actions/verified? (state) connection)) "The changed stream retires its old admission")
      (reply! (:ok (last-call :metadata)) (wire stamp {:folders {} :entries {"x.clj" {:content :x}} :permissions {:write? true}}))
      (is (true? (actions/verified? (state) connection)))
      (is (= stamp (get-in (last-call :file) [:request :expected-target-stamp])))
      (reply! (:ok (last-call :file)) (wire stamp {:path "x.clj" :entry {:content :x} :text "Current"}))
      (is (= "Current" (get-in (state) [:drafts "x.clj" :text])))
      ((:refresh a))
      (is (= 3 (count (filter #(= ["x.clj"] (get-in % [:request :paths])) (:metadata @calls))))
          "Initial admission, stream catch-up, and one shared manual refresh")
      (is (zero? (count (filter #(empty? (get-in % [:request :paths])) (:metadata @calls))))
          "The navigation root shares the editor's bounded request")
      (is (= 3 (count (:metadata @calls)))))))

(deftest old-target-replies-and-errors-cannot-change-any-state-or-run-post-effects
  (doseq [kind [:folder :file :search :analyze :history :history-file :download :office
                :save :rich-save :office-save :move :mkdir]]
    (with-client
      (fn [{:keys [a states state stamp file calls seed! last-call metadata!]}]
        (seed!)
        (when (= :rich-save kind)
          (swap! states assoc-in [(:owner a) :drafts "x.clj" :rich-base]
                 {:text "Original" :analysis {:revision :rich :slots []} :models {}}))
        (when (= :office-save kind)
          (swap! states assoc-in [(:owner a) :drafts "x.clj" :office]
                 {:part "word/document.xml" :text "<a/>" :saved-text "<a/>"}))
        (case kind
          :folder ((:folder a) "slow") :file ((:reload a) "x.clj")
          :search (do ((:set-query a) "x") ((:search a)))
          :analyze ((:analyze a) "x.clj") :history ((:history a) "x.clj")
          :history-file ((:history-file a) "x.clj" "commit") :download ((:download a) "x.clj")
          :office ((:office-open a) "x.clj" nil) :save ((:save a) "x.clj")
          :rich-save ((:save a) "x.clj") :office-save ((:office-save a) "x.clj")
          :move ((:move a) "x.clj" "moved.clj") :mkdir ((:mkdir a) "new-dir"))
        (let [request-kind (if (= kind :mkdir) :save kind)
              old (last-call request-kind) target (random-uuid)]
          (is (some? old) (str "The old operation actually started: " kind))
          (is (= stamp (get-in old [:request :expected-target-stamp])) (str kind))
          (metadata! target)
          (let [before (state)]
            (reply! (:ok old) (wire stamp (assoc file :path "moved.clj" :saved? true
                                          :permissions {:write? false :evaluate? false})))
            (is (= before (state)) (str "Old success has no payload/permission/loading/effect authority: " kind))
            (reply! (:error old) (ex-info "Late failure" {}))
            (is (= before (state)) (str "Old failure has no authority: " kind))
            (is (= stamp (get-in (state) [:drafts "x.clj" :source :target-stamp])))
            (is (true? (get-in (state) [:drafts "x.clj" :target-stale?])))))))))

(deftest same-byte-default-replacement-retains-dirty-rich-and-office-models
  (with-client
    (fn [{:keys [a states state stamp file seed! metadata! last-call calls]}]
      (seed!)
      (swap! states update-in [(:owner a) :drafts "x.clj"]
             assoc :text "Human draft" :rich-base {:id :human :models {0 :retained}}
             :office {:text "Human XML" :saved-text "Old XML"})
      (let [target (random-uuid)]
        (metadata! target)
        (reply! (:ok (last-call :file)) (wire target file))
        (is (= "Human draft" (get-in (state) [:drafts "x.clj" :text])))
        (is (= :human (get-in (state) [:drafts "x.clj" :rich-base :id])))
        (is (= "Human XML" (get-in (state) [:drafts "x.clj" :office :text])))
        (is (= stamp (get-in (state) [:drafts "x.clj" :source :target-stamp])))
        (is (= target (get-in (state) [:drafts "x.clj" :incoming :target-stamp])))
        ((:save a) "x.clj")
        (is (empty? (:save @calls)) "Never rebaseline old draft and publish into replacement")))))

(deftest rejected-generation-finishes-only-its-own-bookkeeping
  (doseq [kind [:analyze :save :move]]
    (with-client
      (fn [{:keys [a states state stamp file seed! last-call]}]
        (seed!)
        (case kind :analyze ((:analyze a) "x.clj") :save ((:save a) "x.clj")
              :move ((:move a) "x.clj" "moved.clj"))
        (swap! states update-in [(:owner a) :drafts "x.clj" :generation] inc)
        (reply! (:ok (last-call kind)) (wire stamp (assoc file :path "moved.clj" :permissions {:write? false})))
        (is (false? (get-in (state) [:loading [(if (= kind :analyze) :analysis kind) "x.clj"]])))
        (is (= "x.clj" (:selected (state))))
        (is (nil? (get-in (state) [:drafts "moved.clj"])))
        (is (true? (get-in (state) [:permissions :write?])))))))

(defn- pending-file []
  (let [callbacks (atom {}) promise #js {}]
    (aset promise "then" (fn [f] (swap! callbacks assoc :then f) promise))
    (aset promise "catch" (fn [f] (swap! callbacks assoc :catch f) promise))
    {:callbacks callbacks :file #js {:name "upload.txt" :size 1 :arrayBuffer (fn [] promise)}}))

(deftest upload-captures-origin-before-promise-and-fences-late-success-and-error
  (with-client
    (fn [{:keys [a state seed! metadata! calls]}]
      (seed!)
      (let [{:keys [file callbacks]} (pending-file)]
        ((:upload a) "out" file)
        (metadata! (random-uuid))
        (let [before (state)]
          ((:then @callbacks) (.-buffer (js/Uint8Array. #js [65])))
          ((:catch @callbacks) (js/Error. "Late rejected promise"))
          (is (= before (state)))
          (is (empty? (:save @calls)) "No upload RPC into a replacement target"))))))

(deftest unavailable-discovery-removes-publication-verification-not-human-work-or-repl
  (with-client
    (fn [{:keys [a states state seed! last-call connection]}]
      (seed!)
      (swap! states assoc-in [(:owner a) :repl] {:inputs {:clojure {:text "(+ 1 2)"}} :history [:receipt]})
      ((:change a) "x.clj" assoc :text "Human work")
      ((:discover a))
      (reply! (:error (last-call :metadata)) (ex-info "Default currently unavailable" {}))
      (is (not (actions/verified? (state) connection)))
      (is (nil? (:permissions (state))))
      (is (= "Human work" (get-in (state) [:drafts "x.clj" :text])))
      (is (= [:receipt] (get-in (state) [:repl :history])))
      (is (true? (get-in (state) [:sandbox-permissions :evaluate?]))))))

(deftest upload-rechecks-the-same-origin-after-encoding-before-sending
  (with-client
    (fn [{:keys [a state seed! metadata! calls]}]
      (seed!)
      (let [{:keys [file callbacks]} (pending-file) original js/btoa]
        ((:upload a) "out" file)
        (try
          (set! js/btoa (fn [text] (metadata! (random-uuid)) (original text)))
          ((:then @callbacks) (.-buffer (js/Uint8Array. #js [65])))
          (is (empty? (:save @calls)))
          (is (= "out/upload.txt" (get-in (state) [:cancelled-upload :path])))
          (finally (set! js/btoa original)))))))

(deftest same-column-tabs-have-independent-drafts-and-create-origin-is-present-at-birth
  (with-client
    (fn [{:keys [a data states state stamp seed! calls last-call]}]
      (seed!)
      (swap! sig/layout-columns update-in [0 :tabs] conj {:id :two :type :memory :data {:room-id "test-room"}})
      (let [other (actions/controller (assoc data :tab-id :two))]
        (is (not= (:owner a) (:owner other)))
        ((:create other) "new.md" stamp)
        (reply! (:ok (last-call :metadata)) (wire (random-uuid) {:folders {} :entries {} :permissions {:write? true}}))
        (is (= stamp (get-in ((:state other)) [:drafts "new.md" :source :target-stamp])))
        (is (true? (get-in ((:state other)) [:drafts "new.md" :target-stale?])))
        (is (nil? (get-in (state) [:drafts "new.md"])))
        (is (= "Original" (get-in (state) [:drafts "x.clj" :text]))))
      (let [birth (atom nil)]
        (with-redefs [sig/layout-columns (signal runtime [{:id :column :tabs []}])
                      ;; Match the real variadic ABI: compiled call sites use
                      ;; cljs$core$IFn$_invoke$arity$variadic directly.
                      sig/open-tab! (fn [_ tab-data & _opts] (reset! birth tab-data))]
          (navigation/create-file! "test-room" "Room" "fresh.md" :column stamp)
          (is (= stamp (:create-origin-stamp @birth)))
          (is (uuid? (:create-intent-id @birth))))))))

(deftest fresh-create-origin-is-confirmed-not-rebaselined-by-first-discovery
  (doseq [same? [true false]]
    (with-client
      (fn [{:keys [data states stamp calls last-call]}]
        (let [intent (assoc data :tab-id :fresh :path "fresh.md"
                            :create-origin-stamp stamp :create-intent-id (random-uuid))
              _ (swap! sig/layout-columns update-in [0 :tabs] conj {:id :fresh :type :memory :data intent})
              fresh (actions/controller intent) target (if same? stamp (random-uuid))]
          ((:ensure-path fresh))
          (is (empty? (:file @calls)) "No body read can race ahead of create origin")
          (reply! (:ok (last-call :metadata))
           (wire target {:folders {} :entries {"fresh.md" nil} :permissions {:write? true}}))
          (let [draft (get-in ((:state fresh)) [:drafts "fresh.md"])]
            (is (= stamp (get-in draft [:source :target-stamp])))
            (is (nil? (get-in draft [:source :entry])))
            (is (= (not same?) (boolean (:target-stale? draft))))
            (is (str/includes? (:text draft) "# Untitled")))
          ((:save fresh) "fresh.md")
          (is (= (if same? 1 0) (count (:save @calls)))))))))

(deftest accepted-save-preserves-newer-typing-and-duplicate-replies-run-no-effects
  (with-client
    (fn [{:keys [a state file stamp calls seed! last-call]}]
      (seed!)
      ((:change a) "x.clj" assoc :text "Submitted")
      ((:save a) "x.clj")
      (let [save (last-call :save)]
        ((:change a) "x.clj" assoc :text "Newer human typing")
        (reply! (:ok save) (wire stamp (assoc file :entry {:content :submitted} :saved? true)))
        (is (= "Newer human typing" (get-in (state) [:drafts "x.clj" :text])))
        (is (= "Submitted" (get-in (state) [:drafts "x.clj" :source :text])))
        (is (editor/dirty? (get-in (state) [:drafts "x.clj"])))
        (let [before (state) before-calls @calls]
          (reply! (:ok save) (wire stamp (assoc file :entry {:content :duplicate} :saved? true)))
          (is (= before (state)))
          (is (= before-calls @calls)))))))

(deftest target-change-error-discovers-once-and-never-retries-a-write
  (with-client
    (fn [{:keys [a state seed! calls last-call]}]
      (seed!)
      ((:save a) "x.clj")
      (reply! (:error (last-call :save))
       (ex-info "Changed" {:type :memory/target-changed
                           :data (pr-str {:error "Workspace changed" :correlation-id (str (random-uuid))})}))
      (is (false? (:target-verified? (state))))
      (is (= 1 (count (:metadata @calls))))
      (is (= 1 (count (:save @calls)))))))

(deftest forced-discovery-during-pending-metadata-queues-one-current-read
  (doseq [failed? [false true]]
    (with-client
      (fn [{:keys [a state stamp calls last-call]}]
        ((:discover a))
        (let [old (last-call :metadata)]
          ((:create a) "late.md" stamp)
          (is (= 1 (count (:metadata @calls))) "At most one request physically pending")
          (if failed?
            (reply! (:error old) (ex-info "Obsolete metadata failure" {}))
            (reply! (:ok old) (wire stamp {:folders {} :entries {} :permissions {:write? true}})))
          (is (= 2 (count (:metadata @calls))) "Finish old token then issue freshest discovery")
          (is (not (:target-verified? (state))) "Superseded response cannot verify the target")
          (is (nil? (get-in (state) [:errors :metadata])) "Superseded errors are not current diagnostics")
          (is (true? (get-in (state) [:loading :metadata])))
          (let [before (state)]
            (reply! (:ok old) (wire stamp {:folders {} :entries {}}))
            (reply! (:error old) (ex-info "Duplicate obsolete failure" {}))
            (is (= before (state)))
            (is (= 2 (count (:metadata @calls)))))
          (reply! (:ok (last-call :metadata))
           (wire stamp {:folders {} :entries {"late.md" nil} :permissions {:write? true}}))
          (is (true? (:target-verified? (state))))
          (is (= stamp (get-in (state) [:drafts "late.md" :source :target-stamp]))))))))

(deftest metadata-own-target-change-failure-is-visible-and-does-not-loop
  (with-client
    (fn [{:keys [a state calls last-call]}]
      ((:discover a))
      (reply! (:error (last-call :metadata))
       (ex-info "Changed" {:type :memory/target-changed :data (pr-str {:error "Changed during discovery"})}))
      (is (= 1 (count (:metadata @calls))))
      (is (false? (get-in (state) [:loading :metadata])))
      (is (string? (get-in (state) [:errors :metadata])))
      ((:discover a))
      (is (= 1 (count (:metadata @calls))) "Routine renders do not retry this error"))))

(deftest same-target-absent-metadata-retires-covered-pending-reads
  (with-client
    (fn [{:keys [a state stamp seed! last-call]}]
      (seed!)
      ((:reload a) "x.clj")
      (let [old (last-call :file)]
        ((:discover a))
        (reply! (:ok (last-call :metadata)) (wire stamp {:folders {} :entries {"x.clj" nil}}))
        (is (false? (get-in (state) [:loading [:file "x.clj"]])))
        (let [before (state)]
          (reply! (:ok old) (wire stamp {:path "x.clj" :entry {:content :old} :text "Stale"}))
          (is (= before (state))))
        (is (true? (get-in (state) [:drafts "x.clj" :remote-deleted?])))))))

(deftest initially-missing-deep-link-has-a-visible-result-not-permanent-pending-intent
  (with-client
    (fn [{:keys [a state stamp calls last-call]}]
      ((:open a) "missing.md")
      (reply! (:ok (last-call :metadata)) (wire stamp {:folders {} :entries {"missing.md" nil}}))
      (is (nil? (get-in (state) [:file-intents "missing.md"])))
      (is (string? (get-in (state) [:errors [:file "missing.md"]])))
      (is (empty? (:file @calls)))
      (is (nil? (get-in (state) [:drafts "missing.md"])) "Missing metadata is not an invented body"))))

(deftest newer-upload-intent-survives-an-earlier-rpc-success-or-failure
  (doseq [same-path? [true false] failed? [true false]]
    (with-client
      (fn [{:keys [a state stamp seed! calls last-call]}]
        (seed!)
        (let [first (pending-file) second (pending-file)]
          (when-not same-path? (aset (:file second) "name" "second.txt"))
          ((:upload a) "out" (:file first))
          ((:then @(:callbacks first)) (.-buffer (js/Uint8Array. #js [65])))
          (let [old (last-call :save)]
            ((:upload a) "out" (:file second))
            (let [intent (:upload-intent (state)) token (:upload-token (state))]
              (if failed?
                (reply! (:error old) (ex-info "First upload failed" {}))
                (reply! (:ok old) (wire stamp {:path "out/upload.txt" :entry {:content :first} :saved? true})))
              (is (= intent (:upload-intent (state))))
              (is (= token (:upload-token (state))))
              (is (nil? (get-in (state) [:errors [:upload "out/upload.txt"]]))
                  "An old RPC failure is not the newer promise's diagnostic")
              (is (= "x.clj" (:selected (state))) "Old ACK cannot open/navigate its file")
              ((:then @(:callbacks second)) (.-buffer (js/Uint8Array. #js [66])))
              (is (= 2 (count (:save @calls))) "New promise still owns its publication intent")
              (reply! (:error (last-call :save)) (ex-info "Second upload failed" {}))
              (is (nil? (:upload-token (state))))
              (is (nil? (:upload-intent (state)))))))))))

(deftest unknown-room-permission-can-be-checked-explicitly-without-memory-discovery
  (with-client
    (fn [{:keys [a states state calls]}]
      ((:repl-change a) assoc-in [:inputs :clojure :text] "(+ 20 22)")
      ((:repl-evaluate a) :clojure)
      (is (= 1 (count (:repl @calls))))
      (is (empty? (:metadata @calls)) "Generic sandbox does not hydrate a file target")
      (is (not (:target-verified? (state))))
      (swap! states assoc-in [(:owner a) :loading :repl] false)
      (swap! states assoc-in [(:owner a) :sandbox-permissions :evaluate?] false)
      ((:repl-evaluate a) :clojure)
      (is (= 1 (count (:repl @calls))) "Known denial stays disabled; server checks actual attempts"))))

(deftest markdown-page-properties-load-without-a-click-and-do-not-replace-block-selection
  (with-client
    (fn [{:keys [a states state stamp file seed! calls last-call]}]
      (seed!)
      (swap! states assoc-in [(:owner a) :drafts "x.clj" :source :kind] :markdown)
      ((:ensure-properties a))
      (is (= [{:kind :page}] (mapv #(get-in % [:request :target]) (:properties @calls))))
      ((:ensure-properties a))
      (is (= 1 (count (:properties @calls))) "Loading renders do not duplicate reads")
      (reply! (:ok (last-call :properties)) (wire stamp {:path "x.clj" :target {:kind :page}
                                                :status :unavailable :reason :unprovisioned-canonical-space}))
      ((:ensure-properties a))
      (is (= 1 (count (:properties @calls))) "Unavailable pages do not loop")
      (let [target {:kind :block :id (random-uuid)}
            page-model (get-in (state) [:property-models ["x.clj" {:kind :page}]])]
        ((:open-properties a) "x.clj" target)
        ((:ensure-properties a))
        (is (= 2 (count (:properties @calls))))
        (is (= target (get-in (state) [:property-targets "x.clj"])))
        (is (= page-model (get-in (state) [:property-models ["x.clj" {:kind :page}]])))
        (reply! (:ok (last-call :properties)) (wire stamp {:path "x.clj" :target target :expected (:entry file)}))
        ((:ensure-properties a))
        (is (= 2 (count (:properties @calls))))
        (is (= target (get-in (state) [:property-targets "x.clj"])))))))

(deftest property-controls-send-exact-text-and-retain-newer-input-after-publication
  (with-client
    (fn [{:keys [a states state stamp file seed! last-call calls]}]
      (seed!)
      (let [target {:kind :page} scope ["x.clj" target] id (random-uuid)
            field {:id id :alias "revenue" :editable? true
                   :value 9007199254740992 :input {:text "9007199254740993"}}
            model (wire stamp {:path "x.clj" :target target :expected (:entry file)
                               :revision (random-uuid) :scope-revision (random-uuid) :fields [field]})]
        ((:open-properties a) "x.clj" target)
        (reply! (:ok (last-call :properties)) model)
        ((:property-input a) "x.clj" target field (:input field))
        ((:property-save a) "x.clj" target field)
        (is (= {:text "9007199254740993"} (get-in (last-call :property-save) [:request :input])))
        (is (nil? (get-in (last-call :property-save) [:request :value])))
        ((:property-input a) "x.clj" target field {:text "9007199254740995"})
        (swap! states update-in [(:owner a) :drafts "x.clj"] editor/changed "Typing while saving" {:anchor 3 :head 3})
        (let [reply (wire stamp {:entry {:content :new} :text "Property published" :analysis {}})
              saved (last-call :property-save)]
          (reply! (:ok saved) reply)
          (is (= "Typing while saving" (get-in (state) [:drafts "x.clj" :text])))
          (is (= (:entry file) (get-in (state) [:drafts "x.clj" :source :entry])))
          (is (= "Property published" (get-in (state) [:drafts "x.clj" :incoming :text])))
          (is (= {:text "9007199254740995"} (get-in (state) [:property-drafts scope id :input])))
          (let [before (state) requests (count (:metadata @calls))]
            (reply! (:ok saved) reply)
            (is (= before (state)))
            (is (= requests (count (:metadata @calls))))))))))

(deftest definition-observation-refreshes-model-without-rebasing-entered-property-drafts
  (with-client
    (fn [{:keys [a states state stamp file seed! last-call metadata!]}]
      (seed!)
      (let [target {:kind :page} scope ["x.clj" target] id (random-uuid)
            field {:id id :alias "revenue" :editable? true :input {:text "0"}}
            model (wire stamp {:path "x.clj" :target target :expected (:entry file)
                               :revision (random-uuid) :scope-revision (random-uuid) :fields [field]})]
        ((:open-properties a) "x.clj" target)
        (reply! (:ok (last-call :properties)) model)
        ((:property-input a) "x.clj" target field {:text "42"})
        (let [original (get-in (state) [:property-drafts scope id])]
          (metadata! stamp)
          ((:ensure-properties a))
          (reply! (:ok (last-call :properties)) (assoc model :scope-revision (random-uuid)))
          (is (= original (get-in (state) [:property-drafts scope id])))
          (is (not (editor/property-draft-current? (get-in (state) [:property-models scope]) original)))
          ((:property-input a) "x.clj" target field {:text "43"})
          (is (= (:baseline original) (get-in (state) [:property-drafts scope id :baseline])))
          ((:property-save a) "x.clj" target field)
          (is (nil? (last-call :property-save)))
          ((:property-discard a) "x.clj" target id)
          (is (nil? (get-in (state) [:property-drafts scope id]))))))))

(deftest moves-and-property-saves-exclude-each-other-and-late-acks-cannot-resurrect-drafts
  (doseq [move-first? [true false]]
    (with-client
      (fn [{:keys [a states state stamp file seed! last-call calls]}]
        (seed!)
        (let [target {:kind :page} field {:id (random-uuid) :alias "revenue" :editable? true :input {:text "0"}}
              model (wire stamp {:path "x.clj" :target target :expected (:entry file)
                                 :revision (random-uuid) :scope-revision (random-uuid) :fields [field]})]
          ((:open-properties a) "x.clj" target)
          (reply! (:ok (last-call :properties)) model)
          (when-not move-first? ((:property-input a) "x.clj" target field {:text "42"}))
          (if move-first?
            (do ((:move a) "x.clj" "moved.clj")
                ((:property-save a) "x.clj" target field)
                (is (= 1 (count (:move @calls))))
                (is (empty? (:property-save @calls))))
            (do ((:property-save a) "x.clj" target field)
                ((:move a) "x.clj" "moved.clj")
                (is (= 1 (count (:property-save @calls))))
                (is (empty? (:move @calls)))
                (swap! states update-in [(:owner a) :drafts] dissoc "x.clj")
                (reply! (:ok (last-call :property-save)) (wire stamp {:entry {:content :new} :text "Property saved" :analysis {}}))
                (is (not (contains? (:drafts (state)) "x.clj"))))))))))

(deftest unapplied-property-drafts-refuse-move-instead-of-becoming-hidden
  (with-client
    (fn [{:keys [a state seed! last-call stamp file calls]}]
      (seed!)
      (let [target {:kind :page} field {:id (random-uuid) :alias "revenue" :editable? true :input {:text "0"}}
            model (wire stamp {:path "x.clj" :target target :expected (:entry file)
                               :revision (random-uuid) :scope-revision (random-uuid) :fields [field]})]
        ((:open-properties a) "x.clj" target)
        (reply! (:ok (last-call :properties)) model)
        ((:property-input a) "x.clj" target field {:text "42"})
        ((:move a) "x.clj" "moved.clj")
        (is (empty? (:move @calls)))
        (is (= "x.clj" (:selected (state))))
        (is (editor/property-drafts? (state) "x.clj"))
        ((:property-discard a) "x.clj" target (:id field))
        ((:move a) "x.clj" "moved.clj")
        (is (= 1 (count (:move @calls))))))))

(deftest boundary-notice-survives-selection-and-stale-events-until-content-changes
  (with-client
    (fn [{:keys [a state seed! states file]}]
      (seed!)
      (let [paragraph {:type "paragraph" :content [{:type "text" :text "Alpha"}]}
            analysis {:revision :baseline :slots [{:from 0 :to 5 :model {:type "doc" :content [paragraph]}}]
                      :semantic {:status :ready :blocks [{:identity-status :provisional :syntax [{:from 0 :to 5}]}]}}
            d (editor/draft (assoc file :kind :markdown :text "Alpha" :analysis analysis))
            base (:rich-base d) model (get-in base [:document :model])]
        (swap! states assoc-in [(:owner a) :drafts "x.clj"] d)
        ((:document-blocked a) "x.clj")
        (is (string? (get-in (state) [:errors [:rich "x.clj"]])))
        ((:document-change a) "x.clj" (:id base) model {:from 2 :to 2})
        (is (string? (get-in (state) [:errors [:rich "x.clj"]])))
        ((:document-change a) "x.clj" :stale model nil)
        (is (string? (get-in (state) [:errors [:rich "x.clj"]])))
        ((:document-change a) "x.clj" (:id base)
                              (assoc-in model [:content 0 :content 0 :content 0 :text] "Changed") {:from 3 :to 3})
        (is (nil? (get-in (state) [:errors [:rich "x.clj"]])))
        (is (= "Changed" (get-in (state) [:drafts "x.clj" :text])))))))

(deftest autosave-serializes-and-coalesces-only-new-text-edits
  (with-client
    (fn [{:keys [a state seed! calls last-call stamp file]}]
      (seed!)
      ((:change a) "x.clj" editor/changed "Existing draft" nil)
      (is (empty? (:save @calls)) "Restored/pre-existing drafts are not enrolled")
      ((:edit a) "x.clj" editor/changed "Existing draft" {:anchor 1 :head 1})
      (is (empty? (:save @calls)) "Selection changes are not edits")
      ((:edit a) "x.clj" editor/changed "First" nil)
      (let [first-save (last-call :save) entry {:content :first}]
        ((:edit a) "x.clj" editor/changed "Second" nil)
        ((:edit a) "x.clj" editor/changed "Latest" nil)
        (is (= 1 (count (:save @calls))))
        (reply! (:ok first-save) (wire stamp {:entry entry}))
        (is (= "First" (get-in (state) [:drafts "x.clj" :source :text])))
        (is (= "Latest" (get-in (state) [:drafts "x.clj" :text])))
        (is (= 2 (count (:save @calls))))
        (is (= entry (get-in (last-call :save) [:request :expected])))
        (is (= "Latest" (get-in (last-call :save) [:request :text])))
        (reply! (:ok first-save) (wire stamp {:entry (:entry file)}))
        (is (= 2 (count (:save @calls))) "Duplicate ACK does not dispatch")
        (reply! (:ok (last-call :save)) (wire stamp {:entry {:content :latest}}))
        (is (not (editor/dirty? (get-in (state) [:drafts "x.clj"]))))))))

(deftest simulated-ui-edits-never-automatically-publish-through-the-live-host
  (with-client
    (fn [{:keys [data seed! state calls]}]
      (seed!)
      (let [child (context/fork-context runtime)]
        (try
          (binding [ec/*execution-context* child]
            (let [simulated (actions/controller data)]
              ((:edit simulated) "x.clj" editor/changed "Simulated text" nil)
              (is (= "Simulated text" (get-in ((:state simulated)) [:drafts "x.clj" :text])))
              (is (empty? (:save @calls)))))
          (is (= "Original" (get-in (state) [:drafts "x.clj" :text])))
          (finally (context/close-context! child)))))))

(deftest same-home-context-rebinding-still-automatically-saves
  (with-client
    (fn [{:keys [data seed! calls]}]
      (seed!)
      (binding [ec/*execution-context* (assoc runtime :bindings (assoc (:bindings runtime) :autosave-probe true))]
        (let [live (actions/controller data)]
          ((:edit live) "x.clj" editor/changed "Same home" nil)
          (is (= 1 (count (:save @calls)))))))))

(deftest a-parent-save-callback-does-not-drain-a-child-forks-edits
  (with-client
    (fn [{:keys [data a seed! state last-call calls stamp]}]
      (seed!)
      ((:edit a) "x.clj" editor/changed "Parent submission" nil)
      (let [pending (last-call :save) child (context/fork-context runtime)]
        (try
          (binding [ec/*execution-context* child]
            (let [simulated (actions/controller data)]
              ((:edit simulated) "x.clj" editor/changed "Child-only edit" nil)
              (reply! (:ok pending) (wire stamp {:entry {:content :parent-submission}}))
              (is (= "Child-only edit" (get-in ((:state simulated)) [:drafts "x.clj" :text])))))
          (is (= 1 (count (:save @calls))))
          (is (= "Parent submission" (get-in (state) [:drafts "x.clj" :source :text])))
          (finally (context/close-context! child)))))))

(deftest local-source-validation-in-a-fork-cannot-indirectly-publish
  (with-client
    (fn [{:keys [data states a seed! state last-call calls file stamp]}]
      (seed!)
      (let [child (context/fork-context runtime)
            analysis (fn [text] {:revision :synthetic
                                 :semantic {:status :ready :blocks [{:identity-status :provisional
                                                                    :syntax [{:from 0 :to (count text)}]}]}
                                 :slots [{:from 0 :to (count text)
                                          :model {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text}]}]}}]})]
        (try
          (binding [ec/*execution-context* child]
            (let [simulated (actions/controller data)
                  d (editor/draft (assoc file :kind :markdown :text "Alpha" :analysis (analysis "Alpha")))
                  region (first (keys (get-in d [:rich-base :document :regions])))]
              (swap! states assoc-in [(:owner a) :drafts "x.clj"] d)
              ((:local-source-open simulated) "x.clj" region)
              ((:local-source-change simulated) "x.clj" "Simulated source" nil)
              (reply! (:ok (last-call :analyze)) (wire stamp (analysis "Simulated source")))
              (is (= "Simulated source" (get-in ((:state simulated)) [:drafts "x.clj" :text])))
              (is (empty? (:rich-save @calls)))
              (is (empty? (:save @calls)))))
          (is (= "Original" (get-in (state) [:drafts "x.clj" :text])))
          (finally (context/close-context! child)))))))

(deftest autosave-reconciles-only-exact-own-write-observations
  (doseq [observation [:own-entry :own-body :other-entry :deleted]]
    (with-client
      (fn [{:keys [a state seed! calls last-call stamp states]}]
        (seed!)
        ((:edit a) "x.clj" editor/changed "First" nil)
        (let [pending (last-call :save) own {:content :first}
              observed (case observation :other-entry {:content :other} :deleted nil own)]
          ((:edit a) "x.clj" editor/changed "Later" nil)
          (swap! states update-in [(:owner a) :drafts "x.clj"] editor/observed-entry observed stamp)
          (when (= observation :own-body)
            (swap! states update-in [(:owner a) :drafts "x.clj"] editor/loaded
                   (wire stamp {:entry own :text "First"})))
          (reply! (:ok pending) (wire stamp {:entry own}))
          (if (#{:own-entry :own-body} observation)
            (do (is (= 2 (count (:save @calls))))
                (is (nil? (get-in (state) [:drafts "x.clj" :incoming-entry]))))
            (do (is (= 1 (count (:save @calls))))
                (is (or (get-in (state) [:drafts "x.clj" :incoming-entry])
                        (get-in (state) [:drafts "x.clj" :remote-deleted?]))))))))))

(deftest autosave-failure-is-sticky-across-typing-and-refresh
  (with-client
    (fn [{:keys [a state seed! calls last-call stamp]}]
      (seed!)
      ((:edit a) "x.clj" editor/changed "First" nil)
      (reply! (:error (last-call :save)) (ex-info "Connection lost after possible commit" {}))
      ((:edit a) "x.clj" editor/changed "Later" nil)
      (is (= 1 (count (:save @calls))))
      ((:refresh a))
      (reply! (:ok (last-call :metadata))
       (wire stamp {:folders {} :entries {} :permissions {:write? true}}))
      ((:edit a) "x.clj" editor/changed "Even later" nil)
      (is (= 1 (count (:save @calls))))
      (is (true? (get-in (state) [:autosave-holds "x.clj"]))))))

(deftest autosave-drain-rechecks-revert-permission-and-target
  (doseq [stop [:revert :permission :target :staging]]
    (with-client
      (fn [{:keys [a seed! state states calls last-call stamp]}]
        (seed!)
        ((:edit a) "x.clj" editor/changed "First" nil)
        (let [pending (last-call :save)]
          ((:edit a) "x.clj" editor/changed "Later" nil)
          (case stop
            :revert ((:edit a) "x.clj" editor/changed "First" nil)
            :permission (swap! states assoc-in [(:owner a) :permissions :write?] false)
            :target (swap! states assoc-in [(:owner a) :drafts "x.clj" :target-stale?] true)
            :staging (swap! states assoc-in [(:owner a) :drafts "x.clj" :local-source] {:text "Incomplete"}))
          (reply! (:ok pending) (wire stamp {:entry {:content :first}}))
          (is (= 1 (count (:save @calls))) (str stop))
          (is (= (if (= stop :revert) "First" "Later") (get-in (state) [:drafts "x.clj" :text]))))))))

(deftest uncertain-autosave-survives-reconnection-and-stale-acknowledgement
  (with-client
    (fn [{:keys [a seed! state states calls last-call connection stamp file]}]
      (seed!)
      ((:edit a) "x.clj" editor/changed "Submitted" nil)
      (let [old (last-call :save)]
        ;; The next connection event retires request tokens, not the durable
        ;; context-owned hold. A late old callback cannot release it.
        (swap! states assoc-in [(:owner a) :connection] :retired-connection)
        (publish-stream! a connection {:connection connection :status :ready :dirty {}})
        (reply! (:ok old) (wire stamp {:entry {:content :possibly-committed}}))
        (reply! (:ok (last-call :metadata))
         (wire stamp {:folders {} :entries {"x.clj" (:entry file)} :permissions {:write? true}}))
        ((:edit a) "x.clj" editor/changed "Still paused" nil)
        (is (= 1 (count (:save @calls))))
        (is (true? (get-in (state) [:autosave-holds "x.clj"])))
        (is (= "Original" (get-in (state) [:drafts "x.clj" :source :text])))))))

(deftest local-source-validates-and-saves-without-apply-and-retains-newer-input
  (with-client
    (fn [{:keys [a state seed! states file last-call calls]}]
      (seed!)
      (let [analysis (fn [text]
                       {:revision (random-uuid) :slots [{:from 0 :to (count text)
                                                        :model {:type "doc" :content [{:type "paragraph" :content [{:type "text" :text text}]}]}}]
                        :semantic {:status :ready :blocks [{:identity-status :provisional :syntax [{:from 0 :to (count text)}]}]}})
            d (editor/draft (assoc file :kind :markdown :text "Alpha" :analysis (analysis "Alpha")))
            key (first (keys (get-in d [:rich-base :document :regions])))]
        (swap! states assoc-in [(:owner a) :drafts "x.clj"] d)
        ((:local-source-open a) "x.clj" key)
        ((:local-source-change a) "x.clj" "Changed" {:anchor 2 :head 2})
        ((:save a) "x.clj")
        (is (empty? (:rich-save @calls)))
        (is (= "Changed" (get-in (last-call :analyze) [:request :text])))
        (let [pending (last-call :analyze)]
          ((:local-source-change a) "x.clj" "Later" {:anchor 2 :head 2})
          (reply! (:ok pending) (wire (:target-stamp (state)) (analysis "Changed")))
          (is (= "Alpha" (get-in (state) [:drafts "x.clj" :text])))
          (is (= "Later" (get-in (state) [:drafts "x.clj" :local-source :text]))))
        (let [pending (last-call :analyze)]
          ((:local-source-cancel a) "x.clj")
          (reply! (:error pending) (ex-info "Cancelled preview failed" {}))
          (is (nil? (get-in (state) [:errors [:local-source "x.clj"]]))))
        ((:local-source-open a) "x.clj" key)
        ((:local-source-change a) "x.clj" "Changed" {:anchor 2 :head 2})
        ((:local-source-change a) "x.clj" "Changed" {:anchor 5 :head 5})
        (reply! (:ok (last-call :analyze)) (wire (:target-stamp (state)) (analysis "Changed")))
        (is (= "Changed" (get-in (state) [:drafts "x.clj" :text])))
        (is (= "Changed" (get-in (state) [:drafts "x.clj" :local-source :opened-text])))
        (is (= {:anchor 5 :head 5} (get-in (state) [:drafts "x.clj" :local-source :selection])))
        (is (= "Changed" (get-in (last-call :rich-save) [:request :source])))
        ((:local-source-change a) "x.clj" "Incomplete" nil)
        (reply! (:ok (last-call :analyze)) (wire (:target-stamp (state)) {:semantic {:status :invalid}}))
        (is (= "Incomplete" (get-in (state) [:drafts "x.clj" :local-source :text])))
        (is (= "Changed" (get-in (state) [:drafts "x.clj" :text])))
        (is (= 1 (count (:rich-save @calls))))))))

(deftest tabs-and-sidebar-share-publication-but-consume-their-own-generations
  (with-client
    (fn [{:keys [a data states state stamp seed! calls last-call connection]}]
      (seed!)
      (swap! sig/layout-columns update-in [0 :tabs] conj
             {:id :two :type :memory :data {:room-id "test-room" :path "x.clj"}})
      (let [second (actions/controller (assoc data :tab-id :two))
            navigation (navigation/controller "test-room")
            before (assoc (state) :drafts {"x.clj" (assoc (get-in (state) [:drafts "x.clj"]) :generation 7)})]
        (swap! states assoc (:owner second) before)
        (swap! sig/memory-navigation-state assoc (:owner navigation)
               (assoc (verified connection stamp) :root-expanded? true))
        ((:discover a))
        ((:discover second))
        ((:discover navigation))
        (is (= 1 (count (:metadata @calls))))
        (swap! states update-in [(:owner second) :drafts "x.clj"]
               #(assoc % :generation 8 :text "New human typing"))
        ;; Publish only. The transport must not directly mutate either view.
        ((:ok (last-call :metadata))
         (wire stamp {:folders {"" {:entries [:shared]}} :entries {"x.clj" {:content :changed}}
                      :permissions {:write? true :evaluate? true}}))
        (is (nil? (:metadata-consumed (state))))
        (is (empty? (:file @calls)))
        (pump!)
        (is (= 1 (count (:metadata @calls))))
        (is (= {:content :changed} (get-in (state) [:drafts "x.clj" :incoming-entry])))
        (is (nil? (get-in ((:state second)) [:drafts "x.clj" :incoming-entry])))
        (is (= "New human typing" (get-in ((:state second)) [:drafts "x.clj" :text])))
        (is (= [:shared] (get-in ((:state navigation)) [:folders "" :entries])))
        (let [reads (count (:file @calls))]
          (pump!)
          (is (= reads (count (:file @calls))) "An envelope is consumed once per view"))))))

(deftest room-removal-fences-inflight-and-cached-observations-without-losing-drafts
  (with-client
    (fn [{:keys [a state seed! calls last-call stamp]}]
      (seed!)
      ((:discover a))
      (let [pending (last-call :metadata)]
        (reset! sig/user-rooms {:rooms [] :agents [] :knowledge-bases []})
        ((:ok pending) (wire stamp {:folders {} :entries {"x.clj" {:content :changed}}
                                   :permissions {:write? true :evaluate? true}}))
        (pump!)
        (is (not (:target-verified? (state))))
        (is (nil? (:permissions (state))))
        (is (nil? (:sandbox-permissions (state))))
        (is (= "Original" (get-in (state) [:drafts "x.clj" :text])))
        (is (empty? (:file @calls)))
        ((:discover a))
        (is (= 1 (count (:metadata @calls))) "A removed Room is not retried")
        (is (every? #(empty? (:envelopes %)) (vals @sig/memory-room-metadata)))))))

(deftest one-oversized-tab-does-not-block-a-valid-sibling
  (with-client
    (fn [{:keys [a data states state seed! calls last-call stamp]}]
      (seed!)
      (swap! states assoc-in [(:owner a) :file-intents] (zipmap (map #(str % ".md") (range 65)) (repeat true)))
      (swap! sig/layout-columns update-in [0 :tabs] conj
             {:id :two :type :memory :data {:room-id "test-room" :path "valid.md"}})
      (let [second (actions/controller (assoc data :tab-id :two))]
        ((:discover a))
        (is (string? (get-in (state) [:errors :metadata])))
        (is (not (:target-verified? (state))))
        ((:discover second))
        (is (= 1 (count (:metadata @calls))))
        (is (= ["valid.md"] (get-in (last-call :metadata) [:request :paths])))
        (reply! (:ok (last-call :metadata))
                (wire stamp {:folders {} :entries {"valid.md" nil} :permissions {:write? true}}))
        (is (:target-verified? ((:state second))))
        (is (not (:target-verified? (state))))))))

(deftest separate-batches-can-both-finish-before-either-view-consumes
  (with-client
    (fn [{:keys [a data states state seed! calls last-call stamp]}]
      (seed!)
      (swap! sig/layout-columns update-in [0 :tabs] conj
             {:id :two :type :memory :data {:room-id "test-room"}})
      (let [second (actions/controller (assoc data :tab-id :two))
            make-state (fn [prefix]
                         (let [paths (mapv #(str prefix % ".md") (range 40))]
                           (assoc (state) :selected (first paths)
                                  :drafts (zipmap paths (repeat {:text "Kept" :generation 0
                                                               :source {:target-stamp stamp :entry nil}})))))
            first-state (make-state "a") second-state (make-state "b")
            coordinator (metadata-io/controller runtime (web/current-session) "test-room" (:owner a) states)]
        (swap! states assoc (:owner a) first-state (:owner second) second-state)
        ((:discover a))
        ((:ok (last-call :metadata)) (wire stamp {:folders {} :entries {} :permissions {:write? true}}))
        ((:ensure coordinator))
        (is (= 2 (count (:metadata @calls))))
        ((:ok (last-call :metadata)) (wire stamp {:folders {} :entries {} :permissions {:write? true}}))
        (is (nil? (:metadata-consumed (state))))
        (is (nil? (:metadata-consumed ((:state second)))))
        (is (= 2 (count (:envelopes (first (vals @sig/memory-room-metadata))))))
        (pump!)
        (is (:target-verified? (state)))
        (is (:target-verified? ((:state second))))
        (is (not= (:metadata-consumed (state)) (:metadata-consumed ((:state second)))))
        (is (= 2 (count (:metadata @calls))))))))

(deftest mounted-sidebar-consumes-shared-publication-and-prunes-closed-owners
  (with-client
    (fn [{:keys [states calls last-call stamp]}]
      (let [navigation (navigation/controller "test-room")
            container (.createElement js/document "div")]
        (reset! sig/user-rooms {:rooms [{:room/id "test-room"}] :agents [] :knowledge-bases []})
        (swap! sig/memory-navigation-state assoc (:owner navigation) {:root-expanded? true})
        (let [handle (render/render-spin! container
                                          (navigation/render {:room/id "test-room" :room/name "Knowledge"})
                                          (browser/make-dom-discharge js/document))]
          (try
            (simple/drain-events! runtime nil)
            (is (= 1 (count (:metadata @calls))))
            ;; No pump or controller callback here: the real rendering Spin
            ;; must track the shared Signal and consume its publication.
            ((:ok (last-call :metadata))
             (wire stamp {:folders {"" {:entries [{:path "Result.md" :name "Result.md" :kind :file}]}}
                          :entries {"x.clj" nil} :permissions {:write? true}}))
            (simple/drain-events! runtime nil)
            (is (.includes (.-textContent container) "Result.md"))
            (is (= 1 (count (:metadata @calls))))
            (reset! sig/layout-columns [])
            ((:toggle-root navigation))
            (simple/drain-events! runtime nil)
            (is (empty? (:envelopes (first (vals @sig/memory-room-metadata)))))
            (is (not (.includes (.-textContent container) "Result.md")))
            (finally ((:stop! handle)))))))))

(deftest memory-sidebar-uses-room-trees-without-opening-legacy-sources
  (with-client
    (fn [_]
      (let [container (.createElement js/document "div")
            legacy {:knowledge-bases [{:kb/id "legacy" :kb/name "Old Research Wiki" :kb/db-scope "legacy-store"}]
                    :drives [{:id "old-drive" :name "Old Drive" :room-id "test-room"}]}]
        (with-redefs [sig/nav-collapsed-projects (signal runtime #{})
                      sig/active-nav-keys (signal runtime #{})
                      db-signal/connect-kb! (fn [& _] (throw (js/Error. "No legacy replica connection")))]
          (reset! sig/user-rooms (assoc legacy :rooms [{:room/id "test-room" :room/name "Knowledge"}]))
          (let [handle (render/render-spin! container (nav/render-memory-section-spin)
                                            (browser/make-dom-discharge js/document))]
            (try
              (simple/drain-events! runtime nil)
              (is (.includes (.-textContent container) "Knowledge"))
              (is (not (.includes (.-textContent container) "Old Research Wiki")))
              (is (not (.includes (.-textContent container) "Old Drive")))
              (is (.includes (.-textContent container) "Add memory"))
              (reset! sig/user-rooms (assoc legacy :rooms []))
              (simple/drain-events! runtime nil)
              (is (.includes (.-textContent container) "No workspaces yet"))
              (is (not (.includes (.-textContent container) "Knowledge")))
              (reset! sig/nav-collapsed-projects #{"memory"})
              (simple/drain-events! runtime nil)
              (is (not (.includes (.-textContent container) "Add memory")))
              (finally ((:stop! handle))))))))))

(deftest memory-sidebar-clicks-stay-in-the-rendering-context
  (with-client
    (fn [_]
      (with-redefs [sig/nav-collapsed-projects (signal runtime #{"parent-only"})
                    sig/active-nav-keys (signal runtime #{})]
        (let [child (context/fork-context runtime)
              container (.createElement js/document "div")
              handle (binding [ec/*execution-context* child]
                       (reset! sig/user-rooms {:rooms []})
                       (reset! sig/nav-collapsed-projects #{"memory"})
                       (render/render-spin! container (nav/render-memory-section-spin)
                                            (browser/make-dom-discharge js/document)))]
          (try
            (simple/drain-events! child nil)
            (.dispatchEvent (.querySelector container ".nav-title") (js/MouseEvent. "click" #js {:bubbles true}))
            (simple/drain-events! child nil)
            (is (= #{"parent-only"} @sig/nav-collapsed-projects))
            (is (= #{} (binding [ec/*execution-context* child] @sig/nav-collapsed-projects)))
            (is (.includes (.-textContent container) "Add memory"))
            (finally
              (binding [ec/*execution-context* child] ((:stop! handle)))
              (context/close-context! child))))))))

(deftest a-new-target-in-another-batch-immediately-retires-old-verification
  (with-client
    (fn [{:keys [a data states state seed! calls last-call stamp]}]
      (seed!)
      (swap! sig/layout-columns update-in [0 :tabs] conj
             {:id :two :type :memory :data {:room-id "test-room"}})
      (let [second (actions/controller (assoc data :tab-id :two))
            paths (mapv #(str "b" % ".md") (range 64))
            second-state (assoc (state) :selected (first paths) :drafts {}
                                :file-intents (zipmap paths (repeat true)))
            next-stamp (random-uuid)
            coordinator (metadata-io/controller runtime (web/current-session) "test-room" (:owner a) states)]
        (swap! states assoc (:owner second) second-state)
        ((:discover a))
        ((:ok (last-call :metadata)) (wire stamp {:folders {} :entries {"x.clj" {:content :same}}}))
        ;; Consume the first batch and dispatch the second without its view.
        ((:consume-metadata a))
        (is (:target-verified? (state)))
        (is (= 2 (count (:metadata @calls))))
        ((:ok (last-call :metadata)) (wire next-stamp {:folders {} :entries {}}))
        ((:consume-metadata a))
        (is (not (:target-verified? (state))))
        (is (= next-stamp (:target-stamp (state))))
        (is (:target-stale? (get-in (state) [:drafts "x.clj"])))
        (is (= "Original" (get-in (state) [:drafts "x.clj" :text])))
        (is (= 3 (count (:metadata @calls))) "Its own current-target snapshot is now pending")
        (is (nil? ((:observation coordinator))))))))

(deftest actual-roster-shape-admits-current-room-and-revokes-a-cached-observation
  (with-client
    (fn [{:keys [a state seed! last-call calls stamp file]}]
      (seed!)
      (reset! sig/user-rooms {:rooms [{:room/id "test-room" :room/name "Current"}]
                              :agents [] :knowledge-bases []})
      ((:discover a))
      (is (= 1 (count (:metadata @calls))))
      (reply! (:ok (last-call :metadata))
              (wire stamp {:folders {} :entries {"x.clj" (:entry file)} :permissions {:write? true}}))
      (is (:target-verified? (state)))
      (reset! sig/user-rooms {:rooms [{:room/id "other-room" :room/name "Still visible"}]
                              :agents [] :knowledge-bases []})
      ;; A direct event before the next render must not reuse cached authority.
      ((:save a) "x.clj")
      (is (empty? (:save @calls)))
      (is (not (:target-verified? (state))))
      (is (nil? (:permissions (state))))
      (is (= "Original" (get-in (state) [:drafts "x.clj" :text])))
      (is (= 1 (count (:metadata @calls))))
      (reset! sig/user-rooms {:rooms [{:room/id "test-room"}] :agents [] :knowledge-bases []})
      ((:discover a))
      (is (= 2 (count (:metadata @calls))) "Restoration requires a fresh observation")
      (reply! (:ok (last-call :metadata))
              (wire stamp {:folders {} :entries {"x.clj" (:entry file)} :permissions {:write? true}}))
      (is (:target-verified? (state))))))

(deftest a-copied-pending-read-is-replaced-in-the-fork-without-sharing-publications
  (with-client
    (fn [{:keys [a data seed! state last-call calls stamp file]}]
      (seed!)
      ((:discover a))
      (let [parent-reply (:ok (last-call :metadata)) child (context/fork-context runtime)]
        (try
          (let [forked (binding [ec/*execution-context* child] (actions/controller data))]
            ((:discover forked))
            (is (= 2 (count (:metadata @calls))) "Fork has its own native request ownership")
            (let [child-reply (:ok (last-call :metadata))
                  payload (wire stamp {:folders {} :entries {"x.clj" (:entry file)} :permissions {:write? true}})]
              (child-reply payload)
              ((:consume-metadata forked))
              (is (:metadata-consumed ((:state forked))))
              (is (nil? (:metadata-consumed (state))))
              (is (nil? (get-in (first (vals @sig/memory-room-metadata)) [:sequence])))
              (parent-reply payload)
              ((:consume-metadata a))
              (is (:metadata-consumed (state)))
              (is (not= (:metadata-consumed (state)) (:metadata-consumed ((:state forked)))))
              (is (= "Original" (get-in (state) [:drafts "x.clj" :text])))))
          (finally (context/close-context! child)))))))

(deftest obsolete-shared-failure-is-not-consumed-by-a-new-owner
  (with-client
    (fn [{:keys [a data states state seed! calls last-call stamp connection file]}]
      (seed!)
      ((:discover a))
      (reply! (:error (last-call :metadata)) (ex-info "Temporary failure" {}))
      (is (string? (get-in (state) [:errors :metadata])))
      (reset! sync/state {:connection connection :status :ready :dirty {"test-room" 1}})
      ((:discover a))
      (swap! sig/layout-columns update-in [0 :tabs] conj
             {:id :two :type :memory :data {:room-id "test-room" :path "x.clj"}})
      (let [second (actions/controller (assoc data :tab-id :two))]
        (swap! states assoc (:owner second)
               (assoc (verified connection stamp) :selected "x.clj" :drafts {"x.clj" (editor/draft file)}))
        ((:discover second))
        (is (= 2 (count (:metadata @calls))))
        (is (nil? (:metadata-consumed ((:state second)))))
        (is (nil? (get-in ((:state second)) [:errors :metadata])))
        (is (true? (get-in ((:state second)) [:loading :metadata])))))))

(deftest socket-replacement-fences-old-shared-reply-and-starts-without-waiting
  (with-client
    (fn [{:keys [a state seed! last-call calls stamp file]}]
      (seed!)
      ((:discover a))
      (let [old (:ok (last-call :metadata)) next-connection {:epoch (random-uuid)}]
        (reset! sig/memory-connection next-connection)
        ((:discover a))
        (is (= 2 (count (:metadata @calls))))
        (let [before (state)]
          (old (wire stamp {:folders {} :entries {"x.clj" nil} :permissions {:write? true}}))
          (is (= before (state)))
          (is (nil? (:sequence (first (vals @sig/memory-room-metadata))))))
        (reply! (:ok (last-call :metadata))
                (wire stamp {:folders {} :entries {"x.clj" (:entry file)} :permissions {:write? true}}))
        (is (= next-connection (:verified-connection (state))))
        (is (:target-verified? (state)))))))

(deftest roster-update-that-retains-room-fences-old-body-read
  (with-client
    (fn [{:keys [a seed! state last-call stamp file]}]
      (seed!)
      (reset! sig/user-rooms {:rooms [{:room/id "test-room" :room/name "Before"}]})
      ((:reload a) "x.clj")
      (let [old (:ok (last-call :file)) before (state)]
        (reset! sig/user-rooms {:rooms [{:room/id "test-room" :room/name "After"}]})
        (old (wire stamp (assoc file :text "Obsolete body")))
        (is (= before (state)))))))

(deftest property-save-requires-a-current-draft-and-never-repeats-an-acknowledged-edit
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          path "wiki/a.md" target {:kind :page} id (random-uuid)
          states (signal runtime {}) calls (atom [])
          field {:id id :alias "revenue" :label "Revenue" :editable? true :input {:text "0"}}]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    remote/save-memory-property! (fn [_ _ request]
                                                   (swap! calls conj request) (fn [_ _]))]
        (let [a (actions/controller {:room-id "room" :state-signal states}) key (:owner a)
              state (assoc (verified connection stamp) :selected path
                           :drafts {path {:source {:entry {:content id} :text "Saved" :target-stamp stamp} :text "Saved"}})
              model {:path path :target target :target-stamp stamp :expected {:content id}
                     :request-marker (editor/property-marker state path target)}]
          (swap! states assoc key (assoc-in state [:property-models [path target]] model))
          ((:property-save a) path target field)
          (is (empty? @calls) "No draft: an old blur callback cannot write")
          ((:property-input a) path target field {:text "1"})
          ((:property-save a) path target field)
          ((:property-save a) path target field)
          (is (= 1 (count @calls)) "Enter followed by blur is single-flight")
          (swap! states update key #(-> % (assoc-in [:loading [:property-save path]] false)
                                         (update-in [:property-drafts [path target]] dissoc id)))
          ((:property-save a) path target field)
          (is (= 1 (count @calls)) "After acknowledgement cleared the draft, late blur is inert"))))))

(deftest saved-view-controller-rejects-old-generations-and-clears-data-after-read-failure
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp page-view-fixture/stamp
          path page-view-fixture/path view page-view-fixture/view-id states (signal runtime {}) calls (atom [])
          result (fn [request basis]
                   (merge (dissoc request :expected-target-stamp)
                          {:profile saved-view-contract/profile :descriptor page-view-fixture/descriptor
                           :target-stamp stamp :status :ready :basis basis :total 0 :complete? true
                           :rows [] :columns [{:kind :title :label "Page"}]}))]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    saved-view-remote/load-memory-saved-view!
                    (fn [_ _ request]
                      (fn [ok error] (swap! calls conj {:request request :ok ok :error error})))]
        (let [a (actions/controller {:room-id "room" :tab-id :table :state-signal states})
              key (:owner a) state #(get @states key)]
          (swap! states assoc key (merge page-view-fixture/state (verified connection stamp) {:selected path}))
          ((:ensure-saved-views a))
          ((:ensure-saved-views a))
          (is (= 1 (count @calls)))
          (swap! states update-in [key :drafts path :generation] inc)
          (reply! (:ok (first @calls)) (result (:request (first @calls)) (random-uuid)))
          (is (nil? (get-in (state) [:saved-views [path view]])))
          ((:ensure-saved-views a))
          (is (= 2 (count @calls)))
          (let [call (second @calls) basis (random-uuid)]
            (reply! (:ok call) (result (:request call) basis))
            (is (= basis (:basis (page-views/visible (state) path view)))))
          (swap! states update-in [key :read-epoch] inc)
          ((:ensure-saved-views a))
          (is (= 3 (count @calls)))
          (is (page-views/visible (state) path view) "prior data remains during refresh")
          (reply! (:error (nth @calls 2)) (js/Error. "Read unavailable"))
          (is (nil? (page-views/visible (state) path view)) "uncertain failure clears prior data")
          ((:ensure-saved-views a))
          (is (= 3 (count @calls)) "an error does not create a render retry loop")
          ((:retry-saved-view a) path view)
          (is (= 4 (count @calls)))
          ((:toggle-saved-view a) path view)
          (reply! (:ok (nth @calls 3)) (result (:request (nth @calls 3)) (random-uuid)))
          (is (nil? (page-views/visible (state) path view)) "Hide rejects a pending reply")
          ((:ensure-saved-views a))
          (is (= 4 (count @calls)) "Hidden views do not load")
          ((:toggle-saved-view a) path view)
          (is (= 5 (count @calls)) "explicit reopening starts a fresh read at an unchanged marker")
          (reply! (:ok (nth @calls 3)) (result (:request (nth @calls 3)) (random-uuid)))
          (is (nil? (get-in (state) [:saved-views [path view]])) "old read cannot win after reopening")
          (let [basis (random-uuid)]
            (reply! (:ok (nth @calls 4)) (result (:request (nth @calls 4)) basis))
            (is (= basis (:basis (page-views/visible (state) path view)))))
          (is (get-in (state) [:page-view-open [path view]])))))))

(deftest source-property-writes-observe-other-tabs-drafts-and-in-flight-saves
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp (random-uuid)
          path "companies/a.md" target {:kind :page} id (random-uuid) scope [path target]
          states (signal runtime {}) calls (atom [])
          field {:id id :alias "revenue" :editable? true :input {:text "0"}}]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    remote/save-memory-property! (fn [_ _ request] (swap! calls conj request) (fn [_ _]))]
        (let [a (actions/controller {:room-id "room" :tab-id :first :state-signal states})
              b (actions/controller {:room-id "room" :tab-id :second :state-signal states})
              base (assoc (verified connection stamp) :selected path
                          :drafts {path {:source {:entry {:content id} :text "Saved" :target-stamp stamp} :text "Saved"}})
              model {:path path :target target :target-stamp stamp :expected {:content id}
                     :request-marker (editor/property-marker base path target)}
              base (assoc-in base [:property-models scope] model)]
          (swap! states assoc (:owner a) base (:owner b) (assoc-in base [:drafts path :text] "Unsaved source"))
          ((:property-input a) path target field {:text "1"})
          ((:property-save a) path target field)
          (is (empty? @calls))
          (is ((:property-blocked? a) path))
          (swap! states assoc-in [(:owner b) :drafts path :text] "Saved")
          ((:property-save a) path target field)
          (is (= 1 (count @calls)))
          ((:property-input b) path target field {:text "2"})
          ((:property-save b) path target field)
          (is (= 1 (count @calls)) "the other tab cannot overlap a pending property save")
          (is (= {:text "2"} (get-in @states [(:owner b) :property-drafts scope id :input])))
          (swap! states update (:owner a)
                 #(-> % (assoc-in [:loading [:property-save path]] false)
                        (update-in [:property-drafts scope] dissoc id)))
          ((:property-save b) path target field)
          (is (= 2 (count @calls))))))))

(deftest failed-row-enrollment-retries-only-on-explicit-action-and-respects-known-absence
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp page-view-fixture/stamp
          path page-view-fixture/path view page-view-fixture/view-id row {:id (random-uuid) :path "companies/a.md"}
          states (signal runtime {}) calls (atom []) metadata-calls (atom [])]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    remote/load-memory-file! (fn [_ _ request]
                                               (fn [ok error] (swap! calls conj {:request request :ok ok :error error})))
                    remote/load-memory-metadata! (fn [_ _ request]
                                                   (swap! metadata-calls conj request) (fn [_ _]))]
        (let [a (actions/controller {:room-id "room" :tab-id :table :state-signal states})
              key (:owner a) state #(get @states key)
              base (merge page-view-fixture/state (verified connection stamp) {:selected path})
              query (page-views/request base path view)
              reply (merge (dissoc query :expected-target-stamp)
                           {:profile saved-view-contract/profile :descriptor page-view-fixture/descriptor
                            :target-stamp stamp :status :ready :basis (random-uuid) :rows [row]
                            :columns [{:kind :title :label "Page"}]})]
          (swap! states assoc key (-> base
                                     (assoc-in [:page-view-open [path view]] true)
                                     (assoc-in [:saved-view-attempts [path view]] (page-views/marker base path view))
                                     (assoc-in [:saved-views [path view]] {:reply reply})))
          ((:edit-page-view-row a) path view row)
          (is (= 1 (count @calls)))
          (is (= path (:selected (state))) "the report stays selected while its row is enrolled")
          (reply! (:error (first @calls)) (js/Error. "File unavailable"))
          (dotimes [_ 3] ((:ensure-saved-views a)))
          (is (= 1 (count @calls)) "render does not retry failed file reads")
          ((:retry-page-view-row a) path view)
          (is (= 2 (count @calls)))
          (reply! (:error (second @calls)) (js/Error. "File unavailable"))
          (swap! states assoc-in [key :observed-entries (:path row)] nil)
          ((:retry-page-view-row a) path view)
          (is (= 2 (count @calls)) "known absence is reconciled through metadata, not implicit loading")
          (is (not (get-in (state) [:loading [:file (:path row)]]))))))))

(deftest returning-to-an-open-form-reloads-its-retired-clean-selection
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp page-view-fixture/stamp
          path page-view-fixture/path view page-view-fixture/view-id
          row {:id (random-uuid) :path "companies/a.md"}
          descriptor (-> page-view-fixture/descriptor (assoc :presentation :form :limit 1)
                         (assoc-in [:source :page] (:id row)))
          states (signal runtime {}) calls (atom [])]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    saved-view-remote/load-memory-saved-view!
                    (fn [_ _ request]
                      (fn [ok error] (swap! calls conj {:request request :ok ok :error error})))
                    remote/load-memory-file! (fn [& _] (fn [_ _]))]
        (let [a (actions/controller {:room-id "room" :tab-id :form :state-signal states})
              key (:owner a) state #(get @states key)
              base (-> (merge page-view-fixture/state (verified connection stamp) {:selected path})
                       (assoc-in [:drafts path :source :analysis :saved-views 0 :descriptor] descriptor))
              result (fn [request]
                       (merge (dissoc request :expected-target-stamp)
                              {:profile saved-view-contract/profile :descriptor descriptor :target-stamp stamp
                               :status :ready :basis (random-uuid) :rows [row] :columns [{:kind :title}]}))]
          (swap! states assoc key
                 (-> base
                     (assoc-in [:page-view-open [path view]] true)
                     (assoc-in [:page-view-edits [path view]] row)
                     (assoc-in [:saved-view-attempts [path view]] (page-views/marker base path view))
                     (assoc-in [:saved-views [path view]] {:reply (result (page-views/request base path view))})
                     (assoc :page-view-enrolled #{(:path row)})
                     (assoc-in [:drafts (:path row)] {:source page-view-fixture/file :text "saved"})))
          (swap! states assoc-in [key :selected] "notes/elsewhere.md")
          ((:ensure-saved-views a))
          (is (nil? (get-in (state) [:page-view-edits [path view]])))
          (is (nil? (get-in (state) [:drafts (:path row)])))
          (swap! states assoc-in [key :selected] path)
          ((:ensure-saved-views a))
          (is (= 1 (count @calls)) "returning obtains a fresh read even with a compatible cached result")
          (let [call (first @calls)] (reply! (:ok call) (result (:request call))))
          (is (= row (get-in (state) [:page-view-edits [path view]])))
          ((:ensure-saved-views a))
          (is (= 1 (count @calls)) "selection restoration does not cause a query loop"))))))

(deftest changing-a-page-view-to-a-scorecard-retains-drafts-without-enrolling-page-controls
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp page-view-fixture/stamp
          path page-view-fixture/path view page-view-fixture/view-id
          row {:id (random-uuid) :path "companies/a.md"}
          scorecard (random-uuid) receipt-context {:room (random-uuid) :store (random-uuid)}
          descriptor {:version 1 :id view :source {:kind :scorecard :room :current :scorecard scorecard}
                      :presentation :table :columns [{:kind :candidate}] :limit 32}
          states (signal runtime {}) reads (atom []) file-reads (atom []) property-reads (atom [])]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    saved-view-remote/load-memory-saved-view!
                    (fn [_ _ request] (fn [ok error] (swap! reads conj {:request request :ok ok :error error})))
                    remote/load-memory-file!
                    (fn [& args] (swap! file-reads conj args) (fn [_ _]))
                    remote/load-memory-properties!
                    (fn [& args] (swap! property-reads conj args) (fn [_ _]))]
        (let [a (actions/controller {:room-id "room" :tab-id :report :state-signal states})
              key (:owner a) state #(get @states key)
              base (-> (merge page-view-fixture/state (verified connection stamp) {:selected path})
                       (assoc-in [:drafts path :source :analysis :saved-views 0 :descriptor] descriptor)
                       (assoc :page-view-open {[path view] true} :page-view-edits {[path view] row}
                              :page-view-enrolled #{(:path row)})
                       (assoc-in [:drafts (:path row)] {:source page-view-fixture/file :text "Human dirty draft"}))]
          (swap! states assoc key base)
          ((:ensure-saved-views a))
          (is (empty? @reads) "no receipt request without authenticated Room/store context")
          ((:observe-receipt-context a) receipt-context)
          ((:ensure-saved-views a))
          (is (= 1 (count @reads)))
          ((:edit-page-view-row a) path view row)
          ((:retry-page-view-row a) path view)
          (is (empty? @file-reads))
          (is (empty? @property-reads))
          (is (= "Human dirty draft" (get-in (state) [:drafts (:path row) :text])))
          (is (= row (get-in (state) [:page-view-edits [path view]])))
          (let [call (first @reads)
                reply (merge (dissoc (:request call) :expected-target-stamp)
                             {:profile saved-view-contract/profile :descriptor descriptor :target-stamp stamp
                              :status :ready :basis (random-uuid) :rows [] :columns []
                              :source (assoc receipt-context :kind :scorecard :scorecard scorecard)})]
            ((:observe-receipt-context a) nil)
            (reply! (:ok call) reply)
            (is (nil? (page-views/visible (state) path view)))
            (is (= "Human dirty draft" (get-in (state) [:drafts (:path row) :text])))))))))

(deftest explicit-snapshot-recovery-reuses-the-frozen-path-and-bytes-after-lost-ack
  (binding [ec/*execution-context* runtime]
    (let [session {:id (random-uuid)} connection {:epoch (random-uuid)} stamp page-view-fixture/stamp
          path page-view-fixture/path view page-view-fixture/view-id
          states (signal runtime {}) writes (atom []) reads (atom []) exists? (atom true)
          delay-metadata? (atom false) metadata-calls (atom [])]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    remote/save-memory-file! (fn [_ _ request]
                                               (fn [ok error] (swap! writes conj {:request request :ok ok :error error})))
                    remote/load-memory-file! (fn [_ _ request]
                                               (fn [ok error] (swap! reads conj {:request request :ok ok :error error})))
                    remote/load-memory-folder! (fn [& _] (fn [_ _]))
                    remote/load-memory-metadata! (fn [_ _ request]
                                                   (fn [ok error]
                                                     (if @delay-metadata?
                                                       (swap! metadata-calls conj {:request request :ok ok :error error})
                                                       (ok (wire stamp {:entries (into {} (map (fn [p] [p (when @exists? {:content (random-uuid) :size 10 :mode 33188})]) (:paths request)))})))))]
        (let [a (actions/controller {:room-id (str (:room snapshot-fixture/context)) :tab-id :export :state-signal states})
              key (:owner a) state #(get @states key)
              base (merge page-view-fixture/state (verified connection stamp)
                          {:selected path :stream-marker [nil nil 0]})
              result (merge snapshot-fixture/reply
                            (dissoc (page-views/request base path view) :expected-target-stamp)
                            {:descriptor page-view-fixture/descriptor :target-stamp stamp})
              initial (assoc-in base [:saved-views [path view]] {:reply result :marker (page-views/marker base path view)})]
          (swap! states assoc key initial)
          ((:save-view-snapshot a) path view)
          (is (= 1 (count @writes)))
          (let [request (:request (first @writes)) intent (get-in (state) [:snapshot-intents [path view]])]
            (is (= (:path request) (:path intent)))
            (is (nil? (:expected request)))
            (is (= stamp (:expected-target-stamp request)))
            (is (str/includes? (:text request) "Page collection snapshot"))
            ((:save-view-snapshot a) path view)
            (is (= 1 (count @writes)) "in-flight clicks cannot allocate another export")
            (reply! (:error (first @writes)) (js/Error. "Connection lost after commit"))
            ;; The frozen export remains meaningful even if the current report
            ;; changes. Recovery must not silently format that newer report.
            (swap! states assoc-in [key :stream-marker] [:newer :ready 2])
            (swap! states assoc-in [key :saved-views [path view] :reply :basis] (random-uuid))
            ((:save-view-snapshot a) path view)
            (is (= 1 (count @reads)))
            (is (= (:path request) (get-in @reads [0 :request :path])))
            (reply! (:ok (first @reads)) (wire stamp {:text (:text request)}))
            (is (= 1 (count @writes)) "exact existing bytes acknowledge the committed artifact without another write")
            (is (= (:path request) (get-in (state) [:snapshot-exports [path view] :path])))
            (is (nil? (get-in (state) [:snapshot-intents [path view]])))
            (is (= path (:selected (state))) "completion does not navigate away from the user's current page")
            ;; A confirmed absent destination retries the exact original CAS.
            (swap! states assoc-in [key :snapshot-intents [path view]] intent)
            (reset! exists? false)
            ((:save-view-snapshot a) path view)
            (is (= 2 (count @writes)))
            (is (= request (:request (second @writes))))
            (reply! (:error (second @writes)) (js/Error. "Uncertain write"))
            (reset! exists? true)
            ((:save-view-snapshot a) path view)
            (is (= 2 (count @reads)))
            (reply! (:ok (second @reads)) (wire stamp {:text "Different content"}))
            (is (= 2 (count @writes)) "a differing destination is never overwritten")
            (is (= intent (get-in (state) [:snapshot-intents [path view]])))
            (is (str/includes? (get-in (state) [:errors [:snapshot path view]]) "different content"))
            ;; A different target cannot replay an outstanding export.
            (swap! states assoc-in [key :target-stamp] (random-uuid))
            ((:save-view-snapshot a) path view)
            (is (= 2 (count @reads)))
            (is (= 2 (count @writes)))
            (is (str/includes? (get-in (state) [:errors [:snapshot path view]]) "original workspace"))
            (swap! states assoc-in [key :target-stamp] stamp)
            (reset! delay-metadata? true)
            ((:save-view-snapshot a) path view)
            (is (= 1 (count @metadata-calls)))
            (reply! (:error (first @metadata-calls)) (js/Error. "Metadata request settled"))
            (reply! (:ok (first @metadata-calls)) (wire stamp {:entries {(:path request) nil}}))
            (is (= 2 (count @writes)) "a callback after request settlement cannot start a nested write")
            ((:save-view-snapshot a) path view)
            (is (= 2 (count @metadata-calls)))
            ;; The visible target UUID is A again, but the initiating epoch is
            ;; obsolete after A -> B -> A. Do not send a nested write.
            (swap! states update-in [key :target-epoch] + 2)
            (reply! (:ok (second @metadata-calls)) (wire stamp {:entries {(:path request) nil}}))
            (is (= 2 (count @writes)))
            (is (= 2 (count @reads)))))))))
