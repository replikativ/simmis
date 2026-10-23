(ns is.simm.uis.web.desktop.memory-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [org.replikativ.spindel.engine.core :as ec]
            [org.replikativ.spindel.engine.context :as context]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.memory-actions :as actions]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.memory-reference-remote :as reference-remote]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.views.memory-navigation :as navigation]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.runtimes.web :as web])
  (:require-macros [org.replikativ.spindel.signal :refer [signal]]))

(defn- wire [stamp reply] (assoc reply :target-stamp stamp :protocol-version 2))

(defn- verified [connection stamp]
  {:connection connection :verified-connection connection :watch-connection connection
   :target-stamp stamp :target-verified? true :target-epoch 0 :read-epoch 0
   :permissions {:write? true :evaluate? true} :sandbox-permissions {:evaluate? true}})

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
          ((:ensure-sync a) connection stream)
          (is (nil? (get-in (state) [:folders "closed"])))
          (@folder (wire stamp {:entries [:stale]}))
          (is (nil? (get-in (state) [:folders "slow"])))
          (@metadata (wire stamp {:folders {"" {:entries []} "slow" {:entries [:current]}} :entries {}}))
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
          ((:ensure-sync a) connection stream)
          (let [before @metadata]
            ((:toggle a) "wiki")
            ((:ensure-sync a) connection (assoc-in stream [:dirty "test-room"] 1))
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
          ((:ok (first @requests)) (wire stamp {:status :resolved :path path :entry {:content :current}}))
          (is (= path (:selected (state))))
          (is (= 1 (count @file-reads)) "Even an already open path receives a current read")
          ((:ok (first @file-reads)) (wire stamp {:path path :kind :text :entry {:content :current} :text "Current source"}))
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
          (swap! states assoc key (assoc (verified connection stamp)
                                         :selected "a.clj" :folders {"" {:entries []}}
                                         :drafts {"a.clj" (editor/draft a-file) "b.clj" (editor/draft b-file)}))
          ((:folder a) "slow")
          (let [old-folder (:folder @replies)]
            ((:ensure-sync a) connection stream)
            ((:ensure-sync a) connection stream)
            ((:metadata @replies) (wire stamp {:folders {"" {:entries [:new]} "slow" {:entries [:current]}}
                                               :entries {"a.clj" (:entry a-file) "b.clj" {:content :new-b}} :revision :v1}))
            (is (= "a.clj" (:selected (state))) "Background B refresh does not select B")
            (old-folder (wire stamp {:entries [:stale]}))
            (is (= [:current] (get-in (state) [:folders "slow" :entries])))
            ((:reload a) "a.clj")
            (let [old-file (get @replies [:file "a.clj"])]
              ((:ensure-sync a) connection (assoc-in stream [:dirty "test-room"] 1))
              ((:metadata @replies) (wire stamp {:folders {"" {:entries []} "slow" {:entries []}}
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
          ((:ensure-sync a) connection stream)
          ((:ensure-sync a) connection stream)
          ((:ensure-sync a) connection later)
          (is (= 1 @calls))
          (@pending (wire stamp {:folders {} :entries {} :revision :one}))
          ((:ensure-sync a) connection later)
          (is (= 2 @calls))
          (@pending (wire stamp {:folders {} :entries {} :revision :two}))
          ((:ensure-sync a) connection later)
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
            ((:ensure-sync a) connection connecting)
            (is (= 1 @calls))
            ((:ensure-sync a) connection first-event)
            (is (= 1 @calls) "Do not race two metadata reads while the first is in flight")
            (@pending (wire stamp {:folders {} :entries {} :revision :before}))
            ((:ensure-sync a) connection first-event)
            (is (= 2 @calls) "The changed stream marker catches up after the old read completes")
            (@pending (wire stamp {:folders {} :entries {} :revision :first-event}))
            ((:ensure-sync a) connection settled)
            (is (= 3 @calls) "Readiness or the later initial hint forces the final catch-up")
            (@pending (wire stamp {:folders {} :entries {} :revision :current}))
            ((:ensure-sync a) connection settled)
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
            ((:ensure-sync a) connection stream)
            ((:ensure-sync a) connection stream)
            (initial (wire stamp {:path "a.clj" :entry {:content :old} :text "old"}))
            ((:metadata @replies) (wire stamp {:folders {} :entries {"a.clj" {:content :new}} :revision :new}))
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
                        (is (not (some #{"uploaded.clj"} (:paths request))))
                        (fn [ok _] (swap! replies assoc :metadata ok)))
                      remote/load-memory-file!
                      (fn [& _] (fn [ok _] (swap! loads conj ok)))]
          (let [a (actions/controller {:room-id "test-room" :col-id :test :state-signal states})
                stream {:connection connection :status :ready :dirty {}}
                state #(get @states (:owner a))]
            ;; Keep the browser-proved ordering exactly: verified target, M
            ;; snapshots no uploaded path, THEN that path's body starts.
            (swap! states assoc (:owner a) (verified connection stamp))
            ((:ensure-sync a) connection stream)
            ((:ensure-sync a) connection stream)
            ;; Like upload completion: this path did not exist when metadata
            ;; captured its request. No selected/deep-link rerender retries it.
            ((:open a) "uploaded.clj")
            ((:select-path a) "other.clj")
            (let [old-body (first @loads)]
              (when body-first? (old-body file))
              ((:metadata @replies) (wire stamp {:folders {} :entries {} :revision :snapshot-before-upload}))
              (when-not body-first?
                (is (= 2 (count @loads)) "Metadata must replace the invalidated, omitted body request")
                (old-body (assoc file :text "Stale response"))
                (is (nil? (get-in (state) [:drafts "uploaded.clj"])))
                (is (true? (get-in (state) [:loading [:file "uploaded.clj"]]))
                    "The old callback must not finish its replacement request")
                ((last @loads) file))
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
                         (swap! calls update kind (fnil conj []) {:request request :ok ok :error error}))))]
      (with-redefs [web/current-session (constantly session) web/current-session? (constantly true)
                    sig/memory-connection (signal runtime connection)
                    sig/memory-navigation-state (signal runtime {}) sig/layout-columns (signal runtime [])
                    sync/ensure! (fn [_]) sync/retry! #(swap! retries conj %)
                    remote/prepare-memory-stream! (fn [& _] (fn [ok _] (ok {:permissions {:evaluate? true}})))
                    remote/load-memory-metadata! (deferred :metadata)
                    remote/load-memory-folder! (deferred :folder) remote/load-memory-file! (deferred :file)
                    remote/search-memory-files! (deferred :search) remote/analyze-memory-text! (deferred :analyze)
                    remote/load-memory-history! (deferred :history) remote/load-memory-history-file! (deferred :history-file)
                    remote/download-memory-file! (deferred :download) remote/load-memory-office-part! (deferred :office)
                    remote/save-memory-file! (deferred :save) remote/save-memory-rich-file! (deferred :rich-save)
                    remote/load-memory-properties! (deferred :properties) remote/save-memory-property! (deferred :property-save)
                    remote/evaluate-memory-repl! (deferred :repl)
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
                           ((:discover a))
                           ((:ok (last (:metadata @calls)))
                            (wire target {:folders {"" {:entries []}} :entries {"x.clj" (:entry file)}
                                          :revision (random-uuid) :permissions {:write? true :evaluate? true}})))}))))))

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
          ((:ok (last-call :history)) (wire stamp {:versions []}))
          (let [metadata (wire stamp {:folders {} :entries {"x.clj" (when old-exists? (:entry file))}
                                      :revision :before-move :permissions {:write? true :evaluate? true}})]
            ((:move a) "x.clj" "moved.clj")
            (when metadata-first? ((:ok old-metadata) metadata))
            (let [old-bodies (filterv #(= "x.clj" (get-in % [:request :path])) (:file @calls))
                  old-histories (:history @calls)]
            ((:ok (last-call :move)) (wire stamp {:path "moved.clj" :entry (:entry file)}))
            (when-not metadata-first? ((:ok old-metadata) metadata))
            (doseq [body old-bodies]
              ((:error body) (ex-info "Old path has been removed" {}))
              ((:ok body) (assoc file :text "Late original body")))
            (doseq [history old-histories]
              ((:ok history) (wire stamp {:versions [{:old-history true}]})))
            (let [current (last (filter #(some #{"moved.clj"} (get-in % [:request :paths])) (:metadata @calls)))]
              (is (some? current) "Superseded discovery drains a current request")
              (is (not (some #{"x.clj"} (get-in current [:request :paths]))))
              ((:ok current) (wire stamp {:folders {} :entries {"moved.clj" (:entry file)}
                                         :revision :after-move :permissions {:write? true :evaluate? true}})))
            ((:ok (last-call :file)) (assoc file :path "moved.clj"))
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
      ((:ok metadata) (wire stamp {:folders {} :entries {"x.clj" old-entry}
                                  :revision :old-path :permissions {:write? true :evaluate? true}}))
      ((:ok (last-call :move)) (wire stamp {:path "moved.clj" :entry (:entry file)}))
      (is (= "Typed during move" (get-in (state) [:drafts "moved.clj" :text])))
      (is (nil? (get-in (state) [:drafts "x.clj"])))
      (is (nil? (get-in (state) [:autosave "x.clj"])))
      (is (= "moved.clj" (get-in (last-call :save) [:request :path])))
      (is (= "Typed during move" (get-in (last-call :save) [:request :text])))
      (is (not (get-in (state) [:drafts "moved.clj" :remote-deleted?])))
      (is (nil? (get-in (state) [:drafts "moved.clj" :incoming-entry])))
      ((:ok (last-call :save)) (wire stamp {:path "moved.clj" :entry {:content :typed}}))
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
            ((:ok old) (wire stamp {:entry (:entry file) :reading {:status :ready :notice "old"}}))
            (is (true? (get-in (state) [:loading [:office-reading "x.clj"]])))
            (is (nil? (get-in (state) [:drafts "x.clj" :office :reading])))
            ((:ok current) (wire stamp {:entry next-entry :text "Server XML" :part "another.xml"
                                       :reading {:status :ready :notice "current"}}))))
        (is (= xml (select-keys (get-in (state) [:drafts "x.clj" :office]) (keys xml))))
        (is (= (:entry file) (get-in (state) [:drafts "x.clj" :source :entry])))
        (is (= next-entry (get-in (state) [:drafts "x.clj" :office :reading-entry])))
        ((:office-open a) "x.clj" nil true)
        (let [pending (last-call :office)]
          (swap! states assoc-in [(:owner a) :drafts "x.clj" :remote-deleted?] true)
          ((:ok pending) (wire stamp {:entry next-entry :reading {:status :ready :notice "deleted"}})))
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
        ((:ok old) (wire stamp {:entry (:entry file) :text "Late XML"}))
        (is (true? (get-in (state) [:loading [:office "x.clj"]])))
        (is (nil? (get-in (state) [:drafts "x.clj" :office])))
        ((:ok (last-call :office)) (wire stamp {:entry (:entry file) :text "Current XML" :reading {:status :ready}}))
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
        ((:ok (last-call :office-save)) (wire stamp {:entry entry :commit :saved}))
        (is (= entry (get-in (last-call :office) [:request :expected])))
        ((:ok (last-call :office)) (wire stamp {:entry entry :text "First XML edit" :reading {:status :ready}}))
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
            ((:ok (last-call :office-correct)) (wire stamp {:entry entry :commit :corrected}))
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
      ((:ensure-sync a) connection {:connection connection :status :error})
      ((:ok (last-call :metadata)) (wire stamp {:folders {} :entries {"x.clj" {:content :x}} :permissions {:write? true}}))
      (is (true? (actions/verified? (state) connection)))
      (is (= stamp (get-in (last-call :file) [:request :expected-target-stamp])))
      ((:ok (last-call :file)) (wire stamp {:path "x.clj" :entry {:content :x} :text "Current"}))
      (is (= "Current" (get-in (state) [:drafts "x.clj" :text])))
      ;; The shared transport spy records BOTH owners: editor Refresh requests
      ;; its open path, then navigation-refresh reconciles a separate tree-only
      ;; controller. Count each request shape, not an assumed shared owner.
      ((:refresh a))
      (is (= 2 (count (filter #(= ["x.clj"] (get-in % [:request :paths])) (:metadata @calls))))
          "Exactly one initial and one manual discovery belong to the editor, despite the broken stream")
      (is (= 1 (count (filter #(empty? (get-in % [:request :paths])) (:metadata @calls))))
          "Refresh also reconciles the independently owned navigation tree once")
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
            ((:ok old) (wire stamp (assoc file :path "moved.clj" :saved? true
                                          :permissions {:write? false :evaluate? false})))
            (is (= before (state)) (str "Old success has no payload/permission/loading/effect authority: " kind))
            ((:error old) (ex-info "Late failure" {}))
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
        ((:ok (last-call :file)) (wire target file))
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
        ((:ok (last-call kind)) (wire stamp (assoc file :path "moved.clj" :permissions {:write? false})))
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
      ((:error (last-call :metadata)) (ex-info "Default currently unavailable" {}))
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
      (let [other (actions/controller (assoc data :tab-id :two))]
        (is (not= (:owner a) (:owner other)))
        ((:create other) "new.md" stamp)
        ((:ok (last-call :metadata)) (wire (random-uuid) {:folders {} :entries {} :permissions {:write? true}}))
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
              fresh (actions/controller intent) target (if same? stamp (random-uuid))]
          ((:ensure-path fresh))
          (is (empty? (:file @calls)) "No body read can race ahead of create origin")
          ((:ok (last-call :metadata))
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
        ((:ok save) (wire stamp (assoc file :entry {:content :submitted} :saved? true)))
        (is (= "Newer human typing" (get-in (state) [:drafts "x.clj" :text])))
        (is (= "Submitted" (get-in (state) [:drafts "x.clj" :source :text])))
        (is (editor/dirty? (get-in (state) [:drafts "x.clj"])))
        (let [before (state) before-calls @calls]
          ((:ok save) (wire stamp (assoc file :entry {:content :duplicate} :saved? true)))
          (is (= before (state)))
          (is (= before-calls @calls)))))))

(deftest target-change-error-discovers-once-and-never-retries-a-write
  (with-client
    (fn [{:keys [a state seed! calls last-call]}]
      (seed!)
      ((:save a) "x.clj")
      ((:error (last-call :save))
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
            ((:error old) (ex-info "Obsolete metadata failure" {}))
            ((:ok old) (wire stamp {:folders {} :entries {} :permissions {:write? true}})))
          (is (= 2 (count (:metadata @calls))) "Finish old token then issue freshest discovery")
          (is (not (:target-verified? (state))) "Superseded response cannot verify the target")
          (is (nil? (get-in (state) [:errors :metadata])) "Superseded errors are not current diagnostics")
          (is (true? (get-in (state) [:loading :metadata])))
          (let [before (state)]
            ((:ok old) (wire stamp {:folders {} :entries {}}))
            ((:error old) (ex-info "Duplicate obsolete failure" {}))
            (is (= before (state)))
            (is (= 2 (count (:metadata @calls)))))
          ((:ok (last-call :metadata))
           (wire stamp {:folders {} :entries {"late.md" nil} :permissions {:write? true}}))
          (is (true? (:target-verified? (state))))
          (is (= stamp (get-in (state) [:drafts "late.md" :source :target-stamp]))))))))

(deftest metadata-own-target-change-failure-is-visible-and-does-not-loop
  (with-client
    (fn [{:keys [a state calls last-call]}]
      ((:discover a))
      ((:error (last-call :metadata))
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
        ((:ok (last-call :metadata)) (wire stamp {:folders {} :entries {"x.clj" nil}}))
        (is (false? (get-in (state) [:loading [:file "x.clj"]])))
        (let [before (state)]
          ((:ok old) (wire stamp {:path "x.clj" :entry {:content :old} :text "Stale"}))
          (is (= before (state))))
        (is (true? (get-in (state) [:drafts "x.clj" :remote-deleted?])))))))

(deftest initially-missing-deep-link-has-a-visible-result-not-permanent-pending-intent
  (with-client
    (fn [{:keys [a state stamp calls last-call]}]
      ((:open a) "missing.md")
      ((:ok (last-call :metadata)) (wire stamp {:folders {} :entries {"missing.md" nil}}))
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
                ((:error old) (ex-info "First upload failed" {}))
                ((:ok old) (wire stamp {:path "out/upload.txt" :entry {:content :first} :saved? true})))
              (is (= intent (:upload-intent (state))))
              (is (= token (:upload-token (state))))
              (is (nil? (get-in (state) [:errors [:upload "out/upload.txt"]]))
                  "An old RPC failure is not the newer promise's diagnostic")
              (is (= "x.clj" (:selected (state))) "Old ACK cannot open/navigate its file")
              ((:then @(:callbacks second)) (.-buffer (js/Uint8Array. #js [66])))
              (is (= 2 (count (:save @calls))) "New promise still owns its publication intent")
              ((:error (last-call :save)) (ex-info "Second upload failed" {}))
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
      ((:ok (last-call :properties)) (wire stamp {:path "x.clj" :target {:kind :page}
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
        ((:ok (last-call :properties)) (wire stamp {:path "x.clj" :target target :expected (:entry file)}))
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
        ((:ok (last-call :properties)) model)
        ((:property-input a) "x.clj" target field (:input field))
        ((:property-save a) "x.clj" target field)
        (is (= {:text "9007199254740993"} (get-in (last-call :property-save) [:request :input])))
        (is (nil? (get-in (last-call :property-save) [:request :value])))
        ((:property-input a) "x.clj" target field {:text "9007199254740995"})
        (swap! states update-in [(:owner a) :drafts "x.clj"] editor/changed "Typing while saving" {:anchor 3 :head 3})
        (let [reply (wire stamp {:entry {:content :new} :text "Property published" :analysis {}})
              saved (last-call :property-save)]
          ((:ok saved) reply)
          (is (= "Typing while saving" (get-in (state) [:drafts "x.clj" :text])))
          (is (= (:entry file) (get-in (state) [:drafts "x.clj" :source :entry])))
          (is (= "Property published" (get-in (state) [:drafts "x.clj" :incoming :text])))
          (is (= {:text "9007199254740995"} (get-in (state) [:property-drafts scope id :input])))
          (let [before (state) requests (count (:metadata @calls))]
            ((:ok saved) reply)
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
        ((:ok (last-call :properties)) model)
        ((:property-input a) "x.clj" target field {:text "42"})
        (let [original (get-in (state) [:property-drafts scope id])]
          (metadata! stamp)
          ((:ensure-properties a))
          ((:ok (last-call :properties)) (assoc model :scope-revision (random-uuid)))
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
          ((:ok (last-call :properties)) model)
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
                ((:ok (last-call :property-save)) (wire stamp {:entry {:content :new} :text "Property saved" :analysis {}}))
                (is (not (contains? (:drafts (state)) "x.clj"))))))))))

(deftest unapplied-property-drafts-refuse-move-instead-of-becoming-hidden
  (with-client
    (fn [{:keys [a state seed! last-call stamp file calls]}]
      (seed!)
      (let [target {:kind :page} field {:id (random-uuid) :alias "revenue" :editable? true :input {:text "0"}}
            model (wire stamp {:path "x.clj" :target target :expected (:entry file)
                               :revision (random-uuid) :scope-revision (random-uuid) :fields [field]})]
        ((:open-properties a) "x.clj" target)
        ((:ok (last-call :properties)) model)
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
        ((:ok first-save) (wire stamp {:entry entry}))
        (is (= "First" (get-in (state) [:drafts "x.clj" :source :text])))
        (is (= "Latest" (get-in (state) [:drafts "x.clj" :text])))
        (is (= 2 (count (:save @calls))))
        (is (= entry (get-in (last-call :save) [:request :expected])))
        (is (= "Latest" (get-in (last-call :save) [:request :text])))
        ((:ok first-save) (wire stamp {:entry (:entry file)}))
        (is (= 2 (count (:save @calls))) "Duplicate ACK does not dispatch")
        ((:ok (last-call :save)) (wire stamp {:entry {:content :latest}}))
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
              ((:ok pending) (wire stamp {:entry {:content :parent-submission}}))
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
              ((:ok (last-call :analyze)) (wire stamp (analysis "Simulated source")))
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
          ((:ok pending) (wire stamp {:entry own}))
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
      ((:error (last-call :save)) (ex-info "Connection lost after possible commit" {}))
      ((:edit a) "x.clj" editor/changed "Later" nil)
      (is (= 1 (count (:save @calls))))
      ((:refresh a))
      ((:ok (last-call :metadata))
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
          ((:ok pending) (wire stamp {:entry {:content :first}}))
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
        ((:ensure-sync a) connection {:connection connection :status :ready :dirty {}})
        ((:ok old) (wire stamp {:entry {:content :possibly-committed}}))
        ((:ok (last-call :metadata))
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
          ((:ok pending) (wire (:target-stamp (state)) (analysis "Changed")))
          (is (= "Alpha" (get-in (state) [:drafts "x.clj" :text])))
          (is (= "Later" (get-in (state) [:drafts "x.clj" :local-source :text]))))
        (let [pending (last-call :analyze)]
          ((:local-source-cancel a) "x.clj")
          ((:error pending) (ex-info "Cancelled preview failed" {}))
          (is (nil? (get-in (state) [:errors [:local-source "x.clj"]]))))
        ((:local-source-open a) "x.clj" key)
        ((:local-source-change a) "x.clj" "Changed" {:anchor 2 :head 2})
        ((:local-source-change a) "x.clj" "Changed" {:anchor 5 :head 5})
        ((:ok (last-call :analyze)) (wire (:target-stamp (state)) (analysis "Changed")))
        (is (= "Changed" (get-in (state) [:drafts "x.clj" :text])))
        (is (= "Changed" (get-in (state) [:drafts "x.clj" :local-source :opened-text])))
        (is (= {:anchor 5 :head 5} (get-in (state) [:drafts "x.clj" :local-source :selection])))
        (is (= "Changed" (get-in (last-call :rich-save) [:request :source])))
        ((:local-source-change a) "x.clj" "Incomplete" nil)
        ((:ok (last-call :analyze)) (wire (:target-stamp (state)) {:semantic {:status :invalid}}))
        (is (= "Incomplete" (get-in (state) [:drafts "x.clj" :local-source :text])))
        (is (= "Changed" (get-in (state) [:drafts "x.clj" :text])))
        (is (= 1 (count (:rich-save @calls))))))))
