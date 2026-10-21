(ns is.simm.uis.web.desktop.memory-actions-test
  (:require [cljs.test :refer-macros [deftest is]]
            [clojure.string :as str]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.memory-actions :as actions]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-remote :as remote]
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
                    remote/evaluate-memory-repl! (deferred :repl)
                    remote/save-memory-office-part! (deferred :office-save) remote/move-memory-file! (deferred :move)]
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
