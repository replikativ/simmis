(ns is.simm.uis.web.desktop.memory-actions
  "Context/session-owned Memory I/O. Late replies cannot replace another
   view's draft, and no browser write is sent to a replicated Datahike store."
  (:require [clojure.string :as str]
            [org.replikativ.spindel.engine.core :as ec]
            [is.simm.uis.web.desktop.runtime :refer [runtime]]
            [is.simm.uis.web.desktop.signals :as sig]
            [is.simm.uis.web.desktop.memory-editor :as editor]
            [is.simm.uis.web.desktop.memory-sync :as sync]
            [is.simm.uis.web.desktop.memory-remote :as remote]
            [is.simm.uis.web.desktop.remote :as rem]
            [is.simm.runtimes.web :as web]))

(defn state-key [data]
  [(:id (web/current-session)) (str (:room-id data)) (:col-id data) (:tab-id data)])

(defn- parent [path] (str/join "/" (butlast (str/split path #"/"))))

(defn controller [data]
  (let [ctx (or ec/*execution-context* runtime) session (web/current-session)
        room (str (:room-id data)) key (state-key data)
        state-signal (or (:state-signal data) sig/memory-editor-state)
        current? #(web/current-session? session)
        state #(binding [ec/*execution-context* ctx] (get @state-signal key))
        update! (fn [f & args]
                  (binding [ec/*execution-context* ctx]
                    (when (current?) (apply swap! state-signal update key f args))))
        change! (fn [path f & args] (apply update! update-in [:drafts path] f args))
        request!
        (fn [slot invoke consume & [after]]
          (let [token (str (random-uuid))
                valid? #(and (:id session) (current?) (editor/response-current? (state) slot token))
                finish! (fn [f] (binding [ec/*execution-context* ctx] (when (valid?) (update! f))))]
            (update! #(-> % (assoc-in [:requests slot] token) (assoc-in [:loading slot] true)
                          (update :errors dissoc slot)))
            (try
              (binding [ec/*execution-context* ctx ec/*spin-id* nil]
                (let [s (invoke)]
                  (s (fn [reply]
                       (if-let [error (when-not (editor/evaluation-receipt? slot reply) (rem/result-failure reply))]
                         (finish! #(-> % (assoc-in [:loading slot] false)
                                       (assoc-in [:errors slot] (rem/error-text error))))
                         (try
                           (when (finish! #(-> (consume % reply)
                                               (assoc-in [:loading slot] false)
                                               (cond-> (:permissions reply) (assoc :permissions (:permissions reply)))))
                             ;; Device/I/O effects are outside the state transform.
                             (when after (after reply)))
                           (catch :default e
                             (finish! #(-> % (assoc-in [:loading slot] false)
                                           (assoc-in [:errors slot] (rem/error-text e))))))))
                     (fn [error]
                       (finish! #(-> % (assoc-in [:loading slot] false)
                                     (assoc-in [:errors slot] (rem/error-text error))))))))
              (catch :default error
                (finish! #(-> % (assoc-in [:loading slot] false)
                              (assoc-in [:errors slot] (rem/error-text error))))))))]
    (letfn [(navigation-refresh! []
              (when-not (= :navigation (:col-id data))
                ((:refresh (controller {:room-id (:room-id data) :col-id :navigation
                                        :state-signal sig/memory-navigation-state})))))
            (folder! [path]
              (let [epoch (:read-epoch (state))
                    connection (binding [ec/*execution-context* ctx] @sig/memory-connection)]
                (request! [:folder path] #(remote/load-memory-folder! web/server-id room path)
                          #(if (and (= connection @sig/memory-connection) (= epoch (:read-epoch %1)))
                             (assoc-in %1 [:folders path] %2) %1))))
            (analyze! [path]
              (let [text (get-in (state) [:drafts path :text])]
                (when (string? text)
                  (request! [:analysis path] #(remote/analyze-memory-text! web/server-id room path text)
                            #(update-in %1 [:drafts path] editor/analyzed text %2)))))
            (open! [path refresh? & [observed]]
              (when-not observed (update! assoc :selected path))
              (when (or refresh? (nil? (get-in (state) [:drafts path])))
                (let [generation (get-in (state) [:drafts path :generation]) epoch (:read-epoch (state))
                      connection (binding [ec/*execution-context* ctx] @sig/memory-connection)]
                  (request! [:file path] #(remote/load-memory-file! web/server-id room path)
                            (fn [s reply]
                              ;; A save/move completed since this read began.
                              (if (or (not= connection @sig/memory-connection)
                                      (not= epoch (:read-epoch s))
                                      (and observed (not= observed (get-in s [:observed-entries path])))) s
                                  (update-in s [:drafts path] editor/loaded-at-generation generation reply)))))))
            (save! [path]
              (let [draft (get-in (state) [:drafts path]) sent (:text draft)]
                (when (and (not (get-in (state) [:loading [:save path]]))
                           (not (get-in (state) [:loading [:move path]])))
                  (request! [:save path]
                            #(if-let [base (:rich-base draft)]
                               (remote/save-memory-rich-file! web/server-id room
                                                              {:path path :expected (get-in draft [:source :entry]) :text sent
                                                               :source (:text base) :revision (get-in base [:analysis :revision])
                                                               :edits (editor/rich-edits draft)})
                               (remote/save-memory-file! web/server-id room
                                                         {:path path :expected (get-in draft [:source :entry]) :text sent}))
                            (fn [s reply] (update-in s [:drafts path] editor/saved-at-generation (:generation draft) sent reply))
                            (fn [_] (folder! (parent path)) (navigation-refresh!))))))
            (mode! [path mode]
              (do
                (change! path #(cond-> (assoc % :mode mode) (= mode :rich) editor/begin-rich))
                (when (and (not= mode :source) (nil? (get-in (state) [:drafts path :analysis])))
                  (analyze! path))))
            (create! [path]
              (when (and path (not (str/blank? path)))
                (let [markdown? (re-find #"(?i)\.(md|markdown)$" path)
                      text (if markdown? (str "---\nsimmis-id: '" (random-uuid) "'\n---\n# Untitled\n\n") "")
                      file {:path path :text "" :entry nil :kind (if markdown? :markdown :text)}]
                  (update! assoc :selected path)
                  (change! path #(or % (assoc (editor/draft file) :text text :mode :source))))))
            (download! [path]
              (let [entry (get-in (state) [:drafts path :source :entry])]
                (request! [:download path]
                          #(remote/download-memory-file! web/server-id room path entry)
                          (fn [s reply] (assoc s :download reply)))))
            (upload! [folder file]
              (let [path (str (when (seq folder) (str folder "/")) (.-name file))]
                (if (> (.-size file) (* 8 1024 1024))
                  (update! assoc-in [:errors :upload] "File exceeds the 8 MiB interactive upload limit.")
                  (-> (.arrayBuffer file)
                      (.then (fn [buffer]
                               (when (current?)
                                 (let [bytes (js/Uint8Array. buffer)
                                       chunks (for [from (range 0 (.-length bytes) 16384)]
                                                (.apply (.-fromCharCode js/String) nil
                                                        (.subarray bytes from (min (+ from 16384) (.-length bytes)))))
                                       payload (js/btoa (apply str chunks))]
                                   (binding [ec/*execution-context* ctx]
                                     (request! [:upload path]
                                               #(remote/save-memory-file! web/server-id room
                                                                          {:path path :expected nil :base64 payload})
                                               (fn [s _] s)
                                               (fn [_] (folder! folder) (navigation-refresh!) (open! path true))))))))
                      (.catch (fn [e] (update! assoc-in [:errors :upload] (rem/error-text e))))))))
            (office-open! [path part]
              (let [draft (get-in (state) [:drafts path]) generation (:generation draft)
                    expected (get-in draft [:source :entry])]
                (if (or (editor/dirty? draft) (get-in (state) [:loading [:office-save path]]))
                  (update! assoc-in [:errors :office] "Save or discard this XML draft before switching parts.")
                  (request! [:office path]
                            #(remote/load-memory-office-part! web/server-id room path expected part)
                            (fn [s reply]
                              (if (and (= generation (get-in s [:drafts path :generation]))
                                       (not (editor/dirty? (get-in s [:drafts path]))))
                                (assoc-in s [:drafts path :office] (assoc reply :saved-text (:text reply))) s))))))
            (office-save! [path]
              (let [draft (get-in (state) [:drafts path]) office (:office draft) sent (:text office)]
                (when-not (or (get-in (state) [:loading [:office-save path]])
                              (get-in (state) [:loading [:move path]]))
                  (request! [:office-save path]
                            #(remote/save-memory-office-part! web/server-id room
                                                             {:path path :expected (get-in draft [:source :entry])
                                                              :part (:part office) :text sent})
                            (fn [s reply]
                              (if (= (:part office) (get-in s [:drafts path :office :part]))
                                (update-in s [:drafts path]
                                         #(-> % (assoc-in [:source :entry] (:entry reply))
                                              (assoc-in [:office :saved-text] sent)
                                              (assoc :last-commit (:commit reply))
                                              (update :generation inc))) s))
                            (fn [_] (folder! (parent path)) (navigation-refresh!))))))]
      {:owner key :state state :change change! :folder folder! :open #(open! % false) :reload #(open! % true)
       :repl-change (fn [f & args] (apply update! update :repl f args))
       :repl-evaluate
       (fn [syntax]
         (when-not (get-in (state) [:loading :repl])
           (let [request {:text (editor/repl-input (state) syntax) :syntax syntax
                          :namespace (or (get-in (state) [:repl :namespace]) "user")}]
             (request! :repl #(remote/evaluate-memory-repl! web/server-id room request)
                       editor/repl-result))))
       :history (fn [path]
                  (request! [:history path] #(remote/load-memory-history! web/server-id room path)
                            #(assoc-in %1 [:history path] %2)))
       :history-file (fn [path commit-id]
                       (request! [:history-file path]
                                 #(remote/load-memory-history-file! web/server-id room path commit-id)
                                 #(assoc-in %1 [:history-preview path] %2)))
       :evaluate (fn [path scope]
                   (let [draft (get-in (state) [:drafts path])
                         {:keys [anchor head]} (:selection draft)
                         request {:path path :text (:text draft) :scope scope
                                  :from (min (or anchor 0) (or head 0)) :to (max (or anchor 0) (or head 0))}]
                     (when-not (get-in (state) [:loading [:evaluate path]])
                       (request! [:evaluate path] #(remote/evaluate-memory-code! web/server-id room request)
                                 #(assoc-in %1 [:evaluations path] (assoc %2 :input-text (:text draft)))))))
       :ensure-path (fn []
                      (when (and (:path data) (not= (:path data) (:opened-path (state))))
                        (update! assoc :opened-path (:path data))
                        (open! (:path data) false)))
       :ensure-root (fn []
                      (when (and (:id session) (current?)
                                 (nil? (get-in (state) [:folders ""]))
                                 (not (get-in (state) [:requests [:folder ""]])))
                        (folder! "")))
       :ensure-sync
       (fn [connection stream]
         (when (and connection (:id session) (current?))
           (if (not= connection (:watch-connection (state)))
             (do
               (update! assoc :watch-connection connection :sync nil)
               (request! :watch #(remote/prepare-memory-stream! web/server-id room)
                         (fn [s _] s)
                         (fn [_]
                           (binding [ec/*execution-context* ctx]
                             (sync/ensure! connection)))))
             (when (and (= connection (:connection stream)) (= :ready (:status stream)))
               (let [s (state)
                     request (editor/metadata-request s)
                     marker [connection (get-in stream [:dirty room] 0) request]]
                 ;; At most one metadata read at a time. An invalidation while
                 ;; it is pending changes marker; the next render immediately
                 ;; performs the remaining reconciliation, without a timer.
                 (when (and (not= marker (get-in s [:sync :attempted]))
                            (not (get-in s [:loading :metadata])))
                   (let [generations (into {} (map (fn [[path draft]] [path (:generation draft)])) (:drafts s))]
                     (update! assoc-in [:sync :attempted] marker)
                     (if (or (> (count (:folders request)) 64) (> (count (:paths request)) 64))
                       (update! assoc-in [:errors :metadata] "Live updates support 64 expanded folders and file buffers per view. Collapse folders or close buffers, then Refresh.")
                      (request! :metadata #(remote/load-memory-metadata! web/server-id room request)
                               (fn [current reply]
                                 (if (or (not= connection @sig/memory-connection)
                                         (not= connection (:watch-connection current))) current
                                     (-> (reduce-kv
                                          (fn [result path entry]
                                            (if (editor/metadata-generation-current? (get generations path) (get-in result [:drafts path :generation]))
                                              (update-in result [:drafts path] editor/observed-entry entry) result))
                                          current (:entries reply))
                                         (update :read-epoch (fnil inc 0))
                                         (update :observed-entries merge (:entries reply))
                                         (update :folders merge (:folders reply))
                                         (assoc :workspace (:workspace reply))
                                         (assoc-in [:sync :revision] (:revision reply))
                                         (assoc-in [:sync :completed] marker))))
                               (fn [reply]
                                 (when (and (= connection (binding [ec/*execution-context* ctx] @sig/memory-connection))
                                            (= connection (:watch-connection (state))))
                                   (do
                                     (binding [ec/*execution-context* ctx]
                                       (when (seq (:query (state))) ((:search (controller data))))
                                     (doseq [path (keys (:history (state)))]
                                       (request! [:history path] #(remote/load-memory-history! web/server-id room path)
                                                 #(assoc-in %1 [:history path] %2))))
                                   (doseq [[path entry] (:entries reply)
                                           :let [draft (get-in (state) [:drafts path])]
                                           :when (and entry
                                                      (editor/metadata-generation-current? (get generations path) (:generation draft))
                                                      (not= entry (get-in draft [:source :entry]))
                                                      (not= entry (get-in draft [:incoming :entry])))]
                                     (open! path true entry))))))))))))))
       :analyze analyze! :save save! :mode mode! :create create! :download download! :upload upload!
       :office-open office-open! :office-save office-save!
       :select-path #(update! assoc :selected %)
       :set-query #(update! assoc :query %)
       :search (fn [] (let [query (:query (state))]
                       (when-not (str/blank? query)
                         (request! :search #(remote/search-memory-files! web/server-id room query)
                                   (fn [s reply] (if (= query (:query s)) (assoc s :search reply) s))))))
       :toggle-root (fn [] (update! update :root-expanded? not) (folder! (or (:focus-folder (state)) "")))
       :focus-folder (fn [path]
                       (update! assoc :focus-folder path :current-folder path :query "" :root-expanded? true)
                       (folder! path))
       :rich-change (fn [path base-id from model selection]
                      (try (change! path editor/rich-changed base-id from model selection)
                           (catch :default error (update! assoc-in [:errors [:rich path]] (rem/error-text error)))))
       :root (fn [] (update! assoc :current-folder "") (folder! ""))
       :mkdir (fn [path]
                (request! [:mkdir path]
                          #(remote/save-memory-file! web/server-id room
                                                     {:path (str path "/.keep") :expected nil :text ""})
                          (fn [s _] (-> s (assoc :current-folder path) (assoc-in [:expanded path] true)))
                          (fn [_] (folder! (parent path)) (folder! path) (navigation-refresh!))))
       :toggle (fn [path]
                 (let [expanded (get-in (state) [:expanded path])]
                   (update! assoc-in [:expanded path] (not expanded))
                   (when expanded (update! update :folders dissoc path))
                   (update! assoc :current-folder path)
                   (when (and (not expanded) (nil? (get-in (state) [:folders path]))) (folder! path))))
       :refresh (fn []
                  (update! #(-> % (assoc :errors {}) (dissoc :watch-connection :sync)))
                  (doseq [folder (or (seq (keys (:folders (state)))) [(or (:focus-folder (state)) "")])]
                    (folder! folder))
                  (navigation-refresh!)
                  (when-let [path (:selected (state))] (open! path true)))
       :use-incoming (fn [path]
                       (when (js/confirm "Replace this local draft with the newer saved version?")
                         (change! path editor/use-incoming)
                         (update! update :errors dissoc [:save path] [:file path] [:rich path])))
       :move
       (fn [path target]
         (let [draft (get-in (state) [:drafts path])]
           (if (or (editor/dirty? draft) (get-in (state) [:loading [:save path]])
                   (get-in (state) [:loading [:office-save path]])
                   (get-in (state) [:loading [:move path]]) (get-in (state) [:drafts target]))
             (update! assoc-in [:errors :move] "Finish pending saves and choose a destination without another open draft before moving.")
             (request! [:move path]
                       #(remote/move-memory-file! web/server-id room
                                                  {:path path :target target :expected (get-in draft [:source :entry])})
                       (fn [s reply]
                         (let [moved (editor/moved (get-in s [:drafts path]) reply)]
                           (if (get-in s [:drafts target])
                             ;; Never overwrite a separate target draft.
                             (assoc-in s [:errors :move] "File moved; both local drafts were retained. Refresh before saving either draft.")
                             (-> s (assoc-in [:drafts target] moved) (update :drafts dissoc path)
                                 (assoc :selected (:path reply))))))
                       (fn [_]
                         (binding [ec/*execution-context* ctx]
                           (swap! sig/layout-columns
                                  (fn [columns]
                                    (mapv (fn [col]
                                            (if (= (:col-id data) (:id col))
                                              (update col :tabs (fn [tabs]
                                                                 (mapv (fn [tab] (if (= (:tab-id data) (:id tab))
                                                                                  (-> tab (assoc-in [:data :path] target)
                                                                                      (assoc :title (last (str/split target #"/")))) tab)) tabs))) col)) columns))))
                         (folder! (parent path)) (folder! (parent target)) (navigation-refresh!) (open! target true))))))})))
